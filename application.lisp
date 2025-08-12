(in-package :cocoagain)

(defvar *startup-hooks* nil)

(cffi:defcallback app-delegate-callback :void ((id :int))
  (case id
    (0 (dolist (hook *startup-hooks*) (funcall hook)))
    (1 (let* ((windows (objc
                        (objc "App" "sharedApplication" :pointer)
                        "windows" :pointer)))
         (dotimes (i (obc windows "count" :int))
           (objc (objc windows "objectAtIndex:" :int i :pointer)
                 "performClose:" :pointer (cffi:null-pointer)))))
    (2 (dolist (hook #+sbcl sb-ext:*exit-hooks*) (funcall hook)))))

(cffi:defcallback app-widget-callback :void ((id :pointer))
  (let* ((task (gethash (cffi:pointer-address id) *widget-id-map*)))
    (when task (handler-case (funcall task id)
                 (error (c)
                   (break (format nil "Caught signal while handling widget callback: \"~a\"" c)))))))
