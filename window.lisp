(in-package :cocoagain)

(defvar *window-table* (make-hash-table))

(cffi:defcallback window-callback :void ((id :int) (event-type :int))
  (when-let* ((window (gethash id *window-table*))
              (handle-fn (case event-type
                           (0 (close-fn window)))))
    (handler-case (funcall handle-fn)
      (error (c) (break (format nil "Caught signal while handling window event ~s" c))))))

(defclass window ()
  ((cocoa-ref :reader cocoa-ref)
   (id :reader id)
   (g-id :initform 0 :reader g-id :allocation :class)
   (title :initarg :title :initform "" :reader title)
   (close-fn :initarg :close-fn :initform nil :accessor close-fn)))

(defun in-screen-rect (rect) ; FIXME 2025-08-13 10:02:40 rect ns cocoagain:rect ?
  (let* ((screen ; TODO 2025-08-13 09:58:42 learn reason for different returns
           #+x86-64 (objc-stret rect (objc "NSScreen" "mainScreen" :pointer)
                                "frame")
           #+arm64 (objc (objc "NSScreen" "mainScreen" :pointer)
                         "frame" (:struct rect)))
         (in-x (- (rect-width screen) (rect-width rect)))
         (in-y (- (rect-height screen) (rect-height rect))))
    (rect (clamp (rect-x rect) 0 in-x)
          (clamp (rect-y rect) 0 in-y)
          (rect-width rect)
          (rect-height rect))))

(defmethod initialise-instance :after
    ((self window) &key rect (x 0) (y 0) (w 400) (h 200)
                     style-mask (closable t) (resizable t) (miniaturisable t))
  (with-slots (cocoa-ref id g-id title close-fn) self
    (setf id g-id)
    (incf g-id)
    (setf cocoa-ref (objc (objc "Window" "alloc" :pointer)
                          "initWithID:frame:styleMask:handleFn:"
                          :int id
                          (:struct rect) (if rect rect (rect x y w h))
                          :int (if style-mask style-mask
                                   (logior (ash 1 0) ; titled
                                           (if closable (ash 1 1) 0)
                                           (if resizable (ash 1 3) 0)
                                           (if miniaturisable (ash 1 2) 0)))
                          :pointer (cffi:callback window-callback)
                          :pointer))
    (objc cocoa-ref "setTitle:" :pointer (autorelease (make-ns-string title)))
    (objc cocoa-ref "setDelegate:" :pointer cocoa-ref)
    (setf (gethash id *window-table*) self)))

(defun window-show (window)
  (objc window "makeKeyAndOrderFront:" :pointer (cffi:null-pointer)))

(defun window-close (window)
  (objc window "performClose:" :pointer (cffi:null-pointer)))

(defun toggle-fullscreen (window)
  (objc window "toggleFullscreen"))

(defun content-view (window)
  (objc window "contentView" :pointer))

(defun (setf content-view) (view window)
  (objc window "setContentView:" :pointer (autorelease view)))

(defun add-subviews (superview subview &rest subviews)
  (dolist (sub (cons subview subviews))
    (objc superview "addSubview:" :pointer (autorelease sub))))

(defun set-always-on-top (window flag)
  (objc window "setLevel:" :int
        (if flag 9 0))) ; kCGStatusWindowLevelKey kCGBaseWindowLevelKey

(defun window-screen (window) (objc window "screen" :pointer))

(defun main-screen () (objc "NSScreen" "mainScreen" :pointer))

(defun list-screens ()
  (let* ((screens (objc "NSScreen" "screens" :pointer)))
    (loop for i below (objc screens "count" :int)
          collect (objc screens "objectAtIndex:" :int i :pointer))))

(defun screen-display (screen)
  (objc (objc (objc screen "deviceDescription" :pointer)
              "objectForKey:" :pointer
              (autorelease (make-ns-string "NSScreenNumber")) :pointer)
        "intValue" :unsigned-int))
