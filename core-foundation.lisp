(in-package :cocoagain)

;; ───────────────────────────────────────────────────────────────────── Objects

(cffi:defcfun ("objc_getClass" cls) :pointer
  (name :string))

(cffi:defcfun ("sel_getUid" sel) :pointer
  (name :string))

(defgeneric cocoa-ref (self))

(defmethod cocoa-ref ((self #+sbcl sb-sys:system-area-pointer))
  self)

(defmethod cocoa-ref ((self string))
  (let* ((object (cls self)))
    (assert (not (cffi:null-pointer-p object)) nil
            "Can't find NSClass \"~a\"" self)
    object))

(defmacro objc (instance sel &rest rest)
  (with-gensyms (object selector)
    `(progn
       ;;(format t "objc ~a ~a" ',instance ',sel)
       (let* ((,object (cocoa-ref ,instance))
              (,selector (sel ,sel)))
         (assert (not (cffi:null-pointer-p ,object)) nil
                 "Selector \"~a\" used with null pointer" ,sel)
         (cffi:foreign-funcall "objc_msgSend"
                               :pointer ,object
                               :pointer ,selector
                               ,@rest)))))

#+x86-64
(defmacro objc-stret (return-type instance sel &rest rest)
  (with-gensyms (object selector result)
    `(progn
       ;;(format t "objc ~a ~a" ',instance ',sel)
       (let* ((,object (cocoa-ref ,instance))
              (,selector (sel ,sel)))
         (assert (not (cffi:null-pointer-p ,object)) nil
                 "Selector \"~a\" used with null pointer" ,sel)
         (cffi:with-foreign-objects ((,result '(:struct ,return-type)))
           (cffi:foreign-funcall "objc_msgSend_stret"
                                 :pointer ,result
                                 :pointer ,object
                                 :pointer ,selector
                                 ,@rest)
           (cffi:mem-ref ,result '(:struct ,return-type)))))))


;; TODO 2025-08-30 11:33:03 overlaps a bit with above checks; use it there?
(defmacro protect (form &rest message)
  "Evaluate form and raise error if it's nil or a cffi null pointer."
  `(let ((pointer ,form))
     (if (or (not pointer) ; i.e. nil
             (cffi:null-pointer-p pointer))
         (error ,@message)
         pointer)))

(defun alloc (cls) (objc cls "alloc" :pointer))

(defmethod init ((instance #+sbcl sb-sys:system-area-pointer))
  (objc instance "init" :pointer))

(defun new (cls) (objc (alloc cls) "init" :pointer))

(defun retain (instance) (objc instance "retain" :pointer))

(defmethod release ((instance #+sbcl sb-sys:system-area-pointer))
  (objc instance "release"))

(defun autorelease (instance) (objc instance "autorelease" :pointer))

(defun cf-retain (cf-instance)
  (cffi:foreign-funcall "CFRetain" :pointer cf-instance :pointer))

(defun cf-release (cf-instance)
  (cffi:foreign-funcall "CFRelease" :pointer cf-instance))

(defun cf-autorelease (cf-instance)
  (cffi:foreign-funcall "CFAutorelease" :pointer cf-instance))

(defun retain-count (instance) (objc instance "retainCount" :unsigned-int))

;; ───────────────────────────────────────────────────────────────────── Strings

(defun make-ns-string (string)
  (cffi:with-foreign-strings ((s string))
    (objc (alloc "NSString") ; TODO 2025-08-30 14:43:26 lifecycle?
          "initWithUTF8String:" :pointer s :pointer)))

(defun ns-string-to-lisp (ns-string)
  (cffi:foreign-string-to-lisp (objc ns-string "UTF8String" :pointer)))

(defun make-cf-string (string)
  (cffi:foreign-funcall "CFStringCreateWithCString"
                        :pointer (cffi:null-pointer)
                        :string string
                        :int 134217984 ; NSUTF8StringEncoding
                        :pointer))

(defun cf-string-to-lisp (cf-string) ; NB 2025-08-30 14:43:50 impl?
  (let* ((len (1+ (* 3 (cffi:foreign-funcall "CFStringGetLength"
                                             :pointer cf-string
                                             :int)))))
    (cffi:with-foreign-objects ((buffer :char len))
      (cffi:foreign-funcall "CFStringGetCString"
                            :pointer cf-string
                            :pointer buffer
                            :int len
                            :int 134217984 ; NSUTF8StringEncoding
                            )
      (cffi:foreign-string-to-lisp buffer))))

;; ────────────────────────────────────────────────────────────────────── Colour

(defun color (&key (r 0.0) (g 0.0) (b 0.0) (a 1.0))
  (ns:objc "NSColor" "colorWithCalibratedRed:green:blue:alpha:"
	   :double (float r 1.0d0)
	   :double (float g 1.0d0)
	   :double (float b 1.0d0)
	   :double (float a 1.0d0)
	   :pointer))

;; ─────────────────────────────────────────────────────────────────────── Types

(ut:bidi-ffi (range :type :unsigned-long) loc len)
(ut:bidi-ffi (point) x y)
(ut:bidi-ffi (size) w h)
(ut:bidi-ffi (rect) x y w h)

;; ─────────────────────────────────────────────────────────────────────── Timer

(defvar *timer-table* (make-hash-table))

(cffi:defcallback timer-callback :void ((id :int) (seconds :double))
  (let* ((timer (gethash id *timer-table*)))
    ;; Could use GET-INTERNAL-REAL-TIME etc instead of passing seconds.
    (funcall (timer-fn timer) seconds)))

(defclass timer ()
  ((id :accessor id)
   (g-id :initform 0 :accessor g-id :allocation :class)
   (timer-fn :initarg :timer-fn
             :initform (error "timer-fn should be specified")
             :reader timer-fn)
   (cocoa-ref :accessor cocoa-ref))
  (:documentation "Wrapped NSTimer for callen timer-fn at interval seconds."))

(defmethod initialize-instance :after ((self timer) &key (interval 1.0))
  (setf (id self) (g-id self))
  (incf (g-id self))
  (setf (gethash (id self) *timer-table*) self)
  (setf (cocoa-ref self) (autorelease
                          (objc (alloc "Timer")
                                "initWithID:timerFn:timerInterval:"
                                :int (id self)
                                :pointer (cffi:callback timer-callback)
                                :double (float interval 1.0d0)
                                :pointer))))

(defmethod invalidate ((self timer))
  ;; "Must send from thread on which timer was installed"
  (ns::queue-for-event-loop (lambda () (objc self "invalidate")))
  (remhash (id self) *timer-table*))

;; (invalidate (gethash 0 *timer-table*))
;; (maphash (lambda (k v) (invalidate v)) *timer-table*)
