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
    (objc (objc "NSString" "alloc" :pointer)
          "initWithUTF8String:" :pointer s :pointer)))

(defun ns-string-to-lisp (ns-string)
  (cffi:foreign-string-to-lisp (objc ns-string "UTF8String" :pointer)))

(defun make-cf-string (string)
  (cffi:foreign-funcall "CFStringCreateWithCString"
                        :pointer (cffi:null-pointer)
                        :string string
                        :int 134217984 ; NSUTF8StringEncoding
                        :pointer))

(defun cf-string-to-lisp (cf-string)
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

;; ─────────────────────────────────────────────────────────────────── Structure

(cffi:defcstruct (range :class %range) ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ range
  (location :unsigned-long) (length :unsigned-long)) ; nominally impl detail rather than official interface

(defstruct (range (:constructor range (location length))) location length)

(defmethod cffi:translate-from-foreign (p (type %range))
  (cffi:with-foreign-slots ((location length) p (:struct range)) (range location length)))

(defmethod cffi:translate-into-foreign-memory (range (type %range) p)
  (cffi:with-foreign-slots ((location length) p (:struct range))
    ;;(format t "Trying to set NSRange with ~a~%" range)
    (setf location (range-location range) ; TODO 2025-08-30 07:59:46 overflow check?
          length (range-length range))))

(cffi:defcstruct (point :class %point) ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ point
  (x :double) (y :double))

(defstruct (point (:constructor point (x y))) x y)

(defmethod cffi:translate-from-foreign (p (type %point))
  (cffi:with-foreign-slots ((x y) p (:struct point)) (point x y)))

(defmethod cffi:translate-into-foreign-memory (point (type %point) p)
  (cffi:with-foreign-slots ((x y) p (:struct point))
    (setf x (coerce (point-x point) 'double-float)
          y (coerce (point-y point) 'double-float))))

(cffi:defcstruct (size :class %size) ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ size
  (width :double) (height :double))

(defstruct (size (:constructor size (width height))) width height)

(defmethod cffi:translate-from-foreign (p (type %size))
  (cffi:with-foreign-slots ((width height) p (:struct size)) (size width height)))

(defmethod cffi:translate-into-foreign-memory (size (type %size) p)
  (cffi:with-foreign-slots ((width height) p (:struct size))
    (setf width (coerce (size-width size) 'double-float)
          height (coerce (size-height size) 'double-float))))

(cffi:defcstruct (rect :class %rect) ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ rect
  (origin (:struct point)) (size (:struct size)))

(defstruct (rect (:constructor rect (x y width height))) x y width height)

(defmethod cffi:translate-from-foreign (p (type %rect))
  (cffi:with-foreign-slots ((origin size) p (:struct rect))
    (rect (point-x origin)
          (point-y origin)
          (size-width size)
          (size-height size))))

(defmethod cffi:translate-into-foreign-memory (rect (type %rect) p)
  (let* ((origin (cffi:foreign-slot-pointer p '(:struct rect) 'origin))
         (size (cffi:foreign-slot-pointer p '(:struct rect) 'size)))
    (cffi:with-foreign-slots ((x y) origin (:struct point))
      (cffi:with-foreign-slots ((width height) size (:struct size))
        (setf x (coerce (rect-x rect) 'double-float)
              y (coerce (rect-y rect) 'double-float)
              width (coerce (rect-width rect) 'double-float)
              height (coerce (rect-height rect) 'double-float))))))

;; ─────────────────────────────────────────────────────────────────────── Timer

(cffi:defcfun ("make_timer" %make-timer) :pointer
  (id :int)
  (timer-fn :pointer)
  (interval :double))

(defvar *timer-table* (make-hash-table))

(cffi:defcallback timer-callback :void ((id :int))
  (let* ((timer (gethash id *timer-table*)))
    (funcall (timer-fn timer))))

(defclass timer ()
  ((id :accessor id)
   (g-id :initform 0 :accessor g-id :allocation :class)
   (timer-fn :initarg :timer-fn
             :initform (error "timer-fn should be specified")
             :reader timer-fn)
   (cocoa-ref :accessor cocoa-ref)))

(defmethod initialize-instance :after ((self timer) &key (interval 1.0))
  (setf (id self) (g-id self))
  (incf (g-id self))
  (setf (gethash (id self) *timer-table*) self)
  (setf (cocoa-ref self) (autorelease
                          (objc (alloc "Timer")
                                "initWithID:timenFn:timerInterval:"
                                :int (id self)
                                :pointer (cffi:callback timer-callback)
                                :double (float interval 1.0d0)
                                :pointer))))

(defun invalidate (timer)
  (objc timer "invalidate")) ; FIXME 2025-08-15 22:40:32 remhash id *timer-table*
