(in-package :cocoagain)

;; ───────────────────────────────────────────────────────────────────── Objects

(cffi:defcfun ("obj_getClass" cls) :pointer
  (name :string))

(cffi:defcfun ("sel_getUid" sel) :pointer
  (name :string))

(defgeneric cocoa-ref (self))

(defmethod cocoa-ref ((self #+sbcl sb-sys:system-area-pointer))
  self)

(defmethod cococa-ref ((self string))
  (let* ((object (cls self)))
    (assert (not (cffi:null-pointer-p object)) nil
            "Can't find NSClass \"~a\"" object)
    object))

(defmacro objc (instance sel &rest rest)
  (with-gensyms (object selector)
    `(let* ((,object (cocoa-ref ,instance))
            (,selector (sel ,sel)))
       (assert (not (cffi:null-pointer-p ,object)) nil
               "Selector \"~a\" used with null pointer" ,sel)
       (cffi:foreign-funcall "objc_msgSend"
                             :pointer ,object
                             :pointer ,selector
                             ,@rest))))

#+x86-64
(defmacro objc-stret (return-type instance sel &rest rest)
  (with-gensyms (object selector result)
    `(let* ((,object (cocoa-ref ,instance))
            (,selector (sel ,sel)))
       (assert (not (cffi:null-pointer-p ,object)) nil
               "Selector \"~a\" used with null pointer" ,sel)
       (cffi:with-foreign-objects ((,result '(:struct ,return-type)))
         (cffi:foreign-funcall "objc_msgSend_stret"
                               :pointer ,result
                               :pointer ,object
                               :pointer ,selector
                               ,@rest)
         (cffi:mem-ref ,result '(:struct ,return-type))))))

(defun alloc (cls) (objc cls "alloc" :pointer))

(defmethod init ((instance #+sbcl sb-sys:system-area-pointer))
  (obj instance "init" :pointer))

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

(defun retain-count (instance) (obj instance "retainCount" :unsigned-int))

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
    (cffi:foreign-funcall "CFStringGetCString"
                          :pointer cf-string
                          :pointer buffer
                          :int len
                          :int 134217984 ; NSUTF8StringEncoding
                          )
    (cffi:foreign-string-to-lisp buffer)))

;; ────────────────────────────────────────────────────────────────────── Colour

;; ─────────────────────────────────────────────────────────────────── Structure
