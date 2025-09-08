(in-package :util)

;; awesome https://stevelosh.com/blog/2018/07/fun-with-macros-if-let/

(defun intern-format (&rest args)
  (intern (apply #'format nil args)))

;; TODO 2025-09-07 20:21:23 approach for aggregate types?
;; TODO 2025-09-07 20:38:47 consider float coercion, maybe better not to
(defmacro bidi-ffi (name &rest args) ; ─────────────────────── Bidirectional FFI
  (let* ((sn (intern-format "%~a" name))
         (ps (intern "P")) ; symbol P interned in calling package
         (params (loop for a in args by #'cddr collect a))
         )
    `(progn
       (cffi:defcstruct (,name :class ,sn)
         ,@(loop for (p ty) on args by #'cddr collect `(,p ,ty)))
       (defstruct (,name (:constructor ,name ,params)) ,@params)
       (defmethod cffi:translate-from-foreign (,ps (type ,sn))
         (cffi:with-foreign-slots (,params ,ps (:struct ,name)) (,name ,@params)))
       (defmethod cffi:translate-into-foreign-memory (,name (type ,sn) ,ps)
         (cffi:with-foreign-slots (,params ,ps (:struct ,name))
           (setf ,@(loop for (p ty) on args by #'cddr nconcing
                         `(,p (,(intern-format "~a-~a" name p) ,name)))))))))

(defclass arena () ; ───────────────────────────────────────────────────── Arena
  ((pointer :accessor pointer :initform nil)
   (size :accessor size :initform 0 :documentation "Size in bytes")
   (cur :accessor cur :initform 0 :documentation "Cursor offset within arena in bytes: first available byte.")))

(defmethod initialize-instance :after ((self arena) &key count (type :char) using)
  "Configure arena from newly allocated memory or :using an existing buffer of :count bytes."
  (assert (and (integerp count) (< 0 count)) nil "Invalid count ~a." count)
  (if using
      (progn
        (assert (eq type :char) nil "Count must be in bytes if using an existing buffer (not ~a)." type)
        (setf (pointer self) using
              (size self) count))
      ;; FIXME 2025-09-08 14:02:47 alloc failure behaviour?
      (setf (pointer self) (cffi:foreign-alloc type :count count) 
            (size self) (* (cffi:foreign-type-size type) count))))

(defmethod index ((self arena) count &optional (type :char))
  "Pointer to index within arena."
  (assert (< -1 (* (cffi:foreign-type-size type) count) (size self)) nil
          "Out of range: can't access ~ath ~a of arena size ~a." count type (size self))
  (cffi:mem-aptr (pointer self) type count))

(defmethod avail ((self arena))
  "Available space in bytes."
  (- (size self) (cur self)))

(defmethod alloc ((self arena) count &optional (type :char))
  "Allocate within arena, returning pointer."
  (let* ((align (cffi:foreign-type-alignment type))
         (pad (- align (mod (cur self) align))) ; Assumes (pointer self) at suitable alignment (if not, rewrite with cur as a ptr)
         (prev (cur self))
         (req (+ pad (* (cffi:foreign-type-size type) count))))
    (assert (<= req (avail self)) nil "Out of arena memory: ~a avail ~a requested (including padding)." (avail self) req)
    (incf (cur self) req)
    ;;(format t "Allocating ~a ~as~%" count type)
    (index self (+ prev pad))))

(defmethod put ((self arena) type vals)
  "Put vals of ffi type into arena. Check returned pointer for start position (after any padding)."
  (let* ((count (if (array-has-fill-pointer-p vals)
                    (fill-pointer vals)
                    (array-total-size vals)))
         (at (alloc self count type))
         (coer (case type
                 (:float (lambda (v) (coence v 'single-float)))
                 (:double (lambda (v) (coerce v 'double-float)))
                 (t (lambda (v) (identity v))))))
    ;; TODO 2025-09-08 16:13:29 use ARRAY-ELEMENT-TYPE somehow?
    ;;(format t "About to put ~a ~a into ~a" type vals at)
    (dotimes (i count) (setf (cffi:mem-aref at type i) (funcall coer (elt vals i))))
    ;;(format t "...success~%")
    at))

(defun fetch (pointer count &optional (as-type :char))
  "No bounds or type checking!"
  (loop for i below count
        collect (cffi:mem-aref pointer as-type i)))

(defmethod free ((self arena))
  "Free whole arena."
  (cffi:foreign-free (pointer self))
  (setf (pointer self) nil
        (size self) 0
        (cur self) 0))

(defmacro with-arena ((name count &optional (type :char)) &body body)
  (alexandria:with-gensyms (ret)
    `(let* ((,name (make-instance 'arena :count ,count :type ,type))
            (,ret ,@body))
       (free ,name)
       ,ret)))
