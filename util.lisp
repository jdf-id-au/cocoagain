(in-package :util)

;; awesome https://stevelosh.com/blog/2018/07/fun-with-macros-if-let/

(defun intern-format (&rest args)
  (intern (apply #'format nil args)))

;; TODO 2025-09-07 20:21:23 approach for aggregate types?
;; TODO 2025-09-07 20:38:47 consider float coercion, maybe better not to
(defmacro bidi-ffi (name &rest args)
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
