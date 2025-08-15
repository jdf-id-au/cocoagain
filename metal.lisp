(in-package :mtl)

(cffi:defcstruct (origin :class %origin)
  (x :unsigned-long) ; not float??
  (y :unsigned-long)
  (z :unsigned-long))

(defstruct (origin (:constructor origin (x y z))) x y z)

(defmethod cffi:translate-from-foreign (o (type %origin))
  (cffi:with-foreign-slots ((x y z) o (:struct origin))
    (origin x y z)))

(defmethod cffi:translate-into-foreign-memory (origin (type %origin) o)
  (cffi:with-foreign-slots ((x y z) o (:struct origin))
    (setf x (floor (origin-x origin))
          y (floor (origin-y origin))
          z (floor (origin-z origin)))))

(cffi:defcstruct (size :class %size))
