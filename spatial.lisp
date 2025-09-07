;; /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/spatial/

(in-package :spatial)

;; Also see core-foundation.lisp
(bidi-ffi angle radians :double) ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ angle
(bidi-ffi rotation-axis x :double y :double z :double) ; ╴╴╴╴╴╴╴╴╴ rotation-axis
(bidi-ffi rotation a :double b :double c :double d :double) ; ╴╴ rotation (quat)
(bidi-ffi point x :double y :double z :double) ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ point
(bidi-ffi vec x :double y :double z :double ) ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ vec(tor)
(bidi-ffi size width :double height :double depth :double) ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴ size
(cffi:defcstruct (rect :class %rect) ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ rect
  (origin (:struct point)) (size (:struct size)))
(defstruct (rect (:constructor rect (x y z width height depth))) ; 3d remember
  x y z width height depth)
(defmethod cffi:translate-from-foreign (p (type %rect))
  (cffi:with-foreign-slots ((origin size) p (:struct rect))
    (rect (point-x origin)
          (point-y origin)
          (point-z origin)
          (size-width size)
          (size-height size)
          (size-depth size))))
(defmethod cffi:translate-into-foreign-memory (rect (type %rect) p)
  (let* ((origin (cffi:foreign-slot-pointer p '(:struct rect) 'origin))
         (size (cffi:foreign-slot-pointer p '(:struct rect) 'size)))
    (cffi:with-foreign-slots ((x y z) origin (:struct point))
      (cffi:with-foreign-slots ((width height depth) size (:struct size))
        (setf x (rect-x rect)
              y (rect-y rect)
              z (rect-z rect)
              width (rect-width rect)
              height (rect-height rect)
              depth (rect-depth rect))))))
;; ray : point origin direction
;; pose : point position, rotation rotation

;; affine-transform : double4x3 matrix
;; projective-transform: double4x4 matrix (column-major)
;; spherical-coordinates: double radius, angle inclination azimuth
