;; /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/spatial/

(in-package :spatial)

;; NB 2025-09-08 08:29:56 Is it worth doing all this labour just for a math lib?
;; ─────────────────────────────────────────────────────────────────────── Types
;; Also see core-foundation.lisp
(bidi-ffi angle radians :double)
(bidi-ffi rotation-axis x :double y :double z :double pad :double)
(bidi-ffi rotation a :double b :double c :double d :double) ; quaternion
(bidi-ffi point x :double y :double z :double pad :double)
(bidi-ffi vec x :double y :double z :double pad :double) ; `vector` is taken
(bidi-ffi size width :double height :double depth :double pad :double)

(cffi:defcstruct (rect :class %rect) ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ (3d) rect
  (origin (:struct point)) (size (:struct size)))

(defstruct (rect (:constructor rect (x y z width height depth)))
  x y z width height depth) ; unfortunately can only :include one thing in defstruct

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

;; TODO 2025-09-07 21:41:40 Surely these can just be arrays...
;; affine-transform : double4x3 matrix
;; projective-transform: double4x4 matrix (column-major)

;; spherical-coordinates: double radius, angle inclination azimuth

#+nil(progn ; TODO 2025-09-07 23:03:36 oh so painful

       (ql:quickload :cocoagain)
       
       (cffi:load-foreign-library "libSpatial.dylib") ; doesn't actually exist? despite libSpatial.tbd contents? header only??

       ;; ────────────────────────────────────────────────────────── Wrap method
       (cffi:defcfun ("wrapSPVector3DApplyAffineTransform" vec-apply-affine-transform)
           (:struct vec) (v (:struct vec)) (mat4x3 :pointer))

       ;; FIXME 2025-09-07 22:43:47 how to access SPAffineTransform3D.h unwrapped?
       (cffi:defcfun ("wrapSPAffineTransform3DMake" affine-transform) :pointer
         (scale (:struct size))
         (rotation (:struct rotation))
         (translation (:struct vec)))

       (vec-apply-affine-transform
        (vec 1.0d0 1.0d0 1.0d0 0.0d0)
        (affine-transform (size 2.0d0 2.0d0 2.0d0 0.0d0)
                          (rotation 0.0d0 0.0d0 0.0d0 1.0d0)
                          (vec 0.2d0 0.3d0 0.4d0 0.0d0)))

       ;; ────────────────────────────────────────────────────── Indirect method

       ;; Could potientially also reuse managed buffer allocated for Metal use...!
       (cffi:with-foreign-objects ((scale :double 4) ; possibly on the stack
                                   (rot :double 4)
                                   (trans :double 4)
                                   (aff :double 16)
                                   (vec-in :double 4)
                                   (vec-out :double 4))
         (let* ((args #(2.0 2.0 2.0 0.0 ; scale x y z
                        0.0 0.0 0.0 1.0 ; rot   a b c d
                        0.2 0.3 0.4 0.0 ; trans x y z
                        ))
                (_ (dotimes (i (array-total-size args))
                     (setf (cffi:mem-aref inbuf :double i)
                           (coerce (elt args i) 'double-float))))
                (n (cffi:foreign-funcall "indirectSPAffineTransform3DMake"
                                         :pointer inbuf
                                         :pointer outbuf
                                         :int32))
                (m (cffi:foreign-funcall "indirectSPVector3DApplyAffineTransform"
                                         :pointer outbuf
                                         :pointer resbuf
                                         :int32)))
           ;;(loop for i below n collect (cffi:mem-aref outbuf :double i))
           (loop for i below 3 collect (cffi:mem-aref resbuf :double i))
           ))

       (with-arena (scratch 100 :double)
         (let* ((scale (put scratch :double #(2 2 0 0)))
                (rot (put scratch :double #(0 0 0 1)))
                (trans (put scratch :double #(0.2 0.3 0.4 0)))
                (aff (alloc scratch 16 :double))
                (vec-in (put scratch :double #(1 1 0 0)))
                (vec-out (alloc scratch 4 :double)))
           (cffi:foreign-funcall "indirectSPAffineTransform3DMake"
                                 :pointer scale
                                 :pointer rot
                                 :pointer trans
                                 :pointer aff)
           (cffi:foreign-funcall "indirectSPVector3DApplyAffineTransform"
                                 :pointer vec-in
                                 :pointer aff
                                 :pointer vec-out)
           (show vec-out)))
       
       )
