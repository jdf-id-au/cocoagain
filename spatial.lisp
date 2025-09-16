;; /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/spatial/

(in-package :spatial)

(defmacro spatial (fname &rest args)
  `(cffi:foreign-funcall ,(format nil "wrap~a" fname)
                         ,@args))

;; TODO 2025-09-14 22:16:56 cache fn lookup/go into cffi weeds
;; TODO 2025-09-14 21:57:33 method for stacking calls without pointless translation; think vaguely about (declare inline...) low priority

(ut:bidi-ffi (v3 :size 32) a b c)
(ut:bidi-ffi (v4 :size 32) a b c d)
(ut:bidi-ffi (angle) r) ; radians
(ut:bidi-ffi (euler-angles) a b c (o :uint32)) ; order eg AxisX defconstant see constants.lisp
(ut:bidi-ffi (rotation-axis :size 32) x y z)
(ut:bidi-ffi (rotation) a b c d)
(ut:bidi-ffi (point :size 32) x y z) ; FIXME 2025-09-17 07:34:57 is padding 1d0?
(ut:bidi-ffi (vec :size 32) x y z) ; don't collide with cl:vector
(ut:bidi-ffi (size :size 32) w h d) ; width height depth
(ut:bidi-ffi (rect :size 64) x y z (w :double :offset 32) h d)
(ut:bidi-ffi (ray :size 64) ox oy oz (dx :double :offset 32) dy dz) ; origin... direction...
(ut:bidi-ffi (pose :size 64) x y z (a :double :offset 32) b c d) ; position... rotation...
(ut:bidi-ffi (scaled-pose :size 68) x y z (a :double :offset 32) b c d s) ; position... rotation... scale
(ut:bidi-ffi (m43 :size 128) ; column-major as row-col
             aa ab ac
             (ba :double :offset 32) bb bc
             (ca :double :offset 64) cb cc
             (da :double :offset 96) db dc)
(ut:bidi-ffi (m44 :size 128) ; column-major as row-col
             aa ab ac ad
             (ba :double :offset 32) bb bc bd
             (ca :double :offset 64) cb cc cd
             (da :double :offset 96) db dc dd)
(ut:bidi-ffi (spherical :size 32) r i a) ; radius inclination azimuth
