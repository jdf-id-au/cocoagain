(ql:quickload :cl-ppcre)

;;(directory "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/Spatial/*.h")

;; FIXME 2025-09-14 11:46:09 over-matching; something to do with escaping?
;; ... better done with libclang...
;; (let* ((re (ppcre:create-scanner "^(SP[A-Z][a-z]\\S*|void) SP[A-Z][a-z]\\S*\\(.*\\)"
;;                                  :single-line-mode t
;;                                  :multi-line-mode t)))

;;   (ppcre:all-matches-as-strings
;;    re
;;    (uiop:read-file-string #P"/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/Spatial/SPAffineTransform3D.h")))

(defconstant +omit-fns+
  ;; Derived from compiler feedback on earlier wrap_spatial.m versions
  '(
    ("SPAffineTransform3D" "SPAffineTransform3DMakeTranslation" (("SPSize3D" "translation")))
    ("SPAffineTransform3D" "SPAffineTransform3DMakeWithProjective" (("SPProjectiveTransform3D" "transform")))
    ("SPPoint3D" "SPPoint3DGetOrigin" (("SPPoint3D" "point")))
    ("SPPoint3D" "SPPoint3DMake" (("double" "x") ("double" "y") ("double" "z")))
    ("SPPoint3D" "SPPoint3DMakeWithVector" (("simd_double3" "xyz")))
    ("SPPoint3D" "SPPoint3DTranslate" (("SPPoint3D" "point") ("SPSize3D" "offset")))
    ("SPPose3D" "SPPose3DTranslate" (("SPPose3D" "pose") ("SPSize3D" "offset")))
    ("SPProjectiveTransform3D" "SPProjectiveTransform3DMakeTranslation" (("SPSize3D" "translation")))
    ("SPRay3D" "SPRay3DTranslate" (("SPRay3D" "ray") ("SPSize3D" "offset")))
    ("SPRect3D" "SPRect3DTranslate" (("SPRect3D" "rect") ("SPSize3D" "offset")))
    ("SPRotation3D" "SPPoint3DRotationToPoint" (("SPPoint3D" "point") ("SPPoint3D" "other")))
    ("SPRotation3D" "SPRect3DRotationToRect" (("SPRect3D" "rect") ("SPRect3D" "other")))
    ("SPRotation3D" "SPRotation3DMake" (("SPRotationAxis3D" "axis") ("SPAngle" "angle")))
    ("SPRotation3D" "SPRotation3DMakeLookAt" (("SPPoint3D" "position") ("SPPoint3D" "target") ("SPVector3D" "up")))
    ("SPRotation3D" "SPRotation3DMakeLookAt" (("SPPoint3D" "target") ("SPVector3D" "up")))
    ("SPRotationAxis3D" "SPRotationAxis3DMakeWithVector" (("simd_double3" "xyz")))
    ("SPScaledPose3D" "SPScaledPose3DConcatenation" (("SPPose3D" "lhs") ("SPScaledPose3D" "rhs")))
    ("SPScaledPose3D" "SPScaledPose3DConcatenation" (("SPScaledPose3D" "lhs") ("SPPose3D" "rhs")))
    ("SPSize3D" "SPSize3DMake" (("double" "width") ("double" "height") ("double" "depth")))
    ("SPSize3D" "SPSize3DMakeWithVector" (("simd_double3" "xyz")))
    ("SPSphericalCoordinates3D" "SPSphericalCoordinates3DMake" (("double" "radius") ("SPAngle" "inclination") ("SPAngle" "azimuth")))
    ("SPSphericalCoordinates3D" "SPSphericalCoordinates3DMakeWithCartesianVector" (("simd_double3" "xyz")))
    ("SPVector3D" "SPAffineTransform3DGetOffset" (("SPAffineTransform3D" "transform")))
    ("SPVector3D" "SPProjectiveTransform3DGetOffset" (("SPProjectiveTransform3D" "transform")))
    ("SPVector3D" "SPVector3DMake" (("double" "x") ("double" "y") ("double" "z")))
    ("SPVector3D" "SPVector3DMakeWithVector" (("simd_double3" "xyz")))
    ("SPVector3D" "SPVector3DMakeWithVector" (("simd_double3" "xyz")))
    ("void" "SPAffineTransform3DSetOffset" (("SPAffineTransform3D" "*transform") ("SPVector3D" "offset")))
    ("void" "SPProjectiveTransform3DSetOffset" (("SPProjectiveTransform3D" "*transform") ("SPVector3D" "offset")))
    ))
    
(defun omitp (fn-desc)
  (if (find fn-desc +omit-fns+ :test 'equal) t nil))

(defun parse-params (paramstr)
  (loop for param in (ppcre:split ",\\s*" paramstr)
        collect (ppcre:split"\\s+" param)))

(defun parse-fn (fnstr)
  (ppcre:register-groups-bind (ret-type fn-name params)
      ("(SP[A-Z][a-z]\\S*|void) (SP[A-Z][a-z]\\S*)\\((.*)\\)" fnstr)
    (list ret-type fn-name (parse-params params))))

(defun parse-file (fname)
  (with-open-file (stream fname :direction :input)
    (loop for line = (read-line stream nil :eof)
          until (eq line :eof)
          for ln = (parse-fn line)
          unless (omitp ln)
          collect ln)))

(defun wrap-params (paramlist)
  (let* ((o (make-string-output-stream))
         (firstp t))
    (loop for (ty param) in paramlist
          do (format o "~a~a ~a"
                     (if firstp (progn (setf firstp nil) "") ", ")
                     ty param))
    (get-output-stream-string o)))

(defun pointer-passthrough (param)
  (ppcre:register-groups-bind
      (star name brack)
      ("(\\*)?([^\\[\\]]*)(\\[\\])?" param)
    (values name (if (or star brack) t))))

(defun wrap-args (paramlist)
  (let* ((o (make-string-output-stream)))
    (destructuring-bind (ty param) (car paramlist)
      (format o "~a" (pointer-passthrough param)))
    (loop for (ty param) in (cdr paramlist)
          do (format o ", ~a" (pointer-passthrough param)))
    (get-output-stream-string o)))

(defun wrap-fns (fns to-stream)
  "Wrap extracted functions in spatial_fns.txt, for writing to target .m
file which will need some massaging in response to compiler messages."
  (format to-stream "// Autogenerated~%#include <Spatial/Spatial.h>~%")
  (loop for (ret-type fn-name params) in fns
        do (format to-stream "~a wrap~a(~a) {~a~a(~a);}~%"
                   ret-type
                   fn-name
                   (wrap-params params)
                   (if (string= ret-type "void") "" "return ")
                   fn-name
                   (wrap-args params))))

(defun all-types (parsed)
  (sort
   (delete-duplicates
    (loop for (ret-type nil params) in parsed
          nconc (cons ret-type (loop for (ty nil) in params collect ty)))
    :test 'equal)
   #'string-lessp))

(defvar types ;; target types from spatial.lisp bidi-ffi calls
  '(("double" . :double)
    ("int" . :int)
    ("simd_double3" . v3)
    ("simd_double4x3" . m43)
    ("simd_double4x4" . m44)
    ("simd_quatd" . v4)
    ("SPAffineTransform3D" . m43)
    ("SPAngle" . :double)
    ("SPEulerAngles" . eul)
    ("SPEulerAngleOrder" . :uint32)
    ("SPPoint3D" . pt)
    ("SPPose3D" . pose)
    ("SPProjectiveTransform3D" . m44)
    ("SPRay3D" . ray)
    ("SPRect3D" . rect)
    ("SPRotation3D" . rot)
    ("SPRotationAxis3D" . rot-axis)
    ("SPScaledPose3D" . spose)
    ("SPSize3D" . size)
    ("SPSphericalCoordinates3D" . sph)
    ("SPVector3D" . vec)))

(defun intern-not-t (param-name)
  (intern (string-upcase
           (if (string= param-name "t") "tp" param-name))))

(defun convert-type (param)
  (destructuring-bind (ty p) param
    (let ((rt (or (cdr (assoc ty types :test 'equal)) :void)))
      (multiple-value-bind (name ptrp) (pointer-passthrough p)
        (if ptrp
            (values :pointer (intern-not-t name))
            (if (keywordp rt)
                (values rt (intern-not-t p))
                (values (list :struct rt) (intern-not-t p))))))))

#+nil(
      (convert-type '("SPAffineTransform3D" "*thing"))
      (convert-type '("SPAffineTransform3D" "thing"))
      (convert-type '("SPAffineTransform3D" nil))
      (convert-type '("SPRotation3D" nil))
      )

(defvar renames
  '(("4x3Matrix" . m43)
    ("4x4Matrix" . m44)
    ("Affine" . aff)
    ("EulerAngles" . eul)
    ("Angle" . ang)
    ("Cartesian" . cart)
    ("Concatenation" . cat)
    ("Degrees" . deg)
    ("Inverse" . inv)
    ("Inverted" . inv)
    ("Normalize" . norm)
    ("Point" . pt)
    ("Projective" . proj) ; larger substring first
    ("Project" . proj)
    ("Quaternion" . quat)
    ("Radians" . rads)
    ("Reflect" . refl)
    ("Rotate" . rot)
    ("Rotation" . rot)
    ("ScaledPose" . spose)
    ("SphericalCoordinates" . sph)
    ("Translation" . tran)
    ("Truncated" . trunc)
    ("Vector" . vec)))

(defun rename-fn (c-name)
  (let ((new c-name))
    (loop for tp in renames
          do (setf new
                   (ppcre:regex-replace-all
                    (car tp) new
                    (symbol-name (cdr tp)))))
    new))

(defun simpler-name (c-name)
  (let* ((shorter (rename-fn (ppcre:regex-replace-all
                                 "(SP|3D|With|Transform)" c-name ""))))
    (multiple-value-bind
     (sym stat)
     (destructuring-bind (to &optional from) (ppcre:split "Make" shorter)
                           (if from (intern (string-upcase (format nil "~a->~a" from to)))
                               (intern (string-upcase to))))
     (values sym stat)
     ;; TODO 2025-09-17 14:02:45 Could fail if already interned; annoying during dev
     #+nil(unless stat sym))))

#+nil(
      (simple-kebab-name "SPPose3DMakeLookAt")
      (simple-kebab-name "SPPose3DGetInverse")
      )

(defun lispify (parsed)
  "Instead of going nuts with CLOS."
  (let* ((defuns
           (loop for (ret-type fn-name params) in parsed
                 ;; TODO 2025-09-17 08:17:20 Is foreign-funcall-pointer any more direct/cacheable?
                 collect
                 `(defun ,(simpler-name fn-name)
                      ,(loop for p in params
                             collect (multiple-value-bind (ty pn) (convert-type p) pn))
                    (cffi:foreign-funcall
                     ,(format nil "wrap~a" fn-name)
                     ,@(loop for p in params
                             nconc (multiple-value-list (convert-type p)))
                     ,(convert-type (list ret-type nil)))))))
    ;; TODO 2025-09-17 13:40:12 check dupe names/help resolve
    `(progn ,@defuns)))

#+nil(;; Output file for massaging and compilation.
      (with-open-file (o "wrap_spatial.m" :direction :output :if-exists :supersede)
        (wrap-fns (parse-file "spatial_fns.txt") o))

      ;; Check types
      (all-types (parse-file "spatial_fns.txt"))
      
      ;; Create ergonomic lisp fns
      (lispify (parse-file "spatial_fns.txt"))
      )
