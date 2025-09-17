;; /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/spatial/

(in-package :spatial)

(defmacro spatial (fname &rest args)
  `(cffi:foreign-funcall ,(format nil "wrap~a" fname)
                         ,@args))

;; TODO 2025-09-14 22:16:56 cache fn lookup/go into cffi weeds
;; TODO 2025-09-14 21:57:33 method for stacking calls without pointless translation; think vaguely about (declare inline...) low priority

(ut:bidi-ffi (v3 :size 32) a b c)
(ut:bidi-ffi (v4 :size 32) a b c d)
(ut:bidi-ffi (ang) r) ; radians
(ut:bidi-ffi (eul) a b c (o :uint32)) ; order eg AxisX defconstant see constants.lisp
(ut:bidi-ffi (rot-axis :size 32) x y z)
(ut:bidi-ffi (rot) a b c d) ; constructor is deliberately clobbered below
(ut:bidi-ffi (pt :size 32) x y z) ; FIXME 2025-09-17 07:34:57 is padding 1d0?
(ut:bidi-ffi (vec :size 32) x y z) ; don't collide with cl:vector
(ut:bidi-ffi (size :size 32) w h d) ; width height depth
(ut:bidi-ffi (rect :size 64) x y z (w :double :offset 32) h d)
(ut:bidi-ffi (ray :size 64) ox oy oz (dx :double :offset 32) dy dz) ; origin... direction...
(ut:bidi-ffi (pose :size 64) x y z (a :double :offset 32) b c d) ; position... rotation...
(ut:bidi-ffi (spose :size 68) x y z (a :double :offset 32) b c d s) ; position... rotation... scale
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
(ut:bidi-ffi (sph :size 32) r i a) ; radius inclination azimuth

#+nil(
      (rot->aff (rot 1d0 2d0 3d0 4d0))
      )

;; ──────────────────────────────────────────────── Made with wrap-spatial.lisp!
(PROGN
 (DEFUN AFFCAT (T1 T2)
   (CFFI:FOREIGN-FUNCALL "wrapSPAffineTransform3DConcatenation" (:STRUCT M43)
                         T1 (:STRUCT M43) T2 (:STRUCT M43)))
 (DEFUN AFFINV (TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPAffineTransform3DInverted" (:STRUCT M43)
                         TRANSFORM (:STRUCT M43)))
 (DEFUN ROT->AFF (ROTATION)
   (CFFI:FOREIGN-FUNCALL "wrapSPAffineTransform3DMakeRotation" (:STRUCT ROT)
                         ROTATION (:STRUCT M43)))
 (DEFUN SCALE->AFF (SCALE)
   (CFFI:FOREIGN-FUNCALL "wrapSPAffineTransform3DMakeScale" (:STRUCT SIZE)
                         SCALE (:STRUCT M43)))
 (DEFUN TRAN->AFF (OFFSET)
   (CFFI:FOREIGN-FUNCALL "wrapSPAffineTransform3DMakeTranslation" (:STRUCT VEC)
                         OFFSET (:STRUCT M43)))
 (DEFUN M43->AFF (MATRIX)
   (CFFI:FOREIGN-FUNCALL "wrapSPAffineTransform3DMakeWith4x3Matrix"
                         (:STRUCT M43) MATRIX (:STRUCT M43)))
 (DEFUN M44->AFF (MATRIX)
   (CFFI:FOREIGN-FUNCALL "wrapSPAffineTransform3DMakeWith4x4Matrix"
                         (:STRUCT M44) MATRIX (:STRUCT M43)))
 (DEFUN POSE->AFF (POSE)
   (CFFI:FOREIGN-FUNCALL "wrapSPAffineTransform3DMakeWithPose" (:STRUCT POSE)
                         POSE (:STRUCT M43)))
 (DEFUN SPOSE->AFF (POSE)
   (CFFI:FOREIGN-FUNCALL "wrapSPAffineTransform3DMakeWithScaledPose"
                         (:STRUCT SPOSE) POSE (:STRUCT M43)))
 (DEFUN TRUNCM44->AFF (MATRIX)
   (CFFI:FOREIGN-FUNCALL "wrapSPAffineTransform3DMakeWithTruncated4x4Matrix"
                         (:STRUCT M44) MATRIX (:STRUCT M43)))
 (DEFUN TRUNCPROJ->AFF (TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPAffineTransform3DMakeWithTruncatedProjective"
                         (:STRUCT M44) TRANSFORM (:STRUCT M43)))
 (DEFUN ANGACOS (X) (CFFI:FOREIGN-FUNCALL "wrapSPAngleAcos" :DOUBLE X :DOUBLE))
 (DEFUN ANGACOSH (X)
   (CFFI:FOREIGN-FUNCALL "wrapSPAngleAcosh" :DOUBLE X :DOUBLE))
 (DEFUN ANGASIN (X) (CFFI:FOREIGN-FUNCALL "wrapSPAngleAsin" :DOUBLE X :DOUBLE))
 (DEFUN ANGASINH (X)
   (CFFI:FOREIGN-FUNCALL "wrapSPAngleAsinh" :DOUBLE X :DOUBLE))
 (DEFUN ANGATAN (X) (CFFI:FOREIGN-FUNCALL "wrapSPAngleAtan" :DOUBLE X :DOUBLE))
 (DEFUN ANGATAN2 (Y X)
   (CFFI:FOREIGN-FUNCALL "wrapSPAngleAtan2" :DOUBLE Y :DOUBLE X :DOUBLE))
 (DEFUN ANGATANH (X)
   (CFFI:FOREIGN-FUNCALL "wrapSPAngleAtanh" :DOUBLE X :DOUBLE))
 (DEFUN DEG->ANG (DEGREES)
   (CFFI:FOREIGN-FUNCALL "wrapSPAngleMakeWithDegrees" :DOUBLE DEGREES :DOUBLE))
 (DEFUN RADS->ANG (RADIANS)
   (CFFI:FOREIGN-FUNCALL "wrapSPAngleMakeWithRadians" :DOUBLE RADIANS :DOUBLE))
 (DEFUN ANGNEGATE (ANGLE)
   (CFFI:FOREIGN-FUNCALL "wrapSPAngleNegate" :DOUBLE ANGLE :DOUBLE))
 (DEFUN ANGNORM (ANGLE)
   (CFFI:FOREIGN-FUNCALL "wrapSPAngleNormalize" :DOUBLE ANGLE :DOUBLE))
 (DEFUN ROTGETANG (ROTATION)
   (CFFI:FOREIGN-FUNCALL "wrapSPRotation3DGetAngle" (:STRUCT ROT) ROTATION
                         :DOUBLE))
 (DEFUN ROTGETEUL (ROTATION ORDER)
   (CFFI:FOREIGN-FUNCALL "wrapSPRotation3DGetEulerAngles" (:STRUCT ROT)
                         ROTATION :UINT32 ORDER (:STRUCT EUL)))
 (DEFUN PTAPPLYAFF (POINT TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPPoint3DApplyAffineTransform" (:STRUCT PT) POINT
                         (:STRUCT M43) TRANSFORM (:STRUCT PT)))
 (DEFUN PTAPPLYPROJ (POINT TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPPoint3DApplyProjectiveTransform" (:STRUCT PT)
                         POINT (:STRUCT M44) TRANSFORM (:STRUCT PT)))
 (DEFUN PTCLAMPTORECT (POINT RECT)
   (CFFI:FOREIGN-FUNCALL "wrapSPPoint3DClampToRect" (:STRUCT PT) POINT
                         (:STRUCT RECT) RECT (:STRUCT PT)))
 (DEFUN SIZE->PT (SIZE)
   (CFFI:FOREIGN-FUNCALL "wrapSPPoint3DMakeWithSize" (:STRUCT SIZE) SIZE
                         (:STRUCT PT)))
 (DEFUN SPH->PT (COORDS)
   (CFFI:FOREIGN-FUNCALL "wrapSPPoint3DMakeWithSphericalCoordinates"
                         (:STRUCT SPH) COORDS (:STRUCT PT)))
 (DEFUN VEC->PT (XYZ)
   (CFFI:FOREIGN-FUNCALL "wrapSPPoint3DMakeWithVector" (:STRUCT VEC) XYZ
                         (:STRUCT PT)))
 (DEFUN PTROT (POINT ROTATION)
   (CFFI:FOREIGN-FUNCALL "wrapSPPoint3DRotate" (:STRUCT PT) POINT (:STRUCT ROT)
                         ROTATION (:STRUCT PT)))
 (DEFUN PTROTAROUNDPT (POINT ROTATION PIVOT)
   (CFFI:FOREIGN-FUNCALL "wrapSPPoint3DRotateAroundPoint" (:STRUCT PT) POINT
                         (:STRUCT ROT) ROTATION (:STRUCT PT) PIVOT
                         (:STRUCT PT)))
 (DEFUN PTROTBYQUAT (POINT QUATERNION)
   (CFFI:FOREIGN-FUNCALL "wrapSPPoint3DRotateByQuaternion" (:STRUCT PT) POINT
                         (:STRUCT V4) QUATERNION (:STRUCT PT)))
 (DEFUN PTROTBYQUATAROUNDPT (POINT QUATERNION PIVOT)
   (CFFI:FOREIGN-FUNCALL "wrapSPPoint3DRotateByQuaternionAroundPoint"
                         (:STRUCT PT) POINT (:STRUCT V4) QUATERNION
                         (:STRUCT PT) PIVOT (:STRUCT PT)))
 (DEFUN PTTRANSLATE (POINT OFFSET)
   (CFFI:FOREIGN-FUNCALL "wrapSPPoint3DTranslate" (:STRUCT PT) POINT
                         (:STRUCT VEC) OFFSET (:STRUCT PT)))
 (DEFUN PTUNAPPLYAFF (POINT TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPPoint3DUnapplyAffineTransform" (:STRUCT PT)
                         POINT (:STRUCT M43) TRANSFORM (:STRUCT PT)))
 (DEFUN PTUNAPPLYPROJ (POINT TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPPoint3DUnapplyProjectiveTransform" (:STRUCT PT)
                         POINT (:STRUCT M44) TRANSFORM (:STRUCT PT)))
 (DEFUN RECTGETCENTER (RECT)
   (CFFI:FOREIGN-FUNCALL "wrapSPRect3DGetCenter" (:STRUCT RECT) RECT
                         (:STRUCT PT)))
 (DEFUN RECTGETMAXIMUM (RECT)
   (CFFI:FOREIGN-FUNCALL "wrapSPRect3DGetMaximum" (:STRUCT RECT) RECT
                         (:STRUCT PT)))
 (DEFUN RECTGETMINIMUM (RECT)
   (CFFI:FOREIGN-FUNCALL "wrapSPRect3DGetMinimum" (:STRUCT RECT) RECT
                         (:STRUCT PT)))
 (DEFUN POSECAT (LHS RHS)
   (CFFI:FOREIGN-FUNCALL "wrapSPPose3DConcatenation" (:STRUCT POSE) LHS
                         (:STRUCT POSE) RHS (:STRUCT POSE)))
 (DEFUN POSEGETINV (POSE)
   (CFFI:FOREIGN-FUNCALL "wrapSPPose3DGetInverse" (:STRUCT POSE) POSE
                         (:STRUCT POSE)))
 (DEFUN LOOKAT->POSE (FORWARD UP)
   (CFFI:FOREIGN-FUNCALL "wrapSPPose3DMakeLookAt" (:STRUCT VEC) FORWARD
                         (:STRUCT VEC) UP (:STRUCT POSE)))
 (DEFUN M44->POSE (MATRIX)
   (CFFI:FOREIGN-FUNCALL "wrapSPPose3DMakeWith4x4Matrix" (:STRUCT M44) MATRIX
                         (:STRUCT POSE)))
 (DEFUN AFF->POSE (TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPPose3DMakeWithAffineTransform" (:STRUCT M43)
                         TRANSFORM (:STRUCT POSE)))
 (DEFUN PROJ->POSE (TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPPose3DMakeWithProjectiveTransform"
                         (:STRUCT M44) TRANSFORM (:STRUCT POSE)))
 (DEFUN POSEROT (POSE ROTATION)
   (CFFI:FOREIGN-FUNCALL "wrapSPPose3DRotate" (:STRUCT POSE) POSE (:STRUCT ROT)
                         ROTATION (:STRUCT POSE)))
 (DEFUN POSEROTBYQUAT (POSE QUATERNION)
   (CFFI:FOREIGN-FUNCALL "wrapSPPose3DRotateByQuaternion" (:STRUCT POSE) POSE
                         (:STRUCT V4) QUATERNION (:STRUCT POSE)))
 (DEFUN POSETRANSLATE (POSE OFFSET)
   (CFFI:FOREIGN-FUNCALL "wrapSPPose3DTranslate" (:STRUCT POSE) POSE
                         (:STRUCT VEC) OFFSET (:STRUCT POSE)))
 (DEFUN PROJINV (TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPProjectiveTransform3DInverted" (:STRUCT M44)
                         TRANSFORM (:STRUCT M44)))
 (DEFUN ROT->PROJ (ROTATION)
   (CFFI:FOREIGN-FUNCALL "wrapSPProjectiveTransform3DMakeRotation"
                         (:STRUCT ROT) ROTATION (:STRUCT M44)))
 (DEFUN SCALE->PROJ (SCALE)
   (CFFI:FOREIGN-FUNCALL "wrapSPProjectiveTransform3DMakeScale" (:STRUCT SIZE)
                         SCALE (:STRUCT M44)))
 (DEFUN TRAN->PROJ (OFFSET)
   (CFFI:FOREIGN-FUNCALL "wrapSPProjectiveTransform3DMakeTranslation"
                         (:STRUCT VEC) OFFSET (:STRUCT M44)))
 (DEFUN M44->PROJ (MATRIX)
   (CFFI:FOREIGN-FUNCALL "wrapSPProjectiveTransform3DMakeWith4x4Matrix"
                         (:STRUCT M44) MATRIX (:STRUCT M44)))
 (DEFUN AFF->PROJ (TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPProjectiveTransform3DMakeWithAffine"
                         (:STRUCT M43) TRANSFORM (:STRUCT M44)))
 (DEFUN POSE->PROJ (POSE)
   (CFFI:FOREIGN-FUNCALL "wrapSPProjectiveTransform3DMakeWithPose"
                         (:STRUCT POSE) POSE (:STRUCT M44)))
 (DEFUN SPOSE->PROJ (POSE)
   (CFFI:FOREIGN-FUNCALL "wrapSPProjectiveTransform3DMakeWithScaledPose"
                         (:STRUCT SPOSE) POSE (:STRUCT M44)))
 (DEFUN PROJSCALEBYSIZE (TRANSFORM SCALE)
   (CFFI:FOREIGN-FUNCALL "wrapSPProjectiveTransform3DScaleBySize" (:STRUCT M44)
                         TRANSFORM (:STRUCT SIZE) SCALE (:STRUCT M44)))
 (DEFUN PROJSCALEUNIFORM (TRANSFORM SCALE)
   (CFFI:FOREIGN-FUNCALL "wrapSPProjectiveTransform3DScaleUniform"
                         (:STRUCT M44) TRANSFORM :DOUBLE SCALE (:STRUCT M44)))
 (DEFUN RAYROT (RAY ROTATION)
   (CFFI:FOREIGN-FUNCALL "wrapSPRay3DRotate" (:STRUCT RAY) RAY (:STRUCT ROT)
                         ROTATION (:STRUCT RAY)))
 (DEFUN RAYROTAROUNDPT (RAY ROTATION PIVOT)
   (CFFI:FOREIGN-FUNCALL "wrapSPRay3DRotateAroundPoint" (:STRUCT RAY) RAY
                         (:STRUCT ROT) ROTATION (:STRUCT PT) PIVOT
                         (:STRUCT RAY)))
 (DEFUN RAYROTBYQUAT (RAY QUATERNION)
   (CFFI:FOREIGN-FUNCALL "wrapSPRay3DRotateByQuaternion" (:STRUCT RAY) RAY
                         (:STRUCT V4) QUATERNION (:STRUCT RAY)))
 (DEFUN RAYROTBYQUATAROUNDPT (RAY QUATERNION PIVOT)
   (CFFI:FOREIGN-FUNCALL "wrapSPRay3DRotateByQuaternionAroundPoint"
                         (:STRUCT RAY) RAY (:STRUCT V4) QUATERNION (:STRUCT PT)
                         PIVOT (:STRUCT RAY)))
 (DEFUN RAYTRANSLATE (RAY OFFSET)
   (CFFI:FOREIGN-FUNCALL "wrapSPRay3DTranslate" (:STRUCT RAY) RAY (:STRUCT VEC)
                         OFFSET (:STRUCT RAY)))
 (DEFUN RECTAPPLYAFF (RECT TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPRect3DApplyAffineTransform" (:STRUCT RECT) RECT
                         (:STRUCT M43) TRANSFORM (:STRUCT RECT)))
 (DEFUN RECTAPPLYPROJ (RECT TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPRect3DApplyProjectiveTransform" (:STRUCT RECT)
                         RECT (:STRUCT M44) TRANSFORM (:STRUCT RECT)))
 (DEFUN RECTINSET (RECT DXYZ)
   (CFFI:FOREIGN-FUNCALL "wrapSPRect3DInset" (:STRUCT RECT) RECT (:STRUCT SIZE)
                         DXYZ (:STRUCT RECT)))
 (DEFUN RECTINTEGRAL (RECT)
   (CFFI:FOREIGN-FUNCALL "wrapSPRect3DIntegral" (:STRUCT RECT) RECT
                         (:STRUCT RECT)))
 (DEFUN RECTINTERSECTION (RECT OTHER)
   (CFFI:FOREIGN-FUNCALL "wrapSPRect3DIntersection" (:STRUCT RECT) RECT
                         (:STRUCT RECT) OTHER (:STRUCT RECT)))
 (DEFUN BOUNDINGFROMPTS->RECT (POINTS POINTCOUNT)
   (CFFI:FOREIGN-FUNCALL "wrapSPRect3DMakeBoundingFromPoints" :POINTER POINTS
                         :INT POINTCOUNT (:STRUCT RECT)))
 (DEFUN RECTROT (RECT ROTATION)
   (CFFI:FOREIGN-FUNCALL "wrapSPRect3DRotate" (:STRUCT RECT) RECT (:STRUCT ROT)
                         ROTATION (:STRUCT RECT)))
 (DEFUN RECTROTAROUNDPT (RECT ROTATION PIVOT)
   (CFFI:FOREIGN-FUNCALL "wrapSPRect3DRotateAroundPoint" (:STRUCT RECT) RECT
                         (:STRUCT ROT) ROTATION (:STRUCT PT) PIVOT
                         (:STRUCT RECT)))
 (DEFUN RECTROTBYQUAT (RECT QUATERNION)
   (CFFI:FOREIGN-FUNCALL "wrapSPRect3DRotateByQuaternion" (:STRUCT RECT) RECT
                         (:STRUCT V4) QUATERNION (:STRUCT RECT)))
 (DEFUN RECTROTBYQUATAROUNDPT (RECT QUATERNION PIVOT)
   (CFFI:FOREIGN-FUNCALL "wrapSPRect3DRotateByQuaternionAroundPoint"
                         (:STRUCT RECT) RECT (:STRUCT V4) QUATERNION
                         (:STRUCT PT) PIVOT (:STRUCT RECT)))
 (DEFUN RECTSCALEBY (RECT X Y Z)
   (CFFI:FOREIGN-FUNCALL "wrapSPRect3DScaleBy" (:STRUCT RECT) RECT :DOUBLE X
                         :DOUBLE Y :DOUBLE Z (:STRUCT RECT)))
 (DEFUN RECTSCALEBYSIZE (RECT SCALE)
   (CFFI:FOREIGN-FUNCALL "wrapSPRect3DScaleBySize" (:STRUCT RECT) RECT
                         (:STRUCT SIZE) SCALE (:STRUCT RECT)))
 (DEFUN RECTSCALEUNIFORM (RECT SCALE)
   (CFFI:FOREIGN-FUNCALL "wrapSPRect3DScaleUniform" (:STRUCT RECT) RECT :DOUBLE
                         SCALE (:STRUCT RECT)))
 (DEFUN RECTSTANDARDIZE (RECT)
   (CFFI:FOREIGN-FUNCALL "wrapSPRect3DStandardize" (:STRUCT RECT) RECT
                         (:STRUCT RECT)))
 (DEFUN RECTTRANSLATE (RECT OFFSET)
   (CFFI:FOREIGN-FUNCALL "wrapSPRect3DTranslate" (:STRUCT RECT) RECT
                         (:STRUCT VEC) OFFSET (:STRUCT RECT)))
 (DEFUN RECTUNAPPLYAFF (RECT TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPRect3DUnapplyAffineTransform" (:STRUCT RECT)
                         RECT (:STRUCT M43) TRANSFORM (:STRUCT RECT)))
 (DEFUN RECTUNAPPLYPROJ (RECT TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPRect3DUnapplyProjectiveTransform"
                         (:STRUCT RECT) RECT (:STRUCT M44) TRANSFORM
                         (:STRUCT RECT)))
 (DEFUN RECTUNION (RECT OTHER)
   (CFFI:FOREIGN-FUNCALL "wrapSPRect3DUnion" (:STRUCT RECT) RECT (:STRUCT RECT)
                         OTHER (:STRUCT RECT)))
 (DEFUN AFFGETROT (TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPAffineTransform3DGetRotation" (:STRUCT M43)
                         TRANSFORM (:STRUCT ROT)))
 (DEFUN PROJGETROT (TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPProjectiveTransform3DGetRotation" (:STRUCT M44)
                         TRANSFORM (:STRUCT ROT)))
 (DEFUN ROTINV (ROTATION)
   (CFFI:FOREIGN-FUNCALL "wrapSPRotation3DInverse" (:STRUCT ROT) ROTATION
                         (:STRUCT ROT)))
 (DEFUN ROT (ANGLE AXIS)
   (CFFI:FOREIGN-FUNCALL "wrapSPRotation3DMake" :DOUBLE ANGLE
                         (:STRUCT ROT-AXIS) AXIS (:STRUCT ROT)))
 (DEFUN LOOKAT->ROT (FORWARD UP)
   (CFFI:FOREIGN-FUNCALL "wrapSPRotation3DMakeLookAt" (:STRUCT VEC) FORWARD
                         (:STRUCT VEC) UP (:STRUCT ROT)))
 (DEFUN EUL->ROT (EULERANGLES)
   (CFFI:FOREIGN-FUNCALL "wrapSPRotation3DMakeWithEulerAngles" (:STRUCT EUL)
                         EULERANGLES (:STRUCT ROT)))
 (DEFUN QUAT->ROT (QUATERNION)
   (CFFI:FOREIGN-FUNCALL "wrapSPRotation3DMakeWithQuaternion" (:STRUCT V4)
                         QUATERNION (:STRUCT ROT)))
 (DEFUN ROTSLERP (FROM TO TP)
   (CFFI:FOREIGN-FUNCALL "wrapSPRotation3DSlerp" (:STRUCT ROT) FROM
                         (:STRUCT ROT) TO :DOUBLE TP (:STRUCT ROT)))
 (DEFUN ROTSLERPLONGEST (FROM TO TP)
   (CFFI:FOREIGN-FUNCALL "wrapSPRotation3DSlerpLongest" (:STRUCT ROT) FROM
                         (:STRUCT ROT) TO :DOUBLE TP (:STRUCT ROT)))
 (DEFUN ROTSPLINE (R0 R1 R2 R3 TP)
   (CFFI:FOREIGN-FUNCALL "wrapSPRotation3DSpline" (:STRUCT ROT) R0
                         (:STRUCT ROT) R1 (:STRUCT ROT) R2 (:STRUCT ROT) R3
                         :DOUBLE TP (:STRUCT ROT)))
 (DEFUN ROTSWING (ROTATION TWISTAXIS)
   (CFFI:FOREIGN-FUNCALL "wrapSPRotation3DSwing" (:STRUCT ROT) ROTATION
                         (:STRUCT ROT-AXIS) TWISTAXIS (:STRUCT ROT)))
 (DEFUN ROTTWIST (ROTATION TWISTAXIS)
   (CFFI:FOREIGN-FUNCALL "wrapSPRotation3DTwist" (:STRUCT ROT) ROTATION
                         (:STRUCT ROT-AXIS) TWISTAXIS (:STRUCT ROT)))
 (DEFUN VECROTTOVEC (VECTOR OTHER)
   (CFFI:FOREIGN-FUNCALL "wrapSPVector3DRotationToVector" (:STRUCT VEC) VECTOR
                         (:STRUCT VEC) OTHER (:STRUCT ROT)))
 (DEFUN ROTGETAXIS (ROTATION)
   (CFFI:FOREIGN-FUNCALL "wrapSPRotation3DGetAxis" (:STRUCT ROT) ROTATION
                         (:STRUCT ROT-AXIS)))
 (DEFUN ROTAXIS (X Y Z)
   (CFFI:FOREIGN-FUNCALL "wrapSPRotationAxis3DMake" :DOUBLE X :DOUBLE Y :DOUBLE
                         Z (:STRUCT ROT-AXIS)))
 (DEFUN VEC->ROTAXIS (XYZ)
   (CFFI:FOREIGN-FUNCALL "wrapSPRotationAxis3DMakeWithVector" (:STRUCT VEC) XYZ
                         (:STRUCT ROT-AXIS)))
 (DEFUN SPOSECAT (LHS RHS)
   (CFFI:FOREIGN-FUNCALL "wrapSPScaledPose3DConcatenation" (:STRUCT SPOSE) LHS
                         (:STRUCT SPOSE) RHS (:STRUCT SPOSE)))
 (DEFUN SPOSEGETINV (SCALEDPOSE)
   (CFFI:FOREIGN-FUNCALL "wrapSPScaledPose3DGetInverse" (:STRUCT SPOSE)
                         SCALEDPOSE (:STRUCT SPOSE)))
 (DEFUN M44->SPOSE (MATRIX)
   (CFFI:FOREIGN-FUNCALL "wrapSPScaledPose3DMakeWith4x4Matrix" (:STRUCT M44)
                         MATRIX (:STRUCT SPOSE)))
 (DEFUN AFF->SPOSE (TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPScaledPose3DMakeWithAffineTransform"
                         (:STRUCT M43) TRANSFORM (:STRUCT SPOSE)))
 (DEFUN PROJ->SPOSE (TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPScaledPose3DMakeWithProjectiveTransform"
                         (:STRUCT M44) TRANSFORM (:STRUCT SPOSE)))
 (DEFUN SPOSEROT (SCALEDPOSE ROTATION)
   (CFFI:FOREIGN-FUNCALL "wrapSPScaledPose3DRotate" (:STRUCT SPOSE) SCALEDPOSE
                         (:STRUCT ROT) ROTATION (:STRUCT SPOSE)))
 (DEFUN SPOSEROTBYQUAT (SCALEDPOSE QUATERNION)
   (CFFI:FOREIGN-FUNCALL "wrapSPScaledPose3DRotateByQuaternion" (:STRUCT SPOSE)
                         SCALEDPOSE (:STRUCT V4) QUATERNION (:STRUCT SPOSE)))
 (DEFUN SPOSETRANSLATE (SCALEDPOSE OFFSET)
   (CFFI:FOREIGN-FUNCALL "wrapSPScaledPose3DTranslate" (:STRUCT SPOSE)
                         SCALEDPOSE (:STRUCT VEC) OFFSET (:STRUCT SPOSE)))
 (DEFUN AFFGETSCALE (TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPAffineTransform3DGetScale" (:STRUCT M43)
                         TRANSFORM (:STRUCT SIZE)))
 (DEFUN PROJGETSCALE (TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPProjectiveTransform3DGetScale" (:STRUCT M44)
                         TRANSFORM (:STRUCT SIZE)))
 (DEFUN SIZEAPPLYAFF (SIZE TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPSize3DApplyAffineTransform" (:STRUCT SIZE) SIZE
                         (:STRUCT M43) TRANSFORM (:STRUCT SIZE)))
 (DEFUN SIZEAPPLYPROJ (SIZE TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPSize3DApplyProjectiveTransform" (:STRUCT SIZE)
                         SIZE (:STRUCT M44) TRANSFORM (:STRUCT SIZE)))
 (DEFUN SIZEINTERSECTION (SIZE OTHER)
   (CFFI:FOREIGN-FUNCALL "wrapSPSize3DIntersection" (:STRUCT SIZE) SIZE
                         (:STRUCT SIZE) OTHER (:STRUCT SIZE)))
 (DEFUN PT->SIZE (POINT)
   (CFFI:FOREIGN-FUNCALL "wrapSPSize3DMakeWithPoint" (:STRUCT PT) POINT
                         (:STRUCT SIZE)))
 (DEFUN VEC->SIZE (XYZ)
   (CFFI:FOREIGN-FUNCALL "wrapSPSize3DMakeWithVector" (:STRUCT VEC) XYZ
                         (:STRUCT SIZE)))
 (DEFUN SIZEROT (SIZE ROTATION)
   (CFFI:FOREIGN-FUNCALL "wrapSPSize3DRotate" (:STRUCT SIZE) SIZE (:STRUCT ROT)
                         ROTATION (:STRUCT SIZE)))
 (DEFUN SIZEROTBYQUAT (SIZE QUATERNION)
   (CFFI:FOREIGN-FUNCALL "wrapSPSize3DRotateByQuaternion" (:STRUCT SIZE) SIZE
                         (:STRUCT V4) QUATERNION (:STRUCT SIZE)))
 (DEFUN SIZESCALEBY (SIZE X Y Z)
   (CFFI:FOREIGN-FUNCALL "wrapSPSize3DScaleBy" (:STRUCT SIZE) SIZE :DOUBLE X
                         :DOUBLE Y :DOUBLE Z (:STRUCT SIZE)))
 (DEFUN SIZESCALEBYSIZE (SIZE SCALE)
   (CFFI:FOREIGN-FUNCALL "wrapSPSize3DScaleBySize" (:STRUCT SIZE) SIZE
                         (:STRUCT SIZE) SCALE (:STRUCT SIZE)))
 (DEFUN SIZESCALEUNIFORM (SIZE SCALE)
   (CFFI:FOREIGN-FUNCALL "wrapSPSize3DScaleUniform" (:STRUCT SIZE) SIZE :DOUBLE
                         SCALE (:STRUCT SIZE)))
 (DEFUN SIZEUNAPPLYAFF (SIZE TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPSize3DUnapplyAffineTransform" (:STRUCT SIZE)
                         SIZE (:STRUCT M43) TRANSFORM (:STRUCT SIZE)))
 (DEFUN SIZEUNAPPLYPROJ (SIZE TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPSize3DUnapplyProjectiveTransform"
                         (:STRUCT SIZE) SIZE (:STRUCT M44) TRANSFORM
                         (:STRUCT SIZE)))
 (DEFUN SIZEUNION (SIZE OTHER)
   (CFFI:FOREIGN-FUNCALL "wrapSPSize3DUnion" (:STRUCT SIZE) SIZE (:STRUCT SIZE)
                         OTHER (:STRUCT SIZE)))
 (DEFUN CARTPT->SPH (XYZ)
   (CFFI:FOREIGN-FUNCALL "wrapSPSphericalCoordinates3DMakeWithCartesianPoint"
                         (:STRUCT PT) XYZ (:STRUCT SPH)))
 (DEFUN CARTVEC->SPH (XYZ)
   (CFFI:FOREIGN-FUNCALL "wrapSPSphericalCoordinates3DMakeWithCartesianVector"
                         (:STRUCT VEC) XYZ (:STRUCT SPH)))
 (DEFUN AFFGETTRAN (TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPAffineTransform3DGetTranslation" (:STRUCT M43)
                         TRANSFORM (:STRUCT VEC)))
 (DEFUN PROJGETTRAN (TRANSFORM)
   (CFFI:FOREIGN-FUNCALL "wrapSPProjectiveTransform3DGetTranslation"
                         (:STRUCT M44) TRANSFORM (:STRUCT VEC)))
 (DEFUN VECCROSSPRODUCT (X Y)
   (CFFI:FOREIGN-FUNCALL "wrapSPVector3DCrossProduct" (:STRUCT VEC) X
                         (:STRUCT VEC) Y (:STRUCT VEC)))
 (DEFUN PT->VEC (POINT)
   (CFFI:FOREIGN-FUNCALL "wrapSPVector3DMakeWithPoint" (:STRUCT PT) POINT
                         (:STRUCT VEC)))
 (DEFUN ROTAXIS->VEC (AXIS)
   (CFFI:FOREIGN-FUNCALL "wrapSPVector3DMakeWithRotationAxis"
                         (:STRUCT ROT-AXIS) AXIS (:STRUCT VEC)))
 (DEFUN SIZE->VEC (SIZE)
   (CFFI:FOREIGN-FUNCALL "wrapSPVector3DMakeWithSize" (:STRUCT SIZE) SIZE
                         (:STRUCT VEC)))
 (DEFUN SPH->VEC (COORDS)
   (CFFI:FOREIGN-FUNCALL "wrapSPVector3DMakeWithSphericalCoordinates"
                         (:STRUCT SPH) COORDS (:STRUCT VEC)))
 (DEFUN VECNORM (X)
   (CFFI:FOREIGN-FUNCALL "wrapSPVector3DNormalize" (:STRUCT VEC) X
                         (:STRUCT VEC)))
 (DEFUN VECPROJ (X Y)
   (CFFI:FOREIGN-FUNCALL "wrapSPVector3DProject" (:STRUCT VEC) X (:STRUCT VEC)
                         Y (:STRUCT VEC)))
 (DEFUN VECREFL (X Y)
   (CFFI:FOREIGN-FUNCALL "wrapSPVector3DReflect" (:STRUCT VEC) X (:STRUCT VEC)
                         Y (:STRUCT VEC)))
 (DEFUN VECROT (VECTOR ROTATION)
   (CFFI:FOREIGN-FUNCALL "wrapSPVector3DRotate" (:STRUCT VEC) VECTOR
                         (:STRUCT ROT) ROTATION (:STRUCT VEC)))
 (DEFUN VECROTBYQUAT (VECTOR QUATERNION)
   (CFFI:FOREIGN-FUNCALL "wrapSPVector3DRotateByQuaternion" (:STRUCT VEC)
                         VECTOR (:STRUCT V4) QUATERNION (:STRUCT VEC)))
 (DEFUN VECSCALEBY (VECTOR X Y Z)
   (CFFI:FOREIGN-FUNCALL "wrapSPVector3DScaleBy" (:STRUCT VEC) VECTOR :DOUBLE X
                         :DOUBLE Y :DOUBLE Z (:STRUCT VEC)))
 (DEFUN VECSCALEBYSIZE (VECTOR SCALE)
   (CFFI:FOREIGN-FUNCALL "wrapSPVector3DScaleBySize" (:STRUCT VEC) VECTOR
                         (:STRUCT SIZE) SCALE (:STRUCT VEC)))
 (DEFUN VECSCALEUNIFORM (VECTOR SCALE)
   (CFFI:FOREIGN-FUNCALL "wrapSPVector3DScaleUniform" (:STRUCT VEC) VECTOR
                         :DOUBLE SCALE (:STRUCT VEC)))
 (DEFUN AFFSETTRAN (TRANSFORM OFFSET)
   (CFFI:FOREIGN-FUNCALL "wrapSPAffineTransform3DSetTranslation" :POINTER
                         TRANSFORM (:STRUCT VEC) OFFSET :VOID))
 (DEFUN PROJSETTRAN (TRANSFORM OFFSET)
   (CFFI:FOREIGN-FUNCALL "wrapSPProjectiveTransform3DSetTranslation" :POINTER
                         TRANSFORM (:STRUCT VEC) OFFSET :VOID))
 (DEFUN RECTGETCORNERPTS (RECT POINTS)
   (CFFI:FOREIGN-FUNCALL "wrapSPRect3DGetCornerPoints" (:STRUCT RECT) RECT
                         :POINTER POINTS :VOID))
 (DEFUN ROTSETANG (ROTATION ANGLE)
   (CFFI:FOREIGN-FUNCALL "wrapSPRotation3DSetAngle" :POINTER ROTATION :DOUBLE
                         ANGLE :VOID))
 (DEFUN ROTSETAXIS (ROTATION AXIS)
   (CFFI:FOREIGN-FUNCALL "wrapSPRotation3DSetAxis" :POINTER ROTATION
                         (:STRUCT ROT-AXIS) AXIS :VOID)))
