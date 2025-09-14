;; /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/spatial/

(in-package :spatial)

;; TODO 2025-09-09 21:48:29 or could use cffi:with-foreign-pointer and
;; constant size to stack-allocate...

;; ...or just straightforward cffi:foreign-funcall using vector translators
;; want C side to have value params (not pointers) so passed by copy

;; TODO 2025-09-09 21:49:05 also consider cffi:with-pointer-to-vector-data and
;; (make-array 20 :element-type 'double-float)

;; ───────────────────────────────────────────────────────────────── Wrap method
;; Too many identically shaped types...
(ut:bidi-ffi v4d a :double b :double c :double d :double)
(ut:bidi-ffi v8d a :double b :double c :double d :double
             e :double f :double g :double h :double)
(ut:bidi-ffi m4x4d ; column-major layout; math convention: row, column
             aa :double ab :double ac :double ad :double
             ba :double bb :double bc :double bd :double
             ca :double cb :double cc :double cd :double
             da :double db :double dc :double dd :double)

;; TODO 2025-09-14 16:23:04 macro for calling wrap_spatial.m fns with ?lisp vectors ?or just structs above
(defmacro spatial (fname &rest args)
  `(cffi:foreign-funcall ,(format nil "wrapSP~a" fname)
                         ,@args))

;; TODO 2025-09-14 22:16:56 cache fn lookup/go into cffi weeds
;;(spatial "AngleAsin" :double 1.0d0 :double) ; works
;;(spatial "Rect3DGetCenter" (:struct v8d) (v8d 0.0d0 0.0d0 0.0d0 0.0d0 1.0d0 1.0d0 1.0d0 0.0d0) (:struct v4d)) ; (npe if misspelled)

;;TODO 2025-09-14 22:16:48 lispified names e.g.:
(defun rect-3d-get-center (origin-size)
  (spatial "Rect3DGetCenter" (:struct v8d) origin-size (:struct v4d)))

;; TODO 2025-09-14 22:20:44 extract (simplified) types too
;; (rect-3d-get-center (v8d 0.0d0 0.0d0 0.0d0 0.0d0 1.0d0 1.0d0 1.0d0 0.0d0))

;; TODO 2025-09-14 21:57:33 method for stacking calls without pointless translation

#+nil(progn ; ────────────────────────────────────────────────── Indirect method
       (ql:quickload :cocoagain)
       (in-package :spatial)

       ;; Reload lib... doesn't seem to reload? despite claim/docs
       (cffi:load-foreign-library
        (let ((path 
                (concatenate 'string
                             (namestring (asdf:system-source-directory :cocoagain))
                             "libcocoagain.dylib")))
          (format t "~%Loading ~a~%" path)
          path))

       (ut:with-arena (scratch 100 :double)
         (let* ((scale (ut:put scratch :double #(2 2 2 0)))
                (rot (ut:put scratch :double #(0 0 0 1)))
                (trans (ut:put scratch :double #(3 3 3 0))) ; ineffective?
                (aff (ut:alloc scratch 16 :double))
                (vec-in (ut:put scratch :double #(1 1 1 0)))
                (vec-out (ut:alloc scratch 4 :double)))
           #+nil(format t "Rotation is valid? ~a~%"
                   (cffi:foreign-funcall "indirectSPRotation3DIsValid"
                                         :pointer rot
                                         :bool))
           ;;(format t "About to try aff make with ~a ~a ~a ~a" scale rot trans aff)
           (cffi:foreign-funcall "indirectSPAffineTransform3DMake"
                                 :pointer scale
                                 :pointer rot
                                 :pointer trans
                                 :pointer aff
                                 :size)
           ;;(format t "...success~%")
           (format t "aff ~a~%" (ut:fetch aff 16 :double))
           ;;(format t "About to try aff trans with ~a ~a ~a" vec-in aff vec-out)
           (cffi:foreign-funcall "indirectSPVector3DApplyAffineTransform"
                                 :pointer vec-in
                                 :pointer aff
                                 :pointer vec-out
                                 :size)
           ;;(format t "...success~%")
           (ut:fetch vec-out 4 :double)))
       )
;;(array-element-type (make-array 3 :element-type 'single-float :initial-contents '(0.0 1.0 1.2)))
