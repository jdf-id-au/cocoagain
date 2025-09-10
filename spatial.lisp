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
(ut:bidi-ffi m4x4d ; column-major layout; math convention: row, column
             aa :double ab :double ac :double ad :double
             ba :double bb :double bc :double bd :double
             ca :double cb :double cc :double cd :double
             da :double db :double dc :double dd :double)

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
