;; /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/spatial/

(in-package :spatial)

;; TODO API for efficient use of indirect fns

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
