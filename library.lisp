(in-package :cocoagain)

(cffi:load-foreign-library
 (let ((path 
         (concatenate 'string
                      (namestring (asdf:system-source-directory :cocoagain))
                      "libcocoagain.dylib")))
   (format t "~%Loading ~a~%" path)
   path))

(format t "Loaded~%")

;; TODO 2025-08-16 19:35:12 test/fix library reload
