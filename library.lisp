(in-package :cocoagain)

(cffi:load-foreign-library
 (let ((path 
         (concatenate 'string
                      (namestring (asdf:system-source-directory :cocoagain))
                      "libcocoagain.dylib")))
   (format t "Loading ~a~%" path)
   path))

(format t "Loaded~%")

;; NB 2025-08-13 14:14:53 does seem to succeed (although wouldn't print success message)
