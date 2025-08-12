;; load (vs eval) this i.e. asdf:load-system

;; (asdf/driver:with-current-directory nil
;;   (let* ((process-path (concatenate 'string (namestring *default-pathname-defaults*)
;;                                     "sbcl.app/Contents/MacOS/sbcl")))
;;     #+sbcl (sb-posix:setenv "CFProcessPath" process-path 1)))

(asdf:defsystem :cocoagain
  :depends-on (:alexandria
               :cffi
               :cffi-libffi
               :float-features
               :bordeax-threads
               :trivial-main-thread)
  :serial t
  :components ((:file "package")
               (:file "core-foundation")
               (:file "application")))

(pushnew :cocoagain *features*)

