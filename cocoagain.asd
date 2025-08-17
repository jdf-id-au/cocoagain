;; after byulparan/cl-nextstep, slower so I can understand

;; load (vs eval) this i.e. asdf:load-system
;; check sly inferior-lisp for ffi hang diagnostics

;; Pretend to be in an .app!
(asdf/driver:with-current-directory nil
  (let* ((process-path (concatenate 'string (namestring *default-pathname-defaults*)
                                    "sbcl.app/Contents/MacOS/sbcl")))
    #+sbcl (sb-posix:setenv "CFProcessPath" process-path 1)))

(asdf:defsystem :cocoagain
  :depends-on (:alexandria
               :cffi
               :cffi-libffi
               
               :float-features
               :bordeaux-threads
               :trivial-main-thread)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "id-map")
               (:file "library")
               (:file "core-foundation")
               (:file "application")
               (:file "window")
               (:file "view")
               (:file "core-graphics")
               (:file "metal")))

(pushnew :cocoagain *features*)
