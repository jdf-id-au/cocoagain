;; after byulparan/cl-nextstep, slower so I can understand

;; load (vs eval) this i.e. asdf:load-system

;; Why is this needed?
;; (asdf/driver:with-current-directory nil
;;   (let* ((process-path (concatenate 'string (namestring *default-pathname-defaults*)
;;                                     "sbcl.app/Contents/MacOS/sbcl")))
;;     #+sbcl (sb-posix:setenv "CFProcessPath" process-path 1)))

;; ERROR: Interceptors are not working. This may be because AddressSanitizer is loaded too late (e.g. via dlopen). Please launch the executable with:
;; DYLD_INSERT_LIBRARIES=/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/17/lib/darwin/libclang_rt.asan_osx_dynamic.dylib
;; emacs setenv doesn't seem to help

(asdf:defsystem :cocoagain
  :depends-on (:alexandria
               :cffi
               :cffi-libffi
               :float-features
               :bordeaux-threads
               :trivial-main-thread)
  :serial t
  :components ((:file "package")
               (:file "id-map")
               (:file "library")
               (:file "core-foundation")
               (:file "application")
               (:file "window")
               (:file "view")
               (:file "core-graphics")))

(pushnew :cocoagain *features*)
