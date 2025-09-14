(ql:quickload :cl-ppcre)

;;(directory "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/Spatial/*.h")

;; FIXME 2025-09-14 11:46:09 over-matching; something to do with escaping?
;; (let* ((re (ppcre:create-scanner "^(SP[A-Z][a-z]\\S*|void) SP[A-Z][a-z]\\S*\\(.*\\)"
;;                                  :single-line-mode t
;;                                  :multi-line-mode t)))

;;   (ppcre:all-matches-as-strings
;;    re
;;    (uiop:read-file-string #P"/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/Spatial/SPAffineTransform3D.h")))

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
          collect (parse-fn line))))

(defun wrap-params (paramlist)
  (let* ((o (make-string-output-stream)))
    (destructuring-bind (ty param) (car paramlist)
      (format o "~a ~a" ty param))
    (loop for (ty param) in (cdr paramlist)
          do (format o ", ~a ~a" ty param))
    (get-output-stream-string o)))

(defun pointer-passthrough (param)
  (if (eq (elt param 0) #\*) (subseq param 1) param))

(defun wrap-args (paramlist)
  (let* ((o (make-string-output-stream)))
    (destructuring-bind (ty param) (car paramlist)
      (format o "~a" (pointer-passthrough param)))
    (loop for (ty param) in (cdr paramlist)
          do (format o ", ~a" (pointer-passthrough param)))
    (get-output-stream-string o)))

(defun wrap-fns (fns)
  (loop for (ret-type fn-name params) in fns
        do (format t "~a wrap~a(~a) {return ~a(~a);}~%"
                   ret-type
                   fn-name
                   (wrap-params params)
                   fn-name
                   (wrap-args params))))

(wrap-fns (parse-file "spatial_fns.txt"))
