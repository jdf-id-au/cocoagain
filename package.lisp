(defpackage :cocoagain
  (:documentation "Closely after byulparan/cl-nextstep etc. Reimplemented for learning.")
  (:nicknames :ns)
  (:use :cl :alexandria)
  (:export ; ugh tedious to maintain
   #:protect
   #:objc
   #:new
   #:autorelease
   #:cf-release
   #:with-event-loop
   #:point
   #:size
   #:rect
   #:make-ns-string
   #:cf-string-to-lisp))

(defpackage :core-graphics
  (:nicknames :cg)
  (:use cl)
  (:export
   #:set-rgb-fill-color
   #:set-line-width
   #:fill-rect
   #:set-rgb-stroke-color
   #:move-to-point
   #:line-to-point
   #:add-line-to-point
   #:add-curve-to-point
   #:stroke-path))

(defpackage :mtl
  (:use cl)
  (:export
   #:make-render-pipeline-descriptor
   #:make-render-pipeline-state
   #:make-command-queue))
