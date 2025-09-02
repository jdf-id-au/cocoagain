(defpackage :cocoagain
  (:documentation "Closely after byulparan/cl-nextstep etc. Reimplemented for learning.")
  (:nicknames :ns)
  (:use :cl :alexandria)
  (:export ; ugh tedious to maintain, also see metal direct export technique
   #:protect
   #:objc
   #:new
   #:release
   #:autorelease
   #:cf-release
   
   #:start-event-loop
   #:with-event-loop

   #:window
   #:content-view
   #:window-show
   
   #:view
   #:current-cg-context
   #:draw
   
   #:mtk-view
   #:device
   #:context   

   #:range
   #:point
   #:size
   #:rect

   #:width
   #:height
   #:in-screen-rect
   
   #:make-ns-string
   #:cf-string-to-lisp

   #:timer
   #:invalidate))

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

(defpackage :metal-kit
  (:nicknames :mtk)
  (:use cl))
