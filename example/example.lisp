;; C-c C-l sly-load-file cocoagain.asd
(ql:quickload :cocoagain)

(defpackage :cocoagain-example (:use cl))
(in-package :cocoagain-example)

;; (maphash #'(lambda (k v) (format t "~S ~S~%" k v)) *view-table*)
;; (redisplay (gethash 0 *view-table*)) ; TODO get `redisplay` working? SIGBUS?

(defun display-all ()
  "Refresh all views."
  (loop for v being each hash-value in ns::*view-table*
        do (ns:objc v "display")))

(ns:start-event-loop)

;; ─────────────────────────────────────────────────────────────── Core Graphics

(progn
  (defmethod ns:draw ((self ns:view))
    (let* ((ctx (ns:current-cg-context))
           (w (ns:width self))
           (h (ns:height self))
           (r (ns:rect 0 0 w h)))
      ;;(format t "bounds ~a~%" (cg:display-bounds 0))
      (cg:set-rgb-fill-color ctx (random 1.0) (random 1.0) (random 1.0))
      (cg:fill-rect ctx r)
      (cg:set-line-width ctx 10.0)
      (cg:set-rgb-stroke-color ctx (random 1.0) 0 0)
      (cg:move-to-point ctx (random w) (random h))
      (cg:add-line-to-point ctx (random w) (random h))
      (cg:add-curve-to-point ctx (random w) (random h)
                             (random w) (random h)
                             (random w) (random h))
      (cg:stroke-path ctx)))
  (display-all))

(ns:with-event-loop (:waitp t)
  (let* ((win (make-instance 'ns:window
                                :rect (ns:in-screen-rect (ns:rect 0 1000 720 450))
                                :title "Core Graphics demo"))
         (view (make-instance 'ns:view)))
    (setf (ns:content-view win) view)
    (ns:window-show win)))

;; ─────────────────────────────────────────────────────────────────── Metal Kit

  #| https://developer.apple.com/library/archive/documentation/Miscellaneous/Conceptual/MetalProgrammingGuide/Cmd-Submiss/Cmd-Submiss.html

(NB Metal 4 is Apple Silicon only... this isn't 4)

shader library [reusable]
  - functions... (e.g. vertex, fragment, ...)

render pipeline x1
  - color attachments... (format)
  - depth attachment (format)
  - vertex function x1
  - fragment function x1
  - vertex descriptor x1
  - command queue x1

vertex descriptor x1
  - attributes... (format, offset, index into buffer)
  - layouts... (stride, step rate, step function)

command queue x1 [reusable, generally thread-safe]
  - command buffers... [transient, autoreleased]
    - command encoders... [transient, autoreleased, one at a time per command buffer]
    -> present drawable
    -> commit

Normally whole frame is rendered using one command buffer. Multiple
command buffers if multithreaded. (MTLParallelRenderCommandEncoder
allows one pass to be split across multiple encoders and threads...)

command endcoder [for given command buffer and render pass]
  - pipeline state x1 [reusable, for given device and render pipeline]
  - depth/stencil states... [reusable]
  - (vertex) buffers... [reusable]
  - textures... [reusable]

   |#

(defclass mtk-context () ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ Context assistance
  ((device :initarg :device :accessor device)
   (render-pipeline :accessor render-pipeline
                    :initform (mtk::make-render-pipeline-descriptor))
   (pipeline-state :accessor pipeline-state)
   (vertex-descriptor :accessor vertex-descriptor
                      :initform (mtk::make-vertex-descriptor))
   (vertex-buffers :accessor vertex-buffers
                   ;; use vector-push-extend
                   :initform (make-array 0 :adjustable t :fill-pointer t))
   (textures :accessor textures
             :initform (make-array 0 :adjustable t :fill-pointer t))
   ;; TODO 2025-08-17 15:57:46 depth stencils, other buffers once understand...
   (command-queue :accessor command-queue))
  (:documentation
   "In-development way of storing all required context such as pipelines,
vertex arrays etc. Could promote to `view.lisp` if ever becomes
general-purpose."))

(defclass buffer-handle () ; maybe track storage mode? ╴╴╴╴╴╴╴╴╴╴╴ Vertex buffer
  ((pointer :accessor pointer :initform nil :documentation "Pointer to MTLBuffer")
   (count :initarg :count :accessor element-count)
   ;; Compare with mtl:+vertex-format-...+ types?
   (padded-element-size :initarg :padded-element-size :accessor padded-element-size)))

(defmethod ns::cocoa-ref ((self buffer-handle))
  (pointer self))

(defmethod size ((self buffer-handle))
  (* (element-count self) (padded-element-size self)))

(defmethod contents ((self buffer-handle))
  (ns:protect (mtk::buffer-contents self)
              "Failed to access buffer contents. Check storage mode?"))

(defmethod initialize-instance :after ((self buffer-handle) &key context)
  "Add new buffer handle to (vertex-buffers context) and configure vertex descriptors."
  ;; TODO 2025-08-17 22:35:02 generalise to handle textures etc as well
  (setf (pointer self)
        (ns:protect
         ;; NB 2025-08-17 21:51:28 curious about when this is freed
         ;; ...need autorelease?
         (progn
           ;;(format t "Making new buffer of size ~a.~%" (size self))
           (mtk::new-buffer (device context) (size self)))
         "Failed to allocate buffer."))
  (let* ((index (vector-push-extend self (vertex-buffers context)))
         (pd (render-pipeline context))
         (vd (vertex-descriptor context)))
    (mtk::set-vertex-descriptor-attribute
     vd index mtk:+vertex-format-float3+ 0 0)
    (mtk::set-vertex-descriptor-layout
     vd index (padded-element-size self) 1 mtk:+vertex-step-function-per-vertex+)
    (mtk::set-vertex-descriptor pd vd))) ; assume safe to repeat with same vd?

(defmethod fill-vertex-buffer ((self mtk-context) index floats)
  ;; TODO 2025-08-17 16:27:37 eventually generalise
  ;; to other numeric types or even cstructs...
  (let* ((bytes-per-float 4)
         (handle (elt (vertex-buffers self) index))
         (buffer-size (size handle))
         (range (ns:range 0 buffer-size))
         )
    (assert (= (* (array-total-size floats) bytes-per-float) buffer-size)
            nil "Wrong data size.")
    (dotimes (i (array-total-size floats))
      (setf (cffi:mem-aref (contents handle) :float i)
            (coerce (elt floats i) 'single-float)))
    ;; (dotimes (i (array-total-size floats)) (format t "~a " (cffi:mem-aref (contents handle) :float i)))
    (format t "About to call didModifyRange ~a~%" (pointer handle))
    ;; for CPU->GPU copy when managed memory:
    ;; FIXME 2025-08-30 10:59:45 oopsie need to pass buffer not contents!
    (ns:objc handle "didModifyRange:" (:struct ns:range) range)
    #+nil(ns:objc "MetalView" "in:at:didModify:"
             :pointer (pointer handle)
             :int 0
             :int buffer-size)
    (format t "Lived to tell the tale.~%")
    ;; later synchronizeResource from GPU->CPU (compute shaders...)
    ))

(progn ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ Draw
  ;; for call frequency see https://stackoverflow.com/a/71655894/780743
  (defmethod ns:draw ((self ns:mtk-view))
    (let* ((ctx (ns:context self))
           (vb (pointer (elt (vertex-buffers ctx) 0))) ; vertex buffers index
           (cb (mtk::command-buffer (command-queue ctx)))
           (rp (mtk::render-pass-descriptor self))
           (ce (mtk::render-command-encoder cb rp))
           (ps (pipeline-state ctx)))
      (unwind-protect
           (progn
             ;;(format t "Setting render pipeline state.~%")     
             (mtk::set-render-pipeline-state ce ps)
             #+nil(format t "Setting vertex buffer ~a ~a.~%" ; seems ok...
                     vb
                     (vertex-buffers ctx))
             (mtk::set-vertex-buffer ce vb :index 0) ; vertex shader arguments index
             ;;(format t "Drawing primitives.~%")
             (mtk::draw-primitives ce mtk:+primitive-type-triangle+ 0 3)
             ;;(format t "Ok.~%")
             )
        ;;(format t "About to end encoding.~%")
        (mtk::end-encoding ce))
      ;;(format t "About to present drawable.~%")
      (mtk::present-drawable cb (mtk::drawable self))
      ;;(format t "About to commit.~%")
      (mtk::commit cb)
      ;;(format t "Committed.~%")
      ))
  (display-all))

(ns:with-event-loop (:waitp t) ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ Event loop
  (let* ((win (make-instance 'ns:window
                             :rect (ns:in-screen-rect (ns:rect 0 1000 720 450))
                             :title "MetalKit demo"))
         (view (make-instance 'ns:mtk-view))
         (ctx (setf (ns:context view) (make-instance 'mtk-context :device (ns:device view))))
         ;; TODO 2025-08-16 20:03:21 separate out so shader (pipeline etc?) can be hot reloaded
         (shader-source (uiop:read-file-string "example/example.metal")) ; FIXME 2025-08-16 14:47:17 what sets cwd?
         ;; Uncompilable shader would be described in sly-inferior-lisp log from objc until I get lisp impl working.
         ;; Doesn't kill repl/runtime, just Continue.
         (library (mtk::make-library (ns:device view) shader-source))
         (vertex-fn (mtk::make-function library "vertex_main"))
         (fragment-fn (mtk::make-function library "fragment_main"))
         (pd (render-pipeline ctx)))
    (mtk::set-color-attachment-pixel-format pd 0 mtk:+pixel-format-a8-unorm+)
    (mtk::set-vertex-function pd vertex-fn)
    (mtk::set-fragment-function pd fragment-fn)
    
    (make-instance 'buffer-handle :count 3 :padded-element-size (* 4 3) :context ctx)
    (fill-vertex-buffer ctx 0 #( 0.0  1.0  0.0
                                -1.0 -1.0  0.0
                                 1.0 -1.0  0.0))

    ;; Do both of these need to be after the other pipeline config has happened?
    (setf (pipeline-state ctx) (mtk::make-render-pipeline-state view pd)
          (command-queue ctx) (mtk::make-command-queue (ns:device view)))
    (setf (ns:content-view win) view)
    (ns:window-show win)))

#+nil(progn ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ Events
       (defun scale-cursor (loc dim)
         "Scale cursor to [-1,1]" ; could DISASSEMBLE and optimise...
         (coerce (1- (* (/ loc dim) 2)) 'single-float))

       (defmethod mouse-moved ((self base-view) event location-x location-y)
         ;;(format t "~a ~a ~%" location-x location-y)
         (setf (aref *vertex-data* 0) (scale-cursor location-x (width self))
               (aref *vertex-data* 1) (scale-cursor location-y (height self)))))

#+nil(uiop/os:getcwd) ; depends on from which buffer sly was started
