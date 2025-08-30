;; C-c C-l sly-load-file cocoagain.asd
;; FIXME 2025-08-30 20:12:29 ?floating point error when evaling ql + too much?
(ql:quickload :cocoagain)

(defpackage :cocoagain-example (:use cl))
(in-package :cocoagain-example)

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

(defclass render-pipeline () ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ Pipeline
  ((cocoa-ref :reader ns::cocoa-ref :initform (mtk::make-render-pipeline-descriptor))
   (label :reader label :initarg :label) ; init-only
   (vertex-descriptor :accessor vertex-descriptor
                      :initform (mtk::make-vertex-descriptor))
   (%states :accessor %states :initform (make-hash-table))))

;; TODO 2025-08-30 17:23:02 once understood https://cffi.common-lisp.dev/manual/html_node/Tutorial_002dTypes.html
;;(defmethod cffi:translate-to-foreign (pipeline (type render-pipeline)) (ns::cocoa-ref self))

(defmethod initialize-instance :after ((self render-pipeline)
                                       ;; avoid circular init, might be another way
                                       &key table)
  "Make pipeline-label the primary identifier (beyond debugging-convenience intent)."
  (let* ((pipeline-label (label self)))
    ;; TODO 2025-08-30 16:02:26 clarify string lifetime/copying (needs make-ns-string)
    (ns:objc self "setLabel:" :pointer (ns:make-ns-string (symbol-name pipeline-label)))
    (when (gethash pipeline-label table)
      ;; TODO 2025-08-30 16:38:13 deal with old pipeline & states... might race?
      (warn "Not properly cleaning up old ~a pipeline and states." pipeline-label))
    (setf (gethash pipeline-label table) self)) ; manky non-factorable syntax
  )

(defmethod pipeline-state ((self render-pipeline) (view ns:mtk-view))
  ;; TODO 2025-08-30 15:20:02 maybe more efficient/non-allocating key fn...?
  (let* ((key (cons (ns::id view) (label self)))
         (cache (%states self))
         (cached (gethash key cache)))
    (if cached cached
        (setf (gethash key cache)
              (mtk::make-render-pipeline-state view (ns::cocoa-ref self))))))

(defclass mtk-context () ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ Context manager
  ((view :initarg :view :accessor view)
   (%render-pipelines :accessor %render-pipelines :initform (make-hash-table))
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

(defmethod ns:device ((self mtk-context))
  (ns:device (view self)))

(defmethod initialize-instance :after ((self mtk-context) &key (pipeline-label :default))
  (make-instance 'render-pipeline :label pipeline-label
                                  :table (%render-pipelines self))
  (setf (command-queue self) (mtk::make-command-queue (ns:device self))))

(defmethod render-pipeline ((self mtk-context) &optional (pipeline-label :default))
  (gethash pipeline-label (%render-pipelines self)))

;; TODO (defmethod (setf render-pipeline) ...)

(defclass buffer-handle () ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ Vertex buffer
  ;; TODO 2025-08-30 20:02:24 track storage mode
  ((cocoa-ref :accessor ns::cocoa-ref :initform nil :documentation "Pointer to MTLBuffer")
   (count :initarg :count :accessor element-count)
   ;; Compare with mtl:+vertex-format-...+ types?
   (padded-element-size :initarg :padded-element-size :accessor padded-element-size)))

(defmethod size ((self buffer-handle))
  (* (element-count self) (padded-element-size self)))

(defmethod contents ((self buffer-handle))
  (ns:protect (mtk::buffer-contents self)
              "Failed to access buffer contents. Check storage mode?"))

(defmethod initialize-instance :after ((self buffer-handle) &key context)
  "Add new buffer handle to (vertex-buffers context) and configure vertex descriptors."
  ;; TODO 2025-08-17 22:35:02 generalise to handle textures etc as well
  (setf (ns::cocoa-ref self)
        (ns:protect
         ;; NB 2025-08-17 21:51:28 curious about when this is freed
         ;; ...need autorelease?
         (progn
           ;;(format t "Making new buffer of size ~a.~%" (size self))
           (mtk::new-buffer (ns:device context) (size self)))
         "Failed to allocate buffer."))
  (let* ((index (vector-push-extend self (vertex-buffers context)))
         (pd (render-pipeline context :default))
         (vd (vertex-descriptor pd)))
    (mtk::set-vertex-descriptor-attribute
     vd index mtk:+vertex-format-float3+ 0 0)
    (mtk::set-vertex-descriptor-layout
     vd index (padded-element-size self) 1 mtk:+vertex-step-function-per-vertex+)
    (mtk::set-vertex-descriptor pd vd))) ; assume safe to repeat with same vd?

(defmethod fill-vertex-buffer ((self mtk-context) index floats)
  ;; TODO 2025-08-17 16:27:37 eventually generalise
  ;; to other numeric types or even cstructs...
  ;; and maybe update-subrange for big buffers with small updates...
  ;; and maybe accommodate shared buffers on arm64...
  (let* ((bytes-per-float 4)
         (handle (elt (vertex-buffers self) index))
         (buffer-size (size handle))
         (range (ns:range 0 buffer-size)))
    (assert (= (* (array-total-size floats) bytes-per-float) buffer-size)
            nil "Wrong data size.")
    (dotimes (i (array-total-size floats))
      (setf (cffi:mem-aref (contents handle) :float i)
            (coerce (elt floats i) 'single-float)))
    ;; (dotimes (i (array-total-size floats)) (format t "~a " (cffi:mem-aref (contents handle) :float i)))
    ;;(format t "About to call didModifyRange ~a~%" (ns::cocoa-ref handle))
    ;; for CPU->GPU copy when managed memory:
    (ns:objc handle "didModifyRange:" (:struct ns:range) range)
    ;;(format t "Lived to tell the tale.~%")
    ;; later synchronizeResource from GPU->CPU (compute shaders...)
    ))

(progn ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ Draw
  ;; for call frequency see https://stackoverflow.com/a/71655894/780743
  (defmethod ns:draw ((self ns:mtk-view))
    (let* ((ctx (ns:context self))
           ;; TODO 2025-08-30 11:29:14 could automate translation of pointer?
           (vb (ns::cocoa-ref (elt (vertex-buffers ctx) 0))) ; vertex buffers index
           (cb (mtk::command-buffer (command-queue ctx)))
           (rp (mtk::render-pass-descriptor self))
           (ce (mtk::render-command-encoder cb rp))
           (pd (render-pipeline ctx :default))
           (ps (pipeline-state pd self))) ; TODO 2025-08-30 17:34:29 debug/carry changes
      (unwind-protect
           (progn
             ;;(format t "Setting render pipeline state.~%")     
             (mtk::set-render-pipeline-state ce ps)
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
         (ctx (setf (ns:context view) (make-instance 'mtk-context :view view)))
         ;; TODO 2025-08-16 20:03:21 separate out so shader (pipeline etc?) can be hot reloaded
         (shader-source (uiop:read-file-string "example/example.metal"))
         ;; Uncompilable shader would be described in sly-inferior-lisp log from objc until I get lisp impl working.
         ;; Doesn't kill repl/runtime, just Continue.
         ;; TODO 2025-08-30 18:59:00 fold library etc into render-pipeline
         (library (mtk::make-library (ns:device view) shader-source))
         (vertex-fn (mtk::make-function library "vertex_main")) ; TODO 2025-08-30 17:50:21 move to render-pipeline obj
         (fragment-fn (mtk::make-function library "fragment_main"))
         (pd (render-pipeline ctx :default)))
    (mtk::set-color-attachment-pixel-format pd 0 mtk:+pixel-format-a8-unorm+)
    (mtk::set-vertex-function pd vertex-fn)
    (mtk::set-fragment-function pd fragment-fn)
    
    (make-instance 'buffer-handle :count 3 :padded-element-size (* 4 3) :context ctx)
    (fill-vertex-buffer ctx 0 #( 0.0  1.0  0.0
                                -1.0 -1.0  0.0
                                1.0 -1.0  0.0))

    (make-instance 'ns:timer :interval 0.0166 :timer-fn
                   (lambda (seconds)
                     (fill-vertex-buffer
                                    ctx 0
                                    (vector 
                                     0.0  1.0  0.0
                                     (sin seconds) -1.0  0.0
                                     1.0 -1.0  0.0))))
    
    (setf (ns:content-view win) view)
    (ns:window-show win)))

#+nil(progn ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ Events
       (defun scale-cursor (loc dim)
         "Scale cursor to [-1,1]" ; could DISASSEMBLE and optimise...
         (coerce (1- (* (/ loc dim) 2)) 'single-float))

       ;; TODO 2025-08-30 11:27:15 double check clos method calling
       (defmethod ns::mouse-moved ((self ns:mtk-view) event location-x location-y)
         (let* ((x (scale-cursor location-x (ns:width self)))
                (y (scale-cursor location-y (ns:height self)))
                ;; NB reader macro for vector #() seemed to quote contents
                (v (vector x y 0.0
                     -1.0 -1.0 0.0
                     (- x) (- y) 0.0)))
           (fill-vertex-buffer (ns:context self) 0 v)))

       ;; TODO 2025-08-30 20:09:03 remove-method?
       (defmethod ns::mouse-moved ((self ns::mtk-view) event location-x location-y)
         (declare (ignorable event location-x location-y)))
       )

#+nil(uiop/os:getcwd) ; depends on from which buffer sly was started
