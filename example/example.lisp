;; C-c C-l sly-load-file cocoagain.asd
;; FIXME 2025-08-30 20:12:29 ?floating point error when evaling ql + too much?
(ql:quickload :cocoagain)

(defpackage :cocoagain-example (:use cl))
(in-package :cocoagain-example)

(defun display-all ()
  "Refresh all views."
  (loop for v being each hash-value in ns::*view-table*
        do (ns:objc v "display")))

(defun real-time ()
  "Arbitrary number rising by 1.0 per wall-clock second."
  (/ (float (get-internal-real-time)) internal-time-units-per-second))

(ns:start-event-loop)

;; ─────────────────────────────────────────────────────────────── Core Graphics
(defstruct draw-ctx randoms timers)

(progn
  (defmethod ns:draw ((self ns:view))
    (let* ((ctx (ns:current-cg-context))
           (dc (ns:context self))
           (re 0)
           (w (ns:width self))
           (h (ns:height self))
           (r (ns:rect 0 0 w h))
           (wobble-max 0.05)) ; displacement
      (flet ((wobble (d) ; wobble using time around stable random fractions
               (let ((rand-frac (elt (draw-ctx-randoms dc) (incf re))))
                 (* d (+ (* (sin (* 2 pi (real-time))) wobble-max) ; period of one second
                         (* rand-frac (- 1.0 (* 2 wobble-max))) ; fit don't clamp
                         wobble-max)))))
        ;;(format t "bounds ~a~%" (cg:display-bounds 0))
        (cg:set-rgb-fill-color ctx (wobble 1.0) (wobble 1.0) (wobble 1.0))
        (cg:fill-rect ctx r)
        (cg:set-line-width ctx 10.0)
        (cg:set-rgb-stroke-color ctx (wobble 1.0) (wobble 1.0) (wobble 1.0))
        (cg:move-to-point ctx (wobble w) (wobble h))
        (cg:add-line-to-point ctx (+ w (wobble (- w))) (wobble h))
        (cg:add-curve-to-point ctx (wobble w) (wobble h)
                               (wobble w) (wobble h)
                               (wobble w) (wobble h))
        (cg:stroke-path ctx))))
  (display-all)) ; redundant refresh when auto-updating...

(defmethod ns:release ((self ns:view))
  ;; FIXME 2025-08-31 11:03:41 doesn't always fire in time
  ;; ...giving "no applicable method for timer-fn when called with nil"
  (format t "Controlled destruction of ~a~%" self)
  ;; FIXME 2025-09-01 21:38:20 double free if already invalidated...
  (mapcar #'ns:invalidate (draw-ctx-timers (ns:context self))))

(ns:with-event-loop (:waitp t)
  (let* ((win (make-instance 'ns:window
                                :rect (ns:in-screen-rect (ns:rect 0 1000 720 450))
                                :title "Core Graphics demo"))
         (randoms (coerce (loop for i below 100 collect (random 1.0)) 'vector))
         (dc (make-draw-ctx :randoms randoms :timers nil))
         (view (make-instance 'ns:view :context dc))
         (timer (make-instance 'ns:timer :interval 0.0166 :timer-fn
                               ;; TODO 2025-08-31 11:31:07 vs setNeedsDisplay ?
                               (lambda (seconds) (ns:objc view "display")))))
    (push timer (draw-ctx-timers dc))
    (setf (ns:content-view win) view)
    (ns:window-show win)))

;; ─────────────────────────────────────────────────────────────────── Metal Kit
(defclass render-pipeline () ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ Pipeline
  ((cocoa-ref :reader ns::cocoa-ref :initform (mtk::make-render-pipeline-descriptor))
   (label :reader label :initarg :label :initform :default) ; init-only
   (vertex-descriptor :accessor vertex-descriptor
                      :initform (mtk::make-vertex-descriptor))
   (%states :accessor %states :initform (make-hash-table))))

;; TODO 2025-08-30 17:23:02 once understood https://cffi.common-lisp.dev/manual/html_node/Tutorial_002dTypes.html
;;(defmethod cffi:translate-to-foreign (pipeline (type render-pipeline)) (ns::cocoa-ref self))

;; TODO 2025-08-31 14:46:02 MTL{Compute,MeshRender,TileRender}PipelineDescriptor... similarities...

(defmethod initialize-instance :after
    ((self render-pipeline) &key library vertex fragment)
  (let* ((pipeline-label (label self)))
    ;; TODO 2025-08-30 16:02:26 clarify string lifetime (needs make-ns-string)
    ;; ...automate passing lisp strings to NSString?
    (ns:objc self "setLabel:" :pointer
             (ns:autorelease (ns:make-ns-string (symbol-name pipeline-label))))
    (if vertex (mtk::set-vertex-function self (mtk::make-function library vertex))
        (error "Need to set vertex function in render pipeline ~a." pipeline-label))
    (when fragment
      (mtk::set-fragment-function self (mtk::make-function library fragment)))))

(defmethod pipeline-state ((self render-pipeline) (view ns:mtk-view))
  ;; FIXME 2025-08-31 22:58:37 This will fail if vbs not configured yet. (e.g. draw too early?)
  ;; Should patch through objc workaround of make-render-pipeline-state's error to CL 

  ;; TODO 2025-08-30 15:20:02 maybe more efficient/non-allocating key fn...?
  (let* ((key (cons (ns::id view) (label self)))
         (cache (%states self))
         (cached (gethash key cache)))
    (if cached cached
        (progn
          ;;(format t "About to make pipeline state for ~a in ~a.~%" (label self) view)
          (setf (gethash key cache)
                (mtk::make-render-pipeline-state view (ns::cocoa-ref self)))))))

(defclass buffer-handle () ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ Metal buffer
  ((cocoa-ref :accessor ns::cocoa-ref :initform nil :documentation "Pointer to MTLBuffer")
   (size :initarg :size :reader size)
   (mode :initarg :mode :reader mode :initform
         (+ mtk:+resource-storage-mode-managed+ ; TODO 2025-08-31 22:20:23 differently on +arm64 ?
            mtk:+resource-cpu-cache-mode-default-cache+)))) ; vs ...write-combined

(defmethod contents ((self buffer-handle))
  (ns:protect (mtk::buffer-contents self)
              "Failed to access buffer contents. Check storage mode?"))

(defmethod initialize-instance :after ((self buffer-handle) &key device)
  (setf (ns::cocoa-ref self) ; TODO 2025-08-31 22:13:14 if setf allowed here could make :reader only
        (ns:protect
         ;; FIXME 2025-08-17 21:51:28 curious about when this is freed
         ;; ...need autorelease?
         (progn
           ;;(format t "Making new buffer of size ~a.~%" (size self))
           (mtk::new-buffer device (size self) (mode self)))
         "Failed to allocate buffer.")))

(defmethod fill-buffer ((self buffer-handle) floats) ; TODO 2025-08-31 22:27:16 other data types!
  ;; TODO 2025-08-17 16:27:37
  ;; maybe update-subrange for big buffers with small updates...
  ;; and maybe accommodate shared buffers on arm64...
  (let* ((buffer-size (size self))
         (incoming-size (array-total-size floats)) 
         (range (ns:range 0 buffer-size)))
    (assert (= (* incoming-size 4) buffer-size) nil "Wrong data size.")
    (dotimes (i incoming-size)
      (setf (cffi:mem-aref (contents self) :float i)
            (coerce (elt floats i) 'single-float)))
    ;; TODO 2025-08-31 22:50:34
    ;; for CPU->GPU copy when managed memory:
    (ns:objc self "didModifyRange:" (:struct ns:range) range)
    ;; later synchronizeResource from GPU->CPU (compute shaders...)
    ))

(defclass mtk-context () ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ Context manager
  ((view :initarg :view :reader view) ; pass in to allow more obvious initialisation...
   (render-pipelines :reader render-pipelines :initform (make-hash-table))
   (vertex-buffers :reader vertex-buffers
                   ;; use vector-push-extend
                   :initform (make-array 0 :adjustable t :fill-pointer t))
   (textures :reader textures :initform (make-array 0 :adjustable t :fill-pointer t))
   ;; TODO 2025-08-17 15:57:46 depth stencils, other buffers once understand...
   (command-queue :accessor command-queue)) ; FIXME 2025-08-31 15:48:44 ideally :reader only
  (:documentation
   "In-development way of storing all required context such as pipelines,
vertex arrays etc. Could promote to `view.lisp` if ever becomes
general-purpose."))

(defmethod ns:device ((self mtk-context))
  (ns:device (view self)))

(defmethod initialize-instance :after ((self mtk-context) &key)
  (setf (ns:context (view self)) self)
  (setf (command-queue self) (mtk::make-command-queue (ns:device self))))

(defmethod add-render-pipeline ((self mtk-context) (p render-pipeline))
  "Add or replace given render pipeline, uniquely by label.
 Second value indicates if replacing."
  (let* ((label (label p))
         (previous (gethash label (render-pipelines self))))
    ;;(when previous) ; TODO 2025-08-31 15:09:38 destruct harmoniously, here vs on passed-out second value?
    (setf (gethash label (render-pipelines self)) p) ; manky non-factorable syntax
    (values p (and previous T))))

(defmethod render-pipeline ((self mtk-context) &optional (pipeline-label :default))
  (gethash pipeline-label (render-pipelines self)))

(defmethod add-vertex-buffer ((self mtk-context) (b buffer-handle))
  "Add a vertex buffer to context if not already there. Returns index."
  (let* ((vbs (vertex-buffers self))
         (index (loop for i below (fill-pointer vbs)
                      if (eq (elt vbs i) b) return i)))
    (assert (not index) nil "Buffer ~a already present in position ~a. Continue to return this index." b index)
    (if index index (vector-push-extend b (vertex-buffers self)))))

(defmethod configure-vertex-buffer ((self mtk-context) buffer-index
                                    &key (pipeline-label :default) ; DRY vs render-pipeline...
                                      (format mtk:+vertex-format-float3+)
                                      (buffer-offset 0)
                                      (argument-index 0)
                                      stride
                                      (step-rate 1)
                                      (step-function mtk:+vertex-step-function-per-vertex+))
  (assert (<= 1 (1+ buffer-index) (fill-pointer (vertex-buffers self)))
          (buffer-index) ; <- opportunity to correct
          "Index ~a doesn't correspond to a vertex buffer." buffer-index)
  (let* ((pd (render-pipeline self pipeline-label))
         (vd (vertex-descriptor pd)))
    (mtk::set-vertex-descriptor-attribute vd buffer-index format buffer-offset argument-index)
    (mtk::set-vertex-descriptor-layout vd buffer-index stride step-rate step-function)
    (mtk::set-vertex-descriptor pd vd)))

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
           (ps (pipeline-state pd self)))
      (unwind-protect
           (progn
             ;;(format t "Setting render pipeline state.~%")     
             (mtk::set-render-pipeline-state ce ps)
             (mtk::set-vertex-buffer ce vb :argument-index 0)
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
         (ctx (make-instance 'mtk-context :view view))
         ;; TODO 2025-08-16 20:03:21 separate out so shader (pipeline etc?) can be hot reloaded
         (shader-source (uiop:read-file-string "example/example.metal"))
         ;; Uncompilable shader would be described in sly-inferior-lisp log from objc until I get lisp impl working.
         ;; Doesn't kill repl/runtime, just Continue.
         (library (mtk::make-library (ns:device view) shader-source))
         (pd (add-render-pipeline
              ctx (make-instance 'render-pipeline :library library
                                                  :vertex "vertex_ndc"
                                                  :fragment "fragment_lsd")))
         ;; TODO 2025-08-31 22:28:36 automate a bit!
         ;; 4 bytes per float, 3 floats per vertex, 3 vertices...
         (vb (make-instance 'buffer-handle :device (ns:device view) :size (* 4 3 3)))
         (vb-index (add-vertex-buffer ctx vb)))
    (configure-vertex-buffer ctx vb-index :stride (* 4 3))
    (mtk::set-color-attachment-pixel-format pd 0
                                            #+x86-64 mtk:+pixel-format-a8-unorm+
                                            #+arm64 mtk:+pixel-format-bgra8-unorm+)
    (mtk::set-color-attachment-blending-enabled pd 0 T)

    (fill-buffer vb #( 0.0  1.0  0.0
                      -1.0 -1.0  0.0
                       1.0 -1.0  0.0))

    (make-instance 'ns:timer :interval 0.0166 :timer-fn
                   (lambda (seconds)
                     (fill-buffer vb (vector 
                                      0.0  1.0  0.0
                                      ;; NB 2025-09-01 21:57:28 weirdly /2000 arm64 vs /2 x86-64
                                      (sin (/ seconds 2000)) -1.0  0.0
                                      1.0 -1.0  0.0))))
    
    ;; NB 2025-08-31 09:02:51 MTKView defaults to timer-redraw 60fps, alts available
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
                (vb (elt (vertex-buffers (ns:context self)) 0)))
           ;; NB reader macro for vector #() seemed to quote contents
           (fill-buffer vb (vector x y 0.0
                                   -1.0 -1.0 0.0
                                   (- x) (- y) 0.0))))

       ;; TODO 2025-08-30 20:09:03 remove-method?
       (defmethod ns::mouse-moved ((self ns::mtk-view) event location-x location-y)
         (declare (ignorable event location-x location-y)))
       )

#+nil(uiop/os:getcwd) ; depends on from which buffer sly was started
