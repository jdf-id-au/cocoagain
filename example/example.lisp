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


(defclass buffer-handle () ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ Metal buffer
  ((cocoa-ref :accessor ns::cocoa-ref :initform nil :documentation "Pointer to MTLBuffer")
   (cap :initarg :cap :reader cap)
   (len :initform 0 :accessor len)
   (mode :initarg :mode :reader mode :initform
         (+ mtl::ResourceStorageModeManaged ; TODO 2025-08-31 22:20:23 differently on +arm64 ?
            mtl::ResourceCPUCacheModeDefaultCache)))) ; vs ...write-combined

(defmethod contents ((self buffer-handle))
  (ns:protect (mtk::buffer-contents self)
              "Failed to access buffer contents. Check storage mode?"))

(defmethod dump ((self buffer-handle) as)
  (loop for i below (len self)
        collect (cffi:mem-ref (contents self) as i)))

(defmethod initialize-instance :after ((self buffer-handle) &key device)
  (setf (ns::cocoa-ref self) ; TODO 2025-08-31 22:13:14 if setf allowed here could make :reader only
        (ns:protect
         ;; FIXME 2025-08-17 21:51:28 curious about when this is freed
         ;; ...need autorelease?
         (progn
           ;;(format t "Making new buffer of size ~a.~%" (cap self))
           (mtk::new-buffer device (cap self) (mode self)))
         "Failed to allocate buffer.")))

(defun parse-vf (vf)
  (let* ((name (symbol-name vf)))
    (assert (uiop:string-prefix-p "VERTEXFORMAT" name))
    (let* ((rest (subseq name 12))
           (ty (cond ((uiop:string-prefix-p "UCHAR" rest) :uint8)
                     ((uiop:string-prefix-p "CHAR" rest) :int8)
                     ((uiop:string-prefix-p "USHORT" rest) :uint16)
                     ((uiop:string-prefix-p "SHORT" rest) :int16)
                     ;; TODO half
                     ((uiop:string-prefix-p "FLOAT" rest) :float)
                     ((uiop:string-prefix-p "UINT" rest) :uint32)
                     ((uiop:string-prefix-p "INT" rest) :int32)))
           (sz (case ty ((:uint8 :int8) 1) ; bytes
                     ((:uint16 :int16) 2)
                     (t 4)))
           (n (cond ((search "2" rest) 2) ; count
                    ((search "3" rest) 3)
                    ((search "4" rest) 4)
                    (t 1)))
           (norm (uiop:string-suffix-p rest "NORMALIZED"))) ; quirky but understandable arg order
      (values rest ty sz n norm))))

;;(parse-vf 'mtl::VertexFormatFloat2Normalized)

(defun stride (vf)
  (multiple-value-bind (rest ty sz n norm) (parse-vf vf)
    (declare (ignorable rest ty norm))
    (* sz n)))

;; TODO 2025-09-07 06:45:42 config non-asserting for max speed? profile first
(defmacro make-putfn (vf)
  "Take constant like mtl::VertexFormatFloat3 and make (put-float3s buf vals &key offset)."
  (multiple-value-bind (name ty sz n norm) (parse-vf vf)
    (declare (ignorable norm))
    (let* ((fn (intern (format nil "PUT-~aS" name))) ; upper case to prevent symbol name escaping...
           (fts (cffi:foreign-type-size ty))
           (fta (cffi:foreign-type-alignment ty))
           (v (case ty (:float `(coerce (elt vals i) 'single-float))
                    (t `(elt vals i)))))
      `(defmethod ,fn ((self buffer-handle) vals
                       &key (offset 0) ; byte offset
                         (update-len t))
         (let* ((max-size (- (cap self) offset))
                (incoming-n (array-total-size vals))
                (incoming-size (* ,sz incoming-n)))
           ;; TODO 2025-09-17 21:19:35 consider when could be legit
           (assert (<= offset (len self)) (offset) "Offset ~a beyond current len ~a." offset (len self))
           ;; TODO 2025-09-06 23:21:12 verify that this would actually be a problem:
           ;; Allows internal updates; different to vertex descriptor's offset which has alignment of 4.
           (assert (zerop (mod offset ,fta)) nil "Wrong alignment: ~a not divisible by ~a." offset ,fta)
           (assert (zerop (mod incoming-n ,n)) nil "Wrong number of values: ~a not divisible by ~a." incoming-n ,n)
           (assert (<= incoming-size max-size) nil "Too much data ~a vs ~a bytes available." incoming-size max-size)
           (dotimes (i incoming-n)
             (setf (cffi:mem-ref (contents self) ,ty (+ offset (* i ,fts))) ,v))
           (when update-len (setf (len self) (+ offset incoming-size)))
           ;; TODO 2025-09-07 06:43:31 later synchronizeResource from GPU->CPU (compute shaders...)
           ;; for CPU->GPU copy when managed memory:
           (ns:objc self "didModifyRange:" (:struct ns:range) (ns:range offset (+ offset incoming-size))))))))

;; eval-when ?
(make-putfn mtl::VertexFormatFloat3)
;; (make-putfn mtl::VertexFormatShort)

(defclass mtk-context () ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ Context manager
  ((view :initarg :view :reader view) ; pass in to allow more obvious initialisation...
   (lock :initform (bt:make-recursive-lock) :reader lock)
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
    (when previous (format t "Overwriting pipeline ~a~%" label)) ; TODO 2025-08-31 15:09:38 destruct harmoniously, here vs on passed-out second value?
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

(defmethod pipeline-state ((self mtk-context) &optional (pipeline-label :default))
  ;; FIXME 2025-08-31 22:58:37 This will fail if vbs not configured yet. (e.g. draw too early?)
  ;; Should patch through objc workaround of make-render-pipeline-state's error to CL 

  ;; TODO 2025-08-30 15:20:02 maybe more efficient/non-allocating key fn...?
  (let* ((rp (render-pipeline self pipeline-label))
         (view (view self))
         (key (cons (ns::id view) pipeline-label))
         (cache (%states rp))
         (cached (gethash key cache)))
    (if cached cached
        (progn
          ;;(format t "About to make pipeline state for ~a in ~a.~%" (label self) view)
          (setf (gethash key cache)
                (mtk::make-render-pipeline-state view (ns::cocoa-ref rp)))))))

(defmethod setup ((self mtk-context))
  ;; TODO 2025-09-17 21:55:37 file watch https://stackoverflow.com/a/11372441/780743
  (bt:with-recursive-lock-held ((lock self))
    (let* ((shader-source (uiop:read-file-string "example/example.metal"))
           (device (ns:device (view self)))
           ;; Uncompilable shader would be described in
           ;; sly-inferior-lisp log from objc until I get lisp impl
           ;; working. Doesn't kill repl/runtime, just Continue.
           (library (mtk::make-library device shader-source))
           ;; TODO 2025-09-17 21:51:28 stop drawing first?
           (pd (add-render-pipeline
                self (make-instance 'render-pipeline
                                    :library library
                                    :vertex "vertex_ndc" :fragment "fragment_lsd")))
           (vd (vertex-descriptor pd))
           (pdp (add-render-pipeline
                 self (make-instance 'render-pipeline
                                     :label :point ; not :default
                                     :library library :vertex "vertex_point" :fragment "fragment_point")))
           (srd (stride 'mtl::VertexFormatFloat3))
           (vb (make-instance 'buffer-handle :device device :cap (* 1024 srd)))
           (vb-index (add-vertex-buffer self vb)))

      (mtk::set-vertex-descriptor-attribute vd 1 mtl::VertexFormatFloat3 :index vb-index)
      (mtk::set-vertex-descriptor-layout vd vb-index srd)
      (mtk::set-vertex-descriptor pd vd)
      (mtk::set-vertex-descriptor pdp vd)
      
      (mtk::set-color-attachment-pixel-format pd 0)
      (mtk::set-color-attachment-pixel-format pdp 0)
      
      (progn
        (mtk::set-color-attachment-blending-enabled pdp 0 T)
        (mtk::set-color-attachment-blend-factor pdp 0 :source :rgb mtl::BlendFactorSourceAlpha)
        (mtk::set-color-attachment-blend-factor pdp 0 :dest :rgb mtl::BlendFactorSourceAlpha))
      
      (put-float3s vb #( 0.0  0.9  0.0
                        -0.7 -0.8  0.0
                        1.0 -1.0  0.0)))))

(progn ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ Draw
  ;; for call frequency see https://stackoverflow.com/a/71655894/780743
  (defmethod ns:draw ((self ns:mtk-view))
    (let ((ctx (ns:context self)))
      (bt:with-recursive-lock-held ((lock ctx))
        (let* ((vb (elt (vertex-buffers ctx) 1)) ; vertex buffers index
               (len (/ (len vb) 3 4))
               ;; TODO 2025-08-30 11:29:14 could automate translation of pointer?
               (vb-ref (ns::cocoa-ref vb))
               (cb (mtk::command-buffer (command-queue ctx)))
               (rp (mtk::render-pass-descriptor self)) ;; MTKView clearColor
               
               (ps (pipeline-state ctx))
               (psp (pipeline-state ctx :point)))
          ;;(format t "len ~a ~%" (len vb))
          (let* ((ce (mtk::render-command-encoder cb rp)))
            (unwind-protect
                 (progn
                   (mtk::set-vertex-buffer ce vb-ref)
                   (mtk::set-render-pipeline-state ce ps)
                   (mtk::draw-primitives ce mtl::PrimitiveTypeTriangle 0 len)
                   (mtk::set-render-pipeline-state ce psp)
                   (mtk::draw-primitives ce mtl::PrimitiveTypePoint 0 len))
              (mtk::end-encoding ce)))
          (mtk::present-drawable cb (mtk::drawable self))
          (mtk::commit cb)))))
  (display-all))

(defvar *last-ctx* nil) ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ Event loop
(ns:with-event-loop (:waitp t)
  (let* ((win (make-instance 'ns:window
                             :rect (ns:in-screen-rect (ns:rect 0 1000 720 450))
                             :title "MetalKit demo"))
         (view (make-instance 'ns:mtk-view))
         (ctx (make-instance 'mtk-context :view view)))
    (setup ctx)
    (setf *last-ctx* ctx)
    ;; NB 2025-08-31 09:02:51 MTKView defaults to timer-redraw 60fps, alts available
    (setf (ns:content-view win) view)
    (ns:window-show win)))

#+nil(progn ;
       (defun scale-cursor (loc dim)
         "Scale cursor to [-1,1]" ; could DISASSEMBLE and optimise...
         (coerce (1- (* (/ loc dim) 2)) 'single-float))

       ;; TODO 2025-08-30 11:27:15 double check clos method calling
       (defmethod ns::mouse-dragged ((self ns:mtk-view) event location-x location-y)
         (let* ((x (scale-cursor location-x (ns:width self)))
                (y (scale-cursor location-y (ns:height self)))
                (vb (elt (vertex-buffers (ns:context self)) 0)))
           ;; NB reader macro for vector #() seemed to quote contents
           (put-float3s vb (vector x y 0.0
                                   -1.0 -1.0 0.0
                                   (- x) (- y) 0.0)
                        :offset (len vb))))

       ;; TODO 2025-08-30 20:09:03 remove-method?
       (defmethod ns::mouse-down ((self ns::mtk-view) event location-x location-y)
         (declare (ignorable event location-x location-y)))

       ;; NB 2025-09-17 20:54:45 Currently needs manual timer
      ;; invalidation for reliable window closing. See
      ;; core-foundation.lisp.
      (let ((vb (elt (vertex-buffers *last_ctx*) 0))
            (make-instance
              ;; NB 2025-09-01 21:57:28 timer behaves differently ~1000x arm64 vs x86-64
              'ns:timer :interval 0.0166 :timer-fn
              (lambda (seconds)
                (put-float3s vb (vector (sin (/ seconds 2)) -0.8 0.0)
                             :offset srd)))))

      ;; FIXME 2025-09-17 22:18:56 re-setup caused
      ;; -[MTLVertexDescriptorInternal validateWithVertexFunction:error:renderPipelineDescriptor:]:904: failed assertion `Vertex Descriptor Validation Attribute at index 1 references a buffer at index 1 that has no stride.
      ;; ...try `help` and `back[trace]`
      (setup *last-ctx*)

      (vertex-buffers *last-ctx*)
      (dump (elt (vertex-buffers *last-ctx*) 2) :float)
      )

#+nil(uiop/os:getcwd) ; depends on from which buffer sly was started

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
