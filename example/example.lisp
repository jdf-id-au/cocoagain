;; C-c C-l sly-load-file cocoagain.asd
(ql:quickload :cocoagain)

(in-package :cocoagain)

;; (maphash #'(lambda (k v) (format t "~S ~S~%" k v)) *view-table*)
;; (redisplay (gethash 0 *view-table*)) ; TODO get `redisplay` working? SIGBUS?

(defun display-all ()
  (loop for v being each hash-value in *view-table*
        do (objc v "display")))

(start-event-loop)

;; ─────────────────────────────────────────────────────────────── Core Graphics

(progn
  (defmethod draw ((self view))
    (let* ((ctx (current-cg-context))
           (w (width self))
           (h (height self))
           (r (rect 0 0 w h)))
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

(with-event-loop (:waitp t)
  (let* ((win (make-instance 'window
                                :rect (in-screen-rect (rect 0 1000 720 450))
                                :title "Core Graphics demo"))
         (view (make-instance 'view)))
    (setf (content-view win) view)
    (window-show win)))

;; ────────────────────────────────────────────────────────────── Metal Tool Kit

(defclass mtk-context () ; TODO 2025-08-16 03:22:06 learn how this changes in complex scenesn
  ((pipeline-state :accessor pipeline-state)
   (command-queue :accessor command-queue))
  (:documentation
   "In-development way of storing all required context such as pipelines,
vertex arrays etc. Could promote to `view.lisp` if ever becomes
general-purpose."))

(defparameter *vertex-data*
  (make-array '(9) :element-type 'single-float
                                       :initial-contents '( 0.0  1.0  0.0
                                                           -1.0 -1.0  0.0
                                                           1.0 -1.0  0.0)))

(defparameter bytes-per-float 4)

(progn
  (defmethod draw ((self mtk-view))
    (cffi:with-foreign-object (fvb :float (array-total-size *vertex-data*))
      ;; TODO 2025-08-16 02:28:20 consider https://www.cliki.net/WAAF-CFFI
      (dotimes (i (array-total-size *vertex-data*))
        (setf (cffi:mem-aref fvb :float i) (aref *vertex-data* i)))
      (let* ((vb (mtl::make-buffer (device self) fvb
                                   (* bytes-per-float (array-total-size *vertex-data*))))
             (cb (mtl::get-command-buffer (command-queue (context self))))
             (rp (mtl::render-pass-descriptor self))
             (ce (mtl::get-render-command-encoder cb rp))
             (ps (pipeline-state (context self))))
        (unwind-protect
             (progn
               (mtl::set-render-pipeline-state ce ps)
               (mtl::set-vertex-buffer ce vb :index 0)
               (mtl::draw-primitives ce mtl:+primitive-type-triangle+ 0 3))
          (mtl::end-encoding ce))
        (mtl::present-drawable cb (mtl::drawable self))
        (mtl::commit cb))))
  (display-all))

(with-event-loop (:waitp t)
  (let* ((win (make-instance 'window
                             :rect (in-screen-rect (rect 0 1000 720 450))
                             :title "Metal Tool Kit demo"))
         (view (make-instance 'mtk-view))
         (ctx (setf (context view) (make-instance 'mtk-context)))
         ;; TODO 2025-08-16 20:03:21 separate out so shader (pipeline etc?) can be hot reloaded
         ;; ... and decide on organisation of resources (pipelines, etc) per view
         (shader-source (uiop:read-file-string "example/example.metal")) ; FIXME 2025-08-16 14:47:17 what sets cwd?
         ;; Uncompilable shader would be described in sly-inferior-lisp log from objc until I get lisp impl working.
         ;; Doesn't kill repl/runtime, just Continue.
         (library (mtl::make-library (device view) shader-source))
         (vertex-fn (mtl::make-function library "vertex_main"))
         (fragment-fn (mtl::make-function library "fragment_main"))
         (pd (mtl::make-render-pipeline-descriptor))
         (vd (mtl::make-vertex-descriptor)))
    (mtl::set-color-attachment-pixel-format pd 0 mtl::+pixel-format-a8-unorm+)
    (mtl::set-vertex-function pd vertex-fn)
    (mtl::set-fragment-function pd fragment-fn)
    (mtl::set-vertex-descriptor-attribute vd 0 mtl:+vertex-format-float3+ 0 0)
    (mtl::set-vertex-descriptor-layout vd 0 (* 3 bytes-per-float) 1 mtl:+vertex-step-function-per-vertex+)
    (mtl::set-vertex-descriptor pd vd)
    (setf (pipeline-state ctx) (mtl::make-render-pipeline-state view pd)
          (command-queue ctx) (mtl::make-command-queue (device view)))
    (setf (content-view win) view)
    (window-show win)))

#+nil(progn
       (defun scale-cursor (loc dim)
         "Scale cursor to [-1,1]" ; could DISASSEMBLE and optimise...
         (coerce (1- (* (/ loc dim) 2)) 'single-float))

       (defmethod mouse-moved ((self base-view) event location-x location-y)
         ;;(format t "~a ~a ~%" location-x location-y)
         (setf (aref *vertex-data* 0) (scale-cursor location-x (width self))
               (aref *vertex-data* 1) (scale-cursor location-y (height self)))))

#+nil(uiop/os:getcwd) ; depends on from which buffer sly was started
