;; C-c C-l sly-load-file cocoagain.asd
;; (ql:quickload '(:alexandria :cffi :cffi-libffi :float-features :bordeaux-threads :trivial-main-thread))
(ql:quickload :cocoagain)

(in-package :cocoagain)

(defconstant +YES+ (objc (alloc "NSNumber") "initWithBool:" :unsigned-char 1 :pointer))
(defconstant +NO+ (objc (alloc "NSNumber") "initWithBool:" :unsigned-char 0 :pointer))

;; (gethash 0 *view-table*)
;; (redisplay (gethash 0 *view-table*))

(defun redisplay-last ()
  (when (not (zerop (hash-table-count *view-table*)))
    (let* ((latest-id (loop for k being each hash-key of *view-table* maximize k))
           (latest-view (gethash latest-id *view-table*)))
      ;; ugh still memory fault
      #+nil(objc latest-view "setValue:forKey:" :pointer +YES+ :string "needsDisplay")
      ;; FIXME 2025-08-16 10:10:06 sig10 ie SIBGUS ie bad access?
      #+nil(redisplay latest-view)
      ;; "simpler":
      (objc latest-view "display"))))

(progn ; ───────────────────────────────────────────────────────── Core Graphics
  (defmethod draw ((self view))
    (let* ((ctx (current-cg-context))
           ;;(r (rect (random 100) 10 20 30))
           (w (width self))
           (h (height self))
           (r (rect 0 0 w h))
           )
      ;;(format t "bounds ~a~%" (cg:display-bounds 0))
      (cg:set-rgb-fill-color ctx (random 1.0) (random 1.0) (random 1.0))
      (cg:fill-rect ctx r)
      (cg:set-line-width ctx 10.0)
      (cg:set-rgb-stroke-color ctx (random 1.0) 0 0)
      (cg:move-to-point ctx (random w) (random h))
      (cg:add-line-to-point ctx (random w) (random h))
      (cg:add-curve-to-point ctx (random w) (random h) (random w) (random h) (random w) (random h))
      (cg:stroke-path ctx)))
  (redisplay-last))

(start-event-loop) ; NB reeval application Run! if broken

(with-event-loop (:waitp t)
  (let* ((win (make-instance 'window
                                :rect (in-screen-rect (rect 0 1000 720 450))
                                :title "Core Graphics demo"))
         (view (make-instance 'view)))
    (setf (content-view win) view)
    (window-show win))) 
;; ────────────────────────────────────────────────────────────── Metal Tool Kit

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
  (redisplay-last))

(with-event-loop (:waitp t)
  (let* ((win (make-instance 'window
                             :rect (in-screen-rect (rect 0 1000 720 450))
                             :title "Metal Tool Kit demo"))
         (view (make-instance 'mtk-view))

         ;; TODO 2025-08-16 20:03:21 separate out so shader (pipeline etc?) can be hot reloaded
         (shader-source (uiop:read-file-string "example/example.metal")) ; FIXME 2025-08-16 14:47:17 what sets cwd?
         ;; Uncompilable shader would be described in sly-inferior-lisp log from objc until I get lisp impl working.
         ;; Doesn't kill repl/runtime, just Continue.
         (library (mtl::make-library (device view) shader-source))
         (vertex-fn (mtl::make-function library "vertex_main"))
         (fragment-fn (mtl::make-function library "fragment_main"))
         (pd (mtl::make-render-pipeline-descriptor))
         (vd (mtl::make-vertex-descriptor))
         (ctx (context view)))
    (mtl::set-color-attachment-pixel-format pd 0 mtl::+pixel-format-a8-unorm+)
    (mtl::set-vertex-function pd vertex-fn)
    (mtl::set-fragment-function pd fragment-fn)
    (mtl::set-vertex-descriptor-attribute vd 0 mtl:+vertex-format-float3+ 0 0)
    (mtl::set-vertex-descriptor-layout vd 0 (* 3 bytes-per-float) 1 mtl:+vertex-step-function-per-vertex+)
    (mtl::set-vertex-descriptor pd vd)
    (setf (pipeline-state ctx) (mtl:make-render-pipeline-state view pd)
          (command-queue ctx) (mtl:make-command-queue (device view)))
    (setf (content-view win) view)
    (window-show win)))

#+nil(maphash #'(lambda (k v)
             (format t "~S ~S~%" k v)) *view-table*)

(defun scale-cursor (loc dim)
  "Scale cursor to [-1,1]"
  (coerce (1- (* (/ loc dim) 2)) 'single-float))

#+nil(defmethod mouse-moved ((self base-view) event location-x location-y)
       (format t "~a ~a ~%" location-x location-y)
       (setf (aref *vertex-data* 0) (scale-cursor location-x (width self))
             (aref *vertex-data* 1) (scale-cursor location-y (height self))))

#+nil(uiop/os:getcwd) ; depends on from which buffer sly was started
