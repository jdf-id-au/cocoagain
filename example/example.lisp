;; C-c C-l sly-load-file cocoagain.asd
;; (ql:quickload '(:alexandria :cffi :cffi-libffi :float-features :bordeaux-threads :trivial-main-thread))
(ql:quickload :cocoagain)

(in-package :cocoagain)

(defconstant +YES+ (objc (alloc "NSNumber") "initWithBool:" :unsigned-char 1 :pointer))
(defconstant +NO+ (objc (alloc "NSNumber") "initWithBool:" :unsigned-char 0 :pointer))

(defun redisplay-last ()
  (when (not (zerop (hash-table-count *view-table*)))
    (let* ((latest-id (loop for k being each hash-key of *view-table* maximize k))
           (latest-view (gethash latest-id *view-table*)))
      #+nil latest-view
      ;; ugh still memory fault
      #+nil(objc latest-view "setValue:forKey:" :pointer +YES+ :string "needsDisplay")
      (redisplay latest-view)
      ;; "simpler":
      #+nil(objc latest-view "display"))))

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
      #+nil(add-line-to-point ctx (random w) (random h))
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

(progn ; ──────────────────────────────────────────────────────── Metal Tool Kit
  (defmethod draw ((self mtk-view))
    )
  (redisplay-last))

(defparameter *inspect* nil)

(with-event-loop (:waitp t)
  (let* ((win (make-instance 'window
                             :rect (in-screen-rect (rect 0 1000 720 450))
                             :title "Metal Tool Kit demo"))
         (view (make-instance 'mtk-view))
         (shader-source (uiop:read-file-string "/Users/jdf/Projects/Learning/common-lisp/cocoagain/example/example.metal"))
         (library (mtl::make-library (device view) shader-source))
         (vertex-fn (mtl::make-function library "vertex_main"))
         (fragment-fn (mtl::make-function library "fragment_main"))
         (pd (mtl::make-render-pipeline-descriptor)))
    ;; FIXME 2025-08-16 00:27:42 Render Pipeline Descriptor Validation vertexFunction must not be nil
    ;; FIXME 2025-08-16 00:45:36 mtl::dont-fail isn't working lol
    (mtl::set-color-attachment-pixel-format pd 0 mtl::+pixel-format-r8-uint+)
    (mtl::set-vertex-function pd vertex-fn)
    (mtl::set-fragment-function pd fragment-fn)
    (let* ((ps (mtl::make-render-pipeline-state (device view) pd)))
      (setf (content-view win) view)
      (window-show win))))

;; #+nil is Common Lisp semi-equivalent of Clojure #_
#+nil (maphash #'(lambda (k v)
             (format t "~S ~S~%" k v)) *view-table*)

#+nil(defmethod mouse-moved ((self base-view) event location-x location-y)
       (format t "~a ~a ~%" location-x location-y))
(cffi:foreign-alloc :pointer)
