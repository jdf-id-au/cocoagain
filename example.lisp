;; C-c C-l sly-load-file cocoagain.asd
;; (ql:quickload '(:alexandria :cffi :cffi-libffi :float-features :bordeaux-threads :trivial-main-thread))
;; (ql:quickload :cocoagain)

(in-package :cocoagain)

(defclass 2d-canvas (view) ()) ; vs straight cocoagain:view?

(defconstant +YES+ (objc (alloc "NSNumber") "initWithBool:" :unsigned-char 1 :pointer))
(defconstant +NO+ (objc (alloc "NSNumber") "initWithBool:" :unsigned-char 0 :pointer))

(defun redisplay ()
  (let* ((latest-id (loop for k being each hash-key of *view-table* maximize k))
         (latest-view (gethash latest-id *view-table*)))
    #+nil latest-view
    ;; ugh still memory fault
    #+nil(objc latest-view "setValue:forKey:" :pointer +YES+ :string "needsDisplay")
    ;; lol this works
    (objc latest-view "display")))

(progn
  (defmethod draw ((self 2d-canvas))
    (let* ((ctx (current-cg-context))
           ;;(r (rect (random 100) 10 20 30))
           (w (width self))
           (h (height self))
           (r (rect 0 0 w h))
           )
      ;;(format t "tried to draw! ~a ~a ~%" self ctx)
      ;;(format t "bounds ~a~%" (cg:display-bounds 0))
      (set-rgb-fill-color ctx (random 1.0) (random 1.0) (random 1.0))
      (fill-rect ctx r)
      (set-rgb-stroke-color ctx (random 1.0) 0 0)
      (move-to-point ctx (random w) (random h))
      #+nil(add-line-to-point ctx (random w) (random h))
      (add-curve-to-point ctx (random w) (random h) (random w) (random h) (random w) (random h))
      (stroke-path ctx)))
  (redisplay))

(start-event-loop) ; NB reeval application Run! if broken

(with-event-loop (:waitp t)
  (let* ((win (make-instance 'window
                                :rect (in-screen-rect (rect 0 1000 720 450))
                                :title "cocoagain demo"))
         (view (make-instance '2d-canvas)))
    (setf (content-view win) view)
    (window-show win)))p

;; #+nil is Common Lisp semi-equivalent of Clojure #_
#+nil (maphash #'(lambda (k v)
             (format t "~S ~S~%" k v)) *view-table*)

