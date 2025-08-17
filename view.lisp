(in-package :cocoagain)

(defvar *view-table* (make-hash-table))
;; (clrhash *view-table*)

(cffi:defcallback view-draw-callback :void
    ((id :int)
     (draw-flag :int)
     (cgl-context :pointer)
     (cgl-pixel-format :pointer)
     (width :int) (height :int))
  (let* ((view (gethash id *view-table*)))
    (setf (width view) width
          (height view) height)
    (handler-case
        (ecase draw-flag
          (0 (init view))
          (1 (draw view))
          (2 (reshape view)) ; NB 2025-08-15 21:29:38 only defined for mtl?
          (3 (release view) (remhash id *view-table*)))
      (error (c) (break (format nil "Caught signal while drawing (~a): \"~a\"" draw-flag c))))))

(cffi:defcallback view-event-callback :void
    ((id :int)
     (mouse-flag :int)
     (event :pointer)
     (x :double) (y :double))
  (let* ((view (gethash id *view-table*)))
    (handler-case
        (ecase mouse-flag
          (0 (mouse-down view event x y))
          (1 (mouse-dragged view event x y))
          (2 (mouse-up view event x y))
          (3 (mouse-moved view event x y))
          (4 (scroll-wheel view event x y))
          (5 (key-down view event
                       (char-code (aref (ns-string-to-lisp
                                         (objc event "characters"
                                               :pointer))
                                        0))))))))

(defclass base-view ()
  ((id :accessor id)
   ;; g-id is monotonically increasing global id (class object shared slot)
   (g-id :initform 0 :accessor g-id :allocation :class)
   (width :accessor width)
   (height :accessor height)
   (cocoa-ref :accessor cocoa-ref)))

(defmethod initialize-instance :after
    ((self base-view)
     &key)
  (setf (id self) (g-id self))
  (incf (g-id self))
  (setf (gethash (id self) *view-table*) self))

(defmethod init ((self base-view)) ())
(defmethod draw ((self base-view)) ())
(defmethod release ((self base-view)) ())
(defmethod key-down ((self base-view) event key-code)
  (declare (ignorable event key-code)))
(defmethod mouse-down ((self base-view) event location-x location-y)
  (declare (ignorable event location-x location-y)))
(defmethod mouse-dragged ((self base-view) event location-x location-y)
  (declare (ignorable event location-x location-y)))
(defmethod mouse-up ((self base-view) event location-x location-y)
  (declare (ignorable event location-x location-y)))
(defmethod mouse-moved ((self base-view) event location-x location-y)
  (declare (ignorable event location-x location-y)))
(defmethod scroll-wheel ((self base-view) event location-x location-y)
  (declare (ignorable event location-x location-y)))

(defun command-p (event)
  (not (zerop (logand (objc event "modifierFlags" :unsigned-int)
                      (ash 1 20)))))
(defun shift-p (event)
  (not (zerop (logand (objc event "modifierFlags" :unsigned-int)
                      (ash 1 17)))))
(defun ctrl-p (event)
  (not (zerop (logand (objc event "modifierFlags" :unsigned-int)
                      (ash 1 18)))))
(defun opt-p (event)
  (not (zerop (logand (objc event "modifierFlags" :unsigned-int)
                      (ash 1 19)))))
(defun redisplay (view) ; FIXME 2025-08-16 22:31:21 hangs
  (objc view "setNeedsDisplayInRect:" (:struct rect)
        (rect 0 0 (width view) (height view))))

;; ─────────────────────────────────────────────────── Core Graphics (etc) View 

(defclass view (base-view) ())

(defmethod initialize-instance :after
    ((self view)
     &key (x 0) (y 0) (w 400) (h 200))
  (setf (cocoa-ref self)
        (objc (alloc "View")
              "initWithID:frame:drawFn:eventFn:"
              :int (id self)
              (:struct rect) (rect x y w h)
              :pointer (cffi:callback view-draw-callback)
              :pointer (cffi:callback view-event-callback)
              :pointer)))

(defun current-cg-context ()
  "Returns the current CGContext of the current thread."
  (let* ((graphic-context (objc "NSGraphicsContext" "currentContext" :pointer)))
    (if (cffi:null-pointer-p graphic-context)
        (error "Failed to get graphics context.")
        (objc graphic-context "CGContext" :pointer))))

;; ────────────────────────────────────────────────────────────── Metal Kit view

(defclass mtk-view (base-view)
  ((device :accessor device)
   (context :accessor context)))

(defmethod reshape ((self mtk-view)) ())

(defmethod initialize-instance :after
    ((self mtk-view)
     &key (x 0) (y 0) (w 400) (h 200))
  (let* ((device (cffi:foreign-funcall "MTLCreateSystemDefaultDevice" :pointer))
         (view (objc (alloc "MetalView") "initWithFrame:device:id:drawFn:eventFn:"
                     (:struct rect) (rect x y w h)
                     :pointer device
                     :int (id self)
                     :pointer (cffi:callback view-draw-callback)
                     :pointer (cffi:callback view-event-callback)
                     :pointer)))
    (setf (device self) device)
    (objc view "setDelegate:" :pointer view)
    (setf (cocoa-ref self) view)
    (init self)))

;; TODO 2025-08-16 19:10:37 do these belong in metal.lisp?
(defun color-pixel-format (mtk-view) (objc mtk-view "colorPixelFormat" :int))
(defun depth-stencil-pixel-format (mtk-view) (objc mtk-view "depthStencilPixelFormat" :int))
(defun set-depth-stencil-pixel-format (mtk-view pixel-format) ; setf??
  (objc mtk-view "setDepthStencilPixelFormat:" :int pixel-format))
(defun drawable-size (mtk-view) (objc mtk-view "drawableSize" (:struct size)))
(defun (setf drawable-size) (size mtk-view) (objc mtk-view "setDrawableSize:" (:struct size) size))
