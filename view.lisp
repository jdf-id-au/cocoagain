(in-package :cocoagain)

(defvar *view-table* (make-hash-table))

(cffi:defcallback view-draw-callback :void
    ((id :int)
     (draw-flag :int)
     (cgl-context :pointer)
     (cgl-pixel-format :pointer)
     (width :int) (height :int))
  (let* ((view (gethash id *view-table*)))
    (setf (cgl-context view) cgl-context
          (cgl-pixel-format view) cgl-pixel-format
          (width view) width
          (height view) height)
    (handler-case
        (ecase draw-flag
          (0 (init view))
          (1 (draw view))
          (2 (reshape view))
          (3 (release view)))
      (error (c) (break (format nil "Caught signal while drawing: \"~a\"" c))))))

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
   (g-id :initform 0 :accessor g-id :allocation :class)
   (cgl-context :accessor cgl-context)
   (cgl-pixel-forat :accessor cgl-pixel-format)
   (width :accessor width)
   (height :accessor height)
   (cocoa-ref :accessor cocoa-ref)))

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
(defun redisplay (view)
  (objc view "setNeedsDisplayInRect:" (:struct rect)
        (rect 0 0 (width view) (height view))))

(defclass view (base-view) ())

(defmethod initialise-instance :after
    ((self view)
     &key (x 0) (y 0) (w 400) (h 200))
  (setf (cocoa-ref self)
        (objc (objc "View" "alloc" :pointer)
              "initWithID:frame:drawFn:eventFn:"
              :int (id self)
              (:struct rect) (rect x y w h)
              :pointer (cffi:callback view-draw-callback)
              :pointer (cffi:callback view-event-callback)
              :pointer)))
(defun current-cg-context ()
  (let* ((graphic-context (objc "NSGraphicsContext" "currentContext"
                                :pointer)))
    (if (cffi:null-pointer-p graphic-context) graphic-context ; ?
        (objc graphic-context "CGContext" :pointer))))
