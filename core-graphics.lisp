(in-package :core-graphics)

(defmacro cgfloat (x) `(float ,x 1.0d0))

(defun save-gstate (context) "Save graphics state."
  (cffi:foreign-funcall "CGContextSaveGState" :pointer context))

(defun restore-gstate (context) "Restore graphics state."
  (cffi:foreign-funcall "CGContextRestoreGState" :pointer context))

;; ─────────────────────────────────────────────────── Macro to reduce verbosity

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cgify-name (symbol)
    "Convert 'lisp-name into \"CGContextLispName\"."
    (let* ((segs (uiop:split-string (string symbol) :separator '(#\-)))
           (title (loop for seg in segs
                        collect (cond ((string= seg "CTM") "CTM")
                                      ((string= seg "EO") "EO")
                                      (t (string-capitalize seg))))))
      (format nil "CGContext~{~a~}" title)))
  
  (defun interpret-args (args)
    "Convert list of args, with optional interleaved type keywords
(default :double), into defun-args (and cffi-args)."
  (let ((next-type :double)
        (defun-args (list))
        (cffi-args (list)))
    (dolist (arg args)
      (if (keywordp arg)
          (setf next-type arg)
          (progn
            (setf defun-args (nconc defun-args `(,arg)))
            (setf cffi-args
                  (nconc cffi-args
                         (case next-type
                           (:double `(:double (cgfloat ,arg)))
                           (:rect `((:struct ns:rect) ,arg))
                           (otherwise `(,next-type ,arg)))))
            (setf next-type :double))))
    (values defun-args cffi-args))))

;; (cgify-name 'scale-ctm)
;; => "CGContextScaleCTM"

(defmacro defcall (symbol &optional args)
  "Simplify cffi Core Graphics defuns."
  (multiple-value-bind (defun-args cffi-args) (interpret-args args)
    `(defun ,symbol (context ,@defun-args)
       (cffi:foreign-funcall ,(cgify-name symbol)
                             :pointer context
                             ,@cffi-args))))

;; (macroexpand-1 '(defcall stroke-rect-with-width (:rect rect width)))
;;  => (DEFUN STROKE-RECT-WITH-WIDTH (CONTEXT RECT WIDTH)
;;   (CFFI:FOREIGN-FUNCALL "CGContextStrokeRectWithWidth" :POINTER CONTEXT
;;                         (:STRUCT CL-NEXTSTEP:RECT) RECT :DOUBLE
;;                         (CGFLOAT WIDTH)))
;; T
;; ─────────────────────────────────────────────────────────────────────────────

(defcall scale-ctm (sx sy))
(defcall translate-ctm (tx ty))
(defcall rotate-ctm (radians))
(defcall set-line-width (width))

(defun set-line-cap (context cap)
  (let ((code (case cap
                (:butt 0) ;; kCGLineCapButt
                (:square 2) ;; kCGLineCapSquare
                (:round 1) ;;  kCGLineCapRound
                (otherwise cap))))
    (cffi:foreign-funcall "CGContextSetLineCap" :pointer context :int code)))

(defun set-line-join (context join)
  (let ((code (ecase join
                (:miter 0) ;; kCGLineJoinMiter
                (:round 1) ;; kCGLineJoinRound
                (:bevel 2) ;; kCGLineJoinBevel
                (otherwise join))))
    (cffi:foreign-funcall "CGContextSetLineJoin" :pointer context :int code)))

(defcall set-miter-limit (limit))
(defcall set-flatness (flatness))
(defcall set-alpha (alpha))

(defparameter *blend-mode-alist*
  '((:normal . 0)
    (:multiply . 1)
    (:screen . 2)
    (:overlay . 3)
    (:darken . 4)
    (:lighten . 5)
    (:color-dodge . 6)
    (:color-burn . 7)
    (:soft-light . 8)
    (:hard-light . 9)
    (:difference . 10)
    (:exclusion . 11)
    (:hue . 12)
    (:saturation . 13)
    (:color . 14)
    (:luminosity . 15)
    (:clear . 16)
    (:copy . 17)
    (:source-in . 18)
    (:source-out . 19)
    (:source-atop . 20)
    (:destination-over . 21)
    (:destination-in . 22)
    (:destination-out . 23)
    (:destination-atop . 24)
    (:xor . 25)
    (:plus-darker . 26)
    (:plus-lighter . 27)))

(defun set-blend-mode (context mode)
  (let ((code (or (cdr (assoc mode *blend-mode-alist*))
                  mode)))
    (cffi:foreign-funcall "CGContextSetBlendMode" :pointer context :int code)))

(defcall begin-path ())
(defcall move-to-point (x y))
(defcall add-line-to-point (x y))
(defcall add-curve-to-point (cp1x cp1y cp2x cp2y x y))
(defcall add-quad-curve-to-point (cpx cpy x y))
(defcall close-path ())
(defcall add-arc (x y radius start-angle end-angle :int clockwise))
(defun draw-path (context mode)
  (let ((code (case mode
                (:fill 0) ;; kCGPathFill
                (:eofill 1) ;; kCGPathEOFill
                (:stroke 2) ;; kCGPathStroke
                (:fill-stroke 3) ;; kCGPathFillStroke
                (:eofill-stroke 4) ;; kCGPathEOFillStroke
                (otherwise mode))))
    (cffi:foreign-funcall "CGContextDrawPath" :pointer context :int code)))

(defcall fill-path ())
(defcall eo-fill-path ())
(defcall stroke-path ())
(defcall fill-rect (:rect rect))
(defcall stroke-rect-with-width (:rect rect width))
(defcall clear-rect (:rect rect))
(defcall fill-ellipse-in-rect (:rect rect))
(defcall stroke-ellipse-in-rect (:rect rect))

(defcall set-fill-color-with-color (:pointer color))
(defcall set-stroke-color-with-color (:pointer color))
(defcall set-fill-color-space (:pointer space))
(defcall set-stroke-color-space (:pointer space))

(defun set-gray-fill-color (context gray &optional (alpha 1))
  (cffi:foreign-funcall "CGContextSetGrayFillColor" :pointer context :double (cgfloat gray) :double (cgfloat alpha)))

(defun set-gray-stroke-color (context gray &optional (alpha 1))
  (cffi:foreign-funcall "CGContextSetGrayStrokeColor" :pointer context :double (cgfloat gray) :double (cgfloat alpha)))

(defun set-rgb-fill-color (context red green blue &optional (alpha 1))
  (cffi:foreign-funcall "CGContextSetRGBFillColor" :pointer context
                                                   :double (cgfloat red)
                                                   :double (cgfloat green)
                                                   :double (cgfloat blue)
                                                   :double (cgfloat alpha)))

(defun set-rgb-stroke-color (context red green blue &optional (alpha 1))
  (cffi:foreign-funcall "CGContextSetRGBStrokeColor" :pointer context
                                                     :double (cgfloat red)
                                                     :double (cgfloat green)
                                                     :double (cgfloat blue)
                                                     :double (cgfloat alpha)))

(defun set-cmyk-fill-color (context cyan magenta yellow black &optional (alpha 1))
  (cffi:foreign-funcall "CGContextSetCMYKFillColor" :pointer context :double (cgfloat cyan) :double (cgfloat magenta)
			:double (cgfloat yellow) :double  (cgfloat black) :double (cgfloat alpha)))

(defun set-cmyk-stroke-color (context cyan magenta yellow black &optional(alpha 1))
  (cffi:foreign-funcall "CGContextSetCMYKStrokeColor" :pointer context :double (cgfloat cyan) :double (cgfloat magenta)
			:double (cgfloat yellow) :double (cgfloat black) :double (cgfloat alpha)))

(defcall draw-image (:rect rect :pointer cg-image))
(defcall set-character-spacing (spacing))
(defcall set-text-position (x y))
(defun set-text-drawing-mode (context mode)
  (let ((code (case mode
                (:fill 0) ;; kCGTextFill
                (:stroke 1) ;; kCGTextStroke
                (:fill-stroke 2) ;; kCGTextFillStroke
                (:invisible 3) ;; kCGTextInvisible
                (:fill-clip 4) ;; kCGTextFillClip
                (:stroke-clip 5) ;; kCGTextStrokeClip
                (:fill-stroke-clip 6) ;; kCGTextFillStrokeClip
                (:clip 7) ;; kCGTextClip
                (otherwise mode))))
    (cffi:foreign-funcall "CGContextSetTextDrawingMode" :pointer context :int code)))
(defcall set-font (:pointer font))
(defcall set-font-size (size))
(defun select-font (context font-name size encoding)
  (let ((code (case encoding
		(:macroman 1) ;; #$kCGEncodingMacRoman
		(:font-specific 0) ;; #$kCGEncodingFontSpecific
		(otherwise encoding))))
    (cffi:foreign-funcall "CGContextSelectFont" :pointer context :string font-name :double (cgfloat size) :int code)))
(defun show-text (context string)
  (let ((n (length (babel:string-to-octets string))))
    (cffi:foreign-funcall "CGContextShowText" :pointer context :string string :int n)))

(defun show-text-at-point (context x y string)
  (let ((n (length (babel:string-to-octets string))))
    (cffi:foreign-funcall "CGContextShowTextAtPoint" :pointer context :double (cgfloat x) 
			  :double (cgfloat y) :string string :int n)))

;; ────────────────────────────────────────────────────────────────────── Colour

(defun make-color-space (name)
  (let* ((name (case name
                 (:color-space-srgb "kCGColorSpaceSRGB")
                 (t name))))
    (cffi:foreign-funcall "CGColorSpaceCreateWithName"
                          :pointer (cffi:mem-ref (cffi:foreign-symbol-pointer name)
                                                 :pointer)
                          :pointer)))


(defun release-color-space (color-space)
  (cffi:foreign-funcall "CGColorSpaceRelease" :pointer color-space))

(defun color-space-copy-name (color-space)
  (let* ((name (cffi:foreign-funcall "CGColorSpaceCopyName" :pointer color-space
                                                            :pointer)))
    (unless (cffi:null-pointer-p name) (ns:cf-string-to-lisp name))))

(defun make-color-generic-rgb (r g b a)
  (cffi:foreign-funcall "CGColorCreateGenericRGB"
                        :double (cgfloat r)
                        :double (cgfloat g)
                        :double (cgfloat b)
                        :double (cgfloat a)
                        :pointer))

;; ───────────────────────────────────────────────────────────────────── Display
;; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ Finding

(cffi:defcfun ("CGMainDisplayID" main-display-id) :unsigned-int
  "The display ID assigned to the main display.
The main display is the display with its screen location at (0,0) in the global display coordinate space. In a system without display mirroring, the display with the menu bar is typically the main display.

If mirroring is enabled and the menu bar appears on more than one display, this function provides a reliable way to find the main display.

In case of hardware mirroring, the drawable display becomes the main display. In case of software mirroring, the display with the highest resolution and deepest pixel depth typically becomes the main display.")

(defconstant +max-displays+ 10)

(defun online-display-list ()
  "Provides a list of displays that are online (active, mirrored, or sleeping)."
  (cffi:with-foreign-objects ((ids :uint32 10)
			      (count :uint32))
    (cffi:foreign-funcall "CGGetOnlineDisplayList" :int32 +max-displays+
						   :pointer ids
						   :pointer count)
    (loop for i below (cffi:mem-ref count :uint32)
	  collect (cffi:mem-aref ids :uint32 i) )))

(defun active-display-list ()
  "Provides a list of displays that are active for drawing."
  (cffi:with-foreign-objects ((ids :uint32 10)
			      (count :uint32))
    (cffi:foreign-funcall "CGGetActiveDisplayList" :int32 +max-displays+
						   :pointer ids
						   :pointer count)
    (loop for i below (cffi:mem-ref count :uint32)
	  collect (cffi:mem-aref ids :uint32 i) )))

(defun displays-with-point (point)
  "Provides a list of online displays with bounds that include the specified point."
  (cffi:with-foreign-objects ((ids :uint32 10)
			      (count :uint32))
    (cffi:foreign-funcall "CGGetDisplaysWithPoint" (:struct ns:point) point
			  :int32 +max-displays+
			  :pointer ids
			  :pointer count)
    (loop for i below (cffi:mem-ref count :uint32)
	  collect (cffi:mem-aref ids :uint32 i) )))

(defun displays-with-rect (rect)
  "Gets a list of online displays with bounds that intersect the specified rectangle."
  (cffi:with-foreign-objects ((ids :uint32 10)
			      (count :uint32))
    (cffi:foreign-funcall "CGGetDisplaysWithRect" (:struct ns:rect) rect
			  :int32 +max-displays+
			  :pointer ids
			  :pointer count)
    (loop for i below (cffi:mem-ref count :uint32)
	  collect (cffi:mem-aref ids :uint32 i) )))

;; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ Creating images

(cffi:defcfun ("CGDisplayCreateImage" display-create-image) :pointer
  "An image containing the contents of the specified display. If the display ID is invalid, the return value is NULL. The caller is responsible for releasing the image created by calling CGImageRelease."
  (display :uint32))

(cffi:defcfun ("CGDisplayCreateImageForRect" display-create-image-for-rect) :pointer
  "An image containing the contents of the specified rectangle. If the display ID is invalid, the return value is NULL. The caller is responsible for releasing the image created by calling CGImageRelease."
  (display :uint32)
  (rect (:struct ns:rect)))

;; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ Getting configuration

(cffi:defcfun ("CGDisplayCopyColorSpace" display-copy-color-space) :pointer
  "The current color space for the specified display. The caller is responsible for releasing the color space with the CGColorSpaceRelease function."
  (display :uint32))

(cffi:defcfun ("CGDisplayIsActive" display-is-active) :bool
  "Returns a Boolean value indicating whether a display is active. If true, the specified display is active; otherwise, false."
  (display :uint32))

(cffi:defcfun ("CGDisplayIsAlwaysInMirrorSet" display-is-always-in-mirror-set) :bool
  "Returns a Boolean value indicating whether a display is always in a mirroring set. If true, the specified display is in a mirroring set and cannot be removed from this set."
  (display :uint32))

(cffi:defcfun ("CGDisplayIsAsleep" display-is-asleep) :bool
  "If YES, the specified display is in sleep mode; otherwise, NO. A display is sleeping when its framebuffer and the attached monitor are in reduced power mode. A sleeping display is still considered to be a part of global display (desktop) space, but it is not drawable."
  (display :uint32))

(cffi:defcfun ("CGDisplayIsBuiltin" display-is-builtin) :bool
  "If true, the specified display is considered to be a built-in display; otherwise, false."
  (display :uint32))

(cffi:defcfun ("CGDisplayIsInHWMirrorSet" display-is-in-hw-mirror-set) :bool
  "If true, the specified display is a member of a hardware mirroring set; otherwise, false."
  (display :uint32))

(cffi:defcfun ("CGDisplayIsInMirrorSet" display-is-in-mirror-set) :bool
  "If true, the specified display is a member of a software or hardware mirroring set; otherwise, false."
  (display :uint32))

(cffi:defcfun ("CGDisplayIsMain" display-is-main) :bool
  "If true, the specified display is currently the main display; otherwise, false."
  (display :uint32))

(cffi:defcfun ("CGDisplayIsOnline" display-is-online) :bool
  "If true, the specified display is connected; otherwise, false."
  (display :uint32))

(cffi:defcfun ("CGDisplayIsStereo" display-is-stereo) :bool
  "If true, the specified display is running in a stereo graphics mode; otherwise, false."
  (display :uint32))

(cffi:defcfun ("CGDisplayMirrorsDisplay" display-mirrors-display) :uint32
  "For a secondary display in a mirroring set, returns the primary display. Returns the primary display in the mirroring set. Returns kCGNullDirectDisplay if the specified display is actually the primary display or is not in a mirroring set."
  (display :uint32))

(cffi:defcfun ("CGDisplayModelNumber" display-model-number) :uint32
  "A model number for the monitor associated with the specified display, or a constant to indicate an exception"
  (display :uint32))

(cffi:defcfun ("CGDisplayPrimaryDisplay" display-primary-display) :uint32
  "The primary display in the mirror set. If display is not hardware-mirrored, this function simply returns display."
  (display :uint32))

(cffi:defcfun ("CGDisplayRotation" display-rotation) :double
  "The rotation angle of the display in degrees, or 0 if the display is not valid."
  (display :uint32))

(cffi:defcfun ("CGDisplayScreenSize" display-screen-size) (:struct ns:size)
  "The size of the specified display in millimeters, or 0 if the display is not valid."
  (display :uint32))

(cffi:defcfun ("CGDisplaySerialNumber" display-serial-number) :uint32
  "The identifier of the display to be accessed."
  (display :uint32))

(cffi:defcfun ("CGDisplayUnitNumber" display-unit-number) :uint32
  "The identifier of the display to be accessed."
  (display :uint32))

(cffi:defcfun ("CGDisplayUsesOpenGLAcceleration" display-uses-opengl-acceleration) :bool
  "If true, Quartz Extreme is used to render in the specified display; otherwise, false."
  (display :uint32))

(cffi:defcfun ("CGDisplayVendorNumber" display-vendor-number) :uint32
  "The identifier of the display to be accessed."
  (display :uint32))

(cffi:defcfun ("CGDisplayBounds" display-bounds) (:struct ns:rect)
  "The bounds of the display, expressed as a rectangle in the global display coordinate space (relative to the upper-left corner of the main display)."
  (display :uint32))

(cffi:defcfun ("CGDisplayPixelsHigh" display-pixels-high) :sizet
  "The display height in pixel units."
  (display :uint32))

(cffi:defcfun ("CGDisplayPixelsWide" display-pixels-wide) :sizet
  "The display width in pixel units."
  (display :uint32))

;; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ Creating and managing modes

(cffi:defcfun ("CGDisplayCopyDisplayMode" display-copy-display-mode) :pointer
  "A display-mode opaque-type reference, or NULL if the display is invalid. The caller is responsible for releasing the display mode using CGDisplayModeRelease."
  (display :uint32))

;; (cffi:defcfun ("CGDisplayCopyAllDisplayModes" display-copy-all-display-modes) :pointer
;;   (display :uint32)
;;   (options :pointer))

(cffi:defcfun ("CGDisplaySetDisplayMode" display-set-display-mode) :int32
  "This function switches the display mode of the specified display. The operation is always synchronous; the function doesn’t return until the mode switch is complete. Note that after switching, display parameters and addresses may change.

The selected display mode persists for the life of the calling program. When the program terminates, the display mode automatically reverts to the permanent setting in the Displays panel of System Preferences.

When you change the display mode of a display in a mirroring set, your change switches other displays in the mirroring set to a mode capable of mirroring the bounds of the adjusted display. To avoid this automatic behavior, you can use the following procedure: call CGBeginDisplayConfiguration, call CGConfigureDisplayWithDisplayMode for each display to explicitly set the mode, and finally call CGCompleteDisplayConfiguration."
  (diplay :uint32)
  (mode :pointer)
  (options :pointer))

(cffi:defcfun ("CGDisplayModeRetain" retain-display-mode) :pointer
  "Retains a Core Graphics display mode."
  (mode :pointer))

(cffi:defcfun ("CGDisplayModeRelease" release-display-mode) :void
  "Releases a Core Graphics display mode. This function is equivalent to CFRelease, except that it does not cause an error if the mode parameter is NULL."
  (mode :pointer))

;; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ Getting information about a mode

(cffi:defcfun ("CGDisplayModeGetWidth" display-mode-width) :sizet
  "The width, in pixels, of the specified display mode."
  (mode :pointer))

(cffi:defcfun ("CGDisplayModeGetHeight" display-mode-height) :sizet
  "The height, in pixels, of the specified display mode."
  (mode :pointer))

(cffi:defcfun ("CGDisplayModeGetRefreshRate" display-mode-refresh-rate) :double
  "The refresh rate, in hertz, of the specified display mode for a CRT display. Some displays may not use conventional video vertical and horizontal sweep in painting the screen; for these displays, the return value is 0."
  (mode :pointer))

(cffi:defcfun ("CGDisplayModeGetIOFlags" display-mode-io-flags) :uint32
  "Returns the I/O Kit flags of the specified display mode."
  (mode :pointer))

(cffi:defcfun ("CGDisplayModeGetIODisplayModeID" display-mode-io-display-mode-id) :int32
  "Returns the I/O Kit display mode ID of the specified display mode."
  (mode :pointer))

(cffi:defcfun ("CGDisplayModeIsUsableForDesktopGUI" display-mode-is-usable-for-desktop-gui) :bool
  "Returns a Boolean value indicating whether the specified display mode is usable for a desktop graphical user interface. If true, the specified display mode is usable for a desktop graphical user interface; otherwise, false."
  (mode :pointer))

(cffi:defcfun ("CGDisplayModeGetTypeID" display-mode-type-id) :unsigned-long
  "Returns the type identifier of Quartz display modes. The type identifier of the CGDisplayMode opaque type.")

;; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ Controlling mouse cursor

(cffi:defcfun ("CGDisplayHideCursor" display-hide-cursor) :int32
  "This function hides the cursor regardless of its current location. The display parameter has no effect. In most cases, the caller must be the foreground application to affect the cursor."
  (display :uint32))

(cffi:defcfun ("CGDisplayShowCursor" display-show-cursor) :int32
  "If the hide cursor count is 0, this function shows the cursor regardless of its current location. The display parameter has no effect. In most cases, the caller must be the foreground application to affect the cursor."
  (display :uint32))

(cffi:defcfun ("CGDisplayMoveCursorToPoint" display-move-cursor-to-point) :int32
  "Moves the mouse cursor to a specified point relative to the upper-left corner of the display."
  (display :uint32)
  (point (:struct ns:point)))

(cffi:defcfun ("CGAssociateMouseAndMouseCursorPosition" associate-mouse-and-mouse-cursor-position) :int32
  "Connects or disconnects the mouse and cursor while an application is in the foreground. Call this function to disconnect the mouse from the cursor. When you call this function, the events your application receives from the system have a constant absolute location but contain delta updates to the X and Y coordinates of the mouse. You can hide the cursor or change it into something appropriate for your application. You can reposition the cursor by using the function CGDisplayMoveCursorToPoint or the function CGWarpMouseCursorPosition."
  (connected :bool))

(cffi:defcfun ("CGWarpMouseCursorPosition" wrap-mouse-cursor-position) :int32
  "Moves the mouse cursor without generating events. You can use this function to “warp” or alter the cursor position without generating or posting an event. For example, this function is often used to move the cursor position back to the center of the screen by games that do not want the cursor pinned by display edges."
  (new-cursor-position (:struct ns:point)))

(defun last-mouse-delta ()
  "Reports the change in mouse position since the last mouse movement event received by the application. This function is not recommended for general use. Instead, you should use the mouse-tracking functions provided by the NSEvent class."
  (cffi:with-foreign-objects ((delta-x :int)
			      (delta-y :int))
    (cffi:foreign-funcall "CGGetLastMouseDelta" :pointer delta-x :pointer delta-y)
    (list (cffi:mem-ref delta-x :int) (cffi:mem-ref delta-y :int))))

;; ─────────────────────────────────────────────────────────────────────── Image

(defun load-image (path)
  (let* ((path (uiop:truenamize path)))
    (unless (probe-file path)
      (assert path nil "can't find file: ~s" path))
    (ns:with-event-loop (:waitp t)
      (let* ((ns-image (ns:objc (ns:objc "NSImage" "alloc" :pointer)
				"initWithContentsOfFile:"
				:pointer (ns:autorelease (ns:make-ns-string (namestring path)))
				:pointer)))
	(ns:objc ns-image "CGImageForProposedRect:context:hints:"
		 :pointer (cffi:null-pointer)
		 :pointer (cffi:null-pointer)
		 :pointer (cffi:null-pointer)
		 :pointer)))))


(defun make-image-from-screen (rect)
  (cffi:foreign-funcall "CGWindowListCreateImage"
                        (:struct ns:rect) rect
			:int 12 	; kCGWindowListOptionIncludingWindow | kCGWindowListOptionOnScreenBelowWindow
			:int 0		; kCGNullWindowID
			:int 16		; kCGWindowImageNominalResolution
			:pointer))


(cffi:defcfun ("CGBitmapContextCreateImage" make-image-from-context) :pointer
  (bitmap-context :pointer))

(cffi:defcfun ("CGImageRetain" retain-image) :pointer
  (cg-image :pointer))

(cffi:defcfun ("CGImageRelease" release-image) :void
  (cg-image :pointer))

(cffi:defcfun ("CGImageGetWidth" image-width) :sizet
  (cg-image :pointer))

(cffi:defcfun ("CGImageGetHeight" image-height) :sizet
  (cg-image :pointer))

(cffi:defcfun ("CGImageGetBitsPerPixel" image-bits-per-pixel) :sizet
  (cg-image :pointer))

(cffi:defcfun ("CGImageGetBytesPerRow" image-bytes-per-row) :sizet
  (cg-image :pointer))

(defun image-data (cg-image)
  "this function should be call in EventLoop"
  (let* ((ns-bitmap (ns:autorelease (ns:objc (ns:objc "NSBitmapImageRep" "alloc" :pointer)
					     "initWithCGImage:"
					     :pointer cg-image
					     :pointer))))
    (ns:objc ns-bitmap "bitmapData" :pointer)))

(defun write-to-png-file (image path)
  (ns:with-event-loop nil
    (let* ((image-destination
	     (cffi:foreign-funcall "CGImageDestinationCreateWithURL"
				   :pointer 
				   (ns:objc "NSURL" "fileURLWithPath:" :pointer (ns:autorelease (ns:make-ns-string (uiop:native-namestring path))) :pointer)
				   :pointer (cffi:mem-ref (cffi:foreign-symbol-pointer "kUTTypePNG") :pointer)
				   :sizet 1
				   :pointer (cffi:null-pointer)
				   :pointer)))
      (unwind-protect (progn
			(cffi:foreign-funcall "CGImageDestinationAddImage" :pointer image-destination
									   :pointer image
									   :pointer (cffi:null-pointer))
			(cffi:foreign-funcall "CGImageDestinationFinalize" :pointer image-destination)))
      (ns:cf-release image-destination))))

(defun make-bitmap-context (width height &key (data (cffi:null-pointer)) (color-space :color-space-srgb) (alpha-info :last) (bitmap-info :order-default))
  (ns:with-event-loop (:waitp t)
    (let* ((color-space (make-color-space color-space)))
      (prog1
	  (cffi:foreign-funcall "CGBitmapContextCreate"
				:pointer data
				:sizet width
				:sizet height
				:sizet 8
				:sizet (* width 4)
				:pointer color-space 
				:unsigned-int (logior (ecase bitmap-info
							(:order-default 0)
							(:order-little 8192)
							(:order-big 16384))
						      (ecase alpha-info
							(:last 1)
							(:first 2)))
				:pointer)
	(release-color-space color-space)))))
      
(cffi:defcfun ("CGBitmapContextGetData" context-data) :pointer
  (context :pointer))

(cffi:defcfun ("CGBitmapContextGetWidth" context-width) :sizet
  (context :pointer))

(cffi:defcfun ("CGBitmapContextGetHeight" context-height) :sizet
  (context :pointer))

(cffi:defcfun ("CGContextRelease" release-context) :void
  (context :pointer))

;; ────────────────────────────────────────────────────────────────────── Layers
(defun make-layer (context width height)
  (cffi:foreign-funcall "CGLayerCreateWithContext"
			:pointer context
			(:struct ns:size) (ns:size width height)
			:pointer (cffi:null-pointer)
			:pointer))

(cffi:defcfun (release-layer "CGLayerRelease") :void
  (cg-layer :pointer))

(cffi:defcfun (retain-layer "CGLayerRetain") :pointer
  (cg-layer :pointer))

(cffi:defcfun (draw-layer-in-rect "CGContextDrawLayerInRect") :void
  (context :pointer)
  (rect (:struct ns:rect))
  (layer :pointer))

(cffi:defcfun (draw-layer-at-point "CGContextDrawLayerAtPoint") :void
  (context :pointer)
  (point (:struct ns:point))
  (layer :pointer))


(cffi:defcfun (layer-size "CGLayerGetSize") (:struct ns:size)
  (cg-layer :pointer))

(cffi:defcfun (layer-context "CGLayerGetContext") :pointer
  (cg-layer :pointer))
