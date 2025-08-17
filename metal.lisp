(in-package :metal-kit)

;; ─────────────────────────────────────────────────────────────────────── Types

(cffi:defcstruct (origin :class %origin)
  (x :unsigned-long) ; why integer?
  (y :unsigned-long)
  (z :unsigned-long))

(defstruct (origin (:constructor origin (x y z))) x y z)

(defmethod cffi:translate-from-foreign (p (type %origin))
  (cffi:with-foreign-slots ((x y z) p (:struct origin))
    (origin x y z)))

(defmethod cffi:translate-into-foreign-memory (origin (type %origin) p)
  (cffi:with-foreign-slots ((x y z) p (:struct origin))
    (setf x (floor (origin-x origin))
          y (floor (origin-y origin))
          z (floor (origin-z origin)))))

(cffi:defcstruct (size :class %size)
  (width :unsigned-long)
  (height :unsigned-long)
  (depth :unsigned-long))

(defstruct (size (:constructor size (width height depth))) width height depth)

(defmethod cffi:translate-from-foreign (p (type %size))
  (cffi:with-foreign-slots ((width height depth) p (:struct size))
    (size width height depth)))

(defmethod cffi:translate-into-foreign-memory (size (type %size) p)
  (cffi:with-foreign-slots ((width height depth) p (:struct size))
    (setf width (floor (size-width size))
          height (floor (size-height size))
          depth (floor (size-depth size)))))

(cffi:defcstruct (region :class %region)
  (origin (:struct origin))
  (size (:struct size)))

(defstruct (region (:constructor region (x y z width height depth)))
  x y z width height depth)

(defmethod cffi:translate-from-foreign (p (type %region))
  (cffi:with-foreign-slots ((origin size) p (:struct region))
    (region (origin-x origin)
            (origin-y origin)
            (origin-z origin)
            (size-width size)
            (size-height size)
            (size-depth size))))

(defmethod cffi:translate-into-foreign-memory (region (type %region) p)
  (let* ((origin (cffi:foreign-slot-pointer p '(:struct region) 'origin))
         (size (cffi:foreign-slot-pointer p '(:struct region) 'size)))
    (cffi:with-foreign-slots ((x y z) origin (:struct origin))
      (cffi:with-foreign-slots ((width height depth) size (:struct size))
        (setf x (floor (region-x region))
              y (floor (region-y region))
              z (floor (region-z region))
              width (floor (region-width region))
              height (floor (region-height region))
              depth (floor (region-depth region)))))))

(cffi:defcstruct (viewport :class %viewport)
  (x :double)
  (y :double)
  (width :double)
  (height :double)
  (near :double)
  (far :double))

(defstruct (viewport (:constructor viewport (x y width height near far)))
  x y width height near far)

(defmethod cffi:translate-from-foreign (p (type %viewport))
  (cffi:with-foreign-slots ((x y width height near far) p (:struct viewport))
    (viewport x y width height near far)))

(defmethod cffi:translate-into-foreign-memory (viewport (type %viewport) p)
  (cffi:with-foreign-slots ((x y width height near far) p (:struct viewport))
    (setf x (coerce (viewport-x viewport) 'double-float)
          y (coerce (viewport-y viewport) 'double-float)
          width (coerce (viewport-width viewport) 'double-float)
          height (coerce (viewport-height viewport) 'double-float)
          near (coerce (viewport-near viewport) 'double-float)
          far (coerce (viewport-far viewport) 'double-float))))

;; ───────────────────────────────────────────────────────────────── Render pass

(defun make-command-queue (device)
  (ns:protect
   (ns:objc device "newCommandQueue" :pointer)
   "Failed to create command queue."))

(defun command-buffer (command-queue)
  (ns:protect
   (ns:objc command-queue "commandBuffer" :pointer)
   "Failed to get command buffer."))

(defun render-pass-descriptor (mtk-view)
  (ns:protect
   (ns:objc mtk-view "currentRenderPassDescriptor" :pointer)
   "Failed to get current pass descriptor."))

(defun render-command-encoder (command-buffer render-pass)
  (ns:protect
   (ns:objc command-buffer "renderCommandEncoderWithDescriptor:" :pointer render-pass :pointer)
   "Failed to get render command encoder."))

(defun drawable (mtk-view)
  (ns:objc mtk-view "currentDrawable" :pointer))

(defun present-drawable (command-buffer drawable)
  (ns:objc command-buffer "presentDrawable:" :pointer drawable))

(defun commit (command-buffer)
  (ns:objc command-buffer "commit"))

(defun set-viewport (command-encoder viewport)
  (ns:objc command-encoder "setViewport:" (:struct viewport) viewport))

(defun set-render-pipeline-state (command-encoder pipeline-state)
  (ns:objc command-encoder "setRenderPipelineState:" :pointer pipeline-state))

(defun set-depth-stencil-state (command-encoder depth-stencil-state)
  (ns:objc command-encoder "setDepthStencilState:" :pointer depth-stencil-state))

;; TODO compare with setVertexBufferOffset:atIndex: !
(defun set-vertex-buffer (command-encoder buffer &key (offset 0) (index 0))
  (ns:objc command-encoder "setVertexBuffer:offset:atIndex:" :pointer buffer
							     :int offset
							     :int index))

(defun set-fragment-buffer (command-encoder buffer &key (offset 0) (index 0))
  (ns:objc command-encoder "setFragmentBuffer:offset:atIndex:" :pointer buffer
							       :int offset
							       :int index))

(defun set-fragment-texture (command-encoder texture &key (index 0))
  (ns:objc command-encoder "setFragmentTexture:atIndex:" :pointer texture :int index))

(defun draw-primitives (command-encoder primitive start count &key (instance-count 1))
  (ns:objc command-encoder "drawPrimitives:vertexStart:vertexCount:instanceCount:"
	   :int primitive
	   :int start
	   :int count
	   :int instance-count))

(defun draw-indexed-primitives (command-encoder primitive count type buffer &key (offset 0) (instance-count 1))
  (ns:objc command-encoder "drawIndexedPrimitives:indexCount:indexType:indexBuffer:indexBufferOffset:instanceCount:"
	   :int primitive
	   :int count
	   :int type
	   :pointer buffer
	   :int offset
	   :int instance-count))

(defun end-encoding (command-encoder)
  (ns:objc command-encoder "endEncoding"))

;; ───────────────────────────────────────────────────────────────────── Shaders

(defun make-library (device source &key (options (cffi:null-pointer)))
  (ns:protect
   (ns:objc device "newLibraryWithSource:options:error:"
                   :pointer (ns:autorelease (ns:make-ns-string source))
                   :pointer options
                   :pointer (cffi:null-pointer) ; FIXME 2025-08-16 00:12:19
                   :pointer)
   "Shader compilation failed: ~% ~a" source))

(defun make-function (library name)
  (ns:protect
   (ns:objc library "newFunctionWithName:" :pointer (ns:autorelease (ns:make-ns-string name)) :pointer)
   "Failed to find function: ~a" name))

;; ────────────────────────────────────────────────── Render pipeline descriptor

(defun make-render-pipeline-descriptor ()
  (ns:new "MTLRenderPipelineDescriptor"))

(defun set-vertex-function (pipeline-descriptor function)
  (ns:objc pipeline-descriptor "setVertexFunction:" :pointer function))

(defun set-fragment-function (pipeline-descriptor function)
  (ns:objc pipeline-descriptor "setFragmentFunction:" :pointer function))

(defun set-vertex-descriptor (pipeline-descriptor vertex-descriptor)
  (ns:objc pipeline-descriptor "setVertexDescriptor:" :pointer vertex-descriptor))

(defun set-color-attachment-pixel-format (pipeline-descriptor index pixel-format)
  (let* ((color-attachment
	   (ns:objc (ns:objc pipeline-descriptor "colorAttachments" :pointer)
		    "objectAtIndexedSubscript:" :int index :pointer)))
    (ns:objc color-attachment "setPixelFormat:" :int pixel-format)))

(defun set-depth-attachment-pixel-format (pipeline-descriptor pixel-format)
  (ns:objc pipeline-descriptor "setDepthAttachmentPixelFormat:" :unsigned-int pixel-format))

;; ─────────────────────────────────────────────────────────── Vertex descriptor

(defun make-vertex-descriptor ()
  (ns:new "MTLVertexDescriptor")) ; TODO 2025-08-16 01:45:11 cleanup after?

(defun set-vertex-descriptor-attribute (vertex-descriptor index format offset buffer-index)
  (assert (<= 0 index 31) "Index out of range") ; Metal-Feature-Set-Tables.pdf
  (let* ((attribute (ns:objc (ns:objc vertex-descriptor "attributes" :pointer)
                             "objectAtIndexedSubscript:" :int index :pointer)))
    ;; The format of the vertex attribute.
    (ns:objc attribute "setFormat:" :int format)
    ;; The location of an attribute in vertex data, determined by the
    ;; byte offset from the start of the vertex data.
    (ns:objc attribute "setOffset:" :int offset)
    ;; The index in the argument table for the associated vertex buffer.
    (ns:objc attribute "setBufferIndex:" :int buffer-index)))

(defun set-vertex-descriptor-layout (vertex-descriptor index stride step-rate step-function)
  (let* ((layout (ns:objc (ns:objc vertex-descriptor "layouts" :pointer)
                          "objectAtIndexedSubscript:" :int index :pointer)))
    ;; The number of bytes between the first byte of two consecutive
    ;; vertices in a buffer. Check the Metal feature set tables (PDF)
    ;; for potential alignment restrictions for stride.
    (ns:objc layout "setStride:" :int stride)
    ;; The interval at which the vertex and its attributes are
    ;; presented to the vertex function. The default value is 1. The
    ;; stepRate value, in conjunction with the stepFunction property,
    ;; determines how often the function fetches new attribute data.
    ;; The stepRate property is generally used when stepFunction is
    ;; MTLVertexStepFunctionPerInstance. If stepRate is equal to 1,
    ;; new attribute data is fetched for every instance; if stepRate
    ;; is equal to 2, new attribute data is fetched for every two
    ;; instances, and so forth.
    (ns:objc layout "setStepRate:" :int step-rate)
    ;; The circumstances under which the vertex and its attributes are
    ;; presented to the vertex function. The default value is
    ;; MTLVertexStepFunctionPerVertex.
    ;; If stepFunction is MTLVertexStepFunctionPerVertex, the function
    ;; fetches new attribute data based on the [[vertex_id]] attribute
    ;; qualifier. The function fetches new attribute data each time a
    ;; new vertex is processed. In this case, stepRate must be set to
    ;; 1, which is its default value.
    ;; If stepFunction is MTLVertexStepFunctionPerInstance, the
    ;; function fetches new attribute data based on the
    ;; [[instance_id]] attribute qualifier. In this case, stepRate
    ;; must be greater than 0 and its value determines how often the
    ;; function fetches new attribute data.
    ;; If stepFunction is MTLVertexStepFunctionConstant, the function
    ;; fetches attribute data just once, and that attribute data is
    ;; used for every vertex. In this case,stepRate must be set to 0.
    (ns:objc layout "setStepFunction:" :int step-function)))

;; ────────────────────────────────────────────────────────────── Pipeline state

#+nil(defun make-render-pipeline-state (device pipeline-descriptor)
  (cffi:with-foreign-object (err :pointer)
    (let ((e (ns:objc "NSError" "errorWithDomain:code:userInfo:"
                      :string "placeholder"
                      :int 0
                      :pointer (cffi:null-pointer)))) ; TODO 2025-08-16 19:50:42 how?
      (setf (cffi:mem-ref err :pointer) e))
    (let ((p
            (ns:objc device "newRenderPipelineStateWithDescriptor:error:"
                     :pointer pipeline-descriptor
                     :pointer err
                     :pointer)))
      (if (cffi:null-pointer-p p)
          (error "Failed to create render pipeline state.~%~a ~a ~a ~a ~a"
                 device ; seemingly ok
                 pipeline-descriptor ; seemingly ok
                 p ; null
                 err
                 ;; TODO 2025-08-16 09:49:20
                 ;; Move this to ns:protect once polished, maybe have unhygienic ERR manager
                 ;; e.g. (dont-fail (thing ERR)) -> (cffi:with-foreign-object (ERR :pointer) (thing ERR) ...)
                 (ns:objc (cffi:mem-ref err :pointer) "localizedDescription")) ; NIL
          p))))

(defun make-render-pipeline-state (mtk-view pipeline-descriptor)
  "Workaround, implemented in objc. Only uses view to get at device."
  (ns:protect (ns:objc mtk-view "deviceRenderPipelineStateWithDescriptor:"
                 :pointer pipeline-descriptor
                 :pointer)
           "Failed to create render pipeline state (objc impl)."))

;; ─────────────────────────────────────────────────────────────── Depth stencil

(defun make-depth-stencil-descriptor ()
  (ns:new "MTLDepthStencilDescriptor"))

(defun set-depth-compare-function (depth-stencil-descriptor compare)
  (ns:objc depth-stencil-descriptor "setDepthCompareFunction:" :unsigned-int compare))

(defun set-depth-write-enabled (depth-stencil-descriptor enabled)
  (ns:objc depth-stencil-descriptor "setDepthWriteEnabled:" :bool enabled))

(defun make-depth-stencil-state (device depth-stencil-descriptor)
  (ns:objc device "newDepthStencilStateWithDescriptor:" :pointer depth-stencil-descriptor :pointer))

;; ────────────────────────────────────────────────────────────────────── Buffer

(defun new-buffer (device length &optional (options (+ +resource-cpu-cache-mode-default-cache+
                                                       +resource-storage-mode-managed+)))
  (ns:objc device "newBufferWithLength:options:"
           :int length
           :int options
           :pointer))

(defun make-buffer (device data length &optional (options (+ +resource-cpu-cache-mode-default-cache+
                                                             +resource-storage-mode-managed+)))
  "Copies data."
  (ns:objc device "newBufferWithBytes:length:options:"
	   :pointer data
	   :int length
	   :int options
	   :pointer))

(defun buffer-contents (buffer)
  (ns:objc buffer "contents" :pointer))

;; ───────────────────────────────────────────────────────────────────── Texture

(defun texture2d-descriptor (pixel-format width height mipmap)
  (ns:objc "MTLTextureDescriptor" "texture2DDescriptorWithPixelFormat:width:height:mipmapped:"
	   :int pixel-format
	   :int width
	   :int height
	   :bool mipmap
	   :pointer))

(defun make-texture (device descriptor)
  (ns:objc device "newTextureWithDescriptor:" :pointer descriptor :pointer))

;; TODO newTextureWithDescriptor:offset:bytesPerRow: on a MTLBuffer

(defun replace-region (texture region mipmap-level data bpr)
  (ns:objc texture "replaceRegion:mipmapLevel:withBytes:bytesPerRow:"
	   (:struct region) region
	   :int mipmap-level
	   :pointer data
	   :int bpr))

;; ──────────────────────────────────────────────────────────────── Clear colour
;; TODO 2025-08-16 22:57:26 move?

(cffi:defcstruct (clear-color :class %clear-color)
  (red :double)
  (green :double)
  (blue :double)
  (alpha :double))

(defstruct (clear-color (:constructor make-clear-color (red green blue alpha)))
  red green blue alpha)

(defmethod cffi:translate-from-foreign (p (type %clear-color))
  (cffi:with-foreign-slots ((red green blue alpha) p (:struct clear-color))
    (make-clear-color red green blue alpha)))

(defmethod cffi:translate-into-foreign-memory (clear-color (type %clear-color) p)
  (cffi:with-foreign-slots ((red green blue alpha) p (:struct clear-color))
    (setf red (coerce (clear-color-red clear-color) 'double-float)
	  green (coerce (clear-color-green clear-color) 'double-float)
	  blue (coerce (clear-color-blue clear-color) 'double-float)
	  alpha (coerce (clear-color-alpha clear-color) 'double-float))))

(defun clear-color (mtk-view red green blue alpha)
  (ns:objc mtk-view "setClearColor:" (:struct clear-color) (make-clear-color red green blue alpha)))

;; ─────────────────────────────────────────────────────────────────── Constants

(defmacro define-constant (name value)
  `(progn
     (defconstant ,name ,value)
     (export ',name)))

(define-constant +primitive-type-point+ 0)
(define-constant +primitive-type-line+ 1)
(define-constant +primitive-type-line-strip+ 2)
(define-constant +primitive-type-triangle+ 3)
(define-constant +primitive-type-triangle-strip+ 4)

(define-constant +index-type-uint16+ 0)
(define-constant +index-type-uint32+ 1)

(define-constant +resource-cpu-cache-mode-default-cache+ 0) ; ╴ resource options
(define-constant +resource-cpu-cache-mode-write-combined+ 1)
(define-constant +resource-storage-mode-shared+ 0)
(define-constant +resource-storage-mode-managed+ 16)
(define-constant +resource-storage-mode-private+ 32)
(define-constant +resource-storage-mode-memoryless+ 48)

(define-constant +vertex-format-invalid+ 0)
(define-constant +vertex-format-uchar+ 45)
(define-constant +vertex-format-uchar2+ 1)
(define-constant +vertex-format-uchar3+ 2)
(define-constant +vertex-format-uchar4+ 3)

(define-constant +vertex-format-char+ 46)
(define-constant +vertex-format-char2+ 4)
(define-constant +vertex-format-char3+ 5)
(define-constant +vertex-format-char4+ 6)

(define-constant +vertex-format-ushort+ 49)
(define-constant +vertex-format-ushort2+ 13)
(define-constant +vertex-format-ushort3+ 14)
(define-constant +vertex-format-ushort4+ 15)

(define-constant +vertex-format-short+ 50)
(define-constant +vertex-format-short2+ 16)
(define-constant +vertex-format-short3+ 17)
(define-constant +vertex-format-short4+ 18)

(define-constant +vertex-format-short+ 50)
(define-constant +vertex-format-short2+ 16)
(define-constant +vertex-format-short3+ 17)
(define-constant +vertex-format-short4+ 18)

(define-constant +vertex-format-half+ 53)
(define-constant +vertex-format-half2+ 25)
(define-constant +vertex-format-half3+ 26)
(define-constant +vertex-format-half4+ 27)

(define-constant +vertex-format-float+ 28)
(define-constant +vertex-format-float2+ 29)
(define-constant +vertex-format-float3+ 30)
(define-constant +vertex-format-float4+ 31)

(define-constant +vertex-format-uint+ 36)
(define-constant +vertex-format-uint2+ 37)
(define-constant +vertex-format-uint3+ 38)
(define-constant +vertex-format-uint4+ 39)

(define-constant +vertex-format-int+ 32)
(define-constant +vertex-format-int2+ 33)
(define-constant +vertex-format-int3+ 34)
(define-constant +vertex-format-int4+ 35)

(define-constant +vertex-step-function-constant+ 0)
(define-constant +vertex-step-function-per-vertex+ 1)
(define-constant +vertex-step-function-per-instance+ 2)
(define-constant +vertex-step-function-per-patch+ 3)
(define-constant +vertex-step-function-per-patch-control-point+ 4)

(define-constant +compare-function-never+ 0)
(define-constant +compare-function-less+ 1)
(define-constant +compare-function-equal+ 2)
(define-constant +compare-function-less-equal+ 3)
(define-constant +compare-function-greater+ 4)
(define-constant +compare-function-not-equal+ 5)
(define-constant +compare-function-greater-equal+ 6)

(define-constant +pixel-format-a8-unorm+ 1)
(define-constant +pixel-format-r8-unorm+ 10)
(define-constant +pixel-format-r8-unorm-srgb+ 11)
(define-constant +pixel-format-r8-snorm+ 12)
(define-constant +pixel-format-r8-uint+ 13)
(define-constant +pixel-format-r8-sint+ 14)

(define-constant +pixel-format-r16-unorm+ 20)
(define-constant +pixel-format-r16-snorm+ 22)
(define-constant +pixel-format-r16-uint+ 23)
(define-constant +pixel-format-r16-sint+ 24)
(define-constant +pixel-format-r16-float+ 25)
(define-constant +pixel-format-rg8-unorm+ 30)
(define-constant +pixel-format-rg8-unorm-srgb+ 31)
(define-constant +pixel-format-rg8-snorm+ 32)
(define-constant +pixel-format-rg8-uint+ 33)
(define-constant +pixel-format-rg8-sint+ 34)

(define-constant +pixel-format-b5g6r5-unorm+ 40) ; ╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴ packed
(define-constant +pixel-format-a1bgr5-unorm+ 41)
(define-constant +pixel-format-abgr4-unorm+ 42)
(define-constant +pixel-format-bgr5a1-unorm+ 43)

(define-constant +pixel-format-r32-uint+ 53)
(define-constant +pixel-format-r32-sint+ 54)
(define-constant +pixel-format-r32-float+ 55)
(define-constant +pixel-format-rg16-unorm+ 60)
(define-constant +pixel-format-rg16-snorm+ 62)
(define-constant +pixel-format-rg16-uint+ 63)
(define-constant +pixel-format-rg16-sint+ 64)
(define-constant +pixel-format-rg16-float+ 65)
(define-constant +pixel-format-rgba8-unorm+ 70)
(define-constant +pixel-format-rgba8-unorm-srgb+ 71)
(define-constant +pixel-format-rgba8-snorm+ 72)
(define-constant +pixel-format-rgba8-uint+ 73)
(define-constant +pixel-format-rgba8-sint+ 74)
(define-constant +pixel-format-bgra8-unorm+ 80)
(define-constant +pixel-format-bgra8-unorm-srgb+ 81)

(define-constant +pixel-format-depth16-unorm+ 250)
(define-constant +pixel-format-depth32-float+ 252)
(define-constant +pixel-format-stencil8+ 253)
(define-constant +pixel-format-depth24-unorm-stencil8+ 255)
(define-constant +pixel-format-depth32-float-stencil8+ 260)
(define-constant +pixel-format-x32-stencil8+ 261)
(define-constant +pixel-format-x24-stencil8+ 262)
