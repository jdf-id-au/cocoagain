(in-package :metal-kit)

;; FIXME 2025-09-18 05:43:52 ns:autorelease new* calls??

;; ─────────────────────────────────────────────────────────────────────── Types

;; Also see :spatial and :core-foundation packages.

(ut:bidi-ffi (origin :type :unsigned-long) x y z)
(ut:bidi-ffi (size :type :unsigned-long) w h d)
(ut:bidi-ffi (viewport) x y w h near far)
(ut:bidi-ffi (region :type :unsigned-long) x y z w h d)

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

;; TODO 2025-08-31 23:19:00 compare with setVertexBufferOffset:atIndex:
;; Updates an entry in the vertex shader argument table with a new
;; location within the entry’s current buffer.
(defun set-vertex-buffer (command-encoder buffer &key (offset 0) (index 0))
  "Assigns a buffer to an entry in the vertex shader argument table." ; also see vertex descriptor section
  (ns:objc command-encoder "setVertexBuffer:offset:atIndex:"
           :pointer buffer
           :int offset ; alignment requirements...
           :int index)) 
;; TODO 2025-08-31 23:19:03 setVertexBuffers:offsets:withRange: low priority

(defun set-vertex-bytes (command-encoder ptr length &key (index 0))
  "Creates a buffer from bytes and assigns it to an entry in the vertex shader argument table.
Avoid MTLBuffer creation overhead, only suitable for <4KB, e.g. uniforms."
  (ns:objc command-encoder "setVertexBytes:length:atIndex:"
           :pointer ptr
           :int length
           :int index))

(defun set-fragment-buffer (command-encoder buffer &key (offset 0) (index 0))
  (ns:objc command-encoder "setFragmentBuffer:offset:atIndex:" :pointer buffer
							       :int offset
							       :int index))

(defun set-fragment-bytes (command-encoder ptr length &key (index 0))
  "Creates a buffer from bytes and assigns it to an entry in the fragment shader argument table.
Avoid MTLBuffer creation overhead, only suitable for <4KB, e.g. uniforms."
  (ns:objc command-encoder "setFragmentBytes:length:atIndex:"
           :pointer ptr
           :int length
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
  (ns:protect ; TODO 2025-09-18 04:54:20 confirm autorelease semantics
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

(defun set-color-attachment-pixel-format (pipeline-descriptor index
                                          &optional
                                            (pixel-format
                                             #+x86-64 mtl::PixelFormatA8Unorm
                                             #+arm64 mtl::PixelFormatBGRA8Unorm))
  (let* ((color-attachment
	   (ns:objc (ns:objc pipeline-descriptor "colorAttachments" :pointer)
		    "objectAtIndexedSubscript:" :int index :pointer)))
    (ns:objc color-attachment "setPixelFormat:" :int pixel-format)))

(defun set-color-attachment-blending-enabled (pipeline-descriptor index state)
  (let* ((color-attachment
           (ns:objc (ns:objc pipeline-descriptor "colorAttachments" :pointer)
                    "objectAtIndexedSubscript:" :int index :pointer)))
    ;; TODO 2025-08-31 16:26:27 test BOOL passing bullshit
    (ns:objc color-attachment "setBlendingEnabled:" :char (if state 1 0))))

(defun set-color-attachment-blend-factor (pipeline-descriptor index
                                          where which state)
  (let* ((color-attachment
           (ns:objc (ns:objc pipeline-descriptor "colorAttachments" :pointer)
                    "objectAtIndexedSubscript:" :int index :pointer)))
    (ns:objc color-attachment
             (concatenate 'string
                          "set"
                          (ecase where (:source "Source")(:dest "Destination"))
                          (ecase which (:rgb "RGB") (:alpha "Alpha"))
                          "BlendFactor:")
             :int state)))

;; TODO 2025-08-31 16:01:51 blendingEnabled alphaBlendingOperation rgbBlendOperation
;; ... from MTLRenderPipelineColorAttachmentDescriptor

;; (defun set-depth-attachment-pixel-format (pipeline-descriptor pixel-format)
;;   (ns:objc pipeline-descriptor "setDepthAttachmentPixelFormat:" :unsigned-int pixel-format))

;; TODO 2025-08-31 16:11:52 stencilAttachmentPixelFormat vs MTKview depthStencilPixelFormat ?

;; ─────────────────────────────────────────────────────────── Vertex descriptor
;; https://metalbyexample.com/vertex-descriptors/

(defun make-vertex-descriptor ()
  (ns:new "MTLVertexDescriptor"))

(defun set-vertex-descriptor-attribute (vertex-descriptor attribute-index format
                                        &key (offset 0) (index 0))
  (assert (<= 0 attribute-index 31) nil "Index out of range") ; Metal-Feature-Set-Tables.pdf
  (let* ((attribute (ns:objc (ns:objc vertex-descriptor "attributes" :pointer)
                             "objectAtIndexedSubscript:" :int attribute-index :pointer)))
    ;; The format of the vertex attribute.
    (ns:objc attribute "setFormat:" :int format)
    ;; The location of an attribute in vertex data, determined by the
    ;; byte offset from the start of the vertex data.
    (ns:objc attribute "setOffset:" :int offset)
    ;; The index in the argument table for the associated vertex buffer.
    (ns:objc attribute "setBufferIndex:" :int index)))

(defun set-vertex-descriptor-layout (vertex-descriptor layout-index stride &optional step-rate step-function)
  (let* ((layout (ns:objc (ns:objc vertex-descriptor "layouts" :pointer)
                          ;; one layout per buffer when vertex data in multiple buffers
                          "objectAtIndexedSubscript:" :int layout-index :pointer)))
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
    (when step-rate
      (ns:objc layout "setStepRate:" :int step-rate))
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
    (when step-function
      (ns:objc layout "setStepFunction:" :int step-function))))

;; ────────────────────────────────────────────────────────────── Pipeline state

#+nil(defun make-render-pipeline-state (device pipeline-descriptor)
  (cffi:with-foreign-object (err :pointer)
    (let ((e (ns:objc "NSError" "errorWithDomain:code:userInfo:"
                      :string "placeholder"
                      :int 0
                      :pointer (cffi:null-pointer)))) ; TODO 2025-08-16 19:50:42 how?
      (setf (cffi:mem-ref err :pointer) e))
    (let ((p
            (ns:objc device "newRenderPipelineStateWithDescriptor:error:" ; TODO 2025-09-18 04:57:25 autorelease
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

(defun new-buffer (device length &optional (options (+ mtl::ResourceCPUCacheModeDefaultCache
                                                       mtl::ResourceStorageModeManaged)))
  (ns:protect (ns:objc device "newBufferWithLength:options:"
                        :int length
                        :int options
                        :pointer)
              "Failed to allocate buffer."))

(defun make-buffer (device data length &optional (options (+ mtl::ResourceCPUCacheModeDefaultCache
                                                             mtl::ResourceStorageModeManaged)))
  "Copies data."
  (ns:objc device "newBufferWithBytes:length:options:"
           :pointer data
           :int length
           :int options
           :pointer))

(defun buffer-contents (buffer)
  (ns:objc buffer "contents" :pointer))

;; ───────────────────────────────────────────────────────────────────── Texture

(defun texture-2d-descriptor (pixel-format width height mipmap)
  (ns:objc "MTLTextureDescriptor" "texture2DDescriptorWithPixelFormat:width:height:mipmapped:"
	   :int pixel-format
	   :int width
	   :int height
	   :bool mipmap
	   :pointer))

(defun texture-cube-descriptor (pixel-format width height mipmap)
  (ns:objc "MTLTextureDescriptor" "textureCubeDescriptorWithPixelFormat:width:height:mipmapped:"
	   :int pixel-format
	   :int width
	   :int height
	   :bool mipmap
	   :pointer))

(defun texture-buffer-descriptor (pixel-format width options usage)
  (ns:objc "MTLTextureDescriptor" "textureBufferDescriptorWithPixelFormat:width:resourceOptions:usage:"
	   :int pixel-format
	   :int width
	   :int options
	   :int usage
	   :pointer))

(defun make-device-texture (device descriptor)
  (ns:objc device "newTextureWithDescriptor:" :pointer descriptor :pointer))

(defun make-buffer-texture (buffer descriptor stride &key (offset 0))
  (ns:objc buffer "newTextureWithDescriptor:offset:bytesPerRow:"
           :pointer descriptor
           :int offset
           :int stride
           :pointer))

(defun replace-region (texture region mipmap-level data bpr)
  (ns:objc texture "replaceRegion:mipmapLevel:withBytes:bytesPerRow:"
	   (:struct region) region
	   :int mipmap-level
	   :pointer data
	   :int bpr))

;; ──────────────────────────────────────────────────────────────── Clear colour
;; TODO 2025-08-16 22:57:26 move?

#+nil(progn ; unused, TODO 2025-09-07 18:17:21 remove?
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
         (ns:objc mtk-view "setClearColor:" (:struct clear-color) (make-clear-color red green blue alpha))))
