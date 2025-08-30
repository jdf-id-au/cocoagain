https://developer.apple.com/library/archive/documentation/Miscellaneous/Conceptual/MetalProgrammingGuide/Cmd-Submiss/Cmd-Submiss.html

(NB Metal 4 is Apple Silicon only... this isn't 4)

# Key
. = single
... = multiple
(x, y) = dependencies
a, b = parameters
*foo, bar* = comments
↑ = depends on parent
→ = steps
# Overview
shader library *reusable*
  - function ... *e.g. vertex, fragment*

render pipeline .
  - color attachment ... format
  - depth attachment . format
  - vertex function .
  - fragment function .
  - vertex descriptor .
  - command queue .

vertex descriptor .
  - attribute ... format, offset, index into buffer
  - layout ... stride, step rate, step function

command queue . *reusable, generally thread-safe*
  - command buffer ... *transient, autoreleased*
    - command encoder ... *transient, autoreleased, one at a time per command buffer*
      → present drawable
      → commit

Normally whole frame is rendered using one command buffer. Multiple
command buffers if multithreaded. (MTLParallelRenderCommandEncoder
allows one pass to be split across multiple encoders and threads...)

command encoder, *for given command buffer and render pass, and pipeline state*
  - pipeline state .  *reusable, for given device and render pipeline*
  - depth/stencil states... *reusable*
  - (vertex) buffers... *reusable*
  - textures... *reusable*

TODO 2025-08-30 13:21:42 understand how to use multiple shaders.
One pipeline state for each combination?
# Practical hierarchy
- command queue . (device)
	- pipeline state ... (view, pipeline)
	- command buffer ... *per frame* (↑)
		- pass ... (view)
		- command encoder ... *as needed* (↑, pass, pipeline state)
		  *i.e. for new pass properties/encoder types*
		  *Only one command encoder active at a time for command buffer (unless parallel).*

Scenes requiring multiple shaders would therefore have:
- 1 pipeline state (and therefore pipeline) per shader combination
- ≥1 command encoder per pipeline state