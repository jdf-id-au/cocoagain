# jdf/cocoagain

**Core Graphics** + **MetalKit** — **Common Lisp** REPL

Read eval graphics loop, take two! In the grand tradition of (but much less sophisticated than):

- https://github.com/cbaggers/cepl
- https://github.com/byulparan/cl-visual ([cl-nextstep](https://github.com/byulparan/cl-nextstep) was the starting point for this project!)
- https://github.com/digego/extempore
- https://www.shadertoy.com/
- https://github.com/oakes/play-cljc
  
Also see the very half-baked [`jdf/render`](https://github.com/jdf-id-au/render)  which uses BGFX and Clojure, and [`repl-frame`](https://github.com/jdf-id-au/comfort/blob/master/src/comfort/ui.clj) from `jdf/comfort` which uses Swing and Clojure.

Other notable projects:

- https://github.com/Shinmera/cocoas
- https://github.com/apr3vau/objc

## Getting started

See `example.lisp`.

## TODO

- more Metal resource management
- test multi-shader scene incl blending
- dynamic controls view, value binding? vs immediate mode something?
