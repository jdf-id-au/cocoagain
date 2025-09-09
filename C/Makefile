CFLAGS=-std=c17 -g3 -pedantic -Wall -Wextra \
-fPIC \
-march=native
# ...increases likelihood of using appropriate SIMD?
# need -O something too? -ffast-math? no?
# -fsanitize=address,undefined # asan breaks cffi iirc; maybe fixed libffi 3.4.7?


OBJCFLAGS=-fno-objc-arc -fmodules -mmacos-version-min=13.0

FRAMEWORKS=-framework Cocoa -framework QuartzCore \
-framework Metal -framework MetalKit \
-framework Accelerate

../libcocoagain.dylib: application.m window.m view.m timer.m spatial.m
	cc -dynamiclib $(CFLAGS) $(OBJCFLAGS) $(FRAMEWORKS) $^ -o $@

constants: constants.m
	cc $(CFLAGS) $(OBJCFLAGS) $(FRAMEWORKS) $< -o $@

../constants.lisp: constants
	./$< > $@

# Prepare compliation db (for lsp...) with $ bear -- make check
check: check.m ../libcocoagain.dylib
	cc $(CFLAGS) $(OBJCFLAGS) $(FRAMEWORKS) -lcocoagain -I./ -L../ $< -o $@

