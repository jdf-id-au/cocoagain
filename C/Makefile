CFLAGS=-std=c17 -g3 -pedantic -Wall -Wextra \
-fPIC \
-march=native
# ...increases likelihood of using appropriate SIMD?
# need -O something too? -ffast-math? no?
# -fsanitize=address,undefined # asan breaks cffi iirc; maybe fixed libffi 3.4.7?


OBJCFLAGS=-fno-objc-arc -fmodules -mmacos-version-min=15.0

FRAMEWORKS=-framework Cocoa -framework QuartzCore \
-framework Metal -framework MetalKit \
-framework Accelerate

# Prepare compliation db (for lsp...) with $ bear -- make
../libcocoagain.dylib: application.m window.m view.m timer.m wrap_spatial.m
	cc -dynamiclib $(CFLAGS) $(OBJCFLAGS) $(FRAMEWORKS) $^ -o $@

constants: constants.m
	cc $(CFLAGS) $(OBJCFLAGS) $(FRAMEWORKS) $< -o $@

../constants.lisp: constants
	./$< > $@

check: check.m ../libcocoagain.dylib
	cc $(CFLAGS) $(OBJCFLAGS) $(FRAMEWORKS) -lcocoagain -I./ -L../ $< -o $@

spatial_fns.txt:
	./fetch_spatial.sh > spatial_fns.txt
