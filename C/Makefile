CFLAGS=-std=c17 -g3 -pedantic -Wall -Wextra \
-fPIC \
-march=native
# ...increases likelihood of using appropriate SIMD?
# need -O something too? -ffast-math? no?

# asan breaks cffi iirc; maybe fixed libffi 3.4.7?
SAN=-fsanitize=address,undefined

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

Sixaxis.app/Contents/MacOS/sixaxis: sixaxis.m
	cc $(CFLAGS) -fpascal-strings $(OBJCFLAGS) -framework IOKit -F/Library/Frameworks/ -framework 3DconnexionClient $< -o $@

sixaxis: Sixaxis.app/Contents/MacOS/sixaxis

.PHONY: sixaxis

sixaxisio: sixaxisio.m
	cc $(CFLAGS) $(SAN) $(OBJCFLAGS) -framework IOKit $< -o $@ 
