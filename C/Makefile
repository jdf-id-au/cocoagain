CFLAGS=-std=c17 -g3 -pedantic -Wall -Wextra \
-fPIC -fsanitize=address,undefined
OBJCFLAGS=-fno-objc-arc -fmodules -mmacos-version-min=12.4
FRAMEWORKS=-framework Cocoa -framework QuartzCore \
-framework Metal -framework MetalKit

../libcocoagain.dylib: \
application.m window.m view.m
	cc -dynamiclib $(CFLAGS) $(OBJCFLAGS) $(FRAMEWORKS) $^ -o $@

../check: check.m ../libcocoagain.dylib
	cc $(CFLAGS) $(OBJCFLAGS) $(FRAMEWORKS) -lcocoagain $< -o $@
