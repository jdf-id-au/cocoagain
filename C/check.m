#import <Cocoa/Cocoa.h>



// Check the lib actually works before struggling in lisp...
int main(void) {
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  NSApplication *app = [App sharedApplication];
  [app activateIgnoringOtherApps: YES];
  [app setDelegateCallback: _];
  [app setWidgetCallback: _];
  [app run];
  [pool release];
}
