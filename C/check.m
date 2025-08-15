#import "application.h"

void delegateCb(int action) {
  NSArray *windows;
  switch (action) {
  case 0:
    // startup functions
    break;
  case 1:
    windows = [[App sharedApplication] windows];
    for (int i = 0; i <  (int)[windows count]; i++)
      [[windows objectAtIndex: i] performClose: 0];
    break;
  case 2:
    // exit functions
    break;
  }
}

void widgetCb(id wId) {
}

// Check the lib actually works before struggling in lisp...
int main(void) {
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  App *app = [App sharedApplication];

  NSRect frame = [[NSScreen mainScreen] frame];
  NSWindowStyleMask style =
    NSWindowStyleMaskTitled |
    NSWindowStyleMaskClosable |
    NSWindowStyleMaskMiniaturizable |
    NSWindowStyleMaskResizable;
  NSWindow *window = [[NSWindow alloc]
                       initWithContentRect: frame
                                 styleMask: style
                                   backing: NSBackingStoreBuffered
                                     defer: false];
  window.title = @"Struggle street";
  [window makeKeyAndOrderFront: nil];
  
  [app activateIgnoringOtherApps: YES];
  [app setDelegateCallback: delegateCb];
  [app setWidgetCallback: widgetCb];
  [app setDelegate: app];
  //NSLog(@"About to run");
  unsigned char yep = 1;
  NSNumber *mmhmm = [NSNumber numberWithBool: yep];
  NSLog(@"numberWithBool: %@", [mmhmm stringValue]);
  [app run];
  [pool release];
}
