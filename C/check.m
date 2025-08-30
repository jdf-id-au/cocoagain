// Check the lib etc actually work before struggling in lisp...

#import "application.h"
#import "view.h"

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

BOOL setError(NSError **errout) {
  *errout = [NSError errorWithDomain: @"replacement"
                                code: 1234
                            userInfo: nil];
  return YES;
}

struct fakeNSRange {
  unsigned long location;
  unsigned long length;
};

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
  [app setDelegate:app];

  // ────────────────────────────────────────────────────────────── Metal buffer
  id device = MTLCreateSystemDefaultDevice();
  id<MTLBuffer> buf = [device newBufferWithLength: 36
                                          options: MTLResourceStorageModeManaged];
  float *cont = [buf contents];
  for (int i = 0; i < 9; i++) cont[i] = (float)i;
  
  NSLog(@"float is %lu bytes", sizeof(float));
  NSRange range = NSMakeRange(0, 36);
  struct fakeNSRange fake_range = (struct fakeNSRange){0, 36};
  //[buf didModifyRange: range];
  [buf didModifyRange: *(NSRange *)&fake_range];
  [MetalView in:buf at: 0 didModify: 36];
  // ─────────────────────────────────────────────────────────────────── logging
  //NSLog(@"About to run");
  // ───────────────────────────────────────────────────────── message with bool
  //unsigned char yep = 1;
  //NSNumber *mmhmm = [NSNumber numberWithBool: yep];
  //NSLog(@"numberWithBool: %@", [mmhmm stringValue]);
  // ──────────────────────────────────────────────────── error as out parameter
  id err = [NSError errorWithDomain: @"actually there's no"
                               code: 0
                           userInfo: nil];
  NSLog(@"%@", [err localizedDescription]);
  BOOL result = setError(&err);
  NSLog(@"%hhd: %@", result, [err localizedDescription]);
  // ───────────────────────────────────────────────────────────────────────────
  [app run];
  [pool release];
}
