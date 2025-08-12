// after byulparan/cl-nextstep, slower so I can understand

#import <Cocoa/Cocoa.h>

@interface App: NSApplication<NSApplicationDelegate> {
  void(*delegateCallback)(int);
  void(*widgetCallback)(id);
}
@end

@implementation App
-(void) setDelegateCallback: (void(*)(int)) cb { delegateCallback = cb; }
-(void) setWidgetCallback: (void(*)(id)) cb { widgetCallback = cb; }
-(void) terminate: (id) sender {
  dispatch_async(dispatch_get_main_queue(), ^{[super terminate: sender];});
}
-(void) applicationDidFinishLaunching: (NSNotification *) n { delegateCallback(0); }
-(void) applicationWillTerminate: (NSNotification *) n { delegateCallback(2); }
-(BOOL) applicationSupportsSecureRestorableState: (NSApplication *) a { return YES; }
-(IBAction) widgetHandle: (id) sender { widgetCallback(sender); }
@end
