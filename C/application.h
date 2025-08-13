#import <Cocoa/Cocoa.h>

@interface App: NSApplication<NSApplicationDelegate> {
  void(*delegateCallback)(int);
  void(*widgetCallback)(id);
}
-(void) setDelegateCallback: (void(*)(int)) cb;
-(void) setWidgetCallback: (void(*)(id)) cb;
@end
