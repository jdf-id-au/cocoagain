#import <Cocoa/Cocoa.h>

typedef void(*HandleFn)(int id, int action);

enum {
  WINDOW_CLOSE = 0
};

@interface Window: NSWindow<NSWindowDelegate> {
  int mID;
  // Remember settings for to/from fullscreen:
  NSString *mTitle;
  NSRect mFrame;
  NSWindowStyleMask mStyleMask;
  HandleFn mHandleFn;
}
@property(assign, nonatomic) bool isFullscreen;
@end
