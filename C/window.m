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

static NSMutableArray *fullscreenWindows = NULL;

@implementation Window
-(id) initWithID: (int) iID
           frame: (NSRect) iFrame
       styleMask: (int) iStyleMask
        handleFn: (HandleFn) iHandleFn {
  self = [super initWithContentRect: iFrame
                          styleMask: iStyleMask
                            backing: NSBackingStoreBuffered
                              defer: NO];
  if (!fullscreenWindows) fullscreenWindows = [[NSMutableArray alloc] init];
  mID = iID;
  mHandleFn = iHandleFn;
  self.isFullscreen = NO;
  return self;
}
-(void) windowWillClose: (NSNotification *) n {
  if (self.isFullscreen) [self exitFullscreen];
  mHandleFn(mID, WINDOW_CLOSE);
}
-(void) detectChangedViewSize: (NSNotification *) n {
  [[NSNotificationCenter defaultCenter]
    removeObserver: self
              name: NSViewFrameDidChangeNotification
            object: [n object]];
  NSRect frame = [self frame];
  NSRect contentRect = [self frameRectForContentRect: [[n object] frame]];
  NSRect newRect = NSMakeRect(frame.origin.x,
                              frame.origin.y - (contentRect.size.height - frame.size.height),
                              contentRect.size.width,
                              contentRect.size.height);
  [self setFrame: newRect display: YES];
  [[NSNotificationCenter defaultCenter]
    addObserver: self
       selector: @selector(detectChangedViewSize:)
           name: NSViewFrameDidChangeNotification
         object: [n object]];
}
-(void) enterFullscreen {
  mTitle = self.title;
  mFrame = self.frame;
  mStyleMask = self.styleMask;
  self.styleMask = NSWindowStyleMaskBorderless;
  [self setFrame: self.screen.frame display: YES];
  self.isFullscreen = YES;
  [fullscreenWindows addObject: self];
  if([fullscreenWindows count] == 1) 
    NSApp.presentationOptions = NSApplicationPresentationAutoHideMenuBar;
}
-(void) exitFullscreen {
  self.styleMask = mStyleMask;
  [self setFrame: mFrame display: YES];
  self.title = mTitle;
  self.isFullscreen = NO;
  [fullscreenWindows removeObject: self];
  if([fullscreenWindows count] == 0)
    NSApp.presentationOptions = NSApplicationPresentationDefault;
}
-(void) toggleFullscreen {
  NSResponder *view = [self firstResponder];
  if(self.isFullscreen) {
    [self exitFullscreen];
    [self makeFirstResponder: view];
  } else {
    [self enterFullscreen];
    [self makeFirstResponder: view];
  }
}
@end
