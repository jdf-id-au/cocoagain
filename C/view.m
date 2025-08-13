#import <Cocoa/Cocoa.h>

typedef void(*DrawFn)(int,int,void*,void*,int,int);
typedef void(*EventFn)(int iID, int type, NSEvent *,
                       double locationX, double locationY);

enum {
  INIT = 0,
  DRAW,
  RESHAPE,
  SHUTDOWN
};

enum {
  DOWN = 0,
  DRAGGED,
  UP,
  MOVED,
  SCROLLWHEEL,
  KEY_DOWN
};

@interface View: NSView {
  DrawFn mDrawFn;
  EventFn mEventFn;
  NSTrackingArea *trackingArea;
}
@property(readonly, nonatomic) int id;
@end

@implementation View
-(id) initWithID: (int) iID
           frame: (NSRect) iFrame
          drawFn: (DrawFn) iDrawFn
         eventFn: (EventFn) iEventFn {
  self = [super initWithFrame: iFrame];
  _id = iID; // NB 2025-08-13 10:30:48 property internal access?
  mDrawFn = iDrawFn;
  mEventFn = iEventFn;
  mDrawFn(self.id, INIT, NULL, NULL, iFrame.size.width, iFrame.size.height);
  int opts = (NSTrackingActiveAlways | NSTrackingMouseEnteredAndExited |
              NSTrackingMouseMoved | NSTrackingInVisibleRect);
  trackingArea = [[NSTrackingArea alloc] initWithRect: [self bounds]
                                              options: opts
                                                owner: self
                                             userInfo: nil];
  [self addTrackingArea: trackingArea];
  return self;
}
-(void) drawRect:(NSRect) frame {
  mDrawFn(self.id, DRAW, NULL, NULL, frame.size.width, frame.size.height);
}
-(void) dealloc {
  NSRect frame = [self bounds];
  mDrawFn(self.id, SHUTDOWN, NULL, NULL, frame.size.width, frame.size.height);
  if(trackingArea != nil) {
    [self removeTrackingArea: trackingArea];
    [trackingArea release];
  }
  [super dealloc];
}
-(void) updateTrackingAreas { // not very DRY
  if(trackingArea != nil) {
    [self removeTrackingArea: trackingArea];
    [trackingArea release];
  }
  int opts = (NSTrackingActiveAlways | NSTrackingMouseEnteredAndExited |
              NSTrackingMouseMoved | NSTrackingInVisibleRect);
  trackingArea = [[NSTrackingArea alloc] initWithRect: [self bounds]
                                              options: opts
                                                owner: self
                                             userInfo: nil];
  [self addTrackingArea: trackingArea];
}
-(BOOL) acceptsFirstResponder { return YES; }
-(void) keyDown:(NSEvent *) e {
  mEventFn(self.id, KEY_DOWN, e, 0, 0);
}
-(void) mouseDown:(NSEvent *) e {
  NSPoint point = [self convertPoint: [e locationInWindow]
                            fromView: nil];
  mEventFn(self.id, DOWN, e, point.x, point.y);
}
-(void) mouseDragged:(NSEvent *) e {
  NSPoint point = [self convertPoint: [e locationInWindow]
                            fromView: nil];
  mEventFn(self.id, DRAGGED, e, point.x, point.y);
}
-(void) mouseUp:(NSEvent *) e {
  NSPoint point = [self convertPoint: [e locationInWindow]
                            fromView: nil];
  mEventFn(self.id, UP, e, point.x, point.y);
}
-(void) mouseMoved:(NSEvent *) e {
  NSPoint point = [self convertPoint: [e locationInWindow]
                            fromView: nil];
  mEventFn(self.id, MOVED, e, point.x, point.y);
}
-(void) scrollWheel:(NSEvent *) e {
  NSPoint point = [self convertPoint: [e locationInWindow]
                            fromView: nil];
  mEventFn(self.id, SCROLLWHEEL, e, point.x, point.y);  
}
@end
