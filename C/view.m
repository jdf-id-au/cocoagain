#import "view.h"

#define EVENT_TRACKING_METHODS                                           \
-(void) updateTrackingAreas {                                            \
  if(trackingArea != nil) {                                              \
    [self removeTrackingArea: trackingArea];                             \
    [trackingArea release];                                              \
  }                                                                      \
  int opts = (NSTrackingActiveAlways | NSTrackingMouseEnteredAndExited | \
              NSTrackingMouseMoved | NSTrackingInVisibleRect);           \
  trackingArea = [[NSTrackingArea alloc] initWithRect: [self bounds]     \
                                              options: opts              \
                                                owner: self              \
                                             userInfo: nil];             \
  [self addTrackingArea: trackingArea];                                  \
}                                                                        \
-(BOOL) acceptsFirstResponder { return YES; }                            \
-(void) keyDown:(NSEvent *) e {                                          \
  mEventFn(self.id, KEY_DOWN, e, 0, 0);                                  \
}                                                                        \
-(void) mouseDown:(NSEvent *) e {                                        \
  NSPoint point = [self convertPoint: [e locationInWindow]               \
                            fromView: nil];                              \
  mEventFn(self.id, DOWN, e, point.x, point.y);                          \
}                                                                        \
-(void) mouseDragged:(NSEvent *) e {                                     \
  NSPoint point = [self convertPoint: [e locationInWindow]               \
                            fromView: nil];                              \
  mEventFn(self.id, DRAGGED, e, point.x, point.y);                       \
}                                                                        \
-(void) mouseUp:(NSEvent *) e {                                          \
  NSPoint point = [self convertPoint: [e locationInWindow]               \
                            fromView: nil];                              \
  mEventFn(self.id, UP, e, point.x, point.y);                            \
}                                                                        \
-(void) mouseMoved:(NSEvent *) e {                                       \
  NSPoint point = [self convertPoint: [e locationInWindow]               \
                            fromView: nil];                              \
  mEventFn(self.id, MOVED, e, point.x, point.y);                         \
}                                                                        \
-(void) scrollWheel:(NSEvent *) e {                                      \
  NSPoint point = [self convertPoint: [e locationInWindow]               \
                            fromView: nil];                              \
  mEventFn(self.id, SCROLLWHEEL, e, point.x, point.y);                   \
}
  

@implementation View // ─────────────────────────────────────────────────── View
-(id) initWithID: (int) iID
           frame: (NSRect) iFrame
          drawFn: (DrawFn) iDrawFn
         eventFn: (EventFn) iEventFn {
  self = [super initWithFrame: iFrame];
  _id = iID;
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
EVENT_TRACKING_METHODS
@end

@implementation MetalView // ───────────────────────────────────────── MetalView
-(id) initWithFrame: (CGRect) iFrame
             device: (id<MTLDevice>) iDevice
                 id: (int) iID
             drawFn: (DrawFn) iDrawFn
            eventFn: (EventFn) iEventFn {
  self = [super initWithFrame: iFrame device: iDevice];
  _id = iID;
  mDrawFn = iDrawFn;
  mEventFn = iEventFn;
  int opts = (NSTrackingActiveAlways | NSTrackingMouseEnteredAndExited |
              NSTrackingMouseMoved | NSTrackingInVisibleRect);
  trackingArea = [[NSTrackingArea alloc] initWithRect: [self bounds]
                                              options: opts
                                                owner: self
                                             userInfo: nil];
  [self addTrackingArea: trackingArea];
  return self;
}
-(void) mtkView: (MTKView *)view drawableSizeWillChange: (CGSize)size {
  mDrawFn(self.id, RESHAPE, NULL, NULL, size.width, size.height);
}
-(void) drawInMTKView: (MTKView *)view {
  mDrawFn(self.id, DRAW, NULL, NULL, view.bounds.size.width, view.bounds.size.height);
}
-(id<MTLRenderPipelineState>) deviceRenderPipelineStateWithDescriptor: (MTLRenderPipelineDescriptor *) d {
  id e = [NSError errorWithDomain: @"dummy"
                               code: 0
                           userInfo: nil]; // only because can't alloc by interface
  id p = [self.device newRenderPipelineStateWithDescriptor: d
                                                     error: &e];
  if (p) return p;
  // NB 2025-08-16 10:47:11 this worked where attempted cffi equiv didn't...
  NSLog(@"Failed to create render pipeline state in objc: %@", [e localizedDescription]);
  return nil;
}
-(void) dealloc {
  mDrawFn(self.id, SHUTDOWN, NULL, NULL, self.bounds.size.width, self.bounds.size.height);
  if(trackingArea != nil) {
    [self removeTrackingArea: trackingArea];
    [trackingArea release];
  }
  [super dealloc];
}
EVENT_TRACKING_METHODS
@end
