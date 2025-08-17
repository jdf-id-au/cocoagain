#import <Cocoa/Cocoa.h>
#import <Metal/Metal.h>
#import <MetalKit/MetalKit.h>

typedef void(*DrawFn)(int,int,void*,void*,int,int);
typedef void(*EventFn)(int iID, int type, NSEvent *e,
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

// https://developer.apple.com/documentation/MetalKit/MTKView?language=objc
@interface MetalView<MTKViewDelegate>: MTKView {
  DrawFn mDrawFn;
  EventFn mEventFn;
  NSTrackingArea *trackingArea;
}
// Workaround for lisp difficulty, see implementation.
-(id<MTLRenderPipelineState>) deviceRenderPipelineStateWithDescriptor: (MTLRenderPipelineDescriptor *) d;
@property(readonly, nonatomic) int id;
@end
