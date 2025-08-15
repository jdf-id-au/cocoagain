#import <Cocoa/Cocoa.h>

typedef void (*TimerFn)(int);

@interface Timer: NSObject {
  int mID;
  TimerFn mTimerFn;
  NSTimer* mTimer;
}
@end
