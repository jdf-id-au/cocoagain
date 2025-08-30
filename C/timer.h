#import <Cocoa/Cocoa.h>
#include <time.h>

typedef void (*TimerFn)(int id, double seconds);

@interface Timer: NSObject {
  int mID;
  TimerFn mTimerFn;
  NSTimer* mTimer;
}
@end
