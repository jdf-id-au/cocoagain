#import <Cocoa/Cocoa.h>
#include <time.h>

typedef void (*TimerFn)(int id, uint64_t nanos);

@interface Timer: NSObject {
  int mID;
  TimerFn mTimerFn;
  NSTimer* mTimer;
}
@end
