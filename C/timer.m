#import "timer.h"

@implementation Timer
-(id) initWithID: (int) iID
         timerFn: (TimerFn) iTimerFn
   timerInterval: (double) interval {
  self = [super init];
  mID = iID;
  mTimerFn = iTimerFn;
  mTimer = [NSTimer timerWithTimeInterval: interval
                                   target: self
                                 selector: @selector(timerHandle:)
                                 userInfo: nil
                                  repeats: YES];
  [[NSRunLoop mainRunLoop] addTimer: mTimer
                            forMode: NSRunLoopCommonModes];
  return self;
}
-(void) timerHandle: (NSTimer*) timer {
  mTimerFn(mID, clock_gettime_nsec_np(CLOCK_REALTIME));
}
-(void) invalidate {
  [mTimer invalidate];
}
@end
