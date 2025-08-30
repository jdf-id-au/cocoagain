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
- (void)timerHandle:(NSTimer *)timer {
  uint64_t nanos = clock_gettime_nsec_np(CLOCK_REALTIME);
  uint64_t B = 1000000000; // wait for C23 for ' ...
  uint64_t nanos_per_day = 24 * 60 * 60 * B;
  double seconds = (double)(nanos % nanos_per_day)/B;
  mTimerFn(mID, seconds);
}
-(void) invalidate {
  [mTimer invalidate];
}
@end
