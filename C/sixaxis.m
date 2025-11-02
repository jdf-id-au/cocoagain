// After 3DxSNAxisDemo, minus all the objc bullshit; not working yet

#include <stdio.h>
#import <MacTypes.h>
#import <IOKit/IOTypes.h>
#import <Cocoa/Cocoa.h>
#import <3DConnexionClient/ConnexionClient.h>
#import <3DConnexionClient/ConnexionClientAPI.h>

UInt16 client_id;

void handler(io_connect_t connection, natural_t messageType, void *messageArgument) {
  NSLog(@"🍎 Handle");
  ConnexionDeviceState *state;
  ConnexionDevicePrefs prefs;
  OSErr error = 0;
  int32_t vidPid;

  switch (messageType) {
  case kConnexionMsgDeviceState:
    state = (ConnexionDeviceState *)messageArgument;
    if (state->client != client_id) return;
    switch (state->command) {
    case kConnexionCmdHandleAxis:
      printf("tx %d ty %d tz %d rx %d ry %d rz %d\n", state->axis[0],state->axis[1],state->axis[2],state->axis[3],state->axis[4],state->axis[5]);
      break;
    case kConnexionCmdHandleButtons: // fall through
    case kConnexionCmdAppSpecific:   // fall through
    case kConnexionCmdHandleRawData:
      // ConnexionControl(kConnexionCtlGetDeviceID, 0, &vidPid);
      // error = ConnexionGetCurrentDevicePrefs(kDevID_AnyDevice, &prefs);
      printf("buttons %d\n", state->buttons);
      break;
    }
    break;
  default:
    break;
  }
}

@interface App : NSApplication <NSApplicationDelegate> {
}
@end

@implementation App
- (void)applicationDidFinishLaunching:(NSNotification *)n {
  NSLog(@"DidFinishLaunching");
  OSErr error = SetConnexionHandlers(handler, 0, 0, NO);

  // Doesn't call handler... because wrong RegisterConnexionClient
  // signature or name? Can't pick out Cocoa/NSApp/.app magic which
  // works in vendor demo... Heaps of init logging though, complete
  // with typos...

  // CFBundleSignature: application's creator signature or 0
  client_id = RegisterConnexionClient('jdfc', 0, kConnexionClientModeTakeOver, kConnexionMaskAll);
  
  // actual executable name or 0 (but would need valid CFBundleSignature value)
  //client_id = RegisterConnexionClient(0, "\psixaxis", kConnexionClientModeTakeOver, kConnexionMaskAll);

  if (client_id) SetConnexionClientButtonMask(client_id, kConnexionMaskAllButtons);
  
  int device_id = 0;
  ConnexionClientControl(client_id, kConnexionCtlGetDeviceID, 0, &device_id);
  device_id = device_id & 0xffff;

  NSLog(@"🍏 Client %x Device %x", client_id, device_id);
  // id loop = [[NSRunLoop alloc] init];
  // while (true) [loop runMode:NSDefaultRunLoopMode beforeDate:[NSDate
  // distantFuture]];

  // TODO try running thread like demo then maybe give up :(
}
- (void)applicationWillTerminate:(NSNotification *)n {
  UnregisterConnexionClient(client_id);
  CleanupConnexionHandlers();
}
@end

int main(int argc, char *argv[]) {
  @autoreleasepool {
    NSLog(@"Starting");
    id app = [App sharedApplication];

    NSRect frame = [[NSScreen mainScreen] frame];
    
    NSWindowStyleMask style =
			NSWindowStyleMaskTitled |
			NSWindowStyleMaskClosable |
			NSWindowStyleMaskMiniaturizable |
			NSWindowStyleMaskResizable;
		NSWindow *window = [[NSWindow alloc]
													initWithContentRect:frame
																		styleMask:style
																			backing:NSBackingStoreBuffered
																				defer:false];
    window.title = @"spacenav pain";
    [window makeKeyAndOrderFront:nil];
    [NSApp setDelegate: app];
    [NSApp run];
  }
}
