#include <CoreFoundation/CFBase.h>
#include <CoreFoundation/CFString.h>
#include <stdio.h>
#include <IOKit/IOKitLib.h>
#include <IOKit/hid/IOHIDKeys.h>

int main(void) {
  // https://developer.apple.com/documentation/iokit/1514494-ioservicegetmatchingservices?language=objc
  @autoreleasepool {
    kern_return_t kr;
    io_iterator_t existing;
    io_object_t service;
    CFMutableDictionaryRef matching =
      IOServiceMatching("AppleUserHIDEventService");
    kr = IOServiceGetMatchingServices(kIOMainPortDefault, matching, &existing);
    if (kr != KERN_SUCCESS) {
      CFRelease(matching);
      return kr;
    }
    while ((service = IOIteratorNext(existing))){
      io_name_t name;
        kr = IORegistryEntryGetName(service, name);
      if (kr == KERN_SUCCESS) {
        printf("service %x name: %s\n", service, name);
        // oh so tedious
        // IOHIDDeviceInterface getReport
        // IOHIDDeviceDeviceInterface [sic] getReport
        // IOUSBDeviceInterface GetDeviceProduct RegisterForNotification
      }
      IOObjectRelease(service);
    }
  } 
  return 0;
}
