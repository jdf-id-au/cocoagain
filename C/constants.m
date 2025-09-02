#include <stdio.h> // printf
#include <string.h> // memcpy_s
#import "view.h"

// Obvious way

struct constant {
  int32_t value;
  char *name;
};

#define S(x) {x, #x}

struct constant constants[] = {
  S(MTLPrimitiveTypeTriangleStrip)
};

// Alternative approach

#define SPELL_ENUMS(...) \
  int32_t codes[] = {__VA_ARGS__};              \
  char *names = #__VA_ARGS__

SPELL_ENUMS(MTLPrimitiveTypePoint, MTLPrimitiveTypeLine,
            MTLPrimitiveTypeLineStrip,
            MTLPrimitiveTypeTriangle, MTLPrimitiveTypeTriangleStrip,
            
            );

#define BUFSZ 1024
int main(void) {
  // Obvious way
  // for (uint32_t i = 0; i < sizeof(constants) / sizeof(*constants); i++)
  //   printf("%s %d\n", constants[i].name, constants[i].value);

  // Alternative approach
  char *cur = names;
  int32_t i = -1;
  BOOL within = NO; // within a name string
  while (*cur) {
    switch (*cur) {
    case ' ':
    case ',':
      if (within) {
        if (i >= 0 && i < sizeof(codes)/sizeof(codes[0])) {
          printf(" %d\n", codes[i]);
        }
      }
      within = NO;
      cur++;
      continue;
    default:
      if (!within) {
        i++;
        within = YES;
      }
      printf("%c", *cur); // easy to implement, maybe inefficient to run, who cares
      cur++;
    }
  }
  
}
