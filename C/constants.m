#include <stdio.h> // printf
#include <string.h>
#import "view.h"

#define countof(arrayptr) sizeof(arrayptr) / sizeof(*(arrayptr))

// Expecting global uniqueness because of C enum semantics!
#define SPELL_ENUMS(...)           \
  int32_t codes[] = {__VA_ARGS__}; \
  char *names = #__VA_ARGS__

// NB group by elided prefix; unelided first
SPELL_ENUMS(MTLTriangleFillModeFill, MTLTriangleFillModeLines,

            MTLWindingClockwise, MTLWindingCounterClockwise,

            MTLCullModeNone, MTLCullModeFront, MTLCullModeBack,

            MTLPrimitiveTypePoint, MTLPrimitiveTypeLine,
            MTLPrimitiveTypeLineStrip, MTLPrimitiveTypeTriangle,
            MTLPrimitiveTypeTriangleStrip,

            MTLIndexTypeUInt16, MTLIndexTypeUInt32,

            MTLDepthClipModeClip, MTLDepthClipModeClamp,

            MTLLoadActionDontCare, MTLLoadActionLoad, MTLLoadActionClear,

            MTLStoreActionDontCare, MTLStoreActionStore, MTLStoreActionMultisampleResolve, MTLStoreActionStoreAndMultisampleResolve, MTLStoreActionUnknown, MTLStoreActionCustomSampleDepthStore,

            MTLResourceCPUCacheModeDefaultCache,
            MTLResourceCPUCacheModeWriteCombined,

            MTLResourceStorageModeShared, MTLResourceStorageModeManaged,
            MTLResourceStorageModePrivate, MTLResourceStorageModeMemoryless,

            MTLVertexFormatFloat3,

            MTLVertexStepFunctionConstant, MTLVertexStepFunctionPerVertex,
            MTLVertexStepFunctionPerInstance, MTLVertexStepFunctionPerPatch,
            MTLVertexStepFunctionPerPatchControlPoint,

            MTLCompareFunctionNever, MTLCompareFunctionLess,
            MTLCompareFunctionEqual, MTLCompareFunctionLessEqual,
            MTLCompareFunctionGreater, MTLCompareFunctionNotEqual,
            MTLCompareFunctionGreaterEqual,
            MTLCompareFunctionAlways,
            
            MTLPixelFormatA8Unorm, MTLPixelFormatBGRA8Unorm,

            MTLBlendFactorZero, MTLBlendFactorOne, MTLBlendFactorSourceAlpha, MTLBlendFactorDestinationAlpha, MTLBlendFactorBlendAlpha,
            );

struct elision {
  char *prefix, *package, *abbrev;
};

struct elision elide[] = {
  {"NS", "cocoagain", "ns"}, // default, no defpackage
  {"MTL", "metal", "mtl"},
};

int main(void) {
  char *cur = names;
  int32_t i = -1, elision = -1;
  BOOL within = NO, def = NO;
  for (size_t e = 1; e < countof(elide); e++)
    printf("(defpackage :%s (:nicknames :%s) (:use cl))\n", 
           elide[e].package, elide[e].abbrev); 
  while (*cur) {
    switch (*cur) {
    case ' ':
    case ',':
      if (within && i >= 0 && i < countof(codes)) 
        printf(" %d)\n", codes[i]);
      within = NO;
      def = NO;
      cur++;
      continue;
    default:
      if (!within) {
        within = YES;
        i++;
        for (size_t e = 0; e < countof(elide); e++) {
          char *prefix = elide[e].prefix;
          if (!strncmp(cur, prefix, strlen(prefix))) { // match
            cur += strlen(prefix);
            if (e != elision) {
              elision = e;
              printf("(in-package :%s)\n", elide[e].package);
            }
            goto elide_continue;
          }
        }
        if (elision == -1) {
          elision = 0;
          printf("(in-package :%s)\n", elide[0].package);
        }
      }
      if (!def) { 
        printf("(defconstant "); 
        def = YES;
      }
      printf("%c", *cur); // easy to implement, maybe inefficient to run, who cares
      cur++;
    }
  elide_continue:
    continue;
  } 
}
