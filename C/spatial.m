#include <stdio.h>
#import <Spatial/Spatial.h>

// ──────────────────────────────────────────────────────────────────────── Wrap

// TODO 2025-09-08 09:33:27 failing call header fns directly, understand why

// SPAffineTransform3D.h
SPAffineTransform3D wrapSPAffineTransform3DMake(SPSize3D scale,
                                            SPRotation3D rotation,
                                            SPVector3D translation) {
  return SPAffineTransform3DMake( scale, rotation, translation);

}

// SPVector3D.h

SPVector3D wrapSPVector3DApplyAffineTransform(SPVector3D vector,
                                              SPAffineTransform3D transform) {
  return SPVector3DApplyAffineTransform(vector, transform);
}

// ──────────────────────────────────────────────────────────────────── Indirect

// working
int32_t indirectSPAffineTransform3DMake(SPSize3D *scale,
                                        SPRotation3D *rotation,
                                        SPVector3D *translation,
                                        SPAffineTransform3D *out) {
  printf("Trying make with %p %p %p %p\n", scale, rotation, translation, out); // not shown in sly-inferior-lisp for some reason
  *out = SPAffineTransform3DMake(*scale, *rotation, *translation);
  return sizeof(SPAffineTransform3D) / sizeof(double);
}

// TODO 2025-09-08 10:11:28 Caller allocates.
// Because painful to spell out so many mundane structs of doubles in cffi.
// Might be able to avoid some copying, reuse existing buffer, etc.
// Vulnerable to API change!
int32_t indirectSPVector3DApplyAffineTransform(SPVector3D *vector,
                                               SPAffineTransform3D *transform,
                                               SPVector3D *out) {
  // printf("Trying apply with %p %p %p\n", vector, transform, out); // not appearing
  // Copy allows secret SIMD_INLINE or __ext_vector_type__ business I think...
  SPVector3D vector_copy = *vector; // needed to prevent memory fault
  *out = SPVector3DApplyAffineTransform(vector_copy, *transform);
  return sizeof(SPVector3D) / sizeof(double);
}
