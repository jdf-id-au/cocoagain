#include <stdio.h>
#import <Spatial/Spatial.h>

// ──────────────────────────────────────────────────────────────────── Rotation

bool indirectSPRotation3DIsValid(SPRotation3D *rotation) {
  return SPRotation3DIsValid(*rotation);
}

size_t indirectSPRotation3DMakeWithEulerAngles(SPEulerAngles *angles,
                                                SPRotation3D *out) {
  // 3 doubles, 1 double pad, 1 int32 enum
  *out = SPRotation3DMakeWithEulerAngles(*angles);
  return sizeof(SPRotation3D) / sizeof(double);
}

size_t indirectSPRotation3DMakeLookAt(SPVector3D *forward,
                                       SPVector3D *up,
                                       SPRotation3D *out) {
  *out = SPRotation3DMakeLookAt(*forward, *up);
  return sizeof(SPRotation3D) / sizeof(double);
}

// ─────────────────────────────────────────────────────── Affine transformation

size_t indirectSPAffineTransform3DMake(SPSize3D *scale,
                                SPRotation3D *rotation,
                                SPVector3D *translation,
                                SPAffineTransform3D *out) {
  *out = SPAffineTransform3DMake(*scale, *rotation, *translation);
  return sizeof(SPAffineTransform3D) / sizeof(double);
}

// TODO 2025-09-08 10:11:28 Caller allocates.
// Because painful to spell out so many mundane structs of doubles in cffi.
// Might be able to avoid some copying, reuse existing buffer, etc.
// Vulnerable to API change!
size_t indirectSPVector3DApplyAffineTransform(SPVector3D *vector,
                                       SPAffineTransform3D *transform,
                                       SPVector3D *out) {
  // Copy allows secret SIMD_INLINE or maybe __ext_vector_type__
  // business to work without memory fault.
  SPVector3D vector_copy = *vector;
  *out = SPVector3DApplyAffineTransform(vector_copy, *transform);
  return sizeof(SPVector3D) / sizeof(double);
}

// ─────────────────────────────────────────────────── Projective transformation

size_t indirectSPProjectiveTransform3DMake(SPSize3D *scale,
                                    SPRotation3D *rotation,
                                    SPVector3D *translation,
                                    SPProjectiveTransform3D *out) {
  *out = SPProjectiveTransform3DMake(*scale, *rotation, *translation);
  return sizeof(SPProjectiveTransform3D) / sizeof(double);
}

size_t indirectSPVector3DApplyProjectiveTransform(SPVector3D *vector,
                                           SPProjectiveTransform3D *transform,
                                           SPVector3D *out) {
  SPVector3D vector_copy = *vector; // see above
  *out = SPVector3DApplyProjectiveTransform(vector_copy, *transform);
  return sizeof(SPVector3D) / sizeof(double);
}

// ...TODO 2025-09-08 21:57:52 this really needs some automation...
