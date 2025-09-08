#import <Spatial/Spatial.h>

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


// TODO 2025-09-08 10:11:28 Caller allocates.
// Just because painful to spell out so many mundane structs of doubles in cffi.
// Might be able to avoid some copying etc. Vulnerable to API change!
int32_t indirectSPVector3DApplyAffineTransform(SPVector3D *vector,
                                               SPAffineTransform3D *transform,
                                               SPVector3D *out) {
  *out = SPVector3DApplyAffineTransform(*vector, *transform);
  return sizeof(SPVector3D) / sizeof(double);
}


int32_t indirectSPAffineTransform3DMake(SPSize3D *scale,
                                        SPRotation3D *rotation,
                                        SPVector3D *translation,
                                        SPAffineTransform3D *out) {
  *out = SPAffineTransform3DMake(*scale, *rotation, *translation);
  return sizeof(SPAffineTransform3D) / sizeof(double);
}
