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


// TODO 2025-09-08 10:11:28 Try bypassing type system! Caller allocates.
// Just because painful to spell out so many mundane structs of doubles.
// Might be able to avoid some copying etc. Vulnerable to API change!
int32_t indirectSPVector3DApplyAffineTransform(double *in, double *out) {
  // TODO 2025-09-08 10:26:31 insz, outsz, check?
  //size_t offset = 0; // just do manually for the moment
  SPVector3D *vector = (SPVector3D *)&in[0];
  //offset += sizeof(SPVector3D);
  SPAffineTransform3D *transform = (SPAffineTransform3D *)&in[4];
  SPVector3D *ret = (SPVector3D *)out;
  *ret = SPVector3DApplyAffineTransform(*vector, *transform);
  return sizeof(SPVector3D) / sizeof(double);
}


int32_t indirectSPAffineTransform3DMake(double *in, double *out) {
  SPSize3D *scale = (SPSize3D *)&in[0];
  SPRotation3D *rotation = (SPRotation3D *)&in[4];
  SPVector3D *translation = (SPVector3D *)&in[8];
  SPAffineTransform3D *ret = (SPAffineTransform3D *)out;
  *ret = SPAffineTransform3DMake(*scale, *rotation, *translation);
  return sizeof(SPAffineTransform3D) / sizeof(double);
}
