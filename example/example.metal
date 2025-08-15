#include <metal_stdlib>
using namespace metal;

// FIXME 2025-08-16 00:04:30 learn msl
struct VertexIn {
  float3 position [[ attribute(0) ]];
};

vertex float4 vertex_main(VertexIn vert [[ stage_in ]]) {
  return float4(vert.position, 1.0f);
}

fragment float4 fragment_main(float4 pixel [[ stage_in ]]) {
  return float4(1,0,0,1);
}
