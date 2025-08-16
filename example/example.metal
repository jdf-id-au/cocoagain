#include <metal_stdlib>
using namespace metal;

// FIXME 2025-08-16 00:04:30 learn msl:
// Metal-Shading-Language-Specification.pdf

/* 5.2.2 For per-vertex input passed as an argument declared with the
[[stage_in]] attribute, each element of the per-vertex input must
specify the vertex attribute location as [[attribute(index)]]. */
struct VertexIn {
  float3 position [[attribute(0)]];
};

struct VertexOut {
  float4 position [[position]]; // pixel coordinates! https://stackoverflow.com/a/30832416/780743
  float4 color; // doesn't need to be US spelling...
}; 

/* 5.2.3.4 If the return type of a vertex function is not void, it must
include the vertex position. If the vertex return type is float4, this
always refers to the vertex position (and the [[position]] attribute
need not be specified). If the vertex return type is a structure, it
must include an element declared with the [[position]] attribute. */
vertex VertexOut vertex_main(VertexIn vert [[stage_in]]) {
  return (VertexOut) {
    .position = float4(vert.position, 1),
    .color = float4((vert.position + 1)/2, 1)
  };
}

/* 5.2.4 Vertex function output and the rasterizer-generated fragments
become the per-fragment inputs to a fragment function. The
[[stage_in]] attribute can assemble the per-fragment inputs. */
fragment float4 fragment_main(VertexOut in [[stage_in]]) {
  return in.color;
}
