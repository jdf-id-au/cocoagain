#include <metal_stdlib>
using namespace metal;

// FIXME 2025-08-16 00:04:30 learn msl e.g.
// Metal-Shading-Language-Specification.pdf ch 5 e.g. 5.2.2 p111

/* 5.2.2 For per-vertex input passed as an argument declared with the
[[stage_in]] attribute, each element of the per-vertex input must
specify the vertex attribute location as [[attribute(index)]]. */
struct VertexIn {
  float3 position [[attribute(0)]];
};

/* 5.2.3.4 If the return type of a vertex function is not void, it must
include the vertex position. If the vertex return type is float4, this
always refers to the vertex position (and the [[position]] attribute
need not be specified). If the vertex return type is a structure, it
must include an element declared with the [[position]] attribute. */
vertex float4 vertex_main(VertexIn vert [[stage_in]]) {
  return float4(vert.position, 1.0f);
}

/* 5.2.4 Vertex function output and the rasterizer-generated fragments
become the per-fragment inputs to a fragment function. The
[[stage_in]] attribute can assemble the per-fragment inputs. */
fragment float4 fragment_main(float4 pixel [[position]]) {
  /* Compiles, but "Link failed: fragment input pixel was not found in
     vertex shader outputs. -> Need to change [[sstage_in]] to
     [[position]] https://stackoverflow.com/a/41559146/780743 */
  return float4(pixel.x/1000,pixel.y/1000,pixel.y/1000,1);
  //return float4(1.0,0.5,0,1);
  
}
