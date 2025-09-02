#include <metal_stdlib>
using namespace metal;

// See Metal-Shading-Language-Specification.pdf

// - uniform buffers pass constant data
// - argument buffers bundle resources and data for a particular
// shader into one object

/* 5.2.2 For per-vertex input passed as an argument declared with the
[[stage_in]] attribute, each element of the per-vertex input must
specify the vertex attribute location as [[attribute(index)]]. */
struct VertexIn {
  float3 position [[attribute(0)]];
};

struct VertexOut {
  // becomes pixel coordinates! https://stackoverflow.com/a/30832416/780743
  float4 position [[position]];
  // straight through TODO 2025-08-31 12:27:12 really needed?!
  float4 ndc;
};

/* 5.2.3.4 If the return type of a vertex function is not void, it must
include the vertex position. If the vertex return type is float4, this
always refers to the vertex position (and the [[position]] attribute
need not be specified). If the vertex return type is a structure, it
must include an element declared with the [[position]] attribute. */
vertex VertexOut vertex_ndc(VertexIn vert [[stage_in]]) {
  return (VertexOut) {
    .position = float4(vert.position, 1),
    .ndc = float4(vert.position, 1)
  };
}

/* 5.2.4 Vertex function output and the rasterizer-generated fragments
become the per-fragment inputs to a fragment function. The
[[stage_in]] attribute can assemble the per-fragment inputs. */
fragment float4 fragment_main(VertexOut in [[stage_in]]) {
  // nice gradient on intel, buggy red on arm64? seems to disregard this return value?
  return float4((in.ndc.xyz + 1)/2, 1);
}

/*
  This trangle covers the quad ([-1, +1], [-1, +1]).
  With origin as . in normalised device coordinates:
   vertex_id 1 is (-1, +1) +-----+ (+3, +1) is vertex_id 2
                           | . |/
                           |__ + (+1, -1) is covered
                           |  /
                           | /
                           + (-1, -3) is vertex_id 0
   Should be clipped before hitting fragment shader.
   https://stackoverflow.com/a/76506864/780743
 */
vertex float4 vertex_fill(uint vid [[vertex_id]]) {
  float4 pos;
  pos.x = (vid == 2) ? 3.0 : -1.0;
  pos.y = (vid == 0) ? -3.0 : 1.0;
  pos.zw = 1.0;
  return pos;
}

fragment float4 fragment_lsd(VertexOut in [[stage_in]]) {
  float4 col;
  col.rg = abs(sin(in.ndc.xy * M_PI_F));
  col.b = abs(sin((in.ndc.x + in.ndc.y)/2 * M_PI_F));
  col.a = 0; // TODO 2025-08-31 12:59:14 seems to do nothing? ...investigate
  return col;
}

struct VertexPointOut {
  float4 position [[position]];
  float point_size [[point_size]];
};

vertex VertexPointOut vertex_point(VertexIn vert [[stage_in]]) {
  return (VertexPointOut) {
    .position = float4(vert.position, 1),
    .point_size = 100
  };
}

fragment float4 fragment_point(VertexPointOut in [[stage_in]]) {
  return float4(1.0, 0.0, 0.0, 0.5);
}
