__constant sampler_t sampler =  CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP_TO_EDGE | CLK_FILTER_NEAREST;

__kernel void apply_filter(__read_only image2d_t in_image, __write_only image2d_t out_image, __constant float* filter)
{
   const int2 pos = {get_global_id(0), get_global_id(1)};

   float4 sum = (float4)(0.0f);
   for (int y = -1; y <= 1; y++)
   {
       for (int x = -1; x <= 1; x++)
       {
           sum.x += filter[(y + 1) * 3 + (x + 1)] * read_imagef(in_image, sampler, pos + (int2)(x,y)).x;
           sum.y += filter[(y + 1) * 3 + (x + 1)] * read_imagef(in_image, sampler, pos + (int2)(x,y)).y;
           sum.z += filter[(y + 1) * 3 + (x + 1)] * read_imagef(in_image, sampler, pos + (int2)(x,y)).z;
       }
   }

   write_imagef (out_image, (int2)(pos.x, pos.y), sum);
}

__kernel void blur(__read_only image2d_t in_image , __write_only image2d_t out_image)
{
    const int2 pos = {get_global_id(0), get_global_id(1)};
float blue_out;
float green_out;
float red_out;
int y;
int x;

red_out = 0.;
green_out = 0.;
blue_out = 0.;
for (y = -1 ; y <= 1 ; y = y + 1) {
for (x = -1 ; x <= 1 ; x = x + 1) {
red_out = red_out + read_imagef(in_image, sampler, pos + (int2)(x,y)).x / 9;
green_out = green_out + read_imagef(in_image, sampler, pos + (int2)(x,y)).y / 9;
blue_out = blue_out + read_imagef(in_image, sampler, pos + (int2)(x,y)).z / 9;

}

}


    float4 _out_ = {red_out, green_out, blue_out, 0.0f};
    write_imagef (out_image, (int2)(pos.x, pos.y), _out_);
}


