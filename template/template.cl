__constant sampler_t sampler =  CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_CLAMP_TO_EDGE | CLK_FILTER_NEAREST;

__kernel void image_copy(__read_only image2d_t in_image, __write_only image2d_t out_image)
{
    const int2 pos = {get_global_id(0), get_global_id(1)};
    float4 sum = (float4)(0.0f);

    for (int y = -3; y <= 3; y++) 
	{
        for (int x = -3; x <= 3; x++) 
		{
            sum += read_imagef(in_image, sampler, pos + (int2)(x,y));
        }
    }
	
    write_imagef (out_image, (int2)(pos.x, pos.y), sum);
}
