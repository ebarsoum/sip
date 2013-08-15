
//
// Blue image in the GPU using dynamic convolution.
//
fun main()
{
  image src;
  image dst;
  
  src << "./blackbuck.bmp"; // Read source image
  
  dst = src ^ blur;         // Convolute the source image with a kernel function.
                            // blue is a full blown function defined below

  dst >> "./test-gpu-kfun-blur.bmp"; //  Save the result to a file.
}

//
// This is how to write a kernel function that will run on the GPU.
// Same exactly as normal function, with the following differences:
//    1. Use “kernel” instead of “fun”, when you define your function.
//    2. Only two arguments of type image, the first is the input and the second is the output. 
//    3. No return type.
//    4. No access to global variables.
//    5. Can’t call another function from within the kernel.
//    6. Can’t read or save file within the kernel.
//    7. Must define: red_out, green_out and blue_out for the resulted pixel.
//         We automatic bind them to the second argument.  
//
kernel blur (image in_image, image out_image)
{
    int x;
    int y;
    float red_out;
    float green_out;
    float blue_out;

    red_out = 0.0;
    green_out = 0.0;
    blue_out = 0.0;

    for (y = -1; y <= 1; y = y + 1) 
    {
        for (x = -1; x <= 1; x = x + 1) 
        {
            red_out = red_out + in_image[y,x]->Red / 9; 
            green_out = green_out + in_image[y,x]->Green / 9;
            blue_out = blue_out + in_image[y,x]->Blue / 9;
        }
    }    
}
