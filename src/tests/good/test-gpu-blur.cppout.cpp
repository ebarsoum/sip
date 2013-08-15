#include "sip.h"
using namespace Sip;

ClProgram g_clProgram;
Image g__sip_temp__;


int main()
{
    g_clProgram.CompileClFile("./test-gpu-blur.cl");

Image im2;
Image im1;
float filter[3][3] = {{0.11, 0.11, 0.11}, {0.11, 0.11, 0.11}, {0.11, 0.11, 0.11}};

im1.read("./blackbuck.bmp");
g_clProgram.ApplyFilter(im1, g__sip_temp__, (float*)&filter);

im2 = g__sip_temp__;
im2.write("./test-gpu-blur.bmp");


    return 0;
}


