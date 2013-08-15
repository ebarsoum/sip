#include "sip.h"
using namespace Sip;

ClProgram g_clProgram;
Image g__sip_temp__;


int main()
{
    g_clProgram.CompileClFile("./test-gpu-kfun-blur.cl");

Image dst;
Image src;

src.read("./blackbuck.bmp");
g_clProgram.RunKernel(src, g__sip_temp__,"blur");

dst = g__sip_temp__;
dst.write("./test-gpu-kfun-blur.bmp");


    return 0;
}



