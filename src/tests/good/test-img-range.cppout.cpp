#include "sip.h"
using namespace Sip;

ClProgram g_clProgram;
Image g__sip_temp__;


int main()
{
    g_clProgram.CompileClFile("./test-img-range.cl");

Image im2;
Image im1;

im1.read("./blackbuck.bmp");
im1.copyRangeTo(0, 0, 100, 100, g__sip_temp__);
im2 = g__sip_temp__;
im2.write("./test-img-range.bmp");


    return 0;
}


