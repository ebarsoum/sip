#include "sip.h"
using namespace Sip;

ClProgram g_clProgram;
Image g__sip_temp__;


int main()
{
    g_clProgram.CompileClFile("./test-img-read-write.cl");

Image im;

im.read("./blackbuck.bmp");
im.write("./test-img-read-write.bmp");


    return 0;
}


