#include "sip.h"
using namespace Sip;

ClProgram g_clProgram;
Image g__sip_temp__;


int main()
{
    g_clProgram.CompileClFile("./test-gpu-edge.cl");

Image dst;
Image src;
float edge[3][3] = {{0., -1., 0.}, {-1., 5., -1.}, {0., -1., 0.}};

src.read("./blackbuck.bmp");
g_clProgram.ApplyFilter(src, g__sip_temp__, (float*)&edge);

dst = g__sip_temp__;
dst.write("./test-gpu-edge.bmp");


    return 0;
}


