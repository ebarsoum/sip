#include "sip.h"
using namespace Sip;

ClProgram g_clProgram;
Image g__sip_temp__;


int main()
{
    g_clProgram.CompileClFile("./test-color-threshold.cl");

Image dst;
Image src;

src.read("./blackbuck.bmp");
g__sip_temp__.clone(src);
for (int row = 0; row <src.height(); ++row)
{
    for (int col = 0; col <src.width(); ++col)
    {
        unsigned int red = src(row, col)->Red;
        unsigned int red_out = src(row, col)->Red;
        unsigned int green = src(row, col)->Green;
        unsigned int green_out = src(row, col)->Green;
        unsigned int blue = src(row, col)->Blue;
        unsigned int blue_out = src(row, col)->Blue;

red_out = ((red > 128)) ? 255:0;
green_out = ((green > 128)) ? 255:0;
blue_out = ((blue > 128)) ? 255:0;

        g__sip_temp__(row, col)->Red   = (char)red_out;
        g__sip_temp__(row, col)->Green = (char)green_out;
        g__sip_temp__(row, col)->Blue  = (char)blue_out;
        g__sip_temp__(row, col)->Alpha = src(row, col)->Alpha;
    }
}

dst = g__sip_temp__;
dst.write("./test-color-threshold.bmp");


    return 0;
}


