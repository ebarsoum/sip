#include "sip.h"
using namespace Sip;

ClProgram g_clProgram;
Image g__sip_temp__;


int main()
{
    g_clProgram.CompileClFile("./test-color-to-gray.cl");

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

red_out = 0.2126 * red + 0.7152 * green + 0.0722 * blue;
green_out = 0.2126 * red + 0.7152 * green + 0.0722 * blue;
blue_out = 0.2126 * red + 0.7152 * green + 0.0722 * blue;

        g__sip_temp__(row, col)->Red   = (char)red_out;
        g__sip_temp__(row, col)->Green = (char)green_out;
        g__sip_temp__(row, col)->Blue  = (char)blue_out;
        g__sip_temp__(row, col)->Alpha = src(row, col)->Alpha;
    }
}

dst = g__sip_temp__;
dst.write("./test-color-to-gray.bmp");


    return 0;
}


