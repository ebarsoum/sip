#include "sip.h"
using namespace Sip;

ClProgram g_clProgram;
Image g__sip_temp__;


int main()
{
    g_clProgram.CompileClFile("./test-flip-colors.cl");

Image im2;
Image im1;

im1.read("./blackbuck.bmp");
g__sip_temp__.clone(im1);
for (int row = 0; row <im1.height(); ++row)
{
    for (int col = 0; col <im1.width(); ++col)
    {
        unsigned int red = im1(row, col)->Red;
        unsigned int red_out = im1(row, col)->Red;
        unsigned int green = im1(row, col)->Green;
        unsigned int green_out = im1(row, col)->Green;
        unsigned int blue = im1(row, col)->Blue;
        unsigned int blue_out = im1(row, col)->Blue;

red_out = green;
green_out = blue;
blue_out = red;

        g__sip_temp__(row, col)->Red   = (char)red_out;
        g__sip_temp__(row, col)->Green = (char)green_out;
        g__sip_temp__(row, col)->Blue  = (char)blue_out;
        g__sip_temp__(row, col)->Alpha = im1(row, col)->Alpha;
    }
}

im2 = g__sip_temp__;
im2.write("./test-flip-colors.bmp");


    return 0;
}


