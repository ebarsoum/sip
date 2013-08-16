#include "sip.h"
using namespace Sip;

ClProgram g_clProgram;
Image g__sip_temp__;


int main()
{
    g_clProgram.CompileClFile("./test-img-attr.cl");

Image src;

src.read("./blackbuck.bmp");
std::cout << src.width() << std::endl;
std::cout << src.height() << std::endl;


    return 0;
}


