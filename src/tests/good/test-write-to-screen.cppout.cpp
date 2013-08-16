#include "sip.h"
using namespace Sip;

ClProgram g_clProgram;
Image g__sip_temp__;


int main()
{
    g_clProgram.CompileClFile("./test-write-to-screen.cl");


std::cout << "This is a test" << std::endl;
std::cout << 1 + 1 << std::endl;
std::cout << 1. / 2. << std::endl;


    return 0;
}


