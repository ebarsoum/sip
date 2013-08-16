#include "sip.h"
using namespace Sip;

ClProgram g_clProgram;
Image g__sip_temp__;


int add(int x , int y)
{

return (x + y);



}

int main()
{
    g_clProgram.CompileClFile("./test-fun.cl");


std::cout << add(100, 100) << std::endl;


    return 0;
}


