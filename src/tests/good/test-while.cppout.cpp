#include "sip.h"
using namespace Sip;

ClProgram g_clProgram;
Image g__sip_temp__;


int main()
{
    g_clProgram.CompileClFile("./test-while.cl");

int sum = 0;
int i = 0;

while (i < 100) 
{
i = i + 1;
sum = sum + 1;

}
std::cout << sum << std::endl;


    return 0;
}


