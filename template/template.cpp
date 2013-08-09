#include "sip.h"

using namespace Sip;

ClProgram g_clProgram;

int main()
{
    g_clProgram.CompileClFile("./template.cl");
	
	Image in;
	in.read("./blackbuck.bmp");

    g_clProgram.RunKernel(in, in, "image_copy");
	
	in.write("./t1.bmp");
	
    return 0;
}

