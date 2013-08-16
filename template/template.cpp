#include "sip.h"

using namespace Sip;

ClProgram g_clProgram;

int main()
{
    g_clProgram.CompileClFile("./template.cl");
	
	Image in, out;
	in.read("./blackbuck.bmp");

    float filter[3][3] = {{0.11f, 0.11f, 0.11f}, {0.11f, 0.11f, 0.11f}, {0.11f, 0.11f, 0.11f}};
    
	//g_clProgram.RunKernel(in, out, "image_copy");

    Histogram hist = in;
	cout << hist(128, 1) << endl;
	
    g_clProgram.ApplyFilter(in, out, (float*)&filter);
    out.write("./t1.bmp");
	
    return 0;
}

