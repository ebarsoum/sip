#include <stdio.h>
#include <stdlib.h>

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif

#include "EasyBMP.h"

#ifndef SIP_H
#define SIP_H

#define MEM_SIZE (128)
#define MAX_SOURCE_SIZE (0x100000)

namespace Sip
{
    class ClProgram
    {
    public:
        ClProgram();
        ~ClProgram();
	};

    class Image
    {
	public:
		int width();
		int height();
		
        RGBApixel* operator()(int i,int j);
		
        void read(const char* path);
        void write(const char* path);

	private:
		BMP _image;
	};

	class Histogram
    {};

}

#endif