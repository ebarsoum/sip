#include <iostream>
#include <stdio.h>
#include <stdlib.h>

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif

#include "EasyBMP.h"

using namespace std;

#ifndef SIP_H
#define SIP_H

#define MEM_SIZE (128)
#define MAX_SOURCE_SIZE (0x100000)

namespace Sip
{
	class Image;

    class ClProgram
    {
    public:
        ClProgram();
        ~ClProgram();

        void CompileClFile(const char* filename);
        void RunKernel(Image& in_image, Image& out_image, const char* kernelName);
        void ApplyFilter(Image& in_image, Image& out_image, float* filter);

    private:
        void Init();
        void Uninit();

    private:
        cl_command_queue _commandQueue;
        cl_device_id     _deviceId;
        cl_context       _context;
        cl_program       _program;
        cl_platform_id   _platformId;
    };

    class Image
    {
    public:
		Image();
		Image(const Image& img);

        int width();
        int height();

        void clone(Image& img);
		
        RGBApixel* operator()(int i,int j);
        Image& operator=(Image &rhs);

        void read(const char* path);
        void write(const char* path);

    private:
        BMP _image;
    };

    class Histogram
    {};

}

#endif