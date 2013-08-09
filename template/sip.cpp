#include "sip.h"

using namespace Sip;

ClProgram::ClProgram() :  _deviceId(NULL),
                          _context(NULL),
                          _program(NULL),
                          _platformId(NULL),
                          _commandQueue(NULL)
{
    Init();
}

ClProgram::~ClProgram()
{
    Uninit();
}

void ClProgram::Init()
{
    cl_int ret = 0;
    cl_uint platformCount = 0;
    cl_uint deviceCount = 0;

    ret = clGetPlatformIDs(1, &_platformId, &platformCount);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: clGetPlatformIDs: " << ret << endl;
        return;
    }

    ret = clGetDeviceIDs(_platformId, CL_DEVICE_TYPE_GPU, 1, &_deviceId, &deviceCount);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: clGetDeviceIDs: " << ret << endl;
        return;
    }

    _context = clCreateContext(NULL, 1, &_deviceId, NULL, NULL, &ret);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: clCreateContext: " << ret << endl;
        return;
    }

    _commandQueue =  clCreateCommandQueue(_context, _deviceId, 0, &ret);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: clCreateCommandQueue: " << ret << endl;

        clReleaseContext(_context);
        _context = NULL;
        return;
    }
}

void ClProgram::Uninit()
{
    if (_program != NULL)
    {
        clReleaseProgram(_program);
        _program = NULL;
    }

    if (_commandQueue != NULL)
    {
        clReleaseCommandQueue(_commandQueue);
        _commandQueue = NULL;
    }

    if (_context != NULL)
    {
        clReleaseContext(_context);
        _context = NULL;
    }
}

void ClProgram::CompileClFile(const char* path)
{
    cl_int ret = 0;
    FILE *file   = NULL;
    char *source = NULL;
    size_t size  = 0;

    file = fopen(fileName, "r");
    if (!file) 
    {
        cout << "Couldn't open: " << path << endl;
        return;
    }

    source = new char[MAX_SOURCE_SIZE];
    size = fread(source, 1, MAX_SOURCE_SIZE, file);

    if (_program != NULL)
    {
        clReleaseProgram(_program);
        _program = NULL;
    }

	_program = clCreateProgramWithSource(_context, 1, (const char **)&source, 0, &ret);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: clCreateProgramWithSource :" << ret << endl;
        return;
    }
 
	ret = clBuildProgram(_program, 1, &_deviceId, NULL, NULL, NULL);
    if ((ret != CL_SUCCESS) || (ret == CL_BUILD_PROGRAM_FAILURE))
    {
        cout << "Error: clBuildProgram: " << ret << endl;
        return;
    }

    delete[] source;
    fclose(file);
}

void ClProgram::RunKernel(Image& in_image, Image& out_image, const char* kernelName)
{
    cl_int ret = 0;
	cl_image_format img_fmt;
 
	img_fmt.image_channel_order = CL_RGBA;
	img_fmt.image_channel_data_type = CL_FLOAT;
 
	cl_mem imageSrc; 
    cl_mem imageDst;
 
	size_t width = in_image.width();
    size_t height = in_image.height();

	float* input  = new float[width * height * 3];
	float* output = new float[width * height * 3];

    for (int row = 0; row < height; ++row)
    {
        for (int col = 0; col < width; ++col)
        {
            input[];
        }
    }

    imageSrc = clCreateImage2D(context, CL_MEM_READ_ONLY, &img_fmt, width, height, 0, 0, &ret);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: imageSrc clCreateImage2D: " << ret << endl;
        return;
    }
 
	imageDst = clCreateImage2D(context, CL_MEM_READ_WRITE, &img_fmt, width, height, 0, 0, &ret);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: imageDst clCreateImage2D: " << ret << endl;
        return;
    }
 
	cl_event clevent[5];
 
	size_t origin[] = {0, 0, 0}; // Defines the offset in pixels in the image from where to write.
	size_t region[] = {width, height, 1}; // Size of object to be transferred
	ret = clEnqueueWriteImage(_commandQueue, imageSrc, CL_TRUE, origin, region, 0, 0, input, 0, NULL, &clevent[0]);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: clEnqueueWriteImage: " << ret << endl;
        return;
    }

	cl_kernel kernel = clCreateKernel(program, kernelName, &ret);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: clCreateKernel: " << ret << endl;
        return;
    } 
 
	ret = clSetKernelArg(kernel, 0, sizeof(cl_mem), (void *)&imageSrc);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: clSetKernelArg: " << ret << endl;
        return;
    }

	ret = clSetKernelArg(kernel, 1, sizeof(cl_mem), (void *)&imageDst);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: clSetKernelArg: " << ret << endl;
        return;
    }

	size_t GWSize[] = {width, height, 1};
	ret = clEnqueueNDRangeKernel(_commandQueue, kernel, 2, NULL, GWSize, NULL, 1, clevent, &clevent[1]);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: clEnqueueNDRangeKernel: " << ret << endl;
        return;
    } 

    ret = clEnqueueReadImage(_commandQueue, imageDst, CL_TRUE, origin, region, 0, 0, output, 2, clevent, &clevent[2]);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: clEnqueueReadImage: " << ret << endl;
        return;
    } 

    delete[] input;
    delete[] output;

    clReleaseMemObject(imageDst);
    clReleaseMemObject(imageSrc);
    clReleaseKernel(kernel);
}

void Image::read(const char* path)
{
    _image.ReadFromFile(path);
}

void Image::write(const char* path)
{
    _image.WriteToFile(path);
}

int Image::width()
{
    return _image.TellWidth();
}
    
int Image::height()
{
    return _image.TellHeight();
}

RGBApixel* Image::operator()(int i,int j)
{
    return _image(i, j);
}
