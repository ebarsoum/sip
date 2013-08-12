#include "sip.h"

using namespace Sip;

ClProgram::ClProgram() :  _commandQueue(NULL),
                          _deviceId(NULL),
                          _context(NULL),
                          _program(NULL),
                          _platformId(NULL)
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

void ClProgram::CompileClFile(const char* filename)
{
    cl_int ret = 0;
    FILE *file   = NULL;
    char *source = NULL;
    size_t size  = 0;

    file = fopen(filename, "r");
    if (!file) 
    {
        cout << "Couldn't open: " << filename << endl;
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
	img_fmt.image_channel_data_type = CL_UNORM_INT8;
 
	cl_mem imageSrc; 
    cl_mem imageDst;
 
	size_t width = in_image.width();
    size_t height = in_image.height();

	char* input  = new char[width * height * 4];
	char* output = new char[width * height * 4];

    out_image.clone(in_image);

    for (size_t row = 0; row < height; ++row)
    {
        for (size_t col = 0; col < width; ++col)
        {
            input[row * 4 * width + 4 * col    ] = (char)in_image(row, col)->Red;
            input[row * 4 * width + 4 * col + 1] = (char)in_image(row, col)->Green;
            input[row * 4 * width + 4 * col + 2] = (char)in_image(row, col)->Blue;
            input[row * 4 * width + 4 * col + 3] = (char)in_image(row, col)->Alpha;
        }
    }

    imageSrc = clCreateImage2D(_context, CL_MEM_READ_ONLY, &img_fmt, width, height, 0, 0, &ret);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: imageSrc clCreateImage2D: " << ret << endl;
        return;
    }
 
	imageDst = clCreateImage2D(_context, CL_MEM_READ_WRITE, &img_fmt, width, height, 0, 0, &ret);
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

	cl_kernel kernel = clCreateKernel(_program, kernelName, &ret);
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

    for (size_t row = 0; row < height; ++row)
    {
        for (size_t col = 0; col < width; ++col)
        {
            out_image(row, col)->Red   = output[row * 4 * width + 4 * col    ];
            out_image(row, col)->Green = output[row * 4 * width + 4 * col + 1];
            out_image(row, col)->Blue  = output[row * 4 * width + 4 * col + 2];
            out_image(row, col)->Alpha = output[row * 4 * width + 4 * col + 3];
        }
    }

    delete[] input;
    delete[] output;

    clReleaseMemObject(imageDst);
    clReleaseMemObject(imageSrc);
    clReleaseKernel(kernel);
}

void ClProgram::ApplyFilter(Image& in_image, Image& out_image, float* filter)
{
    cl_int ret = 0;
	cl_image_format img_fmt;
 
	img_fmt.image_channel_order = CL_RGBA;
	img_fmt.image_channel_data_type = CL_UNORM_INT8;
 
	cl_mem imageSrc; 
    cl_mem imageDst;
	cl_mem imageFilter;
 
	size_t width = in_image.width();
    size_t height = in_image.height();

	char* input  = new char[width * height * 4];
	char* output = new char[width * height * 4];
	float* filterWeights = new float[9];

	memcpy((void*)filterWeights, (const void*)filter, 9 * sizeof(float));

    out_image.clone(in_image);

    for (size_t row = 0; row < height; ++row)
    {
        for (size_t col = 0; col < width; ++col)
        {
            input[row * 4 * width + 4 * col    ] = (char)in_image(row, col)->Red;
            input[row * 4 * width + 4 * col + 1] = (char)in_image(row, col)->Green;
            input[row * 4 * width + 4 * col + 2] = (char)in_image(row, col)->Blue;
            input[row * 4 * width + 4 * col + 3] = (char)in_image(row, col)->Alpha;
        }
    }

    imageSrc = clCreateImage2D(_context, CL_MEM_READ_ONLY, &img_fmt, width, height, 0, 0, &ret);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: imageSrc clCreateImage2D: " << ret << endl;
        return;
    }
 
	imageDst = clCreateImage2D(_context, CL_MEM_READ_WRITE, &img_fmt, width, height, 0, 0, &ret);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: imageDst clCreateImage2D: " << ret << endl;
        return;
    }
 
    imageFilter = clCreateBuffer(_context, CL_MEM_READ_ONLY, 9 * sizeof(float), 0, &ret);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: imageFilter clCreateBuffer: " << ret << endl;
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

    ret = clEnqueueWriteBuffer(_commandQueue, imageFilter, CL_TRUE, 0, 9 * sizeof(float), filterWeights, 0, NULL, &clevent[1]);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: clEnqueueWriteBuffer: " << ret << endl;
        return;
    }

	cl_kernel kernel = clCreateKernel(_program, "apply_filter", &ret);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: clCreateKernel: " << ret << endl;
        return;
    } 
 
	ret = clSetKernelArg(kernel, 0, sizeof(cl_mem), (void *)&imageSrc);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: clSetKernelArg imageSrc: " << ret << endl;
        return;
    }

	ret = clSetKernelArg(kernel, 1, sizeof(cl_mem), (void *)&imageDst);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: clSetKernelArg imageDst: " << ret << endl;
        return;
    }

	ret = clSetKernelArg(kernel, 2, sizeof(cl_mem), (void *)&imageFilter);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: clSetKernelArg filter: " << ret << endl;
        return;
    }

	size_t GWSize[] = {width, height, 1};
	ret = clEnqueueNDRangeKernel(_commandQueue, kernel, 2, NULL, GWSize, NULL, 1, clevent, &clevent[2]);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: clEnqueueNDRangeKernel: " << ret << endl;
        return;
    } 

    ret = clEnqueueReadImage(_commandQueue, imageDst, CL_TRUE, origin, region, 0, 0, output, 2, clevent, &clevent[3]);
    if (ret != CL_SUCCESS) 
    {
        cout << "Error: clEnqueueReadImage: " << ret << endl;
        return;
    } 

    for (size_t row = 0; row < height; ++row)
    {
        for (size_t col = 0; col < width; ++col)
        {
            out_image(row, col)->Red   = output[row * 4 * width + 4 * col    ];
            out_image(row, col)->Green = output[row * 4 * width + 4 * col + 1];
            out_image(row, col)->Blue  = output[row * 4 * width + 4 * col + 2];
            out_image(row, col)->Alpha = output[row * 4 * width + 4 * col + 3];
        }
    }

    delete[] filterWeights;
    delete[] input;
    delete[] output;

    clReleaseMemObject(imageDst);
    clReleaseMemObject(imageSrc);
    clReleaseKernel(kernel);
}

Image::Image()
{}

Image::Image(const Image& img) : _image(const_cast<BMP&>(img._image))
{}

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

void Image::clone(Image& img)
{
	if (this == &img)
	{
	    return;
	}

	_image.SetSize(img._image.TellWidth(), img._image.TellHeight());
	_image.SetBitDepth(img._image.TellBitDepth());
}

RGBApixel* Image::operator()(int i,int j)
{
    return _image(i, j);
}

Image& Image::operator=(Image &rhs)
{
	if (this == &rhs)
	{
	    return *this;
	}

	clone(rhs);

    for (int row = 0; row < height(); ++row)
    {
        for (int col = 0; col < width(); ++col)
        {
            _image(row, col)->Red   = rhs(row, col)->Red;
            _image(row, col)->Green = rhs(row, col)->Green;
            _image(row, col)->Blue  = rhs(row, col)->Blue;
            _image(row, col)->Alpha = rhs(row, col)->Alpha;
        }
    }

    return *this;
}
