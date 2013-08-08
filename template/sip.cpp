#include "sip.h"

using namespace Sip;

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
