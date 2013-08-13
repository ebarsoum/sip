
fun main() 
{ 
  image im1;
  image im2;
  
  im1 << "./blackbuck.bmp";
  
  im2 = im1 ^ blur;
  
  im2 >> "./blackbuck-blur-dyn.bmp";

}

kernel blur (image in_image, image out_image)
{
    int x;
    int y;
    float red_out;
    float green_out;
    float blue_out;
    float red;
    float green;
    float blue;

    red_out = 0.0;
    green_out = 0.0;
    blue_out = 0.0;

    for (y = -3; y <= 3; y = y + 1) 
	{
        for (x = -3; x <= 3; x = x + 1) 
		{
            red = in_image[y,x]->Red;
            green = in_image[y,x]->Green;
            blue = in_image[y,x]->Blue;
        
            red_out = red_out + red;
            green_out = red_out + green;
            blue_out = red_out + blue;
        }
    }
}
