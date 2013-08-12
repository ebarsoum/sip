
fun main() 
{ 
  image filter = [[0.11 0.11 0.11] [0.11 0.11 0.11] [0.11 0.11 0.11]];
  image im1;
  image im2;
  
  im1 << "./blackbuck.bmp";
  
  im2 = im1 ^ filter;
  
  im2 >> "./blackbuck-blur.bmp";
}
