
fun main() 
{ 
  image im1;
  image im2;
  
  im1 << "./blackbuck.bmp";
  
  im2 = im1[0 .. 100; 0 .. 100];
  
  im2 >> "./test-img-range.bmp";
}
