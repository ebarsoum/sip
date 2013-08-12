
fun main() 
{ 
  image im1;
  image im2;
  
  im1 << "./blackbuck.bmp";
  
  im2 = im1 in (red, green, blue) for {red: green, green: blue, blue: red};
  
  im2 >> "./test2.bmp";
}
