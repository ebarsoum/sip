
fun main() 
{ 
  image im1;
  image im2;
  
  im1 << "./test.bmp";
  
  im2 = im1 in (red, green, blue) for {red: red, green: green, blue: blue};
  
  im2 >> "./test2.bmp";
}

