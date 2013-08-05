
fun main() 
{ 
  image im1;
  im1 << "c:\\test.jpg";
  
  im1 in (red, green, blue) for {hue: red, sat: green, v: blue};
}

