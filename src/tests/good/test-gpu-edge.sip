//
// Edge image.
//
fun main()
{
  image edge = [[0.0, -1.0, 0.0] [-1.0, 5.0, -1.0] [0.0, -1.0, 0.0]]; // Set the 3x3 edge filter
  image src;                                  // Source image
  image dst;                                  // Destination image

  src << "./blackbuck.bmp";     // Read source image

  //
  // Convolve 3x3 matrix with the “src” image, the below operation will run on the GPU.
  //
  dst = src ^ edge;

  dst >> "./test-gpu-edge.bmp";
}
