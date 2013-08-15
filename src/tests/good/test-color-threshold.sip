
//
// Color threshold.
//
fun main()
{
    image src;                                  // Source image
    image dst;                                  // Destination image

    src << "./blackbuck.bmp";     // Read source image

    //
    // Set each channel to 255 or 0 depend on certain threshold value.
    //
    dst = src in (red, green, blue) for { red: (red > 128) ? 255 : 0, 
                                          green: (green > 128) ? 255 : 0, 
                                          blue: (blue > 128) ? 255 : 0 };

    dst >> "./test-color-threshold.bmp";
}
