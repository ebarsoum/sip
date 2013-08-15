
//
// Color to gray.
//
fun main()
{
    image src;                                  // Source image
    image dst;                                  // Destination image

    src << "./blackbuck.bmp";     // Read source image

    //
    // Set each channel to the gray value of the src.
    //
    dst = src in (red, green, blue) for { red: 0.2126*red + 0.7152*green + 0.0722*blue, 
                                          green: 0.2126*red + 0.7152*green + 0.0722*blue, 
                                          blue: 0.2126*red + 0.7152*green + 0.0722*blue };

    dst >> "./test-color-to-gray.bmp";
}
