
//
// Print image attribute.
//
fun main()
{
    image src;

    src << "./blackbuck.bmp";

    writeln(src->Width);
    writeln(src->Height);
}
