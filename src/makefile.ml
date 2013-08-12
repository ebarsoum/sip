open Printf

let fwrite name content =
	let out = open_out name in
	  fprintf out "%s\n" content;
	  close_out out

let string_of_makefile t =
  "TARGET = " ^ t ^ ".out\n" ^
  "OBJS = " ^ t ^ ".o sip.o EasyBMP.o\n" ^
  "CC = g++\n" ^
  "CFLAGS = -Wall -O3\n" ^
  "LFLAGS = -Wall\n\n" ^
  "$(TARGET) : $(OBJS)\n" ^
  "$(CC) $(LFLAGS) $(OBJS) -framework opencl -o $@\n\n" ^
  t ^ ".o: " ^ t ^ ".cpp sip.h\n" ^
  "\tg++ $(CFLAGS) -c " ^ t ^ ".cpp\n\n" ^
  "sip.o: sip.cpp EasyBMP.h\n" ^
  "\tg++ $(CFLAGS) -c sip.cpp\n\n" ^
  "EasyBMP.o: EasyBMP.cpp EasyBMP*.h\n" ^
  "\tg++ $(CFLAGS) -c EasyBMP.cpp\n\n" ^
  "clean:\n" ^
  "\trm *.o\n\n" ^
  "cleanall:\n" ^
  "\trm *.o $(TARGET)\n"

let gen_makefile t =
  let mf = string_of_makefile t in
    fwrite "./out/makefile.apple" mf
