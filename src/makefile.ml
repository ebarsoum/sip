open Printf

let fwrite name content =
	let out = open_out name in
	  fprintf out "%s\n" content;
	  close_out out

let string_of_apple_makefile t =
  "TARGET = " ^ t ^ ".out\n" ^
  "OBJS = " ^ t ^ ".o sip.o EasyBMP.o\n" ^
  "CC = g++\n" ^
  "CFLAGS = -Wall -O3\n" ^
  "LFLAGS = -Wall\n\n" ^
  "$(TARGET) : $(OBJS)\n" ^
  "\t$(CC) $(LFLAGS) $(OBJS) -framework opencl -o $@\n\n" ^
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

let string_of_linux_makefile t =
  "TARGET = " ^ t ^ ".out\n" ^
  "OBJS = " ^ t ^ ".o sip.o EasyBMP.o\n" ^
  "CC = g++\n" ^
  "CFLAGS = -Wall -O3\n" ^
  "LFLAGS = -Wall\n\n" ^
  "$(TARGET) : $(OBJS)\n" ^
  "\t$(CC) $(LFLAGS) $(OBJS) -lOpenCL -o $@\n\n" ^
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
  let amf = string_of_apple_makefile t in
  let lmf = string_of_linux_makefile t in
    fwrite "./out/makefile.linux" lmf; fwrite "./out/makefile.apple" amf
