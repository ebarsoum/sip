
TARGET = a.out
OBJS = Example.o EasyBMP.o
CC = g++
CFLAGS = -Wall -O3
LFLAGS = -Wall

$(TARGET) : $(OBJS)
	$(CC) $(LFLAGS) $(OBJS) -framework opencl -o $@

Example.o: Example.cpp EasyBMP.h
	g++ $(CFLAGS) -c Example.cpp

EasyBMP.o: EasyBMP.cpp EasyBMP*.h
	g++ $(CFLAGS) -c EasyBMP.cpp

clean:
	rm *.o

cleanall:
	rm *.o $(TARGET)
