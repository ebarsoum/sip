
TARGET = a.out
OBJS = template.o sip.o EasyBMP.o
CC = g++
CFLAGS = -Wall -O3
LFLAGS = -Wall

$(TARGET) : $(OBJS)
	$(CC) $(LFLAGS) $(OBJS) -framework opencl -o $@

template.o: template.cpp sip.h
	g++ $(CFLAGS) -c template.cpp

sip.o: sip.cpp EasyBMP.h
	g++ $(CFLAGS) -c sip.cpp

EasyBMP.o: EasyBMP.cpp EasyBMP*.h
	g++ $(CFLAGS) -c EasyBMP.cpp

clean:
	rm *.o

cleanall:
	rm *.o $(TARGET)
