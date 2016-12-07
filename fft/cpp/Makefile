.PHONY: all clean

all: fft

fft: bitmap.h wavefile.h fft.cpp
	g++ -Ofast -march=native -std=c++11 -o fft fft.cpp

clean:
	rm -f fft
