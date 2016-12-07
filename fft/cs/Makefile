.PHONY: all clean

all: Fft.exe

Fft.exe: Bitmap.cs Wavefile.cs Fft.cs
	mcs -out:Fft.exe /R:System.Numerics Bitmap.cs Wavefile.cs Fft.cs

clean:
	rm -f Fft.exe
