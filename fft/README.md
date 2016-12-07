# Fast Fourier Transform

A FFT implementation using [Cooley-Tukey algorithm](https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm), in Lua (using [LuaJit](https://luajit.org/)), C#, C++ and Haskell.
I wrote original code in Lua a while ago, and later optimized it a bit. Other languages are more or less a direct copy of Lua code, with adjustments made for language-specific performance quirks. C++ and C# implementations use complex number types from standard libraries, Haskell and Lua use two arrays of doubles.

### Implementation details

All implementations read a wave file and use FFT to compute a spectrogram for a 10-second part of it. Trigonometric functions and window function are cached, as computing them is typically more expensive than reading a value from memory.
Implementations use decimation-in-time recursive calls until they reach an implementation-specific depth (signified by a magic constant near the beginning of ```ifft``` function/method), and then switch to quadratic DFT algorithm. The depth depends on language, being 4 for Lua, and 2 for C# and C++. Haskell is an exception, due to cheapness of function calls it goes all the way down and uses specialized version of "DFT" that computes DFT of a single value.
Spectrogram is computed by bitmap column, summing contributions from individual FFT transforms, and then normalizing them to fit into 0-255 RGB range. The normalization process is per-column, and gives the resulting bitmap striped appearance.
There are no tests, and implementation of wavefile reading and bitmap writing are **extremely** minimalistic, with no validation or handling of multiple formats whatsoever. This means that **you probably should not use this code in any sort of production environment**.

### Building and running
Lua code can be run with ```luajit fft.lua```. It requires the ```ffi``` module available, so it won't run on pretty much any other Lua implementation.
C++ and C# versions have makefiles that use g++ and Mono C# compiler respectively. Feel free to adjust them to your own liking, since they are just an example.
Use [Stack](http://haskellstack.org) for Haskell version. Running ```stack build``` should be enough to build an executable, which can be then run with ```stack exec HaskellFFT```.
**All implementation have paths hardcoded in them**, with the assumption that program is run from its own subfolder. The two paths (input wavefile and output bitmap) are in the main function. FFT size (1024) is also hardcoded, as are the region of wave file (from 00:20 to 00:30) and bitmap dimensions (2200x512).

### Sample results

The following results were obtained on a MacBook with Intel i7-2640M running at 2.8 GHz, with turbo boost disabled. The results are in seconds, averaged over five runs. Time was measured using ```time``` utility.

 C++ | LuaJit | C# (Mono) | Haskell 
-----|--------|-----------|---------
 7.6 | 8.8    |  15.4     |  18.4

### Acknowledgements
This benchmark was done as a part of IFMO University course on functional programming. Some of course materials can be found [here](https://github.com/jagajaga/FP-Course-ITMO).
The included audio fragment was taken from [this YouTube video](https://www.youtube.com/watch?v=8uCMmSDwHNA).
