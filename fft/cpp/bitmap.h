#include <fstream>
#include <cstdint>
#include <string>
#include <vector>
#include <cmath>
#include <cstring>

using namespace std;

struct px_d {
    double r, g, b;
};

struct px {
    uint8_t r, g, b;
};

class bitmap {
    int width;
    int height;
    px_d *pixels;
    ofstream file;
public:
    bitmap(int w, int h, const string& filename) {
        width = w;
        height = h;
        pixels = new px_d[w * h];
        memset(pixels, 0, w * h * sizeof(px_d));
        file = ofstream(filename, ios::binary | ios::out);
        writeHeader();
    }
    
    void writeHeader() {
        int fhdr = 14;
        int ihdr = 40;
        int32_t zero = 0;

        file.put('B');
        file.put('M');
        int32_t fsz = fhdr + ihdr + width*height*3;
        
        file.write((const char*) &fsz, 4);
        file.write((const char*) &zero, 4);
        
        int32_t hsz = fhdr + ihdr;
        file.write((const char*) &hsz, 4);
        
        file.write((const char*) &ihdr, 4);
        
        file.write((const char*) &width, 4);
        file.write((const char*) &height, 4);
        
        file.put(1);
        file.put(0);
        file.put(24);
        file.put(0);
        
        for(int i = 0; i < 6; i++)
            file.write((const char*) &zero, 4);
    }
    
    px_d& operator()(int x, int y) {
        return pixels[x + y * width];
    }
    
    inline uint8_t trunc(double c) {
        if(c > 1.0)
            c = 1.0;
        if(c < 0.0)
            c = 0.0;
        return 255 * c;
    }
    
    void writeAll() {
        vector<px> datas;
        datas.reserve(width * height);
        for(int i = 0; i < width * height; i++) {
            px nd;
            nd.r = trunc(pixels[i].r);
            nd.g = trunc(pixels[i].g);
            nd.b = trunc(pixels[i].b);
            datas.push_back(nd);
        }
        file.write((const char*) datas.data(), datas.size() * sizeof(px));
    }
};

