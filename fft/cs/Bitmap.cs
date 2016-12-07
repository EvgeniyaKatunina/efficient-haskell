using System;
using System.IO;

public struct px_d {
    public double r, g, b;
};

public class Bitmap {
    private int width;
    private int height;
    public px_d[] pixels;
    private BinaryWriter file;

    public Bitmap(int w, int h, string filename) {
        width = w;
        height = h;
        pixels = new px_d[w * h];
        file = new BinaryWriter(new FileStream(filename, FileMode.OpenOrCreate, FileAccess.Write));
        writeHeader();
    }
    
    private void writeHeader() {
        int fhdr = 14;
        int ihdr = 40;

        file.Write((byte) 'B');
        file.Write((byte) 'M');
        var fsz = fhdr + ihdr + width*height*3;
        
        file.Write((int) fsz);
        file.Write((int) 0);
        file.Write((int) (fhdr + ihdr));
        file.Write((int) ihdr);
        file.Write((int) width);
        file.Write((int) height);
        file.Write((short) 1);
        file.Write((short) 24);
        
        for(int i = 0; i < 6; i++) {
            file.Write((int) 0);
        }
    }
    
    public int idx(int x, int y) {
        return x + y * width;
    }
    
    private byte trunc(double c) {
        if(c > 1.0)
            c = 1.0;
        if(c < 0.0)
            c = 0.0;
        return (byte) (255 * c);
    }
    
    public void writeAll() {
        var datas = new byte[width * height * 3];
        for(int i = 0; i < width * height; i++) {
            datas[i * 3] = trunc(pixels[i].r);
            datas[i * 3 + 1] = trunc(pixels[i].g);
            datas[i * 3 + 2] = trunc(pixels[i].b);
        }
        file.Write(datas);
    }
};
