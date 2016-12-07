using System;
using System.IO;

public struct WaveFile {
    public int frequency;
    public short[][] channels;
};

public static class WaveReader {
    public static WaveFile readWAV(string filename) {
        var stream = new FileStream(filename, FileMode.Open, FileAccess.Read);
        var reader = new BinaryReader(stream);
        
        WaveFile rv = new WaveFile();
        
        var hdr = reader.ReadInt32();
        var sz = reader.ReadInt32();
        
        var id = reader.ReadInt32();
        id = reader.ReadInt32();
        
        var sz2 = reader.ReadInt32();
        
        var fmt = reader.ReadInt16();
        var chans = reader.ReadInt16();
        var sampleRate = reader.ReadInt32();
        var byteRate = reader.ReadInt32();
        var blockAlign = reader.ReadInt16();
        var bits = reader.ReadInt16();
        
        Console.WriteLine(hdr);
        Console.WriteLine(chans);
        Console.WriteLine(sampleRate);
        Console.WriteLine(byteRate);
        Console.WriteLine(bits);
        
        rv.frequency = sampleRate;
        rv.channels = new short[chans][];
    
        // todo: check that bits == 16
        
        id = reader.ReadInt32();
        var sz3 = reader.ReadInt32();
        
        Console.WriteLine(sz3);
        
        var readSize = sz3 * bits / 8 * chans;
        
        for(int j = 0; j < chans; j++) {
            rv.channels[j] = new short[readSize * 8 / bits / chans];
        }
        
        byte[] rawdata = new byte[readSize];
        reader.Read(rawdata, 0, readSize);
    
        for(int i = 0; i < sz3 / chans / (bits / 8); i += 1) {
            for(int j = 0; j < chans; j++) {
                rv.channels[j][i] = (short) (rawdata[(i * chans + j) * 2] + 256 * rawdata[(i * chans + j) * 2 + 1]);
            }
        }
        
        return rv;
    }
}
