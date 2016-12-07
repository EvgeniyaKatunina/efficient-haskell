#include <string>
#include <fstream>
#include <vector>
#include <stdint.h>

using namespace std;

inline int32_t readInt(ifstream& in) {
    int32_t tmp;
    in.read((char*) &tmp, 4);
    return tmp;
}

inline int16_t readShort(ifstream& in) {
    int16_t tmp;
    in.read((char*) &tmp, 2);
    return tmp;
}

struct WaveFile {
    int32_t frequency;
    vector<vector<short>> channels;
};

inline WaveFile readWAV(const string& filename) {
    ifstream in(filename, ios::binary | ios::in);
    
    WaveFile rv;
    
    auto hdr = readInt(in);
    // todo: confirm it's RIFF
    auto sz = readInt(in);
    
    auto id = readInt(in);
    id = readInt(in);
    
    auto sz2 = readInt(in);
    
    auto fmt = readShort(in);
    auto chans = readShort(in);
    auto sampleRate = readInt(in);
    auto byteRate = readInt(in);
    auto blockAlign = readShort(in);
    auto bits = readShort(in);
    
    rv.frequency = sampleRate;
    rv.channels.resize(chans);
    
    // todo: check that bits == 16
    
    id = readInt(in);
    auto sz3 = readInt(in);
    
    auto readSize = sz3 * bits / 8 * chans;
    int16_t *datas = new int16_t[readSize];
    in.read((char*) datas, readSize);
    
    in.close();
    
    for(int j = 0; j < chans; j++) {
        rv.channels[j].reserve(readSize * 8 / bits / chans);
    }
    
    for(int i = 0; i < sz3; i += chans) {
        for(int j = 0; j < chans; j++) {
            rv.channels[j].push_back(datas[i + j]);
        }
    }
    
    return rv;
}
