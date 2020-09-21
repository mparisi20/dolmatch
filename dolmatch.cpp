/*
dolmatch

By Max Parisi
github.com/mparisi20
*/

#include <cstdlib>
#include <cinttypes>
#include <iostream>
#include <string>
#include <sstream>
#include <fstream>
#include <algorithm> 
#include <vector>
#include <iomanip>

using namespace std;

typedef uint32_t u32;
typedef unsigned char u8;

struct SymInfo {
    u32 absAddr;
    u32 size;
    string name;
    
    SymInfo(u32 addr, u32 sz, string nm) 
        : absAddr(addr), size(sz), name(nm) { }
};

u32 be32_to_cpu(const u8 *buf)
{
    return (u32)buf[3] | (u32)buf[2] << 8 | (u32)buf[1] << 16 | (u32)buf[0] << 24;
}

// https://stackoverflow.com/questions/216823/whats-the-best-way-to-trim-stdstring
// trim from start (in place)
static inline void ltrim(std::string &s) {
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int ch) {
        return !std::isspace(ch);
    }));
}

// trim from end (in place)
static inline void rtrim(std::string &s) {
    s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
        return !std::isspace(ch);
    }).base(), s.end());
}

// trim from both ends (in place)
static inline void trim(std::string &s) {
    ltrim(s);
    rtrim(s);
}

class PPCInstruction
{
    enum Opcode : u8
    {
        addi = 14,
        addis = 15,
        stw = 36,
        lwz = 32,
        sth = 44,
        lhz = 40,
        lha = 42,
        stb = 38,
        lbz = 34,
        b_x = 18,
        lfs = 48,
        lfd = 50,
        stfs = 52,
        stfd = 54
    };

    u32 instr;
public:
    PPCInstruction(const u8 *pInstr) : instr(be32_to_cpu(pInstr)) { }

    bool operator==(const PPCInstruction& other) const
    {
        return instr == other.instr;
    }
    
    u8 opcode() const {
        return ((instr & 0xfc000000) >> 26);
    }
    
    u8 a_reg() const {
        return ((instr & 0x001f0000) >> 16);
    }
    
    u8 d_reg() const {
        return ((instr & 0x03e00000) >> 21);
    }

    void clearLIField()
    {
        instr &= 0xfc000003;
    }
    
    void clearDispField()
    {
        instr &= 0xffff0000;
    }

    bool isLisInstr() const {
        return opcode() == Opcode::addis && a_reg() == 0;
    }

    // bl, b, bla, ba
    bool isBranch() const {
        return opcode() == Opcode::b_x;
    }
    
    // TODO: What other opcodes should this function look for?
    bool isLoadStoreDisplacement() const {
        switch (opcode()) {
            case stw: case lwz:
            case sth: case lhz:
            case lbz: case stb:
            case lha: case lfd:
            case stfd: case lfs:
            case stfs:
                return true;
        }
        return false;
    }

    // Indicate whether this is an addi instruction using the
    // specified base register (helpful for finding static data accesses)
    bool isAddiReg(u8 reg) const {
        return opcode() == Opcode::addi && reg == a_reg();
    }

    // Indicate whether this instruction is a load or store involving one of the small data
    // areas; or retrieving a pointer from the R2 or R13 SDAs
    // TODO: returning true for addi instructions using R0 creates a conflict with
    // li instructions... Leaving it out for now.
    bool isSDALoadStoreOrAddi() const {
        return (isLoadStoreDisplacement() &&
                 (a_reg() == 13 || 
                  a_reg() == 2 || 
                  a_reg() == 0)
               ) || isAddiReg(13) || isAddiReg(2);
    }
};

class DolTextSection
{
    // assume section index 1 is the .text section of both DOLs
    static const u32 textIndex = 1;
    static const u32 scaledIndex = sizeof(u32) * textIndex;
    static const u32 offSctOffsets = 0;
    static const u32 offSctAddrs = 0x48;
    static const u32 offSctLens = 0x90;
    static const u32 lookAhead = 100; // TODO: design a better strategy than a constant instruction look-ahead
    FILE *dol;
    u32 textOffset;
    u32 size; // of section in bytes
    u32 numInstrs;
    u32 absAddr;
    
    void tryOpenDol(const char *name, const char *mode)
    {
        dol = fopen(name, mode);
        if (!dol) {
            cerr << "failed to open file " << name << endl;
            exit(EXIT_FAILURE);
        }
    }
    
    void tryReadDol(long offset, void *ptr, size_t size, size_t nmemb)
    {
        fseek(dol, offset, SEEK_SET);
        if (fread(ptr, size, nmemb, dol) != nmemb) {
            cerr << "fread error\n";
            exit(EXIT_FAILURE);
        }
    }

public:
    vector<PPCInstruction> instrBuf;
    
    DolTextSection(const DolTextSection& orig) = delete;
    DolTextSection& operator=(const DolTextSection& orig) = delete;

    // Open the specified DOL and read in its .text section's file offset, size, 
    // address, and contents while converting to native endianness
    DolTextSection(const char *dolName)
    {
        tryOpenDol(dolName, "rb");
        
        tryReadDol(offSctOffsets + scaledIndex, &textOffset, sizeof(u32), 1);
        textOffset = be32_to_cpu((const u8 *)&textOffset);
        
        tryReadDol(offSctLens + scaledIndex, &size, sizeof(u32), 1);
        size = be32_to_cpu((const u8 *)&size);
        numInstrs = size / sizeof(PPCInstruction);
        
        tryReadDol(offSctAddrs + scaledIndex, &absAddr, sizeof(u32), 1);
        absAddr = be32_to_cpu((const u8 *)&absAddr);

        u8 *rawBuf = new u8[size];
        tryReadDol(textOffset, rawBuf, sizeof(u8), size);
        
        for (size_t i = 0; i < size; i += sizeof(PPCInstruction))
            instrBuf.push_back(PPCInstruction(rawBuf + i));
        
        delete[] rawBuf;
    }

    ~DolTextSection() {
        fclose(dol);
    }
    
    u32 getAbsAddr() const {
        return absAddr;
    }
    
    u32 getSize() const {
        return size;
    }
    
    // Attempt to clear any instruction fields whose values are 
    // patched by the linker as part of relocation. Disregarding 
    // differences only due to relocation allows more matches to 
    // be found between the compared DOLs.
    void clearRelocationPatches()
    {
        for (size_t i = 0; i < numInstrs; i++) {
            // load big endian word into small endian arch
            PPCInstruction& instr = instrBuf[i];
            if (instr.isBranch()) {
                instr.clearLIField();
            } else if (instr.isSDALoadStoreOrAddi()) {
                instr.clearDispField();
            } else if (instr.isLisInstr()) {
                u8 lisReg = instr.d_reg();
                instr.clearDispField();
                // Clear disp of adds, loads, stores somewhat later in the code that
                // use the same register that lis loaded.
                // Not a perfect solution, but should help find more matches
                for (size_t j = i+1; j < numInstrs && (j-i) <= lookAhead; j++) {
                    PPCInstruction& nextInstr = instrBuf[j];
                    if (nextInstr.isLisInstr() && nextInstr.d_reg() == lisReg)
                        break;
                    if ((nextInstr.isLoadStoreDisplacement() && nextInstr.a_reg() == lisReg)
                            || nextInstr.isAddiReg(lisReg)) {
                        nextInstr.clearDispField();
                        if (nextInstr.d_reg() == lisReg) break;
                    }
                }
            }
        }
    }
};

// Parse the provided symbol map (Dolphin format: "addr size addr 0 symbol_name")
// TODO: add support for other .map formats
vector<SymInfo> parseMapFile(const char *fname)
{
    ifstream mapFile(fname);
    vector<SymInfo> symbolInfos;
    string line;
    for (size_t i = 0; getline(mapFile, line); i++) {
        stringstream ss(line);
        u32 discard;
        u32 absAddr, size;
        string name;
        ss >> hex >> absAddr >> size >> discard >> discard;
        if (!ss.fail()) {
            // The remainder of the line contains the symbol name
            getline(ss, name);
        }
        if (!ss.fail()) {
            trim(name);
            symbolInfos.push_back(SymInfo(absAddr, size, name));
        }
    }
    return symbolInfos;
}

int main(int argc, char *argv[])
{
    if (argc != 4) {
        cerr << "usage: ./dolmatch <searchSymMap>.map <searchDol>.dol <targetDol>.dol" << endl;
        return EXIT_FAILURE;
    }
    
    vector<SymInfo> symbolInfos = parseMapFile(argv[1]);
    DolTextSection searchDolText(argv[2]);
    DolTextSection targetDolText(argv[3]);
    
    searchDolText.clearRelocationPatches();
    targetDolText.clearRelocationPatches();
    
    vector<PPCInstruction> searchFunc;
    cout << "Begin search for identical functions between " << argv[2] << " and " << argv[3] << ":\n" << endl;
    cout << "Symbol_Name\t" << argv[2] << "_Address\t" << argv[3] << "_Address\t" << "Func_Size" << endl;
    u32 matchCount = 0;
    u32 totalMatchSize = 0;
    for (auto sym : symbolInfos) {
        if (sym.absAddr < searchDolText.getAbsAddr() || 
            sym.absAddr >= searchDolText.getAbsAddr() + searchDolText.getSize()) {
            cout << hex << "NOTE: " << sym.name << " is not in the .text section (" << sym.absAddr <<
                " not in [" << searchDolText.getAbsAddr() << ", " << 
                searchDolText.getAbsAddr() + searchDolText.getSize() << ")" << endl;
            continue;
        }
        
        u32 searchOffset = (sym.absAddr - searchDolText.getAbsAddr()) / sizeof(PPCInstruction);
        u32 searchLen = sym.size / sizeof(PPCInstruction);
        auto searchStart = searchDolText.instrBuf.begin() + searchOffset;
        auto searchEnd = searchDolText.instrBuf.begin() + searchOffset + searchLen;
        auto searchResult = 
            search(targetDolText.instrBuf.begin(), targetDolText.instrBuf.end(), searchStart, searchEnd);
        if (searchResult != targetDolText.instrBuf.end()) {
            // found an identical function between DOLs
            u32 foundFuncAddr = targetDolText.getAbsAddr() + 
                std::distance(targetDolText.instrBuf.begin(), searchResult) * sizeof(PPCInstruction);
            cout << sym.name << " " << hex << sym.absAddr << " " << foundFuncAddr << " " << sym.size << "\n";
            matchCount++;
            totalMatchSize += sym.size;
        }
    }
    
    // TODO: if a SDA-relative or lis-relative address is transferred to another
    // register, then disregard offsets from that register as well
    
    // TODO: For every matched pair of functions, get absolute
    // address targets of every bl instruction in the searchDolText version and mark 
    // them as matching the corresponding targetDolText version if it's not already done
    // Do multiple passes until no new matches are found
    
    cout << "\n\nFound " << dec << matchCount 
        << " possibly identical functions between " << argv[1] << " and " 
        << argv[3] << " (0x" << hex << totalMatchSize << " bytes total, " << dec << setprecision(3) 
        << 100.0 * (totalMatchSize*1.0 / targetDolText.getSize()) << "% of " << argv[3] << "\'s .text section)" << endl;
    return 0;
}
