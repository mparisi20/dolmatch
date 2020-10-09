#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include "elf.h"
#include <ctype.h>
#include <string>
#include <iostream>
#include <algorithm>
#include <iomanip>
#include <vector>
#include <string>
#include <sstream>
#include <fstream>

using namespace std;

typedef uint32_t u32;
typedef int32_t s32;
typedef uint16_t u16;
typedef int16_t s16;
typedef unsigned char u8;
typedef signed char s8;

struct SymInfo {
    u32 absAddr;
    u32 size;
    string name;
    string module;
    
    SymInfo(u32 addr, u32 sz, string nm, string mod) 
        : absAddr(addr), size(sz), name(nm), module(mod) { }
};

u32 swap32(u32 word) {
	return word >> 24 | 
        (word >> 8 & 0xff00) | 
		(word << 8 & 0xff0000) |
		word << 24; 
}

u16 swap16(u16 hword) {
	return hword >> 8 | hword << 8;
}

u32 be32_to_cpu(const u8 *buf) {
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

class PPCInstr {
    u32 instr;
public:
    enum Opcode : u8
    {
        addic = 12,
        addic_r = 13,
        addi = 14,
        addis = 15,
        bc = 16,
        b_x = 18,
        ori = 24,
        lwz = 32,
        lwzu = 33,
        lbz = 34,
        lbzu = 35,
        stw = 36,
        stwu = 37,
        stb = 38,
        stbu = 39,
        lhz = 40,
        lhzu = 41,
        lha = 42,
        lhau = 43,
        sth = 44,
        sthu = 45,
        lmw = 46,
        stmw = 47,
        lfs = 48,
        lfsu = 49,
        lfd = 50,
        lfdu = 51,
        stfs = 52,
        stfsu = 53,
        stfd = 54,
        stfdu = 55
    };

    PPCInstr(const unsigned char *pInstr) : instr(be32_to_cpu(pInstr)) { }
    
    friend ostream& operator<<(ostream& s, const PPCInstr& p);
    
    bool operator==(const PPCInstr& other) const
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
    
    // get the signed displacement encoded in a branch instruction
    s32 getLIDisp() const
    {
        return (instr & 0x03fffffc) | ((instr & 0x02000000) ? 0xfc000000 : 0);
    }
    
    // get the signed displacement encoded in a conditional branch instruction
    s32 getBDDisp() const
    {
        return (instr & 0x0000fffc) | ((instr & 0x00008000) ? 0xffff0000 : 0);
    }
    
    void clearDispField()
    {
        instr &= 0xffff0000;
    }
    
    void clearAReg()
    {
        instr &= 0xffe0ffff;
    }

    bool isLisInstr() const {
        return opcode() == Opcode::addis && a_reg() == 0;
    }

    // bl, b, bla, ba
    bool isBranch() const {
        return opcode() == Opcode::b_x;
    }
    
    bool isBranchCond() const {
        return opcode() == Opcode::bc;
    }
    
    // b instruction
    bool isB() const {
        return isBranch() && !((instr & 0x2 == 1) || (instr & 0x1 == 1));
    }
    
    // beq, bne, blt, etc
    bool isBc() const {
        return isBranchCond() && !((instr & 0x2 == 1) || (instr & 0x1 == 1));
    }
    
    bool isBlr() const {
        return instr == 0x4e800020;
    }
    
    // TODO: What other opcodes should this function look for?
    bool isLoadStoreDisplacement(bool *updated) const {
        *updated = false;
        switch (opcode()) {
            case stw: case lwz:
            case sth: case lhz:
            case lbz: case stb:
            case lha: case lfd:
            case stfd: case lfs:
            case stfs: case lmw:
            case stmw:
                return true;
            case stwu: case stbu:
            case lwzu: case lbzu:
            case lhzu: case lhau:
            case sthu: case lfsu:
            case lfdu: case stfsu:
            case stfdu:
                *updated = true;
                return true;
        }
        return false;
    }

    // Indicate whether this is an addi (or similar) instruction using the
    // specified base register (helpful for finding static data accesses)
    bool isAddiReg(u8 reg) const {
        switch (opcode()) {
            case addi: case ori:
            case addic: case addic_r:
                return reg == a_reg();
            default:
                return false;
        }
    }

    // Indicate whether this instruction is a load or store involving one of the small data
    // areas; or retrieving a pointer from the R2 or R13 SDAs
    // TODO: returning true for addi instructions using R0 creates a conflict with
    // li instructions... Leaving it out for now.
    bool isSDALoadStoreOrAddi(bool *updated) const {
        return (isLoadStoreDisplacement(updated) &&
                 (a_reg() == 13 || 
                  a_reg() == 2 || 
                  a_reg() == 0)
               ) || isAddiReg(13) || isAddiReg(2);
    }
};

ostream& operator<<(ostream& s, const PPCInstr& p)
{
    s << (p.instr>>24) << " " << (p.instr>>16 & 0xff) << " " << (p.instr>>8 & 0xff)
        << " " << (p.instr & 0xff) << endl;
}

class TextSection {
protected:
    unsigned char *fileBuf;
    u32 textIndex;
    u32 numInstrs;
    u32 baseAddr;
    
    // Loads contents of file at path into fileBuf. Derived classes 
    // must complete the initialization
    TextSection(const char *path)
    {
        FILE *fp = fopen(path, "rb");
        if (!fp) {
            fprintf(stderr, "ERROR: cannot open file '%s'\n", path);
            exit(EXIT_FAILURE);
        }
        
        fseek(fp, 0, SEEK_END);
        u32 fileSz = ftell(fp);
        rewind(fp);
        
        fileBuf = new unsigned char[fileSz];
        if (!fileBuf) {
            fprintf(stderr, "ERROR: failed to malloc fileBuf\n");
            exit(EXIT_FAILURE);
        }
        
        if (fread(fileBuf, fileSz, 1, fp) != 1) {
            fprintf(stderr, "ERROR: cannot read file '%s'\n", path);
            delete[] fileBuf;
            exit(EXIT_FAILURE);
        }
        
        fclose(fp);
    }
    
public:
    vector<PPCInstr> instrBuf;
    
    virtual ~TextSection()
    {
        delete[] fileBuf;
    }
    
    u32 getBaseAddr() const {
        return baseAddr;
    }
    
    u32 getNumInstrs() const {
        return numInstrs;
    }

    // Attempt to clear any instruction fields whose values are 
    // patched by the linker as part of relocation. Disregarding 
    // differences only due to relocation allows more matches to 
    // be found between the compared DOLs.
    void clearRelocatedFields()
    {
        bool updated;
        for (size_t i = 0; i < numInstrs; i++) {
            PPCInstr& instr = instrBuf[i];
            if (instr.isBranch()) {
                instr.clearLIField();
            } else if (instr.isSDALoadStoreOrAddi(&updated)) {
                instr.clearDispField();
                // Note: In ELF relocatable modules, the A field is cleared for
                // this kind of instruction, so I ensure that it is cleared for DOLs, too.
                instr.clearAReg(); 
            } else if (instr.isLisInstr()) {
                u8 lisReg = instr.d_reg();
                instr.clearDispField();
                s32 skipBlr = 0; // skip ahead amount for functions that continue after a blr
                // Clear disp of adds, loads, stores somewhat later in the code that
                // use the same register that lis loaded.
                // Not a perfect solution, but should help find more matches
                for (size_t j = i+1; j < numInstrs; j++) {
                    PPCInstr& nextInstr = instrBuf[j];
                    if (nextInstr.isB() && nextInstr.getLIDisp() > skipBlr) {
                        skipBlr = nextInstr.getLIDisp();
                    } else if (nextInstr.isBc() && nextInstr.getBDDisp() > skipBlr) {
                        skipBlr = nextInstr.getBDDisp();
                    }
                    if (nextInstr.isLisInstr() && nextInstr.d_reg() == lisReg)
                        break;
                    if (nextInstr.isBlr() && skipBlr == 0) {// try to avoid stepping into other functions
                        break;
                    }
                    if ((nextInstr.isLoadStoreDisplacement(&updated) && nextInstr.a_reg() == lisReg)
                        || nextInstr.isAddiReg(lisReg)) {
                        nextInstr.clearDispField();
                        if (updated || nextInstr.d_reg() == lisReg) {
                          //  if (!(nextInstr.opcode() == PPCInstr::Opcode::addi 
                          //      && nextInstr.a_reg() == lisReg)) {
                                break;
                          //  }
                        }
                    }
                    // decrement
                    if (skipBlr != 0) skipBlr -= sizeof(PPCInstr);
                }
            }
        }
    }
};

class ElfTextSection : public TextSection {
	Elf32_Ehdr *ehdr;
	Elf32_Shdr *shStrTabShdr;
	Elf32_Shdr *symTabShdr;
	Elf32_Shdr *strTabShdr;
    Elf32_Shdr *textShdr;
public:
    static Elf32_Ehdr *swapEhdr(Elf32_Ehdr *ehdr);
    static Elf32_Shdr *swapShdr(Elf32_Shdr *shdr);
    static Elf32_Sym *swapSym(Elf32_Sym *sym);

    ElfTextSection(const char *elfName) 
        : TextSection(elfName)
    {
        baseAddr = 0;
        
        if (memcmp(fileBuf, "\177ELF", 4)) {
            fprintf(stderr, "ERROR: %s is not an ELF file\n", elfName);
            exit(EXIT_FAILURE);
        }
        
        ehdr = swapEhdr((Elf32_Ehdr *)fileBuf);
        
        Elf32_Shdr *shdr;
        for (u32 i = 0; i < ehdr->e_shnum; i++) {
            shdr = getSection(i);
            swapShdr(shdr);
        }
        
        if (ehdr->e_shstrndx != SHN_UNDEF) {
            shStrTabShdr = getSection(ehdr->e_shstrndx);
        }
        
        char *sname;
        for (u32 i = 0; i < ehdr->e_shnum; i++) {
            shdr = getSection(i);
            sname = getSectionName(shdr->sh_name);
            if (sname) {
                if (!strcmp(sname, ".symtab")) {
                    symTabShdr = shdr;
                } else if (!strcmp(sname, ".strtab")) {
                    strTabShdr = shdr;
                } else if (!strcmp(sname, ".text")) {
                    textShdr = shdr;
                    textIndex = i;
                }
            }
        }
        swapSymbolTable();
        
        numInstrs = textShdr->sh_size / sizeof(PPCInstr);
        for (u32 i = 0; i < numInstrs; i++) {
            instrBuf.push_back(PPCInstr(&fileBuf[textShdr->sh_offset + i*sizeof(PPCInstr)]));
        }
    }
    
    Elf32_Shdr *getSection(s32 shndx) const
    {
        return (Elf32_Shdr *)(fileBuf + ehdr->e_shoff + ehdr->e_shentsize * shndx);
    }

    // If this ELF has a .shstrtab section, get the ELF section
    // name at the specified offset into the section header string table
    char *getSectionName(u32 offset) const
    {
        if (offset && shStrTabShdr) {
            return (char*)(fileBuf + shStrTabShdr->sh_offset + offset);
        }
        return NULL;
    }

    // If this ELF has a .strtab section, get the ELF symbol name 
    // at the specified offset into the string table
    char *getName(u32 offset) const
    {
        if (offset && strTabShdr) {
            return (char*)(fileBuf + strTabShdr->sh_offset + offset);
        }
        return NULL;
    }

    // If this ELF has a .symtab section, get the ELF symbol at the
    // specified index of the symbol table
    Elf32_Sym *getSymbol(u32 symTabIndex) const
    {
        if (symTabShdr) {
            return (Elf32_Sym *)(fileBuf + symTabShdr->sh_offset) + symTabIndex;
        }
        return NULL;
    }

    u32 getNumSymbols(void) const
    {
        return symTabShdr->sh_size / sizeof(Elf32_Sym);
    }
    
    void swapSymbolTable(void)
    {
        if (symTabShdr) {
            const u32 numSyms = getNumSymbols();
            for (u32 i = 0; i < numSyms; i++) {
                Elf32_Sym *sym = getSymbol(i);
                swapSym(sym);
            }
        }
    }
    
    vector<SymInfo> parseSymbols(void)
    {
        vector<SymInfo> result;
        u32 numSyms = getNumSymbols();
        for (u32 i = 0; i < numSyms; i++) {
            Elf32_Sym *sym = getSymbol(i);
            if (ELF32_ST_TYPE(sym->st_info) == STT_FUNC && 
                sym->st_shndx == textIndex) {
                SymInfo info(sym->st_value, sym->st_size,
                        string(getName(sym->st_name)), string());
                result.push_back(info);
            }
        }
        return result;
    }

};

Elf32_Ehdr *ElfTextSection::swapEhdr(Elf32_Ehdr *ehdr)
{
	ehdr->e_type = swap16(ehdr->e_type);
	ehdr->e_machine = swap16(ehdr->e_machine);
	ehdr->e_version = swap32(ehdr->e_version);
	ehdr->e_entry = swap32(ehdr->e_entry);
	ehdr->e_phoff = swap32(ehdr->e_phoff);
	ehdr->e_shoff = swap32(ehdr->e_shoff);
	ehdr->e_flags = swap32(ehdr->e_flags);
	ehdr->e_ehsize = swap16(ehdr->e_ehsize);
	ehdr->e_phentsize = swap16(ehdr->e_phentsize);
	ehdr->e_phnum = swap16(ehdr->e_phnum);
	ehdr->e_shentsize = swap16(ehdr->e_shentsize);
	ehdr->e_shnum = swap16(ehdr->e_shnum);
	ehdr->e_shstrndx = swap16(ehdr->e_shstrndx);
	
	return ehdr;
}

Elf32_Shdr *ElfTextSection::swapShdr(Elf32_Shdr *shdr)
{
	shdr->sh_name = swap32(shdr->sh_name);
	shdr->sh_type = swap32(shdr->sh_type);
	shdr->sh_flags = swap32(shdr->sh_flags);
	shdr->sh_addr = swap32(shdr->sh_addr);
	shdr->sh_offset = swap32(shdr->sh_offset);
	shdr->sh_size = swap32(shdr->sh_size);
	shdr->sh_link = swap32(shdr->sh_link);
	shdr->sh_info = swap32(shdr->sh_info);
	shdr->sh_addralign = swap32(shdr->sh_addralign);
	shdr->sh_entsize = swap32(shdr->sh_entsize);
	
	return shdr;
}


Elf32_Sym *ElfTextSection::swapSym(Elf32_Sym *sym)
{
	sym->st_name = swap32(sym->st_name);
	sym->st_value = swap32(sym->st_value);
	sym->st_size = swap32(sym->st_size);
	sym->st_shndx = swap16(sym->st_shndx);
	
	return sym;
}

class DolTextSection : public TextSection {
    static const u32 offSctOffsets = 0;
    static const u32 offSctAddrs = 0x48;
    static const u32 offSctLens = 0x90;
public:    
    DolTextSection(const DolTextSection& orig) = delete;
    DolTextSection& operator=(const DolTextSection& orig) = delete;

    // Open the specified DOL and read in its .text section's file offset, size, 
    // address, and contents while converting to native endianness.
    
    DolTextSection(const char *elfName) 
        : TextSection(elfName)
    {
        // Assume section index 1 corresponds to the .text section of the DOL
        textIndex = 1; 
        u32 textOffset; // beginning of .text section in the DOL
        u32 scaledIndex = textIndex * sizeof(PPCInstr);
        textOffset = be32_to_cpu(&fileBuf[offSctOffsets + scaledIndex]);        
        numInstrs = be32_to_cpu(&fileBuf[offSctLens + scaledIndex]) / sizeof(PPCInstr);
        baseAddr = be32_to_cpu(&fileBuf[offSctAddrs + scaledIndex]);
        
        for (size_t i = 0; i < numInstrs; i++)
            instrBuf.push_back(PPCInstr(&fileBuf[textOffset + i*sizeof(PPCInstr)]));
    }
};

// Parse the provided symbol map (Dolphin format: "addr size addr 0 symbol_name")
// TODO: add support for other .map formats
vector<SymInfo> parseMapFile(const char *fname)
{
    ifstream mapFile(fname);
    vector<SymInfo> symbolInfos;
    string line;
    while (getline(mapFile, line)) {
        istringstream ss(line);
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
            symbolInfos.push_back(SymInfo(absAddr, size, name, ""));
        }
    }
    return symbolInfos;
}

class MapFile {
    struct MapFileLine {
        u32 addr;
        string symbol;
        MapFileLine(u32 a, string sym) : addr(a), symbol(sym) { }
        
        bool operator<(const MapFileLine& other) const {
            return addr < other.addr;
        }
    };
public:
    vector<MapFileLine> lines;
    
    MapFile(const char *fname)
    {
        ifstream mapFile(fname);
        string line, sym;
        u32 addr;
        while (getline(mapFile, line)) {
            istringstream ss(line);
            ss >> hex >> addr;
            getline(ss, sym);
            trim(sym);
            lines.push_back(MapFileLine(addr, sym));
        }
        sort(lines.begin(), lines.end());
    }
};

// parse the map file format found in Brawl
vector<SymInfo> parseMapFile2(const char *fname)
{
    MapFile map(fname);
    vector<SymInfo> symbolInfos;
    string line, nextLine;
    
    for (u32 i = 0; i+1 < map.lines.size(); i++) {
        istringstream ss(map.lines[i].symbol);
        char discardChar;
        u32 size;
        string funcName(""), className(""), moduleName("");
        size = map.lines[i+1].addr - map.lines[i].addr;
        getline(ss, funcName, '/');
        trim(funcName);
        if (ss.peek() == '[') {
            ss.read(&discardChar, 1);
            getline(ss, className, ']');
            ss.read(&discardChar, 1);
        }
        ss.read(&discardChar, 1);
        getline(ss, moduleName, ')');
        symbolInfos.push_back(
            SymInfo(
                map.lines[i].addr, 
                size, 
                (className != "") ? className + "." + funcName : funcName, 
                moduleName
            )
        );
    }
    if (map.lines.size() != 0) {
        // TODO: push the final symbol, setting its size to 4 (since there's no next symbol
        // to compute the size from)
        //symbolInfos.push_back(SymInfo(
    }
    return symbolInfos;
}

void debugDump(const DolTextSection& s1, const DolTextSection& s2, u32 addr1, u32 addr2, u32 size)
{
    ofstream f1("f1.dump"), f2("f2.dump");
    u32 offset1 = (addr1 - s1.getBaseAddr())/sizeof(PPCInstr);
    u32 offset2 = (addr2 - s2.getBaseAddr())/sizeof(PPCInstr);
    for (size_t i = 0; i < size/sizeof(PPCInstr); i++) {
        f1 << hex << addr1 + i*sizeof(PPCInstr) << ": " << setfill('0') << setw(2) << right << s1.instrBuf[offset1 + i];
        f2 << hex << addr2 + i*sizeof(PPCInstr) << ": " << setfill('0') << setw(2) << right << s2.instrBuf[offset2 + i];
    }
}

void failUsage(void)
{
    cerr << "usage: ./dolmatch <searchSymMap>.map <mapType> <searchDol>.dol <targetDol>.dol" << endl;
    cerr << "  Supported mapTypes:\n    1: Dolphin MAP file\n    2: Brawl format map file" << endl;
    cerr << "\nor\n./dolmatch <searchElf>.o <targetDol>.dol" << endl;
    exit(EXIT_FAILURE);
}

enum Mode
{
    DOL_COMPARE, // search another .dol file
    ELF_COMPARE, // search an ELF .o file
};

int main(int argc, char *argv[])
{
    Mode mode;
    int mapType;
    TextSection *searchText, *targetText;
    vector<SymInfo> symbolInfos;
    if (argc == 5) {
        mode = DOL_COMPARE;
        mapType = atoi(argv[2]);
        if (mapType == 1) {
            symbolInfos = parseMapFile(argv[1]);
        } else if (mapType == 2) {
            symbolInfos = parseMapFile2(argv[1]);
        } else {
            failUsage();
        }
        searchText = new DolTextSection(argv[3]);
        targetText = new DolTextSection(argv[4]);
    } else if (argc == 3) {
        mode = ELF_COMPARE;
        ElfTextSection *elf = new ElfTextSection(argv[1]);
        symbolInfos = elf->parseSymbols();
        searchText = static_cast<TextSection*>(elf);
        targetText = new DolTextSection(argv[2]);
    } else {
        failUsage();
    }
    const char *searchName, *targetName;    
    if (mode == DOL_COMPARE) {
        searchName = argv[3];
        targetName = argv[4];
    } else {
        searchName = argv[1];
        targetName = argv[2];
    }
    
    searchText->clearRelocatedFields();
    targetText->clearRelocatedFields();
    
    #if 0 // DEBUG
        debugDump(searchText, targetText, 0x801ede0c, 0x8027dedc, 0x910);
    #endif
    
    vector<PPCInstr> searchFunc;
    
    cout << "Begin search for identical functions between " << searchName << " and " << targetName << ":\n" << endl;
    cout << "Symbol_Name\t" << searchName << "_Address\t" << targetName << "_Address\t" << "Func_Size" << endl;
    u32 matchCount = 0;
    u32 totalMatchSize = 0;
    for (auto sym : symbolInfos) {
        if (sym.absAddr < searchText->getBaseAddr() || 
            sym.absAddr >= searchText->getBaseAddr() + searchText->getNumInstrs()*sizeof(PPCInstr)) {
            cout << hex << "NOTE: " << sym.name << " is not in the .text section" << endl;
            continue;
        }
        
        u32 searchOffset = (sym.absAddr - searchText->getBaseAddr()) / sizeof(PPCInstr);
        u32 searchLen = sym.size / sizeof(PPCInstr);
        auto searchStart = searchText->instrBuf.begin() + searchOffset;
        auto searchEnd = searchText->instrBuf.begin() + searchOffset + searchLen;
        auto searchResult = 
            search(targetText->instrBuf.begin(), targetText->instrBuf.end(), searchStart, searchEnd);
        if (searchResult != targetText->instrBuf.end()) {
            // found an identical function between DOLs
            u32 foundFuncAddr = targetText->getBaseAddr() + 
                std::distance(targetText->instrBuf.begin(), searchResult) * sizeof(PPCInstr);
            cout << sym.name << " " << hex << sym.absAddr << " " << foundFuncAddr << " " << sym.size << "\n";
            matchCount++;
            totalMatchSize += sym.size;
        }
    }
    
    cout << "\n\nFound " << dec << matchCount 
        << " possibly identical functions between " << searchName << " and " 
        << targetName << " (0x" << hex << totalMatchSize << " bytes total, " << dec << setprecision(3) 
        << 100.0 * (totalMatchSize*1.0 / (targetText->getNumInstrs() * sizeof(PPCInstr))) << "% of " << targetName << "\'s .text section)" << endl;
    delete searchText;
    delete targetText;
    return 0;
}
