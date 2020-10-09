# dolmatch 
###### by Max Parisi

This program searches for identical strings of object code within the `.text` sections of two DOLs, 
or an ELF relocatable object and a DOL, while trying its best to ignore differences due only to 
link-time relocation. If a match is found, it notes its symbol from the provided `.map` file (which
may be either in Dolphin format or the format found in Brawl's symbol map) and the absolute addresses 
of the function's location in both DOLs. No `.map` file needs to be provided for an ELF relocatable object,
however, as `dolmatch` will parse the symbols from the ELF object's `.symtab` section, instead. Also, 
the "address" of a function from an ELF object is its starting offset in the `.text` section.

This program may allow one to use a DOL with a symbol table to migrate symbols to
another DOL for which no symbol table is available. This can be useful when it is
likely that both DOLs were linked with the same object code libraries, such as those which
may be provided by a common SDK.

## Usage
`./dolmatch <searchSymMap>.map <mapType> <searchDol>.dol <targetDol>.dol`
    `Supported mapTypes:` 
       `1: Dolphin MAP file`
       `2: Brawl format map file`

or

`./dolmatch <searchElf>.o <targetDol>.dol`
