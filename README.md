# dolmatch 
###### by Max Parisi

This program searches for identical strings of object code within the .text section of two DOLs, 
while trying its best to ignore differences due only to link-time relocation.
If a match is found, it notes its symbol from the provided .map file (which
may be either in Dolphin format or the format found in Brawl's symbol map)
and the absolute addresses of the function's location in both DOLs.

This program may allow one to use a DOL with a symbol table to migrate symbols to
another DOL for which no symbol table is available. This can be useful when it is
likely that both DOLs were linked with the same object code libraries, such as those which
may be provided by a common SDK.
