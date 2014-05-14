simple-forth
============

This is a FORTH implementation for the Amiga, tested on Workbench 1.2. It was
created sometime during 1994 (version string says 24.7.4, so I assume '94), when I got my hands on an old book on FORTH from
the Physics Department of Aristotle University of Thessaloniki; sorry, I can't remember the books name.

This forth is written as a kernel in m68k assembly (forth.asm) (which needs a proper assembler BTW), and a series of FORTH files (extension .FTH) in the VOCAB folder. 
It's a bit limited, but I remember it working properly - sadly I no longer have access to my other media from that era.

Interesting features:
* words are directly executable; get the word address, and a JSR will execute it. 
* as the words are created, they are compiled directly to machine code. Most words are simply CALLs, but all intrinsics (stack manipulation, arithmetic, etc) are implemented by inlining their contents. Thus they are both special and non-special. The user can tag newly created words like that.
* words can be tagged to be executed after recognition
* support for vocabularies, which are separate lists of words; these can be forgotten separately from the other vocabularies.
* vocabularies can be loaded directly from executable files (using LoadSeg); these can define their own load/unload code.
* apparently assumes that there will be a driven named "FORTH:" that has a file named "forth.opt" containing a list of files to read before the first argument - and a # marks a comment.
* I remember making it more than 100x faster when I implemented input buffering...

Interfacing
-----------

convert.bas is an AmigaBASIC file that can read .fd files, files that describe the system/library calls of AmigaOS, and create the corresponding assembly stubs for simple-FORTH. In the VOCAB folder are several, some compiled (FEX files) - you will notice that the preamble is copy-pasted from fle FORTH.ASM by hand...
