# ZX Native Disc Transformer Utilities

A selection of native tools to convert between various disc formats.

# History

This is a collection of source code to tools that I wrote in the early 90s. The utilities were
written on +3 using a modified version of the OCP Editor Assembler.

The files were transferred from the original discs and made readable using the tools in
http://github.com/suborb/zx_conversion_utils

The following modifications have been made to the source code:

* Updated to compile with z80asm from z88dk
    * db, dw, ds -> defb, defw, defs
    * equ -> defc
    * String constants now double quoted
* A very old address has been removed


# Lost source code

## Transfer (+3 -> +D discs on +3 B: drive).

The writing counterpart of convert. I have a revision of the source
code that's probably 5 versions behind the last release.

## Slowdos 1.0

Was a screen resident program to read/write MSDOS discs on a +3 B: drive.

I have some versions of the source, but not verified that a loadable program can be made.

