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
    * MODULE directive added
* Help pages embedded into source code
    * A very old address has been removed

Otherwise, the source is as written (6 character labels, < 40 characters per line)

# Missing source code

This set of tools are related - a lot of the code is shared between them. As things
stand two tools are missing from the repository.

## Transfer (+3 -> +D discs on +3 B: drive).

The writing counterpart of convert. I have a revision of the source
code that's probably 5 versions behind the last release.

## Slowdos 1.0

Was a screen resident program to read/write MSDOS discs on a +3 B: drive.

I have some versions of the source, but not verified that a loadable program can be made.

The source code for Slowdos 2.x which extended +3 BASIC to allow access to MSDOS discs is
located here: http://github.com/suborb/slowdos

