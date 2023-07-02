\ Z80 meta compiler to be run from an ANSI standard FORTH system
\ that contains the FILE wordset.

\ We need the word VOCABULARY. It's not in the standard though it will
\ be in most actual implementations.
: VOCABULARY WORDLIST CREATE  ,  \ Make a new wordlist and store it in def.
  DOES> >R                      \ Replace last item in the search order.
  GET-ORDER SWAP DROP R> @ SWAP SET-ORDER ;

.( Loading the assembler "asmz80.4th") CR
S" asmez80.4th" INCLUDED

.( Loading the meta compiler "metaz80.4th") CR
S" metaez80.4th" INCLUDED

\ .( Compiling the asm test from "asmtst80.4th") CR
\ S" asmtst80.4th" INCLUDED

.( Compiling the kernel from "kernl24a.4th") CR
S" kernl24a.4th" INCLUDED
.( Compiling the kernel from "kernl24b.4th") CR
S" kernl24b.4th" INCLUDED
.( Compiling the kernel from "kernl24c.4th") CR
S" kernl24c.4th" INCLUDED

\ Save the binary image of the Forth system.
VARIABLE FILEHAND
DECIMAL
: SAVE-IMAGE ( --- )
    S" kernel24.bin" W/O CREATE-FILE THROW FILEHAND !  
  IMAGE THERE ORIGIN - FILEHAND @ WRITE-FILE THROW
  FILEHAND @ CLOSE-FILE THROW
;
SAVE-IMAGE 
.( Image saved as "kernel24.bin") CR

BYE
