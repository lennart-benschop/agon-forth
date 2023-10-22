# agon-forth
Forth for Agon computer

## forth16

The forth16 directory contains a 16-bit FORTH for Agon, to be run in Z-80 mode.

## forth24

The forth124 directory contains a 24-bit FORTH for Agon, to be run in
eZ80 ADL mode. This can use all available RAM. This has slightly more words
than 16-bit FORTH.

## examples

The examples directory contains FORTH example programs that are not specific to Agon FORTH, but that can run under it (and under both versions).
* tetris.4th is a Tetris-like program for text terminals. I used to run it a lot in the early days of Linux.
* tester.4th and core.4th is a small test suite for Forth.
* glosgen.4th is a program to generate glossary files.
* squares.4th is a small example program to run on Agon FORTH.

## examples_agon

The examples_agon directory contains FORTH example programs and utilities
that are specific to Agon FORTH, but that run under both the 16-bit and the
24-bit versions of it.
* serpent.4th Snake-type game originally submitted to the Olimex WPC June 2023.
  This is a slight adaptation of this game.
* restit.4th Tetris-type game originally submitted to the Olimex WPS July 2023.
  Slight modifications.
* graphics.4th Graphics library, including turtle graphics.
* grpdemo.4th Graphics demo, runs on top of graphics.4th
* dodemo.4th. Load graphics.4th and grpdemo.4th and then runs the demo. Put forth.bin (or forth24.bin), graphics.4th, grpdemo.4th and dodemo.4th all in one directory, then the following
  commands run the demo.
```
load forth.bin
run . dodemo.4th
```

