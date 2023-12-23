\ PART 8: Agon related words. (@jackokring)

REQUIRE misc.4th
REQUIRE files.4th

1 CELLS 2 = [IF]

: VLOAD ( daddr dlen "ccc-file" "ccc-name" ---)
\G Load the file named "file" at address daddr, and make a dictionary entry
\G to play the files sequence as a VDU sequence.
    2DUP 2>R 2SWAP 2DUP 2>R 2SWAP DBLOAD CREATE 2R> SWAP , , 2R> SWAP , ,
    DOES> CR 4  0 DO DUP I CELLS + @ SWAP LOOP CR DROP \ Have daddr dlen
    BEGIN
        2DUP 0. D= NOT
    WHILE
        2>R 2DUP XC@ EMIT
        1 0 D+ \ Next byte
        2R> 1 0 D- \ One less to do
    REPEAT
    2DROP 2DROP ;   

[ELSE]

: VLOAD ( daddr dlen "ccc-file" "ccc-name" ---)
\G Load the file named "file" into the dictionary, and make a dictionary entry
\G to play the files sequence as a VDU sequence.
\G Ignore daddr parameter, which is only used for 16-bit FORTH.
    DROP BL WORD COUNT R/O OPEN-FILE -38 ?THROW >R
    CREATE DUP , HERE OVER ALLOT SWAP R@ READ-FILE 2DROP R> CLOSE-FILE DROP
    2DROP
  DOES> DUP @ SWAP CELL+ SWAP
      BOUNDS DO I C@ EMIT LOOP
  ;
[THEN]
