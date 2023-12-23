1 CELLS 2 = [IF]

\ PART 6: File related words.
REQUIRE misc.4th

: NAME ( "ccc" --- addr len)
\G Make a name.
    BL WORD COUNT ;
   
: OSNAME ( addr len --- daddr)
\G Makes an OS string from a name.
    OSSTRING >ASCIIZ OSSTRING MB@ ;

: (FILE) ( "ccc" --- ud)
\G Makes OSSTRING filled with the name in mos format and presents it in 24 bit.
    NAME OSNAME ;

: DBSAVE ( daddr dlen "ccc" ---)
\G Save memory at address daddr, length dlen bytes to a file
\G filename is the next word parsed.       
   2>R 2>R (FILE) 2R> 2R> 2 DOSCALL -37 ?THROW DROP ;

: DBLOAD ( daddr dlen "ccc" ---)
\G Load a file in memory at address daddr, filename is the next word parsed.
\G The dlen parameter is maximum allowed size, but file can be shorter.    
   2>R 2>R (FILE) 2R> 2R> 1 DOSCALL -38 ?THROW DROP ;

: READ-FILE ( c-addr u1 fid --- u2 ior)
\G Read data from the file indicated by fid, into the buffer starting
\G at c-addr. Read at most u1 bytes. u2 is the number of bytes read.
  >R >R MB@ R> 0 R> 0 $1A DOSCALL DROP 0
;

: WRITE-FILE ( c-addr u1 fid --- u2 ior)
\G Write data from the file indicated by fid, from the buffer starting
\G at c-addr. Write at most u1 bytes. u2 is the number of bytes written.
  >R >R MB@ R> 0 R> 0 $1B DOSCALL DROP 0
;

: REPOSITION-FILE ( ud fid --- ior )
\G Position the file indicated by fid to the offset indicated by ud.    
  >R SPLIT 0 R> 0 $1C DOSCALL NIP
;

: RESIZE-FILE ( ud fid --- ior )
\G Resize the file indicated by fid to the size indicated by ud. Currently
\G unimplemented
    2DROP DROP -1 ;
;

: FILE-POSITION ( fid --- ud ior)
\G Return the file position. Currently unimplemented.    
  DROP 0 0 -1
;

: FILE-SIZE ( fid --- ud ior)
\G Return the file size. Currently unimplemented.    
  DROP 0 0 -1
;

[THEN]