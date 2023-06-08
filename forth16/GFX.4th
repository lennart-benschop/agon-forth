: MODE ( n ---)
\G Select graphics mode
  22 EMIT DUP EMIT ;

: 2EMIT ( n ---)
\G EMIT n as two characters, LSB first.
  DUP 8 RSHIFT SWAP EMIT EMIT ;

0 CONSTANT BLACK
 1 CONSTANT RED
 2 CONSTANT GREEN
 3 CONSTANT YELLOW
 4 CONSTANT BLUE
 5 CONSTANT MAGENTA
 6 CONSTANT CYAN
 7 CONSTANT LIGHT-GREY 7 CONSTANT LIGHT-GRAY
 8 CONSTANT DARK-GREY 8 CONSTANT DARK-GRAY
 9 CONSTANT BRIGHT-RED
10 CONSTANT BRIGHT-GREEN
11 CONSTANT BRIGHT-YELLOW
12 CONSTANT BRIGHT-BLUE
13 CONSTANT BRIGHT-MAGENTA
14 CONSTANT BRIGHT-CYAN
15 CONSTANT WHITE


: FG ( c ---)
  17 EMIT EMIT ;
: BG ( c ---)
  17 EMIT 128 + EMIT ;
: GC ( c ---)
  18 EMIT 0 EMIT EMIT ;

: CELLS+ ( ---)
cells + ;

: ? ( addr ---)
@ . ;

: c? ( addr ---)
C@ . ;

: .FREE ( ---)
 $fe00 pad 80 + - u. ;

\ gfx words

: *FX19 ( ---)
sysvars xC@ begin sysvars xC@ over <> until DROP ;

:
VDU23 ( ---)
23 EMIT 27 EMIT ;

: UDC ( d0 ... d7 char --- );
23 EMIT
EMIT
8 0 do
EMIT
loop ;

: SELECT_BITMAP ( n ---)
VDU23 0 EMIT EMIT ;

: LOAD_BITMAP_RGB ( data w h --)
VDU23 1 EMIT \ load bitmap
2DUP 2EMIT 2EMIT \ width & height
* 3 * 0 do
255 EMIT 
DUP i 2 + + C@ EMIT
DUP i 1 + + C@ EMIT
DUP i 0 + + C@ EMIT
3 +LOOP 
DROP ;

: LOAD_BITMAP_RGBA ( data w h --)
VDU23 1 EMIT \ load bitmap
2DUP 2EMIT 2EMIT \ width & height
* 4 * 0 do
DUP i 3 + + C@ EMIT 
DUP i 2 + + C@ EMIT
DUP i 1 + + C@ EMIT
DUP i 0 + + C@ EMIT
4 +LOOP 
DROP ;


: DRAW_BITMAP ( y x --)
VDU23 3 EMIT 2EMIT 2EMIT ;

: SELECT_SPRITE ( n --)
VDU23 4 EMIT 2EMIT ;

: CLEAR_SPRITE ( --)
VDU23 5 EMIT ;

: BITMAP2SPRITE ( n --)
VDU23 6 EMIT 2EMIT ;

: ACTIVATE_SPRITES ( n --)
VDU23 7 EMIT 2EMIT ;

: SHOW_SPRITE ( --)
VDU23 11 EMIT ;

: HIDE_SPRITE ( --)
VDU23 12 EMIT ;

: MOVE_SPRITE_TO ( y x --)
VDU23 13 EMIT 2EMIT 2EMIT ;

: MOVE_SPRITE_BY ( y x --)
VDU23 14 EMIT 2EMIT 2EMIT ;

: UPDATE_SPRITES ( --)
VDU23 15 EMIT ;

: RESET_SPRITES ( --)
VDU23 16 EMIT ;

VARIABLE SEED 
SYSVARS XC@ SEED !

: RAND ( n1 --- n2)
\G A simple random generator return a number in range 0..n2-1
  SEED @ 3533 * 433 + DUP SEED ! UM* NIP ;

: CLEARSTACK ( ? --);
depth 0 do DROP loop ;

: N>S ( u -- addr u)
DUP >R ABS S>D <# #S R> SIGN #> ;

: CUROFF ( ---)
\G Switch cursor off
  23 EMIT 1 EMIT 0 EMIT ;

: CURON ( ---)
\G Switch cursor on
  23 EMIT 1 EMIT 1 EMIT ;

\ eof
