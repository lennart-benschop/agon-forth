\ PART 8: Agon related words. (@jackokring)

REQUIRE misc.4th

: VDU ( "name" --- mark) 
\G Loop over all placed C, and , values placed before the following END-VDU.
    !CSP CREATE >MARK DOES> DUP @ SWAP CELL+ DO
	I C@ EMIT LOOP ;
	
: VDUDG ( c "name" ---)
\G Makes a VDU header for the character on the stack
    DUP 32 < OVER 127 = OR IF DROP -21 THROW THEN \ Error. 
    >R VDU R> 23 C, C, ; \ Make UDG header. 
	
: END-VDU ( mark ---)
\G End a VDU definition which then has a name to use.
    >RESOLVE ?CSP ; 
    
: VWAIT ( ---)
\G Wait for system vertical blank as is done in BBC basic.
    0 SYSVARS@
    BEGIN DUP 0 SYSVARS@ = WHILE REPEAT DROP ; 
        
: 2EMIT ( u1 u2 ---)
\G Emits u2 followed by u1.
    EMIT EMIT ;

: 2CEMIT ( u ---)
\G Emit 2 characters in little endian order.
    SPLIT 2EMIT ;
    
: EMIT-XY ( x y ---)
\G Emit a 16 bit coordinate.
    SWAP 2CEMIT 2CEMIT ;

: 23EMIT ( ---)
\G Emit special code 23.
    23 EMIT ;

: 25EMIT ( ---)
\G Emit special code 25.
    25 EMIT ;

: 0EMIT ( ---)
\G Emit a NUL character.
    0 EMIT ;
    
: CURSOR ( f ---)
\G Ser cursor visibility by flag f.
    23EMIT 1 EMIT IF 1 ELSE 0 THEN EMIT ;

\ all use fg colour, add 2 to shape for bg colour.

VARIABLE USEBG
\G Set ON or OFF for drawing mode uses background. Default OFF.

VARIABLE RELCOORD
\G Set ON or OFF for drawing mode uses relative coordiantes. Default OFF.

: BGCOL? ( n --- n')
\G Applies 2+ if USEBG is ON.
    USEBG @ IF 2+ THEN RELCOORD @ IF 4 - THEN ;

: MOVETO ( x y ---)
\G Move graphics cursor to x, y.
    25EMIT $04 BGCOL? EMIT EMIT-XY ; 

: PLOT ( x y ---)
\G Plot a point at x, y.
    25EMIT $45 BGCOL? EMIT EMIT-XY ; 

: LINE ( x y ---)
\G Draw a line to x, y.
    25EMIT $05 BGCOL? EMIT EMIT-XY ;

: TRIANGLE ( x y ---)
\G Complete a triangle using x, y.
    25EMIT $55 BGCOL? EMIT EMIT-XY ;

: CIRCLE ( x y ---)
\G Circle to x, y.
    25EMIT $95 BGCOL? EMIT EMIT-XY ;
    
: BOX ( x y ---)
\G Box to x, y.
    25EMIT $65 BGCOL? EMIT EMIT-XY ;
    
: COL ( col ---)
\G Set text colour.
    17 2EMIT ;
    
: GCOL ( col ---)
\G Set graphics colour. Add 128 to col for background select.
    18 EMIT 0 2EMIT ;
    
: FLIP ( ---)
\G Flip draw buffer.
    25EMIT 0 EMIT $C3 EMIT ;
    
: MODE ( mode ---)
\G Set graphics video mode.
    22 2EMIT ;    

: CLG ( ---)
\G Clear graphics.
    16 EMIT ;
    
: PAL64 ( col pal ---)
\G Set the colour to the palette index.
    19 EMIT SWAP 2EMIT 0EMIT 0EMIT 0EMIT ;
    
: PALRGB ( col r g b ---)
\G Set a colour to an RGB value.
    19 EMIT 2>R SWAP EMIT 255 2EMIT 2R> SWAP 2EMIT ;
    
: (AUDIO) ( chan ---)
\G Sends audio preamble for channel.
    23EMIT 0EMIT $85 2EMIT ;
    
: VOL ( vol chan ---)
\G Set channel volume.
    (AUDIO) 2 2EMIT ;
    
: ADSR ( a d s r chan ---)
\G Set an envelope on a channel.
    (AUDIO) 6 EMIT 1 EMIT 2>R SWAP 2CEMIT 2CEMIT 2R> SWAP 2CEMIT 2CEMIT ;

: FREQ ( freq chan ---)
\G Set channel frequency.
    (AUDIO) 3 EMIT 2CEMIT ;
    
: FM ( len count chan ---)
\G Apply a frequency envelope to an audio channel. The len sets the overall
\G step length. The count is the number of pairs of steps and offset following
\G by perhaps using VDU and , for storing count many pairs of 16 bit cells.
    (AUDIO) 7 EMIT 1 EMIT EMIT 7 EMIT 2CEMIT ;

: WAVE ( wave chan ---)
\G Set channel waveform. Negative values are for samples.
    (AUDIO) 4 2EMIT ;    
    
: SAMPLE ( ud u --- u')
\G Emit a sample header and stack a number suitable for WAVE. The 24 bit length
\G ud is for long files. Then emit the file content of length ud and use
\G chan WAVE.
    (AUDIO) INVERT 6 EMIT 0EMIT -ROT SWAP 2CEMIT EMIT ;

: ENABLE ( f chan ---)
\G Set audio channel enabled.
    (AUDIO) IF 8 ELSE 9 THEN EMIT ;

: SILENCE ( chan ---)
\G Stop audio channel.
    (AUDIO) 10 EMIT ;

: (GFX) ( ---)
\G Emit bitmap preamble.
    23EMIT 27 EMIT ;
    
: BMP ( u ---)
\G Select bitmap number u.
    (GFX) 0EMIT EMIT ;

: BMP-DATA ( w h ---)
\G Load index colour bitmap data of width w and height h following as w*h bytes.
    (GFX) 1 EMIT EMIT-XY ;

: BMP-XY ( x y ---)
\G draw the bitmap selected by BMP at graphics point x, y.
    (GFX) 3 EMIT EMIT-XY ;
    
\ This completes the simple audio-visual interface.

: JOYX ( --- x)
\G Get the joystick x value.
    $9E P@ 0 OVER 128 AND NOT IF 1+ THEN
    SWAP 32 AND NOT IF 1- THEN ;
    
: JOYY ( --- y)
\G Get the joystick y value.
    $9E P@ 0 OVER 2 AND NOT IF 1+ THEN
    SWAP 8 AND NOT IF 1- THEN ;

: JOYF ( --- x)
\G Get the joystick fire value.
    $A2 P@ 0 OVER 128 AND NOT IF 2+ THEN
    SWAP 32 AND NOT IF 1+ THEN ;

    
