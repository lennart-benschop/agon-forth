\ Spacer, a 2-d space shooter program written in FORTH for AgonLight.
\ Loosely inspired by TI Parsec.
\ Copyright 2024. L.C. Benschop.
\ Released under GPLv3
\ Requires Agon Forth 0.2x, VDP 1.04 or 2.x.

REQUIRE agon.4th

\ stupid random number generator

FORGET SEED

VARIABLE SEED

: RANDOMIZE	0 SYSVARS@ 1 SYSVARS@ 8 LSHIFT + SEED ! ;

: RANDOM	\ max --- n ; return random number < max
    SEED @ 1103515245 * 12345 + 
    DUP SEED ! UM* NIP ;


127 VALUE VOLUME
\ Generate a tone.
: TONE ( chan wf vol freq dur --- )
    BEGIN
      0	4 SYSVARS!
      23 EMIT 0 EMIT $85 EMIT 4 PICK EMIT 3 PICK EMIT 2 PICK EMIT OVER 2CEMIT DUP 2CEMIT
	BEGIN 4 SYSVARS@ 8 AND UNTIL
	14 SYSVARS@ 
  UNTIL
  2DROP 2DROP DROP ;


\ Agon specifc scancodes for keys.	    
57 CONSTANT KEY-UP
41 CONSTANT KEY-DOWN
25 CONSTANT KEY-LEFT
121 CONSTANT KEY-RIGHT
98 CONSTANT KEY-SPACE
112 CONSTANT KEY-ESC
48 CONSTANT KEY-1
49 CONSTANT KEY-2
17 CONSTANT KEY-3

CREATE PLAYFIELD 1200 ALLOT

VARIABLE #SHIPS
VARIABLE SCORE
VARIABLE BONUS-SCORE
VARIABLE FUEL
VARIABLE LIFT

VARIABLE QUITTING

\ UDGs for gackground.
128 VDUDG BG0
 $FF C,  $FF C, $FF C, $FF C, $FF C, $FF C, $FF C, $FF C, 
END-VDU

129 VDUDG BG1
 $80 C, $C0 C, $E0 C, $F0 C, $F8 C, $FC C, $FE C, $FF C,
END-VDU

130 VDUDG BG2
$01 C, $03 C, $07 C, $0F C, $1F C, $3F C, $7f C, $FF C,
END-VDU

131 VDUDG BG3
$7E C, $FF C, $FF C, $FF C, $FF C, $FF C, $FF C, $FF C, 
END-VDU

132 VDUDG LASER-G
$00 C, $00 C, $00 C, $FF C, $00 C, $00 C, $00 C, $00 C, 
END-VDU

133 VDUDG MIS1
$00 C, $00 C, $7F C, $FF C, $7F C, $00 C, $00 C, $00 C,
END-VDU

134 VDUDG MIS2
$00 C, $01 C, $FF C, $FE C, $FF C, $01 C, $00 C, $00 C,
END-VDU


135 VDUDG SHIP1
$0F C, $4F C, $6F C, $FF C, $FF C, $6F C, $4F C, $0F C,
END-VDU
136 VDUDG SHIP2
$F0 C, $F8 C, $FC C, $FF C, $FF C, $FC C, $F8 C, $F0 C,
END-VDU

137 VDUDG EN1-1
$0F C, $3F C, $7F C, $E6 C, $E6 C, $7F C, $3F C, $0F C,
END-VDU
138 VDUDG EN1-2
$F0 C, $FC C, $FE C, $67 C, $67 C, $FE C, $FC C, $F0 C,
END-VDU

139 VDUDG EN2-1
$0F C, $3F C, $7F C, $E6 C, $E6 C, $7F C, $31 C, $C1 C,
END-VDU
140 VDUDG EN2-2
$F0 C, $FC C, $FE C, $67 C, $67 C, $FE C, $8C C, $83 C,
END-VDU

141 VDUDG EN3-1
$1F C, $3F C, $7F C, $FF C, $FF C, $7F C, $3F C, $1F C,
END-VDU
142 VDUDG EN3-2
$FC C, $FE C, $FF C, $FF C, $FF C, $FF C, $FE C, $FC C,
END-VDU
143 VDUDG EN3-3
$01 C, $03 C, $07 C, $0F C, $1F C, $3F C, $7f C, $FF C,
END-VDU
144 VDUDG EN3-4
$FF C, $FE C, $FC C, $F8 C, $F0 C, $E0 C, $C0 C, $80 C,
END-VDU
145 VDUDG EN3-5
$FF C, $7F C, $3F C, $1F C, $0F C, $07 C, $03 C, $01 C,
END-VDU
146 VDUDG EN3-6
$80 C, $C0 C, $E0 C, $F0 C, $F8 C, $FC C, $FE C, $FF C,
END-VDU

147 VDUDG EXPL1
$14 C, $00 C, $8A C, $00 C, $92 C, $00 C, $44 C, $00 C,
END-VDU
148 VDUDG EXPL2
$00 C, $14 C, $00 C, $8A C, $00 C, $92 C, $00 C, $44 C,
END-VDU

149 VDUDG ASTEROID1
$0F C, $3F C, $3F C, $7F C, $F7 C, $FF C, $FB C, $FF C,
END-VDU

150 VDUDG ASTEROID2
$F0 C, $FC C, $FC C, $FE C, $EF C, $FF C, $DF C, $FF C,
END-VDU

151 VDUDG ASTEROID3
$FF C, $F7 C, $FF C, $FD C, $7F C, $7F C, $3F C, $0F C,
END-VDU

152 VDUDG ASTEROID4
$FF C, $EF C, $FF C, $7F C, $FE C, $FC C, $EC C, $F0 C,
END-VDU

: ,"
\G Add a counted string to the dictionary.
    [CHAR] " WORD COUNT 1+ ALLOT DROP ;    

CREATE NORMAL-BACKGROUND
," 0003   "
," 0001   "
," 001    "
," 03     "
," 03     "
," 03     "
," 03     "
," 002    "
," 0002   "
," 00002  "
," 00003  "
," 00003  "
," 00003  "
," 00003  "
," 000002 "
," 000003 "
," 000003 "
," 000003 "
," 000003 "
," 000003 "
," 000003 "
," 000001 "
," 00001  "
," 00002  "
," 00001  "
," 00002  "
," 000002 "
," 000003 "
," 000003 "
," 000001 "
," 00001  "
0 C,

CREATE REFUEL-BACKGROUND
," 0003   "
," 0001   "
," 001    "
," 01     "
," 02     "
," 01     "
," 0      "
," 0      "
," 0      "
," 0  02  "
," 0  03  "
," 0  002 "
," 0  003 "
," 0  003 "
," 0  003 "
," 0 0003 "
," 0 0003 "
," 0 0003 "
," 0 0001 "
," 0F003  "
," 0U003  "
," 0E003  "
," 0L0002 "
," 0 0003 "
," 0 0003 "
," 0 0003 "
," 0 0003 "
," 0  003 "
," 0  003 "
," 0  003 "
," 0  003 "
," 0  003 "
," 0  003 "
," 0  003 "
," 0   01 "
," 0      "
," 0      "
," 0      "
," 0      "
," 02     "
," 0002   "
," 00003 "
," 00001  "
0 C,


VARIABLE BGPTR

VDU SCROLL-SEQ
  28 C, 0 C, 27 C, 39 C, 0 C,   \ Set text viewport
  23 C, 7 C, 0 C, 1 C, 8 C,     \ scroll text window 8 bytes to the left
  26 C,                         \ Cancel text viewport
END-VDU

VARIABLE ROW
VARIABLE SCROLL-TIMER
VARIABLE SCROLL-PERIOD

VARIABLE LASER-TEMP

VARIABLE ADD-ENEMY-TIMER
VARIABLE ADD-ENEMY-PERIOD

VARIABLE ENEMIES-ADDED
VARIABLE ENEMIES-KILLED
VARIABLE ENEMY-MODE 

VARIABLE LAST-CHAR 
: NEWCHAR ( rowaddr row --- c)
    BGPTR @ COUNT
    ROT 27 SWAP - DUP ROT >= IF
	2DROP 20 RANDOM 0= IF '.' ELSE BL THEN \ star or empty for background
	LAST-CHAR @ 149 151 WITHIN IF
	    DROP LAST-CHAR @ 2+    \ top half char of asteroid, select bottom
	ELSE
	    OVER 38 + DUP C@ 149 = IF
		2DROP 150          \ previous colum has left top of asteroid, tkae right half.  
	    ELSE
		ENEMY-MODE @ 3 = IF
		    41 + C@ 149 <> 100 RANDOM 0= AND  ROW @ 19 < AND IF DROP 149 THEN \ start a new asteroid if enemy-mode=3 last colum 1 cell below does not have asteroid and at random.
		ELSE
		    DROP
		THEN
	    THEN
	THEN
    ELSE
	+ C@ \ pick char from counted string of background.
    THEN
    NIP \ discard row address
;

: SCROLL-BACKGROUND
    0 ROW !
    0 LAST-CHAR !
    SCROLL-SEQ \ scroll 8 pixels to the left.
    15 COL
    1120 0 DO
	PLAYFIELD I + DUP DUP 1+ SWAP 39 CMOVE \ move row in playfield.
	ROW @ NEWCHAR DUP PLAYFIELD I + 39 + C! \ add new char to the right.
	DUP LAST-CHAR !
	39 ROW @ AT-XY EMIT            \ print it.
	ROW @ 20 = IF 11 COL THEN 
	1 ROW +!
    40 +LOOP
    \ select new column of background string array.
    BGPTR @ COUNT + DUP C@ 0=
    IF \ at end.
	FUEL @ 1024 < IF
	    DROP REFUEL-BACKGROUND BGPTR ! \ low on fuel, then add tunnel.
	    10 28 AT-XY 15 COL ." Time to refuel in tunnel"
	    8 SCROLL-PERIOD !
        ELSE	    
	    DROP NORMAL-BACKGROUND BGPTR ! \ otherwise normal background.
	    4 SCROLL-PERIOD !
	THEN
    ELSE
	BGPTR ! \ select next string in current array.
    THEN
;

6 CONSTANT MAX-SHIPS
12 CONSTANT BYTES-PER-SHIP
\ Offset 0 state
\          0 not active
\          1 active, wrap around X direction
\          2 active, stop when reaching edge in X direction.
\          3 active, disappear when reaching edge in X direction relaunch.
\        1 X timer
\        2 X dir
\        3 X rate
\        4 X pos
\        5 Y timer
\        6 Y dir
\        7 Y rate
\        8 Y pos
\        9 Colour
\       10 Char1
\       11 Char2

MAX-SHIPS BYTES-PER-SHIP * CONSTANT SHIP-ARRAY-SIZE
CREATE SHIP-DATA  SHIP-ARRAY-SIZE ALLOT

: SET-POS-X ( y ship --- )
    BYTES-PER-SHIP * SHIP-DATA + 4 + C! ;
: SET-POS-Y ( y ship --- )
    BYTES-PER-SHIP * SHIP-DATA + 8 + C! ;
: SET-RATE-X ( r ship--- )
    BYTES-PER-SHIP * SHIP-DATA + >R
    DUP 0= IF
	0 R@ 2 + C!
    ELSE
	DUP 0< IF
	    $FF R@ 2 + C!
	    NEGATE
	ELSE
	    1 R@ 2 + C!
	THEN
    THEN
    R> 3 + C! ;

: SET-RATE-Y ( r ship--- )
    BYTES-PER-SHIP * SHIP-DATA + >R
    DUP 0= IF
	0 R@ 6 + C!
    ELSE
	DUP 0< IF
	    $FF R@ 6 + C!
	    NEGATE
	ELSE
	    1 R@ 6 + C!
	THEN
    THEN
    R> 7 + C! ;

: ADD-SHIP ( col c1 c2 state ship# --- )
    BYTES-PER-SHIP * SHIP-DATA + >R
    R@ C! R@ 11 + C! R@ 10 + C! R> 9 + C!
;

: TYPE-BACKGROUND
    OVER 20 > IF 11 ELSE 15 THEN COL
    >R 2DUP AT-XY
    40 * + PLAYFIELD + R> TYPE
;    

: HIDE-SHIPS
\G Hide any moving ships and missiles
    SHIP-DATA SHIP-ARRAY-SIZE  BOUNDS DO
	I C@ IF
	    I 4 + C@ I 8 + C@ 2 TYPE-BACKGROUND
	THEN
    BYTES-PER-SHIP +LOOP    
;


: BOUNCE-BIG-SHIP
\ When in enemy mode 2 our enemy ship cosists of 3 smaller 2-character
\ ships that move in sync. THey move vertically. WHen they reach top or
\ bottom, reverse direction.    
    SHIP-DATA BYTES-PER-SHIP + 6 + C@ $FF =
    SHIP-DATA BYTES-PER-SHIP + 8 + C@ 1 = AND IF
	1 SHIP-DATA BYTES-PER-SHIP + 6 + C!
	1 SHIP-DATA BYTES-PER-SHIP 2* + 6 + C!
	1 SHIP-DATA BYTES-PER-SHIP 3 * + 6 + C!
    THEN
    SHIP-DATA BYTES-PER-SHIP + 6 + C@ 1 =
    SHIP-DATA BYTES-PER-SHIP + 8 + C@ 20 >= AND IF
	$FF SHIP-DATA BYTES-PER-SHIP + 6 + C!
	$FF SHIP-DATA BYTES-PER-SHIP 2* + 6 + C!
	$FF SHIP-DATA BYTES-PER-SHIP 3 * + 6 + C!
    THEN
;

: MOVE-BY-RATE ( addr --- pos)
\ Alter the position of a ship as indicated by its rate.
\ addr offs 0 timer (counter incremented by rate, if exceeds 16, do the move)
\           1 dir 00 no move, 1 increment $FF deccrement    
\           2 rate, to be added to timer each cycle
\                   (if rate==16 move each cycle).
\           3 pos is position in character cells. 
    >R       \ Save address
    R@ 2+ C@ R@ C@  + DUP $0F AND R@ C! \ Increment timer by rate
    4 RSHIFT \ How much to move by (16 translates to 1, anything less to 0).
    R@ 1+ C@ $FF = IF NEGATE THEN \ Decrement depending on direction.
    R@ 3 + C@ + DUP  \ Increment position.
    0 MAX 38 MIN R> 3 + C! \ Clip position and store back (return unclipped position).
;
    
: MOVE-SHIPS
    ENEMY-MODE @ 2 = IF BOUNCE-BIG-SHIP THEN
    SHIP-DATA SHIP-ARRAY-SIZE BOUNDS DO
	I C@ IF \ Is this ship active?
	    \ Adjust X position.
	    I 1+ MOVE-BY-RATE
	    DUP 0< IF
		DROP 
		\ Wrap around X of to the left.
		I C@ 1 = IF
		    38 I 4 + C!
		ELSE
		    I C@ 3 = IF 
			SHIP-DATA BYTES-PER-SHIP + 4 + C@ 2-
			I 4 + C! \ Appear to relaunch from main ship
			SHIP-DATA BYTES-PER-SHIP + 8 + C@ I 8 + C!
			0 0 VOLUME 500 20 TONE
		    THEN
		THEN		
	    ELSE
		38 > IF
		    \ Wrap around X of to the right.
		    I C@ 1 = IF
			0 I 4 + C!
		    THEN
		THEN
	    THEN
	    I 5 + MOVE-BY-RATE DROP
	    I C@ IF
		I 4 + C@ I 8 + C@ AT-XY 17 EMIT
		I 9 + 3 TYPE
	    THEN		
	THEN
    BYTES-PER-SHIP +LOOP    
;

: SHOW-SCORE
    SCORE @ BONUS-SCORE @ >= IF
	2000 BONUS-SCORE +!
	1 #SHIPS +!
    THEN
    0 29 AT-XY 15 COL ." SHIPS " #SHIPS @ 3 .R SPACE
    ." FUEL " 9 COL FUEL @ 10 RSHIFT 1+ 0 DO '+' EMIT LOOP 10 SPACES
    29 29 AT-XY 15 COL ." SCORE" SCORE @ 5 .R
;


: SHIP-X SHIP-DATA 4 + C@ ;
: SHIP-Y SHIP-DATA 8 + C@ ;

: LASER-ON
    IF
	0 0 VOLUME 1000 20 TONE
	SHIP-X 2+ SHIP-Y AT-XY 15 COL
	40 SHIP-X 2+ DO 132 EMIT LOOP \ show laser beam.
	VWAIT
	2 LASER-TEMP +!
	ENEMY-MODE @ 2 = IF
	    SHIP-DATA BYTES-PER-SHIP +
	    DUP C@ 1 = OVER 8 + C@ SHIP-Y = AND SWAP 4 + C@ SHIP-X > AND
	    IF
		\ Laser has hit main enemy ship.
		1 ENEMIES-KILLED +!
		HIDE-SHIPS
		75 SCORE +! SHOW-SCORE
		SHIP-DATA BYTES-PER-SHIP + BYTES-PER-SHIP 4 * ERASE
	    THEN
	ELSE
	    SHIP-DATA SHIP-ARRAY-SIZE  BOUNDS BYTES-PER-SHIP + ?DO
		I C@ 1 = IF
		    \ Laser has hit one or more enemy ships
		    I 8 + C@ SHIP-Y = I 4 + C@ SHIP-X > AND IF
			ENEMY-MODE @ 10 * 50 + SCORE +! SHOW-SCORE
			0 I C!
			1 ENEMIES-KILLED +!
		    THEN
		THEN
	    BYTES-PER-SHIP +LOOP
	THEN
	SHIP-X 2+ SHIP-Y
	40 SHIP-X 2+ - TYPE-BACKGROUND \ restore background, remove beam.
    THEN
;

: SHOW-EXPL
\ Show exploding ship    
    SHIP-X SHIP-Y AT-XY 9 COL 147 EMIT 148 EMIT
    4 2 WAVE 0 127 DO I 2 VOL 16 MS -1 +LOOP
;    
   

: COLLISION-DETECT ( --- f)
    PLAYFIELD SHIP-Y 40 * + SHIP-X + @ DUP $455546 =
    IF
	DROP 10000 FUEL !
	SHOW-SCORE
	10 28 AT-XY 20 SPACES
	FALSE EXIT
    THEN	
    $8080 AND IF
	TRUE
	10 28 AT-XY 15 COL ." Crashed into ground"
	SHOW-EXPL
	EXIT
    THEN 
    LASER-TEMP @ 20 > IF
	TRUE
	10 28 AT-XY 15 COL ." Laser overheated" 
	SHOW-EXPL
	EXIT
    THEN
    SHIP-DATA SHIP-ARRAY-SIZE  BOUNDS BYTES-PER-SHIP + DO
	I C@ IF
	    I 8 + C@ SHIP-Y = I 4 + C@ SHIP-X - ABS 2 <  AND IF
		10 28 AT-XY 15 COL
		I C@ 1 = IF
		    ." Collision with enemy"
		ELSE
		    ." Hit by missile"
		THEN
		SHOW-EXPL
		UNLOOP TRUE
		EXIT
	    THEN
	THEN
    BYTES-PER-SHIP +LOOP
    FALSE
;

: SHOW-LIFT
    0 28 AT-XY 15 COL  ." LIFT "
    LIFT @ 16 = IF
	3
    ELSE
	LIFT @ 8 = IF
	    2   
	ELSE
	    1
	THEN
    THEN
    .
;

: INITIALIZE-LEVEL
    PAGE
    PLAYFIELD 1200 BLANK
    SHIP-DATA SHIP-ARRAY-SIZE ERASE
    10 135 136 2 0 ADD-SHIP
    16 0 SET-POS-X 
    12 0 SET-POS-Y 
    SHOW-SCORE
    8 LIFT !
    SHOW-LIFT 
    0 ENEMIES-ADDED !
    0 ENEMIES-KILLED !
    0 LASER-TEMP !
    QUITTING OFF
    NORMAL-BACKGROUND BGPTR !
    40 0 DO SCROLL-BACKGROUND LOOP
;

: ADD-ENEMY
    ENEMIES-ADDED @ MAX-SHIPS 1- <
    IF
	ENEMY-MODE @ 0= IF
	    0 0 VOLUME 400 100 TONE
	    0 0 VOLUME 200 100 TONE
	    5 137 138 1 ENEMIES-ADDED @ 1+ ADD-SHIP
	    1 ENEMIES-ADDED +!
	    20 RANDOM ENEMIES-ADDED @ SET-POS-Y
	    38 ENEMIES-ADDED @ SET-POS-X
	    -8 ENEMIES-ADDED @ SET-RATE-X
	    0 ENEMIES-ADDED @ SET-RATE-Y
	ELSE
	    ENEMY-MODE @ 1 = IF
		0 0 VOLUME 200 100 TONE
		0 0 VOLUME 100 100 TONE
		6 139 140 1 ENEMIES-ADDED @ 1+ ADD-SHIP
		1 ENEMIES-ADDED +!
		20 RANDOM ENEMIES-ADDED @ SET-POS-Y
		0 ENEMIES-ADDED @ SET-POS-X
		8 ENEMIES-ADDED @ SET-RATE-X
		0 ENEMIES-ADDED @ SET-RATE-Y
	    ELSE
		ENEMY-MODE @ 2 = IF
		    \ ENEMY-MODE = 2
		    ENEMIES-ADDED @ ENEMIES-KILLED @ = IF
			0 0 VOLUME 600 100 TONE
			0 0 VOLUME 200 100 TONE
			15 141 142 1 1 ADD-SHIP
			1 ENEMIES-ADDED +!
			18 RANDOM 1+ >R
			R@ 1 SET-POS-Y
			37 1 SET-POS-X
			0 1 SET-RATE-X
			4 1 SET-RATE-Y
			15 143 144 1 2 ADD-SHIP
			R@ 1- 2 SET-POS-Y
			38 2 SET-POS-X
			0 2 SET-RATE-X
			4 2 SET-RATE-Y
			15 145 146 1 3 ADD-SHIP
			R@ 1+ 3 SET-POS-Y
			38 3 SET-POS-X
			0 3 SET-RATE-X
			4 3 SET-RATE-Y \ Ships 1, 2 and 3 form the big white ship
			\ All are making the same movement.
			15 133 134 3 4 ADD-SHIP
			R> 4 SET-POS-Y
			35 4 SET-POS-X
			-16 4 SET-RATE-X
			\ 'ship 4' is our missile.
		    THEN
		ELSE
		    1 ENEMIES-ADDED +!
		    ENEMIES-ADDED @ ENEMIES-KILLED !
		    \ Don't have enemy ships in this mode, just asteroids.
		THEN
	    THEN
	THEN
    ELSE	
	ENEMIES-ADDED @ ENEMIES-KILLED @ =
	IF
	    0 ENEMIES-ADDED !
	    0 ENEMIES-KILLED !
	    1 ENEMY-MODE +!
	    ENEMY-MODE @ 3 > IF 0 ENEMY-MODE ! THEN
	THEN
    THEN
;

: INITIALIZE-GAME
    NORMAL-BACKGROUND REFUEL-BACKGROUND
    2 0 DO
	BGPTR !
	BEGIN
	    BGPTR @ C@
	WHILE
		BGPTR @ COUNT 2DUP + BGPTR !
		BOUNDS DO I C@ 48 58 WITHIN IF I C@ 48 - 128 + I C! THEN LOOP
		\ Change characters in background array to UDG range.
	REPEAT
    LOOP
    0 0 WAVE
    0 ENEMY-MODE !
    3 #SHIPS !
    0 SCORE !
    2000 BONUS-SCORE !
    10000 FUEL !
    4 SCROLL-PERIOD !
    100 ADD-ENEMY-PERIOD !
;    

: MAINLOOP
    \G Main loop of teh game.
    BEGIN
	VWAIT
	0 0 SET-RATE-X
	0 0 SET-RATE-Y
	KEY-1 KEYCODE? IF 4 LIFT ! SHOW-LIFT THEN
	KEY-2 KEYCODE? IF 8 LIFT ! SHOW-LIFT THEN
	KEY-3 KEYCODE? IF 16 LIFT ! SHOW-LIFT THEN
	KEY-UP KEYCODE?   IF LIFT @ NEGATE  0 SET-RATE-Y THEN
	KEY-DOWN KEYCODE? IF LIFT @  0 SET-RATE-Y THEN
	KEY-LEFT KEYCODE? IF -8 0 SET-RATE-X THEN
	KEY-RIGHT KEYCODE? IF 8 0 SET-RATE-X THEN
	KEY-SPACE KEYCODE? LASER-ON
	KEY-ESC KEYCODE? IF QUITTING ON 0 #SHIPS ! THEN
	LASER-TEMP @ IF
	    -1 LASER-TEMP +!
	THEN
	HIDE-SHIPS
	1 SCROLL-TIMER +!
	SCROLL-TIMER @ SCROLL-PERIOD @ >= IF
	    SCROLL-BACKGROUND
	    0 SCROLL-TIMER !
	THEN
	1 ADD-ENEMY-TIMER +!
	ADD-ENEMY-TIMER @ ADD-ENEMY-PERIOD @ >= IF
	    ADD-ENEMY
	    0 ADD-ENEMY-TIMER !
	THEN
	FUEL @
	IF
	    -1 FUEL +!
	ELSE
	    2 0 SET-RATE-Y \ Force ship to hit the ground if no more fuel.
	THEN
	FUEL @ $3FF AND $3FF = IF SHOW-SCORE THEN
	MOVE-SHIPS
	COLLISION-DETECT IF QUITTING ON -1 #SHIPS +! 10000 FUEL ! THEN
	QUITTING @
    UNTIL ;

: INSTRUCTIONS
    \ Define all UDGs
    BG0 BG1 BG2 BG3 LASER-G MIS1 MIS2 SHIP1 SHIP2
    EN1-1 EN1-2 EN2-1 EN2-2 EN3-1 EN3-2 EN3-3 EN3-4 EN3-5 EN3-6
    EXPL1 EXPL2
    ASTEROID1 ASTEROID2 ASTEROID3 ASTEROID4
    11 COL 14 0 AT-XY ." S-P-A-C-E-R" 15 COL
    CR
    CR ." v0.1 Copyright 2024 L.C. Benschop"
    CR
    CR ." Cursor keys to control your ship " 10 COL 135 EMIT 136 EMIT
    15 COL
    CR ." Keys 1, 2, 3 set rate of ascent/descent"
    CR
    CR ." SPACE fires your laser."
    CR ." Be careful, do not overheat!"
    CR ." Destroy all enemy ships with your laser"
    CR
    CR ." Descend into tunnel when low on fuel"
    CR
    CR ." Asteroids " 149 EMIT 150 EMIT SPACE SPACE ." you cannot destroy"
    CR ."           " 151 EMIT 152 EMIT
    CR ." Navigate around them."
    CR
    CR
    CR 5 COL 137 EMIT 138 EMIT 15 COL 10 SPACES ." 50 points"
    CR
    CR 6 COL 139 EMIT 140 EMIT 15 COL 10 SPACES ." 60 points"
    CR
    CR SPACE 143 EMIT 144 EMIT 
    CR  141 EMIT 142 EMIT 10 SPACES ." 75 points"
    CR SPACE 145 EMIT 146 EMIT
    0 29 AT-XY ." Press any key to start game " KEY DROP
;

: SPACER
    8 MODE 0 CURSOR
    RANDOMIZE
    INSTRUCTIONS
    BEGIN 
	INITIALIZE-GAME
	BEGIN
	    INITIALIZE-LEVEL
	    MAINLOOP
	    #SHIPS @ 0=
	UNTIL
	3 12 AT-XY 15 COL ." Game Over !!! Play again (Y/N)?" 0 5 SYSVARS! 1000 MS
	KEY DUP $4E = SWAP $6E = OR
    UNTIL
    0 MODE 1 CURSOR
;

: RUN-SPACER
    500 MS
    0 5 SYSVARS! \ Clear key code.    
    SPACER BYE ;

CR .( Type SPACER to start the game)
CR .( Type ' RUN-SPACER TURNKEY spacer.bin BYE to save as binary)
CR  	    


