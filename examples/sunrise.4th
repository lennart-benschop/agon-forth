\ Sunrise and sunset calendar.
\ Sunrise expressions taken from https://en.wikipedia.org/wiki/Sunrise_equation

\ Usage example
\ CAMBERRA ( select location)
\ 12 2024 MONTH-CALENDAR (show calendar on screen)
\
\ TO-FILE foo.txt ( output to file foo.txt)
\ 2024 YEAR-CALENDAR
\ END-FILE
\

VARIABLE OUTHANDLE

: OUT-LINE ( c-addr u --- )
    OUTHANDLE @ 0= IF
	TYPE CR
    ELSE
	OUTHANDLE @ WRiTE-LINE DROP
    THEN ;

: TO-FILE ( "ccc" --- )
    BL WORD COUNT 2DUP DELETE-FILE DROP
    W/O CREATE-FILE DROP OUTHANDLE !
;

: END-FILE ( ---)
    OUTHANDLE @ CLOSE-FILE DROP 0 OUTHANDLE ! ;

CREATE OUTBUFFER 80 CHARS ALLOT
VARIABLE OUTLEN
: TYPE-OUT ( addr n --- )
    DUP >R OUTBUFFER OUTLEN @ + SWAP CMOVE
    R> OUTLEN +!
;
: CR-OUT
    OUTBUFFER OUTLEN @ OUT-LINE
    0 OUTLEN ! ;
: SPACES-OUT ( n ---)
    OUTBUFFER OUTLEN @ + OVER BLANK
    OUTLEN +!
;
    

: >JULIAN2000 ( d m y --- julianday)
\ Compute Julian day where day 1 is Jan 1, 2000.
\ Only takes into account years 1901--2099, not using century leapyear rules.
\ Requires int size at least 24 bit.
\ return 367*year-7*(year+(month+9)//12)//4+275*month//9+day-730530
    DUP 367 *
    2 PICK 9 + 12 / ROT + 7 * 4 / -
    SWAP 275 * 9 / +
    + 730530 - ;

CREATE MONTH-LENGTHS
 31 C, 28 C, 31 C, 30 C, 31 C, 30 C, 31 C, 31 C, 30 C, 31 C, 30 C, 31 C,

: ,"
    [CHAR] " WORD C@ 1+ ALLOT ;

CREATE MONTH-NAMES
," January" ," February" ," March" ," April" ," May" ," June"
," July" ," August"  ," September" ," October" ," November" ," December"

: >MONTH-NAME ( n --- c-addr len )
  MONTH-NAMES SWAP  
    1- 0 ?DO
	COUNT +
    LOOP COUNT
;

CREATE DAY-NAMES
," Sun" ," Mon" ," Tue" ," Wed" ," Thu" ," Fri" ," Sat"

: >DAY-NAME ( n --- c-addr len)
    DAY-NAMES SWAP
    0 ?DO COUNT + LOOP COUNT ;


: MONTH-LENGTH ( m y --- n)
    OVER MONTH-LENGTHS + 1- C@
    SWAP 3 AND 0= ROT 2 = AND IF
	1+
    THEN
;

[DEFINED] FRAD 0= [IF]
PI 180e F/ FCONSTANT PI/180
180e PI F/ FCONSTANT 180/PI

: FRAD ( F: r1 --- r2)
\ Convert angle from degrees to radians.
  PI/180 F* ;    

: FDEG ( F: r1 --- r2)
\ Convert angle from radians to degrees.
  180/PI F* ;    
[THEN]    

: LASTSUNPREV ( m y --- jd )
\ Return the julian2000 date of the last sunday of the previous month
  1 ROT ROT >JULIAN2000 2 - 7 /MOD SWAP 0= IF 1- THEN  7 * 2 + ;    

VARIABLE ST-START
VARIABLE ST-END
: SUMMERTIME-RULE ( month-start #sun-start month-end #sun-end ---)
\ Runtime: ( year --- )    
  CREATE C, C, C, C,
  DOES>  >R
    R@ C@ 0= IF
	R> 2DROP
	0 ST-START !
	0 ST-END ! \ No summertime at all.
    ELSE
	R@ 2 + C@ OVER LASTSUNPREV R@ 3 + C@ 7 * + ST-START !
	R@     C@ SWAP LASTSUNPREV R> 1 + C@ 7 * + ST-END !	
    THEN
;

0 0 0 0  SUMMERTIME-RULE NO-DST
0 4 0 11 SUMMERTIME-RULE EU  \ European summertime (last sunday represented by
                             \ #0 of next month)
2 3 1 11 SUMMERTIME-RULE USA \ US Daylight saving time
1 10 1 4 SUMMERTIME-RULE AUS \ Australian summertime.

   

: HOURS 60 * ;

: LAT-N ( deg min sec --- F: angle-f )
     SWAP 60 * + SWAP 3600 * + 0 D>F 3600E0 F/ ; 
: LAT-S ( deg min sec --- F: angle-f )
    LAT-N FNEGATE ; 
: LONG-E ( deg min sec --- F: angle-f )
    LAT-N ;
: LONG-W ( deg min sec --- F: angle-f )
    LAT-S ;

FVARIABLE LATITUDE
FVARIABLE LONGITUDE
VARIABLE TIMEZONE
VARIABLE DST-RULE
VARIABLE LOCATION-NAME

: LOCATION ( tz-min xt-dst --- F: lat long ---)
\ Runtime: ( --- )   Set variables according to location.  
    >IN @ CREATE >IN ! \ Save name in input string
    , , F, F,
    BL WORD COUNT 1+ ALLOT DROP \ store location name
  DOES> DUP @ DST-RULE !
    CELL+ DUP @ TIMEZONE !
    CELL+ DUP F@ LONGITUDE F!
    FLOAT+ DUP F@ LATITUDE F!
    FLOAT+ LOCATION-NAME !
;
    
\ Lat           long            Timezone  DST 
 51 39 0 LAT-N  5 18 0 LONG-E   1 HOURS ' EU     LOCATION VUGHT
 38.889E             -77.000E  -5 HOURS ' USA    LOCATION WASHINGTON-DC
-35.304E             149.095E  10 HOURS ' AUS    LOCATION CAMBERRA
 23 30 0 LAT-S 46 37 0 LONG-W  -3 HOURS ' NO-DST LOCATION SAO-PAULO

\ Select one location by default.
VUGHT


VARIABLE IS-ST
: IS-ST? ( jd --- f)
\ Check if this date has summertime.
    ST-START @ 0= IF
	DROP 0
    ELSE
	ST-START @ ST-END @ < IF
	    DUP ST-START @ >= SWAP ST-END @ < AND
	ELSE
	    DUP ST-START @ >= SWAP ST-END @ < OR
	THEN
    THEN
    IS-ST !
;

: JUL2000>TIME ( --- t | f: jt --- )
    0.5E F+
    TIMEZONE @ S>D D>F 1440E F/ F+ IS-ST @ IF 1E 24E F/ F+ THEN
    FDUP FLOOR F- 1440E F* 0.5E F+ F>D DROP 
;

: TIME-OUT ( t --- )
    DUP 0< IF
	DROP S" -----" TYPE-OUT
    ELSE
	0 <# # 6 BASE ! # 10 BASE ! [CHAR] : HOLD # # #> TYPE-OUT
    THEN
;


: JUL>WD ( julan-day --- )
   5 + 7 MOD >DAY-NAME TYPE-OUT 1 SPACES-OUT ;

: NUM-OUT ( n width -- )
  >R 0 <# #S #> R> OVER - SPACES-OUT TYPE-OUT 1 SPACES-OUT ;

FVARIABLE MEAN-SOLAR-TIME
FVARIABLE MEAN-ANOMALY
FVARIABLE CENTER-EQ
FVARIABLE ECLIPTIC-LONG
FVARIABLE SIN-DECL
FVARIABLE COS-DECL
FVARIABLE HOUR-ANGLE
FVARIABLE J-TRANSIT
: CALC-SUNRISE-SUNSET
    DUP IS-ST?
    S>D D>F LONGITUDE F@ 360E0 F/ F- 0.0009E F+ MEAN-SOLAR-TIME F!
    357.5291E MEAN-SOLAR-TIME F@ 0.98560028E F* F+ FRAD  MEAN-ANOMALY F!
    MEAN-ANOMALY F@ FSIN 1.9148E F*
    MEAN-ANOMALY F@ 2.0E F* FSIN 0.02E F* F+
    MEAN-ANOMALY F@ 3.0E F* FSIN 0.0003E F* F+ FRAD CENTER-EQ F!
    MEAN-ANOMALY F@ CENTER-EQ F@ F+ 282.9273E FRAD F+ ECLIPTIC-LONG F!
    MEAN-SOLAR-TIME F@ MEAN-ANOMALY F@ FSIN 0.0053E F* F+
    ECLIPTIC-LONG F@ 2.0E F* FSIN 0.0069E F* F- J-TRANSIT F!
    23.4397E FRAD FSIN ECLIPTIC-LONG F@ FSIN F* SIN-DECL F!
    SIN-DECL F@ FDUP F* 1.0E FSWAP F- FSQRT COS-DECL F!
    -0.833E FRAD FSIN LATITUDE F@ FRAD FSIN SIN-DECL F@ F* F-
    LATITUDE F@ FRAD FCOS COS-DECL F@ F* F/
    FDUP FABS 1.0E F> IF
	FDROP -1 -1
    ELSE
	FACOS FDEG 360.0E F/ HOUR-ANGLE F!
	J-TRANSIT F@ HOUR-ANGLE F@ F- JUL2000>TIME \ sunrise time
	J-TRANSIT F@ HOUR-ANGLE F@ F+ JUL2000>TIME \ sunset time
    THEN
;

: SHOW-SUNRISE
    CALC-SUNRISE-SUNSET
    SWAP TIME-OUT
    S" -" TYPE-OUT
    TIME-OUT
    IS-ST @ IF S"  s" ELSE S"   " THEN TYPE-OUT
;

: FNUM-OUT ( F: f ---) 
    FDUP F0< >R FABS 1000E0 F* 0.5E F+ F>D
    <# # # # [CHAR] . HOLD #S R> SIGN #> TYPE-OUT 1 SPACES-OUT
;
: TZ-OUT ( t ---)
    DUP 0< >R ABS 0 <# # 6 BASE ! # 10 BASE ! [CHAR] : HOLD #S
    R> IF [CHAR] - ELSE [CHAR] + THEN HOLD #> TYPE-OUT 1 SPACES-OUT
;
    
: MONTH-CALENDAR ( month year ---)
    DUP DST-RULE @ EXECUTE \ Set summertime start and end dates.
    CR-OUT
    OVER >MONTH-NAME TYPE-OUT DUP 5 NUM-OUT
    LOCATION-NAME @ COUNT TYPE-OUT CR-OUT
    S" Lat " TYPE-OUT LATITUDE F@ FNUM-OUT
    S" Long " TYPE-OUT LONGITUDE F@ FNUM-OUT
    S" UTC" TYPE-OUT TIMEZONE @ TZ-OUT
    CR-OUT
    2DUP 1 ROT ROT >JULIAN2000
    ROT ROT MONTH-LENGTH 
    16 0 DO
	OVER I + DUP JUL>WD I 1+ 2 NUM-OUT SHOW-SUNRISE
	4 SPACES-OUT
	I 16 + OVER < IF
	    OVER I + 16 + DUP JUL>WD I 17 + 2 NUM-OUT SHOW-SUNRISE  THEN
	CR-OUT
    LOOP
    2DROP 
;
    
: YEAR-CALENDAR ( year --- )
    13 1 DO
	I OVER MONTH-CALENDAR
    LOOP DROP
;

1 CELLS 3 = [IF] \ Are we Agon 24-bit FORTH
include /examples_agon/graphics.4th
VARIABLE MONTH-X    
: YEAR-GRAPH ( year --- )
    22 EMIT 0 EMIT
    DUP DST-RULE @ EXECUTE \ Set summertime start and end dates.
    365 OVER 3 AND 0= IF 1+ THEN >R
    ." Sunrise and Sunset " LOCATION-NAME @ COUNT TYPE SPACE DUP .
    7 GC
    5 EMIT
    25 0 DO
	100 900 I 30 * - DUP >R MOVETO 1200 R@ LINETO 50 R> 5 + MOVETO I .
    LOOP
    100 MONTH-X !
    13 0 DO
	MONTH-X @ DUP >R 900 MOVETO R> 180 LINETO
	I 12 < IF 
	    MONTH-X @ 920 MOVETO I 1+ >MONTH-NAME DROP 3 TYPE 
	    I 1+ OVER MONTH-LENGTH 3 * MONTH-X +!
	THEN
    LOOP
    1 1 ROT >JULIAN2000
    R> 0 DO
	DUP I +  CALC-SUNRISE-SUNSET
	14 GC
	100 I 3 * + 900 ROT 2/ - 69 PLOT
	10 GC
	100 I 3 * + 900 ROT 2/ - 69 PLOT
    LOOP	
    4 EMIT KEY DROP
;


[THEN]

