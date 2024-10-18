\ Floating point wordset for eZ80 Agon FORTH.
\ Copyright 2024, L.C. Benschop
\ Transcendental functions.
\ 2024-10-18: speed up cordic, eliminate f*, fix sin of tiny numbers.

: FMOD ( F: r1 r2 --- r3)
\G Compute r1 modulo r2
  FOVER FOVER F/ FLOOR FSWAP F* F-    
;

: FSMOD ( F: r1 r2 --- r3)
\G Compute r1 modulo r2, symmetric around zero -r2/2 <= r3 <= r2/2
  FOVER FOVER F/ FROUND FSWAP F* F-    
;


3.141592653589793e
FCONSTANT PI ( F: --- r)
\ The floating point constant PI.
PI 2e F* FCONSTANT 2PI
PI 2e F/ FCONSTANT 0.5PI

0.6931471805599453e FCONSTANT LN2

CREATE ATNTAB
\ Table containing arctan(2**-i) for i=0 to 49
0.7853981633974483e F,
0.4636476090008061e F,
0.24497866312686414e F,
0.12435499454676144e F,
0.06241880999595735e F,
0.031239833430268277e F,
0.015623728620476831e F,
0.007812341060101111e F,
0.0039062301319669718e F,
0.0019531225164788188e F,
0.0009765621895593195e F,
0.0004882812111948983e F,
0.00024414062014936177e F,
0.00012207031189367021e F,
6.103515617420877e-05 F,
3.0517578115526096e-05 F,
1.5258789061315762e-05 F,
7.62939453110197e-06 F,
3.814697265606496e-06 F,
1.907348632810187e-06 F,
9.536743164059608e-07 F,
4.7683715820308884e-07 F,
2.3841857910155797e-07 F,
1.1920928955078068e-07 F,
5.960464477539055e-08 F,
2.9802322387695303e-08 F,
1.4901161193847655e-08 F,
7.450580596923828e-09 F,
3.725290298461914e-09 F,
1.862645149230957e-09 F,
9.313225746154785e-10 F,
4.656612873077393e-10 F,
2.3283064365386963e-10 F,
1.1641532182693481e-10 F,
5.820766091346741e-11 F,
2.9103830456733704e-11 F,
1.4551915228366852e-11 F,
7.275957614183426e-12 F,
3.637978807091713e-12 F,
1.8189894035458565e-12 F,
9.094947017729282e-13 F,
4.547473508864641e-13 F,
2.2737367544323206e-13 F,
1.1368683772161603e-13 F,
5.684341886080802e-14 F,
2.842170943040401e-14 F,
1.4210854715202004e-14 F,
7.105427357601002e-15 F,
3.552713678800501e-15 F,
1.7763568394002505e-15 F,

: ATNTAB@
    FLOATS ATNTAB + F@ ;

FVARIABLE FX
FVARIABLE FY

: CORDIC ( F: r ---)
\ This is the core of the CORDIC algorithm.
\ Rotate the vector stored in variables FX, FY by the angle r.
\ R has to be set in the range -pi/2 <= r <= pi/2
\ Usually FX=1 and FY=0, but we can set FX=-1 to obtain sine, cosine
\ of the angles in the other half of the circle (r + pi).
\
\ At the end, the vector has been rotated by the angle r, but scaled by
\ 1/COSPROD, By rotating over every angle in the table exactly once
\ (either clockwise or counterclockwise), the scaling of the vector is constant.
    49 0 DO
	FDUP F0< IF
	    \ clockwise rotate.
	    I ATNTAB@ F+ \ Add angle from table
	    FX F@  FY F@ I NEGATE F-SCALE F+ \ new x
	    FY F@  FX F@ I NEGATE F-SCALE F- \ new y
	    FY F! FX F!
	ELSE
	    \ counterclockwise rotate
	    I ATNTAB@ F- \ Subtract angle from table.
	    FX F@  FY F@ I NEGATE F-SCALE F- \ new x
	    FY F@  FX F@ I NEGATE F-SCALE F+ \ new y
	    FY F! FX F!
	THEN
    LOOP FDROP
;
    
\ The product of all cosines corresponding to the angles listed in ATNTAB
\ As each of the tangents is 2**-i, each of the cosines equals.
\ 1/sqrt(1+2**(-2*i))
\ This value is slightly tweaked from the computed value to get sin(pi/2)==1.0
\ exactly in our system.
\ 0.6072529350088814e FCONSTANT COSPROD 
  0.607252935008888e FCONSTANT COSPROD 


: REDUCE-ANGLE ( F: r1 --- r2)
\ Reduce the angle to the range -pi/2 <= r2 <= pi/2.
\ Set FX and FY to their initial valies.
    2PI FSMOD             \ Reduce to range 0..2*pi
    FDUP FABS 0.5PI F> IF     \ Take care of angles in the range 0.5pi..pi
	F-SGN PI IF FNEGATE THEN F- \ Subtract pi, but set FX to -1, so CORDIC will
	-1.0E FX F!      \ rotate the vector to the desired angle.
    ELSE
	1.0E FX F!
    THEN
    0.0e FY F!
;

: FSIN ( F: r1 --- r2)
\G Sine of r1    
    REDUCE-ANGLE FDUP FABS 1.0E-5 F< IF
	FX F@ F0< IF FNEGATE THEN
    ELSE
	CORDIC FY F@ COSPROD F*
    THEN ;

: FCOS ( F: r1 --- r2)
\G Cosine of r1    
   REDUCE-ANGLE CORDIC FX F@ COSPROD F* ;

: FTAN ( F: r1 --- r2)
\G Tangent of r1    
    REDUCE-ANGLE FDUP FABS 1.0E-5 F< IF
	FX F@ F0< IF FNEGATE THEN
    ELSE
	CORDIC FY F@ FX F@ F/
    THEN ;

: INVERSE-CORDIC ( F:  --- r)
\ Rotate the angle represented by FX & FY  towards zero and return the angle
\ by which it was rotated.    
    0.0E
    49 0 DO
	FY F@ F0< IF
	    \ counterclockwise rotate
	    I ATNTAB@ F- \ Subtract angle from table.
	    FX F@  FY F@ I NEGATE F-SCALE F- \ new x
	    FY F@  FX F@ I NEGATE F-SCALE F+ \ new y
  	    FY F! FX F!
	ELSE
	    \ clockwise rotate.
	    I ATNTAB@ F+ \ Add angle from table
	    FX F@  FY F@ I NEGATE F-SCALE F+ \ new x
	    FY F@  FX F@ I NEGATE F-SCALE F- \ new y
	    FY F! FX F!
	THEN
    LOOP
;

: FATAN ( F: r1 --- r2)
\G Inverse tangent of r1, return angle in range -pi/2..pi/2
    FDUP FABS 1.0E-5 F> IF
	FY F! 1.0E FX F!
	INVERSE-CORDIC
    THEN ;

: FASIN ( F: r1 --- r2)
\G Inverse sine of r1, return angle in range -pi/2..pi/2
    FDUP FABS 1.0E F> IF
	FDROP NAN
    ELSE
	FDUP FABS 1.0E-5 F> IF	    
	    FDUP FDUP F* 1.0E FSWAP F- FSQRT  \ Compute consine sqrt(1-r**2) to FX
	    FX F!
	    FY F!                             \ Store r (sine) in FY  
	    INVERSE-CORDIC
	THEN
    THEN ;

: FACOS ( F: r1 ... r2)
\G Inverse consine of r1, return angle in range 0..pi    
    FASIN 0.5PI FSWAP F- ;
    
PI 180e F/ FCONSTANT PI/180
180e PI F/ FCONSTANT 180/PI

: FRAD ( F: r1 --- r2)
\G Convert angle from degrees to radians.
  PI/180 F* ;    

: FDEG ( F: r1 --- r2)
\G Convert angle from radians to degrees.
  180/PI F* ;    

CREATE LNTAB
\ Table containing ln(1+2**-i) for i=0 to 49
0.6931471805599453e F,
0.4054651081081644e F,
0.22314355131420976e F,
0.11778303565638346e F,
0.06062462181643484e F,
0.030771658666753687e F,
0.015504186535965254e F,
0.007782140442054949e F,
0.003898640415657323e F,
0.0019512201312617493e F,
0.0009760859730554589e F,
0.0004881620795013512e F,
0.0002441108275273627e F,
0.00012206286252567737e F,
6.103329368063853e-05 F,
3.051711247318638e-05 F,
1.5258672648362398e-05 F,
7.6293654275675724e-06 F,
3.8146899896858897e-06 F,
1.9073468138254095e-06 F,
9.536738616591883e-07 F,
4.7683704451632344e-07 F,
2.384185506798576e-07 F,
1.1920928244535446e-07 F,
5.960464299903386e-08 F,
2.9802321943606113e-08 F,
1.4901161082825355e-08 F,
7.4505805691682525e-09 F,
3.72529029152302e-09 F,
1.8626451474962336e-09 F,
9.313225741817976e-10 F,
4.6566128719931904e-10 F,
2.3283064362676457e-10 F,
1.1641532182015855e-10 F,
5.820766091177334e-11 F,
2.9103830456310187e-11 F,
1.4551915228260973e-11 F,
7.275957614156956e-12 F,
3.6379788070850955e-12 F,
1.8189894035442021e-12 F,
9.094947017725146e-13 F,
4.547473508863607e-13 F,
2.273736754432062e-13 F,
1.1368683772160957e-13 F,
5.68434188608064e-14 F,
2.8421709430403604e-14 F,
1.4210854715201903e-14 F,
7.105427357600977e-15 F,
3.5527136788004946e-15 F,
1.7763568394002489e-15 F,

: LNTAB@
    FLOATS LNTAB + F@ ;

: FEXP ( F: r1 --- r2)
\G Compute e**r1, the inverse of the natural logarithm.
    F-SGN FABS          \ sign on stack
    FDUP LN2 F/ F>UD OVER >R   \ floor(x/ln2) on return stack
    UD>F LN2 F* F- \ Modulo ln2
    0.0E FX F! \ FX contains exp(x)-1 for the partial terms of the log used.
    49 0 DO
	FDUP I LNTAB@ F< 0= IF
	    I LNTAB@ F- \ Subtract partial logarithm.
	    1.0E0 FX F@ F+ I NEGATE F-SCALE FX F@ F+ FX F!
	    \ Compute exp(x)-1 for next 2**-i factor
	    \ FX := (FX + 1) * 2**-i + FX
	THEN
    LOOP
    FDROP FX F@ 1.0E F+       
    R> F-SCALE    \ add floor(x/ln2) to exponent.
    IF 1.0E FSWAP F/ THEN
;   
    
: FLN ( F: r1 --- r2)
\G Compute the natural logarithm of r1.
    FDUP 0.0E F> 0= IF
	FDROP NAN FNEGATE
    ELSE
	FDUP 1.0E F= IF
	    FDROP 0.0E
	ELSE
	    F-EXP@ 0 F-EXP! \ Extract original exponent and force exp to 0.
	    \ Number is now between 1 and 2.
	    1.0E F- FX F! \ Subtract one store in FX, iteration gives us ln(x+1) 
	    LN2 \ Initial logaritm, at 0.
	    49 1 DO
		1.0E FX F@ F+ I NEGATE F-SCALE FX F@ F+
		\ Compute exp(x)-1 for next 2**-i factor
		FDUP 1.0E F< IF FX F! I LNTAB@ F- ELSE FDROP THEN
	    LOOP
	    S>D D>F LN2 F* F+ \ add binary exponent times ln2.
	THEN
    THEN
;

10.0E FLN FCONSTANT LN10
: FLOG ( F: r1 --- r2)
\G Base-10 logarithm.
    FLN LN10 F/ ;

: FALOG ( F: r1 --- r2)
\G 10 to the power of X (inverse base 10 logarithm)
    LN10 F* FEXP ;

: F** ( F: r1 r2 --- r3)
\G Raise r1 to the power r2. r1 must be positive
    FSWAP FLN F* FEXP ;

[DEFINED] TESTING [IF]
: SINTAB
    361 0 do
	i s>d d>f frad
	cr i 4 .r
	fdup fsin 17 13 f.r
	fdup fcos 17 13 f.r
	ftan  17 13 f.r
    15 +loop ;
[THEN]
