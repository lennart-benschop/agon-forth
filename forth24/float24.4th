\ Floating point wordset for eZ80 Agon FORTH.
\ Copyright 2024, L.C. Benschop

\ Note: VARIABLEs FP and F0 already defined in kernel.

\ Floating point format:
\ byte 0..1 little endian: bit 15 is sign (0 = positive, 1 is negative).
\           bits 14..0 offset exponent. 16384 is means range 1.0<=x<2.0
\ bytes 2..7 48-bit significand little endian order, scaled integer,
\            MSB usually set.

: FALIGN ( --- )
\G align the dictionary pointer to align to a float addres, no-op.    
;

: FALIGNED ( addr1 -- addr2)
\G increment address to next float-aligned address, no-op.    
;

: FLOAT+ ( addr1 --- addr2)
\G Point the address to the next float     
    8 + ;

CODE FLOATS ( n --- n2)
\G Compute the number of bytes occupied by n floats.
    EX DE, HL
    ADD HL, HL
    ADD HL, HL
    ADD HL, HL
    EX DE, HL
    NEXT
END-CODE

64 CONSTANT F-STACK-DEPTH
CREATE F-STACK F-STACK-DEPTH 4 + FLOATS ALLOT
\ The floating point stack grows from low to high address, this is the
\ opposite direction compared to the data and returns stacks.
\ The stack pointer is kept in the FP variable. When working with the
\ FP stack, the IY register is typically used.
\ The stack pointer points to the first free byte in the FP stack.
\ Note allocate 4 extra numbers: 3 to allow for stack underflow, one
\ to use as scratch space beyond TOS.
F-STACK 3 FLOATS + FP !
FP @ F0 !


CODE FDROP ( F: r ---- )
\G Remove top from the FP stack    
    LD HL, FP ()
    LD BC, -8
    ADD HL, BC
    LD FP (), HL
    NEXT
END-CODE

CODE FDUP ( F: r --- r r)
\G Duplicate top item on the FP stack.    
    PUSH IY
    LD IY, FP ()
    LD HL, -8 (IY+)
    LD 0 (IY+), HL
    LD HL, -5 (IY+)
    LD 3 (IY+), HL
    LD HL, -2 (IY+)
    LD 6 (IY+), HL      \ Allow to transfer one too many bytes.
    LEA IY, 8 IY+
    LD FP (), IY
    POP IY
    NEXT
END-CODE

CODE FOVER ( F: r1 r2 --- r1 r2 r1 )
\G Duplicate second item on the FP stack.    
    PUSH IY
    LD IY, FP ()
    LD HL, -16 (IY+)
    LD 0 (IY+), HL
    LD HL, -13 (IY+)
    LD 3 (IY+), HL
    LD HL, -10 (IY+)
    LD 6 (IY+), HL      \ Allow to transfer one too many bytes.
    LEA IY, 8 IY+
    LD FP (), IY
    POP IY
    NEXT
END-CODE

CODE FSWAP ( F: r1 r2 --- r2 r1 )
\G Swap top two items on the FP stack
    PUSH IY
    LD IY, FP ()
    LD HL, -16 (IY+)
    LD BC, -8 (IY+)
    LD -16 (IY+), BC
    LD -8 (IY+), HL
    LD HL, -13 (IY+)
    LD BC, -5 (IY+)
    LD -13 (IY+), BC
    LD -5 (IY+), HL
    LD HL, -10 (IY+)
    LD BC, -2 (IY+)
    LD -10 (IY+), C
    LD -9 (IY+), B
    LD -2 (IY+), HL
    POP IY
    NEXT
END-CODE

CODE FROT ( F: r1 r2 r3 --- r2 r3 r1)
\G Rotate top three items on the FP stack
    PUSH IY
    PUSH DE
    LD IY, FP ()
    LD HL, -24 (IY+)
    LD DE, -16 (IY+)
    LD BC, -8 (IY+)
    LD -24 (IY+), DE
    LD -16 (IY+), BC
    LD -8 (IY+), HL
    LD HL, -21 (IY+)
    LD DE, -13 (IY+)
    LD BC, -5 (IY+)
    LD -21 (IY+), DE
    LD -13 (IY+), BC
    LD -5 (IY+), HL
    LD HL, -18 (IY+)
    LD DE, -10 (IY+)
    LD BC, -2 (IY+)
    LD -18 (IY+), E
    LD -17 (IY+), D
    LD -10 (IY+), C
    LD -9 (IY+), B
    LD -2 (IY+), HL
    POP DE
    POP IY
    NEXT
END-CODE


: FDEPTH ( --- n)
\G Return the depth of the FP stack    
    FP @ F0 @ - 3 RSHIFT ;

CODE F@ ( addr --- , F: --- r)
\G Read a floating point number from memory and add it to the FP stack.
    PUSH IY
    LD IY, FP ()
    EX DE, HL
    LD DE, (HL)
    INC HL
    INC HL
    INC HL
    LD 0 (IY+), DE
    LD DE, (HL)
    INC HL
    INC HL
    INC HL
    LD 3 (IY+), DE
    LD DE, (HL)
    INC HL
    INC HL
    INC HL
    LD 6 (IY+), DE    
    LEA IY, 8 IY+
    LD FP (), IY
    POP IY
    POP DE
    NEXT
END-CODE

CODE F! ( addr ----. F: r ---)
\G Write a floating point number to memory, which was taken from the FP stack.
    PUSH IY
    LD IY, FP ()
    EX DE, HL
    LD DE, -8 (IY+)
    LD (HL), DE
    INC HL
    INC HL
    INC HL
    LD DE, -5 (IY+)
    LD (HL), DE
    INC HL
    INC HL
    INC HL
    LD DE, -2 (IY+)
    LD (HL), E
    INC HL
    LD (HL), D
    LEA IY, -8 IY+
    LD FP (), IY
    POP IY
    POP DE
    NEXT
END-CODE    

CODE FLIT ( F: --- r)
\G Runtime routine for FP literals.    
    LD HL, FP ()
    LD BC, 0 (IY+)
    LD (HL), BC
    INC HL
    INC HL
    INC HL
    LD BC, 3 (IY+)
    LD (HL), BC
    INC HL
    INC HL
    INC HL
    LD BC, 6 (IY+)
    LD (HL), BC
    INC HL
    INC HL
    LD FP (), HL
    LEA IY, 8 IY+
    NEXT
END-CODE    

: F, ( F: r --- )
\G Add the floating point number r to the dictionary.    
    HERE F! 8 ALLOT ;

: FVARIABLE ( --- )
    CREATE 8 ALLOT ;

: FCONSTANT ( F: r --- )
    CREATE F, DOES> F@ ;

: FLITERAL ( F: r --- )
    POSTPONE FLIT F, ; IMMEDIATE

CODE F0< ( --- f  F: r -- -)
\G Test if floating point number is less than 0    
    PUSH DE
    PUSH IY
    LD IY, FP ()
    LD DE, 0
    BIT 7 -7 (IY+)
    0<> IF
	LD HL, -6 (IY+)
	LD A, L
	OR H
	LD HL, -4 (IY+)
	OR L
	OR H
	LD HL, -2 (IY+)
	OR L
	OR H          \ Don't count -0 as less than 0.
	0<> IF
	    DEC DE 
	THEN
    THEN
    LEA IY, -8 IY+
    LD FP (), IY
    POP IY
    NEXT    
END-CODE

CODE F0= ( --- f  F: r -- -)
\G Test if floating point number is equal to zero.    
    PUSH DE
    PUSH IY
    LD IY, FP ()
    LD DE, 0
    LD A, -8 (IY+)
    INC A
    0= IF
	LD A, -7 (IY+)
	AND $7F
	CP $7F
	0= IF
	    LEA IY, -8 IY+
	    LD FP (), IY
	    POP IY
	    NEXT    
	THEN
    THEN
    LD HL, -6 (IY+)
    LD A, L
    OR H
    LD HL, -4 (IY+)
    OR L
    OR H
    LD HL, -2 (IY+)
    OR L
    OR H
    0= IF
	DEC DE
    THEN
    LEA IY, -8 IY+
    LD FP (), IY
    POP IY
    NEXT    
END-CODE

CODE F< ( --- f F: r1 r2 ---)
    PUSH DE
    PUSH IY
    LD IY, FP ()
    LD DE, 0
    BIT 7 -7 (IY+)  \ Is r2 negative?
    0<> IF
	BIT 7 -15 (IY+) 
	0<> IF
	    \ Both numbers negative?
	    LD HL, 0
	    LD BC, 0
	    LD L, -8 (IY+)
	    LD H, -7 (IY+)
	    LD C, -16 (IY+)
	    LD B, -15 (IY+)
	    AND A
	    SBC HL, BC
	    U< IF
		DEC DE  \ exp[ r2 < exp r1, therefore true
	    ELSE
		0= IF
		    \ Exponents equal. compare significand.
		    AND A  
		    LD HL, -6 (IY+)
		    LD BC, -14 (IY+)
		    SBC HL, BC
		    LD HL, -3 (IY+)
		    LD BC, -11 (IY+)
		    SBC HL, BC
		    U< IF
			DEC DE
		    THEN
		THEN
	    THEN	    
	THEN
	\ If r1 positive r2 negative, we know F< is false
    ELSE
	BIT 7 -15 (IY+) 
	0= IF
	    \ Both numbers positive?
	    LD HL, 0
	    LD BC, 0
	    LD L, -16 (IY+)
	    LD H, -15 (IY+)
	    LD C, -8 (IY+)
	    LD B, -7 (IY+)
	    AND A
	    SBC HL, BC
	    U< IF
		DEC DE  \ exp[ r1 < exp r2, therefore true
	    ELSE
		0= IF
		    \ Exponents equal. compare significand.
		    AND A  
		    LD HL, -14 (IY+)
		    LD BC, -6 (IY+)
		    SBC HL, BC
		    LD HL, -11 (IY+)
		    LD BC, -3 (IY+)
		    SBC HL, BC
		    U< IF
			DEC DE
		    THEN
		THEN
	    THEN
	ELSE 
	    \ r1 negative r2 positive, less unless both numbers zero.
	    LD HL, -14 (IY+)
	    LD A, L
	    OR H
	    LD HL, -12 (IY+)
	    OR L
	    OR H
	    LD HL, -10 (IY+)
	    OR L
	    OR H
	    LD HL, -6 (IY+)
	    OR L
	    OR H
	    LD HL, -4 (IY+)
	    OR L
	    OR H
	    LD HL, -2 (IY+)
	    OR L
	    OR H
	    0<> IF
		DEC DE
	    THEN
	THEN
    THEN	
    LEA IY, -16 IY+
    LD FP (), IY
    POP IY
    NEXT    
END-CODE

CODE FNEGATE ( F; r1 --- r2)
\G Negate the floating point number.    
    PUSH IY
    LD IY, FP ()
    LD A, -7 (IY+)
    XOR $80
    LD -7 (IY+), A
    POP IY
    NEXT
END-CODE

CODE FABS ( F: r1 --- r2)
\G Take the absolute value of the floating point number.    
    PUSH IY
    LD IY, FP ()
    RES 7 -7 (IY+)
    POP IY
    NEXT
END-CODE

SUBROUTINE ORDER-ABS
\ Put the number with the highest abolute value in the IY-16 position, the
\ other number in the IY-8 position.
    LD HL, 0
    LD BC, 0
    LD L, -8 (IY+)
    LD H, -7 (IY+)
    LD A, H
    AND $7F
    LD H, A
    LD C, -16 (IY+)
    LD B, -15 (IY+)
    LD A, B
    AND $7F
    LD B, A
    SBC HL, BC
    RET C   
    0= IF
	\ Exponents equal. compare significand.
	XOR A  
	LD HL, -6 (IY+)
	LD BC, -14 (IY+)
	SBC HL, BC
	0= IF
	    INC A
	THEN    
	LD HL, -3 (IY+)
	LD BC, -11 (IY+)
	SBC HL, BC
	RET C
	0= IF
	    DEC A
	    RET Z
	THEN
    THEN	    
    \ If we got here, the numbers need swapping.
    LD HL, -16 (IY+)
    LD BC, -8 (IY+)
    LD -16 (IY+), BC
    LD -8 (IY+), HL
    LD HL, -13 (IY+)
    LD BC, -5 (IY+)
    LD -13 (IY+), BC
    LD -5 (IY+), HL
    LD HL, -10 (IY+)
    LD BC, -2 (IY+)
    LD -10 (IY+), C
    LD -9 (IY+), B
    LD -2 (IY+), HL    
    RET
END-CODE

SUBROUTINE F-ALIGN
\ Align the number at IY-8 position to make its exponent equal to the
\ num in the IY-16 position. If exponent difference is too large make
\ return with C flag set immediately, otherwise clear C flag
\ Use byte at IY+1 as guard digit
\ Numbers are already ordered by absolute value, so the IY-8 number cannot
\ have a lerge exponent.
    LD 1 (IY+), 0 
    LD HL, 0
    LD BC, 0
    LD L, -16 (IY+)
    LD H, -15 (IY+)
    LD A, H
    AND $7F
    LD H, A
    LD C, -8 (IY+)
    LD B, -7 (IY+)
    LD A, B
    AND $7F
    LD B, A
    SBC HL, BC
    RET Z           \ Done if exponent difference is 0
    LD BC, #49
    SBC HL, BC
    CCF
    RET C           \ If Exponent differnce > 48, do not bother
    ADD HL, BC
    LD A, L
    LD BC, -6 (IY+) 
    LD DE, -4 (IY+)
    LD HL, -2 (IY+) \ Load significand into 3 regs, 2 bytes each.
    BEGIN
	SRL H \ Shift significand right repeatedly.
	RR L
	RR D
	RR E
	RR B
	RR C
	RR 1 (IY+) \ Rotate into guard digit
	DEC A
    0= UNTIL
    LD -6 (IY+), BC
    LD -4 (IY+), DE
    LD -2 (IY+), HL \ Store it back    
    AND A  \ clear carry flag
    RET
END-CODE

SUBROUTINE F-NORMALIZE
\ Normalize the number at IY-16 so it has a leading bit set.
\ Unless the exponent hits the minimum, then keep unnormalized.
\ CLear sign bit and set exp to zero if the whole number euqals zero.
    LD A, 1 (IY+)
    LD HL, -14 (IY+)
    EXX
    LD HL, -11 (IY+)
    OR L
    OR H
    OR -12 (IY+)
    EXX
    OR L
    OR H
    OR -9 (IY+)
    0= IF
	\ Number is completely zero?
	LD A, -15 (IY+)
	AND $80
	LD -15 (IY+), A \ Keep sign, rest of exp will be zero.
	XOR A
	LD -16 (IY+), A
    ELSE
      LD DE, -16 (IY+)
      LD A, D
      AND $7F
      LD D, A	
      LD C, 1 (IY+)
      @BEGIN
	DEC DE
	LD A, E
	OR D
	0= IF
	    LD -14 (IY+), HL
	    EXX
	    LD -11 (IY+), HL
	    EXX
     	    LD A, -15 (IY+)
	    AND $80
	    LD -15 (IY+), A \ Keep sign, rest of exp will be zero.
	    XOR A
	    LD -16 (IY+), A
	    RET
	THEN
	SLA C
        ADC HL, HL
	EXX
	ADC HL, HL
	EXX
      0< @UNTIL
      LD 1 (IY+), C
      LD -14 (IY+), HL
      EXX
      LD -11 (IY+), HL
      EXX
      LD -16 (IY+), E
      LD A, -15 (IY+)
      AND $80
      OR D
      LD -15 (IY+), A	
    THEN	
    RET
END-CODE

SUBROUTINE F-OVERFLOW
\ Shift the number at IY-16 one to the right and increase the exponent.
\ Because the addition in the significand just overflowed.
    LD BC, -14 (IY+) 
    LD DE, -12 (IY+)
    LD HL, -10 (IY+) \ Load significand into 3 regs, 2 bytes each.
    RR H \ Shift significand right
    RR L
    RR D
    RR E
    RR B
    RR C
    RR 1 (IY+) \ push LSB into the gard bits.
    LD -14 (IY+), BC
    LD -12 (IY+), DE
    LD -10 (IY+), HL \ Store it back    
    LD C, -16 (IY+)
    LD B, -15 (IY+)
    INC BC
    LD -16 (IY+), C
    LD -15 (IY+), B  \ Increment exponent, might increment into the Inf value.
    RET
END-CODE

SUBROUTINE F-ROUND-RESULT
\ Round the number at IY-16. Look at the MSB in the guard bits and add one if
\ set. This could everflow.
    SLA 1 (IY+)
    U< IF \ Increment HL
	LD BC, 0
	LD HL, -14 (IY+)
	ADC HL, BC
	LD -14 (IY+), HL
	LD HL, -11 (IY+)
	ADC HL, BC
	U< IF
	    \ Rounding has overflowed,
	    \ can only happen when original is all $fffff_ffff_ffff.
	    \ increments into $0000_0000_0000, should become $8000_0000_0000
	    \ plus increment exponent.
	    LD HL, $800000
	    LD C, -16 (IY+)
	    LD B, -15 (IY+)
	    INC BC
	    LD -16 (IY+), C
	    LD -15 (IY+), B  \ Increment exponent, might increment into the Inf value.
	THEN
	LD -11 (IY+), HL
    THEN
    RET
END-CODE

CODE F+ ( F: r1 r2 --- r3)
\G Add floating point numbers r1 and r2, giving r3
    PUSH DE
    PUSH IY
    LD IY, FP ()
    CALL ORDER-ABS
    LD L, -16 (IY+)
    LD A, -15 (IY+)
    AND $7F
    LD H, A
    INC HL
    LD A, $80
    AND H
    0= IF      \ Check if exponent == 0x7fff (inf/nan), keep that way, no op.
	CALL F-ALIGN
	U>= IF
	    \ No carry, so there is something to add/subtract.
	    LD A, -15 (IY+)
	    XOR -7 (IY+)
	    AND $80
	    0= IF
		\ Equal signs, addition.
		AND A
		LD HL, -14 (IY+)
		LD BC, -6 (IY+)
		ADC HL, BC
		LD -14 (IY+), HL
		LD HL, -11 (IY+)
		LD BC, -3 (IY+)
		ADC HL, BC
		LD -11 (IY+), HL
		CALL C, F-OVERFLOW
	    ELSE
		\ Different signs, subtraction.
		LD A, 1 (IY+)
		NEG
		LD 1 (IY+), A
		LD HL, -14 (IY+)
		LD BC, -6 (IY+)
		SBC HL, BC
		LD -14 (IY+), HL
		LD HL, -11 (IY+)
		LD BC, -3 (IY+)
		SBC HL, BC
		LD -11 (IY+), HL
		CALL P, F-NORMALIZE
	    THEN
	THEN
	CALL F-ROUND-RESULT
    THEN
    LEA IY, -8 IY+
    LD FP (), IY
    POP IY
    POP DE
    NEXT
END-CODE

: F- ( F: r1 r2 --- r3)
\G Subtract floating point numbers r1 and r2, giving r3    
  FNEGATE F+ ;

: F> ( --- f F: r1 r2 ---)
\G Return true if r1 is greater than r2.
    FSWAP F< ;

: F= ( --- f F: r1 r2 ---)
\G Return true if r1 is equal to r2.
    F- F0= ;

: FMIN ( F: r1 r2 --- r3)
\G Return the minimum of r1 and r2
  FOVER FOVER F< IF FDROP ELSE FSWAP FDROP THEN ;

: FMAX ( F: r1 r2 --- r3)
\G Return the maximum of r1 and r2
  FOVER FOVER F< IF FSWAP FDROP ELSE FDROP THEN ;

SUBROUTINE MUL-ROW
\ HL points to LSW of multiplier
\ DE points to LSW of product
\ C  is byte from multiplicand
\ A is number of bytes.
    PUSH IX
    LD IXL, C
    LD IXH, A
    XOR A
    BEGIN
	LD B, (HL)
	INC HL
	LD C, IXL
	MLT BC
	ADD C
	LD (DE), A
	INC DE
	LD A, B
	ADC A, 0
	DEC IXH
    0= UNTIL
    LD (DE), A
    POP IX
    RET
END-CODE

CREATE MUL-ACC 9 ALLOT

SUBROUTINE RESULT-ZERO
    LD E, 0
    LD -16 (IY+), DE
    LD DE, 0
    LD -14 (IY+), DE
    LD -11 (IY+), DE
    LEA IY, -8 IY+
    LD FP (), IY
    POP IY
    POP DE
    NEXT
END-CODE

SUBROUTINE RESULT-INF
    LD A, D
    OR $7F
    LD D, A
    LD E, $FF
    LD -16 (IY+), DE
    LD DE, 0
    LD -14 (IY+), DE
    LD -11 (IY+), DE
    LEA IY, -8 IY+
    LD FP (), IY
    POP IY
    POP DE
    NEXT
END-CODE


CODE F* ( F: r1 r2 --- r3)
\G Multiply floating point numbers r1 and r2, giving r3    
    PUSH DE
    PUSH IY
    LD IY, FP ()
    \ XOR signs
    LD HL, 0
    LD L, -16 (IY+)
    LD H, -15 (IY+)
    LD BC, 0
    LD C, -8 (IY+)
    LD B, -7 (IY+)
    LD A, H
    XOR B
    AND $80
    LD D, A  \ Target sign in D
    LD A, B
    AND $7F
    LD B, A
    LD A, H
    AND $7F
    LD H, A  \ BC and HL are the exponents sans sign        
    \ Check exponents any inf -> resut = inf
    \                 any zero -> result is zero
    INC HL
    LD A, H
    AND $80
    JP NZ, RESULT-INF
    DEC HL
    INC BC
    LD A, B
    AND $80
    JP NZ, RESULT-INF
    DEC BC
    LD A, C
    OR B
    JP Z, RESULT-ZERO
    LD A, L
    OR H
    JP Z, RESULT-ZERO
    
    \ Add exponents overflow -> result is inf
    \               underflow -> result is 0
    ADD HL, BC
    AND A
    LD BC, $3fff
    SBC HL, BC
    JP C, RESULT-ZERO
    JP Z, RESULT-ZERO
    LD BC, $8000
    SBC HL, BC
    JP NC, RESULT-INF
    ADD HL, BC
    LD A, H
    OR D              \ Include sign in exponent word.
    LD -16 (IY+), L
    LD -15 (IY+), A
    \ Main multiplication.
    
    \ multiply Byte 5 (MSB)
    LD DE, MUl-ACC
    LEA HL, -6 IY+
    LD C, -9 (IY+)
    LD A, 6
    CALL MUL-ROW
    \ Multiply byte 4
    LEA DE, 0 IY+
    LEA HL, -5 IY+
    LD C, -10 (IY+)
    LD A, 5
    CALL MUL-ROW
    LD HL, 0 (IY+)
    LD DE, MUL-ACC  ()
    ADD HL, DE
    LD MUL-ACC  (), HL
    LD HL, 3 (IY+)
    LD DE, MUL-ACC 3 + ()
    ADC HL, DE
    LD MUL-ACC 3 + (), HL
    LD A, MUL-ACC 6 + ()
    ADC A, 0
    LD MUL-ACC 6 + (), A
    \ Multiply byte 3
    LEA DE, 0 IY+
    LEA HL, -4 IY+
    LD C, -11 (IY+)
    LD A, 4
    CALL MUL-ROW
    LD HL, 0 (IY+)
    LD DE, MUL-ACC ()
    ADD HL, DE
    LD MUL-ACC (), HL
    LD HL, 0
    LD L, 3 (IY+)
    LD H, 4 (IY+)
    LD DE, MUL-ACC 3 + ()
    ADC HL, DE
    LD MUL-ACC 3 + (), HL
    LD A, MUL-ACC 6 + ()
    ADC A, 0
    LD MUL-ACC 6 + (), A    
    \ Multiply byte 2    
    LEA DE, 0 IY+
    LEA HL, -3 IY+
    LD C, -12 (IY+)
    LD A, 3
    CALL MUL-ROW
    LD HL, 0 (IY+)
    LD DE, MUL-ACC ()
    ADD HL, DE
    LD MUL-ACC (), HL
    LD HL, 0
    LD L, 3 (IY+)
    LD DE, MUL-ACC 3 + ()
    ADC HL, DE
    LD MUL-ACC 3 + (), HL
    LD A, MUL-ACC 6 + ()
    ADC A, 0
    LD MUL-ACC 6 + (), A    
    \ Multiply byte 1
    LEA DE, 0 IY+
    LEA HL, -2 IY+
    LD C, -13 (IY+)
    LD A, 2
    CALL MUL-ROW
    LD HL, 0 (IY+)
    LD DE, MUL-ACC ()
    ADD HL, DE
    LD MUL-ACC (), HL
    LD HL, 0
    LD DE, MUL-ACC 3 + ()
    ADC HL, DE
    LD MUL-ACC 3 + (), HL
    LD A, MUL-ACC 6 + ()
    ADC A, 0
    LD MUL-ACC 6 + (), A    
    \ Multiply byte 0 (LSB)
    LEA DE, 0 IY+
    LEA HL, -1 IY+
    LD C, -14 (IY+)
    LD A, 1
    CALL MUL-ROW
    LD HL, 0
    LD L, 0 (IY+)
    LD H, 1 (IY+)
    LD DE, MUL-ACC ()
    ADD HL, DE
    LD MUL-ACC (), HL
    LD HL, 0
    LD DE, MUL-ACC 3 + ()
    ADC HL, DE
    LD MUL-ACC 3 + (), HL
    LD A, MUL-ACC 6 + ()
    ADC A, 0
    LD MUL-ACC 6 + (), A    
    \ Copy from mul-acc to number at IY-16, guard bits to IY+1
    LD A, MUL-ACC ()
    LD 1 (IY+), A
    LD HL, MUL-ACC 1+ ()
    LD -14 (IY+), HL
    LD HL, MUL-ACC 4 + ()
    LD BC, 0
    AND A
    ADC HL, BC
    LD -11 (IY+), HL    
    CALL P,  F-NORMALIZE
    CALL F-ROUND-RESULT
    LEA IY, -8 IY+
    LD FP (), IY
    POP IY
    POP DE
    NEXT
END-CODE

CODE F/ ( F: r1 r2 --- r3)
\G Divide floating point numbers r1 and r2, giving r3    
    PUSH DE
    PUSH IY
    LD IY, FP ()
    \ XOR signs
    LD HL, 0
    LD L, -16 (IY+)
    LD H, -15 (IY+)
    LD BC, 0
    LD C, -8 (IY+)
    LD B, -7 (IY+)
    LD A, H
    XOR B
    AND $80
    LD D, A  \ Target sign in D
    LD A, B
    AND $7F
    LD B, A
    LD A, H
    AND $7F
    LD H, A  \ BC and HL are the exponents sans sign        
    \ Check exponents: dividend inf -> result is inf.
    \                  divisor inf  -> result is zero
    \                  divisor zero -> result is inf
    \                  dividend zero -> result is 0
    INC HL
    LD A, H
    AND $80
    JP NZ, RESULT-INF
    DEC HL
    INC BC
    LD A, B
    AND $80
    JP NZ, RESULT-ZERO
    DEC BC
    LD A, C
    OR B
    JP Z, RESULT-INF
    LD A, L
    OR H
    JP Z, RESULT-ZERO

    \ Subtract exponents; overflow -> result is inf
    \                     underflow -> result is zero.
    AND A
    SBC HL, BC
    AND A
    LD BC, $4000
    ADC HL, BC
    LD BC, 1
    AND A
    SBC HL, BC
    JP M, RESULT-ZERO
    ADD HL, BC
    AND A
    LD BC, $7fff
    SBC HL, BC
    JP NC, RESULT-INF
    ADD HL, BC
    LD A, H
    OR D              \ Include sign in exponent word.
    LD -16 (IY+), L
    LD -15 (IY+), A
    \ Main division loop. dividend in HL',HL, divisor in DE' DE
    LD HL, -14 (IY+)
    LD DE, -6 (IY+)
    EXX
    LD HL, -11 (IY+)
    LD DE, -3 (IY+)
    EXX
    AND A
    LD B, #56
    BEGIN
	U< IF \ Carry out means subtracion will work.
	    CCF
	    SBC HL, DE \ Subtract divisor.
	    EXX
	    SBC HL, DE
	    EXX
	    SCF    \ Set carry will shift a one into quatient.
	ELSE
	    SBC HL, DE \ Subtract divisor, trial subtraction.
	    EXX
	    SBC HL, DE
	    EXX
	    U< IF
		ADD HL, DE
		EXX
		ADC HL, DE 
		EXX         \ undo the current subtraction
		SCF
	    THEN
	    CCF
	THEN \ shft quotient bit in.
	RL 1 (IY+)
	RL 2 (IY+)
	RL 3 (IY+)
	RL 4 (IY+)
	RL 5 (IY+)
	RL 6 (IY+)
	RL 7 (IY+)
	ADD HL, HL \ Shift dividend left.
	EXX
	ADC HL, HL 
	EXX         
    B--0= UNTIL
    LD HL, 2 (IY+)
    LD -14 (IY+), HL
    LD HL, 5 (IY+)
    LD -11 (IY+), HL
    LD BC, 0
    AND A
    ADC HL, BC
    CALL P, F-NORMALIZE
    CALL F-ROUND-RESULT
    LEA IY, -8 IY+
    LD FP (), IY
    POP IY
    POP DE
    NEXT
END-CODE

CODE UD>F ( ud --- F: ---- r)
\G Convert unsigned double number to floating point.
    EX DE, HL
    EXX
    POP HL
    PUSH IY
    LD IY, FP ()
    LD BC, 0
    XOR A
    ADC HL, BC
    0= IF
	INC A
    THEN	
    EXX
    LD  BC, 0
    ADC HL, BC
    EXX
    0= IF
	INC A
    THEN
    CP 2
    0= IF
	\ Number is 0, store all-zero float value.
	LD 0 (IY+), HL
	LD 3 (IY+), HL
	LD 6 (IY+), HL
    ELSE
	EXX
	AND A
	ADC HL, BC
	EXX
	LD BC, $402F \ Initialize exponent to largest exponent of double int.
	@BEGIN
	0>= @WHILE
	   DEC BC   
	   ADD HL, HL
	   EXX
	   ADC HL, HL  \ Normalize the number until MSB is set.
	   EXX
	@REPEAT
	LD 0 (IY+), BC \ Store exponent
	LD 2 (IY+), HL
	EXX
	LD 5 (IY+), HL \ Store significand
    THEN
    LEA IY, 8 IY+
    LD FP (), IY
    POP IY
    POP DE
    NEXT
END-CODE	
    
CODE F>UD ( --- ud F: r ---)
\G Convert floating point to unsigned double, return 2**48-1 if out of range
    PUSH DE
    PUSH IY
    LD IY, FP ()
    LD HL, 0
    LD L, -8 (IY+)
    LD H, -7 (IY+)
    LD A, H
    AND $7F
    LD H, A          \ Obtain exponent.
    LD BC, $4000
    AND A
    SBC HL, BC       \ Subtract $4000
    U< IF
	\ We are zero
	LD HL, 0
	EXX
	LD HL, 0
    ELSE
	LD BC, #48
	SBC HL, BC
	U>= IF
	    \ We are >= 2**48
	    LD HL, -1
	    EXX
	    LD HL, -1
	ELSE
	    ADD HL, BC
	    LD A, L
	    CP #47
	    0<> IF
		NEG
		ADD #47 
		LD BC, -6 (IY+) 
		LD DE, -4 (IY+)
		LD HL, -2 (IY+) \ Load significand into 3 regs, 2 bytes each.
		BEGIN
		    SRL H \ Shift significand right repeatedly.
		    RR L
		    RR D
		    RR E
		    RR B
		    RR C
		    DEC A
		0= UNTIL
		LD -6 (IY+), BC
		LD -4 (IY+), DE
		LD -2 (IY+), HL \ Store it back
	    THEN
	    LD HL, -6 (IY+)
	    EXX
	    LD HL, -3 (IY+) \ Load into HL & HL'
	THEN
    THEN
    LEA IY, -8 IY+
    LD FP (), IY
    POP IY
    EXX
    PUSH HL
    EXX
    EX DE, HL
    NEXT
END-CODE

: D>F ( d --- F: --- r)
\G Convert signed double number to floating point.
    DUP 0< >R
    DABS UD>F
    R> IF FNEGATE THEN ;

: F>D ( --- d : r --- )
\G Convert floating point number to signed double, return -2**47 if out of range
    FDUP F0< >R FABS
    F>UD DUP 0< IF 2DROP 0 $800000 THEN
    R> IF DNEGATE THEN 
;

0. D>F FCONSTANT 0.0E
1. D>F FCONSTANT 1.0E
10. D>F FCONSTANT 10.0E
1.0e 2. d>f f/ FCONSTANT 0.5E

: FI** ( n --- F: r1 ---r2)
\G Raise a floating point number to an integer power.
    DUP 0< >R ABS
    1.0E 
    BEGIN
	DUP
    WHILE
	DUP 1 AND IF
	    FOVER F* 
	THEN
	FSWAP FDUP F* FSWAP
	1 RSHIFT
    REPEAT
    FSWAP FDROP DROP R> IF 1.0E fSWAP F/ THEN ;

VARIABLE EXP-STATE
VARIABLE EXP
: >FLOAT ( c-addr u --- true | false F: ---r | ) 
\G Convert the string at c-addr u to a floating point number. If
\G success rutrn true and a floating point number on the FP stack, else
    \G return false and nothing on the FP stack.
    0 EXP-STATE !
    0 EXP !
    0.0E \ Initial value of float
    BL SKIP
    -1 DPL !
    DUP IF
	OVER C@ '+' = IF
	    SWAP 1+ SWAP 1-
	ELSE
	    OVER C@ '-' = IF
		SWAP 1+ SWAP 1- -1
	    ELSE
		0
	    THEN
	ELSE
	    0
	THEN >R \ Store sign.
	BOUNDS ?DO
	    \ CR I . I C@ . DPL @ . EXP-STATE @ . EXP @ .
	    EXP-STATE @ 0= IF
		I C@ DIGIT? IF
		    10.0E F*
		    0 D>F F+ 
		    DPL @ 0< 0= IF 1 DPL +! THEN
		ELSE
		    I C@ '.' =  DPL @ 0<  AND  IF
			0 DPL !
		    ELSE
			I C@ $DE AND 'D' = IF
			    1 EXP-STATE !
			ELSE
			    I C@ BL <> IF FDROP UNLOOP R> DROP FALSE  EXIT THEN
			THEN
		    THEN
		THEN
	    ELSE
		EXP-STATE @ 1 = IF
		    I C@ '+' = IF
			2 EXP-STATE !
		    ELSE
			I C@ '-' = IF
			    3 EXP-STATE !
			ELSE
			    2 EXP-STATE !
			    I C@ DIGIT? IF
				 EXP !
			    ELSE
				I C@ BL <> IF FDROP  UNLOOP  R> DROP FALSE  EXIT THEN
			    THEN
			THEN
		    THEN
		ELSE
		    I C@ DIGIT? IF
			EXP @ 10 * + EXP !
		    ELSE
			I C@ BL <> IF FDROP  UNLOOP  R> DROP FALSE  EXIT THEN
		    THEN
		THEN
	    THEN
	LOOP
	EXP-STATE @ 3 = IF EXP @ NEGATE EXP ! THEN
	EXP @ DPL @ 0< 0= IF DPL @ - THEN
	DUP 0< 0= IF
	    10.0E FI** F*
	ELSE
	    NEGATE 10.0E FI** F/
	THEN
	R> IF FNEGATE  THEN
    ELSE 2DROP	
    THEN
    TRUE
;

CODE IS-FLOAT-LIT? ( c-addr --- c-addr f)
\G Test if the counted string at c-addr can be a floating point
\G literal. BASE has to be 10 and the string has to contain an E.
    PUSH DE
    LD HL, BASE ()
    LD BC, 10
    AND A
    SBC HL, BC
    0= IF
	EX DE, HL
	LD B, (HL)
	INC HL
	BEGIN
	    LD A, (HL)
	    INC HL
	    CP 'E'
	    0= IF
		LD DE, -1 \ Found E in string, return true.
		NEXT
	    THEN
	B--0= UNTIL
	LD DE, 0 \ Not found E, return false.
    ELSE
	LD DE, 0 \ Base not 10, return false
    THEN
    NEXT
END-CODE    

: FNUMBER-EXEC ( c-addr ---- c-addr 0 | -1 )
\G Code to handle float literals in the interpreter.    
  IS-FLOAT-LIT? IF
      DUP COUNT >FLOAT IF
	  DROP STATE @ IF POSTPONE FLITERAL THEN TRUE
      ELSE
	  FALSE
      THEN
  ELSE
      FALSE
  THEN
;

' FNUMBER-EXEC FNUMBER-VECTOR !
\ From now on, floating point literals are accepted in the interpreter.

CODE F-EXP@ ( --- n F: r --- r)
\G Extract binary the exponent from a number, convert to signed integer.
    PUSH DE
    LD HL, FP ()
    DEC HL
    DEC HL
    DEC HL
    DEC HL
    DEC HL
    DEC HL
    DEC HL
    LD DE, 0
    LD A, (HL)
    AND $7F      \ Mask out sign bit.
    LD D, A
    DEC HL
    LD E, (HL) 
    LD HL, -$4000 \ subtract the offset
    ADD HL, DE
    EX DE, HL
    NEXT    
END-CODE

CODE F-EXP! ( n --- F: r --- r)
\G Store binary exponent into a number. Clamp to minimum and maximum value.
    EX DE, HL
    LD DE, $4000 \ Add offet back in.
    AND A
    ADC HL, DE
    0< @IF
      LD HL, 0    \ CLamp to minimum 0.
    @THEN 
    LD DE, $7fff
    AND A
    SBC HL, DE
    U>= IF
	LD HL, 0   \ Clamp to 7fff (infinity).
    THEN
    ADD HL, DE
    EX DE, HL
    LD HL, FP ()
    DEC HL
    DEC HL
    DEC HL
    DEC HL
    DEC HL
    DEC HL
    DEC HL
    LD A, (HL)
    AND $80        \ Preserve the sign.
    OR D
    LD (HL), A
    DEC HL
    LD (HL), E
    POP DE
    NEXT
END-CODE    

: F-SCALE ( n --- F: r1 --- r2 )
    F-EXP@ + F-EXP! ;

: F-SGN ( --- n F: r --- r)
\ Extract the sign of a floating point number, also return TRUE for -0    
    FP @ 7 - C@ $80 AND 0= 0= ;
    
: F-ISINF ( --- n : F: r --- r)
\G check if the number is infinity or NaN
    F-EXP@ $3fff = ;

FVARIABLE F-LIMIT
: REPRESENT ( c-addr u --- n flag1 flag2 F: r ---)
\G Convert floating point nunmber to a string of decimal dtgits at c-addr
\G (length is u).
\G n is the base 10 exponent. 0 is decimal point should be just before the
\G digits, +n means the decimal point should be after n digits, -n means
\G decimal point should be n zeros to the left of the digits returned.
\G Flag1 is the sign (true for negative), flag2 is set if there was a valid
\G number (not infinity or NaN)
    2DUP '0' FILL \ Fill buffer with zeros.
    DUP 1- 13 MIN EXP !  \ store precision-1 
    F-SGN >R
    F-ISINF IF
	FDROP 2DROP 0 R> 0
    ELSE
	FDUP F0= IF
	    FDROP 2DROP 1 R> -1 
	ELSE
	    FABS F-EXP@ 301 1000 */ \ Convert binary exponent to base 10 exp
	    DUP >R \ Store exponent on return stack
	    NEGATE EXP @ + DUP 0< IF
		NEGATE 10.0e FI** F/
	    ELSE
		10.0e FI** F* \ Try to get num in range 10*(prec-1)..10**prec
	    THEN
	    10.0e EXP @ FI** 0.5E F- F-LIMIT F!
	    BEGIN
		FDUP F-LIMIT F@ F<
	    WHILE
		    10.0e F*
		    R> 1- >R 
	    REPEAT
	    F-LIMIT F@ 10.0E F* 4.5E F+ F-LIMIT F!
	    BEGIN
		FDUP F-LIMIT F@ F< 0=
	    WHILE
		    10.0e F/
		    R> 1+ >R
	    REPEAT
	    0.5e f+
	    \ Now the number should be in range 10**(prec-1)..10**prec, correct deciamal exp
	    F>UD <# EXP @ 1+ 0 DO # LOOP #> \ Convert to double int, to decimal digits
	    DROP SWAP 14 MIN >R SWAP R> CMOVE \ store digits
	    R> 1+ R> -1
	THEN
    THEN	
;

14 VALUE PRECISION

: SET-PRECISION
  1 MAX 14 MIN TO PRECISION ;


: FS. ( F: --- r )
\G Print a floating point number in scientific notation.    
    PAD PRECISION REPRESENT
    SWAP IF '-' EMIT THEN
    0= IF
	DROP ." Inf "
    ELSE
	PAD C@ EMIT '.' EMIT PAD 1+ PRECISION 1- TYPE
	'E' EMIT 1- .
    THEN
;


: F. ( F: --- r )
\G Print a floating point number in normal (non-scientific) notation.    
    F-SGN IF '-' EMIT THEN
    FABS
    FDUP F0= IF
	FDROP ." 0.0 "
    ELSE
	FDUP 1E-5 F< FDUP PRECISION 10.0E FI** 0.5e F- F< 0= OR IF
	    FS.
	ELSE
	    PAD PRECISION REPRESENT
	    2DROP
	    DUP 0<= IF
		." 0." NEGATE 0 ?DO '0' EMIT LOOP PAD PRECISION TYPE 
	    ELSE
		PAD OVER TYPE '.' EMIT PAD OVER + PRECISION ROT -
		DUP 0> IF TYPE ELSE 2DROP THEN
	    THEN
	    SPACE
	THEN
    THEN
;

: F.S ( --- )
\G Print the contents of the floating point stack.
  FDEPTH DUP 0= IF
    DROP ." Empty "
  ELSE	
    0 DO F0 @ I FLOATS + F@ F. LOOP
  THEN
;

1E 0E F/ FCONSTANT NAN

: FSQRT ( F: r1  --- r2)
\G Compute the square root.    
    F-ISINF 0= IF
	FDUP F0< IF
	    FDROP NAN
	ELSE
	    FDUP F-EXP@ 2/ F-EXP!
	    5 0 DO
		FOVER FOVER F/ F+ -1 F-SCALE 
	    LOOP
	    FSWAP FDROP
	THEN
    THEN
;

: F.R ( u1 u2 --- F: r ---)
\G Print a floating point number with u2 digits after the decimal point and
\G n1 positions total, right-justified (like printf %9.5f notation)
    F-SGN >R FABS
    10.0e DUP FI** F* 0.5e F+ F>UD
    2DUP AND -1 = IF
	\ Overflows integer range
	R> 2DROP 2DROP  0 DO '*' EMIT LOOP
    ELSE
	<# ROT 0 ?DO # LOOP '.' HOLD #S R> SIGN #> ROT OVER - 0 MAX SPACES TYPE
    THEN
;

: FTRUNC-ODD ( --- n  F: r1 --- r2)
\G Truncte r1 towards zero, also return 1 if integer number is odd.
    F-EXP@ 48 <
    IF
	F-SGN F>UD OVER 1 AND >R UD>F IF FNEGATE THEN R>
    ELSE
	0 \ Do not change, assume number is even.
    THEN ;

: FLOOR ( --- F: r1 --- r2)
\G Return the floor (round towards -infinity) of floating point number.
    F-SGN
    FDUP FTRUNC-ODD DROP
    IF
	FDUP FROT F= 0= IF
	    1.0E F- \ Negative number, subtract 1 if not already integer.   
	THEN
    ELSE
	FSWAP FDROP \ Number not negative, floor is trunc.
    THEN
;

: FROUND ( --- F: r1 --- r2)
\G Return the number rounded to the nearest integer. Round to even if
    \G exactly halfway
    FDUP F0< \ Set sign apart.
    FABS
    FDUP FTRUNC-ODD 
    FDUP FROT F- FDUP -0.5E0 F= IF
	\ Difference exactly 0.5
	FDROP ( TOS is odd result) IF 1.0E F+ THEN
    ELSE
	DROP -0.5E F< IF 1.0E F+ THEN
    THEN
    IF FNEGATE THEN \ Re-apply sign.
;
