\ This is the file kernl80a.4th, included by the cross compiler.
\ created 1994 by L.C. Benschop.
\ copyleft (c) 1994-2014 by the sbc09 team, see AUTHORS for more details.
\ Most Z80 primitives: ; Copyright (c) 1994,1995 Bradford J. Rodriguez
\ copyleft (c) 2022 L.C. Benschop for Cerberus 2080.
\ copyleft (c) 2023 L.C. Benschop Agon FORTH
\ license: GNU General Public License version 3, see LICENSE for more details.
\ It is extensively commented as it must serve as an introduction to the
\ construction of Forth compilers.

\ Lines starting with \G are comments that are included in the glossary.

ALSO TRANSIENT DEFINITIONS
FORWARD THROW
FORWARD COLD
FORWARD WARM
FORWARD F-STARTUP
PREVIOUS DEFINITIONS

ALSO ASSEMBLER DEFINITIONS

PREVIOUS DEFINITIONS

ASSEMBLE HEX

\ PART 0: Boot vectors.
\ ORIGIN ORG
 C3 C,  TRANSIENT COLD ASSEMBLER
 46 C, 04F C, 52 C, 54 C, 48 C, 2E C, 42 C, 49 C, 4E C, 0 C, \ FORTH.BIN
 32 ALLOT-T
 4D C, 04F C, 53 C, 0 C, 1 C, \ Agon MOS header at 64

ENDASM

DECIMAL
CROSS-COMPILE

\ PART 1: Runtime parts of defining words

\ Note: the NEXT and NEXTHL macros are defined in assmz80.4th.
\ NEXT is expanded in-line.

\ Run-time part for constants:
\ Gets Parameter field addres on data stack from CALL instruction.
LABEL DOCON
  POP HL          \ Get param address
  PUSH DE         \ Store old TOS
  LD DE, (HL)
  NEXT
ENDASM

\ Run-time part for variables
LABEL DOVAR
  POP HL          \ Get param address
  PUSH DE         \ Store old TOS
  EX DE, HL
  NEXT
ENDASM

\ Run-time part for colon definitions
\ Paramter addres will be new IP
LABEL DOCOL
  LEA IX, -3 IX+
  LD 0 (IX+), IY
  POP IY          \ Get parameter field addres, new IP
  NEXT
ENDASM

\ Run-time part of CREATE DOES> defining words.
\ Each defined word contains a CALL to the part after DOES>, which
\ starts with a CALL to DODOES.
LABEL DODOES
  LEA IX, -3 IX+
  LD 0 (IX+), IY \ Push oid IP on return stackl
  POP IY         \ Pop new IP from  stack
  POP HL         \ Pop new TOS
  PUSH DE        \ Push old TOS
  EX DE, HL      \ New TOS to DE.
  NEXT
ENDASM

\ PART 2: Code definitions, laid out by compiler.

CODE LIT ( --- n)
\G Run-time part of LITERAL. Pushes next cell in instruction thread to stack.
    PUSH DE        \ Push old TOS
    LD DE, 0 (IY+) \ Load literal at instruction pointer.
    LEA IY, 3 IY+
    NEXT
END-CODE
    
CODE BRANCH ( --- )
\G High-level unconditional jump, loads IP from next cell in instruction
\G thread
LABEL BR
    LD IY, 0 (IY+)    
    NEXT
END-CODE

CODE ?BRANCH ( f ---)
\G High-level conditonal jump, jumps only if TOS=0
    LD HL, $FFFFFF
    ADD HL, DE \ Test TOS=0, add will give carry unless DE=0
    POP DE     \ Pop next entry into TOS
    JR NC, BR
    LEA IY, 3 IY+ \ skip over branch address
    NEXT
END-CODE    

CODE EXECUTE ( xt ---)
\G Execute the word with execution token xt. 
    EX DE, HL  \ Move CFA to HL
    POP DE   \ Pop next stack entry into TOS
    JP (HL)  \ Jump to it
END-CODE

CODE EXIT ( --- )
    \G Return from colon definition.
    LD IY, 0 (IX+)
    LEA IX, 3 IX+
    NEXT
END-CODE

CODE UNNEST ( --- )
\G Synonym for EXIT, used by compiler, so decompiler can use this as end of
\G colon definition.   
    \G Return from colon definition.
    LD IY, 0 (IX+)
    LEA IX, 3 IX+
    NEXT
END-CODE


CODE (DO) ( n1 n2 ---)
\G Runtime part of DO. n2 is initial counter, n1 is limit
    POP HL
LABEL DO1
    LD BC, $800000
    ADD HL, BC \ XOR Limit with $800000
    EX DE, HL \ Swap DE and HL. start in HL, limit xor 0x8000 inDE
    AND A
    SBC HL, DE \ HL = start - (limit xor 0x8000)
    LEA IX, -6 IX+
    LD 0 (IX+), HL
    LD 3 (IX+), DE
    \ terminate when crossing limit-1, limit in any directions.
    \ End-of-loop condition is indicated by overflow flag when adding
    \ modified loop variable. 
    \ regardless of positive or negative increment in +LOOP.
    POP DE    \ Load new TOS
    NEXT
END-CODE

CODE (?DO) ( n1 n2 ---)
\G Runtime part of ?DO. n2 is initial counter, n1 is limit
\G The next cell contains a branch address to skip the
\G loop when limit and start are equal.
    POP HL
    AND A
    SBC HL, DE \ Compare start and limit.
    0<> IF
      ADD HL, DE \ Add back to restore HL
      LEA IY, 3 IY+ \ Skip branch address.
      JR DO1 \ Continue into (DO) function.
    THEN 
    POP DE  \ Load new TOS
    JP BR   \ Branch beyond loop if operands were equal.
END-CODE

LABEL ENDLOOP
    LEA IY, 3 IY+ \ Skip branch back address.
    LEA IX, 6 IX+ \ Remove 2 loop values from return stack
    NEXT
ENDASM

CODE (LOOP) ( --- )
\G Runtime part of LOOP
    PUSH DE
    LD DE, 1
    LABEL LOOP1
    LD HL, 0 (IX+) \ Get loop variable
    AND A
    ADC HL, DE       \ Add increment to loop variable. Need to use ADC not ADD
                     \ because ADD HL,reg does not change parity/overflow
    POP DE           \ Restore TOS
    JP PE, ENDLOOP   \ End-of-loop when overflow flag is set.
                     \ On Z80 this is even parity.
    LD 0 (IX+), HL   \ Store updated loop variable back
    LD IY, 0 (IY+)   \ Load IP from cell after (LOOP), branch back.
    NEXT
END-CODE

CODE (+LOOP) ( n ---)
\G Runtime part of +LOOP    
    JR LOOP1        \ Loop increment is already in DE
END-CODE

CODE (LEAVE) ( ---)
\G Runtime of LEAVE
    LEA IX, 6 IX+
    LD IY, 0 (IY+)
    NEXT
END-CODE

\ PART 3: Code definitions used in programs.

CODE I ( --- n)
\G Get loop variable of innermost loop.
\ As the loop variable is stored in modifed form to facilitate end-of-loop
\ testing, we need to reverse this when obtaining I.    
    PUSH DE           \ Save old TOS
    LD HL, 0 (IX+)
    LD DE, 3 (IX+)
    ADD HL, DE        \ Add limit^xor 0x8000 to obtain original loop variable.
    EX DE, HL           \ Put in TOS
    NEXT
END-CODE

CODE I' ( ---n)
\G Get limit variable of innermost loop.    
    PUSH DE
    LD HL, 3 (IX+)
    LD DE, $800000
    ADD HL, DE
    EX DE, HL
    NEXT
END-CODE

CODE J ( ---n)
\G Get loop variable of next outer loop.    
    PUSH DE           \ Save old TOS
    LD HL, 6 (IX+)
    LD DE, 9 (IX+)
    ADD HL, DE        \ Add limit^xor 0x8000 to obtain original loop variable.
    EX DE, HL           \ Put in TOS
    NEXT
END-CODE

CODE UNLOOP ( --- )
\G Undo return stack effect of loop, can be followed by EXIT    
    LEA IX, 6 IX+
END-CODE

CODE R@ ( --- x)
\G x is a copy of the top of the return stack.
    PUSH DE
    LD DE, 0 (IX+)
    NEXT
END-CODE

CODE >R ( x ---)
\G Push x on the return stack. 
    LEA IX, -3 IX+
    LD 0 (IX+), DE
    POP DE
    NEXT
END-CODE

CODE R> ( --- x)
\G Pop the top of the return stack and place it on the stack.
    PUSH DE
    LD DE, 0 (IX+)
    LEA IX, 3 IX+
    NEXT
END-CODE

CODE RP@ ( --- a-addr)
\G Return the address of the return stack pointer.
    PUSH DE
    PUSH IX
    POP DE
    NEXT
END-CODE

CODE RP! ( a-addr --- )
\G Set the return stack pointer to a-addr.
    PUSH DE
    POP IX
    POP DE
    NEXT
END-CODE

CODE SP@ ( --- a-addr)
\G Return the address of the stack pointer (before SP@ was executed).
\G Note: TOS is in a register, hence stack pointer points to next cell.
    LD HL, 0
    ADD HL, SP
    PUSH DE
    EX DE, HL
    NEXT
END-CODE

CODE SP! ( a-addr ---)
\G Set the stack pointer to a-addr.
    EX DE, HL
    LD SP, HL
    NEXT
END-CODE

CODE * ( w1 w2 --- w3)
\G Multiply single numbers, signed or unsigned give the same result.
\G Multiply two unsigned numbers, giving double result.
\ Register usage: Operand1: IYL D E, operand2: IYH H L
\                 BC temporary registers for the 8*8 multiplications
\                 A scratch register    
\                 H' L' B' result
    POP HL \ get second operand
    PUSH IY \ save instruction pointer
    LD -3 (IX+), DE
    LD C, -1 (IX+)
    LD IYL, C      \ Get most sigificant byte of DE into IYL
    LD -3 (IX+), HL
    LD C, -1 (IX+)
    LD IYH, C      \ Get most sigificant byte of HL into IYH
    LD C, E
    LD B, L
    MLT BC         \ LSB * LSB
    LD A, C
    EXX
    LD B, A
    EXX
    LD A, B
    EXX
    LD L, A
    EXX
    LD C, E
    LD B, H
    MLT BC        \ LSB * ISB
    LD A, C
    EXX
    ADD A, L
    LD L, A
    EXX
    LD A, B
    EXX
    ADC A, 0
    LD H, A
    EXX
    LD C, D
    LD B, L
    MLT BC       \ ISB * LSB
    LD A, C
    EXX
    ADD A, L
    LD L, A
    EXX
    LD A, B
    EXX
    ADC A, H
    LD H, A
    EXX
    LD C, D
    LD B, H
    MLT BC       \ ISB * ISB
    LD A, C
    EXX
    ADD A, H
    LD H, A
    EXX
    LD C, E
    LD B, IYH
    MLT BC       \ LSB * MSB
    LD A, C
    EXX
    ADD A, H
    LD H, A
    EXX
    LD C, L
    LD B, IYL
    MLT BC       \ MSB * LSB
    LD A, C
    EXX
    ADD A, H
    LD H, A
    POP IY
    \ Shift HL to most significant two bytes of  result
    ADD HL, HL  
    ADD HL, HL
    ADD HL, HL
    ADD HL, HL
    ADD HL, HL
    ADD HL, HL
    ADD HL, HL
    ADD HL, HL
    EX DE, HL
    LD E, B
    NEXT
END-CODE    
    
CODE UM* ( u1 u2 --- ud)
\G Multiply two unsigned numbers, giving double result.
\ Register usage: Operand1: IYL D E, operand2: IYH H L
\                 BC temporary registers for the 8*8 multiplications
\                 A scratch register    
\                 H' L' B' result least sigificant word
\                 D' E' C' result most significant word    
    POP HL \ get second operand
    PUSH IY \ save instruction pointer
    LD -3 (IX+), DE
    LD C, -1 (IX+)
    LD IYL, C      \ Get most sigificant byte of DE into IYL
    LD -3 (IX+), HL
    LD C, -1 (IX+)
    LD IYH, C      \ Get most sigificant byte of HL into IYH
    LD C, E
    LD B, L
    MLT BC         \ LSB * LSB
    LD A, C
    EXX
    LD B, A
    EXX
    LD A, B
    EXX
    LD L, A
    EXX
    LD C, E
    LD B, H
    MLT BC        \ LSB * ISB
    LD A, C
    EXX
    ADD A, L
    LD L, A
    EXX
    LD A, B
    EXX
    ADC A, 0
    LD H, A
    EXX
    LD C, D
    LD B, L
    MLT BC       \ ISB * LSB
    LD A, C
    EXX
    ADD A, L
    LD L, A
    EXX
    LD A, B
    EXX
    ADC A, H
    LD H, A
    LD A, 0
    ADC A, 0
    LD C, A
    EXX
    LD C, D
    LD B, H
    MLT BC       \ ISB * ISB
    LD A, C
    EXX
    ADD A, H
    LD H, A
    EXX
    LD A, B
    EXX
    ADC A, C
    LD C, A
    LD A, 0
    ADC A, 0
    LD E, A
    EXX
    LD C, E
    LD B, IYH
    MLT BC       \ LSB * MSB
    LD A, C
    EXX
    ADD A, H
    LD H, A
    EXX
    LD A, B
    EXX
    ADC A, C
    LD C, A
    LD A, E
    ADC A, 0
    LD E, A
    EXX
    LD C, L
    LD B, IYL
    MLT BC       \ MSB * LSB
    LD A, C
    EXX
    ADD A, H
    LD H, A
    EXX
    LD A, B
    EXX
    ADC A, C
    LD C, A
    LD A, E
    ADC A, 0
    LD E, A
    EXX
    LD C, D
    LD B, IYH
    MLT BC       \ ISB * MSB
    LD A, C
    EXX
    ADD A, C
    LD C, A
    EXX
    LD A, B
    EXX
    ADC A, E
    LD E, A
    LD A, 0
    ADC A, 0
    LD D, A
    EXX
    LD C, H
    LD B, IYL
    MLT BC       \ MSB * ISB
    LD A, C
    EXX
    ADD A, C
    LD C, A
    EXX
    LD A, B
    EXX
    ADC A, E
    LD E, A
    LD A, D
    ADC A, 0
    LD D, A
    EXX
    LD C, IYH
    LD B, IYL
    MLT BC       \ MSB * MSB
    LD A, C
    EXX
    ADD A, E
    LD E, A
    EXX
    LD A, B
    EXX
    ADC A, D
    LD D, A
    ADD HL, HL  \ Shift HL to most significant two bytes of LSW of result
    ADD HL, HL
    ADD HL, HL
    ADD HL, HL
    ADD HL, HL
    ADD HL, HL
    ADD HL, HL
    ADD HL, HL
    LD  L, B
    POP IY
    PUSH HL     \ Push LSW of result
    EX DE, HL   \ Shift DE to most significant two bytes of MSW of result
    ADD HL, HL  
    ADD HL, HL
    ADD HL, HL
    ADD HL, HL
    ADD HL, HL
    ADD HL, HL
    ADD HL, HL
    ADD HL, HL
    EX DE, HL
    LD E, C
    NEXT
END-CODE

CODE UM/MOD  ( ud u1 --- urem uquot)
\G Divide the unsigned double number ud by u1, giving unsigned quotient
\G and remainder.        
    POP HL
    EXX
    POP HL    \ Get dividend in HL:HL'
    LD A, $18 \ 24 iterations
    ADD HL, HL  \ Shift HL:HL' left.
    EXX
    BEGIN
	ADC HL, HL  \ Continue left shift
	U< IF
	    OR A    \ If carry, unconditionally subtract BC 
	    SBC HL, DE
	    OR A    \ and clear carry
	ELSE
	    SBC HL, DE \ else conditonal subtraction
	    U< IF
		ADD HL, DE \ if borrow, undo subtraction
		SCF        \ and set carry
	    THEN
	THEN
	EXX               \ shift quotient bit in.
	ADC HL, HL
	EXX
	DEC A
    0= UNTIL
    EXX
    EX DE, HL
    LD HL, 0
    AND A
    SBC HL, DE
    DEC HL                \ Complement quotient
    EXX       
    PUSH HL               \ Push remainder
    EXX
    EX DE, HL             \ Move quotient to TOS
    NEXT
END-CODE

CODE + ( n1 n2 ---n3)
\G Add the top two numbers on the stack.
    POP HL
    ADD HL, DE
    EX DE, HL
    NEXT
END-CODE

CODE - ( w1 w2 ---w3)
\G Subtract the top two numbers on the stack (w2 from w1).
    POP HL
    AND A
    SBC HL, DE
    EX DE, HL
    NEXT
END-CODE

CODE NEGATE ( n1 --- -n1)
\G Negate top number on the stack.    
    LD HL, 0
    AND A
    SBC HL, DE
    EX DE, HL
    NEXT
END-CODE

CODE AND ( x1 x2 ---x3)
\G Bitwise and of the top two cells on the stack.
\ It sucks that you have to store the operands
\ like this, just for the upper bytes.
    POP HL  \ Get the second operand
    LD -3 (IX+), DE
    LD -6 (IX+), HL \ Store both operands, just below the return stack.
    LD A, -1 (IX+)  
    AND -4 (IX+)
    LD -1 (IX+), A  \ AND the upper bytes.
    LD DE, -3 (IX+) \ Reload TOS with correct upper byte.
    LD A, E
    AND L
    LD E, A
    LD A, D
    AND H
    LD D, A         \ AND the other 2 bytes in registers.
    NEXT
END-CODE

CODE OR ( x1 x2 ---x3)
\G Bitwise or of the top two cells on the stack. 
    POP HL  \ Get the second operand
    LD -3 (IX+), DE
    LD -6 (IX+), HL \ Store both operands, just below the return stack.
    LD A, -1 (IX+)  
    OR -4 (IX+)
    LD -1 (IX+), A  \ OR the upper bytes.
    LD DE, -3 (IX+) \ Reload TOS with correct upper byte.
    LD A, E
    OR L
    LD E, A
    LD A, D
    OR H
    LD D, A         \ OR the other 2 bytes in registers.
    NEXT
END-CODE

CODE XOR ( x1 x2 ---x3)
\G Bitwise exclusive or of the top two cells on the stack.
    POP HL  \ Get the second operand
    LD -3 (IX+), DE
    LD -6 (IX+), HL \ Store both operands, just below the return stack.
    LD A, -1 (IX+)  
    XOR -4 (IX+)
    LD -1 (IX+), A  \ XOR the upper bytes.
    LD DE, -3 (IX+) \ Reload TOS with correct upper byte.
    LD A, E
    XOR L
    LD E, A
    LD A, D
    XOR H
    LD D, A         \ XOR the other 2 bytes in registers.
    NEXT
END-CODE

CODE 1+ ( w1 --- w2)
\G Add 1 to the top of the stack.
    INC DE
    NEXT
END-CODE

CODE 1- ( w1 --- w2)
\G Subtract 1 from the top of the stack.
    DEC DE
    NEXT
END-CODE

CODE 2+ ( w1 --- w2)
\G Add 2 to the top of the stack.
    INC DE
    INC DE
    NEXT
END-CODE

CODE 2- ( w1 --- w2)
\G Subtract 2 from the top of the stack.
    DEC DE
    DEC DE
    NEXT
END-CODE

CODE 2* ( w1 --- w2)
\G Multiply w1 by 2.
    EX DE, HL
    ADD HL, HL
    EX DE, HL
    NEXT
END-CODE

CODE 2/ ( n1 --- n2)
\G Divide signed number n1 by 2.
    LD -3 (IX+), DE  \ Store 24-bit value below return stack
    SRA -1 (IX+)     \ shift MSB right
    LD DE, -3 (IX+)  \ Reload value
    RR D             \ shift the other two bytes
    RR E
    NEXT
END-CODE


\ The next few words manipulate addresses in a system-independent way.
\ Use CHAR+ instead of 1+ and it will be portable to systems where you
\ have to add something different from 1.

CODE CHAR+ ( c-addr1 --- c-addr2)
\G c-addr2 is the next character address after c-addr1.
    INC DE
    NEXT
END-CODE

CODE CHAR- ( c-addr1 --- c-addr2)
\G c-addr2 is the previous character address before c-addr1.
    DEC DE
    NEXT
END-CODE

CODE CHARS ( n1 --- n2)
\G n2 is the number of address units occupied by n1 characters.
    NEXT
END-CODE    

CODE CELL+ ( a-addr1 --- a-addr2)
\G a-addr2 is the address of the next cell after a-addr2.
    INC DE
    INC DE
    INC DE
    NEXT
END-CODE

CODE CELL- ( a-addr1 --- a-addr2)
\G a-addr2 is the address of the previous cell before a-addr1.
    DEC DE
    DEC DE
    DEC DE
    NEXT
END-CODE

CODE CELLS ( n2 --- n1) 
\G n2 is the number of address units occupied by n1 cells.
    LD HL, 0
    ADD HL, DE
    ADD HL, HL
    ADD HL, DE
    EX DE, HL
    NEXT
END-CODE


CODE ALIGNED ( c-addr --- a-addr )
\G a-addr is the first aligned address after c-addr.
   NEXT
END-CODE

CODE D+ ( d1 d2 --- d3)
\G Add the double numbers d1 and d2.
    EXX
    POP HL
    POP DE
    POP BC
    ADD HL, BC
    PUSH HL
    PUSH DE
    EXX
    POP HL
    ADC HL, DE
    EX DE, HL
    NEXT
END-CODE

CODE DNEGATE ( d1 --- d2)
\G Negate the top double number on the stack.
    POP BC
    LD HL, 0
    AND A
    SBC HL, BC
    PUSH HL
    LD HL, 0
    SBC HL, DE
    EX DE, HL
    NEXT
END-CODE

CODE LSHIFT ( x1 u --- x2)
\G Shift x1 left by u bits, zeros are added to the right.
    POP HL
    LD B, E
    LD A, E
    AND A
    0<> IF
	BEGIN
	    ADD HL, HL
	B--0= UNTIL
    THEN
    EX DE, HL
    NEXT
END-CODE

CODE RSHIFT ( x1 u --- x2)
\G Shift x1 right by u bits, zeros are added to the left.
    POP HL
    LD -3 (IX+), HL
    LD B, E
    LD A, E
    AND A
    0<> IF
	BEGIN
	    SRL -1 (IX+)
	    RR  -2 (IX+)
	    RR  -3 (IX+)
	B--0= UNTIL
    THEN
    LD DE, -3 (IX+)
    NEXT
END-CODE

CODE DROP ( x --- )
\G Discard the top item on the stack.        
    POP DE
    NEXT
END-CODE

CODE DUP   ( x --- x x )
\G Duplicate the top cell on the stack.    
    PUSH DE
    NEXT
END-CODE

CODE SWAP  ( n1 n2 --- n2 n1)
\G Swap the two top items on the stack.
    POP HL
    PUSH DE
    EX DE, HL
    NEXT
END-CODE

CODE OVER  ( x1 x2 --- x1 x2 x1)
\G Copy the second cell of the stack. 
    POP HL
    PUSH HL
    PUSH DE
    EX DE, HL
    NEXT
END-CODE

CODE ROT ( x1 x2 x3 --- x2 x3 x1)
\G Rotate the three top items on the stack.  
    POP HL
    EX (SP), HL
    PUSH DE
    EX DE, HL
    NEXT
END-CODE

CODE -ROT ( x1 x2 x3 --- x3 x1 x2)
\G Rotate the three top items on the stack reverse direction compared to ROT.  
    EX DE, HL
    POP DE
    EX (SP), HL
    PUSH HL
    NEXT
END-CODE

CODE 2DROP ( d ---)
\G Discard the top double number on the stack.
    POP DE
    POP DE
    NEXT
END-CODE

CODE 2DUP ( d --- d d )
\G Duplicate the top cell on the stack, but only if it is nonzero.
    POP HL
    PUSH HL
    PUSH DE
    PUSH HL
    NEXT
END-CODE

CODE 2SWAP ( d1 d2 --- d2 d1)
\G Swap the top two double numbers on the stack.
    EXX
    POP HL
    EXX
    POP HL
    EXX
    EX (SP), HL
    EXX
    PUSH DE
    EXX
    PUSH HL
    EXX
    EX DE, HL
    NEXT
END-CODE

CODE 2OVER ( d1 d2 --- d1 d2 d1)
\G Take a copy of the second double number of the stack and push it on the 
\G stack.
    EXX
    POP DE
    POP BC
    POP HL
    PUSH HL
    PUSH BC
    PUSH DE
    EXX
    PUSH DE
    EXX
    PUSH HL
    PUSH BC
    EXX
    POP DE
    NEXT
END-CODE

CODE PICK ( u --- x)
\G place a copy of stack cell number u on the stack. 0 PICK is DUP, 1 PICK
\G is OVER etc.
    LD HL, 0
    ADD HL, DE
    ADD HL, HL
    ADD HL, DE
    ADD HL, SP
    LD DE, (HL)
    NEXT
END-CODE

CODE ROLL ( u ---)
\G  Move stack cell number u to the top. 1 ROLL is SWAP, 2 ROLL is ROT etc.
    PUSH DE
    EXX
    EX (SP), HL
    INC HL
    EX DE, HL
    LD HL, 0
    ADD HL, DE
    ADD HL, HL
    ADD HL, DE
    PUSH HL
    POP BC
    ADD HL, SP
    LD DE, (HL)
    INC HL
    INC HL
    PUSH DE
    PUSH HL
    POP DE
    DEC HL
    DEC HL
    DEC HL
    LDDR
    POP HL
    POP BC
    EX (SP), HL
    EXX
    POP DE
    NEXT
END-CODE

CODE C@ ( c-addr --- c)
\G Fetch character c at c-addr.
    LD A, (DE)
    LD DE, 0
    LD E, A
    NEXT
END-CODE

CODE @ ( a-addr --- x)
\G Fetch cell x at a-addr.
    EX DE, HL
    LD DE, (HL)
    NEXT
END-CODE

CODE H@ ( a-addr --- x)
\G fetch 16-bit value x at a-addr.
    EX DE, HL
    LD DE, 0
    LD E, (HL)
    INC HL
    LD D, (HL)
    NEXT
END-CODE    

CODE C! ( c c-addr ---)
\G Store character c at c-addr
    EX DE, HL
    POP DE
    LD (HL), E
    POP DE
    NEXT
END-CODE

CODE ! ( x a-addr ---)
\G Store cell x at a-addr
    EX DE, HL
    POP DE
    LD (HL), DE
    POP DE
    NEXT
END-CODE

CODE H! ( x a-addr ---)
\G store 16-bit value x at a-addr
    EX DE, HL
    LD (HL), E
    INC HL
    LD (HL), D
    POP DE
    NEXT
END-CODE

CODE +! ( w a-addr ---)
\G Add w to the contents of the cell at a-addr.
    EX DE, HL
    LD DE, (HL)
    EX DE, HL
    POP BC
    ADD HL, BC
    EX DE, HL
    LD (HL), DE
    POP DE
    NEXT
END-CODE

CODE 2@ ( a-addr --- d)
\G Fetch double number d at a-addr.
    EX DE, HL
    LD DE, (HL)
    INC HL
    INC HL
    INC HL
    LD BC, (HL)
    PUSH BC
    NEXT
END-CODE

CODE 2! ( d a-addr ---)
\G Store the double number d at a-addr.
    EX DE, HL
    POP DE
    LD (HL), DE
    INC HL
    INC HL
    INC HL
    POP DE
    LD (HL), DE
    POP DE
    NEXT
END-CODE

CODE P@ ( p-addr --- c)
\G Read byte c from I/O address p-addr
    LD C, E
    LD B, D
    LD DE, 0
    IN E, (C)
    NEXT
END-CODE

CODE P! ( c p-addr ---)
\G Write byte c to I/O address p-addr   
    LD C, E
    LD B, D
    POP DE
    OUT (C), E
    POP DE
    NEXT
END-CODE
    
CODE SYSVARS ( --- addr)
\G Obtain the address of the system variables.
    PUSH DE    
    PUSH IX         
    LD A, $8
    RST .LIL $8
    PUSH IX         \ Push address 
    POP DE
    POP IX          \ Restore RSP
    NEXT
END-CODE    

LABEL YES       \ Store a true flag on stack.
 DEC DE         \ Change false to true flag.
 NEXT
ENDASM

CODE 0= ( x --- f)
\G f is true if and only if x is 0.
    LD HL, $FFFFFF
    ADD HL, DE
    LD DE, 0   \ False value.
    JR NC, YES
    NEXT
END-CODE

CODE 0< ( n --- f)
\G f is true if and only if n is less than 0.
    EX DE, HL
    ADD HL, HL
    LD DE, 0
    JR C, YES
    NEXT
END-CODE


LABEL LESS_OVF \ overflow detected, less if not negative
    JP P, YES
    NEXT
ENDASM

CODE < ( n1 n2 --- f)
\G f is true if and only if signed number n1 is less than n2. 
    POP HL
    AND A
    SBC HL, DE \ Subtract n1-n2
    LD DE, 0  \ False result to TOS
    JP PE, LESS_OVF
    JP M, YES \ No overflow, less if negative
    NEXT
END-CODE

CODE U< ( u1 u2 --- f)
\G f is true if and only if unsigned number u1 is less than u2.   
    POP HL
    AND A
    SBC HL, DE \ Subtract u1-u2
    LD DE, 0
    JR C, YES
    NEXT
END-CODE

CODE = ( x1 x2 --- f)
\G f is true if and only if x1 is equal to x2.
    POP HL
    AND A
    SBC HL, DE
    LD DE, 0
    JP Z, YES
    NEXT
END-CODE

CODE INVERT ( x1 --- x2)
\G Invert all the bits of x1 (one's complement)
    LD HL, 0
    AND A
    SBC HL, DE \ Negate DE
    EX DE, HL
    DEC DE     \ And decrement it.
    NEXT
END-CODE    
    
CODE CMOVE ( c-addr1 c-addr2 u ---)
\G Copy u bytes starting at c-addr1 to c-addr2, proceeding in ascending
\G order.
    PUSH DE
    POP BC
    LD HL, $FFFFFF
    ADD HL, BC
    POP DE
    POP HL
    U< IF
	LDIR
    THEN
    POP DE
    NEXT
END-CODE

CODE CMOVE> ( c-addr1 c-addr2 u ---)
\G Copy a block of u bytes starting at c-addr1 to c-addr2, proceeding in
\G descending order.
    PUSH DE
    POP BC
    LD HL, $FFFFFF
    ADD HL, BC
    POP DE
    POP HL
    U< IF
	ADD HL, BC
	DEC HL
	EX DE, HL
	ADD HL, BC
	DEC HL
	EX DE, HL
	LDDR
    THEN
    POP DE
    NEXT
END-CODE

CODE FILL ( c-addr u c ---)
\G Fill a block of u bytes starting at c-addr with character c.
    LD A, E
    POP BC
    LD HL, $FFFFFF
    ADD HL, BC 
    POP DE
    U< IF
	LD (DE), A \ Write first byte
	DEC BC
	LD HL, $FFFFFF
	ADD HL, BC
	U< IF
	    LD HL, 0
	    ADD HL, DE
	    INC DE
	    LDIR \ Copy byte to next lcoation repeatedly.
	THEN
    THEN
    POP DE
    NEXT
END-CODE

CODE (FIND) ( c-addr u nfa  --- cfa/word f )
\G find the string at c-addr, length u in the dictionary starting at nfa.
\G Search in a single hash chain. Return the cfa of the found word. If
\G If the word is not found, return the string addres instead. Flag values:
\G 0 for not found, 1 for immediate word, -1 for normal word.   
    PUSH DE
    EXX
    POP DE
    POP BC     \ Get string length in C
    PUSH DE
    BEGIN
	POP DE  \ nfa in DE
	POP HL  \ string start in HL
	PUSH HL
	PUSH DE
	LD A, (DE) \ Get count byte at nfa
	INC DE
	AND $1F    \ remove flag bits
	CP C      \ Compare with string length.
	0= IF     \ count bytes match
	    LD B, A
	    LABEL CMPLOOP
	    LD A, (DE)
	    INC DE
	    CP (HL)
	    INC HL
	    0= IF
		DJNZ CMPLOOP
		\ Got here, word is found
		EX DE, HL
		POP DE
		POP BC
		LD A, (DE)
		AND $40 \ Check immedate flag
		0= IF
		    LD BC, $FFFFFF
		ELSE
		    LD BC, $000001
		THEN
		PUSH HL
		PUSH BC
		EXX
		POP DE
		NEXT
	    THEN
	THEN
	POP HL
	DEC HL
	DEC HL
	DEC HL
	LD DE, (HL)
	PUSH DE
	LD HL, 0
	OR A
	SBC HL, DE
    0= UNTIL
    EXX             \ Not found, zero value already on stack.
    POP DE
    NEXT
END-CODE

CODE SCAN ( c-addr1 u1 c --- c-addr2 u2 )
\G Find the first occurrence of character c in the string c-addr1 u1
\G c-addr2 u2 is the remaining part of the string starting with that char.
\G It is a zero-length string if c was not found.
    LD A, E     \ character to A
    EXX         \ Use shadow registers
    POP BC      \ Get length
    POP HL      \ and address
    LD E, A     \ character to E
    LD A, B    
    OR C        \ Test for zero length.
    0<> IF
	LD A, E \ char back to A.
	CPIR    \ Use CPIR to find mactching character
	0= IF
	    INC BC \ Match found, go back one character.
	    DEC HL 
	THEN
    THEN
LABEL SCANDONE
    PUSH HL   
    PUSH BC       \ Push address and length
    EXX           \ Back to normal registers
    POP DE        \ Get length into TOS
    NEXT    
END-CODE

CODE SKIP ( c-addr1 u1 c --- c-addr2 u2 )
\G Find the first character not equal to c in the string c-addr1 u1
\G c-addr2 u2 is the remaining part of the string starting with the
\G nonmatching char. It is a zero-length string if no other chars found.
    LD A, E     \ character to A
    EXX         \ Use shadow registers
    POP BC      \ Get length
    POP HL      \ and address
    LD E, A     \ character to E
    LD A, B    
    OR C        \ Test for zero length.
    0<> IF
	LD A, E \ char back to A.  
	LABEL SKIPLOOP
	CPI     \ Test character pointed to by HL
	0= IF
	    JP PE, SKIPLOOP \ If still matching test parity flag to determine
	    \ if we can search further.
	    JR SCANDONE \ No non-matching character found.
	THEN
	INC BC  \ Back 1 character up.
	DEC HL
    THEN
    PUSH HL     
    PUSH BC     \ Push address and length.
    EXX         \ Back to normal registers
    POP DE      \ Get length into TOS
    NEXT
END-CODE

CODE NOOP ( --- )
\G No operation    
    NEXT
END-CODE    

END-CROSS
