\ EZ80 assembler
\ Copyright 2022 L.C. Benschop, The Netherlands.
\ This program is released under the GNU General Public License v3

BASE @ HEX

: DEFER ( "ccc" --- )
\G Defining word to cretate a word that can be made to execute an arbitrary
\G Forth word.    
    CREATE 0 , DOES> @ EXECUTE ;
: IS  ( xt "ccc" --- )
\G Set the word to be executed by the deferred word.    
    ' >BODY ! ;

\G Vocabulary containing the ASSEMBLER words.
VOCABULARY ASSEMBLER ( --- )
ASSEMBLER ALSO DEFINITIONS

' C! DEFER VC! IS VC! \ Vectorize the important words so we can cross
' C@ DEFER VC@ IS VC@ \ assemble and self-assemble using the same code.
' !  DEFER V!  IS V!
' @  DEFER V@  IS V@
' C, DEFER C_ IS C_
' ,  DEFER ,  IS ,
' HERE DEFER HERE IS HERE
' ALLOT DEFER ALLOT IS ALLOT
: C, C_ ;


: VH! ( w addr ---)
\ Store 16-bit value.
    2DUP VC! SWAP 8 RSHIFT SWAP 1+ VC! ;

VARIABLE ADLFLAG

\ Used in IF/THEN/ELSE/BEGIN/UNTIL/WHILE/REPEAT/AGAIN commands.
: <MARK ( --- addr )
  HERE ;
: <RESOLVE ( addr ---)
  HERE 1+ - C, ;
: >MARK ( --- addr )
  HERE 0 C, ;
: >RESOLVE ( addr --- )
  HERE OVER 1+ - SWAP VC! ;

\ Used in @IF/@THEN/@ELSE/@BEGIN/@UNTIL/@WHILE/@REPEAT/@AGAIN commands.
: <@MARK ( --- addr )
  HERE ;
: <@RESOLVE ( addr ---)
  , ADLFLAG @ 0= IF -1 ALLOT THEN ;
: >@MARK ( --- addr )
  HERE 0 , ADLFLAG @ 0= IF -1 ALLOT THEN ;
: >@RESOLVE ( addr --- )
  SWAP ADLFLAG @ IF V! ELSE VH! THEN ;


\ This is a prefix assembler, so the source code looks much like
\ traditional assembler source, for example INC HL or LD DE, $4444
\ It is inspired by the 8086 assembler that came with F-PC under MS-DOS.
\ In 1991-1994 I created such an assembler for the 6809.
\ In 2022 I ported the same concept to the Z80.
\ The Z80 is a very un-orthogonal instruction set. For example the
\ opcodes for the LD mnemonic are literally all over the place.
\ Therefore most instrucitons need a customized handler to compute the
\ opcode from the various operand fields. This is in the IHANDLER variable.
\ In 2022 I started a version for the EZ80, which has even more quirks.

\ Each opcode word sets some of the following variables. Words that
\ represent registers or other operands update some of these words as well.

\ The A; instruction finally assembles the instruction using thes variables.
VARIABLE SIZEPREF \ Size prefix .SIS .SIL .LIS or .LIL
VARIABLE IXBYTE \  0 when not used DD or FF when instruction needs a DD/FF
\ prefix. 1DD or 1FF when a displacement is also required.
\ 100 when no DD or FD prebyte, but a displacement is required. For example
\ the LEA/PEA instructions on the ED page.
VARIABLE DISPL  \ Displacement for IX instructions.
VARIABLE OPCODE  \ Opcode. FFFF when no instruction, 8-bit value for
                 \ single byte opcdoes, also $CBxx or $EDxx for 2 byte opcode
VARIABLE SRCREG   \ Source register or operand.
VARIABLE DSTREG   \ Destination register or operand/
VARIABLE IHANDLER \ Function to handle the instrruction.
VARIABLE ?OPERAND \ Type of operand on the stack 0 = none, 1 = 1 byte,
                  \ 2 = 2 byte, 3 = 3 byte, 4 = 1 byte program counter relative.

: NOINSTR \ Clear the instruction related variables.
    SIZEPREF OFF
    IXBYTE OFF ['] NOOP IHANDLER ! FFFF OPCODE ! ?OPERAND OFF
    SRCREG OFF DSTREG OFF
;
NOINSTR

\ Size suffixes
: .SIS 40 SIZEPREF ! ;
: .SIL 52 SIZEPREF ! ;
: .LIS 49 SIZEPREF ! ;
: .LIL 5B SIZEPREF ! ;
: .S ADLFLAG @ IF .SIL ELSE .SIS THEN ;
: .L ADLFLAG @ IF .LIL ELSE .LIS THEN ;
: .IS ADLFLAG @ IF .LIS ELSE .SIS THEN ;
: .IL ADLFLAG @ IF .LIL ELSE .SIL THEN ;

: ADJUST-OPSIZE
    ?OPERAND @ 2 =
      SIZEPREF @ 50 >
       ADLFLAG @ 0<> SIZEPREF @ 0= AND
      OR
    AND
    IF
	3 ?OPERAND !
    THEN
;

: ASSUME-ADL  ( f ---)
    ADLFLAG ! ;
1 ASSUME-ADL

: H. BASE @ HEX SWAP U. BASE ! ;
: A; \ Assemble current instruction and reset instruction variables
    IHANDLER @ EXECUTE
    SIZEPREF @ IF SIZEPREF @ C, THEN
    IXBYTE @ 0FF AND IF IXBYTE @ C, THEN
    OPCODE @ FFFF = IF EXIT THEN
    \ ." Assembling instruction " SIZEPREF @ H. IXBYTE @ H. OPCODE @ H. ?OPERAND @ H. SRCREG @ H. DSTREG @ H. CR
    OPCODE @ FF U> IF
	OPCODE @ 8 RSHIFT C, \ Assemble first byte CB or ED of opcode.
	IXBYTE @ 100 U> IF
	    DISPL @ C,  \ For CB opcodes displacement between CB and main opcode
	    0 IXBYTE !
	THEN	
    THEN
    OPCODE @ C, \ Set main opcode
    IXBYTE @ FF U> IF
	DISPL @ C,
    THEN
    ?OPERAND @ IF
	ADJUST-OPSIZE
	CASE ?OPERAND @
	    1 OF C, ENDOF \ 8 bit operand
	    2 OF , HERE H. -1 ALLOT HERE H. ." JJJ" CR ENDOF \ 16 bit operand
	    3 OF , ENDOF \ 24 bit operand
	    4 OF HERE 1+ - C, ENDOF \ relative address
	ENDCASE
    THEN
    NOINSTR
;


\ The LOCAL-LABELS array contains the administration for 9 local labels.
\ Each label occupies 4 cells:
\ Cell 0: Current value for backward references.
\ Cell 1: Link to forward references with relative branches.
\ Cell 2: link to forward references with 16-bit addresses.
\ Cell 3: link to forward references with 24-bit addresses.
CREATE LOCAL-LABELS 9 4 * CELLS ALLOT
LOCAL-LABELS 9 4 * CELLS ERASE

\ Create a back-reference to local label.
: LL-BACK CREATE 1- , DOES> @ 4 CELLS * LOCAL-LABELS + @ ;
1 LL-BACK 1B
2 LL-BACK 2B
3 LL-BACK 3B
4 LL-BACK 4B
5 LL-BACK 5B
6 LL-BACK 6B
7 LL-BACK 7B
8 LL-BACK 8B
9 LL-BACK 9B

\ Create a forward reference to local label.
: LL-FWD CREATE 1- , DOES> @ 4 CELLS * LOCAL-LABELS + 
    ?OPERAND @ 0= IF 2 ?OPERAND ! THEN
    ADJUST-OPSIZE
    ?OPERAND @ 4 = IF
	CELL+ DUP @ HERE 1+ ROT ! DUP 0= IF DROP HERE 1+ THEN
    ELSE
	?OPERAND @ CELLS  +  DUP @ HERE 1+ ROT !
    THEN
;
1 LL-FWD 1F
2 LL-FWD 2F
3 LL-FWD 3F
4 LL-FWD 4F
5 LL-FWD 5F
6 LL-FWD 6F
7 LL-FWD 7F
8 LL-FWD 8F
9 LL-FWD 9F

\ Define a local label and  resolve all forward references to it.
: LL-DEF CREATE 1- , DOES> >R A; R>
    @ 4 CELLS * LOCAL-LABELS +
    HERE OVER ! \ Set the label value for future back references
    \ Resolve the relative forward references
    DUP CELL+ @
    ?DUP IF
	BEGIN
	    DUP VC@ HERE 2 PICK - 1- 2 PICK VC!
	    DUP FF <> 
	WHILE
 	   -FF + +  
	REPEAT 2DROP
    THEN
    \ Resolve the 16-bit absolute forward references
    DUP 2 CELLS + @ 
    BEGIN
	?DUP
    WHILE
	    DUP V@ FFFF AND HERE FF0000 AND + HERE ROT VH!
    REPEAT 
    \ Resolve the 24-bit absolute forward references.
    DUP 3 CELLS + @
    BEGIN
	?DUP
    WHILE
	    DUP V@ HERE ROT V!
    REPEAT 
    CELL+ 3 CELLS ERASE \ Erase the pointers to the now resolved forward refs
;
1 LL-DEF 1:
2 LL-DEF 2:
3 LL-DEF 3:
4 LL-DEF 4:
5 LL-DEF 5:
6 LL-DEF 6:
7 LL-DEF 7:
8 LL-DEF 8:
9 LL-DEF 9:


\ Most instruction categories have both a customized IHANDLER
\ (to select the correct opcode for the operands) and a defining word.
\ Some instructions have a simple colon definition instead of the defining word.
\ Some instructions do not use the IHANDLER fuction.

\ Instructions with no operands.
: INS0
    CREATE , DOES> >R A; R> @ OPCODE ! ;
00 INS0 NOP
07 INS0 RLCA 0f INS0 RRCA
017 INS0 RLA  01F INS0 RRA
027 INS0 DAA  02F INS0 CPL
037 INS0 SCF  03F INS0 CCF
076 INS0 HALT 0D9 INS0 EXX
0F3 INS0 DI   0FB INS0 EI
ED44 INS0 NEG
ED45 INS0 RETN ED4D INS0 RETI
ED67 INS0 RRD  ED6F INS0 RLD
ED7D INS0 STMIX ED7E INS0 RSMIX
EDA0 INS0 LDI  EDA1 INS0 CPI
EDA2 INS0 INI  EDA3 INS0 OUTI
EDA8 INS0 LDD  EDA9 INS0 CPD
EDAA INS0 IND  EDAB INS0 OUTD
EDB0 INS0 LDIR EDB1 INS0 CPIR
EDB2 INS0 INIR EDB3 INS0 OTIR
EDB8 INS0 LDDR EDB9 INS0 CPDR
EDBA INS0 INDR EDBB INS0 OTDR
ED76 INS0 SLP
ED82 INS0 INIM ED83 INS0 OTIM
ED84 INS0 INI2
ED8A INS0 INDM ED8B INS0 OTDM
ED8C INS0 IND2
ED92 INS0 INIMR ED93 INS0 OTIMR
ED94 INS0 INI2R
ED9A INS0 INDMR ED9B INS0 OTDMR
ED0C INS0 IND2R
EDA4 INS0 OUTI2 EDAC INS0 OUTD2
EDB4 INS0 OTI2R EDBC INS0 OTD2R
EDC2 INS0 INIRX EDC3 INS0 OTIRX
EDCA INS0 INDRX EDCB INS0 OTDRX

\ INC and DEC instructions
: INCDEC-HANDLER
    SRCREG @ 1FF > IF
	OPCODE @ 8 RSHIFT SRCREG @ FF AND 10 * + OPCODE ! \ 16 bit
    ELSE
	OPCODE @ FF AND SRCREG @ FF AND 8 * +  OPCODE ! \ 8 bit
    THEN
;
: INCDEC
    CREATE ,
  DOES> >R A; R> @ OPCODE ! ['] INCDEC-HANDLER IHANDLER ! ;
0304 INCDEC INC
0B05 INCDEC DEC

\ PUSH and POP instructions
: PUSHPOP-HANDLER
    SRCREG @ FF AND 10 * OPCODE +! 
;
: PUSHPOP
    CREATE ,
  DOES> >R A; R> @ OPCODE ! ['] PUSHPOP-HANDLER IHANDLER ! ;
C1 PUSHPOP POP C5 PUSHPOP PUSH
ED4C PUSHPOP MLT

\ Rotate and shift instructions
: ROT-SHIFT-HANDLER
    SRCREG @ FF AND OPCODE +!
;
: ROT-SHIFT
    CREATE ,
    DOES> >R A; R> @ OPCODE ! ['] ROT-SHIFT-HANDLER IHANDLER ! ;
CB00 ROT-SHIFT RLC CB08 ROT-SHIFT RRC
CB10 ROT-SHIFT RL  CB18 ROT-SHIFT RR
CB20 ROT-SHIFT SLA CB28 ROT-SHIFT SRA
CB38 ROT-SHIFT SRL

\ Bit operations, SET. RES, BIT
: BITOP-HANDLER
      8 * \ bit number expected on stack
      SRCREG @ FF AND + OPCODE +! ;
: BITOP
    CREATE ,
    DOES> >R A; R> @ OPCODE ! ['] BITOP-HANDLER IHANDLER ! ;
CB40 BITOP BIT CB80 BITOP RES
CBC0 BITOP SET

\ Jump instructions JP, CALL, RET, JR
: JMP-HANDLER
    SRCREG @ 106 = IF
	0 ?OPERAND ! E9 OPCODE ! EXIT \ JP (HL)
    THEN
    ?OPERAND @ 0= IF SRCREG ELSE DSTREG THEN @
    DUP 101 = IF DROP 503 THEN
    \ Get proper value for C (not C register but Carry condition) 
    DUP 0= IF
	DROP OPCODE @ 8 RSHIFT OPCODE ! \ Non-conditional.
    ELSE
	FF AND 8 * OPCODE @ FF AND + OPCODE !
    THEN
;
\ Defining word takes  both opcode (for conditiona & nonconditonal)
\ and operand inputs
: JMPINST
    CREATE , ,
  DOES> >R A; R> DUP @ ?OPERAND ! CELL+ @ OPCODE ! ['] JMP-HANDLER IHANDLER ! ;
1820 4  JMPINST JR
C3C2 2  JMPINST JP
C9C0 0  JMPINST RET
CDC4 2  JMPINST CALL
\ Special 
: DJNZ A; 10 OPCODE ! 4 ?OPERAND !  ;

\ RST instruction.
: RST-HANDLER
   C7 + OPCODE !
;
: RST A; ['] RST-HANDLER IHANDLER !  0 OPCODE ! ;

\ IM instruction
: IM-HANDLER
    DUP 0 >  IF 1+ THEN 8 *  ED46 + OPCODE ! ;
: IM A; ['] IM-HANDLER IHANDLER !  0 OPCODE ! ;

\ IN instruction
: IN-HANDLER
    SRCREG @ 300 = IF
	DSTREG @ FF AND 8 * ED40 + OPCODE !
    ELSE
	1 ?OPERAND ! DB OPCODE !
    THEN ;
: IN A; ['] IN-HANDLER IHANDLER !  0 OPCODE ! ;
\ OUT instruction
: OUT-HANDLER
    DSTREG @ 300 = IF
	SRCREG @ FF AND 8 * ED41 + OPCODE !
    ELSE
	1 ?OPERAND ! D3 OPCODE !
    THEN ;

: OUT A; ['] OUT-HANDLER IHANDLER !  0 OPCODE ! ;

\ IN0 instruction
: IN0-HANDLER
    DSTREG 8 * ED00 + OPCODE ! ;
: IN0 A; ['] IN-HANDLER IHANDLER ! 0 OPCODE ! ;

\ OUT0 instruction
: OUT0-HANDLER
    SRCREG 8 * ED01 + OPCODE ! ;
: OUT0 A; ['] OUT0-HANDLER IHANDLER ! 0 OPCODE ! ;

\ TSTIO instruction
: TSTIO
    A; 1 ?OPERAND ! ED74 OPCODE ! ;
    
\ EX has only 3 single-byte opcodes, but they are unrelated to one another.
: EX-HANDLER
    CASE DSTREG @
	0201 OF 0EB OPCODE ! ENDOF \ EX DE. HL
	0302 OF 0E3 OPCODE ! ENDOF \ EX (SP), HL
	0203 OF 008 OPCODE ! ENDOF \ EX AF, AF'
    ENDCASE
;
: EX
    A; ['] EX-HANDLER IHANDLER !  0 OPCODE ! ;

: LEA-HANDLER
    DSTREG @ 701 = IF
	SRCREG @ 701 = IF ED32 ELSE ED54 THEN OPCODE !
    ELSE
	DSTREG @ 702 = IF
	    SRCREG @ 701 = IF ED55 ELSE ED33 THEN OPCODE !
	ELSE
	    SRCREG @ FF AND 1 + DSTREG @ EDFF AND 4 LSHIFT + OPCODE !
	THEN
    THEN
;

: LEA
    A; ['] LEA-HANDLER IHANDLER ! 0 OPCODE ! ;



: PEA-HANDLER
    SRCREG @ 0FF AND ED64 + OPCODE ! ;

: PEA
    A; ['] PEA-HANDLER IHANDLER ! 0 OPCODE ! ;

: TST-HANDLER
    SRCREG @ 0= IF
	ED64 OPCODE ! 1 ?OPERAND ! \ TST A, imm
    ELSE
	SRCREG @ 0FF AND 8 * ED04 + OPCODE !
    THEN 	
;

: TST
    A; ['] TST-HANDLER IHANDLER ! 0 OPCODE ! ;


\ The LD instruction is by far the most complex to handle.
: LD-HANDLER
    CASE DSTREG @ 8 RSHIFT
	01 OF \ Destination is 8-bit register incuding (HL).
	    CASE SRCREG @ 8 RSHIFT
		00 OF \ immediate
		    06 DSTREG @ FF AND 8 * + OPCODE ! 1 ?OPERAND !
		ENDOF
		01 OF \ 8 bit register to register
		    40 DSTREG @ FF AND 8 * + SRCREG @ FF AND + OPCODE !
		ENDOF
		02 OF \ LD (HL), rp or LD (IX/Y+), rp
		    CASE IXBYTE @
			0 OF SRCREG @ FF AND 10 * ED0F + OPCODE ! ENDOF \ LD (HL), rp
			0DD OF ED3F OPCODE ! 0 IXBYTE ! ENDOF \ LD (HL), IX
			0FD OF ED3E OPCODE ! 0 IXBYTE ! ENDOF \ LD (HL), IY
			1DD OF 0F SRCREG @ FF AND 10 * + OPCODE ! ENDOF \ LD (IX+d), rp
			1FD OF 0F SRCREG @ FF AND 10 * + OPCODE ! ENDOF \ LD (IY+d), rp
		    ENDCASE
		ENDOF
		03 OF \ LD A, (BC) or LD A, (DE)
		    SRCREG @ FF AND 10 * 0A + OPCODE !
		ENDOF
		04 OF \ LD A, direct
		    2 ?OPERAND ! 3A OPCODE !
		ENDOF
		06 OF \ LD A, I or LD A, R
		    SRCREG @ FF AND 8 * ED57 + OPCODE !
		ENDOF
		07 OF \ LD (IX/Y+), IX/IY
		    IXBYTE @ 1DD = IF
			SRCREG @ 701 = IF 03F ELSE 03E THEN OPCODE !
		    ELSE
			SRCREG @ 701 = IF 03E ELSE 03F THEN OPCODE !
		    THEN
                ENDOF 
		08 OF \ LD A, MB
		    ED6E OPCODE !
		ENDOF
	    ENDCASE
	ENDOF
	02 OF \ Destination is 16-bit register pair.
	    CASE SRCREG @ 8 RSHIFT 
		00 OF \ Immeditate
		    2 ?OPERAND !
		    DSTREG @ FF AND 10 * 01 + OPCODE !
		ENDOF
		01 OF \ LD rp, (HL)/(IX+)/(IY+)
		    CASE IXBYTE @
			0 OF DSTREG @ FF AND 10 * ED07 + OPCODE ! ENDOF \ LD rp, (HL)
			0DD OF ED37 OPCODE ! 0 IXBYTE ! ENDOF \ LD IX, (HL)
			0FD OF ED31 OPCODE ! 0 IXBYTE ! ENDOF \ LD IY, (HL)
			1DD OF 07 DSTREG @ FF AND 10 *  + OPCODE ! ENDOF \ LD rp, (IX+d)
			1FD OF 07 DSTREG @ FF AND 10 *  + OPCODE ! ENDOF \ LD rp, (IY+d)
		    ENDCASE
		ENDOF
		02 OF \ LD SP, HL
		    F9 OPCODE !
		ENDOF
		04 OF \ LD rp,(direct)
		    2 ?OPERAND !
		    DSTREG @ 202 = IF
			2A OPCODE ! \ HL
		    ELSE
			DSTREG @ FF AND 10 * ED4B + OPCODE !
		    THEN
		ENDOF
		06 OF \ LD HL, I
		    EDD7 OPCODE !
		ENDOF
	    ENDCASE
	ENDOF
	03 OF \ Destination is register pair (BC), (DE), only A as source 
		DSTREG @ FF AND 10 * 02 + OPCODE !
	ENDOF
	04 OF \ Destination is direct address
	    2 ?OPERAND !
	    SRCREG @ 0107 = IF
		32 OPCODE ! \ LD (direct), A
	    ELSE
		SRCREG @ 0202 = IF
		    22 OPCODE ! \ LD (direct), HL
		ELSE
		    SRCREG @ FF AND 10 * ED43 + OPCODE !
		THEN
	    THEN
	ENDOF
	06 OF \ Destination is I or R register
	    SRCREG @ 0202 = IF
		EDC7 OPCODE ! \ LD I,HL
	    ELSE
		DSTREG @ FF AND 8 * ED47 + OPCODE ! \ LD I, A or LD R, A
	    THEN
	ENDOF
	07 OF \ Destination is IX or IY, combined with (IX/iY+d)
	    IXBYTE @ 1DD = IF
		DSTREG @ 701 = IF 037 ELSE 031 THEN OPCODE !
	    ELSE
		DSTREG @ 701 = IF 031 ELSE 037 THEN OPCODE !
	    THEN
	ENDOF
	08 OF \ Destination is MB register
	    ED6D OPCODE ! \ LD MB. A
        ENDOF
    ENDCASE 
;
: LD
    A; ['] LD-HANDLER IHANDLER ! 0 OPCODE ! ;

: ADJUST-DEST \ Helper function to adjust destination register when combined
              \ with (IX+)/(IY+)
    IXBYTE @ DD = IF
	0701 DSTREG !
    THEN
    IXBYTE @ FD = IF
	0702 DSTREG !
    THEN ;

\ Arithmetic instructions
: ARITH-HANDLER
    DSTREG @ 1FF > IF
	OPCODE @ 8 RSHIFT SRCREG @ FF AND  10 * + OPCODE ! \ HL,rp 16 bit ops
	OPCODE @ 03F > IF ED00 OPCODE +! THEN \ ADC and SBC
    ELSE
	SRCREG @ 0= IF
	 OPCODE @ FF AND 46  + OPCODE ! 1 ?OPERAND ! \ A, Immediate
	ELSE    
	    OPCODE @ FF AND SRCREG @ FF AND + OPCODE ! \ 8 bit A,Reg
	THEN
    THEN
;
: ARITH
    CREATE ,
  DOES> >R A; R> @ OPCODE ! ['] ARITH-HANDLER IHANDLER ! ;
\ This instruction category comes last to prevent AND OR and XOR from
\ being shadowed by the assembler words.
0980 ARITH ADD 4A88 ARITH ADC
  90 ARITH SUB 4298 ARITH SBC
  A0 ARITH AND   A8 ARITH XOR
  B0 ARITH OR    B8 ARITH CP  


\ This definition of LABEL is only useful when cross-assemling/metacompiling.
\ In its current form supports no forward references.
\ When cross-assembling it contains a definition in the host dictionary only,
\ not in the target dictionary.
: LABEL A; HERE CONSTANT ;


\ The following small words define source/destination registers and
\ other operation modes (), and () for direct addressing.
\ Condition codes for jp/jr/call/ret are also included here.
\ These come at the end, primarily to prevent C, from being shadowed
\ by the assembler word.
: SRC-REG
    CREATE ,
  DOES> @ SRCREG ! ;
: DST-REG
    CREATE ,
  DOES> @ DSTREG ! ;

\ 0x01xx 8-bit regs + (HL)
0100 DST-REG B, 0100 SRC-REG B
0101 SRC-REG C
\ This is a dirty trick. When C, is called after an instruction it will
\ act like a DSTREG type word (sets destination register to C).
\ Outside an instruction it acts as the regular C, word (allot and store 1 byte)
: C, OPCODE @ FFFF  = IF C, ELSE 0101 DSTREG ! THEN ;
0102 DST-REG D, 0102 SRC-REG D
0103 DST-REG E, 0103 SRC-REG E
0104 DST-REG H, 0104 SRC-REG H
0105 DST-REG L, 0105 SRC-REG L
0106 DST-REG (HL), 0106 SRC-REG (HL)
0107 DST-REG A, 0107 SRC-REG A
\ $02xx 16 bit registers (pairs)
0200 DST-REG BC, 0200 SRC-REG BC
0201 DST-REG DE, 0201 SRC-REG DE
0202 DST-REG HL, 0202 SRC-REG HL
0203 DST-REG SP, 0203 SRC-REG SP
0203 DST-REG AF, 0203 SRC-REG AF
0203 SRC-REG AF' \ Only used in EX AF, AF'
\ $03xx indirect via 16-bit register pair.
0300 DST-REG (BC), 0300 SRC-REG (BC)
0301 DST-REG (DE), 0301 SRC-REG (DE)
0300 DST-REG (C), 300 SRC-REG (C) \ For IN and OUT
0302 DST-REG (SP), \ Only used in EX (SP), HL
0500 DST-REG NZ, 0500 SRC-REG NZ
\ $05xx jump conditon codes.
0501 DST-REG Z,  0501 SRC-REG Z
0502 DST-REG NC, 0502 SRC-REG NC
\ JP, CALL, JR and RET convert 0101 value for C, C to 0503
0504 DST-REG PO, 0504 SRC-REG PO
0505 DST-REG PE, 0505 SRC-REG PE
0506 DST-REG P,  0506 SRC-REG P
0507 DST-REG M,  0507 SRC-REG M
\ $06xx I and R registers.
0600 DST-REG I,  0600 SRC-REG I
0601 DST-REG R,  0601 SRC-REG R
\ $07xx IX+ and IY+ (without parentheses) in LEA/PEA, see later.
\ also used for IX/IY register when it is combined with (IX+)/(IY+)
\ $08xx MB register
0801 DST-REG MB, 0801 SRC-REG MB
\ $04xx direct addressing 
: (), 0400 DSTREG ! 2 ?OPERAND ! ; \ Direct addressing LD addr (), HL
: ()  0400 SRCREG ! 2 ?OPERAND ! ; \ Direct addressing LD A, addr ()
\ Some words to handle IX/IY registers.
: (IX+), 1DD IXBYTE ! DISPL ! (HL), ;
: (IY+), 1FD IXBYTE ! DISPL ! (HL), ;
: (IX+) ADJUST-DEST 1DD IXBYTE ! DISPL ! (HL) ;
: (IY+) ADJUST-DEST 1FD IXBYTE ! DISPL ! (HL) ;
: (IX) DD IXBYTE ! (HL) ; \ Only used with JP (IX)
: (IY) FD IXBYTE ! (HL) ; \ Only used with JP (IY)
: IX IXBYTE @ 0FF > IF 701 SRCREG ! ELSE DD IXBYTE ! HL THEN ;   
: IY IXBYTE @ 0FF > IF 702 SRCREG ! ELSE FD IXBYTE ! HL THEN ;   
: IX, DD IXBYTE ! HL, ;   
: IY, FD IXBYTE ! HL, ;
: IX+ ADJUST-DEST 100 IXBYTE ! 701 SRCREG ! DISPL ! ; \ Used in LEA/PEA
: IY+ ADJUST-DEST 100 IXBYTE ! 702 SRCREG ! DISPL ! ; \ Used in LEA/PEA
\ The eZ80 actually documents the IXH/IXL IYH/IYL registers.
: IXH, DD IXBYTE ! H, ; : IXH DD IXBYTE ! H ;
: IXL, DD IXBYTE ! L, ; : IXL DD IXBYTE ! L ;
: IYH, FD IXBYTE ! H, ; : IYH FD IXBYTE ! H ;
: IYL, FD IXBYTE ! L, ; : IYL FD IXBYTE ! L ;

\ Structured assembler constructs.
\ These use JR instruction. The only available condition codes here are:
\ 0= 0<> (for Z flag) and U< U>= (for carry flag)
\ and B--0= for decrement B and test for zero (DJNZ instruction).
: IF >R A; R> C_ >MARK ;
: THEN A; >RESOLVE ;
: ELSE A; 18 C_ >MARK SWAP >RESOLVE ;
: BEGIN A; <MARK ;
: UNTIL >R A; R> C_ <RESOLVE ;
: WHILE >R A; R> C_ >MARK ;
: REPEAT  A;  18 C_ SWAP <RESOLVE >RESOLVE ;
: AGAIN 18 UNTIL ;


\ Structured assembler constructs using long jumps.
\ We require these of the 0< 0>= VC VS PO and PE conditions or when
\ the distance is too long for JR
\ VS and VC are for parity/overflow flag.
\ 0< and 0>= are for sign flag.
: @IF >R A; R> 8 RSHIFT C_ >@MARK ;
: @THEN A; >@RESOLVE ;
: @ELSE A; C3 C_ >@MARK SWAP >@RESOLVE ;
: @BEGIN A; <@MARK ;
: @UNTIL >R A; R> 8 RSHIFT C_ <@RESOLVE ;
: @WHILE >R A; R> 8 RSHIFT C_ >@MARK ;
: @REPEAT A; C3 C, SWAP <@RESOLVE >@RESOLVE ;
: @AGAIN C300 @UNTIL ;
0010 CONSTANT B--0= 
D230 CONSTANT U<  DA38 CONSTANT U>=
C220 CONSTANT 0=  CA28 CONSTANT 0<>
E200 CONSTANT VS  EA00 CONSTANT VC
F200 CONSTANT 0<  FA00 CONSTANT 0>=


\ Register assignment:
\ DE is top of stack
\ IY is Instruction pointer (IP)
\ HL, BC and AF are free for scratch use, as are the shadow registers.
\ IX is return stack pointer, grows down.
\ SP is data stack pointer, grows down.

\ Forth is direct threading, the runtimes for non-code words always
\ contain a CALL instruction.

\ Next will be expanded in-line after every code word.
: NEXT
    LD HL, 0 (IY+) \ Get address at instruction pointer.
    LEA IY, 3 IY+  \ Increment instruction pointer
    JP (HL)
;

: ENDASM \ End assembly.
  A; PREVIOUS ;
FORTH DEFINITIONS
: ASSEMBLE ( ---)
\G Start assembling.
  ALSO ASSEMBLER NOINSTR ;

: CODE ( "ccc" --- )
\G Defining word to create a code definition.
    CREATE -4 ALLOT \ Remove the CALL made by CREATE.
    ASSEMBLE ;
: END-CODE ( --- )
\G Terminate a code definition
  [ ASSEMBLER ] ENDASM [ FORTH ] ;


PREVIOUS FORTH DEFINITIONS

BASE ! \ Restore the original base.
