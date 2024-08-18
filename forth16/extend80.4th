\ Extensions to sod Forth kernel to make a complete Forth system.
\ created 1994 by L.C. Benschop.
\ copyleft (c) 1994-2014 by the sbc09 team, see AUTHORS for more details.
\ copyleft (c) 2022 L.C. Benschop for Cerberus 2080.
\ license: GNU General Public License version 3, see LICENSE for more details.

CAPS ON

: \G POSTPONE \ ; IMMEDIATE
\G comment till end of line for inclusion in glossary.

\ PART 1: MISCELLANEOUS WORDS.

: ?TERMINAL ( ---f)
\G Test whether the ESC key is pressed, return a flag.     
    KEY? IF KEY 27 = IF -1 ELSE KEY DROP 0 THEN  ELSE 0 THEN ;

: ERASE ( c-addr u )
\G Fill memory region of u bytes starting at c-addr with zero.    
    0 FILL ;

: <= ( n1 n2 --- f)
\G f is true if and only if n1 is less than or equal to n2.
  > 0= ;

: 0<= ( n1 --- f)
\G f is true if and only if n1 is less than zero.
  0 <= ;

: >=  ( n1 n2 ---f)
\G f is true if and only if n1 is greater than or equal to n2.    
  < 0= ;

: 0<> ( n1 n2 ---f)
\G f is true of and only of n1 and n2 are not equal.   
  0= 0= ;

: WITHIN ( u1 u2  u3 --- f)
\G f is true if u1 is greater or equal to u2 and less than u3
  2 PICK U> ROT ROT U< 0= AND ;

: -TRAILING ( c-addr1 u1 --- c-addr2 u2)
\G Adjust the length of the string such that trailing spaces are excluded.
  BEGIN
   2DUP + 1- C@ BL =
  WHILE
   1-
  REPEAT
;

: NIP ( x1 x2 --- x2)
\G Discard the second item on the stack.
  SWAP DROP ;

: TUCK ( x1 x2 --- x2 x1 x2 )
\G Copy the top of stack to a position under the second item.  
    SWAP OVER ;

: .(  ( "ccc<rparen>" ---)
\G Print the string up to the next right parenthesis.
   41 PARSE TYPE ;

\ PART 2: SEARCH ORDER WORDLIST

VARIABLE VOC-LINK ( --- a-addr)
FORTH-WORDLIST VOC-LINK !
\G Variable that links all vocabularies together, so we can link.

VARIABLE FENCE ( --- a-addr)
\G Address below which we are not allowed to forget.

: GET-ORDER ( --- w1 w2 ... wn n )
\G Return all wordlists in the search order, followed by the count.
  #ORDER @ 0 ?DO CONTEXT I CELLS + @ LOOP #ORDER @ ;

: SET-ORDER ( w1 w2 ... wn n --- )
\G Set the search order to the n wordlists given on the stack.
  #ORDER ! 0 #ORDER @ 1- DO CONTEXT I CELLS + ! -1 +LOOP ;

: ALSO ( --- )
\G Duplicate the last wordlist in the search order.
  CONTEXT #ORDER @ CELLS + DUP CELL- @ SWAP ! 1 #ORDER +! ;

: PREVIOUS ( --- )
\G Remove the last wordlist from search order.
   -1 #ORDER +! ;

VARIABLE #THREADS ( --- a-addr)
\G This variable holds the number of threads a word list will have.

: WORDLIST ( --- wid)
\G Make a new wordlist and give its address.
    HERE DUP VOC-LINK @ , VOC-LINK !
    #THREADS @ , #THREADS @ CELLS ALLOT HERE #THREADS @ CELLS -
    #THREADS @ CELLS ERASE ;


: DEFINITIONS  ( --- )
\G Set the definitions wordlist to the last wordlist in the search order.
CONTEXT #ORDER @ 1- CELLS + @ CURRENT ! ;

: FORTH ( --- )
\G REplace the last wordlist in the search order with FORTH-WORDLIST
  FORTH-WORDLIST CONTEXT #ORDER @ 1- CELLS + ! ;

1 #THREADS !
WORDLIST
CONSTANT ROOT-WORDLIST ( --- wid )
\G Minimal wordlist for ONLY

4 #THREADS !

: ONLY ( --- )
\G Set the search order to the minimal wordlist.
  1 #ORDER ! ROOT-WORDLIST CONTEXT ! ;

: VOCABULARY ( --- )
\G Make a definition that will replace the last word in the search order
\G by its wordlist.
  CREATE WORDLIST DROP          \ Make a new wordlist and store it in def.
  DOES> >R                      \ Replace last item in the search order.
  GET-ORDER SWAP DROP R> SWAP SET-ORDER ;

VARIABLE SOURCE-LINK
\G Variable that contains a link to all files included via RQUIRE

: FIND-SOURCEFILE ( c-addr u --- f)
    SOURCE-LINK @ >R
    BEGIN
         2DUP R@ CELL+ COUNT COMPARE R@ 0<> AND
    WHILE
	    R> @ >R
    REPEAT
    2DROP R> 0<> ;

: REQUIRED ( c-addr u ---)
\G Check that the file with the name c-addr u is already loaded and
\G load it only if it is not already loaded.    
    S" /forthlib/" OSSTRING >ASCIIZ \ Put path prefix into string buffer
    OSSTRING ASCIIZ> + >ASCIIZ \ Append file name.
    OSSTRING ASCIIZ> FIND-SOURCEFILE 0=
    IF
	HERE SOURCE-LINK @ , SOURCE-LINK !
	OSSTRING ASCIIZ> 2DUP HERE PLACE DUP 1+ ALLOT
	INCLUDED
    THEN
;

: REQUIRE  ( "ccc" --- )
\G Parse file name from source file and load that if not already loaded.    
  BL WORD COUNT REQUIRED ;  



\ Part 1A: Conditional compilation

: [IF] ( f ---)
\G If the flag is false, conditionally skip till the next [ELSE] or [THEN]
    0= IF
	BEGIN 
	    BEGIN
		BL WORD UPPERCASE? COUNT
		DUP WHILE
		    2DUP S" [ELSE]" COMPARE 0= IF
			2DROP NESTING @ 0= IF EXIT THEN
		    ELSE
			2DUP S" [THEN]" COMPARE 0= IF
			    2DROP NESTING @ 0= IF EXIT ELSE -1 NESTING +! THEN
			ELSE
			    S" [IF]" COMPARE 0= IF
				1 NESTING +!
			    THEN
			THEN
		    THEN	    
	    REPEAT
	    2DROP REFILL 0=
	UNTIL
	NESTING OFF
    THEN	
; IMMEDIATE

: [ELSE] ( --- )
    0 POSTPONE [IF] ; IMMEDIATE
\G Used in [IF] [ELSE] [THEN] for conditional compilation.    

: [THEN] ( --- )
\G Terminate [IF] [THEN] does nothing.
    ; IMMEDIATE

: [DEFINED] ( "ccc" --- f)
\G Produce a flag indicating whether the next word is defined.	
    BL WORD UPPERCASE? FIND SWAP DROP 0<> ; IMMEDIATE

[DEFINED] FULL-SRC [IF]
: (FORGET) ( xt ---)
\G Forget the word indicated by xt and everything defined after it.    
    >NAME CELL- DUP FENCE @ U< -6 ?THROW \ Check we are not below fence.
    >R \ Store new dictionary pointer to return stack.
    VOC-LINK @   
    BEGIN  \ Traverse all worlists
	DUP R@ U> IF
	    DUP @ VOC-LINK ! \ Wordlist entirely above new DP, remove it.
	ELSE
	    R@
	    OVER CELL+ @ 0 DO
	   	OVER I 2+ CELLS + CELL+
		BEGIN
	   	   CELL- @ DUP 2 PICK U<
		UNTIL
		2 PICK I 2+ CELLS + !
	    LOOP
	    DROP
	THEN
	@
	DUP 0=
    UNTIL DROP
    SOURCE-LINK 
    BEGIN
	@
	R@ OVER U< 0=
    UNTIL
    SOURCE-LINK ! \ Fix the list of loaded source files.
    R> DP ! \ Adjust dictionary pointer.
;

: FORGET ( "ccc" ---)
\G Remove word "ccc" from the dictionary, and anything defined later.
    32 WORD UPPERCASE? FIND 0=
    IF
	DROP \ Exit silently if word not found.
    ELSE
	(FORGET)
    THEN
;

: MARKER ( "ccc" --)
\G Create a word that when executeed forgets itself and everything defined
\G after it.
   CREATE DOES> 3 - (FORGET)    
;
[ELSE]
: MARKER ( "ccc" --- )
    BL WORD DROP 
;
: FORGET ( "ccc" --- )
    BL WORD DROP 
;
[THEN]    
: ENVIRONMENT? ( c-addr u --- false | val true)
\G Return an environmental query of the string c-addr u    
    2DROP 0 ;


\ PART 3: SOME UTILITIES, DUMP .S WORDS

[DEFINED] FULL-SRC [IF]
: DL ( addr1 --- addr2 )
\G hex/ascii dump in one line of 16 bytes at addr1 addr2 is addr1+16
  BASE @ >R 16 BASE ! CR
  DUP 0 <# # # # # #> TYPE ." : "
  16 0 DO
   DUP I + C@ 0 <# # # #> TYPE
  LOOP
  SPACE
  16 0 DO
   DUP I + C@ DUP 32 < OVER 127 = OR IF DROP ." ." ELSE EMIT THEN
  LOOP
  16 + R> BASE ! ;


: DUMP ( addr len --- )
\G Show a hex/ascii dump of the memory block of len bytes at addr
  7 + 4 RSHIFT 0 DO
   DL ?TERMINAL IF LEAVE THEN
  LOOP DROP CR ;

: H. ( u ----)
    BASE @ >R HEX U. R> BASE ! ;

: .S ( --- )
\G Show the contents of the stack.
     DEPTH IF
      0 DEPTH 2 - DO I PICK . -1 +LOOP
     ELSE ." Empty " THEN ;


: ID. ( nfa --- )
\G Show the name of the word with name field address nfa.
  COUNT 31 AND TYPE SPACE ;

: WORDS ( --- )
\G Show all words in the last wordlist of the search order.
    CONTEXT #ORDER @ 1- CELLS + @
    2+ DUP @ >R \ number of threads to return stack.
    2+ R@ 0 DO DUP I CELLS + @ SWAP LOOP DROP \ All thread pointers to stack.
    BEGIN
	0 0
	R@ 0 DO
	    I 2 + PICK OVER U> IF
		DROP DROP I I 1 + PICK
	    THEN
	LOOP \ Find the thread pointer with the highest address.
	?TERMINAL 0= AND
    WHILE
	    DUP 1+ PICK DUP ID. \ Print the name.
	    CELL- @             \ Link to previous.
	    SWAP 1+ CELLS SP@ + ! \ Update the right thread pointer.
    REPEAT
    DROP R> 0 DO DROP LOOP  \ Drop the thread pointers.
;
[THEN]

ROOT-WORDLIST CURRENT !
: FORTH FORTH ;
: ALSO ALSO ;
: ONLY ONLY ;
: PREVIOUS PREVIOUS ;
: DEFINITIONS DEFINITIONS ;
[DEFINED] FULL-SRC [IF]
: WORDS WORDS ;
[THEN]    
DEFINITIONS
\ Fill the ROOT wordlist.

\ PART 4: ERROR MESSAGES

: MESS" ( n "cccq" --- )
\G Create an error message for throw code n.
  , ERRORS @ , HERE 2 CELLS - ERRORS ! 34 WORD C@ 1+ ALLOT ;

-3 MESS" Stack overflow"
-4 MESS" Stack underflow"
-5 MESS" Dictionary full"
-6 MESS" Below fence"
-13 MESS" Undefined word"
-22 MESS" Incomplete control structure"
-37 MESS" File I/O error"
-38 MESS" File does not exist"
-39 MESS" Bad system command"
-40 MESS" Directory does not exist"

\ PART 5: Miscellaneous words

: 2CONSTANT  ( d --- )
\G Create a new definition that has the following runtime behavior.
\G Runtime: ( --- d) push the constant double number on the stack.
  CREATE HERE 2! 2 CELLS ALLOT DOES> 2@ ;

: D.R ( d n --- )
\G Print double number d right-justified in a field of width n.
  >R SWAP OVER DABS <# #S ROT SIGN #> R> OVER - 0 MAX SPACES TYPE ;

: U.R ( u n --- )
\G Print unsigned number u right-justified in a field of width n.
  >R 0 R> D.R ;

: .R ( n1 n2 --- )
\G Print number n1 right-justified in a field of width n2.
 >R S>D R> D.R ;


: VALUE ( n --- )
\G Create a variable that returns its value when executed, prefix it with TO
\G to change its value.    
  CREATE , DOES> @ ;

: TO ( n "ccc" ---)
\G Change the value of the following VALUE type word.    
  ' >BODY STATE @ IF
      POSTPONE LITERAL POSTPONE !
  ELSE
   !
  THEN
; IMMEDIATE

: D- ( d1 d2 --- d3)
\G subtract double numbers d2 from d1.    
  DNEGATE D+ ;

: D0= ( d ---f)
\G f is true if and only if d is equal to zero.    
  OR 0= ;

: D= ( d1 d1  --- f)
\G f is true if and only if d1 and d2 are equal.
  D- D0= ;

: BLANK ( c-addr u ----)
\G Fill the memory region of u bytes starting at c-addr with spaces.    
  32 FILL ;

: AGAIN ( x ---)
\G Terminate a loop forever BEGIN..AGAIN loop.    
  POSTPONE 0 POSTPONE UNTIL ; IMMEDIATE

: CASE ( --- )
\G Start a CASE..ENDCASE construct. Inside are one or more OF..ENDOF blocks.
\G runtime the CASE blocks takes one value from the stack and uses it to
\G select one OF..ENDOF block.    
  CSP @ SP@ CSP ! ; IMMEDIATE
: OF ( --- x)
\G Start an OF..ENDOF block. At runtime it pops a value from the stack and
\G executes the block if this value is equal to the CASE value.    
  POSTPONE OVER POSTPONE = POSTPONE IF POSTPONE DROP ; IMMEDIATE
: ENDOF ( x1 --- x2)
\G Terminate an OF..ENDOF block.    
  POSTPONE ELSE ; IMMEDIATE
: ENDCASE ( variable# ---)
\G Terminate a CASE..ENDCASE construct.     
  POSTPONE DROP BEGIN SP@ CSP @ - WHILE POSTPONE THEN REPEAT
  CSP ! ; IMMEDIATE

\ PART 6: File related words.

: BSAVE ( addr len "ccc" ---)
\G Save memory at address addr, length len bytes to a file
\G filename is the next word parsed.       
  BL WORD COUNT OSSTRING >ASCIIZ OSSTRING ROT ROT 2 OSCALL -37 ?THROW ;

: BLOAD ( addr len "ccc" ---)
\G Load a file in memory at address addr, filename is the next word parsed.    
   BL WORD COUNT OSSTRING >ASCIIZ OSSTRING ROT ROT 1 OSCALL -38 ?THROW ;

: DELETE ( "ccc"  --)
\G Delete the specified file.    
  BL WORD COUNT DELETE-FILE DROP ;

: CD ( "ccc"  --)
\G Go to the specified directory.    
  BL WORD COUNT OSSTRING >ASCIIZ OSSTRING 0 0 3 OSCALL -40 ?THROW ;

[DEFINED] FULL-SRC [IF]
: SYSTEM ( c-addr u ---)
\G Execute the specified system command.    
  CR OSSTRING >ASCIIZ OSSTRING 0 0 16 OSCALL -39 ?THROW ;  

: EDIT-FILE ( c-addr u lineno --- )
\G Invoke the system editor on the file whose name is specified by c-addr u
\G at the specified line number.    
    >R
    S" nano " OSSTRING >ASCIIZ \ put the editor name in the string buffer.
    OSSTRING ASCIIZ> + >ASCIIZ \ put the file name in the string buffer.
    S"  &90000 " OSSTRING ASCIIZ> + >ASCIIZ
    \ put additional editor parameter in string buffer (buffer address).
    R> 0 BASE @ >R DECIMAL <# #S #> OSSTRING ASCIIZ> + >ASCIIZ
    R> BASE ! \ Add line number to OS string.
    OSSTRING 0 0 16 OSCALL -39 ?THROW
    0 SYSVARS 5. D+ XC!
;

: ED ( --- )
\G Invoke the editor on the current file (selected with OPEN)   
    CURFILENAME C@ 0= -38 ?THROW
    CURFILENAME ASCIIZ> 1 EDIT-FILE ;


REQUIRE asmz80.4th
REQUIRE see.4th
[THEN]

: SAVE-SYSTEM ( "ccc"  --- )
\G Save the current FORTH system to the specifed file.    
    0 CURFILENAME C! \ Do not want stray current file here.
    0 HERE BSAVE ;

: TURNKEY ( xt "ccc" --- )
\G Save the current FORTH system is a way it automatically starts xt
\G when loaded and run.
  AT-STARTUP ! SAVE-SYSTEM ;    

: CAT ( ---)
    \G Show  the disk catalog
    CR S" ." OSSTRING >ASCIIZ OSSTRING 0 0 4 OSCALL DROP
;

: SYSVARS@ ( idx --- c)
\G Read value C from byte offset idx in the system variabled    
    SYSVARS -ROT + SWAP XC@ ;

: SYSVARS! ( c idx ---)
\G Store value C into byte offset idx in the system variabled    
    SYSVARS -ROT + SWAP XC! ;

: MS ( n --- )
\G Delay for n milliseconds.
   8 + 16 / 0 DO 
     0 SYSVARS@ BEGIN 
      DUP 0 SYSVARS@ <> \ Wait until system time changes (50 times per second)
     UNTIL DROP LOOP  ;


HERE FENCE !

[DEFINED] FULL-SRC [IF]
    DELETE forth.bin
    CR .( Saving system as forth.bin ) CR
    SAVE-SYSTEM forth.bin
[ELSE]
    DELETE cf.bin
    CR .( Saving system as cf.bin ) CR
    SAVE-SYSTEM cf.bin
[THEN]
BYE
