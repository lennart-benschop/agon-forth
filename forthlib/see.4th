\ Decompiler for Agon FORTH.
\ copyleft (c) 2023 L.C. Benschop for Agon Forth.
\ license: GNU General Public License version 3, see LICENSE for more details.


' 1 1+ @ CONSTANT 'DOCON
' LOADLINE 1+ @ CONSTANT 'DOVAR
' FORGET 1+ @ CONSTANT 'DOCOL

: DECODE-CONSTANT ( xt ---)
    DUP >BODY @ . ." CONSTANT "  >NAME ID. 
;   

: DECODE-VARIABLE ( xt ---)
    ." VARIABLE " >NAME ID. 
;

: DECODE-CODE ( xt ---)
    ." CODE " >NAME ID.
;

: DECODE-BRANCH (  ip1 --- ip2 )
    DUP @ OVER - 1 CELLS / . CELL+ 
;

: DECODE-LIT ( ip1 --- ip2 )
    DUP @ . CELL+ 
;

: DECODE-STRING ( ip1 --- ip2)
    COUNT 2DUP TYPE '"' EMIT SPACE + 
;

: DECODE-POSTPONE ( ip1 --- ip2)
    DUP @ >NAME ID. CELL+ ;

: DECODE-THREAD ( ip ---)
    BEGIN 
	DUP @ ['] UNNEST <>
    WHILE
	    DUP @ DUP >NAME ID. 
	    SWAP CELL+ SWAP
	    DUP ['] LIT = IF
		DROP DECODE-LIT
	    ELSE
		DUP ['] BRANCH = OVER ['] ?BRANCH =  OR
		OVER ['] (?DO) = OR OVER ['] (LEAVE) = OR
		OVER ['] (LOOP) = OR OVER ['] (+LOOP) = OR
		IF
		    DROP DECODE-BRANCH
		ELSE
		    DUP ['] (.") = OVER ['] (S") = OR OVER ['] (ABORT") = OR
		    IF
			DROP DECODE-STRING
		    ELSE
			DUP ['] (POSTPONE) =
			IF
			    DROP DECODE-POSTPONE
			ELSE
			    DROP
			THEN
		    THEN
		THEN
	    THEN
    REPEAT
    DROP ." ; "
;

: DECODE-COLON ( xt ---)
    ." : " DUP >NAME DUP ID. SWAP 1+ CELL+ DECODE-THREAD
    C@ $40 AND IF ." IMMEDIATE" THEN
;

: DECODE-DOES ( xt ---)
    ." CREATE ... DOES> " 1+ @ 1+ CELL+ DECODE-THREAD
;


: SEE ( "ccc" ---)
\G Decompile the dictionary entry specified by "ccc" and show it.
    BASE @ >R    
    CR ' DUP
    C@ $CD <> IF
	DECODE-CODE
    ELSE
	DUP 1+ @ 'DOCON = IF
	    DECODE-CONSTANT
	ELSE
	    DUP 1+ @ 'DOVAR = IF
		DECODE-VARIABLE
	    ELSE
		DUP 1+ @ 'DOCOL = IF
		    DECODE-COLON
		ELSE
		    DECODE-DOES
		THEN
	    THEN
	THEN
    THEN
    CR R> BASE !	
;

