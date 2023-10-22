\ Example code definitions.

CODE 3* ( n1 --- n2)
\G Multiply a number by 3
  LD HL, 0
  ADD HL, DE
  ADD HL, HL   \ Times 2
  ADD HL, DE   \ Add original value, so times 3
  EX  DE, HL    \ Move result to TOS
  NEXT
END-CODE


CODE CLZ ( u --- n)
\G Count leading zeros.
   LD HL, 0     
   AND A
   ADC HL, DE   \ Test if TOS = 0 (and move to HL).
   0= IF
     LD DE, $18 \ If zero we have 24 leading zeros.
   ELSE
     LD DE, $FFFFFF \ Start at -1 as we increment 1 more than leading zeros.
     BEGIN
	ADD HL, HL \ Shift left
     	INC DE     \ Count one more
     U< UNTIL      \ Until the last bit shifted out is 1.
   THEN
   NEXT
END-CODE   
