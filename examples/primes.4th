8190 CONSTANT SIZE

CREATE FLAGS SIZE 1+ ALLOT

: DO-PRIMES
  FLAGS SIZE 1+ 1 FILL
  0 SIZE 0
  DO FLAGS I + C@
     IF I DUP + 3 + DUP I +
          BEGIN DUP SIZE <=
	  WHILE 0 OVER FLAGS + C! OVER + REPEAT
	  DROP DROP 1+
     THEN
  LOOP
  . ." PRIMES" ;

: RUN-BENCHMARK 0 DO DO-PRIMES CR LOOP ;

