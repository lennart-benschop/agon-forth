\ PART 7: Miscellaneous words (@jackokring)

: <MARK ( --- addr )
\G Mark a backward reference.
  HERE ;

: <RESOLVE ( addr ---)
\G Resolve a backward reference.
  , ;

: >MARK ( --- addr )
\G Mark a forward reference.
  HERE 0 , ;

: >RESOLVE ( addr --- )
\G Resolve a forward reference.
  HERE SWAP ! ;

: NOT ( w1 --- f)
\G Return true flag is w1 == 0, else return false flag
    0= ; 

: 2R> ( R: d --- d)
\G Bring a double from the return stack.
    R> R> R> SWAP ROT >R ;
    
: 2>R ( d --- R: d)
\G Place a double on the return stack.
    R> -ROT SWAP >R >R >R ;
    
: 2R@ ( R: d --- d R: d)
\G Copy a double from the return stack.
    R> R@ R@ SWAP ROT >R ; 

