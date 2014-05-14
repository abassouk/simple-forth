VOCABULARY ARRAYS IMMEDIATE
ARRAYS DEFINITIONS

( COMPILE : ARN .... AR2 AR1 NUM ---
  EXECUTE : IN ...... I2 I1 ---  ADR )

: ARRAY
  <BUILDS
    DUP , 1 SWAP 0
    DO
      OVER , *
    LOOP
    2* 2*
    ALLOT
  DOES>
    DUP 4+ >R
    @ 0 SWAP 2* 2* 0 DO
      J I + @ * +
    4 +LOOP
    2* 2* R> DUP 4 - @ 2* 2* + +
  ;

forth definitions

