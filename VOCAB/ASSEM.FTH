VOCABULARY ASSEMBLER IMMEDIATE
ASSEMBLER DEFINITIONS
DEC

0 VARIABLE INS
0 VARIABLE SIZE
0 VARIABLE FMODE
0 VARIABLE FREG
0 VARIABLE FEXTRA
0 VARIABLE TMODE
0 VARIABLE TREG
0 VARIABLE TEXTRA

: _D0 0 0 ; immediate
: _D1 1 0 ; immediate
: _D2 2 0 ; immediate
: _D3 3 0 ; immediate
: _D4 4 0 ; immediate
: _D5 5 0 ; immediate
: _D6 6 0 ; immediate
: _D7 7 0 ; immediate

: _A0 0 1 ; immediate
: _A1 1 1 ; immediate
: _A2 2 1 ; immediate
: _A3 3 1 ; immediate
: _A4 4 1 ; immediate
: _A5 5 1 ; immediate
: _A6 6 1 ; immediate
: _A7 7 1 ; immediate
: _SP 7 1 ; immediate

: FROM 2* 2* 2* + ;
: TO   SWAP FROM ;

: INDEX 1- >R FROM 2* R> + 2048 * 255 AND + ;

: _(AN)     1- if 8 error else 2 endif ; immediate
: _(AN)+    1- if 8 error else 3 endif ; immediate
: _-(AN)    1- if 8 error else 4 endif ; immediate
: _d(AN)    1- if 8 error else 5 endif ; immediate
: _D(AN,RX) 1- if 8 error else >R INDEX R> 6 endif ; immediate
: _$.W            0 7 ; IMMEDIATE
: _$.L            1 7 ; IMMEDIATE
: _D(PC)          2 7 ; IMMEDIATE
: _D(PC,RX) INDEX 3 7 ; IMMEDIATE
: _#              4 7 ; IMMEDIATE

: FROMPARAM
  2DUP FMODE ! FREG !
  2DUP FROM INS @ OR INS !
  DUP 7 - IF
    SWAP DROP 4 > IF FEXTRA ! ENDIF
  ELSE
    2DROP FEXTRA !
  ENDIF ;

: TOPARAM
  2DUP TMODE ! TREG !
  2DUP TO 64 * INS @ OR INS !
  DUP 7 - IF
    SWAP DROP 4 > IF TEXTRA ! ENDIF
  ELSE
    2DROP TEXTRA !
  ENDIF ;

: WRITE1
  INS @ W,
  FMODE @ DUP 7 - IF
    4 > IF FEXTRA @ W, ENDIF
  ELSE
    DROP FEXTRA @ FREG @ 
    DUP 4 = SIZE @ 2 = AND SWAP 1 = OR IF , ELSE W, ENDIF
  ENDIF ;
 
: WRITE2
  WRITE1
  TMODE @ DUP 7 - IF
    4 > IF TEXTRA @ W, ENDIF DROP
  ELSE
    DROP TEXTRA @ TREG @ 
    DUP 4 = SWAP 1 = OR IF , ELSE W, ENDIF
  ENDIF ;

: .L 2 ; IMMEDIATE
: .W 1 ; IMMEDIATE
: .B 0 ; IMMEDIATE

: WIDTH DUP SIZE ! 64 * INS @ OR INS ! ;

HEX

: ONEPARAM INS ! WIDTH FROMPARAM WRITE1 ;
: 2PARAM INS ! WIDTH
  DUP IF
    FROMPARAM TOPARAM WRITE2 
  ELSE
    TOPARAM DUP IF 100 INS @ OR INS ! ENDIF FROMPARAM WRITE2
  ENDIF ;

: TST 4A00 ONEPARAM ; IMMCODE
: JSR 4E00 ONEPARAM ; IMMCODE
: CLR 4200 ONEPARAM ; IMMCODE
: NOT 4600 ONEPARAM ; IMMCODE
: SWAP 4840 ONEPARAM ; IMMCODE
: MOVE DUP WIDTH ! DUP 2 AND IF DROP 2000 ELSE 2* 1+ 1000 * ENDIF INS !
  FROMPARAM TOPARAM WRITE2 ; IMMCODE
: ADD D000 2PARAM ; IMMCODE
: SUB 9000 2PARAM ; IMMCODE
: CMP B000 2PARAM ; IMMCODE
: AND C000 2PARAM ; IMMCODE
: OR  8000 2PARAM ; IMMCODE

( _a0 _[an] .l tst -> 4a90 W, )
( 5 _D1 .W _A0 _D[AN,RX] .L TST -> 4AB01005 , )

dec
FORTH DEFINITIONS
