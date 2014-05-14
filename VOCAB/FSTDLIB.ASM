 	opt i+,w+,o1+
	incdir	"df0:assembler_headers/"
	include	"exec/types.i"

BPTR	MACRO
	LONG	\1
	Endm

	STRUCTURE	DATA,0
*** this is public data, won't change ***
	APTR	current	;voc.
	APTR	datastack	;data stackpointer
	APTR	dataend	;end of dataspointer
	APTR	returnstack ;return stackpointer
	APTR	returnend	;end of return SP
	APTR	fence	;pointer to the functions not affected by forget
	APTR	jumptab	;jumptable
	UBYTE	flags	;see down
	UBYTE	count1	;....
	UWORD	base	;numerical base
	APTR	dosbase	;dosbase
	APTR	create	;Word that is being currently created ,NULL otherwise
	BPTR	currentin	;current input
	BPTR	currentout	;current output
	APTR	context	;voc.
	APTR	voclist	;list of vocabularies(the dictionary)
*** here comes the private part ***
	APTR	extlist	;list of externs
	APTR	mem_buf	;current buffer for creating WORDS
	LONG	mem_length	;how many bytes are used
	LONG	mem_size	;size of buffer
	APTR	savestack	;stackpointer for quit
	APTR	fence_2	;internal functions protection
	BPTR	stdout	;std output
	BPTR	stdin	;std input
	BPTR	optfile	;file that names all included files
	UWORD	argc	;argument count
	APTR	argv	;* to array of * to arguments
	APTR	carg	;**string of current argument
	UWORD	thisarg	;number of this arg
	LONG	eolflag	;flag for printing out OK.
	APTR	ThisTask	;ptr to my task
	LABEL	SIZEOF_data ;Length of the structure

F_CHECKS	equ	1
F_COMPILE	EQU	2
F_COMPIMM	EQU	4
F_PROMPT	EQU	8

B_CHECKS	equ	0
B_COMPILE	EQU	1
B_COMPIMM	EQU	2
B_PROMPT	EQU	3

	STRUCTURE	word,0	;structure	of a word
	APTR	w_next	;points to next word
	LONG	w_size	;size of it(automatically calculated)
	UBYTE	w_flags	;flags of this Word (see down)
	UBYTE	w_length	;length of name
	LABEL	SIZEOF_word ;size so far..

* this is only the header. after it comes the name (padded with $00
* to word if necessary.then there is executable code who will be
* executed when this word is executed.(only if WB_SMUDGE is set)

WF_IMMEDIATE	equ	%00000001
WF_SMUDGE		equ	%00000010
WF_INTERN		equ	%00000100
WF_PRODCODE	equ	%00001000
WF_BINARY		EQU	%01000000
WF_VOCABULARY	equ	%10000000

WB_IMMEDIATE	equ	0
WB_SMUDGE		equ	1
WB_INTERN		equ	2
WB_PRODCODE	equ	3
WB_BINARY		EQU	6
WB_VOCABULARY	equ	7

	STRUCTURE	VOCAB,0		;vocabulary
	APTR	v_next		;next in the chain,previous in time
	APTR	v_name		;word with the name of the voc.
	STRUCT	v_word,SIZEOF_word	;the the header of words
	UWORD	v_pad		;the name of the header
	LABEL	SIZEOF_vocab

	STRUCTURE	extern,0
	APTR	e_next	;next in list
	APTR	e_first	;first function
	APTR	e_last	;last function
	APTR	e_init	;to call at load
	APTR	e_exit	;to call at exit
	APTR	e_name	;name of this extension
	LABEL	SIZEOF_ext

SETWORD	MACRO
	move.l	#$\1,-(DS)
	bsr	putword
	endm
SETLONG	MACRO
	move.l	#$\1,-(DS)
	bsr	putlong
	endm

DATASIZE	equ	4096
RETSIZE	equ	1024
BUF_DEFAULT equ	256

DS	equr	a5
RS	equr	a3
DP	equr	a4

	CODE
	dc.l	0
	dc.l	w_hex
	dc.l	w_16bit
	dc.l	init
	dc.l	0
	dc.l	name

name	dc.b	"Forth arithmetic words V1.0",0
	even

init	movem.l	a0-a6/d0-d7,-(a7)
	lea	tab(pc),a0
.loop	move.l	(a0)+,d0
	beq.s	.exit
	move.l	A0,-(A7)
	move.l	d0,a0
	move.l	jumptab(DP),a2
	jsr	8(a2)
	tst.l	d0
	beq.s	.err
	move.l	jumptab(DP),a2
	move.l	d0,a0
	jsr	12(a2)
	move.l	(a7)+,a0
	move.l	(a0)+,a1
	move.l	d0,2(a1)
	bra.s	.loop
.exit	movem.l	(a7)+,a0-a6/d0-d7
	lea	w_voc(pc),a0
	move.l	voclist(DP),(a0)
	move.l	a0,voclist(DP)
	move.l	current(DP),w_16bitudivmod
	moveq	#0,d1
	rts
.err	addq.l	#4,a7
	movem.l	(a7)+,a0-a6/d0-d7
	moveq	#0,d0
	moveq	#1,d1
	rts

n_putlong	dc.b	1,","
n_putword	dc.b	2,"W,",0

tab	dc.l	n_putlong,putlong
	dc.l	n_putword,putword
	dc.l	0

putlong	jmp	0.l
putword	jmp	0.l

w_hex	dc.l	w_immediate
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE
	dc.b	3,"HEX",0
	move.w	#16,base(DP)
	rts

w_immediate dc.l	w_immcode
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE
	dc.b	9,"IMMEDIATE",0
	move.l	current(DP),a0
	move.l	(a0),a0
	ori.b	#WF_IMMEDIATE,w_flags(A0)
	rts

w_immcode	dc.l	w_fflags
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE
	dc.b	7,"IMMCODE",0
	move.l	current(DP),a0
	move.l	(a0),a0
	ori.b	#WF_IMMEDIATE|WF_PRODCODE,w_flags(A0)
	rts

w_fflags	dc.l	w_rfrom
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE
	dc.b	5,"FLAGS",0
	lea	flags(DP),a0
	move.l	a0,-(DS)
	rts

w_rfrom	dc.l	w_tor
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_PRODCODE|WF_IMMEDIATE
	dc.b	2,"R>"
	move.l	#$2b1b,-(DS)
	bra	putword

w_tor	dc.l	w_rplain
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_PRODCODE|WF_IMMEDIATE
	dc.b	2,">R"
	move.l	#$271d,-(DS)
	bra	putword
	
w_rplain	dc.l	w_drop
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_PRODCODE|WF_IMMEDIATE
	dc.b	1,"R",0
	move.l	#$2b13,-(DS)
	bra	putword

w_drop	dc.l	w_dup
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_PRODCODE|WF_IMMEDIATE
	dc.b	4,"DROP"
	move.l	#$588d,-(DS)
	bra	putword

w_dup	dc.l	w_over
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_PRODCODE|WF_IMMEDIATE
	dc.b	3,"DUP",0
	move.l	#$2b15,-(DS)
	bra	putword

w_over	dc.l	w_swap
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_PRODCODE|WF_IMMEDIATE
	dc.b	4,"OVER"
c_over	SETLONG	2b2d0004
	rts

w_swap	dc.l	w_rot
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_PRODCODE|WF_IMMEDIATE
	dc.b	4,"SWAP"
	SETLONG	202D0004
	SETLONG	2B550004
	SETWORD	2A80
	RTS

w_rot	dc.l	w_2dup
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE
	dc.b	3,"ROT",0
	movem.l	(DS)+,d0-d2
	move.l	d1,-(DS)
	move.l	d0,-(DS)
	move.l	d2,-(DS)
	rts

w_2dup	dc.l	w_2drop
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	4,"2DUP"
	bsr.s	c_over
	bra.s	c_over

w_2drop	dc.l	w_lset
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	5,"2DROP",0
	SETWORD	508D
	RTS

w_lset	dc.l	w_wset
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	1,"!",0
	SETLONG	205D209D
	rts

w_wset	dc.l	w_cset
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	2,"W!"
	SETLONG	205D548D
	SETWORD	309D
	RTS

w_cset	dc.l	w_dec
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	2,"C!"
	SETLONG	205D568D
	SETWORD	109D
	RTS

w_dec	dc.l	w_lat
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE
	dc.b	3,"DEC",0
	move.w	#10,base(DP)
	rts

w_lat	dc.l	w_wat
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	1,"@",0
	SETLONG	20552A90
	RTS

w_wat	dc.l	w_cat
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	2,"W@"
	SETLONG	20557000
	SETLONG	30102A80
	RTS

w_cat	dc.l	w_add
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	2,"C@"
	SETLONG	20557000
	SETLONG	10102A80
	RTS

w_add	dc.l	w_sub
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	1,"+",0
	SETLONG	201DD195
	RTS

w_sub	dc.l	w_neg
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	1,"-",0
	SETLONG	201D9195
	RTS

w_neg	dc.l	w_mul
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	6,"NEGATE"
	SETWORD	4495
	RTS

w_mul	dc.l	w_div
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE
	dc.b	1,"*",0
	move.l	(DS)+,d0
	move.l	(DS),d1
	move.l	jumptab(DP),a1
	jsr	$14(a1)
	move.l	d0,(DS)
	rts

w_div	dc.l	w_udiv
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE
	dc.b	1,"/",0
	move.l	(DS)+,d1
	move.l	(DS),d0
	move.l	jumptab(DP),a1
	jsr	$18(a1)
	move.l	d0,(DS)
	rts

w_udiv	dc.l	w_mod
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE
	dc.b	2,"U/"
	move.l	(DS)+,d1
	move.l	(DS),d0
	move.l	jumptab(DP),a1
	jsr	$1c(a1)
	move.l	d0,(DS)
	rts

w_mod	dc.l	w_divmod
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE
	dc.b	3,"MOD",0
	move.l	(DS)+,d1
	move.l	(DS),d0
	move.l	jumptab(DP),a1
	jsr	$18(a1)
	move.l	d1,(DS)
	rts

w_divmod	dc.l	w_udivmod
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE
	dc.b	4,"/MOD"
	move.l	(DS)+,d1
	move.l	(DS),d0
	move.l	jumptab(DP),a1
	jsr	$18(a1)
	move.l	d1,(DS)
	move.l	d0,-(DS)
	rts

w_udivmod	dc.l	w_and
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE
	dc.b	5,"U/MOD",0
	move.l	(DS)+,d1
	move.l	(DS),d0
	move.l	jumptab(DP),a1
	jsr	$1c(a1)
	move.l	d1,(DS)
	move.l	d0,-(DS)
	rts

w_and	dc.l	w_or
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	3,"AND",0
	SETLONG	201DC195
	RTS

w_or	dc.l	w_eor
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	2,"OR"
	SETLONG	201D8195
	RTS

w_eor	dc.l	w_add1
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	3,"XOR",0
	SETLONG	201DB195
	RTS

w_add1	dc.l	w_add2
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	2,"1+"
	SETWORD	5295
	rts

w_add2	dc.l	w_sub1
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	2,"2+"
	SETWORD	5495
	rts

w_sub1	dc.l	w_sub2
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	2,"1-"
	SETWORD	5395
	rts

w_sub2	dc.l	w_div2
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	2,"2-"
	SETWORD	5595
	rts

w_div2	dc.l	w_mul2
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	2,"2/"
	SETLONG	2015e280
	SETWORD	2A80
	rts

w_mul2	dc.l	w_checkson
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	2,"2*"
	SETLONG	2015D195
	RTS

w_checkson	dc.l	w_checksoff
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	9,"CHECKS_ON",0
	ori.b	#F_CHECKS,flags(DP)
	rts

w_checksoff dc.l	w_equal
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	10,"CHECKS_OFF"
	andi.b	#~F_CHECKS,flags(DP)
	rts

w_equal	dc.l	w_gth
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	1,"=",0
	SETLONG	7000BB8D
	SETWORD	57C0
	SETLONG	44002B00
	RTS

w_gth	dc.l	w_lth
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	1,">",0
	SETLONG	7000BB8D
	SETWORD	5EC0
	SETLONG	44002B00
	RTS

w_lth	dc.l	w_iszero
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	1,"<",0
	SETLONG	7000BB8D
	SETWORD	5DC0
	SETLONG	44002B00
	RTS

w_iszero	dc.l	w_isminus
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	2,"=0"
	SETLONG	70004A9D
	SETWORD	57C0
	SETLONG	44002B00
	RTS

w_isminus	dc.l	w_isplus
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	2,"<0"
	SETLONG	70004A9D
	SETWORD	5BC0
	SETLONG	44002B00
	RTS

w_isplus	dc.l	w_16bit
	dc.l	0
	dc.b	WF_BINARY|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	2,">0"
	SETLONG	70004A9D
	SETWORD	52C0
	SETLONG	44002B00
	RTS

w_16bit	dc.l	0
	dc.l	0
	dc.b	WF_BINARY|WF_IMMEDIATE|WF_SMUDGE|WF_VOCABULARY
	dc.b	9,"16BITMATH",0
	jsr	exe
w_voc	dc.l	0
	dc.l	w_16bit
w_word	dc.l	w_16bitmul
	dc.l	0
	dc.l	$00012020
exe	move.l	(a7)+,d0
	addq.l	#8,d0
	move.l	d0,context(DP)
	rts

w_16bitmul	dc.l	w_16bitumul
	dc.l	0
	dc.b	WF_SMUDGE|WF_BINARY|WF_IMMEDIATE|WF_PRODCODE
	DC.B	1,"*",0
	SETLONG	4CDD0003
	SETLONG	C3C02b01
	RTS
w_16bitumul dc.l	w_16bitdiv
	dc.l	0
	dc.b	WF_SMUDGE|WF_BINARY|WF_IMMEDIATE|WF_PRODCODE
	DC.B	2,"U*"
	SETLONG	4CDD0003
	SETLONG	C2C02b01
	RTS

w_16bitdiv	dc.l	w_16bitudiv
	dc.l	0
	dc.b	WF_SMUDGE|WF_BINARY|WF_IMMEDIATE|WF_PRODCODE
	DC.B	1,"/",0
	SETLONG	221d201d
	SETLONG	81c148c0
	SETWORD	2B00
	RTS
w_16bitudiv dc.l	w_16bitmod
	dc.l	0
	dc.b	WF_SMUDGE|WF_BINARY|WF_IMMEDIATE|WF_PRODCODE
	DC.B	2,"U/"
	SETLONG	221d201d
	SETLONG	80c13b00
	SETWORD	4265

w_16bitmod dc.l	w_16bitumod
	dc.l	0
	dc.b	WF_SMUDGE|WF_BINARY|WF_IMMEDIATE|WF_PRODCODE
	DC.B	3,"MOD",0
	SETLONG	221d201d
	SETLONG	81c14840
	SETLONG	48c02b00
	RTS

w_16bitumod dc.l	w_16bitdivmod
	dc.l	0
	dc.b	WF_SMUDGE|WF_BINARY|WF_IMMEDIATE|WF_PRODCODE
	DC.B	4,"UMOD"
	SETLONG	221d201d
	SETLONG	80c14840
	SETLONG	3b004265
	RTS
w_16bitdivmod dc.l	w_16bitudivmod
	dc.l	0
	dc.b	WF_SMUDGE|WF_BINARY|WF_IMMEDIATE|WF_PRODCODE
	DC.B	4,"/MOD"
	SETLONG	221d201d
	SETLONG	81c12200
	SETLONG	48C04841
	SETLONG	48C12B01
	SETWORD	2B00
	RTS
w_16bitudivmod dc.l	0
	dc.l	0
	dc.b	WF_SMUDGE|WF_BINARY|WF_IMMEDIATE|WF_PRODCODE
	DC.B	5,"U/MOD",0
	SETLONG	221d201d
	SETLONG	80c14840
	SETLONG	3b004265
	SETLONG	48403B00
	SETWORD	4265
	RTS

	end
