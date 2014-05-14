 	opt i+,w+,o1+,m+

;	incdir	"df0:assembler_headers/"
	include	"types.i"
;	include	"libraries/dos_lib.i"

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

DATASIZE	equ	4096
RETSIZE	equ	1024
BUF_DEFAULT equ	256

DS	equr	a5
RS	equr	a3
DP	equr	a4

	CODE
	dc.l	0
	dc.l	w_vocab
	dc.l	w_vocab
	dc.l	init
	dc.l	ende
	dc.l	name

init	lea	mathname(pc),a1
	move.l	4.w,a6
	jsr	-408(a6)
	move.l	d0,_MathBase
	beq.s	.l1
	lea	w_voc(pc),a0
	move.l	voclist(DP),(a0)
	move.l	a0,voclist(DP)
	move.l	current(DP),w_SPFix
	moveq	#0,d1
	moveq	#0,d0
	rts
.l1	moveq	#8,d0
	moveq	#1,d1
	rts

ende	move.l	_MathBase(pc),a1
	move.l	4.w,a6
	jmp	-414(a6)

name	dc.b	"Forth FFP extension V1.0",0
mathname	dc.b	"mathffp.library",0
	even

w_vocab	dc.l	0
	dc.l	0
	dc.b	WF_IMMEDIATE|WF_SMUDGE|WF_BINARY|WF_VOCABULARY
	dc.b	3,"FFP",0
	jsr	execute
w_voc	dc.l	0
	dc.l	w_vocab
w_word	dc.l	w_SPCeil
	dc.l	0
	dc.b	WF_BINARY,0," "," "
execute	addq.l	#8,(a7)
	move.l	(a7)+,context(DP)
	rts

_MathBase	dc.l 0

w_SPFix: dc.l               0
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 5,"SPFIX"
 even
 move.l _MathBase(pc),a6
 move.l (a5)+,D0
 jsr -30(a6)
 move.l d0,-(a5)
 rts
w_SPFlt: dc.l               w_SPFix
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 5,"SPFLT"
 even
 move.l _MathBase(pc),a6
 move.l (a5)+,D0
 jsr -36(a6)
 move.l d0,-(a5)
 rts
w_SPCmp: dc.l               w_SPFlt
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 5,"SPCMP"
 even
 move.l _MathBase(pc),a6
 move.l (a5)+,D1
 move.l (a5)+,D0
 jsr -42(a6)
 move.l d0,-(a5)
 rts
w_SPTst: dc.l               w_SPCmp
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 5,"SPTST"
 even
 move.l _MathBase(pc),a6
 move.l (a5)+,D1
 jsr -48(a6)
 move.l d0,-(a5)
 rts
w_SPAbs: dc.l               w_SPTst
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 5,"SPABS"
 even
 move.l _MathBase(pc),a6
 move.l (a5)+,D0
 jsr -54(a6)
 move.l d0,-(a5)
 rts
w_SPNeg: dc.l               w_SPAbs
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 5,"SPNEG"
 even
 move.l _MathBase(pc),a6
 move.l (a5)+,D0
 jsr -60(a6)
 move.l d0,-(a5)
 rts
w_SPAdd: dc.l               w_SPNeg
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 5,"SPADD"
 even
 move.l _MathBase(pc),a6
 move.l (a5)+,D1
 move.l (a5)+,D0
 jsr -66(a6)
 move.l d0,-(a5)
 rts
w_SPSub: dc.l               w_SPAdd
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 5,"SPSUB"
 even
 move.l _MathBase(pc),a6
 move.l (a5)+,D1
 move.l (a5)+,D0
 jsr -72(a6)
 move.l d0,-(a5)
 rts
w_SPMul: dc.l               w_SPSub
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 5,"SPMUL"
 even
 move.l _MathBase(pc),a6
 move.l (a5)+,D1
 move.l (a5)+,D0
 jsr -78(a6)
 move.l d0,-(a5)
 rts
w_SPDiv: dc.l               w_SPMul
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 5,"SPDIV"
 even
 move.l _MathBase(pc),a6
 move.l (a5)+,D1
 move.l (a5)+,D0
 jsr -84(a6)
 move.l d0,-(a5)
 rts
w_SPFloor: dc.l             w_SPDiv
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 7,"SPFLOOR"
 even
 move.l _MathBase(pc),a6
 move.l (a5)+,D0
 jsr -90(a6)
 move.l d0,-(a5)
 rts
w_SPCeil: dc.l              w_SPFloor
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 6,"SPCEIL"
 even
 move.l _MathBase(pc),a6
 move.l (a5)+,D0
 jsr -96(a6)
 move.l d0,-(a5)
 rts

	end

