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
	dc.l	w_SPLog10
	dc.l	w_SPAtan
	dc.l	init
	dc.l	end
	dc.l	name

init	lea	mathname(pc),a1
	move.l	4.w,a6
	jsr	-408(a6)
	move.l	d0,_MathTransBase
	beq.s	.l1
	moveq	#0,d1
	moveq	#0,d0
	rts
.l1	moveq	#8,d0
	moveq	#1,d1
	rts

end	move.l	_MathTransBase(pc),a1
	move.l	4.w,a6
	jmp	-414(a6)

name	dc.b	"Forth FFP extension V1.0(transcendal functions)",0
mathname	dc.b	"mathtrans.library",0
	even

_MathTransBase	dc.l 0
w_SPAtan: dc.l              0
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 6,"SPATAN"
 even
 move.l _MathTransBase(pc),a6
 move.l (a5)+,D0
 jsr -30(a6)
 move.l d0,-(a5)
 rts
w_SPSin: dc.l               w_SPAtan
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 5,"SPSIN"
 even
 move.l _MathTransBase(pc),a6
 move.l (a5)+,D0
 jsr -36(a6)
 move.l d0,-(a5)
 rts
w_SPCos: dc.l               w_SPSin
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 5,"SPCOS"
 even
 move.l _MathTransBase(pc),a6
 move.l (a5)+,D0
 jsr -42(a6)
 move.l d0,-(a5)
 rts
w_SPTan: dc.l               w_SPCos
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 5,"SPTAN"
 even
 move.l _MathTransBase(pc),a6
 move.l (a5)+,D0
 jsr -48(a6)
 move.l d0,-(a5)
 rts
w_SPSincos: dc.l            w_SPTan
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"SPSINCOS"
 even
 move.l _MathTransBase(pc),a6
 move.l (a5)+,D1
 move.l (a5)+,D0
 jsr -54(a6)
 move.l d0,-(a5)
 rts
w_SPSinh: dc.l              w_SPSincos
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 6,"SPSINH"
 even
 move.l _MathTransBase(pc),a6
 move.l (a5)+,D0
 jsr -60(a6)
 move.l d0,-(a5)
 rts
w_SPCosh: dc.l              w_SPSinh
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 6,"SPCOSH"
 even
 move.l _MathTransBase(pc),a6
 move.l (a5)+,D0
 jsr -66(a6)
 move.l d0,-(a5)
 rts
w_SPTanh: dc.l              w_SPCosh
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 6,"SPTANH"
 even
 move.l _MathTransBase(pc),a6
 move.l (a5)+,D0
 jsr -72(a6)
 move.l d0,-(a5)
 rts
w_SPExp: dc.l               w_SPTanh
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 5,"SPEXP"
 even
 move.l _MathTransBase(pc),a6
 move.l (a5)+,D0
 jsr -78(a6)
 move.l d0,-(a5)
 rts
w_SPLog: dc.l               w_SPExp
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 5,"SPLOG"
 even
 move.l _MathTransBase(pc),a6
 move.l (a5)+,D0
 jsr -84(a6)
 move.l d0,-(a5)
 rts
w_SPPow: dc.l               w_SPLog
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 5,"SPPOW"
 even
 move.l _MathTransBase(pc),a6
 move.l (a5)+,D1
 move.l (a5)+,D0
 jsr -90(a6)
 move.l d0,-(a5)
 rts
w_SPSqrt: dc.l              w_SPPow
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 6,"SPSQRT"
 even
 move.l _MathTransBase(pc),a6
 move.l (a5)+,D0
 jsr -96(a6)
 move.l d0,-(a5)
 rts
w_SPTieee: dc.l             w_SPSqrt
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 7,"SPTIEEE"
 even
 move.l _MathTransBase(pc),a6
 move.l (a5)+,D0
 jsr -102(a6)
 move.l d0,-(a5)
 rts
w_SPFieee: dc.l             w_SPTieee
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 7,"SPFIEEE"
 even
 move.l _MathTransBase(pc),a6
 move.l (a5)+,D0
 jsr -108(a6)
 move.l d0,-(a5)
 rts
w_SPAsin: dc.l              w_SPFieee
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 6,"SPASIN"
 even
 move.l _MathTransBase(pc),a6
 move.l (a5)+,D0
 jsr -114(a6)
 move.l d0,-(a5)
 rts
w_SPAcos: dc.l              w_SPAsin
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 6,"SPACOS"
 even
 move.l _MathTransBase(pc),a6
 move.l (a5)+,D0
 jsr -120(a6)
 move.l d0,-(a5)
 rts
w_SPLog10: dc.l             w_SPAcos
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 7,"SPLOG10"
 even
 move.l _MathTransBase(pc),a6
 move.l (a5)+,D0
 jsr -126(a6)
 move.l d0,-(a5)
 rts

	end

