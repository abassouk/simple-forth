 	opt i+,w+
	incdir	"df0:assembler_headers/"
	include	"exec/types.i"
	include	"exec/funcdef.i"
	include	"exec/exec_lib.i"
	include	"exec/memory.i"
	include	"exec/tasks.i"
	include	"libraries/dos_lib.i"
	include	"LIbraries/dos.i"

	xref	_DOSBase
	xref	_exit
	xref	_stdin
	xref	_stdout
	xdef	_main

FMAX	equ	10

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
	APTR	errlist	;list of error handlers
	APTR	extlist	;list of externs
*** here comes the private part ***
	APTR	mem_buf	;current buffer for creating WORDS
	LONG	mem_length	;how many bytes are used
	LONG	mem_size	;size of buffer
	APTR	inp_buf	;file input buffer
	LONG	inp_size	;size of the input buffer
	LONG	inp_pos	;position in the buffer
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
	STRUCT	farray,FMAX*4 ;file array
	WORD	filnum	;number of iterations
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
* to word if necessary.then there is executable code ALLWAYS, who will be
* executed when this word is executed.

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

CALL	macro
	jsr	_LVO\1(a6)
	endm

CALLSYS	macro
	move.l	4.w,a6
	CALL	\1
	endm

DATASIZE	 equ	4096
RETSIZE	 equ	1024
BUF_DEFAULT equ	256
INP_BUFSIZE equ	1024

DS	equr	a5
RS	equr	a3
DP	equr	a4

ERROR_UNKNOWN_WORD	EQU	0
ERROR_OUT_OF_MEM	EQU	1
ERROR_AREADY_DEFINING	EQU	2
ERROR_NOT_DEFINING	EQU	3
ERROR_USER_ABORT	EQU	4
ERROR_DATASTACK_WRONG	EQU	5
ERROR_RETSTACK_WRONG	EQU	6
ERROR_NOT_BINARY	EQU	7

*works ok.
_main	moveq	#103,d2			;error for exit
	move.l	#SIZEOF_data,d0
	move.l	#MEMF_PUBLIC|MEMF_CLEAR,d1
	CALLSYS	AllocMem			;allocate dataspace
	move.l	d0,DP
	tst.l	d0
	beq.w	nomem
	move.l	_DOSBase,dosbase(DP)
	move.l	#DATASIZE,d0
	move.l	#MEMF_PUBLIC|MEMF_CLEAR,d1	;allocate data stack
	CALL	AllocMem
	move.l	d0,dataend(DP)
	beq.w	nodatamem
	add.l	#DATASIZE-4,d0
	move.l	d0,datastack(DP)
	move.l	d0,DS
	move.l	#RETSIZE,d0
	move.l	#MEMF_PUBLIC|MEMF_CLEAR,d1	;allocate return stack
	CALL	AllocMem
	move.l	d0,returnend(DP)
	beq.w	noretmem
	add.l	#RETSIZE-4,d0
	move.l	d0,returnstack(DP)
	move.l	d0,RS
	move.l	a7,savestack(DP)	;save stackptr
	sub.l	a1,a1
	CALL	FindTask
	move.l	d0,ThisTask(DP)	;find task for ctrl-c
	clr.b	flags(DP)		;clear flags
	clr.l	optfile(DP)	;clear options file
	clr.l	inp_buf(DP)	;clear position of the input buffer
	clr.w	filnum(DP)
	move.w	#10,base(DP)	;set numeric base
	move.w	6(a7),argc(DP)	;argument count
	move.l	8(a7),argv(DP)	;argument values
	move.l	_stdout,d0
	move.l	d0,stdout(DP)	;set outputs
	move.l	d0,currentout(DP)
	move.l	_stdin,d0
	move.l	d0,stdin(DP)	;and inputs
	move.l	d0,currentin(DP)
	lea	tdata(pc),a0
	move.l	a0,jumptab(DP)
	clr.l	mem_buf(DP)
	bsr	allocbuf		;allocate a buffer
	bsr.s	initwords		;create the voc. header for forth
	lea	.t(pc),a0
	moveq	#.te-.t,d0
	bsr	print
	bsr	initfiles		;initialise startup files etc.
	bra.w	main_loop
.t	dc.b	"Simple FORTH V1.4 by TB 24.7.4",$a,$d
.te	
	even

tdata	bra.w	wantfree
	bra.w	readone
	bra.w	findword
	bra.w	findcfa
	bra.w	emptybufs
	bra.w	Mul32
	bra.w	Divs32
	bra.w	Divu32

initwords	moveq	#8+1+1+5+1+6+SIZEOF_vocab,d0
	move.l	#MEMF_CLEAR|MEMF_PUBLIC,d1
	CALLSYS	AllocMem
	tst.l	d0
	beq.w	cleanup+2
	clr.l	extlist(DP)
	clr.l	errlist(DP)
	lea	w_error(pc),a0
	exg.l	d0,a0
	move.l	d0,w_next(a0)
	move.l	#22+SIZEOF_vocab,w_size(a0)
	move.l	#$83050000+"FO",w_flags(a0)
	move.l	#"RTH ",12(a0)
	move.w	#$4eb9,16(a0)
	move.l	#c_vocs,18(a0)
	lea	22(a0),a1
	move.l	a0,v_name(a1)
	clr.l	v_next(a1)
	move.l	a1,voclist(DP)
	lea	v_word(a1),a2
	move.l	a2,context(DP)
	move.l	a2,current(DP)
	move.l	a0,w_next(a2)
	clr.l	w_size(a2)
	move.l	#$00022020,w_flags(a2)
	move.l	a0,fence(DP)
	move.l	a0,fence_2(DP)
	rts

erasewords	move.l	voclist(DP),d6
	move.l	4.w,a6
.lop	move.l	d6,a2
	move.l	v_next(a2),d6
	move.l	v_word(a2),d5
.l1	beq.s	.nextvoc
	move.l	d5,a1
	move.l	w_next(a1),d5
	btst	#WB_BINARY,w_flags(a1)
	bne.s	.next
	move.l	w_size(a1),d0
	beq.s	.nextvoc
	CALL	FreeMem
.next	move.l	d5,d0
	bra.s	.l1
.nextvoc	tst.l	d6
	bne.s	.lop
	move.l	extlist(DP),d7
.rpt	tst.l	d7
	beq.s	.ende
	move.l	d7,a0
	move.l	e_exit(a0),d0
	beq.s	.noe
	pea	(a0)
	move.l	d0,a1
	jsr	(a1)
	move.l	(a7)+,a0
.noe	move.l	(a0),d7
	move.l	a0,d1
	subq.l	#4,d1
	lsr.l	#2,d1
	move.l	dosbase(DP),a6
	CALL	UnLoadSeg
	bra.s	.rpt
.ende	rts

*works
cleanup	moveq	#0,d2
	move.l	savestack(DP),a7
	move.l	d2,-(a7)
	bsr	resetfiles
	move.l	4.w,a6
	move.l	inp_buf(DP),d1
	beq.s	.noinp
	move.l	d1,a1
	move.l	#INP_BUFSIZE,d0
	CALL	FreeMem
.noinp	move.l	mem_buf(DP),d7	;free all buffers
	beq.s	.ende
	move.l	d7,a1
	move.l	mem_size(DP),d0
	CALL	FreeMem
.ende	bsr	erasewords
	move.l	returnend(DP),a1
	move.l	#RETSIZE,d0
	CALLSYS	FreeMem
noretmem	move.l	dataend(DP),a1
	move.l	#DATASIZE,d0
	CALL	FreeMem
nodatamem	move.l	DP,a1
	move.l	#SIZEOF_data,d0
	CALL	FreeMem
nomem	jmp	_exit

* allocbuf allocates 256 bytes
* allocany	allocates amount in d0
allocbuf	move.l	#BUF_DEFAULT,d0
allocany	add.l	mem_size(DP),d0
	move.l	d0,d2
	move.l	#MEMF_CLEAR|MEMF_PUBLIC,D1
	CALLSYS	AllocMem
	tst.l	d0
	beq.s	.err
	move.l	d0,a1
	move.l	d0,d3
	moveq	#0,d4
	move.l	mem_buf(DP),a0
	move.l	a0,d0
	beq.s	.nocopy
	move.l	mem_size(DP),d0
.lop	move.b	(a0)+,(a1)+
	subq.l	#1,d0
	bne.s	.lop
	move.l	mem_length(DP),d4
	move.l	mem_buf(DP),a1
	move.l	mem_size(DP),d0
	CALL	FreeMem
.nocopy	move.l	d4,mem_length(DP)
	move.l	d3,mem_buf(DP)
	move.l	d2,mem_size(DP)
	move.l	d3,d0
	rts

.err	moveq	#1,d0
	move.l	d0,-(DS)
	bra.w	c_error

emptybufs	clr.l	mem_length(DP)
	andi.b	#~F_COMPILE,flags(DP)
	clr.l	create(DP)
	rts

doerrors	move.l	errlist(DP),d0
	beq.s	.l1
	move.l	d0,a0
	jsr	(a0)
.l1	rts

*not tested
w_forget	dc.l	0	;next word
	dc.l	0	;size (if 0 no remove)
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	10,"INT_FORGET"
c_forget	move.l	(DS)+,d6
	move.l	current(DP),a2
	move.l	(a2),a2
.loop	cmp.l	fence(DP),a2
	beq.s	.ok
	cmp.l	fence_2(DP),a2
	beq.s	.ok
	move.l	a2,d2
	beq.s	.ok
	move.l	a2,a1
	move.l	w_next(a1),a2
	move.l	w_size(a1),d0
	beq.s	.ok
	CALLSYS	FreeMem
	cmp.l	d6,d2
	bne.s	.loop
.ok	move.l	current(DP),a1
	move.l	a2,(a1)
	rts

*has to work ok...
w_here	dc.l	w_forget		;next word
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	4,"HERE"
c_here	move.l	mem_buf(DP),d0	;put address down
	add.l	mem_length(DP),d0
	move.l	d0,-(DS)
	rts

* makes sure there are at least d0 bytes free at HERE
*works ok(no multiple buffers yet)
wantfree	move.l	mem_length(DP),d3
	add.l	d0,d3
	sub.l	mem_size(DP),d3
	bls.s	.ok
	cmp.l	#BUF_DEFAULT,d0
	bge.s	.l1
	move.l	#BUF_DEFAULT,d0
.l1	bsr	allocany
.ok	rts

*works ok so far
w_allot	dc.l	w_here
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	5,"ALLOT",0
c_allot	move.l	(DS),d0
	bsr.s	wantfree
	move.l	(DS)+,d0
	add.l	d0,mem_length(DP)
	rts

*works ok ... 
w_makeword	dc.l	w_allot		;enter a word into the dictionary
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	8,"MAKEWORD"
c_makeword	move.l	mem_length(DP),d0
	beq.s	.nothing
	move.l	#MEMF_CLEAR|MEMF_PUBLIC,d1
	CALLSYS	AllocMem
	move.l	d0,a1
	move.l	a1,d2
	beq.s	.err
	move.l	mem_buf(DP),a0
	move.l	mem_length(DP),d0
.copy	subq.l	#1,d0
	bmi.s	.ok
	move.b	(a0)+,(a1)+
	bra.s	.copy
.ok	move.l	d2,a0
	ori.b	#WF_SMUDGE,w_flags(a0)
	move.l	current(DP),a1
	move.l	(a1),w_next(a0)
	move.l	mem_length(DP),w_size(a0)
	move.l	a0,(a1)
	bsr	emptybufs
.nothing	rts
.err	moveq	#1,d0
	move.l	d0,-(DS)
	bra.w	c_error

*has to work ok....
w_len	dc.l	w_makeword		;get the position of the current
	dc.l	0		;word to be created
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	3,"LEN",0
c_len	move.l	mem_length(DP),-(DS)
	rts

*works ok
resetfiles	move.l	dosbase(DP),a6
	move.l	optfile(DP),d1
	beq.s	resetinout
	clr.l	optfile(DP)
	CALL	Close
resetinout	move.l	dosbase(DP),a6
	move.w	filnum(DP),d4
	beq.s	.noin
	subq.w	#1,d4
	add.w	d4,d4
	add.w	d4,d4
.lop	move.l	farray(DP,d4.w),d1
	cmp.l	stdin(DP),d1
	beq.s	.noclose
	CALL	Close
.noclose	subq.w	#4,d4
	bge.s	.lop
	clr.w	filnum(DP)
.noin	move.l	currentout(DP),d1
	cmp.l	stdout(DP),d1
	beq.s	.ok1
	CALL	Close
	move.l	stdout(DP),currentout(DP)
.ok1	move.l	currentin(DP),d1
	cmp.l	stdin(DP),d1
	beq.s	.ok2
	CALL	Close
	clr.w	thisarg(DP)
	move.l	stdin(DP),currentin(DP)
	clr.l	inp_size(DP)
	clr.l	inp_pos(DP)
	bset	#B_PROMPT,flags(DP)
	move.l	#-1,eolflag(DP)
	bra.s	.ende
.ok2	tst.l	eolflag(DP)
	bne.s	.ende
	lea	-256(a7),a7
	move.l	a7,a2
	move.l	currentin(DP),d1
	moveq	#$7f,d0
	bsr	readline
	moveq	#-1,d0
	move.l	d0,eolflag(DP)
	lea	256(a7),a7
.ende	rts

*works ok
initfiles	move.l	dosbase(DP),a6		;intialise files to read 
	move.l	optfile(DP),d1	;the definition words
	beq.s	.q1
	CALL	Close
.q1	tst.l	inp_buf(DP)
	bne.s	.q2
	move.l	#INP_BUFSIZE,D0
	move.l	#MEMF_PUBLIC|MEMF_CLEAR,d1
	CALLSYS	AllocMem
	move.l	dosbase(DP),a6
	move.l	d0,inp_buf(DP)
	bne.s	.q2
	moveq	#103,d2
	bra	cleanup+2
.q2	clr.l	inp_size(DP)
	clr.l	inp_pos(DP)
	lea	.fname1(PC),a0
	move.l	a0,d1
	move.l	#MODE_OLDFILE,d2
	CALL	Open
	move.l	d0,optfile(DP)
	bne.s	.ok
	lea	.fname2(pc),a0
	move.l	a0,d1
	CALL	Open
	move.l	d0,optfile(DP)
.ok	bsr.s	nextfile
	rts
.fname2	dc.b	"FORTH:"
.fname1	dc.b	"forth.opt",0
	even

*works ok
nextfile	move.l	currentin(DP),d1
	move.l	dosbase(DP),a6
	cmp.l	stdin(DP),d1
	beq.s	.s2
	CALL	Close
	move.l	stdin(DP),currentin(DP)
.s2	move.w	filnum(DP),d0
	beq.s	.noothers
	subq.w	#1,d0
	move.w	d0,filnum(DP)
	add.w	d0,d0
	add.w	d0,d0
	move.l	farray(DP,d0.w),currentin(DP)
	bclr	#B_PROMPT,flags(DP)
	rts
.noothers	move.l	optfile(DP),d1	;set the next file as input
	beq.w	.parameters		;or as the default input 
	lea	-256(a7),a7
.l1	move.l	a7,a2
	move.l	optfile(DP),d1
	move.l	#256,d0
	bsr	readline
	tst.l	d0
	beq.s	.l1
	bmi.s	.nomore
	move.l	a7,d1
	cmpi.b	#"#",(a7)
	bne.s	.l4
	addq.l	#1,d1
	bsr	c_bload2
	bra.s	.l1
.l4	move.l	#MODE_OLDFILE,d2
	CALL	Open
	tst.l	d0
	beq.s	.l1
	move.l	d0,currentin(DP)
	bsr	set_inpbuf
	bclr	#B_PROMPT,flags(DP)
.l2	lea	256(a7),a7
	rts
.nomore	lea	256(a7),a7
	move.l	optfile(DP),d1
	clr.l	optfile(DP)
	CALL	Close
	move.w	argc(DP),d0
	subq.w	#1,d0
	move.w	d0,thisarg(DP)
	move.l	argv(DP),a0
	addq.l	#4,a0
	move.l	a0,carg(DP)
.parameters move.w	thisarg(DP),d0
	ble.s	.standardin
	subq.w	#1,thisarg(DP)
	move.l	carg(DP),a0
	move.l	(a0)+,d1
	move.l	a0,carg(DP)
	move.l	d1,a0
	cmpi.b	#"-",(a0)
	beq.s	.parameters
	move.l	#MODE_OLDFILE,d2
	CALL	Open
	tst.l	d0
	beq.s	.parameters
	move.l	d0,currentin(DP)
	bsr	set_inpbuf
	bclr	#B_PROMPT,flags(DP)
	rts
.standardin move.l	stdin(DP),currentin(DP)
	bset	#B_PROMPT,flags(DP)
	bsr	printok
	rts

* works ok
readline	movem.l	d1-d6/a0-a2/a6,-(a7)
	move.l	d1,d4
	move.l	a2,d2
	move.l	d0,d5
	moveq	#1,d3
	moveq	#0,d6
.loop	move.l	d4,d1
	CALL	Read
	cmp.l	d0,d3
	bne.s	.err
	move.l	d2,a0
	cmpi.b	#$a,(a0)
	beq.s	.ok
	addq.l	#1,d6
	addq.l	#1,d2
	cmp.l	d6,d5
	bne.s	.loop
.ok	move.l	d2,a0
	clr.b	(a0)
.exit	move.l	d6,d0
	movem.l	(a7)+,d1-d6/a0-a2/a6
	rts
.err	moveq	#-1,d6
	bra.s	.exit

*works ok
readone	move.l	currentin(DP),d1
	cmp.l	stdin(DP),d1
	beq.s	.l2
	move.l	inp_buf(DP),a0
	move.l	inp_pos(DP),d0
	cmp.l	inp_size(DP),d0
	beq.s	.l1
	move.l	d2,a1
	move.b	0(a0,d0.l),(a1)
	addq.l	#1,d0
	move.l	d0,inp_pos(DP)
	rts

.l1	movem.l	d2-d4/a2/a6,-(a7)
	bsr.s	set_inpbuf
	movem.l	(a7)+,d2-d4/a2/a6
	bra.s	readone

.l2	move.l	currentin(DP),d1
	moveq	#1,d3
	CALL	Read
	cmp.l	d0,d3
	bne.s	.l2
	rts

set_inpbuf	move.l	currentin(DP),d1
	move.l	inp_buf(DP),d2
	move.l	#INP_BUFSIZE,d3
	move.l	dosbase(DP),a6
	CALL	Read
	clr.l	inp_pos(DP)
	move.l	d0,inp_size(DP)
	ble.s	.nomore
	rts
.nomore	clr.l	inp_size(DP)
	bra	nextfile

*works ok
w_putlong	dc.l	w_len
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	1,",",0
c_putlong	moveq	#4,d0
	bsr	wantfree
	move.l	mem_buf(DP),a0
	add.l	mem_length(DP),a0
	move.l	(DS)+,(a0)
	addq.l	#4,mem_length(DP)
	rts

*works ok
w_putword	dc.l	w_putlong
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	2,"W,"
c_putword	moveq	#2,d0
	bsr	wantfree
	move.l	mem_buf(DP),a0
	add.l	mem_length(DP),a0
	addq.l	#2,DS
	move.w	(DS)+,(a0)
	addq.l	#2,mem_length(DP)
	rts

*not tested
w_putbyte	dc.l	w_putword
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	2,"C,"
c_putbyte	moveq	#1,d0
	bsr	wantfree
	move.l	mem_buf(DP),a0
	add.l	mem_length(DP),a0
	addq.l	#3,DS
	move.b	(DS)+,(A0)
	addq.l	#1,mem_length(DP)
	rts

* reads a word and stores it at HERE, with length at front
* requires delimiter in DS
* works ok
w_word	dc.l	w_putbyte
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	4,"WORD"
c_word	move.l	(DS)+,d7
	move.l	dosbase(DP),a6
.wrept	move.l	mem_size(DP),d0
	sub.l	mem_length(DP),d0	;free space
	cmpi.l	#10,d0
	bge.s	.l1		;less than 10 bytes
	bsr	allocbuf
	bra.s	.wrept
.l1	move.l	d0,a2
	move.l	mem_buf(DP),a1
	add.l	mem_length(DP),a1
	moveq	#0,d4	;length
	move.l	a1,d2
	bsr.s	.skip		;skip delimiters
	move.l	a1,d5
	move.b	(a1),1(a1)
	clr.b	(a1)
	addq.l	#1,a1
	bra.s	.l2
.lop	move.l	a1,d2
	cmp.l	d4,a2
	beq.s	.e3
	bsr	readone
	move.l	d2,a1
.l2	move.b	(a1)+,d0
	cmpi.b	#"a",d0
	blt.s	.u1
	cmpi.b	#"z",d0
	bgt.s	.u1
	subi.b	#"a"-"A",-1(a1)
.u1	addq.l	#1,d4
	cmp.b	d7,d0
	beq.s	.e1
	cmp.b	#$a,d0
	beq.s	.e2
	cmp.b	#$c,d0
	bne.s	.lop
.e1	subq.l	#1,d4
	moveq	#0,d0
.x1	move.l	d5,a0
	move.b	d4,(a0)
	beq.w	.wrept
	move.l	a0,-(DS)
	move.l	d0,eolflag(DP)
	rts
.e2	subq.l	#1,d4
	moveq	#-1,d0
	bra.s	.x1
.e3	moveq	#1,d0
	bra.s	.x1
	
.skip	move.l	a1,d2	;skip delimiters
	bsr	readone
	move.l	d2,a1
	cmp.b	(a1),d7
	beq.s	.skip
	cmpi.b	#"(",(a1)
	bne.s	.ende
.s2	move.l	a1,d2	;skip comment
	bsr	readone	;non-complex only
	move.l	d2,a1
	cmpi.b	#")",(a1)
	bne.s	.s2
	bra.s	.skip
.ende	rts

* calculates the code field address of a word
*works ok
findcfa	lea	w_length(a0),a1
	moveq	#0,d0
	move.b	(a1)+,d0
	lea	1(a1,d0.l),a1
	move.l	a1,d0
	and.w	#$fffe,d0
	move.l	d0,a1
	rts

*finds the word * given the name (works OK)
* a0=bstr * 
findword	move.l	a0,d2
	move.l	context(DP),a1
	move.l	(a1),d7
	beq.s	.l1
.lop	move.l	d7,a1
	lea	w_length(a1),a0
	moveq	#0,d0
	move.b	(a0),d0
	move.l	d2,a2
.l2	cmp.b	(a0)+,(a2)+
	dbne	d0,.l2
	beq.s	.found
.next	move.l	w_next(a1),d7
	bne.s	.lop
.l1	moveq	#0,d0
	rts
.found	btst	#WB_SMUDGE,w_flags(a1)
	beq.s	.next
	move.l	a1,d0
	rts

*all interpreting stuff works ok.
printok	btst	#B_PROMPT,flags(DP)
	beq.s	.c
	btst	#B_COMPILE,flags(DP)
	bne.s	.c
	lea	.OK(pc),a0
	move.l	a0,d2
	move.l	stdout(DP),d1
	moveq	#4,d3
	move.l	dosbase(DP),a6
	CALL	Write
.c	rts
.OK	dc.b	"OK.",$a
	even

main_loop	tst.l	eolflag(DP)
	beq.s	.comploop
	bsr.s	printok
.comploop	moveq	#" ",d0
	move.l	d0,-(DS)		;delimiter = space
	bsr	c_word		;get a word
	move.l	(DS),a0
	bsr.s	findword		;find it
	tst.l	d0
	beq.w	.noword		;it`s no word,is it a number ?
	addq.l	#4,DS
	move.l	d0,a0
	btst	#WB_IMMEDIATE,w_flags(a0)
	bne.s	.immed
	btst	#B_COMPILE,flags(DP)
	bne.s	.comp
.immed	btst	#B_COMPIMM,flags(DP)
	bne.s	.comp
	move.l	d0,a0
	bsr	findcfa
	move.l	a0,-(a7)
	move.l	mem_length(DP),-(A7)
	move.l	a1,-(A7)
	bsr	datacheck
	move.l	(A7)+,a1
	jsr	(a1)
	move.l	(a7)+,d2
	move.l	(a7)+,a0
	btst	#WB_PRODCODE,w_flags(a0)
	beq.s	.ok
	btst	#B_COMPILE,flags(DP)
	bne.s	.ok
	move.l	mem_buf(DP),a1
	move.l	mem_length(DP),d0
	move.w	#"Nu",0(a1,d0.l)
	move.l	d2,-(a7)
	jsr	0(a1,d2.l)
	move.l	(a7)+,d2
	move.l	d2,mem_length(DP)
	bra.s	.ok
.comp	move.l	a0,-(a7)
	move.l	#$4eb9,-(DS)
	bsr	c_putword
	move.l	(a7)+,a0
	bsr	findcfa
	move.l	a1,-(DS)
	bsr	c_putlong
.ok	tst.b	count1(DP)
	beq.s	.ok2
	subq.b	#1,count1(DP)
	bne.w	main_loop
.ok2	andi.b	#~F_COMPIMM,flags(DP)
	bra.w	main_loop

.noword	bsr.s	makenum
	btst	#B_COMPILE,flags(DP)
	beq.s	.ok
.l2	move.l	(DS),-(a7)
	moveq	#6,d0
	move.l	d0,(DS)
	bsr	wantfree
	move.l	mem_buf(DP),a0
	add.l	mem_length(DP),a0
	move.w	#$2b3c,(a0)+
	move.l	(a7)+,(a0)+
	bsr	c_allot
	bra.s	.ok

w_number	dc.l	w_word
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	4,"ATOI"
makenum	move.l	(DS)+,a0
	moveq	#0,d0
	move.b	(a0)+,d0
	moveq	#0,d2
	moveq	#0,d3
	moveq	#0,d4
	move.l	d0,d1
	move.w	base(DP),d3
	move.l	a0,a1
.lop	move.b	(a0)+,d0
	cmpi.b	#"-",d0
	bne.s	.nomin
	moveq	#-1,d4
	bra.s	.next
.nomin	subi.b	#"0",d0
	bmi.s	.err
	cmpi.b	#10,d0
	bmi.s	.lt10
	subq.b	#7,d0
.lt10	cmp.b	d0,d3
	bmi.s	.err
	movem.l	d0-d1,-(a7)
	move.l	d3,d0
	move.l	d2,d1
	bsr	Mul32
	move.l	d0,d2
	movem.l	(a7)+,d0-d1
	add.l	d0,d2
.next	subq.b	#1,d1
	bne.s	.lop
	tst	d4
	beq.s	.l1
	neg.l	d2
.l1	move.l	d2,-(DS)
	rts

.err	clr.l	-(DS)
	bra.w	c_error

*works ok
CR	dc.b	$a,$d
printCR	lea	CR(PC),a0
	moveq	#2,d0
*print text at a0,len is d0
print	move.l	dosbase(DP),a6		;it`s a ."print this text"
	move.l	currentout(DP),d1
	move.l	d0,d3
	move.l	a0,d2
	CALL	Write
	rts

* works ok
w_wordheader dc.l	w_number	;make a word with header
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE|WF_IMMEDIATE
	dc.b	1,"`",0
c_startdef	moveq	#9,d0
	move.l	d0,-(DS)
	bsr	wantfree
	move.l	mem_buf(DP),a0
	add.l	mem_length(DP),a0
	clr.b	w_flags(a0)
	bsr	c_allot
	moveq	#" ",d0
	move.l	d0,-(DS)
	bsr	c_word
	move.l	(DS)+,a0
	moveq	#0,d0
	move.b	(a0)+,d0
	addq.w	#3,d0
	andi.w	#$fffe,d0
	subq.w	#1,d0
	move.l	d0,-(DS)
	bsr	c_allot
	rts

w_cold	dc.l	w_wordheader
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	DC.B	4,"COLD"
c_cold	move.l	returnstack(DP),RS
	move.l	savestack(DP),SP
	moveq	#-1,d0
	move.l	d0,-(DS)
	bsr	doerrors
	move.l	datastack(DP),DS
	bsr	emptybufs
	bsr	erasewords
	bsr	initwords
	bsr	resetfiles
	bsr	initfiles
	bra	main_loop

* works ok.
w_warm	dc.l	w_cold
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	4,"WARM"
c_warm	bsr	resetfiles
	bsr	emptybufs
	move.l	datastack(DP),DS
	move.l	returnstack(DP),RS
	move.l	savestack(DP),SP
	bra	main_loop

* works ok.
w_vlist	dc.l	w_warm
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	5,"VLIST",0
c_vlist	move.l	context(DP),a0
	move.l	(a0),a0
.lop	move.l	w_next(a0),-(a7)
	moveq	#0,d0
	move.b	w_length(a0),d0
	lea	w_length+1(a0),a0
	bsr	print
	bsr	printCR
	move.l	(a7)+,a0
	move.l	a0,d0
	bne.s	.lop
	rts

*works ok.
w_exit	dc.l	w_vlist
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	4,"EXIT"
c_exit	move.l	#$80000000,-(DS)
	bsr	doerrors
	bra	cleanup

w_defstart	dc.l	w_exit
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE|WF_IMMEDIATE
	dc.b	1,":",0
c_defstart	btst	#B_COMPILE,flags(DP)
	bne.s	.err
	bsr	emptybufs
	bsr	c_startdef
	move.l	mem_buf(DP),create(DP)
	bset	#B_COMPILE,flags(DP)
	btst	#B_CHECKS,flags(DP)
	beq.s	.l1
	move.l	#$4eb9,-(DS)
	bsr	c_putword
	move.l	#datacheck,-(DS)
	bsr	c_putlong
.l1	rts
.err	moveq	#2,d0
	move.l	d0,-(DS)
	bra.w	c_error

w_check	dc.l	w_defstart
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	5,"CHECK",0
datacheck	btst	#B_CHECKS,flags(DP)
	bne.s	.l1
	rts
.l1	move.l	ThisTask(DP),a0
	move.l	TC_SIGRECVD(a0),d0
	btst	#SIGBREAKB_CTRL_C,D0
	beq.s	.l2
	bclr	#SIGBREAKB_CTRL_C,d0
	move.l	d0,TC_SIGRECVD(a0)
	moveq	#4,d0
	move.l	d0,-(DS)
	bra.w	c_error
.l2	cmpa.l	datastack(DP),DS
	bhi.s	.errds
	cmpa.l	dataend(DP),DS
	bmi.s	.errds
	cmpa.l	returnstack(DP),RS
	bhi.s	.errrs
	cmpa.l	returnend(DP),RS
	bmi.s	.errrs
	rts
.errds	move.l	datastack(DP),DS
	moveq	#5,d0
	move.l	d0,-(DS)
	bra.w	c_error
.errrs	moveq	#6,d0
	move.l	d0,-(DS)
	bra.w	c_error

w_defstop	dc.l	w_check
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE|WF_IMMEDIATE
	dc.b	1,";",0
	btst	#B_COMPILE,flags(DP)
c_defstop	beq.s	.err
	move.l	#"Nu",-(DS)
	bsr	c_putword
	bclr	#B_COMPILE,flags(DP)
	clr.l	create(DP)
	bsr	c_makeword
	rts
.err	moveq	#3,d0
	move.l	d0,-(DS)
	bra.w	c_error

w_key	dc.l	w_defstop
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	3,"KEY",0
c_key	move.l	DS,d2
	subq.l	#1,d2
	clr.l	-(DS)
	move.l	dosbase(DP),a6
	bsr	readone
	rts

w_emit	dc.l	w_key
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	4,"EMIT"
c_emit	lea	3(DS),a0
	moveq	#1,d0
	bsr	print
	addq.l	#4,DS
	rts

w_comp	dc.l	w_emit
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE|WF_IMMEDIATE
	DC.B	9,"[COMPILE]",0
c_comp	bset	#B_COMPIMM,flags(DP)
	move.b	#2,count1(DP)
	rts

w_endcomp	dc.l	w_comp
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE|WF_IMMEDIATE
	dc.b	1,"[",0
c_endcomp	bclr	#B_COMPILE,flags(DP)
	rts

w_startcomp dc.l	w_endcomp
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE|WF_IMMEDIATE
	DC.B	1,"]",0
c_startcomp bset	#B_COMPILE,flags(DP)
	rts

* works ok.
w_type	dc.l	w_startcomp
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	4,"TYPE"
c_type	move.l	(DS)+,d0
	move.l	(DS)+,a0
	bsr	print
	rts

w_prttxt	dc.l	w_type
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE|WF_IMMEDIATE|WF_PRODCODE
	dc.b	2,".",34
c_prttxt	moveq	#16,d0
	bsr	wantfree
	move.l	mem_buf(DP),d0
	add.l	mem_length(DP),d0
	move.l	d0,-(A7)
	move.l	#$41fa000f,-(DS)
	bsr	c_putlong
	move.l	#$70004eb9,-(DS)
	bsr	c_putlong
	lea	print(pc),a0
	move.l	a0,-(DS)
	bsr	c_putlong
	move.l	#$60000000,-(DS)
	bsr	c_putlong
	moveq	#34,d0
	move.l	d0,-(DS)
	bsr	c_word
	move.l	(DS)+,A0
	move.l	(A7)+,a1
	moveq	#0,d0
	move.b	(a0),d0
	move.b	d0,5(a1)
	addq.w	#4,d0
	andi.w	#$fffe,d0
	move.w	d0,14(a1)
	subq.w	#2,d0
	move.l	d0,-(DS)
	bsr	c_allot
	rts

*works ok
w_base	dc.l	w_prttxt
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	DC.B	4,"BASE"
c_base	lea	base(DP),a0
	move.l	a0,-(DS)
	rts

c_prtdec	lea	-256(a7),a7
	move.l	a7,a2
	move.l	a3,-(a7)
	move.l	a2,a3
	lea	.prg(pc),a2
	move.l	a5,a1
	moveq	#0,d7
	CALLSYS	RawDoFmt
	move.l	d7,d3
	subq.l	#1,d3
	move.l	(a7)+,a3
	move.l	dosbase(DP),a6
	move.l	a7,d2
	move.l	currentout(DP),d1
	CALL	Write
	lea	256(a7),a7
	rts
.prg	move.b	d0,(a3)+
	addq.l	#1,d7
	rts	

w_extlist	dc.l	w_base
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	DC.B	8,"EXT-LIST"
c_extlist	lea	extlist(DP),a0
	move.l	a0,-(DS)
	rts
w_errlist	dc.l	w_extlist
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	DC.B	8,"ERR-LIST"
c_errlist	lea	errlist(DP),a0
	move.l	a0,-(DS)
	rts
w_voclist	dc.l	w_errlist
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	8,"VOC-LIST"
c_voclist	lea	voclist(DP),a0
	move.l	a0,-(DS)
	rts
w_current	dc.l	w_voclist
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	7,"CURRENT",0
c_current	lea	current(DP),a0
	move.l	a0,-(DS)
	rts
w_context	dc.l	w_current
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	7,"CONTEXT",0
c_context	lea	context(DP),a0
	move.l	a0,-(DS)
	rts

w_vocabulary dc.l	w_context
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	10,"VOCABULARY"
c_vocabulary bsr	emptybufs
	bsr	c_startdef
	move.l	#$4eb9,-(DS)
	bsr	c_putword
	lea	c_vocs(pc),a0
	move.l	a0,-(DS)
	bsr	c_putlong
	moveq	#20,d0
	move.l	d0,-(DS)
	bsr	wantfree
	move.l	mem_buf(DP),a0
	move.l	mem_length(DP),d0
	lea	0(a0,d0.l),a1
	move.b	#$82,w_flags(a0)
	move.l	voclist(DP),(a1)+
	clr.l	(A1)+
	move.l	current(DP),(a1)+
	clr.l	(a1)+
	move.l	#$00012020,(a1)+
	bsr	c_allot
	bsr	c_makeword
	move.l	current(DP),a0
	move.l	(a0),a0
	move.l	a0,-(a7)
	bsr	findcfa
	addq.l	#6,a1
	move.l	a1,voclist(DP)
	move.l	(a7)+,v_name(a1)
	rts
c_vocs	move.l	(a7)+,a1
	addq.l	#8,a1
	move.l	a1,context(DP)
	rts

w_bload	dc.l	w_vocabulary
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	5,"#LOAD",0
c_bload	bsr	emptybufs
	moveq	#" ",d0
	move.l	d0,-(DS)
	bsr	c_word
	move.l	(DS)+,a0
	moveq	#0,d0
	move.b	(a0)+,d0
	clr.b	(a0,d0.w)
	move.l	a0,d1
c_bload2	move.l	dosbase(DP),a6
	CALL	LoadSeg
	tst.l	d0
	beq.s	.err
	move.l	d0,d1
	lsl.l	#2,d0
	addq.l	#4,d0
	move.l	d0,a0
	move.l	extlist(DP),e_next(A0)
	move.l	a0,extlist(DP)
	move.l	e_last(a0),a1
	move.l	current(DP),a2
	move.l	w_next(a2),w_next(a1)
	move.l	e_first(a0),w_next(a2)
	move.l	e_init(a0),a1
	move.l	a1,d1
	beq.s	.l1
	jsr	(a1)
	tst.l	d1
	bne.s	.err2
.l1	rts
.err	moveq	#7,d0
	move.l	d0,-(DS)
	bra	c_error
.err2	move.l	extlist(DP),a0
	move.l	e_next(a0),extlist(DP)
	move.l	d0,-(DS)
	move.l	dosbase(DP),a6
	move.l	a0,d1
	lsr.l	#2,d1
	subq.l	#1,d1
	CALL	UnLoadSeg
	bra	c_error

w_include	dc.l	w_bload
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	DC.B	8,"#INCLUDE"
c_include	moveq	#" ",d0
	move.l	d0,-(DS)
	bsr	c_word
	move.l	(DS)+,a0
	moveq	#0,d0
	move.b	(a0)+,d0
	clr.b	0(a0,d0.w)
	move.l	a0,d1
	move.l	#MODE_OLDFILE,d2
	move.l	dosbase(DP),a6
	CALL	Open
	move.l	d0,-(DS)
	bne.s	c_setinput
	moveq	#7,d0
	move.l	d0,(DS)
	bra	c_error

w_setinput	dc.l	w_include
	dc.l	0
	dc.b	WF_SMUDGE|WF_INTERN
	dc.b	8,"SETINPUT"
c_setinput	move.l	dosbase(DP),a6
	cmpi.w	#FMAX,filnum(DP)
	beq.s	.enough
	move.l	currentin(DP),d1
	cmp.l	stdin(DP),d1
	beq.s	.noput
	move.l	inp_pos(DP),d2
	sub.l	inp_size(DP),d2
	beq.s	.noseek
	moveq	#OFFSET_CURRENT,d3
	CALL	Seek
.noseek	lea	farray(DP),a0
	move.w	filnum(DP),d0
	add.w	d0,d0
	add.w	d0,d0
	move.l	currentin(DP),0(a0,d0.w)
	addq.w	#1,filnum(DP)
.noput	move.l	(DS)+,currentin(DP)
	andi.b	#~F_PROMPT,flags(DP)
	bra	set_inpbuf
.enough	move.l	(DS)+,d1
	CALL	Close
	moveq	#9,d0
	move.l	d0,-(DS)
	bra.s	c_error
	
*works ok
errmsg	dc.b	"MSG #%ld",$a,0
	even
w_error	dc.l	w_setinput
	dc.l	0
	dc.b	WF_INTERN|WF_SMUDGE
	dc.b	5,"ERROR",0
c_error	bsr	doerrors
	moveq	#-1,d0
	cmp.l	(DS),d0
	bne.s	.l1
	addq.l	#4,DS
	rts
.l1	lea	errmsg(pc),a0
	bsr	c_prtdec
	bra.w	c_warm

Divs32	tst.l	d0
	bpl.s	.apos
	neg.l	d0
	tst.l	d1
	bpl.s	.bpos
	neg.l	d1
	bsr.s	Divu32
	neg.l	d1
	rts

.bpos	bsr.s	Divu32
	neg.l	d0
	neg.l	d1
	rts

.apos	tst.l	d1
	bpl.s	Divu32
	neg.l	d1
	bsr.s	Divu32
	neg.l	d0
	rts

Divu32	move.l	d2,-(sp)
	swap	d1
	move.w	d1,d2
	bne.s	.bigert
	swap	d0
	swap	d1
	swap	d2
	move.w	d0,d2
	beq.s	.l1
	divu	d1,d2
	move.w	d2,d0
.l1	swap	d0
	move.w	d0,d2
	divu	d1,d2
	move.w	d2,d0
	swap	d2
	move.w	d2,d1
	move.l	(sp)+,d2
	rts

.bigert	move.l	d3,-(sp)
	moveq	#$10,d3
	cmp.w	#$80,d1
	bcc.s	.l2
	rol.l	#8,d1
	subq.w	#8,d3
.l2	cmp.w	#$800,d1
	bcc.s	.l3
	rol.l	#4,d1
	subq.w	#4,d3
.l3	cmp.w	#$2000,d1
	bcc.s	.l4
	rol.l	#2,d1
	subq.w	#2,d3
.l4	tst.w	d1
	bmi.s	.l7
	rol.l	#1,d1
	subq.w	#1,d3
.l7	move.w	d0,d2
	lsr.l	d3,d0
	swap	d2
	clr.w	d2
	lsr.l	d3,d2
	swap	d3
	divu	d1,d0
	move.w	d0,d3
	move.w	d2,d0
	move.w	d3,d2
	swap	d1
	mulu	d1,d2
	sub.l	d2,d0
	bcc.s	.l5
	subq.w	#1,d3
	add.l	d1,d0
.l6	bcc.s	.l6
.l5	moveq	#0,d1
	move.w	d3,d1
	swap	d3
	rol.l	d3,d0
	swap	d0
	exg	d0,d1
	move.l	(sp)+,d3
	move.l	(sp)+,d2
	rts

Mul32	movem.l	d2-d3,-(a7)
	move.l	d0,d2
	move.l	d1,d3
	swap	d2
	swap	d3
	mulu	d1,d2
	mulu	d0,d3
	mulu	d1,d0
	add.w	d3,d2
	swap	d2
	clr.w	d2
	add.l	d2,d0
	movem.l	(a7)+,d2-d3
	rts

 	END 
