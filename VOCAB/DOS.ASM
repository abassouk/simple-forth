 	opt i+,w+,o1+,m+

;	incdir	"df0:assembler_headers/"
	include	":types.i"
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
	dc.l	0
	dc.l	name

init	lea	w_voc(pc),a0
	move.l	voclist(DP),(a0)
	move.l	a0,voclist(DP)
	move.l	current(DP),w_Open
	moveq	#0,d1
	moveq	#0,d0
	rts

W_0	equ	0

name	dc.b	"Forth DOS extension V1.0.1",0
	even

w_vocab	dc.l	0
	dc.l	0
	dc.b	WF_IMMEDIATE|WF_SMUDGE|WF_BINARY|WF_VOCABULARY
	dc.b	3,"DOS",0
	jsr	execute
w_voc	dc.l	0
	dc.l	w_vocab
w_word	dc.l	w_Execute
	dc.l	0
	dc.b	WF_BINARY,1," "," "
execute	addq.l	#8,(a7)
	move.l	(a7)+,context(DP)
	rts

w_Open: dc.l  0
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"DOS_OPEN"
 even
 move.l dosbase(DP),a6
 movem.l (a5)+,D1/D2
 jsr -30(a6)
 move.l d0,-(a5)
 rts
w_Close: dc.l               w_Open
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"DOS_CLOSE"
 even
 move.l dosbase(DP),a6
 move.l (a5)+,D1
 jmp -36(a6)
w_Read: dc.l  w_Close
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"DOS_READ"
 even
 move.l dosbase(DP),a6
 movem.l (a5)+,D1/D2/D3
 jsr -42(a6)
 move.l d0,-(a5)
 rts
w_Write: dc.l               w_Read
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"DOS_WRITE"
 even
 move.l dosbase(DP),a6
 movem.l (a5)+,D1/D2/D3
 jsr -48(a6)
 move.l d0,-(a5)
 rts
w_Input: dc.l               w_Write
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"DOS_INPUT"
 even
 move.l dosbase(DP),a6
 jsr -54(a6)
 move.l d0,-(a5)
 rts
w_Output: dc.l              w_Input
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 10,"DOS_OUTPUT"
 even
 move.l dosbase(DP),a6
 jsr -60(a6)
 move.l d0,-(a5)
 rts
w_Seek: dc.l  w_Output
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"DOS_SEEK"
 even
 move.l dosbase(DP),a6
 movem.l (a5)+,D1/D2/D3
 jsr -66(a6)
 move.l d0,-(a5)
 rts
w_DeleteFile: dc.l          w_Seek
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"DOS_DELETEFILE"
 even
 move.l dosbase(DP),a6
 move.l (a5)+,D1
 jsr -72(a6)
 move.l d0,-(a5)
 rts
w_Rename: dc.l              w_DeleteFile
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 10,"DOS_RENAME"
 even
 move.l dosbase(DP),a6
 movem.l (a5)+,D1/D2
 jsr -78(a6)
 move.l d0,-(a5)
 rts
w_Lock: dc.l  w_Rename
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"DOS_LOCK"
 even
 move.l dosbase(DP),a6
 movem.l (a5)+,D1/D2
 jsr -84(a6)
 move.l d0,-(a5)
 rts
w_UnLock: dc.l              w_Lock
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 10,"DOS_UNLOCK"
 even
 move.l dosbase(DP),a6
 move.l (a5)+,D1
 jmp -90(a6)
w_DupLock: dc.l             w_UnLock
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"DOS_DUPLOCK"
 even
 move.l dosbase(DP),a6
 move.l (a5)+,D1
 jsr -96(a6)
 move.l d0,-(a5)
 rts
w_Examine: dc.l             w_DupLock
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"DOS_EXAMINE"
 even
 move.l dosbase(DP),a6
 movem.l (a5)+,D1/D2
 jsr -102(a6)
 move.l d0,-(a5)
 rts
w_ExNext: dc.l              w_Examine
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 10,"DOS_EXNEXT"
 even
 move.l dosbase(DP),a6
 movem.l (a5)+,D1/D2
 jsr -108(a6)
 move.l d0,-(a5)
 rts
w_Info: dc.l  w_ExNext
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"DOS_INFO"
 even
 move.l dosbase(DP),a6
 movem.l (a5)+,D1/D2
 jsr -114(a6)
 move.l d0,-(a5)
 rts
w_CreateDir: dc.l           w_Info
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"DOS_CREATEDIR"
 even
 move.l dosbase(DP),a6
 move.l (a5)+,D1
 jsr -120(a6)
 move.l d0,-(a5)
 rts
w_CurrentDir: dc.l          w_CreateDir
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"DOS_CURRENTDIR"
 even
 move.l dosbase(DP),a6
 move.l (a5)+,D1
 jsr -126(a6)
 move.l d0,-(a5)
 rts
w_IoErr: dc.l               w_CurrentDir
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"DOS_IOERR"
 even
 move.l dosbase(DP),a6
 jsr -132(a6)
 move.l d0,-(a5)
 rts
w_CreateProc: dc.l          w_IoErr
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"DOS_CREATEPROC"
 even
 move.l dosbase(DP),a6
 movem.l (a5)+,D1/D2/D3/D4
 jsr -138(a6)
 move.l d0,-(a5)
 rts
w_Exit: dc.l  w_CreateProc
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"DOS_EXIT"
 even
 move.l dosbase(DP),a6
 move.l (a5)+,D1
 jmp -144(a6)
w_LoadSeg: dc.l             w_Exit
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"DOS_LOADSEG"
 even
 move.l dosbase(DP),a6
 move.l (a5)+,D1
 jsr -150(a6)
 move.l d0,-(a5)
 rts
w_UnLoadSeg: dc.l           w_LoadSeg
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"DOS_UNLOADSEG"
 even
 move.l dosbase(DP),a6
 move.l (a5)+,D1
 jmp -156(a6)
w_DeviceProc: dc.l          w_UnLoadSeg
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"DOS_DEVICEPROC"
 even
 move.l dosbase(DP),a6
 move.l (a5)+,D1
 jsr -174(a6)
 move.l d0,-(a5)
 rts
w_SetComment: dc.l          w_DeviceProc
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"DOS_SETCOMMENT"
 even
 move.l dosbase(DP),a6
 movem.l (a5)+,D1/D2
 jsr -180(a6)
 move.l d0,-(a5)
 rts
w_SetProtection: dc.l       w_SetComment
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 17,"DOS_SETPROTECTION"
 even
 move.l dosbase(DP),a6
 movem.l (a5)+,D1/D2
 jsr -186(a6)
 move.l d0,-(a5)
 rts
w_DateStamp: dc.l           w_SetProtection
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"DOS_DATESTAMP"
 even
 move.l dosbase(DP),a6
 move.l (a5)+,D1
 jsr -192(a6)
 move.l d0,-(a5)
 rts
w_Delay: dc.l               w_DateStamp
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"DOS_DELAY"
 even
 move.l dosbase(DP),a6
 move.l (a5)+,D1
 jmp -198(a6)
w_WaitForChar: dc.l         w_Delay
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"DOS_WAITFORCHAR"
 even
 move.l dosbase(DP),a6
 movem.l (a5)+,D1/D2
 jsr -204(a6)
 move.l d0,-(a5)
 rts
w_ParentDir: dc.l           w_WaitForChar
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"DOS_PARENTDIR"
 even
 move.l dosbase(DP),a6
 move.l (a5)+,D1
 jsr -210(a6)
 move.l d0,-(a5)
 rts
w_IsInteractive: dc.l       w_ParentDir
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 17,"DOS_ISINTERACTIVE"
 even
 move.l dosbase(DP),a6
 move.l (a5)+,D1
 jsr -216(a6)
 move.l d0,-(a5)
 rts
w_Execute: dc.l             w_IsInteractive
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"DOS_EXECUTE"
 even
 move.l dosbase(DP),a6
 movem.l (a5)+,D1/D2/D3
 jsr -222(a6)
 move.l d0,-(a5)
 rts

	end

