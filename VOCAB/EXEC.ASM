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
	move.l	current(DP),w_InitCode
	moveq	#0,d1
	moveq	#0,d0
	rts

W_0	equ	0

name	dc.b	"Forth Exec extension V1.0",0
	even

w_vocab	dc.l	0
	dc.l	0
	dc.b	WF_IMMEDIATE|WF_SMUDGE|WF_BINARY|WF_VOCABULARY
	dc.b	4,"EXEC"
	jsr	execute
w_voc	dc.l	0
	dc.l	w_vocab
w_word	dc.l	w_CopyMemQuick
	dc.l	0
	dc.b	WF_BINARY,0," "," "
execute	addq.l	#8,(a7)
	move.l	(a7)+,context(DP)
	rts

w_InitCode: dc.l            0
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"_INITCODE"
 even
 move.l 4.w,a6
 movem.l (a5)+,D0/D1
 jmp -72(a6)
w_InitStruct: dc.l          w_InitCode
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_INITSTRUCT"
 even
 move.l 4.w,a6
 movem.l (a5)+,A1/A2
 move.l (a5)+,D0
 jmp -78(a6)
w_MakeLibrary: dc.l         w_InitStruct
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"_MAKELIBRARY"
 even
 move.l 4.w,a6
 movem.l (a5)+,A0/A1/A2
 movem.l (a5)+,D0/D1
 jsr -84(a6)
 move.l d0,-(a5)
 rts
w_MakeFunctions: dc.l       w_MakeLibrary
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"_MAKEFUNCTIONS"
 even
 move.l 4.w,a6
 move.l (a5)+,A0
 move.l (a5)+,A1
 move.l (a5)+,A2
 jmp -90(a6)
w_FindResident: dc.l        w_MakeFunctions
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"_FINDRESIDENT"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jsr -96(a6)
 move.l d0,-(a5)
 rts
w_InitResident: dc.l        w_FindResident
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"_INITRESIDENT"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 move.l (a5)+,D1
 jmp -102(a6)
w_Alert: dc.l               w_InitResident
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 6,"_ALERT"
 even
 move.l 4.w,a6
 move.l a5,-(a7)
 move.l (a5)+,D7
 move.l (a5)+,A5
 jsr -108(a6)
 move.l (a7)+,a5
 rts
w_Debug: dc.l               w_Alert
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 6,"_DEBUG"
 even
 move.l 4.w,a6
 jmp -114(a6)
w_Disable: dc.l             w_Debug
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"_DISABLE"
 even
 move.l 4.w,a6
 jmp -120(a6)
w_Enable: dc.l              w_Disable
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 7,"_ENABLE"
 even
 move.l 4.w,a6
 jmp -126(a6)
w_Forbid: dc.l              w_Enable
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 7,"_FORBID"
 even
 move.l 4.w,a6
 jmp -132(a6)
w_Permit: dc.l              w_Forbid
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 7,"_PERMIT"
 even
 move.l 4.w,a6
 jmp -138(a6)
w_SetSR: dc.l               w_Permit
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 6,"_SETSR"
 even
 move.l 4.w,a6
 movem.l (a5)+,D0/D1
 jsr -144(a6)
 move.l d0,-(a5)
 rts
w_SuperState: dc.l          w_SetSR
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_SUPERSTATE"
 even
 move.l 4.w,a6
 jsr -150(a6)
 move.l d0,-(a5)
 rts
w_UserState: dc.l           w_SuperState
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 10,"_USERSTATE"
 even
 move.l 4.w,a6
 move.l (a5)+,D0
 jmp -156(a6)
w_SetIntVector: dc.l        w_UserState
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"_SETINTVECTOR"
 even
 move.l 4.w,a6
 movem.l (a5)+,D0/A1
 jsr -162(a6)
 move.l d0,-(a5)
 rts
w_AddIntServer: dc.l        w_SetIntVector
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"_ADDINTSERVER"
 even
 move.l 4.w,a6
 movem.l (a5)+,D0/A1
 jmp -168(a6)
w_RemIntServer: dc.l        w_AddIntServer
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"_REMINTSERVER"
 even
 move.l 4.w,a6
 movem.l (a5)+,D0/A1
 jmp -174(a6)
w_Cause: dc.l               w_RemIntServer
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 6,"_CAUSE"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jmp -180(a6)
w_Allocate: dc.l            w_Cause
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"_ALLOCATE"
 even
 move.l 4.w,a6
 move.l (a5)+,A0
 move.l (a5)+,D0
 jsr -186(a6)
 move.l d0,-(a5)
 rts
w_Deallocate: dc.l          w_Allocate
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_DEALLOCATE"
 even
 move.l 4.w,a6
 movem.l (a5)+,A0/A1
 move.l (a5)+,D0
 jmp -192(a6)
w_AllocMem: dc.l            w_Deallocate
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"_ALLOCMEM"
 even
 move.l 4.w,a6
 movem.l (a5)+,D0/D1
 jsr -198(a6)
 move.l d0,-(a5)
 rts
w_AllocAbs: dc.l            w_AllocMem
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"_ALLOCABS"
 even
 move.l 4.w,a6
 movem.l (a5)+,D0/A1
 jsr -204(a6)
 move.l d0,-(a5)
 rts
w_FreeMem: dc.l             w_AllocAbs
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"_FREEMEM"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 move.l (a5)+,D0
 jmp -210(a6)
w_AvailMem: dc.l            w_FreeMem
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"_AVAILMEM"
 even
 move.l 4.w,a6
 move.l (a5)+,D1
 jsr -216(a6)
 move.l d0,-(a5)
 rts
w_AllocEntry: dc.l          w_AvailMem
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_ALLOCENTRY"
 even
 move.l 4.w,a6
 move.l (a5)+,A0
 jsr -222(a6)
 move.l d0,-(a5)
 rts
w_FreeEntry: dc.l           w_AllocEntry
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 10,"_FREEENTRY"
 even
 move.l 4.w,a6
 move.l (a5)+,A0
 jmp -228(a6)
w_Insert: dc.l              w_FreeEntry
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 7,"_INSERT"
 even
 move.l 4.w,a6
 movem.l (a5)+,A0/A1/A2
 jmp -234(a6)
w_AddHead: dc.l             w_Insert
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"_ADDHEAD"
 even
 move.l 4.w,a6
 movem.l (a5)+,A0/A1
 jmp -240(a6)
w_AddTail: dc.l             w_AddHead
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"_ADDTAIL"
 even
 move.l 4.w,a6
 movem.l (a5)+,A0/A1
 jmp -246(a6)
w_Remove: dc.l              w_AddTail
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 7,"_REMOVE"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jmp -252(a6)
w_RemHead: dc.l             w_Remove
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"_REMHEAD"
 even
 move.l 4.w,a6
 move.l (a5)+,A0
 jsr -258(a6)
 move.l d0,-(a5)
 rts
w_RemTail: dc.l             w_RemHead
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"_REMTAIL"
 even
 move.l 4.w,a6
 move.l (a5)+,A0
 jsr -264(a6)
 move.l d0,-(a5)
 rts
w_Enqueue: dc.l             w_RemTail
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"_ENQUEUE"
 even
 move.l 4.w,a6
 movem.l (a5)+,A0/A1
 jmp -270(a6)
w_FindName: dc.l            w_Enqueue
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"_FINDNAME"
 even
 move.l 4.w,a6
 movem.l (a5)+,A0/A1
 jsr -276(a6)
 move.l d0,-(a5)
 rts
w_AddTask: dc.l             w_FindName
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"_ADDTASK"
 even
 move.l 4.w,a6
 move.l a3,-(a7)
 movem.l (a5)+,A1/A2/A3
 jsr -282(a6)
 move.l (a7)+,a3
 rts
w_RemTask: dc.l             w_AddTask
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"_REMTASK"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jmp -288(a6)
w_FindTask: dc.l            w_RemTask
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"_FINDTASK"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jsr -294(a6)
 move.l d0,-(a5)
 rts
w_SetTaskPri: dc.l          w_FindTask
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_SETTASKPRI"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 move.l (a5)+,D0
 jsr -300(a6)
 move.l d0,-(a5)
 rts
w_SetSignal: dc.l           w_SetTaskPri
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 10,"_SETSIGNAL"
 even
 move.l 4.w,a6
 movem.l (a5)+,D0/D1
 jsr -306(a6)
 move.l d0,-(a5)
 rts
w_SetExcept: dc.l           w_SetSignal
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 10,"_SETEXCEPT"
 even
 move.l 4.w,a6
 movem.l (a5)+,D0/D1
 jsr -312(a6)
 move.l d0,-(a5)
 rts
w_Wait: dc.l  w_SetExcept
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 5,"_WAIT"
 even
 move.l 4.w,a6
 move.l (a5)+,D0
 jsr -318(a6)
 move.l d0,-(a5)
 rts
w_Signal: dc.l              w_Wait
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 7,"_SIGNAL"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 move.l (a5)+,D0
 jmp -324(a6)
w_AllocSignal: dc.l         w_Signal
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"_ALLOCSIGNAL"
 even
 move.l 4.w,a6
 move.l (a5)+,D0
 jsr -330(a6)
 move.l d0,-(a5)
 rts
w_FreeSignal: dc.l          w_AllocSignal
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_FREESIGNAL"
 even
 move.l 4.w,a6
 move.l (a5)+,D0
 jmp -336(a6)
w_AllocTrap: dc.l           w_FreeSignal
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 10,"_ALLOCTRAP"
 even
 move.l 4.w,a6
 move.l (a5)+,D0
 jsr -342(a6)
 move.l d0,-(a5)
 rts
w_FreeTrap: dc.l            w_AllocTrap
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"_FREETRAP"
 even
 move.l 4.w,a6
 move.l (a5)+,D0
 jmp -348(a6)
w_AddPort: dc.l             w_FreeTrap
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"_ADDPORT"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jmp -354(a6)
w_RemPort: dc.l             w_AddPort
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"_REMPORT"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jmp -360(a6)
w_PutMsg: dc.l              w_RemPort
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 7,"_PUTMSG"
 even
 move.l 4.w,a6
 movem.l (a5)+,A0/A1
 jmp -366(a6)
w_GetMsg: dc.l              w_PutMsg
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 7,"_GETMSG"
 even
 move.l 4.w,a6
 move.l (a5)+,A0
 jsr -372(a6)
 move.l d0,-(a5)
 rts
w_ReplyMsg: dc.l            w_GetMsg
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"_REPLYMSG"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jmp -378(a6)
w_WaitPort: dc.l            w_ReplyMsg
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"_WAITPORT"
 even
 move.l 4.w,a6
 move.l (a5)+,A0
 jsr -384(a6)
 move.l d0,-(a5)
 rts
w_FindPort: dc.l            w_WaitPort
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"_FINDPORT"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jsr -390(a6)
 move.l d0,-(a5)
 rts
w_AddLibrary: dc.l          w_FindPort
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_ADDLIBRARY"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jmp -396(a6)
w_RemLibrary: dc.l          w_AddLibrary
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_REMLIBRARY"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jsr -402(a6)
 move.l d0,-(a5)
 rts
w_OldOpenLibrary: dc.l      w_RemLibrary
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"_OLDOPENLIBRARY"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jsr -408(a6)
 move.l d0,-(a5)
 rts
w_CloseLibrary: dc.l        w_OldOpenLibrary
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"_CLOSELIBRARY"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jmp -414(a6)
w_SetFunction: dc.l         w_CloseLibrary
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"_SETFUNCTION"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 move.l (a5)+,A0
 move.l (a5)+,D0
 jsr -420(a6)
 move.l d0,-(a5)
 rts
w_SumLibrary: dc.l          w_SetFunction
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_SUMLIBRARY"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jmp -426(a6)
w_AddDevice: dc.l           w_SumLibrary
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 10,"_ADDDEVICE"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jmp -432(a6)
w_RemDevice: dc.l           w_AddDevice
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 10,"_REMDEVICE"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jsr -438(a6)
 move.l d0,-(a5)
 rts
w_OpenDevice: dc.l          w_RemDevice
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_OPENDEVICE"
 even
 move.l 4.w,a6
 move.l (a5)+,A0
 movem.l (a5)+,D0/A1
 move.l (a5)+,D1
 jsr -444(a6)
 move.l d0,-(a5)
 rts
w_CloseDevice: dc.l         w_OpenDevice
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"_CLOSEDEVICE"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jmp -450(a6)
w_DoIO: dc.l  w_CloseDevice
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 5,"_DOIO"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jsr -456(a6)
 move.l d0,-(a5)
 rts
w_SendIO: dc.l              w_DoIO
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 7,"_SENDIO"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jmp -462(a6)
w_CheckIO: dc.l             w_SendIO
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"_CHECKIO"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jsr -468(a6)
 move.l d0,-(a5)
 rts
w_WaitIO: dc.l              w_CheckIO
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 7,"_WAITIO"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jsr -474(a6)
 move.l d0,-(a5)
 rts
w_AbortIO: dc.l             w_WaitIO
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"_ABORTIO"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jmp -480(a6)
w_AddResource: dc.l         w_AbortIO
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"_ADDRESOURCE"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jmp -486(a6)
w_RemResource: dc.l         w_AddResource
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"_REMRESOURCE"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jmp -492(a6)
w_OpenResource: dc.l        w_RemResource
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"_OPENRESOURCE"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 move.l (a5)+,D0
 jsr -498(a6)
 move.l d0,-(a5)
 rts
w_GetCC: dc.l               w_OpenResource
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 6,"_GETCC"
 even
 move.l 4.w,a6
 jsr -528(a6)
 move.l d0,-(a5)
 rts
w_TypeOfMem: dc.l           w_GetCC
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 10,"_TYPEOFMEM"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 jsr -534(a6)
 move.l d0,-(a5)
 rts
w_Procure: dc.l             w_TypeOfMem
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"_PROCURE"
 even
 move.l 4.w,a6
 movem.l (a5)+,A0/A1
 jsr -540(a6)
 move.l d0,-(a5)
 rts
w_Vacate: dc.l              w_Procure
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 7,"_VACATE"
 even
 move.l 4.w,a6
 move.l (a5)+,A0
 jmp -546(a6)
w_OpenLibrary: dc.l         w_Vacate
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"_OPENLIBRARY"
 even
 move.l 4.w,a6
 move.l (a5)+,A1
 move.l (a5)+,D0
 jsr -552(a6)
 move.l d0,-(a5)
 rts
w_InitSemaphore: dc.l       w_OpenLibrary
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"_INITSEMAPHORE"
 even
 move.l 4.w,a6
 move.l (a5)+,A0
 jmp -558(a6)
w_ObtainSemaphore: dc.l     w_InitSemaphore
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 16,"_OBTAINSEMAPHORE"
 even
 move.l 4.w,a6
 move.l (a5)+,A0
 jmp -564(a6)
w_ReleaseSemaphore: dc.l    w_ObtainSemaphore
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 17,"_RELEASESEMAPHORE"
 even
 move.l 4.w,a6
 move.l (a5)+,A0
 jmp -570(a6)
w_AttemptSemaphore: dc.l    w_ReleaseSemaphore
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 17,"_ATTEMPTSEMAPHORE"
 even
 move.l 4.w,a6
 move.l (a5)+,A0
 jsr -576(a6)
 move.l d0,-(a5)
 rts
w_ObtainSemaphoreList: dc.l               w_AttemptSemaphore
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 20,"_OBTAINSEMAPHORELIST"
 even
 move.l 4.w,a6
 move.l (a5)+,A0
 jmp -582(a6)
w_ReleaseSemaphoreList: dc.l              w_ObtainSemaphoreList
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 21,"_RELEASESEMAPHORELIST"
 even
 move.l 4.w,a6
 move.l (a5)+,A0
 jmp -588(a6)
w_FindSemaphore: dc.l       w_ReleaseSemaphoreList
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"_FINDSEMAPHORE"
 even
 move.l 4.w,a6
 move.l (a5)+,A0
 jsr -594(a6)
 move.l d0,-(a5)
 rts
w_AddSemaphore: dc.l        w_FindSemaphore
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"_ADDSEMAPHORE"
 even
 move.l 4.w,a6
 move.l (a5)+,A0
 jmp -600(a6)
w_RemSemaphore: dc.l        w_AddSemaphore
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"_REMSEMAPHORE"
 even
 move.l 4.w,a6
 move.l (a5)+,A0
 jmp -606(a6)
w_SumKickData: dc.l         w_RemSemaphore
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"_SUMKICKDATA"
 even
 move.l 4.w,a6
 jsr -612(a6)
 move.l d0,-(a5)
 rts
w_AddMemList: dc.l          w_SumKickData
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_ADDMEMLIST"
 even
 move.l 4.w,a6
 movem.l (a5)+,D0/D1/D2/A0/A1
 jsr -618(a6)
 move.l d0,-(a5)
 rts
w_CopyMem: dc.l             w_AddMemList
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"_COPYMEM"
 even
 move.l 4.w,a6
 movem.l (a5)+,A0/A1
 move.l (a5)+,D0
 jmp -624(a6)
w_CopyMemQuick: dc.l        w_CopyMem
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"_COPYMEMQUICK"
 even
 move.l 4.w,a6
 movem.l (a5)+,A0/A1
 move.l (a5)+,D0
 jmp -630(a6)

	end

