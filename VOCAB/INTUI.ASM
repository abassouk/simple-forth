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
	dc.l	ende
	dc.l	name

init	lea	intname(pc),a1
	move.l	4.w,a6
	jsr	-408(a6)
	move.l	d0,_IntuitionBase
	beq.s	.l1
	lea	w_voc(pc),a0
	move.l	voclist(DP),(a0)
	move.l	a0,voclist(DP)
	move.l	current(DP),w_AddGadget
	moveq	#0,d1
	moveq	#0,d0
	rts
.l1	moveq	#8,d0
	moveq	#1,d1
	rts

ende	move.l	_IntuitionBase(pc),a1
	move.l	4.w,a6
	jmp	-414(a6)

name	dc.b	"Forth Intuition extension V1.0",0
intname	dc.b	"intuition.library",0
	even

w_vocab	dc.l	0
	dc.l	0
	dc.b	WF_IMMEDIATE|WF_SMUDGE|WF_BINARY|WF_VOCABULARY
	dc.b	9,"INTUITION",0
	jsr	execute
w_voc	dc.l	0
	dc.l	w_vocab
w_word	dc.l	w_NewModifyProp
	dc.l	0
	dc.b	WF_BINARY,1," "," "
execute	addq.l	#8,(a7)
	move.l	(a7)+,context(DP)
	rts

_IntuitionBase	dc.l 0
w_AddGadget: dc.l           0
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 10,"_ADDGADGET"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,A0/A1
 move.l (a5)+,D0
 jsr -42(a6)
 move.l d0,-(a5)
 rts
w_ClearDMRequest: dc.l      w_AddGadget
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"_CLEARDMREQUEST"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 jsr -48(a6)
 move.l d0,-(a5)
 rts
w_ClearMenuStrip: dc.l      w_ClearDMRequest
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"_CLEARMENUSTRIP"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 jmp -54(a6)
w_ClearPointer: dc.l        w_ClearMenuStrip
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"_CLEARPOINTER"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 jmp -60(a6)
w_CloseScreen: dc.l         w_ClearPointer
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"_CLOSESCREEN"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 jmp -66(a6)
w_CloseWindow: dc.l         w_CloseScreen
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"_CLOSEWINDOW"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 jmp -72(a6)
w_CloseWorkBench: dc.l      w_CloseWindow
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"_CLOSEWORKBENCH"
 even
 move.l _IntuitionBase(pc),a6
 jsr -78(a6)
 move.l d0,-(a5)
 rts
w_CurrentTime: dc.l         w_CloseWorkBench
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"_CURRENTTIME"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,A0/A1
 jmp -84(a6)
w_DisplayAlert: dc.l        w_CurrentTime
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"_DISPLAYALERT"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,D0/A0
 move.l (a5)+,D1
 jsr -90(a6)
 move.l d0,-(a5)
 rts
w_DisplayBeep: dc.l         w_DisplayAlert
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"_DISPLAYBEEP"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 jmp -96(a6)
w_DoubleClick: dc.l         w_DisplayBeep
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"_DOUBLECLICK"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,D0/D1/D2/D3
 jsr -102(a6)
 move.l d0,-(a5)
 rts
w_DrawBorder: dc.l          w_DoubleClick
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_DRAWBORDER"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,A0/A1
 movem.l (a5)+,D0/D1
 jmp -108(a6)
w_DrawImage: dc.l           w_DrawBorder
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 10,"_DRAWIMAGE"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,A0/A1
 movem.l (a5)+,D0/D1
 jmp -114(a6)
w_EndRequest: dc.l          w_DrawImage
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_ENDREQUEST"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,A0/A1
 jmp -120(a6)
w_GetDefPrefs: dc.l         w_EndRequest
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"_GETDEFPREFS"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 move.l (a5)+,D0
 jsr -126(a6)
 move.l d0,-(a5)
 rts
w_GetPrefs: dc.l            w_GetDefPrefs
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"_GETPREFS"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 move.l (a5)+,D0
 jsr -132(a6)
 move.l d0,-(a5)
 rts
w_InitRequester: dc.l       w_GetPrefs
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"_INITREQUESTER"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 jmp -138(a6)
w_ItemAddress: dc.l         w_InitRequester
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"_ITEMADDRESS"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 move.l (a5)+,D0
 jsr -144(a6)
 move.l d0,-(a5)
 rts
w_ModifyIDCMP: dc.l         w_ItemAddress
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"_MODIFYIDCMP"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 move.l (a5)+,D0
 jmp -150(a6)
w_ModifyProp: dc.l          w_ModifyIDCMP
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_MODIFYPROP"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,A0/A1/A2
 movem.l (a5)+,D0/D1/D2/D3/D4
 jmp -156(a6)
w_MoveScreen: dc.l          w_ModifyProp
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_MOVESCREEN"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 movem.l (a5)+,D0/D1
 jmp -162(a6)
w_MoveWindow: dc.l          w_MoveScreen
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_MOVEWINDOW"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 movem.l (a5)+,D0/D1
 jmp -168(a6)
w_OffGadget: dc.l           w_MoveWindow
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 10,"_OFFGADGET"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,A0/A1/A2
 jmp -174(a6)
w_OffMenu: dc.l             w_OffGadget
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"_OFFMENU"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 move.l (a5)+,D0
 jmp -180(a6)
w_OnGadget: dc.l            w_OffMenu
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"_ONGADGET"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,A0/A1/A2
 jmp -186(a6)
w_OnMenu: dc.l              w_OnGadget
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 7,"_ONMENU"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 move.l (a5)+,D0
 jmp -192(a6)
w_OpenScreen: dc.l          w_OnMenu
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_OPENSCREEN"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 jsr -198(a6)
 move.l d0,-(a5)
 rts
w_OpenWindow: dc.l          w_OpenScreen
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_OPENWINDOW"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 jsr -204(a6)
 move.l d0,-(a5)
 rts
w_OpenWorkBench: dc.l       w_OpenWindow
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"_OPENWORKBENCH"
 even
 move.l _IntuitionBase(pc),a6
 jsr -210(a6)
 move.l d0,-(a5)
 rts
w_PrintIText: dc.l          w_OpenWorkBench
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_PRINTITEXT"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,A0/A1
 movem.l (a5)+,D0/D1
 jmp -216(a6)
w_RefreshGadgets: dc.l      w_PrintIText
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"_REFRESHGADGETS"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,A0/A1/A2
 jmp -222(a6)
w_RemoveGadget: dc.l        w_RefreshGadgets
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"_REMOVEGADGET"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,A0/A1
 jsr -228(a6)
 move.l d0,-(a5)
 rts
w_ReportMouse: dc.l         w_RemoveGadget
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"_REPORTMOUSE"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,A0/D0
 jmp -234(a6)
w_Request: dc.l             w_ReportMouse
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"_REQUEST"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,A0/A1
 jsr -240(a6)
 move.l d0,-(a5)
 rts
w_ScreenToBack: dc.l        w_Request
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"_SCREENTOBACK"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 jmp -246(a6)
w_ScreenToFront: dc.l       w_ScreenToBack
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"_SCREENTOFRONT"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 jmp -252(a6)
w_SetDMRequest: dc.l        w_ScreenToFront
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"_SETDMREQUEST"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,A0/A1
 jsr -258(a6)
 move.l d0,-(a5)
 rts
w_SetMenuStrip: dc.l        w_SetDMRequest
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"_SETMENUSTRIP"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,A0/A1
 jmp -264(a6)
w_SetPointer: dc.l          w_SetMenuStrip
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_SETPOINTER"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,A0/A1
 movem.l (a5)+,D0/D1/D2/D3
 jmp -270(a6)
w_SetWindowTitles: dc.l     w_SetPointer
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 16,"_SETWINDOWTITLES"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,A0/A1/A2
 jmp -276(a6)
w_ShowTitle: dc.l           w_SetWindowTitles
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 10,"_SHOWTITLE"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 move.l (a5)+,D0
 jmp -282(a6)
w_SizeWindow: dc.l          w_ShowTitle
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_SIZEWINDOW"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 movem.l (a5)+,D0/D1
 jmp -288(a6)
w_ViewAddress: dc.l         w_SizeWindow
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"_VIEWADDRESS"
 even
 move.l _IntuitionBase(pc),a6
 jsr -294(a6)
 move.l d0,-(a5)
 rts
w_ViewPortAddress: dc.l     w_ViewAddress
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 16,"_VIEWPORTADDRESS"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 jsr -300(a6)
 move.l d0,-(a5)
 rts
w_WindowToBack: dc.l        w_ViewPortAddress
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"_WINDOWTOBACK"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 jmp -306(a6)
w_WindowToFront: dc.l       w_WindowToBack
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"_WINDOWTOFRONT"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 jmp -312(a6)
w_WindowLimits: dc.l        w_WindowToFront
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"_WINDOWLIMITS"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 movem.l (a5)+,D0/D1/D2/D3
 jsr -318(a6)
 move.l d0,-(a5)
 rts
w_SetPrefs: dc.l            w_WindowLimits
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"_SETPREFS"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 movem.l (a5)+,D0/D1
 jmp -324(a6)
w_IntuiTextLength: dc.l     w_SetPrefs
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 16,"_INTUITEXTLENGTH"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 jsr -330(a6)
 move.l d0,-(a5)
 rts
w_WBenchToBack: dc.l        w_IntuiTextLength
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"_WBENCHTOBACK"
 even
 move.l _IntuitionBase(pc),a6
 jsr -336(a6)
 move.l d0,-(a5)
 rts
w_WBenchToFront: dc.l       w_WBenchToBack
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"_WBENCHTOFRONT"
 even
 move.l _IntuitionBase(pc),a6
 jsr -342(a6)
 move.l d0,-(a5)
 rts
w_AutoRequest: dc.l         w_WBenchToFront
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"_AUTOREQUEST"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 move.l (a5)+,A1
 move.l (a5)+,A2
    ***** move.l (a5)+,A3
 move.l (a5)+,D0
 move.l (a5)+,D1
 move.l (a5)+,D2
 move.l (a5)+,D3
 jsr -348(a6)
 move.l d0,-(a5)
 rts
w_BeginRefresh: dc.l        w_AutoRequest
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"_BEGINREFRESH"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 jmp -354(a6)
w_BuildSysRequest: dc.l     w_BeginRefresh
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 16,"_BUILDSYSREQUEST"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 move.l (a5)+,A1
 move.l (a5)+,A2
    ***** move.l (a5)+,A3
 move.l (a5)+,D0
 move.l (a5)+,D1
 move.l (a5)+,D2
 jsr -360(a6)
 move.l d0,-(a5)
 rts
w_EndRefresh: dc.l          w_BuildSysRequest
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_ENDREFRESH"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 move.l (a5)+,D0
 jmp -366(a6)
w_FreeSysRequest: dc.l      w_EndRefresh
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"_FREESYSREQUEST"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 jmp -372(a6)
w_MakeScreen: dc.l          w_FreeSysRequest
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"_MAKESCREEN"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 jmp -378(a6)
w_RemakeDisplay: dc.l       w_MakeScreen
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"_REMAKEDISPLAY"
 even
 move.l _IntuitionBase(pc),a6
 jmp -384(a6)
w_RethinkDisplay: dc.l      w_RemakeDisplay
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"_RETHINKDISPLAY"
 even
 move.l _IntuitionBase(pc),a6
 jmp -390(a6)
w_AllocRemember: dc.l       w_RethinkDisplay
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"_ALLOCREMEMBER"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 move.l (a5)+,D0
 move.l (a5)+,D1
 jsr -396(a6)
 move.l d0,-(a5)
 rts
w_AlohaWorkbench: dc.l      w_AllocRemember
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"_ALOHAWORKBENCH"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 jmp -402(a6)
w_FreeRemember: dc.l        w_AlohaWorkbench
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"_FREEREMEMBER"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 move.l (a5)+,D0
 jmp -408(a6)
w_LockIBase: dc.l           w_FreeRemember
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 10,"_LOCKIBASE"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,D0
 jsr -414(a6)
 move.l d0,-(a5)
 rts
w_UnlockIBase: dc.l         w_LockIBase
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"_UNLOCKIBASE"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 jmp -420(a6)
w_GetScreenData: dc.l       w_UnlockIBase
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"_GETSCREENDATA"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 move.l (a5)+,D0
 move.l (a5)+,D1
 move.l (a5)+,A1
 jmp -426(a6)
w_RefreshGList: dc.l        w_GetScreenData
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"_REFRESHGLIST"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,A0/A1/A2
 move.l (a5)+,D0
 jmp -432(a6)
w_AddGList: dc.l            w_RefreshGList
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"_ADDGLIST"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,A0/A1
 movem.l (a5)+,D0/D1/A2
 jsr -438(a6)
 move.l d0,-(a5)
 rts
w_RemoveGList: dc.l         w_AddGList
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"_REMOVEGLIST"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,A0/A1
 move.l (a5)+,D0
 jsr -444(a6)
 move.l d0,-(a5)
 rts
w_ActivateWindow: dc.l      w_RemoveGList
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"_ACTIVATEWINDOW"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 jmp -450(a6)
w_RefreshWindowFrame: dc.l  w_ActivateWindow
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 19,"_REFRESHWINDOWFRAME"
 even
 move.l _IntuitionBase(pc),a6
 move.l (a5)+,A0
 jmp -456(a6)
w_ActivateGadget: dc.l      w_RefreshWindowFrame
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"_ACTIVATEGADGET"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,A0/A1/A2
 jsr -462(a6)
 move.l d0,-(a5)
 rts
w_NewModifyProp: dc.l       w_ActivateGadget
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"_NEWMODIFYPROP"
 even
 move.l _IntuitionBase(pc),a6
 movem.l (a5)+,A0/A1/A2
 movem.l (a5)+,D0/D1/D2/D3/D4/D5
 jmp -468(a6)

	end

