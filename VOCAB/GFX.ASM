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

init	lea	mathname(pc),a1
	move.l	4.w,a6
	jsr	-408(a6)
	move.l	d0,_GfxBase
	beq.s	.l1
	lea	w_voc(pc),a0
	move.l	voclist(DP),(a0)
	move.l	a0,voclist(DP)
	move.l	current(DP),w_BltBitMap
	moveq	#0,d1
	moveq	#0,d0
	rts
.l1	moveq	#8,d0
	moveq	#1,d1
	rts

ende	move.l	_GfxBase(pc),a1
	move.l	4.w,a6
	jmp	-414(a6)

name	dc.b	"Forth GFX extension V1.0",0
mathname	dc.b	"graphics.library",0
	even

w_vocab	dc.l	0
	dc.l	0
	dc.b	WF_IMMEDIATE|WF_SMUDGE|WF_BINARY|WF_VOCABULARY
	dc.b	8,"GRAPHICS"
	jsr	execute
w_voc	dc.l	0
	dc.l	w_vocab
w_word	dc.l	w_AttemptLockLayerRom
	dc.l	0
	dc.b	WF_BINARY,0," "," "
execute	addq.l	#8,(a7)
	move.l	(a7)+,context(DP)
	rts
_GfxBase	dc.l 0

w_BltBitMap: dc.l           0
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"GFX_BLTBITMAP"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 movem.l (a5)+,D0/D1
 move.l (a5)+,A1
 movem.l (a5)+,D2/D3/D4/D5/D6/D7/A2
 jsr -30(a6)
 move.l d0,-(a5)
 rts
w_BltTemplate: dc.l         w_BltBitMap
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"GFX_BLTTEMPLATE"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 movem.l (a5)+,D0/D1/A1
 movem.l (a5)+,D2/D3/D4/D5
 jmp -36(a6)
w_ClearEOL: dc.l            w_BltTemplate
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"GFX_CLEAREOL"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 jmp -42(a6)
w_ClearScreen: dc.l         w_ClearEOL
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"GFX_CLEARSCREEN"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 jmp -48(a6)
w_TextLength: dc.l          w_ClearScreen
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"GFX_TEXTLENGTH"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 move.l (a5)+,A0
 move.l (a5)+,D0
 jsr -54(a6)
 move.l d0,-(a5)
 rts
w_Text: dc.l  w_TextLength
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"GFX_TEXT"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 move.l (a5)+,A0
 move.l (a5)+,D0
 jsr -60(a6)
 move.l d0,-(a5)
 rts
w_SetFont: dc.l             w_Text
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"GFX_SETFONT"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 move.l (a5)+,A0
 jsr -66(a6)
 move.l d0,-(a5)
 rts
w_OpenFont: dc.l            w_SetFont
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"GFX_OPENFONT"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 jsr -72(a6)
 move.l d0,-(a5)
 rts
w_CloseFont: dc.l           w_OpenFont
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"GFX_CLOSEFONT"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 jmp -78(a6)
w_AskSoftStyle: dc.l        w_CloseFont
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 16,"GFX_ASKSOFTSTYLE"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 jsr -84(a6)
 move.l d0,-(a5)
 rts
w_SetSoftStyle: dc.l        w_AskSoftStyle
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 16,"GFX_SETSOFTSTYLE"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 movem.l (a5)+,D0/D1
 jsr -90(a6)
 move.l d0,-(a5)
 rts
w_AddBob: dc.l              w_SetSoftStyle
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 10,"GFX_ADDBOB"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 move.l (a5)+,A1
 jmp -96(a6)
w_AddVSprite: dc.l          w_AddBob
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"GFX_ADDVSPRITE"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,A0/A1
 jmp -102(a6)
w_DoCollision: dc.l         w_AddVSprite
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"GFX_DOCOLLISION"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 jmp -108(a6)
w_DrawGList: dc.l           w_DoCollision
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"GFX_DRAWGLIST"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 move.l (a5)+,A0
 jmp -114(a6)
w_InitGels: dc.l            w_DrawGList
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"GFX_INITGELS"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,A0/A1/A2
 jmp -120(a6)
w_InitMasks: dc.l           w_InitGels
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"GFX_INITMASKS"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 jmp -126(a6)
w_RemIBob: dc.l             w_InitMasks
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"GFX_REMIBOB"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,A0/A1/A2
 jmp -132(a6)
w_RemVSprite: dc.l          w_RemIBob
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"GFX_REMVSPRITE"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 jmp -138(a6)
w_SetCollision: dc.l        w_RemVSprite
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 16,"GFX_SETCOLLISION"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,D0/A0/A1
 jmp -144(a6)
w_SortGList: dc.l           w_SetCollision
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"GFX_SORTGLIST"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 jmp -150(a6)
w_AddAnimOb: dc.l           w_SortGList
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"GFX_ADDANIMOB"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,A0/A1/A2
 jmp -156(a6)
w_Animate: dc.l             w_AddAnimOb
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"GFX_ANIMATE"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,A0/A1
 jmp -162(a6)
w_GetGBuffers: dc.l         w_Animate
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"GFX_GETGBUFFERS"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,A0/A1
 move.l (a5)+,D0
 jmp -168(a6)
w_InitGMasks: dc.l          w_GetGBuffers
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"GFX_INITGMASKS"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 jmp -174(a6)
w_DrawEllipse: dc.l         w_InitGMasks
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"GFX_DRAWELLIPSE"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 movem.l (a5)+,D0/D1/D2/D3
 jmp -180(a6)
w_AreaEllipse: dc.l         w_DrawEllipse
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"GFX_AREAELLIPSE"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 movem.l (a5)+,D0/D1/D2/D3
 jsr -186(a6)
 move.l d0,-(a5)
 rts
w_LoadRGB4: dc.l            w_AreaEllipse
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"GFX_LOADRGB4"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,A0/A1
 move.l (a5)+,D0
 jmp -192(a6)
w_InitRastPort: dc.l        w_LoadRGB4
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 16,"GFX_INITRASTPORT"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 jmp -198(a6)
w_InitVPort: dc.l           w_InitRastPort
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"GFX_INITVPORT"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 jmp -204(a6)
w_MrgCop: dc.l              w_InitVPort
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 10,"GFX_MRGCOP"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 jmp -210(a6)
w_MakeVPort: dc.l           w_MrgCop
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"GFX_MAKEVPORT"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,A0/A1
 jmp -216(a6)
w_LoadView: dc.l            w_MakeVPort
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"GFX_LOADVIEW"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 jmp -222(a6)
w_WaitBlit: dc.l            w_LoadView
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"GFX_WAITBLIT"
 even
 move.l _GfxBase(pc),a6
 jmp -228(a6)
w_SetRast: dc.l             w_WaitBlit
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"GFX_SETRAST"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 move.l (a5)+,D0
 jmp -234(a6)
w_Move: dc.l  w_SetRast
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"GFX_MOVE"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 movem.l (a5)+,D0/D1
 jmp -240(a6)
w_Draw: dc.l  w_Move
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 8,"GFX_DRAW"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 movem.l (a5)+,D0/D1
 jmp -246(a6)
w_AreaMove: dc.l            w_Draw
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"GFX_AREAMOVE"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 movem.l (a5)+,D0/D1
 jsr -252(a6)
 move.l d0,-(a5)
 rts
w_AreaDraw: dc.l            w_AreaMove
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"GFX_AREADRAW"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 movem.l (a5)+,D0/D1
 jsr -258(a6)
 move.l d0,-(a5)
 rts
w_AreaEnd: dc.l             w_AreaDraw
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"GFX_AREAEND"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 jmp -264(a6)
w_WaitTOF: dc.l             w_AreaEnd
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"GFX_WAITTOF"
 even
 move.l _GfxBase(pc),a6
 jmp -270(a6)
w_QBlit: dc.l               w_WaitTOF
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"GFX_QBLIT"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 jmp -276(a6)
w_InitArea: dc.l            w_QBlit
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"GFX_INITAREA"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,A0/A1
 move.l (a5)+,D0
 jmp -282(a6)
w_SetRGB4: dc.l             w_InitArea
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"GFX_SETRGB4"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 movem.l (a5)+,D0/D1/D2/D3
 jmp -288(a6)
w_QBSBlit: dc.l             w_SetRGB4
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"GFX_QBSBLIT"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 jmp -294(a6)
w_BltClear: dc.l            w_QBSBlit
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"GFX_BLTCLEAR"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 movem.l (a5)+,D0/D1
 jmp -300(a6)
w_RectFill: dc.l            w_BltClear
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"GFX_RECTFILL"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 movem.l (a5)+,D0/D1/D2/D3
 jmp -306(a6)
w_BltPattern: dc.l          w_RectFill
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"GFX_BLTPATTERN"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 move.l (a5)+,A0
 movem.l (a5)+,D0/D1/D2/D3/D4
 jmp -312(a6)
w_ReadPixel: dc.l           w_BltPattern
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"GFX_READPIXEL"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 movem.l (a5)+,D0/D1
 jsr -318(a6)
 move.l d0,-(a5)
 rts
w_WritePixel: dc.l          w_ReadPixel
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"GFX_WRITEPIXEL"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 movem.l (a5)+,D0/D1
 jmp -324(a6)
w_Flood: dc.l               w_WritePixel
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"GFX_FLOOD"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 move.l (a5)+,D2
 movem.l (a5)+,D0/D1
 jmp -330(a6)
w_PolyDraw: dc.l            w_Flood
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"GFX_POLYDRAW"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 move.l (a5)+,D0
 move.l (a5)+,A0
 jmp -336(a6)
w_SetAPen: dc.l             w_PolyDraw
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"GFX_SETAPEN"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 move.l (a5)+,D0
 jmp -342(a6)
w_SetBPen: dc.l             w_SetAPen
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"GFX_SETBPEN"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 move.l (a5)+,D0
 jmp -348(a6)
w_SetDrMd: dc.l             w_SetBPen
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"GFX_SETDRMD"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 move.l (a5)+,D0
 jmp -354(a6)
w_InitView: dc.l            w_SetDrMd
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"GFX_INITVIEW"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 jmp -360(a6)
w_CBump: dc.l               w_InitView
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"GFX_CBUMP"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 jmp -366(a6)
w_CMove: dc.l               w_CBump
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"GFX_CMOVE"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 movem.l (a5)+,D0/D1
 jmp -372(a6)
w_CWait: dc.l               w_CMove
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 9,"GFX_CWAIT"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 movem.l (a5)+,D0/D1
 jmp -378(a6)
w_VBeamPos: dc.l            w_CWait
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"GFX_VBEAMPOS"
 even
 move.l _GfxBase(pc),a6
 jsr -384(a6)
 move.l d0,-(a5)
 rts
w_InitBitMap: dc.l          w_VBeamPos
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"GFX_INITBITMAP"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 movem.l (a5)+,D0/D1/D2
 jmp -390(a6)
w_ScrollRaster: dc.l        w_InitBitMap
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 16,"GFX_SCROLLRASTER"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 movem.l (a5)+,D0/D1/D2/D3/D4/D5
 jmp -396(a6)
w_WaitBOVP: dc.l            w_ScrollRaster
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"GFX_WAITBOVP"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 jmp -402(a6)
w_GetSprite: dc.l           w_WaitBOVP
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"GFX_GETSPRITE"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 move.l (a5)+,D0
 jsr -408(a6)
 move.l d0,-(a5)
 rts
w_FreeSprite: dc.l          w_GetSprite
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"GFX_FREESPRITE"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,D0
 jmp -414(a6)
w_ChangeSprite: dc.l        w_FreeSprite
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 16,"GFX_CHANGESPRITE"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,A0/A1/A2
 jmp -420(a6)
w_MoveSprite: dc.l          w_ChangeSprite
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"GFX_MOVESPRITE"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,A0/A1
 movem.l (a5)+,D0/D1
 jmp -426(a6)
w_LockLayerRom: dc.l        w_MoveSprite
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 16,"GFX_LOCKLAYERROM"
 even
 move.l _GfxBase(pc),a6
 move.l a5,-(a7)
 move.l (a5)+,A5
 jsr -432(a6)
 move.l (a7)+,a5
 rts
w_UnlockLayerRom: dc.l      w_LockLayerRom
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 18,"GFX_UNLOCKLAYERROM"
 even
 move.l _GfxBase(pc),a6
 move.l a5,-(a7)
 move.l (a5)+,A5
 jsr -438(a6)
 move.l (a7)+,a5
 rts
w_SyncSBitMap: dc.l         w_UnlockLayerRom
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"GFX_SYNCSBITMAP"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 jmp -444(a6)
w_CopySBitMap: dc.l         w_SyncSBitMap
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"GFX_COPYSBITMAP"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,A0/A1
 jmp -450(a6)
w_OwnBlitter: dc.l          w_CopySBitMap
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"GFX_OWNBLITTER"
 even
 move.l _GfxBase(pc),a6
 jmp -456(a6)
w_DisownBlitter: dc.l       w_OwnBlitter
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 17,"GFX_DISOWNBLITTER"
 even
 move.l _GfxBase(pc),a6
 jmp -462(a6)
w_InitTmpRas: dc.l          w_DisownBlitter
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"GFX_INITTMPRAS"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,A0/A1
 move.l (a5)+,D0
 jsr -468(a6)
 move.l d0,-(a5)
 rts
w_AskFont: dc.l             w_InitTmpRas
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"GFX_ASKFONT"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 move.l (a5)+,A0
 jmp -474(a6)
w_AddFont: dc.l             w_AskFont
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"GFX_ADDFONT"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 jmp -480(a6)
w_RemFont: dc.l             w_AddFont
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"GFX_REMFONT"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A1
 jsr -486(a6)
 move.l d0,-(a5)
 rts
w_AllocRaster: dc.l         w_RemFont
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"GFX_ALLOCRASTER"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,D0/D1
 jsr -492(a6)
 move.l d0,-(a5)
 rts
w_FreeRaster: dc.l          w_AllocRaster
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 14,"GFX_FREERASTER"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 movem.l (a5)+,D0/D1
 jmp -498(a6)
w_AndRectRegion: dc.l       w_FreeRaster
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 17,"GFX_ANDRECTREGION"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,A0/A1
 jmp -504(a6)
w_OrRectRegion: dc.l        w_AndRectRegion
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 16,"GFX_ORRECTREGION"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,A0/A1
 jmp -510(a6)
w_NewRegion: dc.l           w_OrRectRegion
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"GFX_NEWREGION"
 even
 move.l _GfxBase(pc),a6
 jsr -516(a6)
 move.l d0,-(a5)
 rts
w_ClearRectRegion: dc.l     w_NewRegion
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 19,"GFX_CLEARRECTREGION"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,A0/A1
 jsr -522(a6)
 move.l d0,-(a5)
 rts
w_ClearRegion: dc.l         w_ClearRectRegion
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"GFX_CLEARREGION"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 jmp -528(a6)
w_DisposeRegion: dc.l       w_ClearRegion
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 17,"GFX_DISPOSEREGION"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 jmp -534(a6)
w_FreeVPortCopLists: dc.l   w_DisposeRegion
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 21,"GFX_FREEVPORTCOPLISTS"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 jmp -540(a6)
w_FreeCopList: dc.l         w_FreeVPortCopLists
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"GFX_FREECOPLIST"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 jmp -546(a6)
w_ClipBlit: dc.l            w_FreeCopList
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 12,"GFX_CLIPBLIT"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 movem.l (a5)+,D0/D1
 move.l (a5)+,A1
 movem.l (a5)+,D2/D3/D4/D5/D6
 jmp -552(a6)
w_XorRectRegion: dc.l       w_ClipBlit
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 17,"GFX_XORRECTREGION"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,A0/A1
 jmp -558(a6)
w_FreeCprList: dc.l         w_XorRectRegion
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"GFX_FREECPRLIST"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 jmp -564(a6)
w_GetColorMap: dc.l         w_FreeCprList
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"GFX_GETCOLORMAP"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,D0
 jsr -570(a6)
 move.l d0,-(a5)
 rts
w_FreeColorMap: dc.l        w_GetColorMap
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 16,"GFX_FREECOLORMAP"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 jmp -576(a6)
w_GetRGB4: dc.l             w_FreeColorMap
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 11,"GFX_GETRGB4"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 move.l (a5)+,D0
 jsr -582(a6)
 move.l d0,-(a5)
 rts
w_ScrollVPort: dc.l         w_GetRGB4
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 15,"GFX_SCROLLVPORT"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 jmp -588(a6)
w_UCopperListInit: dc.l     w_ScrollVPort
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 19,"GFX_UCOPPERLISTINIT"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 move.l (a5)+,D0
 jmp -594(a6)
w_FreeGBuffers: dc.l        w_UCopperListInit
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 16,"GFX_FREEGBUFFERS"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,A0/A1
 move.l (a5)+,D0
 jmp -600(a6)
w_BltBitMapRastPort: dc.l   w_FreeGBuffers
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 21,"GFX_BLTBITMAPRASTPORT"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 movem.l (a5)+,D0/D1
 move.l (a5)+,A1
 movem.l (a5)+,D2/D3/D4/D5/D6
 jmp -606(a6)
w_OrRegionRegion: dc.l      w_BltBitMapRastPort
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 18,"GFX_ORREGIONREGION"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,A0/A1
 jsr -612(a6)
 move.l d0,-(a5)
 rts
w_XorRegionRegion: dc.l     w_OrRegionRegion
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 19,"GFX_XORREGIONREGION"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,A0/A1
 jsr -618(a6)
 move.l d0,-(a5)
 rts
w_AndRegionRegion: dc.l     w_XorRegionRegion
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 19,"GFX_ANDREGIONREGION"
 even
 move.l _GfxBase(pc),a6
 movem.l (a5)+,A0/A1
 jsr -624(a6)
 move.l d0,-(a5)
 rts
w_SetRGB4CM: dc.l           w_AndRegionRegion
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 13,"GFX_SETRGB4CM"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 movem.l (a5)+,D0/D1/D2/D3
 jmp -630(a6)
w_BltMaskBitMapRastPort: dc.l             w_SetRGB4CM
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 25,"GFX_BLTMASKBITMAPRASTPORT"
 even
 move.l _GfxBase(pc),a6
 move.l (a5)+,A0
 movem.l (a5)+,D0/D1
 move.l (a5)+,A1
 movem.l (a5)+,D2/D3/D4/D5/D6
 move.l (a5)+,A2
 jmp -636(a6)
w_AttemptLockLayerRom: dc.l               w_BltMaskBitMapRastPort
 dc.l 0
 dc.b WF_BINARY|WF_SMUDGE
 dc.b 23,"GFX_ATTEMPTLOCKLAYERROM"
 even
 move.l _GfxBase(pc),a6
 move.l a5,-(a7)
 move.l (a5)+,A5
 jsr -$28e(a6)
 move.l (a7)+,a5
 move.l d0,-(a5)
 rts

	end

