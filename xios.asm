	TITLE	'XIOS200, Copyright 1980, ALTOS COMPUTER SYSTEMS'
;----------------------------------------------------------
;
;	ALTOS COMPUTER SYSTEMS
;	2360 BERING DRIVE
;	SAN JOSE, CALIFORNIA 95131
;
;	Copyright 1980, ALTOS COMPUTER SYSTEMS
;
;	This program is a copyright program product of
;	ALTOS COMPUTER SYSTEMS and is distributed to the
;	owners of ALTOS SUN SERIES 8000 computers for
;	use on those systems only. Any other use of this
;	software constitutes a breach of the copyright
;	license to the purchaser. However, permission is
;	granted to use this listing as a sample for the
;	construction of the reader's own XIOS.
;
;	VERSION NUMBER: 1.12*
;	VERSION DATE:   June 28, 1980
;	.       Add support for CP/M version   2.0
;	.       Add support for Hard disk drives
;	.       Add support for disk MODE selection
;	.       Provide compatability MODE for 1.4 operatio
;	.       Remove CTC/1791 counter reset
;	.       CORRECT HARD DISK SEEK PROBLEM
;	.       Add code to recover from WD1791 going to sl
;	.       Initialize parallel port for Centronics printer
;	VERSION DATE:   March 17, 1981
;	Virtual disk in banks 1,2,3:  M DISK !;
;	VERSION DATE:   April 11, 1981
;	.       Conditional assembly for virtual disks
;	Conditional assembly for MP/M 2.0
;	VERSION DATE:   April 14, 1981
;	.       Equates added for LDRBIOS hooks  !
;	VERSION DATE:   April 16, 1981
;	.       Testing for bank setup added
;
;
;	Mode	0	IBM single density
;		1	ALTOS double density Version 2.0
;		2	ALTOS double density Version 1.4
;		3	ALTOS hard disk Version 2.0 (8 MEG AT 000)
;		4	ALTOS HARD DISK VERSION 2.0 (8 MEG AT 512)
;		5	ALTOS HARD DISK VERSION 2.0 (8 MEG AT 1024)
;		6	ALTOS HARD DISK VERSION 2.0 (4 MEG AT 512)
;
;----------------------------------------------------------
	
;----------------------------------------------------------
;
;	ASSEMBLER CONTROL STATEMENTS
;
;----------------------------------------------------------
	include "diskdef.lib"

TRUE	set	0FFFFH	;VALUE FOR TRUE
FALSE	set	~~TRUE	;VALUE FOR FALSE

mdisk	equ	false	;Virtual Disk cond asm bool
mpm20	equ	true	;MP/M 2.0 cond asm boolean

;----------------------------------------------------------
ldrbiosbase	equ	1700h	; for MPMLDR.COM

DENSITY_MASK_OFFSET	equ	37h ;density mask offset from LDRBIOSBASE
MISC_PARAMS_OFFSET	equ	0bbh ;misc. parameters offset from LDRBIOSBASE

;-----------------------------------------------------------------------
;
;	THE FOLLOWING EQUATES ARE USER MODIFIABLE BASED ON THE
;	PARTICULAR USER SYSTEM AND OPTIONS SELECTED.
;
;-----------------------------------------------------------------------

DMA	EQU	FALSE		;DMA HARDWARE SUPPORT ??
HARDSK	EQU	TRUE		;HARD DISK SUPPORT

;----------------------------------------------------------
;	THE FOLLOWING CONSTANTS APPLY TO THE DEBLOCKING OF
;	SECTORS	LARGER THAN 128 FOR THE ALTOS DOUBLE DENSITY
;	AND THE ALTOS HARD DISK.
;----------------------------------------------------------

BLKSIZ	EQU	16384		;CP/M ALLOCATION SIZE
HSTSIZ	EQU	1024		;HOST DISK SECTOR SIZE
HSTSPT	EQU	16		;HOST DISK SECTORS PER TRAC
HSTBLK	EQU	HSTSIZ/128	;CP/M SECTORS PER HOST BUFF
CPMSPT	EQU	HSTBLK * HSTSPT	;CP/M SECTORS PER TRACK
SECMSK	EQU	HSTBLK - 1	;SECTOR MASK
SECSHF	EQU	3		;LOG2(HHSTBLK)

;-----------------------------------------------------------------------
;
;	THE FOLLOWING EQUATES APPLY TO THE RELOCATABILITY
;	OF THE CBIOS AND SHOULD NOT BE USER ALTERED.
;
;-----------------------------------------------------------------------

RELOC	EQU	TRUE		;RELOCATABLE VERSION ??

;----------------------------------------------------------

	if	mdisk
maxdsk	equ	13
	else
	IF	HARDSK
MAXDSK	EQU	12		;MAXIMUM NUMBER OF LOGICAL
	ELSE
MAXDSK	EQU	4		;MAXIMUM NUMBER OF LOGICAL
	ENDIF
	endif

	IF	RELOC
	ORG	0000H
	ELSE
	ORG	0C000H
	ENDIF

BASE	EQU	$

;----------------------------------------------------------

WRALL	EQU	0		;WRITE TO ALLOCATED
WRDIR	EQU	1		;WRITE TO DIRECTORY
WRUAL	EQU	2		;WRITE TO UNALLOCATED

NMBCNS	EQU	4	; NUMBER OF CONSOLES

POLL	EQU	131	; XDOS POLL FUNCTION
FLAGWT	EQU	132	; XDOS FLAG WAIT FUNCTION
FLAGST	EQU	133	; XDOS FLAG SET FUNCTION

TKFLAG	EQU	1	;System unit tick
S1FLAG	EQU	2	;One second interval
M1FLAG	EQU	3	;One minute interval
HDFLAG	EQU	5	;HARD DISK FLAG FOR WAIT & SET
FPYFLAG	EQU	6	;FLOPPY DISK FLAG FOR WAIT & SET

PLLPT	EQU	0	; POLL PRINTER
PLCO0	EQU	PLLPT+1	; POLL CONSOLE OUT #0 (CRT:)
PLCO1	EQU	PLCO0+1	; POLL CONSOLE OUT #1 (CRT:)
PLCO2	EQU	PLCO1+1	; POLL CONSOLE OUT #2 (CRT:)
PLCO3	EQU	PLCO2+1	; POLL CONSOLE OUT #3 (CRT:)
PLCI0	EQU	PLCO3+1	; POLL CONSOLE IN #0 (CRT:)
PLCI1	EQU	PLCI0+1	; POLL CONSOLE IN #1 (CRT:)
PLCI2	EQU	PLCI1+1	; POLL CONSOLE IN #2 (CRT:)
PLCI3	EQU	PLCI2+1	; POLL CONSOLE IN #3 (CRT:)

;-----------------------------------------------------------------------
;
;	JUMP VECTORS FOR ENTRIES TO CBIOS ROUTINES
;
;-----------------------------------------------------------------------

;	EXTERNAL JUMP TABLE (BELOW XIOS BASE)

;PDISP	EQU	 $-3
;XDOS	EQU	 PDISP-3

	if	mpm20
	jp	commonbase
	else
	JP	COLDSTART	;COLD START
	endif
WBOTE:
	JP	WARMSTART	;WARM START
	JP	CONST		;CONSOLE STATUS - called before banner 2
	JP	CONIN		;CONSOLE CHARACTER IN
	JP	CONOUT		;CONSOLE CHARACTER OUT - called before banner 6
	JP	LIST		;LIST CHARACTER OUT - THIS
;				;  "CLIST" IF SETUP PROGRAM
;				;  PARALLEL PRINTER PORT

	JP	RTNEMPTY	;PUNCH NOT IMPLEMENTED
	JP	RTNEMPTY	;READER NOT IMPLEMENTED
	JP	home		;MOVE HEAD TO HOME - called after banner 1
	JP	SELDSK		;SELECT DISK - called before banner 1
	JP	SETTRK		;SET TRACK NUMBER - called after banner 2
	JP	SETSEC		;SET SECTOR NUMBER - called after banner 4
	JP	SETDMA		;SET DMA ADDRESS - called before banner 4
	JP	READ		;READ DISK - called after banner 5
	JP	WRITE		;WRITE DISK
	JP	debugp;POLLPT		;LIST STATUS
	JP	SECTRAN		;SECTOR TRANSLATE - called after banner 3

;	EXTENDED I/O SYSTEM JUMP VECTOR

	JP	SELMEMORY	; SELECT MEMORY - called before banner 3
	JP	POLLDEVICE	; POLL DEVICE - called before banner 7
	JP	debugt;STARTCLOCK	; START CLOCK
	JP	debugu;STOPCLOCK	; STOP CLOCK
	JP	EXITREGION	; EXIT REGION
	JP	MAXCONSOLE	; MAXIMUM CONSOLE NUMBER - called before banner 5

	JP	SYSTEMINIT	; SYSTEM INITIALIZATION
	NOP			; NO JMP HERE
	NOP			; FOR MP/M DELAY
	NOP			;

	JP	debugw;SETMOD		;ROUTINE TO SET DISK MODE
	JP	debugx;RETMOD		;ROUTINE TO RETURN CURRENT

	if	~~mpm20
COLDSTART:
WARMSTART:
	LD	C,0		; SEE SYSTEM INIT
				; COLD & WARM START INCLUDE
				; FOR COMPATIBILITY WITH CP
	JP	XDOS		; SYSTEM RESET, TERMINATE P

RTNEMPTY:
	XOR	A		; NOT USED
	RET			;
	endif

LAST:
	ORG	(((LAST-BASE)+0A2H) & 0FF00H) +05EH

INTERUPT:
	DW	DSKINT		;DISK INTERRUPTS
	DW	NULL_INT	;
	DW	NULL_INT	;
	DW	NULL_INT	;
	DW	INT1HND		;CTC INTERRUPT
	DW	NULL_INT	;
	DW	NULL_INT	;
	DW	NULL_INT	;
	DW	NULL_INT	;
	DW	NULL_INT	;

	if	~~mpm20
NULL_INT:
	EI
	RETI
	endif

	include	"floppy.asm"
	if	hardsk
	include "hard.asm"
	endif

	if	mpm20

; *********************************************************
; *
; *	      M P / M  2 . 0   C O M M O N   B A S E
; *
; *********************************************************

commonbase:
	 jp	coldstart
swtuser: jp	$-$
swtsys:	 jp	$-$
pdisp:	 jp	$-$
xdos:	 jp	$-$
sysdat:	 dw	$-$
COLDSTART:
WARMSTART:
	LD	C,0			; SEE SYSTEM INIT
					; COLD & WARM START INCLUDE
					; FOR COMPATIBILITY WITH CP
	JP	XDOS			; SYSTEM RESET, TERMINATE P

	include "disk.asm"
	include "console.asm"
	
rtnempty:
	xor	a
	ret

NULL_INT:
	call	regdump
	EI
	RETI
	endif

; SELECT / PROTECT MEMORY

SELMEMORY:
				; REG BC = ADR OF MEM DESCRIPTOR
				; BC -> BASE   1 BYTE,
				;	SIZE   1 BYTE,
				;	ATTRIB 1 BYTE,
				;	BANK   1 BYTE.

;; 	pop	hl		; Get caller's address
;; 	push	hl
;; 	or	a
;; 	ld	de,0F468h
;; 	sbc	hl,de
;; 	add	hl,de
;; 	jr	z,selok

;; 	or	a
;; 	ld	de,0DF6Ch
;; 	sbc	hl,de
;; 	add	hl,de
;; 	jr	z,selok
	
;; 	or	a
;; 	ld	de,0E855h
;; 	sbc	hl,de
;; 	add	hl,de
;; 	jr	z,selok
	
;; 	;; or	a
;; 	;; ld	de,0E869h
;; 	;; sbc	hl,de
;; 	;; add	hl,de
;; 	;; jr	z,selok
	
;; 	call	regdump2
;; 	jp	$
;; selok:
	inc	bc
	inc	bc
	inc	bc
	ld	a,(bc)		;  GET REQUESTED BANK

	rlca
	rlca
	rlca
	ld	b,a

	in	a,(bankpt)
	and	a,018h
	cp	b
	ret	z

	;; ld	a,b
	;; rra
	;; rra
	;; rra
	;; and	3	
	;; add	a,03Ch
	;; ld	c,a
	;; call	dbgout
	;; and	a,3
	
	in	a,(bankpt)
	and	dmamsk		; Preserve DMA bank and write protect
	or	b
	out	(bankpt),a
	ret

; START CLOCK

STARTCLOCK:
				; WILL CAUSE FLAG #1 TO BE SET
				;  AT EACH SYSTEM TIME UNIT TICK
	LD	A,0FFH
	LD	(TICKN),A
	RET

; STOP CLOCK

STOPCLOCK:
				; WILL STOP FLAG #1 SETTING AT
				; SYSTEM  TIME UNIT TICK
	XOR	A
	LD	(TICKN),A
	RET

; EXIT REGION

EXITREGION:
				; EI IF NOT PREEMPTED
	LD	A,(PREEMP)
	OR	A
	RET	NZ
	EI
	RET

; MAXIMUM CONSOLE NUMBER

MAXCONSOLE:
	LD	A,NMBCNS
	RET

; MP/M 1.0   INTERRUPT HANDLERS

DSPTCH	EQU	142

INT1HND:
				; INTERRUPT 1 HANDLER ENTRY POINT
				;
T20MS:
	LD	(SVDHL),HL
	LD	HL,TIMERINT
	JR	INTINIT
TIMERINT:
	LD	A,(TICKN)
	OR	A		; TEST TICKN, INDICATES
				;  DELAYED PROCESS(ES)
	JR	Z,NOTICKN
	LD	C,FLAGST
	LD	E,TKFLAG
	CALL	XDOS		; SET FLAG #1 EACH TICK
NOTICKN:
	LD	HL,CNTX
	DEC	(HL)		; DEC TICK CNTR
	JR	NZ,NOT1SEC
	LD	A,125
	DEC	HL
	SUB	(HL)
	LD	(HL),A		; *** TOGGLE COUNT 62 <-> 6
	INC	HL
	LD	(HL),A		; *** ACTUAL #/SEC = 62.5
	LD	C,FLAGST
	LD	E,S1FLAG
	CALL	XDOS		; SET FLAG #2 @ 1 SEC
	;; LD	HL,(FPYTIME)	;IS FLOPPY TIME CHECK IN EF
	;; LD	A,H		;
	;; OR	A		;
	;; JR	Z,NOT1SEC	;IF NOT IN EFFECT, FINISH
	;; DEC	L		;SUBTRACT A SECOND

	;; LD	(FPYTIME),HL	;SAVE FOR NEXT TIME
	;; JR	NZ,NOT1SEC	;IF NOT TOO LONG, FINISH
	;; LD	H,L		;ZERO OUT INDICATOR
	;; LD	(FPYTIME),HL	;PREVENT RE-ENTRY OF THIS R
	;; LD	C,FLAGST	;
	;; LD	E,FPYFLAG	;
	;; CALL	XDOS		;CAUSE I/O FOR FLOPPY TO CO
	;; LD	A,10010000B
	;; LD	(STATUS),A	;SHOW ERROR IN FLOPPY I/O
	;; LD	HL,(FPYTCNT)
	;; INC	HL		;COUNT TIMES WD1791 GOES TO
	;; LD	(FPYTCNT),HL	;

NOT1SEC:
INTDONE:
	XOR	A
	LD	(PREEMP),A	; CLEAR PREEMPTED FLAG
	POP	BC
	POP	DE
	LD	HL,(SVDSP)
	LD	SP,HL		; RESTORE STK PTR
	POP	AF
	LD	HL,(SVDRET)
	PUSH	HL
	LD	HL,PDISP	; MP/M DISPATCH
	PUSH	HL		; PUT ON STACK FOR RETURN
	LD	HL,(SVDHL)

; THE FOLLOWING DISPATCH CALL WILL FORCE ROUND ROBIN
;  SCHEDULING OF PROCESSES EXECUTING AT THE SAME PRIORITY
;  EACH 1/32ND OF A SECOND.
; NOTE: INTERRUPTS ARE NOT ENABLED UNTIL THE DISPATCHER
;  RESUMES THE NEXT PROCESS. THIS PREVENTS INTERRUPT
;  OVER-RUN OF THE STACKS WHEN STUCK OR HIGH FREQUENCY
;  INTERRUPTS ARE ENCOUNTERED.

	RETI			; DISPATCH

INTINIT:			;SAVE MACHINE STATE FOR INTRPT HNDL
	LD	(ADRINTHD),HL
	POP	HL
	LD	(SVDRET),HL
	PUSH	AF
	LD	HL,0
	ADD	HL,SP
	LD	(SVDSP),HL	; SAVE USERS STK PTR
	LD	SP,LSTINTSTK	; LCL STK FOR INTR HNDL
	PUSH	DE
	PUSH	BC

	LD	A,0FFH
	LD	(PREEMP),A	; SET PREEMPTED FLAG
	LD	HL,(ADRINTHD)
	JP	(HL)		;JUMP TO INTERRUPT HANDLER

;
; BIOS DATA SEGMENT
;
TOGCNT:	DB	62		; TOGGLE COUNTER 62 <-> 63
CNTX:	DB	62		; TICK CNTR TO 1 SEC
INTSTK:				; LOCAL INTRPT STK
	DW	0C7C7H,0C7C7H,0C7C7H,0C7C7H,0C7C7H
	DW	0C7C7H,0C7C7H,0C7C7H,0C7C7H,0C7C7H
	DW	0C7C7H,0C7C7H,0C7C7H,0C7C7H,0C7C7H
	DW	0C7C7H,0C7C7H,0C7C7H,0C7C7H,0C7C7H
LSTINTSTK:
ADRINTHD: DW	0		; INTERRUPT HANDLER ADDRESS
SVDHL:	DW	0		; SAVED REGS HL DURING INT HNDL
SVDSP:	DW	0		; SAVED SP DURING INT HNDL
SVDRET:	DW	0		; SAVED RETURN DURING INT HNDL
TICKN:	DB	0		; TICKING BOOLEAN,TRUE = DELAYED
PREEMP:	DB	0		; PREEMPTED BOOLEAN

	if	mpm20
FPYTIME:
	DW	0

FPYTCNT:
	DW	0
	endif

SYSTEMINIT:
	; C  = BREAKPOINT RESTART NUMBER
	; DE = BREAKPOINT RESTART HANDLER ADDRESS
	; HL = DIRECT XIOS INTERCEPT JUMP TABLE ADDRESS

	LD	(SVDJT),HL
	LD	L,C
	LD	H,0
	ADD	HL,HL
	ADD	HL,HL
	ADD	HL,HL		;HL= RESTART JUMP ADDRESS
	LD	(SVDBPA),HL

;; 	if	~~MDISK
;; 	ld	hl,(SYSDAT)
;; 	ld	l,15		;hl = .nmbmemsegs
;; 	ld	b,(hl)		;b =  nmbmemsegs
;; test_bank_setup_loop:
;; 	inc	hl
;; 	inc	hl
;; 	inc	hl
;; 	inc	hl		;hl = .memseg(i). bank
;; 	ld	a,(hl)
;; 	or	a
;; 	jp	nz,bank_setup
;; 	dec	b
;; 	jp	nz,test_bank_setup_loop
;; 	jp	after_bank_setup
;; bank_setup:
;; 	LD	A,3<<3		; SELECT BANK 3
;; 	CALL	STMVTR		; SET UP VECTORS
;; 	LD	A,2<<3		; SELECT BANK 2
;; 	CALL	STMVTR		; SET UP VECTORS
;; 	LD	A,1<<3		; SELECT BANK 1
;; 	CALL	STMVTR		; SET UP VECTORS
;; after_bank_setup:
;; 	else
;; 	ld	a,3<<3		; bank 3 select for directo
;; 	out	(bankpt),a
;; 	ld	hl,0bffeh
;; 	ld	a,0e5h
;; 	cp	(hl)
;; 	inc	hl
;; 	jr	nz,fill
;; 	cp	(hl)
;; 	jr	z,dontfill
;; fill:
;; 	ld	(hl),a		;set directory initialized
;; 	dec	hl
;; 	ld	(hl),a

;; 	ld	bc,07ffh	;first 2 k of bank one gets
;; 	ld	hl,0
;; 	ld	de,1
;; 	ld	a,1<<3		; select bank 1
;; 	out	(bankpt),a
;; 	ld	(hl),0e5h
;; 	ldir
;; dontfill:
;; 	endif
;; 	LD	A,000H		; SELECT BANK 0
;; 	CALL	STMVTR		; SET UP VECTORS

;;	ld	hl,LDRBIOSBASE+DENSITY_MASK_OFFSET
;;	LD	DE,SEL0		;	THE SETUP PROGRAM
;;	LD	BC,4		;  4 SELECT MASKS
;;	LDIR			;
;;	LD	DE,MODE		;
;;	LD	BC,4		;  4 MODE BYTES
;;	LDIR			;
	;; ld	hl,(LDRBIOSBASE+MISC_PARAMS_OFFSET)
	;; LD	(MPARMS),HL	;
	;; LD	A,(MPARMS)	; NOW TEST FOR CENTRONICS P
	;; AND	2		;
	;; JR	Z,PRTOK		;    NO - LEAVE SERIAL
	;; LD	HL,CLIST	;
	;; LD	(WBOTE+13),HL	; CHANGE PRINTER ROUTINE
	;; LD	HL,CNSTAT	;   AND STATUS CHECK
	;; LD	(DEVTBL),HL	;
	;; LD	A,003H		;INITIALIZE PARALLEL PORT
	;; OUT	(013H),A
	;; LD	A,00FH		;
	;; OUT	(013H),A

;; PRTOK:
;; 	LD	BC,003H		;SET THE MODE FOR DRIVES IN
;; MODESET:
;; 	CALL	SELSDP		;SELECT DRIVE FOR MODESET
;; 	LD	HL,MODE		;
;; 	ADD	HL,BC		;POINT TO CORRECT MODE BYTE
;; 	PUSH	BC		;SAVE COUNT OF DRIVES
;; 	LD	B,C		; B = DRIVE #
;; 	LD	C,(HL)		;
;; 	CALL	XETMOD		;SET MODE
;; 	POP	BC		;
;; 	DEC	C		;END OF LIST YET ??
;; 	JP	P,MODESET	;SET MODE FOR ALL DRIVES
;; 	CALL	SDCONF		;SET DISK CONFIGURATION

	LD	BC,80H
	CALL	SETDMA		;SET DMA ADDRESS

	push	hl

	if	mpm20
	ld	hl,(sysdat)
	ld	l,7
	ld	a,(hl)
	else
	ld	hl,INTERUPT
	ld	a,h
	endif

	pop	hl
	LD	I,A		;
	LD	A,60H		; SET VECTOR FOR CTC
	OUT	(30H),A		; CTC CHANNEL 0
	LD	A,0A7H		; RESET  /  LOAD TIME CONST
	OUT	(33H),A		; CHANNEL 3
	LD	A,250		; TIME CONSTANT
	OUT	(033H),A	;

	IF HARDSK
;	set PIO for hard disk interrupt

	ld	b,piolen	;length of commands to PIO
	ld	c,hdipt		;port address for PIO for hard disk interrupts
	ld	hl,piocmds	;point to PIO commands
	otir			;issue commands to PIO
	ENDIF
	
	;; IF	HARDSK
	;; XOR	A		;ZERO ACCUMULATOR
	;; LD	(HSTACT),A	;SET HOST BUFFER INACTIVE
	;; LD	(UNACNT),A	;SET UNALLOCATED COUNT TO Z

	;; LD	HL,HSTBUF-1	;SETUP WRITE CONTROL BYTE F
	;; LD	(HL),00DH	;
	;; ENDIF

	RET			;

STMVTR:
	OUT	(BANKPT),A
	LD	A,0C3H		; SET VECTORS FOR BDOS
	LD	(0),A		;  JMP INSTRUCTION
	LD	HL,(SVDJT)	;
	LD	(1),HL
	;; call	regdump2
	LD	HL,(SVDBPA)
	LD	(HL),A
	INC	HL
	LD	(HL),E
	INC	HL
	LD	(HL),D
	RET			;

SVDJT:	DS	2		; SAVED DIRECT JUMP TABLE ADDRESS
SVDBPA:	DS	2		; SAVED BREAK POINT ADDRESS

	include "debug.asm"

	END
