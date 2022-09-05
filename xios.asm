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
;	MACLIB	DISKDEF	; Replaced by include statement
;	MACLIB	Z80S	; Not needed, have a Z80 assembler

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

MEMPORT	EQU	009H	; MEMORY SELECT PORT
MEMSK	EQU	002H	; MEMORY SELECT MASK

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
	JP	debuga;COLDSTART	;COLD START
	endif
WBOTE:
	JP	debugb;WARMSTART	;WARM START
	JP	CONST		;CONSOLE STATUS - called before banner 2
	JP	CONIN		;CONSOLE CHARACTER IN
	JP	CONOUT		;CONSOLE CHARACTER OUT - called before banner 6
	JP	debugf;LIST		;LIST CHARACTER OUT - THIS
;				;  "CLIST" IF SETUP PROGRAM
;				;  PARALLEL PRINTER PORT

	JP	debugg;RTNEMPTY	;PUNCH NOT IMPLEMENTED
	JP	debugh;RTNEMPTY	;READER NOT IMPLEMENTED
	JP	home		;MOVE HEAD TO HOME - called after banner 1
	JP	SELDSK		;SELECT DISK - called before banner 1
	JP	SETTRK		;SET TRACK NUMBER - called after banner 2
	JP	SETSEC		;SET SECTOR NUMBER - called after banner 4
	JP	SETDMA		;SET DMA ADDRESS - called before banner 4
	JP	READ		;READ DISK - called after banner 5
	JP	debugo;WRITE		;WRITE DISK
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
	DW	FLOPPY_INT	;FLOPPY DISK INTERR
	DW	NULL_INT	;
	DW	NULL_INT	;
	DW	NULL_INT	;
	DW	INT1HND		;CTC INTERRUPT
	DW	NULL_INT	;
	DW	HARD_INT	;HARD DISK INTERRUP
	DW	NULL_INT	;
	DW	NULL_INT	;

	if	~~mpm20
NULL_INT:
	EI
	RETI
	endif

	include	"floppy.asm"
	include "hard.asm"

;-----------------------------------------------------------------------
;
;	THE FOLLOWING AREA CONTAINS THE DISK/WORK SAVE AREAS
;	USED BY THE CBIOS IN THE NORMAL COURSE OF ACTIVITY.
;
;-----------------------------------------------------------------------

	if	mpm20
;tempbuf	equ	(dirbuf-base)+128
	else
TEMPBUF	EQU	(DIRBUF-BASE)+256
	ORG TEMPBUF+((INITEND-BASE)/TEMPBUF)*((INITEND-BASE
	endif

BEGDAT	EQU	$			;START OF BDOS AREAS
;DIRBUF:	DS	128		;OVERLAYS SYSTEMINIT CODE
ALV0:	DS	32
CSV0:	DS	32
ALV1:	DS	32
CSV1:	DS	32
ALV2:	DS	32
CSV2:	DS	32
ALV3:	DS	32
CSV3:	DS	32
	IF	HARDSK
ALV4:	DS	64
CSV4:	;DS	0
ALV5:	DS	64
CSV5:	;DS	0
ALV6:	DS	64
CSV6:	;DS	0
ALV7:	DS	64
CSV7:	;DS	0
ALV8:	DS	64
CSV8:	;DS	0
ALV9:	DS	64
CSV9:	;DS	0
ALVA:	DS	36
CSVA:	;DS	0
ALVB:	DS	36
CSVB:	;DS	0
	endif

	if	mdisk
ALVC:	DS	32			;VIRTUAL DISK
CSVC:	;DS	0
	endif

	if	~~mpm20
	if	hardsk
	DS	1			;MUST PRECEDE HSTBU
HSTBUF:	DS	1024			;HOST BUFFER AREA
	DS	1			;MUST FOLLOW HSTBUF
	ENDIF

FPYBUF	EQU	DIRBUF+128		;FLOPPY I/O BUFFER
	endif

NEWDSK:	DS	1			;SEEKDISK NUMBER
NEWTRK:	DS	2			;SEEK TRACK NUMBER
NEWSEC:	DS	1			;SEEK SECTOR NUMBER

HSTDSK:	DS	1			;HOST DISK NUMBER
HSTTRK:	DS	2			;HOST TRACK NUMBER
HSTSEC:	DS	1			;HOST SECTOR NUMBER

NEWHST:	DS	1			;SEEK SHR SECSHF
HSTACT:	DS	1			;HOST ACTIVE FLAG
HSTWRT:	DS	1			;HOST WRITTEN FLAG

UNACNT:	DS	1			;UNALLOCATED RECORD COUNT
UNADSK:	DS	1			;LAST UNALLOCATED DISK
UNATRK:	DS	2			;LAST UNALLOCATED TRACK
UNASEC:	DS	1			;LAST UNALLOCATED SECTOR

ERFLAG:	DS	1			;ERROR REPORTING
RSFLAG:	DS	1			;READ SECTOR FLAG
READOP:	DS	1			;1 IF READ OPERATION
WRTYPE:	DS	1			;WRITE OPERATION TYPE

CMD:	DB	0			;COMMANDS FOR NEXT I/O
MASK:	DB	0			;STATUS MASKS BUFFER FOR DISK I/O
STATUS:	DB	0			;STATUS SAVE LOCATION FOR DISK I/O

SAVE1:	DB	000H,000H,000H,000H	;SAVE AREA FOR NMI ROUTINE
P_RETRIES: DB	000H			;COUNTER FOR PERMANENT ERRORS
T_RETRIES: DB	000H			;COUNTER FOR TEMPORARY ERRORS
HOME_TOGGLE:
	DB	000H			;INDICATOR TO TELL HARD DISK ERROR RECOVERY
;					;.. IF HOME SHOULD BE DONE BEFORE NEXT I/O

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

rtnempty:
	xor	a
	ret

NULL_INT:
	EI
	RETI
	endif

;-----------------------------------------------------------------------
;
;	CENTRONICS PRINTER ROUTINE (WITH SEPARATE BUSY TEST FOR SPOOLER)
;
;-----------------------------------------------------------------------

CNSTAT:
	LD	A,001H		;TO SET STROBE HIGH
	OUT	(010H),A	;
	IN	A,(010H)	;READ PRINTER STATUS
	AND	020H		;REMOVE ALL BUT BUSY BIT
	LD	A,0FFH		;ASSUME NOT BUSY
	RET	Z		;CHECK ASSUMPTION
	XOR	A		;SET TO SHOW STILL BUSY
	RET			;
;
CLIST:
	CALL	CNSTAT		;IS PRINTER READY NOW?
	OR	A
	JR	NZ,CLIST1	;IF READY, SKIP POLL

	PUSH	BC		;
	LD	C,POLL		; POLL DEVICE
	LD	E,PLLPT		;  PRINTER
	CALL	XDOS		;WAIT FOR PRINTER TO FREE U
	POP	BC		;

CLIST1:
	LD	A,C		;CHARACTER TO PRINT
	OUT	(011H),A	;WRITE IT TO DATA PORT
	LD	A,000H		;TO FORCE STROBE LOW
	OUT	(010H),A	;
	LD	A,001H		;TO FORCE STROBE HIGH
	OUT	(010H),A	;
	RET			;

;-----------------------------------------------------------------------
;
;	DISK INTERRUPT ROUTINE
;
;-----------------------------------------------------------------------

FLOPPY_INT:
	LD	(SVDHL),HL
	LD	HL,FDINTH
	JP	INTINIT
FDINTH:
	IN	A,(004H)	;GET STATUS
	LD	(STATUS),A	;SAVE FOR I/O ROUTINE
;	call	regdump
	LD	A,0		;STOP TIMING OF RESPONSE TO
	LD	(FPYTIME+1),A	;
	LD	E,FPYFLAG	;SHOW I/O COMPLETED
	JR	HDSTFLG

HARD_INT:
	LD	(SVDHL),HL
	LD	HL,HDINTH
	JP	INTINIT
HDINTH:
	IN	A,(024H)	;GET STATUS
	LD	(STATUS),A	;SAVE FOR CHECK LATER

	XOR	A
	OUT	(023H),A	;RESET INTERRUPT BY RELOADI

	LD	E,HDFLAG	;SHOW I/O COMPLETED
HDSTFLG:
	LD	C,FLAGST
	CALL	XDOS
	JP	INTDONE

;-----------------------------------------------------------------------
;
;	CONSOLE DISPLAY ROUTINES
;
;-----------------------------------------------------------------------

CONST:				; CONSOLE STATUS
	CALL	PTBLJMP		; COMPUTE AND JUMP TO HNDLR
	DW	PT0ST		; CONSOLE #0 STATUS ROUTINE
	DW	PT1ST		; CONSOLE #1 STATUS ROUTINE
	DW	PT2ST		; CONSOLE #2 STATUS ROUTINE
	DW	PT3ST		; CONSOLE #3 STATUS ROUTINE

CONIN:				; CONSOLE INPUT
	CALL	PTBLJMP		; COMPUTE AND JUMP TO HNDLR
	DW	PT0IN		; CONSOLE #0 INPUT
	DW	PT1IN		; CONSOLE #1 INPUT
	DW	PT2IN		; CONSOLE #2 INPUT
	DW	PT3IN		; CONSOLE #3 INPUT

CONOUT:				; CONSOLE OUTPUT
	CALL	PTBLJMP		; COMPUTE AND JUMP TO HNDLR
	DW	PT0OUT		; CONSOLE #0 OUTPUT
	DW	PT1OUT		; CONSOLE #1 OUTPUT
	DW	PT2OUT		; CONSOLE #2 OUTPUT
	DW	PT3OUT		; CONSOLE #3 OUTPUT

PTBLJMP:			; COMPUTE AND JUMP TO HANDLER
				; D = CONSOLE #
				; DO NOT DESTROY <D>
	LD	A,D
	CP	NMBCNS
	JR	C,TBLJMP
	POP	AF		; THROW AWAY TABLE ADDRESS
	XOR	A
	RET
TBLJMP:				; COMPUTE AND JUMP TO HANDLER
				; A = TABLE INDEX
	ADD	A,A		; DOUBLE TABLE INDEX FOR ADR OFFST
	POP	HL		; RETURN ADR POINTS TO JUMP TBL
	LD	E,A
	LD	D,0
	ADD	HL,DE		; ADD TABLE INDEX * 2 TO TBL BASE
	LD	E,(HL)		; GET HANDLER ADDRESS
	INC	HL
	LD	D,(HL)
	EX	DE,HL
	JP	(HL)		; JUMP TO COMPUTED CNS HANDLER

;-----------------------------------------------------------------------
;
;	SERIAL PORT ADDRESS EQUATES
;
;-----------------------------------------------------------------------

DATA0	EQU	01CH		;CONSOLE #0 DATA
STS0	EQU	DATA0+1		;CONSOLE #0 STATUS
DATA1	EQU	02CH		;CONSOLE #1 DATA
STS1	EQU	DATA1+1		;CONSOLE #1 STATUS
DATA2	EQU	02EH		;CONSOLE #2 DATA
STS2	EQU	DATA2+1		;CONSOLE #2 STATUS
DATA3	EQU	02AH		;CONSOLE #3 DATA
STS3	EQU	DATA3+1		;CONSOLE #3 STATUS
LPTPRT0	EQU	01EH		;PRINTER #0 DATA
LPTSTS0	EQU	LPTPRT0+1	;PRINTER #0 STATUS
LPTPRT1	EQU	028H		;PRINTER #1 DATA
LPTSTS1	EQU	LPTPRT1+1	;PRINTER #1 STATUS


;-----------------------------------------------------------------------
;
;	POLL CONSOLE  # 0  INPUT
;
;-----------------------------------------------------------------------

POLCI0:
PT0ST:				; TEST CONSOLE STATUS
	XOR	A		;  RETURN  0FFH IF READY
	OUT	(STS0),A	;          000H IF NOT
	IN	A,(STS0)	;
	AND	1		;  RX CHAR ?
	RET	Z		;   NO
	LD	A,0FFH		;   YES - SET FLAG
	RET			;

;-----------------------------------------------------------------------
;
;	CONSOLE  # 0  INPUT
;
;-----------------------------------------------------------------------
;
PT0IN:				; RETURN CHAR IN REG A
	CALL	POLCI0		;IS IT READY NOW?
	OR	A		;
	JR	NZ,PT0IN1	;IF READY, SKIP POLL
	LD	C,POLL		;
	LD	E,PLCI0		; POLL CONSOLE #0 INPUT
	CALL	XDOS		;
PT0IN1:	IN	A,(DATA0)	; READ CHARACTER
	AND	7FH		; STRIP PARITY
	RET			;

;-----------------------------------------------------------------------
;
;	CONSOLE  # 0  OUTPUT
;
;-----------------------------------------------------------------------

PT0OUT:				; REG C = CHAR TO OUTPUT
	CALL	POLCO0		;IS IT READY NOW?
	OR	A		;
	JR	NZ,PT0OUT1	;IF READY, SKIP POLL
	PUSH	BC		;
	LD	C,POLL		;
	LD	E,PLCO0		;
	CALL	XDOS		; POLL CONSOLE #0 OUTPUT
	POP	BC		;
PT0OUT1:
	LD	A,C		;
	OUT	(DATA0),A	; TRANSMIT CHARACTER
	RET			;
;
;
;-----------------------------------------------------------------------
;
;	POLL CONSOLE  # 0  OUTPUT
;
;-----------------------------------------------------------------------
;
POLCO0:				; RETURN 0FFH IF READY
	LD	A,10H		;        000H IF NOT
	OUT	(STS0),A	; RESET INT BIT
	IN	A,(STS0)	; READ STATUS
	AND	0CH		; MASK FOR DTR AND TXE
	CP	0CH		; MUST HAVE BOTH
	LD	A,0		;
	RET	NZ		; RETURN NOT READY
	DEC	A		;CHANGE "A" TO 0FFH
	RET			; RETURN READY

;-----------------------------------------------------------------------
;
;	POLL CONSOLE  # 1  INPUT
;
;-----------------------------------------------------------------------

POLCI1:
PT1ST:				; TEST CONSOLE STATUS
	XOR	A		;  RETURN 0FFH IF READY
	OUT	(STS1),A	;	  000H IF NOT
	IN	A,(STS1)	;
	AND	1		;  RX CHAR ?
	RET	Z		;   NO
	LD	A,0FFH		;   YES - SET FLAG
	RET			;

;-----------------------------------------------------------------------
;
;	CONSOLE  # 1  INPUT
;
;-----------------------------------------------------------------------

PT1IN:				; RETURN CHAR IN REG A
	CALL	POLCI1		;READY NOW?
	OR	A		;
	JR	NZ,PT1IN1	;IF READY, SKIP POLL
	LD	C,POLL		;
	LD	E,PLCI1		; POLL CONSOLE #1 INPUT
	CALL	XDOS		;
PT1IN1:	IN	A,(DATA1)	; READ CHARACTER
	AND	7FH		; STRIP PARITY
	RET			;
;
;
;-----------------------------------------------------------------------
;
;	CONSOLE  # 1  OUTPUT
;
;-----------------------------------------------------------------------
;
PT1OUT:				; REG C = CHAR TO OUTPUT
	CALL	POLCO1		;ARE WE READY NOW?
	OR	A		;
	JR	NZ,PT1OUT1	;IF READY, SKIP POLL
	PUSH	BC		;
	LD	C,POLL		;
	LD	E,PLCO1		;
	CALL	XDOS		; POLL CONSOLE #1 OUTPUT
	POP	BC		;
PT1OUT1:
	LD	A,C		;
	OUT	(DATA1),A	; TRANSMIT CHARACTER
	RET			;

;-----------------------------------------------------------------------
;
;	POLL CONSOLE  # 1  OUTPUT
;
;-----------------------------------------------------------------------

POLCO1:				; RETURN 0FFH IF READY
	LD	A,10H		;	 000H IF NOT
	OUT	(STS1),A	; RESET INT BIT
	IN	A,(STS1)	; READ STATUS
	AND	0CH		; MASK FOR DTR AND TXE
	CP	0CH		; MUST HAVE BOTH
	LD	A,0		;
	RET	NZ		; RETURN NOT READY
	DEC	A		;CHANGE "A" TO 0FFH
	RET			; RETURN READY

;-----------------------------------------------------------------------
;
;	POLL CONSOLE  # 2  INPUT
;
;-----------------------------------------------------------------------

POLCI2:
PT2ST:				; TEST CONSOLE STATUS
	XOR	A		;  RETURN 0FFH IF READY
	OUT	(STS2),A	;	  000H IF NOT
	IN	A,(STS2)	;
	AND	1		;  RX CHAR ?
	RET	Z		;   NO
	LD	A,0FFH		;   YES - SET FLAG
	RET			;
;
;-----------------------------------------------------------------------
;
;	CONSOLE  # 2  INPUT
;
;-----------------------------------------------------------------------
;
PT2IN:				; RETURN CHAR IN REG A
	CALL	POLCI2		;READY NOW?
	OR	A		;
	JR	NZ,PT2IN1	;IF READY, SKIP POLL
	LD	C,POLL		;
	LD	E,PLCI2		; POLL CONSOLE #2 INPUT
	CALL	XDOS		;
PT2IN1:	IN	A,(DATA2)	; READ CHARACTER
	AND	7FH		; STRIP PARITY
	RET			;
;
;-----------------------------------------------------------------------
;
;	CONSOLE  # 2  OUTPUT
;
;-----------------------------------------------------------------------
;
PT2OUT:				; REG C = CHAR TO OUTPUT
	CALL	POLCO2		;READY NOW?
	OR	A		;
	JR	NZ,PT2OUT1	;IF READY, SKIP POLL
	PUSH	BC		;
	LD	C,POLL		;
	LD	E,PLCO2		;
	CALL	XDOS		; POLL CONSOLE #2 OUTPUT
	POP	BC		;
PT2OUT1:
	LD	A,C		;
	OUT	(DATA2),A	; TRANSMIT CHARACTER
	RET			;
;
;
;-----------------------------------------------------------------------
;
;	POLL CONSOLE  # 2  OUTPUT
;
;-----------------------------------------------------------------------
;
POLCO2:				; RETURN 0FFH IF READY
	LD	A,10H		;	 000H IF NOT
	OUT	(STS2),A	; RESET INT BIT
	IN	A,(STS2)	; READ STATUS
	AND	0CH		; MASK FOR DTR AND TX
	CP	0CH		; MUST HAVE BOTH
	LD	A,0		;
	RET	NZ		; RETURN NOT READY
	DEC	A		;CHANGE "A" TO 0FFH
	RET			; RETURN READY

;-----------------------------------------------------------------------
;
;	POLL CONSOLE  # 3  INPUT
;
;-----------------------------------------------------------------------

POLCI3:
PT3ST:				; TEST CONSOLE STATUS
	XOR	A		;  RETURN  0FFH IF READY
	OUT	(STS3),A	;          000H IF NOT
	IN	A,(STS3)	;
	AND	1		;  RX CHAR ?
	RET	Z		;   NO
	LD	A,0FFH		;   YES - SET FLAG
	RET			;

;-----------------------------------------------------------------------
;
;	CONSOLE  # 3  INPUT
;
;-----------------------------------------------------------------------

PT3IN:				; RETURN CHAR IN REG A
	CALL	POLCI3		;READY NOW?
	OR	A		;
	JR	NZ,PT3IN1	;IF READY, SKIP POLL
	LD	C,POLL		;
	LD	E,PLCI3	; POLL CONSOLE #3 INPUT
	CALL	XDOS		;
PT3IN1:	IN	A,(DATA3)	; READ CHARACTER
	AND	7FH		; STRIP PARITY
	RET			;
;
;
;-----------------------------------------------------------------------
;
;	CONSOLE  # 3  OUTPUT
;
;-----------------------------------------------------------------------
;
PT3OUT:				; REG C = CHAR TO OUTPUT
	CALL	POLCO3		;READY NOW?
	OR	A		;
	JR	NZ,PT3OUT1	;IF READY, SKIP POLL
	PUSH	BC		;
	LD	C,POLL		;
	LD	E,PLCO3		;
	CALL	XDOS		; POLL CONSOLE #3 OUTPUT
	POP	BC		;
PT3OUT1:
	LD	A,C		;
	OUT	(DATA3),A	; TRANSMIT CHARACTER
	RET
;
;
;-----------------------------------------------------------------------
;
;	POLL CONSOLE  # 3  OUTPUT
;
;-----------------------------------------------------------------------
;
POLCO3:				; RETURN 0FFH IF READY
	LD	A,10H		;	 000H IF NOT
	OUT	(STS3),A	; RESET INT BIT
	IN	A,(STS3)	; READ STATUS
	AND	0CH		; MASK FOR DTR AND TXE
	CP	0CH		; MUST HAVE BOTH
	LD	A,0		;
	RET	NZ		; RETURN NOT READY
	DEC	A		;CHANGE "A" TO 0FFH
	RET			; RETURN READY

;-----------------------------------------------------------------------
;
;	LINE PRINTER  #0  DRIVER
;
;-----------------------------------------------------------------------
;
LIST:				;LIST OUTPUT #0
	CALL	POLLPT
	OR	A		;IS PRINTER READY NOW?
	JR	NZ,LIST1	;IF READY, SKIP POLL

	PUSH	BC
	LD	C,POLL		; POLL PRINTER STATUS
	LD	E,PLLPT		;
	CALL	XDOS		;
	POP	BC		;

LIST1:
	LD	A,C		; CHARACTER TO PRINT
	OUT	(LPTPRT0),A	;
	RET			;

;
;-----------------------------------------------------------------------
;
;	POLL PRINTER OUTPUT
;
;-----------------------------------------------------------------------
;
POLLPT:				; RETURN 0FFH IF READY
	LD	A,10H		;	 000H IF NOT
	OUT	(LPTSTS0),A	; RESET INT BIT
	IN	A,(LPTSTS0)	; READ STATUS
	AND	0CH		; MASK FOR DTR AND TXE
	CP	0CH		; MUST HAVE BOTH
	LD	A,0		;
	RET	NZ		; RETURN NOT READY
	DEC	A		;CHANGE "A" TO 0FFH
	RET			; RETURN READY
;

;
;  MP/M 1.0   EXTENDED I/O SYSTEM
;
;

POLLDEVICE:
				; REG C = DEVICE # TO BE POLLED
				; RETURN 0FFH IF READY,
				;        000H IF NOT
	LD	A,C
	CP	NMBDEV
	JR	C,DEVOK
	LD	A,NMBDEV	; IF DEV # >= NMBDEV,
				; SET TO NMBDEV
DEVOK:
	CALL	TBLJMP		; JUMP TO DEV POLL CODE

DEVTBL:
	DW	POLLPT		; POLL PRINTER OUTPUT - THIS WILL P
				;  SPECIFIED PARALLEL PORT FOR PRIN
	DW	POLCO0		; POLL CONSOLE #0 OUTPUT
	DW	POLCO1		; POLL CONSOLE #1 OUTPUT
	DW	POLCO2		; POLL CONSOLE #2 OUTPUT
	DW	POLCO3		; POLL CONSOLE #3 OUTPUT
	DW	POLCI0		; POLL CONSOLE #0 INPUT
	DW	POLCI1		; POLL CONSOLE #1 INPUT
	DW	POLCI2		; POLL CONSOLE #2 INPUT
	DW	POLCI3		; POLL CONSOLE #3 INPUT
NMBDEV	EQU	($-DEVTBL)/2
	DW	RTNEMPTY	; BAD DEVICE HANDLER

; SELECT / PROTECT MEMORY

SELMEMORY:
				; REG BC = ADR OF MEM DESCRIPTOR
				; BC -> BASE   1 BYTE,
				;	SIZE   1 BYTE,
				;	ATTRIB 1 BYTE,
				;	BANK   1 BYTE.
				;
				;   BIOS TABLE MODIFIED
	CP	20H		;
	JP	Z,$
	LD	HL,3		; POINT TO BANK
	ADD	HL,BC		;
	LD	A,(HL)		;  GET IT
	LD	(BANKNO),A	; SAVE BANK NUMBER
	RLA			;
	RLA			;
	RLA			;
	AND	018H		; MASK FOR PIO
	OR	MEMSK		;
	LD	(CURMEM),A	; STORE CURRENT BANK MASK
	OUT	(009H),A	; SET PIO
	RET

BANKNO:	DB	0		; LAST SELECTED MEMORY BANK NUMBER
CURMEM:	DB	0		; LAST SELECTED MEMORY BANK MASK

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
	LD	E,1
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
	LD	E,2
	CALL	XDOS		; SET FLAG #2 @ 1 SEC
	LD	HL,(FPYTIME)	;IS FLOPPY TIME CHECK IN EF
	LD	A,H		;
	OR	A		;
	JR	Z,NOT1SEC	;IF NOT IN EFFECT, FINISH
	DEC	L		;SUBTRACT A SECOND

	LD	(FPYTIME),HL	;SAVE FOR NEXT TIME
	JR	NZ,NOT1SEC	;IF NOT TOO LONG, FINISH
	LD	H,L		;ZERO OUT INDICATOR
	LD	(FPYTIME),HL	;PREVENT RE-ENTRY OF THIS R
	LD	C,FLAGST	;
	LD	E,FPYFLAG	;
	CALL	XDOS		;CAUSE I/O FOR FLOPPY TO CO
	LD	A,10010000B
	LD	(STATUS),A	;SHOW ERROR IN FLOPPY I/O
	LD	HL,(FPYTCNT)
	INC	HL		;COUNT TIMES WD1791 GOES TO
	LD	(FPYTCNT),HL	;

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

;-----------------------------------------------------------------------
;
;	THESE ARE THE DISK TYPE DEFINITION BLOCKS
;	EACH OF WHICH CORRESPONDS TO A PARTICULAR MODE.
;
;-----------------------------------------------------------------------

DPB0	EQU	$		;VERSION 2.0, IBM SINGLE DE
	DW	26		;SECTORS PER TRACK
	DB	3		;BLOCK SHIFT
	DB	7		;BLOCK SHIFT MASK
	DB	0		;EXTENT MASK
	DW	242		;DISK SIZE MINUS 1
	DW	63		;DIRECTORY MAX
	DB	192		;ALLOC0
	DB	0		;ALLOC1
	DW	16		;CHECK AREA SIZE
	DW	2		;OFFSET TO START TRACK

DPB1	EQU	$		;VERSION 2.0, IBM DOUBLE DE
	DW	52		;SECTORS PER TRACK
	DB	4		;BLOCK SHIFT
	DB	15		;BLOCK SHIFT MASK
	DB	1		;EXTENT MASK
	DW	242		;DISK SIZE MINUS 1
	DW	127		;DIRECTORY MAX
	DB	192		;ALLOC0
	DB	0		;ALLOC1
	DW	32		;CHECK AREA SIZE
	DW	2		;OFFSET TO START TRACK

DPB2	EQU	$		;VERSION 1.4 ALTOS DOUBLE D
	DW	48		;SECTORS PER TRACK
	DB	4		;BLOCK SHIFT
	DB	15		;BLOCK SHIFT MASK
	DB	0		;EXTENT MASK (1.4 COMPATABI
	DW	224		;DISK SIZE MINUS 1
	DW	95		;DIRECTORY MAX
	DB	192		;ALLOC0
	DB	0		;ALLOC1
	DW	24		;CHECK AREA SIZE
	DW	2		;OFFSET TO START TRACK

	IF	HARDSK
	if	mpm20
DPB3:	DISKDEF	3,0,127,,16384,512,512,0,1,,0

DPB4:	DISKDEF	4,0,127,,16384,512,512,0,513,,0

DPB5:	DISKDEF 5,0,127,,16384,512,512,0,1025,,0

DPB6:	DISKDEF	6,0,127,,16384,288,512,0,513,,0
	else
DPB3:	DISKDEF	3,0,127,,16384,512,512,0,1

DPB4:	DISKDEF	4,0,127,,16384,512,512,0,513

DPB5:	DISKDEF	5,0,127,,16384,512,512,0,1025

DPB6:	DISKDEF	6,0,127,,16384,288,512,0,513
	endif

	ENDIF

	if	mdisk
DPB7	EQU	$		;VIRTUAL DISK
	DW	24		;SECTORS PER TRACK
	DB	3		;BLOCK SHIFT
	DB	7		;BLOCK SHIFT MASK
	DB	0		;EXTENT MASK
	DW	142		;DISK SIZE MINUS 1
	DW	63		;DIRECTORY MAX
	DB	0C0H		;ALLOCO
	DB	0		;ALLOC1
	DW	0		;CHECK AREA SIZE
	DW	0		;OFFSET TO START TRACK
	endif

;
;	MOVE SUBROUTINE
;

	if	hardsk
RWMOVE:
	push	de
	push	hl
	call	swtuser		;switch in user bank
	pop	hl
	pop	de
	ld	bc,128
	LDIR			;MOVE DATA TO/FROM BUFFER
	call	swtsys		;switch system back in
;
;	DATA HAS BEEN MOVED TO/FROM HOST BUFFER
;
	LD	A,(WRTYPE)	;WRITE TYPE ??

	if	mpm20
	and	WRDIR		;TO DIRECTORY ??
	JR	Z,RWEND		;NO, JUST END UP HERE
	else
	CP	WRDIR		;TO DIRECTORY ??
	JR	NZ,RWEND	;NO, JUST END UP HERE
	endif

;
;	CLEAR HOST BUFFER FOR DIRECTORY WRITE
;

	LD	A,(ERFLAG)	;CHECK PRIOR TO DIR ACTIVIT
	OR	A		;ERRORS ??
	JR	NZ,RWEND	;SKIP IF SO....
	XOR	A		;ZERO TO ACCUMULATOR
	LD	(HSTWRT),A	;BUFFER WRITTEN
	CALL	WRITEHST	;

RWEND:
	LD	A,(ERFLAG)	;
	OR	A		;IF ERRORS, RESET SO NO MAT
	RET	Z		;NONE, JUST RETURN
	LD	HL,HSTDSK	;
	LD	(HL),0FFH	;CANT POSSIBLY MATCH, MUST
	ENDIF
	RET			;

MVDTB:
	LD	HL,(DMAADR)	; MOVE DATA TO FLOPPY BUFFER
	push	hl
	call	SWTUSER		;switch in user bank,
	pop	hl		;   cannot access non-common BNKXIOS
	LD	DE,FPYBUF	;
	LD	BC,128		; 128 BYTES
	LDIR			;
	jp	swtsys		;switch system back in
	RET			;

MVDFB:
	call	regdump
	PUSH	AF		;MOVE DATA FROM FLOPPY BUFFER
	LD	A,(CMD)		;
	AND	20H		; CHECK FOR READ
	JR	NZ,MVDFX	; NO - BYPAS MOVE
	LD	HL,(DMAADR)	;
	push	hl
	call	swtuser		;switch in user bank,
	pop	de		;   cannot access non-common BNKXIOS
	LD	HL,FPYBUF	;
	LD	BC,128		; 128 BYTES
	LDIR			;
	call	swtsys		;switch system back in
MVDFX:	POP	AF		;
	push	af
	push	hl
	push	bc
	ld	hl,dmaadr
	call	dumprec
	pop	bc
	pop	hl
	pop	af
	call	regdump
	RET			;

	IF	HARDSK

	DS	1		;MUST PRECEDE HSTBU
HSTBUF:	DS	1024		;HOST BUFFER AREA
	DS	1		;MUST FOLLOW HSTBUF
	ENDIF

	if	mpm20
dirbuf	equ	$
fpybuf	equ	dirbuf+128
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

	if	~~MDISK
	ld	hl,(SYSDAT)
	ld	l,15		;hl = .nmbmemsegs
	ld	b,(hl)		;b =  nmbmemsegs
TEST_BANK_SETUP_LOOP:
	inc	hl
	inc	hl
	inc	hl
	inc	hl		;hl = .memseg(i). bank
	ld	a,(hl)
	or	a
	jp	nz,BANK_SETUP
	dec	b
	jp	nz,TEST_BANK_SETUP_LOOP
	jp	AFTER_BANK_SETUP
BANK_SETUP:
	LD	A,01AH		; SELECT BANK 3
	CALL	STMVTR		; SET UP VECTORS
	LD	A,012H		; SELECT BANK 2
	CALL	STMVTR		; SET UP VECTORS
	LD	A,00AH		; SELECT BANK 1
	CALL	STMVTR		; SET UP VECTORS
AFTER_BANK_SETUP:
	else
	ld	a,lah		; bank 3 select for directo
	out	(09h),a
	ld	hl,0bffeh
	ld	a,0e5h
	cp	(hl)
	inc	hl
	jr	nz,fill
	cp	(hl)
	jr	z,dontfill
FILL:
	ld	(hl),a		;set directory initialized
	dec	hl
	ld	(hl),a

	ld	bc,07ffh	;first 2 k of bank one gets
	ld	hl,0
	ld	de,1
	ld	a,0ah		; select bank 1
	out	(09h),a
	ld	(hl),0e5h
	ldir
dontfill:
	endif
	LD	A,002H		; SELECT BANK 0
	CALL	STMVTR		; SET UP VECTORS

;;	ld	hl,LDRBIOSBASE+DENSITY_MASK_OFFSET
;; ;;;;;	LD	HL,1737H	; MOVE PARAMETERS CHANGED B
;;	LD	DE,SEL0		;	THE SETUP PROGRAM
;;	LD	BC,4		;  4 SELECT MASKS
;;	LDIR			;
;;	LD	DE,MODE		;
;;	LD	BC,4		;  4 MODE BYTES
;;	LDIR			;
	ld	hl,(LDRBIOSBASE+MISC_PARAMS_OFFSET)
;;;;;	LD	HL,(17BBH)	; GET MISC. PARAMETERS
	LD	(MPARMS),HL	;
	LD	A,(MPARMS)	; NOW TEST FOR CENTRONICS P
	AND	2		;
	JR	Z,PRTOK		;    NO - LEAVE SERIAL
	LD	HL,CLIST	;
	LD	(WBOTE+13),HL	; CHANGE PRINTER ROUTINE
	LD	HL,CNSTAT	;   AND STATUS CHECK
	LD	(DEVTBL),HL	;
	LD	A,003H		;INITIALIZE PARALLEL PORT
	OUT	(013H),A
	LD	A,00FH		;
	OUT	(013H),A

PRTOK:
	LD	BC,003H		;SET THE MODE FOR DRIVES IN
MODESET:
	CALL	SELSDP		;SELECT DRIVE FOR MODESET
	LD	HL,MODE		;
	ADD	HL,BC		;POINT TO CORRECT MODE BYTE
	PUSH	BC		;SAVE COUNT OF DRIVES
	LD	B,C		; B = DRIVE #
	LD	C,(HL)		;
	CALL	XETMOD		;SET MODE
	POP	BC		;
	DEC	C		;END OF LIST YET ??
	JP	P,MODESET	;SET MODE FOR ALL DRIVES
	CALL	SDCONF		;SET DISK CONFIGURATION

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
	DB	0EDH,047H	;---- FAKE STAI INSTRUCTION
	LD	A,60H		; SET VECTOR FOR CTC
	OUT	(30H),A		; CTC CHANNEL 0
	LD	A,0A7H		; RESET  /  LOAD TIME CONST
	OUT	(33H),A		; CHANNEL 3
	LD	A,250		; TIME CONSTANT
	OUT	(033H),A	;

	IF	HARDSK
	XOR	A		;ZERO ACCUMULATOR
	LD	(HSTACT),A	;SET HOST BUFFER INACTIVE
	LD	(UNACNT),A	;SET UNALLOCATED COUNT TO Z

	LD	HL,HSTBUF-1	;SETUP WRITE CONTROL BYTE F
	LD	(HL),00DH	;
	ENDIF

	RET			;

STMVTR:
	OUT	(MEMPORT),A
	LD	A,0C3H		; SET VECTORS FOR BDOS
	LD	(0),A		;  JMP INSTRUCTION
	LD	HL,(SVDJT)	;
	LD	(1),HL
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
	
	if	mpm20
xiosend	equ	$
fdbuf	equ	(dirbuf-base) +256
	org fdbuf+((xiosend-base)/fdbuf)*((xiosend-base)-fdbuf)
	db	0
	endif

	END
