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

;; ptlb:				;Printer/Terminal Lookup Block
;; 	dw	pat		;address of printer address table
;; 	dw	tat		;address of terminal address table
;; 	db	lstptr		;number for last printer
;; numltm:	db	lstcon		;number for last terminal

;; pat:				;Printer Address Table
;; 	dw	sioptr		;NOTE: The addresses for the two printers on
;; 	dw	cenptr		;..    this list will be interchanged by
;; ;				;..    the cold boot routine if CPMSETUP
;; ;				;..    specified a Centronics type printer
;; lstptr	equ	($-pat)/2 - 1	;printer number for last printer

;; tat:				;Terminal Address Table
;; 	dw	con00
;; lstcon	equ	($-tat)/2 - 1	;console number for last console

;; sioptr	equ	$-4
;; 	dw	coutst		;address of output status routine
;; 	dw	coutda		;address of output data routine
;; 	db	01FH		;I/O status port number
;; 	db	01EH		;I/O data port number

;; cenptr	equ	$-4
;; 	dw	cenost
;; 	dw	cenoda
;; 	db	010H
;; 	db	011H

;; con00:
;; 	dw	cinst		;address of input status routine
;; 	dw	cindat		;address of input data routine
;; 	dw	coutst		;address of output status routine
;; 	dw	coutda		;address of output data routine
;; 	db	01DH		;I/O status port number
;; 	db	01CH		;I/O data port number
