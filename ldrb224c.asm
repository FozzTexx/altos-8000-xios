.z80
	title	"LDRB224C, Copyright 1981, Altos Computer Systems - 27 Feb 81"

;; 	name	('LDRB2C')

;----------------------------------------------------------------------
;
;	Altos Computer Systems
;	2360 Bering Drive
;	San Jose, California 95131
;
;	(408) 946-6700
;
;	Copyright 1981, Altos Computer Systems
;
;	This program is a copyright program product of Altos Computer
;	Systems and is distributed to the owners of Altos ACS8000 series
;	computers for use on those systems only.  Any other use of this
;	software constitutes a breach of the copyright license to the
;	purchaser.
;
;	Version Number: 2.24
;	Version Date:	February 27, 1981
;
;	This module of the CP/M 2.24 Loader BIOS contains the interrupt
;	handlers.
;
;----------------------------------------------------------------------

	;; public	dskint
	;; public	fdstat,fioflg

	page	62
;----------------------------------------------------------------------
;
;	Various equates
;
;----------------------------------------------------------------------

dskpio	equ	008H		;port to monitor for disk interrupts
fdbit	equ	6		;interrupt indication from floppy disk
fdstpt	equ	004H		;port for status from floppy disk


;----------------------------------------------------------------------
;
;	All floppy interrupts are vectored here.  All
;	registers and status bits must be preserved so that the
;	interrupted task can be properly restarted.
;
;----------------------------------------------------------------------

dskint:
	push	af

	in	a,(fdstpt)	;get status and clear interrupt from
;				;.. floppy disk controller
	ld	(fdstat),a	;save status
	ld	a,0ffh
	ld	(fioflg),a	;change flag to show I/O complete

	pop	af		;restore registers
	ei			;allow interrupts
	reti			;exit and reset daisy chain

	newpage
fdstat:	db	0		;floppy disk status byte
fioflg:	db	0ffh		;initialize to show no I/O pending

;; 	end
