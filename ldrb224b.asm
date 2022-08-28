.z80
	title	"LDRB224B, Copyright 1981, Altos Computer Systems -  3 Apr 81"

;; 	name	('LDRB2B')

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
;	Version Number:	2.24
;	Version Date:	April 3, 1981
;
;	This module of the CP/M 2.24 Loader BIOS contains the console
;	output routine.
;
;----------------------------------------------------------------------

	;; public	conout

	page	62
;----------------------------------------------------------------------
;
;	Various equates
;
;----------------------------------------------------------------------

statpt	equ	01dh		;SIO status port for console
datapt	equ	01ch		;SIO data port for console


;----------------------------------------------------------------------
;
;	CONSOLE OUTPUT entry point to CBIOS.
;	<C> contains character to be written.
;
;----------------------------------------------------------------------

conout:
	ld	a,010h		;reset EXT/STATUS interrupts
	out	(statpt),a	;issue command to Z80-SIO
	in	a,(statpt)	;read status
	and	00ch		;look for DCD (wired to DTR) and xmit ready
	cp	00ch		;both must be set
	jr	nz,conout	;if not...branch

	ld	a,c
	out	(datapt),a	;send data character on its way
	ret			;exit

;; 	end
