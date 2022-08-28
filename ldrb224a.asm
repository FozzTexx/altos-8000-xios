.z80
	title	"LDRB224A, Copyright 1981, Altos Computer Systems -  17 Jun 81"

;	name	('LDRB2A')

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
;	Version Date:	April 10, 1981
;
;	Version Number: 2.24*
;	Version Date:	June 17, 1981
;		Change version logon message (to 2.24*).
;
;	This module of the CP/M 2.24 Loader CBIOS, contains the main jump
;	table, the interrupt vector table, the warm boot routine and the
;	cold boot routine.
;
;----------------------------------------------------------------------

	org	01700h
	
	;; public	bdos

	;; extrn	conout
	;; extrn	seldsk,home,settrk
	;; extrn	setsec,setdma,sectrn
	;; extrn	read

	;; extrn	dskint,mpt,bspit

	page	62
;----------------------------------------------------------------------
;
;	Various equates
;
;----------------------------------------------------------------------

cpmvers	equ	22		;CP/M version number
altosv	equ	4		;Altos cbios version number
revlvl	equ	'*'		;Revision level

cr	equ	00dh		;carriage return
lf	equ	00ah		;line feed
eom	equ	'$'		;end of message indicator

bdos	equ	$-00dfah	;BDOS entry point
ccp	equ	$-01600h	;base of CCP
biosln	equ	01200h		;length of CBIOS
bsplen	equ	12		;length of Bootstrap Parameter Info Table

iobyte	equ	003h		;location of Intel IOBYTE
cdisk	equ	004h		;location of current drive number
wrkspc	equ	0080h		;work area for move program
tbase	equ	0100h		;start of TPA

bankpt	equ	025h		;port for memory bank select

;	BDOS function codes
dspmsg	equ	9		;print (ie. display on CRT) string
rstcpm	equ	13		;reset disk system
open	equ	15		;open file
reads	equ	20		;read sequential
setmem	equ	26		;set DMA address

;----------------------------------------------------------------------
;
;	Your standard CBIOS jump table
;
;----------------------------------------------------------------------

cbstr:				;starting address of cbios
	jp	cstart		;cold start
wboot:
	jp	$		;no warm start in loader BIOS
	ld	a,0		;console status
	ret			;.. not supported in loader BIOS

	jp	$		;console character input
;				;.. not supported in loader BIOS

	jp	conout		;console character output
	ret			;list character out
	nop			;.. not supported in loader BIOS
	nop

	jp	$		;punch not implemented
	jp	$		;reader not implemented

	jp	home		;home disk
	jp	seldsk		;select a disk
	jp	settrk		;select a specific track
	jp	setsec		;select a specific sector
	jp	setdma		;set the "Disk Memory Access" address
	jp	read		;read the disk
	ld	a,1		;write to the disk
	ret			;.. not supported in loader BIOS

	ld	a,0		;list status
	ret			;.. not supported in loader BIOS

	jp	sectrn		;disk sector translate

nobios:
	ld	c,dspmsg
	ld	de,nomsg	;point to bad news message
	call	bdos		;display message
	di
	halt			;die

fcb:	db	0,'CBIOSxx COM',0,0,0,0
	db	0,0,0,0,0,0,0,0
	db	0,0,0,0,0,0,0,0
	db	0
fcbnam	equ	fcb+6

	org	( ( ($-cbstr) + 0a1h) & 0ff00h) + 05eh
;				;start of interrupt vector table
ivctbl:
	dw	dskint		;floppy and hard disk interrupt

	newpage
	dw	mpt		;the address to the Master Pointer Table
;				;.. must immediately precede the "cstart"
;				;.. label
cstart:
;----------------------------------------------------------------------
;
;	This routine receives control from the bootstrap loader sector
;	(ie. Track 0, Sector 1).  It initializes BDOS prior to attempting
;	to load the full CBIOS.  If the full CBIOS cannot be located,
;	a message is given to the operator.
;
;----------------------------------------------------------------------

	ld	sp,ccp		;put stack pointer out of the way

	xor	a
	ld	(iobyte),a
	ld	(cdisk),a	;initialize iobyte and cdisk

	ld	hl,ivctbl	;get address of vector table
	ld	a,h
	ld	i,a		;set the <I> register

;	Copy the CCP and BDOS code to location 0100H in bank 1.  The warm
;	boot routine in the regular CBIOS will use this copy to do the warm
;	boot.  This means that the warm boot will require no I/O.

	in	a,(bankpt)	;get current port value
	ld	b,a		;save
	and	0e7h		;mask out bank select
	or	008h
	out	(bankpt),a	;select bank 1
	ld	a,b		;get original port value

	ld	hl,ccp		;"from" address
	ld	de,tbase	;"to" address
	ld	bc,cbstr-ccp	;length of move
	ldir

	out	(bankpt),a	;select bank 0

;	Initialize BDOS

	ld	c,rstcpm
	call	bdos		;reset disk system

	ld	c,dspmsg
	ld	de,msg		;point to loader message
	call	bdos		;display on CRT

;				;try to find real CBIOS and load it
	ld	hl,(msz)	;load ASCII characters giving memory size
	ld	(fcbnam),hl	;put it in the FCB
	ld	(nomsz),hl	;put in error message in case CBIOS not found

	ld	c,open
	ld	de,fcb		;point to FCB
	call	bdos		;open file
	cp	0ffh		;check to see what happened
	jp	z,nobios	;if not found...branch

	ld	de,tbase	;point to start of TPA

bootlp:
	push	de		;save DMA address
	ld	c,setmem
	call	bdos		;set the DMA address

	ld	c,reads
	ld	de,fcb
	call	bdos		;read 128 bytes

	pop	de		;retrieve DMA address
	or	a		;check for end of file
	jr	nz,movinf	;if eof...branch

	ld	hl,128
	add	hl,de
	ex	de,hl		;point to next location for read
	jr	bootlp

movinf:
	ld	hl,bspit	;point to "from" location
	ld	de,wrkspc	;point to "to" location
	ld	bc,bsplen	;length of BSPIT
	ldir			;move parameter info to low memory

	ld	hl,movpgm	;point to "from" location
	ld	de,wrkspc+bsplen	;point to "to" location
	ld	bc,mplen	;get length of move
	ldir			;move the program which will move CBIOS

	jp	wrkspc+bsplen	;execute the move program

movpgm:				;this program will move CBIOS from the TPA
;				;.. to high memory
	ld	hl,tbase	;point to "from" location
	ld	de,cbstr	;point to "to" location (ie. overlay loader)
	ld	bc,biosln	;length of move
	ldir			;move CBIOS to proper location

	jp	cbstr		;jump to cold boot location
mplen	equ	$-movpgm	;length of code to do the move of CBIOS

	newpage
msg:
	db	cr,lf
msz:	db	'xx'		;size of this BIOS
	db	'K Altos Loader '
	db	cpmvers/10 + '0','.',cpmvers # 10 + '0'
	db	altosv + '0'
	db	revlvl
	db	eom

nomsg:
	db	cr,lf
	db	'CBIOS'
nomsz:	db	'xx'
	db	'.COM not found on cold boot disk.'
	db	eom

;; 	end
