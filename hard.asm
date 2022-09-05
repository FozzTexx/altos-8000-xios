;----------------------------------------------------------------------
;
;	Drive select routine for the 8" hard disk drives
;	At entry: <IX> points to DGB (Device Group Block)
;		  <IY> points to PDB (Physical Device Block)
;
;----------------------------------------------------------------------

drvhd1:
	ld	a,(iy+selmsk)	;get new mask
	cp	(ix+dgslmsk)	;is it the same as last time ??
	ret	z		;exit if same

	out	(drhdpt),a	;.. else select new drive
	ld	(ix+dgslmsk),a	;update DGB
	ret			;exit	

;----------------------------------------------------------------------
;
;	Home routine for the 8" hard disk drives
;	At entry: <IY> points to PDB
;	Returns:  <A> = 0, if no error
;		  <A> = 1, if error
;	This routine does not retry home errors.
;
;----------------------------------------------------------------------

homhd1:
	res	pdfhome,(iy+pdflgs)	;show no home done
	xor	a
	ld	(iy+pdcyl),a
	ld	(iy+pdcyl+1),a	;show current cylinder number as 0
	ld	a,cmdhom	;home command
	call	iogo		;issue command and wait for finish
	and	srdy+stc+sbusy
	cp	srdy+stc	;everything go ok ??
	ld	a,1		;preset <A> to show error
	ret	nz		;exit if error

	set	pdfhome,(iy+pdflgs)	;show home has been done
	xor	a		;clear <A> to show no error
	out	(ncylpt),a	;low byte of cylinder address
	out	(ncylpt),a	;high byte of cylinder address
;				;The home command does not set the
;				;.. cylinder ports.  It is necessary to set
;				;.. the new cylinder port so that a following
;				;.. read or write command will compare to the
;				;.. correct cylinder.
	ret			;exit

;----------------------------------------------------------------------
;
;	Seek routine for the 8" hard disk drives.
;	At entry: <IX> points to BPB (Buffer Pointer Block)
;		  <IY> points to PDB
;	Returns:  <A> = 0, if no error
;		  <A> = 1, if error
;	This routine does not retry seek errors.
;
;----------------------------------------------------------------------

cylhd1:
	ld	h,(iy+pdcyl+1)	;high order byte of cylinder number from PDB
	ld	l,(iy+pdcyl)	;low order byte of cylinder number from PDB
	ld	a,l		;low byte
	out	(pcylpt),a
	ld	a,h		;high byte
	out	(pcylpt),a	;set old cylinder register
	ld	d,(ix+bpcyl+1)
	ld	e,(ix+bpcyl)	;cylinder number from BPB
	ld	a,e		;low byte
	out	(ncylpt),a
	ld	a,d		;high byte
	out	(ncylpt),a	;set new cylinder number
;				;Note: The new cylinder register must
;				;..    be reloaded with the current cylinder
;				;..    number in case a seek is not necessary.
	xor	a		;preset for return with no errors
;				;.. and reset carry
	sbc	hl,de		;compare cylinder numbers
	ret	z		;return if equal

	ld	(iy+pdcyl+1),d
	ld	(iy+pdcyl),e	;save new cylinder in PDB
	ld	a,cmdsk		;seek command
	call	iogo		;issue command and wait for interrupt
	and	srdy+stc+sbusy
	cp	srdy+stc	;everything go ok ??
	ld	a,0		;preset for good return
	ret	z		;exit if no error
	
	inc	a		;show error (set <A> = 1)
	ret			;exit

;----------------------------------------------------------------------
;
;	Head select routine for 8" hard disks drives.
;	At entry: <IX> points to BPB
;		  <IY> points to PDB
;
;----------------------------------------------------------------------

hedhd1:
	ld	a,(ix+bphead)	;head number from BPB
	ld	(iy+pdhead),a	;show current head number

	rlca			;shift into proper position for command
	rlca
	rlca
	rlca

	or	(iy+selmsk)	;combine with select mask for this drive
	out	(drhdpt),a	;select proper side
	ret			;exit


;----------------------------------------------------------------------
;
;	Sector select routine for 8" hard disk drives.
;	At entry: <IX> points to BPB
;		  <IY> points to PDB
;
;----------------------------------------------------------------------

sechd1:
	ld	a,(ix+bpsec)	;sector number from BPB
	ld	(iy+pdsec),a	;show current sector number
	out	(secpt),a	;inform controller
	ret			;exit


;----------------------------------------------------------------------
;
;	Read routine for 8" hard disk drives.
;	At entry: <IX> points to BPB
;		  <IY> points to PDB
;	Returns:  <A> = 0, if no error
;		  <A> = 1, if error
;	An error return code is given only if the attempt at error
;	recovery is unsuccessfull.
;
;----------------------------------------------------------------------

redhd1:
	ld	a,cmdred
	ld	(rwcmd),a	;show that common routine is to read a sector
	ld	a,ldmard	;length of read commands for DMA
	ld	(rwlen),a	;save for common routine
	ld	hl,dmared	;point to the DMA read commands
	ld	(rwdmap),hl	;save for common routine

	call	rwc100		;get buffer address and length for DMA
	add	hl,bc		;add offset to length of data
	ld	(rdmem),de	;put buffer address in DMA command string
	ld	(rdbln),hl	;put adjusted buffer length in DMA
;				;.. command string

	jr	rw100		;go to common I/O routine

;----------------------------------------------------------------------
;
;	Write routine for 8" hard disk drives.
;	At entry: <IX> points to BPB
;		  <IY> points to PDB
;	Returns:  <A> = 0, if no error
;		  <A> = 1, if error
;	An error return code is given only if the attempt at error
;	recovery is unsuccessfull.
;
;----------------------------------------------------------------------

wrthd1:
	ld	a,cmdwrt
	ld	(rwcmd),a	;show that common routine is to write a sector
	ld	a,ldmawr	;length of write commands for DMA
	ld	(rwlen),a	;save for common routine
	ld	hl,dmawrt	;point to the DMA write commands
	ld	(rwdmap),hl	;save for common routine

	call	rwc100		;get buffer address and length for DMA
	ex	de,hl
	add	hl,bc		;add offset to buffer address
	ld	(wrtmem),hl	;put buffer address in DMA command string
	ld	(wrtbln),de	;put adjusted buffer length in DMA
;				;.. command string

	jr	rw100		;go to common I/O routine

	page
;----------------------------------------------------------------------
;
;	Various common read/write routines
;
;----------------------------------------------------------------------

rw100:
	in	a,(statpt)	;get status
	and	srdy+sbusy	;isolate interesting bits
	cp	srdy		;check for ready and not busy
	ld	a,1		;preset for error return
	ret	nz		;exit if error

;	Set up retry counters

	ld	a,mirty		;minor retry counter (number of reads or
;				;.. writes for each home)
	ld	(mictr),a	;set counter workarea
	ld	a,marty		;major retry counter (number of homes + 1)
	ld	(mactr),a	;set counter workarea

	jr	rw104

rw102:
	call	homhd1		;home drive
	call	cylhd1		;seek once more to proper cylinder
;				;It is unnecessary to restore the head
;				;.. number.  (The seek destroys the sector
;				;.. number but not the head number.)
	call	sechd1		;restore the proper sector number

rw104:
;	Set up DMA and bank select port

	in	a,(bankpt)	;get current bank select info
	and	00111111b	;zero out current DMA bank number
	or	(ix+bpmsk)	;combine with mask from BPB
	out	(bankpt),a	;set DMA bank number

	ld	bc,(rwptnc)	;length of commands for DMA controller
;				;.. port address for DMA chip
	ld	hl,(rwdmap)	;point to commands
	otir			;initialize DMA chip

	ld	a,(rwcmd)	;I/O command
	call	iogo		;issue command and wait for interrupt
	ld	b,a		;save status
	and	srdy+swrtf+scrcer+srnf+sbdsec+stc+sbusy
	cp	srdy+stc	;check for errors
	ld	a,0		;preset for good return
	ret	z		;return if no errors

	ld	a,(rwcmd)	;get previous I/O command
	cp	cmdred		;check for read command
	ld	a,0		;preset for read
	jr	z,rw106		;if read...branch

	inc	a		;else show write

rw106:
;				;<B> = status
	call	rwc200		;record temporary I/O error

	ld	a,b
	and	swrtf		;check for write fault
	jr	z,rw108		;branch if not write fault

	xor	a
	out	(drhdpt),a	;deselect drive to clear write fault
	call	hedhd1		;reselect head and drive

rw108:
	ld	hl,mictr
	dec	(hl)		;decrement minor loop control
	jr	nz,rw104

	ld	(hl),mirty	;reset minor retry counter

	ld	hl,mactr
	dec	(hl)		;decrement major loop control
	jr	nz,rw102

;	Place permanent error code in <A>
;	Reduce temporary error counter by the number of temporary errors
;	in one permanent error.  Then bump permanent error counter.

	ld	de,marty*mirty	;total temporary errors in a permanent error
	jp	rwc300		;put a 1 in <A>, record permanent error
;				;.. and exit to routine which called read or
;				;.. write entry point

	page
;----------------------------------------------------------------------
;
;	Common routine to be sure flag bit is not set,
;	to issue command and to wait for completion.
;	Status is saved and also returned in <A>.
;
;----------------------------------------------------------------------

hdiogo:
iogo:
	ld	c,a		;save command
	in	a,(statpt)	;get current status
	ld	b,a		;save status
	and	srdy+sbusy	;isolate interesting bits
	cp	srdy		;only ready should be on
	ld	a,b		;restore status
	ret	nz		;return if not ready or if busy

	ld	a,c		;get command
	ld	hl,hioflg	;point to hard disk
	ld	(hl),0		;show I/O is pending
	out	(cmdpt),a	;give hard disk command
	xor	a		;same as I/O flag

iogo4:
	cp	(hl)		;look at I/O flag
	jr	z,iogo4		;loop until I/O complete

	ld	a,(hdstat)	;retrieve status from I/O operation
	ret			;exit

	page
mactr:	db	0		;major retry counter workarea
mictr:	db	0		;minor retry counter workarea

rwcmd:	db	0		;command for common read/write routine

;	The following 2 variables must be kept together and in the order
;	shown.
rwptnc:
	db	dmapt		;DMA port address
rwlen:	db	0		;count of DMA commands

rwdmap:	dw	0		;address of DMA commands

;	DMA read commands

dmared:
	db	0C3H		;reset DMA controller
	db	014H		;port A is memory, port A address increments
	db	028H		;port B is I/O, port B address is fixed
	db	08AH		;ready active high, CE(bar) only,
;				;.. stop on end of block

	db	079H		;transfer port B to port A
rdmem:	dw	0		;port A (ie. memory) address
rdbln:	dw	0		;block length (always 1 byte less than actual)
	db	0A5H		;continuous transfer
	db	datpt		;port B (ie. device) address
	db	0CFH		;load starting address for both ports
;				;.. clear byte counter
	db	087H		;enable DMA controller
ldmard	equ	$-dmared	;length of read commands for DMA controller

;	DMA write commands

dmawrt:
	db	0C3H		;reset DMA controller
	db	014H		;port A is memory, port A address increments
	db	028H		;port B is I/O, port B address is fixed
	db	08AH		;ready active high, CE(bar) only,
;				;.. stop on end of block

	db	079H		;transfer port B to port A
wrtmem:	dw	0		;port A (ie. memory) address
wrtbln:	dw	0		;block length (always 1 byte less than actual)
	db	0A5H		;continuous transfer
	db	datpt		;port B (ie. device) address
	db	0CFH		;load starting address for both ports
;				;.. clear byte counter
	db	005H		;transfer port A to port B
	db	0CFH		;load starting address for both ports
;				;.. clear byte counter
	db	087H		;enable DMA controller
ldmawr	equ	$-dmawrt	;length of write commands for DMA controller


	end
---------------------------------------------------
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
;
;	Version Date:	February 24, 1981
;
;	This module of the CP/M 2.24 CBIOS contains several I/O routines
;	used by both the floppy and hard disk driver routines.  These
;	include a routine to obtain the address of the buffer and
;	length of the data and two routines to record I/O errors in
;	the PDB (Physical Device Block).
;
;----------------------------------------------------------------------

	public	rwc100,rwc200,rwc300

	page	62
	include	PDB.EQU

	include	BPTBPB.EQU

	page
;----------------------------------------------------------------------
;
;	Routine to get I/O information from PDB and BPB in preparation
;	for setting up DMA controller.
;	At entry: <IX> points to BPB
;		  <IY> points to PDB
;	Returns:  <BC> = number of bytes preceding data in buffer
;		  <DE> = buffer address
;		  <HL> = (physical sector data length) - 1
;
;----------------------------------------------------------------------

rwc100:
	ld	c,(iy+pdbrsv)	;number of bytes in buffer preceding data
	ld	b,0		;make into 16 bit number
	ld	d,(ix+@buf+1)
	ld	e,(ix+@buf)	;get address of buffer
	ld	h,(iy+pddata+1)
	ld	l,(iy+pddata)	;length of data in physical sector
	dec	hl		;subtract 1 for DMA controller
	ret			;exit


;----------------------------------------------------------------------
;
;	Record temporary I/O error in PDB
;	At entry: <A>  = 0 if read, 1 if write
;		  <B>  = status byte
;		  <IY> points to PDB
;
;----------------------------------------------------------------------

rwc200:
	res	pdertp,(iy+pderflg)	;show temporary error
	res	pderrw,(iy+pderflg)	;assume read error
	or	a		;verify assumption
	jr	z,rwc204	;if correct...branch

	set	pderrw,(iy+pderflg)	;show this as a write error

rwc204:
	ld	(iy+pderst),b	;save status
	ld	a,(iy+pdcyl)
	ld	(iy+pdercyl),a
	ld	a,(iy+pdcyl+1)
	ld	(iy+pdercyl+1),a	;current cylinder
	ld	a,(iy+pdhead)
	ld	(iy+pderhd),a	;current head
	ld	a,(iy+pdsec)
	ld	(iy+pdersc),a	;current sector

;	Add 1 to temporary error counter

	inc	(iy+pdterr)	;bump low order byte of temporary error
;				;.. counter
	ret	nz		;exit unless carry
	inc	(iy+pdterr+1)	;bump high order byte
	ret			;ret

	page
;----------------------------------------------------------------------
;
;	Routine to record permanent I/O error in PDB
;	At entry: <DE> = number of temporary errors for 1 permanent error
;		  <IY> points to PDB
;	Returns:  <A>  = 1 (to show permanent I/O error)
;
;----------------------------------------------------------------------

rwc300:
	ld	a,1		;put permanent error code in <A>
	set	pdertp,(iy+pderflg)	;show permanent error
	ld	h,(iy+pdterr+1)
	ld	l,(iy+pdterr)
	or	a		;clear carry
	sbc	hl,de
	ld	(iy+pdterr+1),h
	ld	(iy+pdterr),l	;update temporary error counter

	inc	(iy+pdperr)	;bump low order byte of permanent error
;				;.. counter
	ret	nz		;exit unless carry
	inc	(iy+pdperr+1)	;bump high order byte
	ret			;exit

	end
.z80
	title	BIOS224H, Copyright 1981, Altos Computer Systems - 22 Jul 81

	name	('BIOS2H')

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
;		Add CP/M signon message to cold boot routine.
;		Add "special" entry point address to MPT.
;		Change DPB for second logical drive on hard disk to allow
;		for diagnostic area.
;
;	Version Number: 2.24F0
;	Version Date:	July 22, 1981
;		Change revision level.  (Change was made to BIOS224F)
;
;	This module of the CP/M 2.24 CBIOS contains the cold boot
;	routine for CBIOS, most of the disk control blocks including
;	the Master Pointer Table and various buffers and workareas.
;
;----------------------------------------------------------------------

	public	mpt,sysint
	public	ldat,ldsbld,ldsbxl
	public	bpt,bpbpl,bpbend,dmabpb

	extrn	drv800,hom800,cyl800,hed800,sec800,red800,wrt800
	extrn	drvhd1,homhd1,cylhd1,hedhd1,sechd1,redhd1,wrthd1
	extrn	pat,ptlb,ivctbl,bdos,spcial

	page	62
;----------------------------------------------------------------------
;
;	Various equates
;
;----------------------------------------------------------------------

version	equ	22		;CP/M version number (times 10)
altosv	equ	4		;Altos version level
media	equ	'F'		;Floppy disk system
revlvl	equ	'0'		;Revision level

dspmsg	equ	9		;BDOS function for "print string" (on CRT)

cr	equ	00DH		;Carriage return
lf	equ	00AH		;Line feed
eom	equ	'$'		;End of message indicator

iobyte	equ	003H		;Location of Intel IOBYTE
cdisk	equ	004H		;Location of current drive number
fcb1	equ	05CH		;default location for FCB in low memory
wrkspc	equ	080H		;Loader BIOS work space.  Contains BSPIT.
tbase	equ	100H		;Base of CP/M Transient Program Area

hdipt	equ	00AH		;Hard disk interrupt command port
ctcp1pt	equ	00EH		;Baud rate generator for printer #1
cnfgpt	equ	024H		;configuration port
ctcc2pt	equ	030H		;Buad rate generator for console #2
ctcc3pt	equ	031H		;Baud rate generator for consoles #3 & #4
ctcp2pt	equ	032H		;Baud rate generator for printer #2

	include	DGB.EQU

	include	PDB.EQU

	include	LDB.EQU

	include	BPTBPB.EQU

;----------------------------------------------------------------------
;
;	Control blocks for disk I/O
;
;----------------------------------------------------------------------

mpt:				;Master Pointer Table
	dw	ldsb		;Address of LDSB (Logical Disk Select Block)
	dw	dgb0		;Address of first DGB (Device Group Block)
	dw	bpt		;Address of BPT (Buffer Pool Table)
	dw	fdmt		;Address of FDMT (Floppy Disk Mode Table)
	dw	ptlb		;Address of PTLB (Printer/Terminal Lookup
;				;.. Block)
	dw	0		;Address of BSPIT (Bootstrap Parameter
;				;.. Information Table).  Not used in CBIOS.
	dw	spcial		;Address of "special" entry point in CBIOS.
;				;.. The code at this entry point provides
;				;.. support for certain diagnostic and utility
;				;.. functions.

dgb0:				;Device Group Block for Shugart 800 floppy
;				;.. disk drives
	dw	dgb1		;Address of next DGB
	dw	pdb00		;Address of first PDB in this Device Group
	db	0		;Select mask from currently selected device
	db	0		;Reserved
	dw	drv800		;Address of drive select routine
	dw	hom800		;Address of home routine
	dw	cyl800		;Address of seek routine
	dw	hed800		;Address of head select routine
	dw	sec800		;Address of physical sector select routine
	dw	red800		;Address of physical sector read routine
	dw	wrt800		;Address of physical sector write routine

pdb00:				;Physical Device Block for floppy drive #1
	dw	dgb0		;DGB for this PDB
	dw	pdb01		;Address of next PDB
	dw	ldb000		;Address of first LDB for this PDB
	db	000h		;Shugart 800 floppy disk drive
	db	004h		;Select mask - floppy drive #1
	db	005h		;Flag bits
	db	002h		;Mode number
	db	002h		;Stepping rate indicator
	db	000h		;Reserved
	dw	0		;Reserved
	dw	128		;Number of data bytes in physical sector
	db	0		;Logical to physical sector shift factor
	db	0		;Logical sector offset mask
	db	0		;Number of buffer bytes preceding data
	db	1		;Sector number of first sector
	dw	77		;Number of cylinders on this device
	db	1		;Number of heads per cylinder
	db	48		;Number of physical sectors per track
	dw	0		;Current cylinder number
	db	0		;Current head (ie. side) number
	db	0		;Current physical sector number
	dw	0		;Count of temporary I/O errors
	dw	0		;Count of permanent I/O errors
	db	0		;Flag bits for I/O errors
	db	0		;Status from most recent I/O error
	dw	0		;Cylinder number for most recent I/O error
	db	0		;Head number for most recent I/O error
	db	0		;Physical sector number for most recent
;				;.. I/O error

ldb000:				;Logical device block for first logical device
	dw	pdb00		;Address of PDB for this LDB
	dw	0		;Address of next LDB
	dw	dph000		;Address of DPH for this logical device
	dw	0		;Offset for logical track 0 from physical
;				;.. track 0

pdb01:				;Physical Device Block for floppy drive #2
	dw	dgb0		;DGB for this PDB
	dw	pdb02		;Address of next PDB
	dw	ldb010		;Address of first LDB for this PDB
	db	000h		;Shugart 800 floppy disk drive
	db	008h		;Select mask - floppy drive #2
	db	001h		;Flag bits
	db	001h		;Mode number
	db	002h		;Stepping rate indicator
	db	000h		;Reserved
	dw	0		;Reserved
	dw	512		;Number of data bytes in physical sector
	db	2		;Logical to physical sector shift factor
	db	003h		;Logical sector offset mask
	db	0		;Number of buffer bytes preceding data
	db	1		;Sector number of first sector
	dw	77		;Number of cylinders on this device
	db	1		;Number of heads per cylinder
	db	15		;Number of physical sectors per track
	dw	0		;Current cylinder number
	db	0		;Current head (ie. side) number
	db	0		;Current physical sector number
	dw	0		;Count of temporary I/O errors
	dw	0		;Count of permanent I/O errors
	db	0		;Flag bits for I/O errors
	db	0		;Status from most recent I/O error
	dw	0		;Cylinder number for most recent I/O error
	db	0		;Head number for most recent I/O error
	db	0		;Physical sector number for most recent
;				;.. I/O error

ldb010:				;Logical device block for second logical device
	dw	pdb01		;Address of PDB for this LDB
	dw	0		;Address of next LDB
	dw	dph010		;Address of DPH for this logical device
	dw	0		;Offset for logical track 0 from physical
;				;.. track 0

pdb02:				;Physical Device Block for floppy drive #3
	dw	dgb0		;DGB for this PDB
	dw	pdb03		;Address of next PDB
	dw	ldb020		;Address of first LDB for this PDB
	db	000h		;Shugart 800 floppy disk drive
	db	010h		;Select mask - floppy drive #3
	db	001h		;Flag bits
	db	001h		;Mode number
	db	002h		;Stepping rate indicator
	db	000h		;Reserved
	dw	0		;Reserved
	dw	512		;Number of data bytes in physical sector
	db	2		;Logical to physical sector shift factor
	db	003h		;Logical sector offset mask
	db	0		;Number of buffer bytes preceding data
	db	1		;Sector number of first sector
	dw	77		;Number of cylinders on this device
	db	1		;Number of heads per cylinder
	db	15		;Number of physical sectors per track
	dw	0		;Current cylinder number
	db	0		;Current head (ie. side) number
	db	0		;Current physical sector number
	dw	0		;Count of temporary I/O errors
	dw	0		;Count of permanent I/O errors
	db	0		;Flag bits for I/O errors
	db	0		;Status from most recent I/O error
	dw	0		;Cylinder number for most recent I/O error
	db	0		;Head number for most recent I/O error
	db	0		;Physical sector number for most recent
;				;.. I/O error

ldb020:				;Logical device block for third logical device
	dw	pdb02		;Address of PDB for this LDB
	dw	0		;Address of next LDB
	dw	dph020		;Address of DPH for this logical device
	dw	0		;Offset for logical track 0 from physical
;				;.. track 0

pdb03:				;Physical Device Block for floppy drive #4
	dw	dgb0		;DGB for this PDB
	dw	0		;Address of next PDB
	dw	ldb030		;Address of first LDB for this PDB
	db	000h		;Shugart 800 floppy disk drive
	db	020h		;Select mask - floppy drive #4
	db	001h		;Flag bits
	db	001h		;Mode number
	db	002h		;Stepping rate indicator
	db	000h		;Reserved
	dw	0		;Reserved
	dw	512		;Number of data bytes in physical sector
	db	2		;Logical to physical sector shift factor
	db	003h		;Logical sector offset mask
	db	0		;Number of buffer bytes preceding data
	db	1		;Sector number of first sector
	dw	77		;Number of cylinders on this device
	db	1		;Number of heads per cylinder
	db	15		;Number of physical sectors per track
	dw	0		;Current cylinder number
	db	0		;Current head (ie. side) number
	db	0		;Current physical sector number
	dw	0		;Count of temporary I/O errors
	dw	0		;Count of permanent I/O errors
	db	0		;Flag bits for I/O errors
	db	0		;Status from most recent I/O error
	dw	0		;Cylinder number for most recent I/O error
	db	0		;Head number for most recent I/O error
	db	0		;Physical sector number for most recent
;				;.. I/O error

ldb030:				;Logical device block for fourth logical device
	dw	pdb03		;Address of PDB for this LDB
	dw	0		;Address of next LDB
	dw	dph030		;Address of DPH for this logical device
	dw	0		;Offset for logical track 0 from physical
;				;.. track 0

dgb1:				;Device Group Block for Shugart 1004 hard
;				;.. disk drives
	dw	0		;Address of next DGB
	dw	pdb10		;Address of first PDB in this Device Group
	db	0		;Select mask from currently selected device
	db	0		;Reserved
	dw	drvhd1		;Address of drive select routine
	dw	homhd1		;Address of home routine
	dw	cylhd1		;Address of seek routine
	dw	hedhd1		;Address of head select routine
	dw	sechd1		;Address of physical sector select routine
	dw	redhd1		;Address of physical sector read routine
	dw	wrthd1		;Address of physical sector write routine

pdb10:				;Physical Device Block for hard disk drive #1
	dw	dgb1		;DGB for this PDB
	dw	pdb11		;Address of next PDB
	dw	ldb100		;Address of first LDB for this PDB
	db	010h		;Shugart 1004 hard disk drive
	db	001h		;Select mask - hard disk #1
	db	001h		;Flag bits
	db	000h		;Not used
	db	000h		;Not used
	db	000h		;Reserved
	dw	0		;Reserved
	dw	512		;Number of data bytes in physical sector
	db	2		;Logical to physical sector shift factor
	db	003h		;Logical sector offset mask
	db	3		;Number of buffer bytes preceding data
	db	0		;Sector number of first sector
	dw	256		;Number of cylinders on this device
	db	4		;Number of heads per cylinder
	db	17		;Number of physical sectors per track
	dw	0		;Current cylinder number
	db	0		;Current head (ie. side) number
	db	0		;Current physical sector number
	dw	0		;Count of temporary I/O errors
	dw	0		;Count of permanent I/O errors
	db	0		;Flag bits for I/O errors
	db	0		;Status from most recent I/O error
	dw	0		;Cylinder number for most recent I/O error
	db	0		;Head number for most recent I/O error
	db	0		;Physical sector number for most recent
;				;.. I/O error

ldb100:				;Logical device block for fifth logical device
	dw	pdb10		;Address of PDB for this LDB
	dw	ldb101		;Address of next LDB
	dw	dph100		;Address of DPH for this logical device
	dw	1		;Offset for logical track 0 from physical
;				;.. track 0

ldb101:				;Logical device block for sixth logical device
	dw	pdb10		;Address of PDB for this LDB
	dw	0		;Address of next LDB
	dw	dph101		;Address of DPH for this logical device
	dw	965		;Offset for logical track 0 from physical
;				;.. track 0

pdb11:				;Physical Device Block for hard disk drive #2
	dw	dgb1		;DGB for this PDB
	dw	0		;Address of next PDB
	dw	ldb110		;Address of first LDB for this PDB
	db	010h		;Shugart 1004 hard disk drive
	db	002h		;Select mask - hard disk #2
	db	001h		;Flag bits
	db	000h		;Not used
	db	000h		;Not used
	db	000h		;Reserved
	dw	0		;Reserved
	dw	512		;Number of data bytes in physical sector
	db	2		;Logical to physical sector shift factor
	db	003h		;Logical sector offset mask
	db	3		;Number of buffer bytes preceding data
	db	0		;Sector number of first sector
	dw	256		;Number of cylinders on this device
	db	4		;Number of heads per cylinder
	db	17		;Number of physical sectors per track
	dw	0		;Current cylinder number
	db	0		;Current head (ie. side) number
	db	0		;Current physical sector number
	dw	0		;Count of temporary I/O errors
	dw	0		;Count of permanent I/O errors
	db	0		;Flag bits for I/O errors
	db	0		;Status from most recent I/O error
	dw	0		;Cylinder number for most recent I/O error
	db	0		;Head number for most recent I/O error
	db	0		;Physical sector number for most recent
;				;.. I/O error

ldb110:				;Logical device block for 7th logical device
	dw	pdb11		;Address of PDB for this LDB
	dw	ldb111		;Address of next LDB
	dw	dph110		;Address of DPH for this logical device
	dw	1		;Offset for logical track 0 from physical
;				;.. track 0

ldb111:				;Logical device block for 8th logical device
	dw	pdb11		;Address of PDB for this LDB
	dw	0		;Address of next LDB
	dw	dph111		;Ad
