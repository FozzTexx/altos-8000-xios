.z80
	title	"LDRB224E, Copyright 1981, Altos Computer Systems - 30 Apr 81"

;; 	name	('LDRB2E')

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
;	Version Date:	April 3, 1981
;
;	Version Number: 2.24*
;	Version Date:	April 30, 1981
;		Change seek to seek with verify.  Change is made to
;		improve probability of good read during boot.
;
;	This module of the CP/M 2.24 Loader CBIOS contains the basic
;	I/O routines for Shugart 800 floppy disk drives.  The addresses
;	of these routines are contained in the DGB (Device Group Block)
;	for the floppy disks.
;
;----------------------------------------------------------------------

	;; public	drv800,hom800,cyl800,hed800,sec800,red800

	;; extrn	fdstat,fioflg

	page	62
	include	dgb.equ

	include	pdb.equ

	include	bptbpb.equ

	include	ldb.equ

	newpage
;----------------------------------------------------------------------
;
;	Various equates
;
;----------------------------------------------------------------------

dmapt	equ	000H		;DMA controller port
;	Status bit from DMA port
dmaeob	equ	020H		;End of block (value of 0 = eob)

fdspt	equ	008H		;Floppy disk select port
;	Various bits in above port
hldmsk	equ	002H		;Shows whether or not head is loaded
denmsk	equ	001H		;Sets recording density

sdslpt	equ	009H		;Side select port
;	Various bits in above port
scmsk	equ	080H		;Side count bit
ssmsk	equ	020H		;Side select bit

;	ports for 1797

cmdpt	equ	004H		;1797 command register
cylpt	equ	005H		;1797 cylinder register (The 1797
;				;.. booklet calls this a track register.)
secpt	equ	006H		;1797 sector register
datpt	equ	007H		;1797 data register

;	Commands for FD1797

cmdnul	equ	0D0H		;Null command
cmdsku	equ	010H		;Seek, unload head at beginning, no verify
cmdskl	equ	01CH		;Seek, load head at beginning, verify
cmdhom	equ	008H		;Restore (Home), load head at beginning,
;				;.. no verify
cmdred	equ	088H		;Read, single record, IBM sector length,
;				;.. no 15 ms. delay -- side select determined
;				;.. dynamically
cmdwrt	equ	0A8H		;Write, single record, IBM sector length,
;				;.. no 15 ms. delay, data mark -- side select
;				;.. determined dynamically

;	Status bit meanings - Type I  and II Commands

snrdy	equ	080H		;Not ready
swrtp	equ	040H		;Write protect
swrtf	equ	020H		;Write fault
srnf	equ	010H		;Record not found
sskerr	equ	010H		;Seek error
scrcer	equ	008H		;CRC error
sldata	equ	004H		;Lost data
sbusy	equ	001H		;Busy

;	Miscellaneous
marty	equ	4		;Major I/O error retry counter
;				;.. (number of homes + 1)
mirty	equ	5		;Minor I/O error retry counter
;				;.. (number of reads or writes per home)

;----------------------------------------------------------------------
;
;	Drive select routine for the Shugart 800 floppy disk drives
;	At entry: <IX> points to DGB (Device Group Block)
;		  <IY> points to PDB (Physical Device Block)
;
;----------------------------------------------------------------------

drv800:
	ld	b,(iy+selmsk)	;get select mask for this device
	ld	a,(iy+modenum)	;get mode number to find density
	or	a
	jr	z,drv805	;if single density...branch
	ld	a,denmsk	;.. else set double density

drv805:
	or	b		;combine mask with density
	out	(fdspt),a	;select the new drive
	ret			;exit

;----------------------------------------------------------------------
;
;	Home routine for the Shugart 800 floppy disk drives
;	At entry: <IY> points to PDB
;	Returns:  <A> = 0, if no error
;		  <A> = 1, if error
;	This routine does not retry home errors.
;
;----------------------------------------------------------------------

hom800:
	xor	a
	ld	(iy+pdcyl),a	;show current cylinder number as 0
	ld	a,(iy+steprt)	;get drive stepping rate
	or	cmdhom		;combine with home command
	call	iogo		;issue command and wait for finish
	and	snrdy+sskerr+scrcer	;everything go ok ??
	ret	z		;if yes...exit
	ld	a,1		;show error

	ret			;exit

;----------------------------------------------------------------------
;
;	Seek routine for the Shugart 800 floppy disk drives.
;	Only the low order byte of the 16 bit cylinder number in the
;	control block is used.  The high order byte is assumed to
;	be zero.
;	At entry: <IX> points to BPB (Buffer Pointer Block)
;		  <IY> points to PDB
;	Returns:  <A> = 0, if no error
;		  <A> = 1, if error
;	This routine does not retry seek errors.
;
;----------------------------------------------------------------------

cyl800:
	ld	a,(iy+pdcyl)	;cylinder number from PDB
	out	(cylpt),a	;always set current cylinder (track) reg.
	cp	(ix+bpcyl)	;cylinder number from BPB
	ld	a,0		;preset for return with no errors
	ret	z		;return if equal

	ld	a,(ix+bpcyl)	;get new cylinder
	ld	(iy+pdcyl),a	;save in PDB
	out	(datpt),a	;send to controller
	ld	a,(iy+steprt)	;get stepping rate
	or	cmdskl		;seek command, load head combined with
;				;.. proper stepping rate
	call	iogo		;issue command and wait for interrupt
	and	snrdy+sskerr+scrcer	;check for not ready, seek error
;					;.. and crc error
	ret	z		;exit if no error

	ld	a,1		;show error
	ret			;exit

;----------------------------------------------------------------------
;
;	Head select (ie. side select) routine for Shugart 800 floppy
;	disk drives.
;	At entry: <IX> points to BPB
;		  <IY> points to PDB
;
;----------------------------------------------------------------------

hed800:
	ld	a,(ix+bphead)	;head number from BPB
	ld	(iy+pdhead),a	;show current head number
	rlca			;shift into proper position for command
;				;.. remember that there will be only
;				;.. sides 0 and 1 on any floppy
	ld	(sidenm),a	;save for later read or write command
;				;.. so that the side compare will be done

	rlca
	rlca
	rlca
	rlca

	ld	b,a		;save shifted value
	in	a,(sdslpt)	;get current info from side select port
	and	0ffh-ssmsk	;eliminate current side select value
	or	b		;combine with new side select
	out	(sdslpt),a	;select proper side
	ret			;exit


;----------------------------------------------------------------------
;
;	Sector select routine for Shugart 800 floppy disk drives.
;	At entry: <IX> points to BPB
;		  <IY> points to PDB
;
;----------------------------------------------------------------------

sec800:
	ld	a,(ix+bpsec)	;sector number from BPB
	ld	(iy+pdsec),a	;show current sector number
	ret			;exit

	newpage
;----------------------------------------------------------------------
;
;	Read routine for Shugart 800 floppy disk drives.
;	At entry: <IX> points to BPB
;		  <IY> points to PDB
;	Returns:  <A> = 0, if no error
;		  <A> = 1, if error
;	An error return code is given only if the attempt at error
;	recovery is unsuccessfull.
;
;----------------------------------------------------------------------

red800:
;	Put buffer address in <DE>, put adjusted buffer length in <HL>.
;	These values are for the DMA command string.

	ld	d,(ix+_buf+1)
	ld	e,(ix+_buf)	;<DE> = buffer address
	ld	h,(iy+pddata+1)
	ld	l,(iy+pddata)
	dec	hl		;<HL> = physical sector length - 1
	ld	(rdmem),de	;put buffer address in DMA command string
	ld	(rdbln),hl	;put adjusted buffer length in DMA
;				;.. command string

	ld	a,mirty
	ld	(mictr),a	;set retry counter

red804:
;	Set sector register in 1797

	ld	a,(iy+pdsec)	;current sector
	out	(secpt),a	;set 1797

;	Initialize the DMA chip for data transfer

	ld	b,ldmard	;length of commands for DMA controller
	ld	c,dmapt		;port for DMA controller
	ld	hl,dmared	;point to commands
	otir			;set up DMA

	ld	a,(sidenm)	;get side select flag
	or	cmdred		;combine with read command
	call	iogo		;issue command and wait for interrupt
	and	snrdy+srnf+scrcer+sldata+sbusy
;				;check for errors
	ld	b,a		;save status
	in	a,(dmapt)	;read status from DMA controller
	and	dmaeob		;look only at "end of block" indicator
;				;.. (0 = eob)
	or	b		;status
	ret	z		;return if no errors

	ld	hl,mictr
	dec	(hl)		;decrement retry counter
	jr	nz,red804

	ld	a,1		;show permanent error
	ret			;exit

;----------------------------------------------------------------------
;
;	Common routine to be sure flag bit is not set,
;	to issue command and to wait for completion.
;	Status is saved and also returned in <A>.
;
;----------------------------------------------------------------------

iogo:
	ld	hl,fioflg	;point to floppy I/O flag
	ld	(hl),0		;show I/O is pending
	out	(cmdpt),a	;give floppy disk command
	xor	a		;same as I/O flag

iogo4:
	cp	(hl)		;look at I/O flag
	jr	z,iogo4		;loop until I/O complete

	ld	a,(fdstat)	;retrieve status from I/O operation
	ret			;exit

	newpage
sidenm:	db	0		;side number for floppy disk drive

mictr:	db	0		;minor retry counter workarea

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
	db	085H		;byte transfer
	db	datpt		;port B (ie. device) address
	db	0CFH		;load starting address for both ports
;				;.. clear byte counter
	db	0BFH		;set read status so next read is from
;				;.. status register
	db	087H		;enable DMA controller
ldmard	equ	$-dmared	;length of read commands for DMA controller

;; 	end
