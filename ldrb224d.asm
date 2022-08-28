.z80
	title	"LDRB224D, Copyright 1981, Altos Computer Systems - 26 Feb 81"

;; 	name	('LDRB2D')

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
;	Systems and is disributed to the owners of Altos ACS8000 series
;	computers for use on those systems only.  Any other use of this
;	software constitutes a breach of the copyright license to the
;	purchaser.
;
;	Version Number:	2.24
;	Version Date:	February 26, 1981
;
;	This module of the CP/M 2.24 Loader BIOS contains support for
;	floppy and hard disk drives.
;
;----------------------------------------------------------------------

	;; public	seldsk,home,settrk
	;; public	setsec,setdma,sectrn
	;; public	read

	;; extrn	drv800,hom800,cyl800,hed800,sec800,red800
	;; extrn	dgb0,pdb00,ldb000,dph000
	;; extrn	bpbpl

	page	62
	include dgb.equ

	include	pdb.equ

	include	ldb.equ

	include	bptbpb.equ

	newpage
;----------------------------------------------------------------------
;
;	SELDSK entry point in CBIOS.
;	<C> contains drive number.
;	(0 = drive A:, 1 = drive B:, etc.)
;	Return DPH address in <HL>.
;	NOTE: The loader BIOS always assumes that drive A: is being
;	      selected.
;
;----------------------------------------------------------------------

seldsk:
	ld	hl,dph000	;put DPH address in <HL>

	ret			;exit

;----------------------------------------------------------------------
;
;	HOME entry point in CBIOS.
;
;----------------------------------------------------------------------

home:
	ld	iy,(newpdb)	;get currently selected PDB
	ld	ix,dgb0		;<IX> points to DGB

	call	drv800		;select the drive
	call	hom800		;home the drive

	ld	bc,0		;show track 0
	call	settrk		;set to track 0
	ret			;exit

	newpage
;----------------------------------------------------------------------
;
;	SETTRK entry point in CBIOS.
;	<BC> has track number for currently selected drive.
;
;----------------------------------------------------------------------

settrk:
	ld	iy,(newpdb)	;point to PDB
	ld	a,(iy+pdhedc)	;heads per cyl
	dec	a
	jr	z,settr4	;if only single sided diskette...branch

	xor	a
	srl	b
	rr	c		;divide track number by two
	rla			;get remainder in <A>

settr4:
	ld	(newcyl),bc	;save cylinder number
	ld	(newhed),a	;save head number
	ret			;exit


;----------------------------------------------------------------------
;
;	SETSEC entry point in CBIOS.
;	<BC> contains sector number.
;
;----------------------------------------------------------------------

setsec:
	ld	iy,(newpdb)	;point to PDB
	ld	l,c
	ld	h,b		;put sector in <HL>
	ld	c,(iy+pdfsec)	;find out number of first sector
	ld	b,0		;make it a 16 bit number
	or	a		;reset carry
	sbc	hl,bc		;adjust for number of first sector
	ld	a,l		;save low order byte
	ld	e,(iy+pdshft)	;get shift factor
	inc	e		;up by one

setse1:
	dec	e
	jr	z,setse2	;exit when finished shifting
	srl	h
	rr	l		;shift <HL> right 1 bit
	jr	setse1

setse2:
	add	hl,bc		;add back first sector number
	ld	(newpsc),hl	;save physical sector number

	and	(iy+pdloff)	;turn off high order bits
	ld	h,a
	ld	l,0
	srl	h
	rr	l		;the hand is quicker than the eye **
;				;.. the previous 4 instructions have
;				;.. created a 16 bit value = 128 * <A>
	ld	(newoff),hl	;save offset
	ret			;exit


;----------------------------------------------------------------------
;
;	SETDMA entry point for CBIOS.
;	<BC> contains caller's disk memory area.
;
;----------------------------------------------------------------------

setdma:
	ld	(usrdma),bc	;save caller's "DMA" address
	ret			;exit

;----------------------------------------------------------------------
;
;	SECTRAN entry point for CBIOS.
;	<BC> contains sector number to be translated.
;	<DE> contains address of translate table.
;	Returns translated sector number in <HL>.
;
;----------------------------------------------------------------------

sectrn:
	ex	de,hl
	ld	a,h
	or	l		;find out if translate table exists
	jr	z,sectr5	;if not...branch
	add	hl,bc		;point to translated sector number
	ld	l,(hl)		;get (one byte) sector value
	ld	h,0
	ret			;exit

sectr5:
	ld	iy,(newpdb)	;get currently selected PDB
	ld	l,(iy+pdfsec)	;find out whether first sector is
;				;sector #0 or sector #1
	add	hl,bc		;put adjusted sector number in <HL>
	ret			;exit

	newpage
;----------------------------------------------------------------------
;
;	READ entry point for CBIOS
;	Returns: <A> = 0 if no errors occured
;		 <A> = 1 if errors
;
;----------------------------------------------------------------------

read:
	ld	iy,(newpdb)	;point to PDB
	ld	ix,bpbpl	;point to only BPB

;				;Search BPB for match on
;				;.. new disk address.
	
	ld	d,(ix+bpcyl+1)
	ld	e,(ix+bpcyl)	;get BPB cylinder
	ld	hl,(newcyl)	;get new cylinder
	or	a		;clear carry
	sbc	hl,de		;are cylinders equal ??
	jr	nz,read10	;if not...branch

	ld	d,(ix+bpsec)	;get BPB sector
	ld	e,(ix+bphead)	;get BPB head
	ld	hl,(newhed)	;get both new head and sector numbers
	or	a		;clear carry
	sbc	hl,de		;are head and sector numbers equal ??
	jr	z,read15	;if yes...branch

read10:
	ld	de,bpbpl+bpcyl	;point to disk address in BPB
	ld	hl,newcyl	;point to new disk address
	ld	bc,4		;4 bytes to be moved
	ldir			;move disk address to BPB

	call	drv800		;do drive select
	call	cyl800		;seek
	call	hed800		;select the head
	call	sec800		;select the sector
	call	red800		;do the read

	or	a		;<A> = 0 if no error, else <A> = 1
	ret	nz

read15:
;				;transfer data between caller and buffer
	ld	hl,(newoff)	;offset from start of data in physical sector
;				;.. to start of logical sector
	ld	c,(iy+pdbrsv)	;number of bytes in buffer before first data
;				;.. byte
	ld	b,0
	ld	d,(ix+_buf+1)	;buffer address
	ld	e,(ix+_buf)
	add	hl,bc
	add	hl,de		;<HL> points to logical sector

	ld	de,(usrdma)	;caller's "DMA" address
	ld	bc,128		;length of data transfer
	ldir			;move

	xor	a		;show no error.
	ret			;exit

	newpage
newldb:	dw	ldb000		;new LDB address
usrdma:	dw	0		;caller's "DMA" address

newpdb:	dw	pdb00		;new PDB address

;	** The following 3 variables represent the new disk address.
;	** These variables should be kept as a group and in the order shown.
newcyl:	dw	0		;new cylinder (or track)
newhed:	db	0		;new head (or side)
newpsc:	db	0		;new physical sector

newoff:	dw	0		;offset from start of physical sector
;				;.. to start of logical sector

;; 	end
