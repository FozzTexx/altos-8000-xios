	include	bptbpb.equ
	include	pdb.equ
	include	ldb.equ
	include	dgb.equ

bankpt	equ	025H		;Bank select port for both "DMA" and memory
	
;----------------------------------------------------------------------
;
;	SELDSK entry point in CBIOS.
;	<C> contains drive number.
;	(0 = drive A:, 1 = drive B:, etc.)
;	Return DPH address in <HL>.  (<HL> = 0 if drive number invalid.)
;
;----------------------------------------------------------------------
seldsk:
	ld	a,(ldsbld)	;get number of last logical drive
	cp	c		;is this drive number valid ??
	jr	c,selerr

	ld	hl,ldsbxl	;point to logical disk translate block
	ld	b,0
	add	hl,bc		;point to translated drive number
	ld	c,(hl)		;get translated drive number
	cp	c		;if translated drive number is greater than
;				;.. number of last logical drive then the
;				;.. translated number must be equal 0FFH
	jr	c,selerr	;if translated number = 0FFH...branch

	ld	hl,ldat		;point to table of addresses of logical
				;.. disks
	add	hl,bc
	add	hl,bc		;point to appropriate address
	ld	c,(hl)
	inc	hl
	ld	b,(hl)		;get address of LDB (Logical Device Block)

	push	bc
	pop	ix
	ld	b,(ix+_pdb+1)
	ld	c,(ix+_pdb)	;get address of PDB (Physical Device Block)

	push	bc
	pop	iy
	bit	pdfdev,(iy+pdflgs)	;is device available ??
	jr	z,selerr	;if not...branch

	ld	(newpdb),iy	;save PDB address for later
	ld	(newldb),ix	;also save pointer to LDB
	ld	h,(ix+_dph+1)
	ld	l,(ix+_dph)	;put DPH address in <HL>

	bit	pdfhome,(iy+pdflgs)	;has it been homed yet ??
	ret	nz		;if already homed...exit

;				;<IX> still has address of LDB
;				;<IY> still has address of PDB
	push	hl		;save DPH address
	call	drhome		;home drive
	pop	hl
	or	a		;was home done ok ??
	ret	z		;if yes...exit
;				;.. else drive not ready - show error

selerr:
	ld	hl,0		;show error
	ret

;----------------------------------------------------------------------
;
;	HOME entry point in CBIOS.
;
;----------------------------------------------------------------------
home:
	ld	iy,(newpdb)	;get currently selected PDB
	ld	a,(iy+devtyp)
	and	0f0h		;isolate major device type
	cp	dtflpy		;is this a floppy disk ??
	jr	nz,home5	;if not...skip physical home

;				;<IY> points to PDB
	call	drhome		;do the home

home5:
	ld	bc,0		;show track 0
	call	settrk		;set to track 0
	ret			;exit

drhome:
;				;<IY> points to PDB
	ld	h,(iy+_dgb+1)
	ld	l,(iy+_dgb)	;<HL> points to DGB
	push	hl
	pop	ix

	ld	h,(ix+_drsel+1)
	ld	l,(ix+_drsel)	;<HL> points to drive select routine
	ld	bc,drhom3
	push	bc		;put return address on stack
	jp	(hl)		;go do drive select
;				;.. <IX> points to DGB
;				;.. <IY> points to PDB

drhom3:
	ld	h,(ix+_home+1)
	ld	l,(ix+_home)	;get address for home routine
	jp	(hl)		;go do home and return to routine
;				;.. that called this routine.
;				;.. <IY> points to PDB
;				;Note: This routine returns 0 if home ok
;				;      and 1 if error on home.
	
;----------------------------------------------------------------------
;
;	SETTRK entry point in CBIOS.
;	<BC> has track number for currently selected drive.
;
;----------------------------------------------------------------------
settrk:
	ld	ix,(newldb)	;get currently selected LDB
	ld	h,(ix+t0off+1)
	ld	l,(ix+t0off)	;get value to offset track value
	add	hl,bc		;do the offset
	ex	de,hl		;put the dividend in <DE>
	ld	iy,(newpdb)	;point to PDB
	ld	c,(iy+pdhedc)
	ld	b,0		;put divisor (ie. heads per cyl) in <BC>
	call	divide		;do the division
	ld	(newcyl),de	;save quotient (ie. cylinder number)
	ld	a,l
	ld	(newhed),a	;save remainder (ie. head number)
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
sectran:
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


divide:				;divide  a 16 bit divisor  in <BC>
;				;.. into a 16 bit dividend in <DE>
;				;.. yield:
;				;..	divisor (unchanged) in <BC>
;				;..	quotient  in <DE>
;				;..	remainder in <HL>
;				;.. All numbers assumed to be unsigned
;				;.. integers.  The divisor must not be 0.
;				;.. <A> is destroyed.  <IX> and <IY>
;				;.. not changed.
	ld	a,16		;loop control
	ld	hl,0		;zero out "accumulator"

div1:
	rl	e		;pick up leftmost bit of quotient
	rl	d		;.. (quotient is in 1's complement form)
	rl	l		;.. shift dividend left 1 bit
	rl	h		;.. Note: this instruction clears the carry

	sbc	hl,bc		;trial division
	jr	nc,div2		;branch if ok
	add	hl,bc		;.. else restore dividend and set carry

div2:
	dec	a
	jr	nz,div1		;do all 16 bits

	rl	e
	rl	d		;pick up low order quotient bit
	ld	a,e
	cpl
	ld	e,a
	ld	a,d
	cpl
	ld	d,a		;put quotient in correct form
	ret			;exit

;----------------------------------------------------------------------
;
;	READ entry point for CBIOS
;	Returns: <A> = 0 if no errors occured
;		 <A> = 1 if errors
;
;----------------------------------------------------------------------
read:
	ld	iy,(newpdb)		;point to PDB
	bit	pdfbuf,(iy+pdflgs)	;is the buffer to be the caller's
;					;.. "DMA" area ??
	jr	z,read10		;if not...branch

	call	rw110		;fill in special BPB,
;				;.. on return <IX> points to special BPB
	call	rw250		;<IX> must point to BPB.  As necessary,
;				;.. do drive select, seek,
;				;.. head select and sector select.
;				;.. Do the read.
;				;<A> = 0 if not error, else <A> = 1

read04:
	ld	hl,0
	ld	(dmabpb+_bppdb),hl	;release special BPB
;				;<A> contains 0 if I/O is ok, otherwise 1
	ret			;return to BDOS

read10:
	call	rw150		;Search buffer pool BPB's for match on
;				;.. PDB address and disk address.  If match
;				;.. occurs, move BPB to top of list.  Return
;				;.. 0 (match) or 1 (no match) in <A>.  Return
;				;.. pointer to BPB in <IX> if match.
	or	a
	jr	z,read15	;skip physical I/O if match found
	call	rw170		;Purge last buffer pool BPB.  Write out buffer
;				;.. if necessary.  Init BPB with PDB address
;				;.. and disk address.  Put BPB address in <IX>.
	call	rw250		;<IX> must point to BPB.  As necessary,
;				;.. do drive select, seek,
;				;.. head select and sector select.
;				;.. Do the read.
	or	a		;<A> = 0 if no error, else <A> = 1
	ret	nz

	call	rw150		;Move BPB to top of list.  Return pointer to
;				;BPB in <IX>.

read15:
	xor	a		;indicate direction of data transfer
;				;.. (ie. from buffer to caller's "DMA" area)
	call	rw190		;move logical sector to user
	xor	a		;show no error.
	ret			;exit

rw100:				;Fill in BPB pointed to by <IX>
	ld	hl,(newpdb)
	ld	(ix+_bppdb+1),h
	ld	(ix+_bppdb),l	;put new PDB address in BPB
	push	ix
	pop	hl
	ld	de,bpcyl
	add	hl,de		;point to disk address in BPB
	ex	de,hl		;put in <DE>
	ld	hl,newcyl	;point to new disk address
	ld	bc,4		;4 bytes to be moved
	ldir			;move disk address to BPB

	xor	a
	ld	(ix+bpflgs),a	;clear flag bits
	ret			;exit

rw110:				;routine to initailize the special BPB
	ld	ix,dmabpb	;point to special BPB
	call	rw100		;fill in most of BPB
	ld	hl,(usrdma)
	ld	(ix+_buf+1),h
	ld	(ix+_buf),l	;put address of caller's "DMA" area in BPB

	in	a,(bankpt)	;get current memory and DMA bank select info
	and	00011000b	;get current CPU bank number since this is
;				;.. where caller's "DMA" area is
	rlca
	rlca
	rlca			;move bank number into position for DMA bank
;				;.. number
	ld	(ix+bpmsk),a	;save for later

	ret			;exit

rw150:				;Search buffer pool BPB's for match on
;				;.. new PDB address and new disk address.
;				;.. If match occurs, move BPB to top of list.
;				;.. Return 0 (match) or 1 (no match) in <A>.
;				;.. If match, return pointer to BPB in <IX>.
	ld	a,(bpt+numbuf)	;get count of BPB's in buffer pool
	ld	ix,bpbpl	;point to first BPB
	ld	bc,bpblen	;length of 1 BPB
	
rw152:
	ld	d,(ix+_bppdb+1)
	ld	e,(ix+_bppdb)	;<DE> contains PDB address from BPB
	ld	hl,(newpdb)	;get new PDB address
	or	a		;clear carry
	sbc	hl,de		;are PDB addresses equal ??
	jr	z,rw156		;if yes...branch
rw154:
	add	ix,bc		;point to next BPB
	dec	a
	jr	nz,rw152	;look at next BPB
	inc	a		;put a 1 in <A> for no match
	ret			;exit

rw156:
	ld	d,(ix+bpcyl+1)
	ld	e,(ix+bpcyl)	;get BPB cylinder
	ld	hl,(newcyl)	;get new cylinder
	or	a		;clear carry
	sbc	hl,de		;are cylinders equal ??
	jr	nz,rw154	;if not...branch
	ld	d,(ix+bpsec)	;get BPB sector
	ld	e,(ix+bphead)	;get BPB head
	ld	hl,(newhed)	;get both new head and sector numbers
	or	a		;clear carry
	sbc	hl,de		;are head and sector numbers equal ??
	jr	nz,rw154	;if not...branch

	xor	a		;clear <A> to show match, clear carry
	push	ix
	pop	hl
	ld	de,bpbpl
	sbc	hl,de		;compute position of BPB
	ret	z		;exit if BPB already at top of list

	ld	de,bpblen
	add	hl,de		;number of bytes for second move
	push	hl		;save for second move
	ld	de,bpbpl-bpblen	;point to work area
	push	ix
  	pop	hl		;point to BPB
	ld	bc,bpblen	;length of first move
	ldir			;move BPB to work area

	push	ix
	pop	de
	dec	de		;"from" address
	ld	hl,bpblen
	add	hl,de		;"to" address
	ex	de,hl		;get addresses in correct registers
	pop	bc		;get previously computed length
	lddr			;move all BPB's into place

	ld	ix,bpbpl	;put BPB address in <IX>
;				;<A> already 0
	ret			;exit

rw170:				;Purge last buffer pool BPB.  Write out buffer
;				;.. if necessary.  Init BPB.  Put BPB address
;				;.. in <IX>.
	ld	ix,bpbend-bpblen	;point to last BPB
	bit	bpfwrt,(ix+bpflgs)	;do we need to write this buffer ??
	jr	z,rw172			;if not...branch

	call	rw280		;<IX> must point to BPB.  As necessary,
;				;.. do drive select, seek,
;				;.. head select and sector select.
;				;.. Do the write.
;				;ignore any I/O error since we don't know who
;				;belongs to it.

rw172:
	jp	rw100		;branch to routine to fill in BPB
;				;.. and exit to caller

rw190:				;transfer data between caller and buffer
;				;.. At entry: <A> shows transfer direction
;				;..		  0 = to caller's "DMA" area
;				;..		  1 = to buffer
;				;..	      <IX> points to BPB
;				;..	      <IY> points to PDB
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
	or	a		;check direction of transfer
	jr	z,rw192		;if already ok...branch
	ex	de,hl		;.. else exchange addresses

rw192:
	ld	bc,128		;length of data transfer
	ldir			;move
	ret			;exit

rw200:				;As necessary, do drive select, seek,
;				;.. head select and sector select.
;				;.. At entry <IX> points to BPB.
;				;.. At entry <IY> points to PDB.

	push	ix		;save BPB pointer

	ld	h,(iy+_dgb+1)
	ld	l,(iy+_dgb)
	push	hl
	pop	ix		;point to DGB

	ld	de,_drsel	;offset for drive select routine address
;				;.. <IX> points to DGB
;				;.. <IY> points to PDB
	call	rw220		;go do drive select

	pop	ix		;retrieve BPB address

	ld	de,_seek	;offset for seek routine
;				;.. <IX> points to BPB
;				;.. <IY> points to PDB
	call	rw220		;do seek
;				;Note: Seek errors are ignored here.  They
;				;      will show up again later as read or
;				;      write errors

	ld	de,_head	;offset for head select routine
;				;.. <IX> points to BPB
;				;.. <IY> points to PDB
	call	rw220		;do head select

	ld	de,_sec		;offset for sector select routine
;				;.. <IX> points to BPB
;				;.. <IY> points to PDB
	jr	rw220		;do sector select and exit to routine which
;				;.. called rw200

rw220:				;Put address of I/O routine in <HL>, jump
;				;.. to that routine.  The return will be
;				;.. to the routine which called this one.
;				;.. <DE> must have offset from start of DGB
;				;.. to address of routine.  <IY> must point
;				;.. to PDB.
	ld	h,(iy+_dgb+1)
	ld	l,(iy+_dgb)	;put DGB address in <HL>
	add	hl,de		;point to address of I/O routine
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	ex	de,hl		;<HL> points to I/O routine
	jp	(hl)		;jump to requested routine

rw250:				;As necessary, do drive select, seek,
;				;.. head select and sector select,
;				;.. do read.
;				;.. At entry <IX> points to BPB.
;				;.. At exit <A> has return code.
;				;.. if <A> = 0 then read ok, else <A> = 1
	call	rw270		;put PDB address in <IY>
	call	rw200		;As necessary, do drive select, seek,
;				;.. head select and sector select.
	ld	de,_read	;offset for read routine
;				;.. <IX> points to BPB
;				;.. <IY> points to PDB
	jr	rw220		;Do read including any retries.  Put return
;				;.. code in <A> and return to routine which
;				;.. called rw250

rw270:				;Put PDB address in <IY>.
	ld	h,(ix+_bppdb+1)
	ld	l,(ix+_bppdb)
	push	hl
	pop	iy		;point to PDB
	ret			;exit

rw280:				;As necessary, do drive select, seek,
;				;.. head select and sector select,
;				;.. do write.
;				;.. At entry <IX> points to BPB.
;				;.. At exit <A> has return code.
;				;.. if <A> = 0 then write ok, else <A> = 1
	call	rw270		;put PDB address in <IY>
	call	rw200		;As necessary, do drive select, seek,
;				;.. head select and sector select.
	ld	de,_write	;offset for write routine
;				;.. <IX> points to BPB
;				;.. <IY> points to PDB
	jr	rw220		;Do write including any retries.  Put return
;				;.. code in <A> and return to routine which
;				;.. called rw280

dmabpb:				;This is the special BPB which is used to
;				;.. refer to the caller's "DMA" area
	dw	0		;Address of PDB for this BPB
	dw	0		;Address of buffer area
	dw	0		;Cylinder number
	db	0		;Head number
	db	0		;Physical sector number
	db	0		;Bank mask for bank containing caller
	db	0		;Flag bits

ldsbld:	db	7		;Last logical drive number
ldsbxl:	db	0,1,2,3,4,5,6,7	;Logical Disk Translate Table
ldat:
flpwbd:	dw	ldb000		;Address of LDB (Logical Device Block) for
;				;.. logical drive 0, (first floppy)
	dw	ldb010		;.. logical drive 1,
	dw	ldb020		;.. logical drive 2,
	dw	ldb030		;.. logical drive 3,
hrdwbd:	dw	ldb100		;.. logical drive 4, (first hard disk)
	dw	ldb101		;.. logical drive 5,
	dw	ldb110		;.. logical drive 6,
	dw	ldb111		;.. logical drive 7.

newldb:	dw	0		;new LDB address
usrdma:	dw	0		;caller's "DMA" address

newpdb:	dw	0		;new PDB address

;	** The following 3 variables represent the new disk address.
;	** These variables should be kept as a group and in the order shown.
newcyl:	dw	0		;new cylinder (or track)
newhed:	db	0		;new head (or side)
newpsc:	db	0		;new physical sector

newoff:	dw	0		;offset from start of physical sector
;				;.. to start of logical sector

bpt:				;Buffer Pool Table
	dw	dmabpb		;Address of special BPB
	db	0		;Reserved
	db	(bpbend-bpbpl)/bpblen	;Number of buffers in buffer pool
	dw	515		;length of one buffer in buffer pool
	ds	bpblen		;Work area to allow re-ordering of BPB's
bpbpl:				;These are the BPB's for buffers in the
;				;.. buffer pool.
	dw	0		;Address of PDB for this BPB
	dw	buff1		;Address of buffer area
	dw	0		;Cylinder number
	db	0		;Head Number
	db	0		;Physical sector number
	db	0		;Bank mask for bank containing buffer
	db	0		;Flag bits
	dw	0
	dw	buff2
	dw	0
	db	0
	db	0
	db	0
	db	0
bpbend:

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
	dw	dph111		;Address of DPH for this logical device
	dw	965		;Offset for logical track 0 from physical
;				;.. track 0

dph000:				;DPH for first floppy disk drive
	dw	0		;Address of sector translate table
	dw	0
	dw	0,0		;Work areas
	dw	dirbuf		;Address of directory buffer work area
	dw	dpbm2		;Address of DPB
	dw	csv0		;Address of disk change work area
	dw	alv0		;Address of disk allocate work area

dph010:				;DPH for second floppy disk drive
	dw	0		;Address of sector translate table
	dw	0
	dw	0,0		;Work areas
	dw	dirbuf		;Address of directory buffer work area
	dw	dpbm1		;Address of DPB
	dw	csv1		;Address of disk change work area
	dw	alv1		;Address of disk allocate work area

dph020:				;DPH for third floppy disk drive
	dw	0		;Address of sector translate table
	dw	0
	dw	0,0		;Work areas
	dw	dirbuf		;Address of directory buffer work area
	dw	dpbm1		;Address of DPB
	dw	csv2		;Address of disk change work area
	dw	alv2		;Address of disk allocate work area

dph030:				;DPH for fourth floppy disk drive
	dw	0		;Address of sector translate table
	dw	0
	dw	0,0		;Work areas
	dw	dirbuf		;Address of directory buffer work area
	dw	dpbm1		;Address of DPB
	dw	csv3		;Address of disk change work area
	dw	alv3		;Address of disk allocate work area

dph100:				;DPH for first hard disk drive
	dw	0		;Address of sector translate table
	dw	0
	dw	0,0		;Work areas
	dw	dirbuf		;Address of directory buffer work area
	dw	dpbhd1		;Address of DPB
	dw	csv4		;Address of disk change work area
	dw	alv4		;Address of disk allocate work area

dph101:				;DPH for second hard disk drive
	dw	0		;Address of sector translate table
	dw	0
	dw	0,0		;Work areas
	dw	dirbuf		;Address of directory buffer work area
	dw	dpbhd2		;Address of DPB
	dw	csv5		;Address of disk change work area
	dw	alv5		;Address of disk allocate work area

dph110:				;DPH for third hard disk drive
	dw	0		;Address of sector translate table
	dw	0
	dw	0,0		;Work areas
	dw	dirbuf		;Address of directory buffer work area
	dw	dpbhd1		;Address of DPB
	dw	csv6		;Address of disk change work area
	dw	alv6		;Address of disk allocate work area

dph111:				;DPH for fourth hard disk drive
	dw	0		;Address of sector translate table
	dw	0
	dw	0,0		;Work areas
	dw	dirbuf		;Address of directory buffer work area
	dw	dpbhd2		;Address of DPB
	dw	csv7		;Address of disk change work area
	dw	alv7		;Address of disk allocate work area

buff1:	ds	515
buff2:	ds	515

;----------------------------------------------------------------------
;
;	Drive select routine for the Shugart 800 floppy disk drives
;	At entry: <IX> points to DGB (Device Group Block)
;		  <IY> points to PDB (Physical Device Block)
;
;----------------------------------------------------------------------

drv800:
	ld	a,(ix+dgslmsk)	;get currently selected mask
	cp	(iy+selmsk)	;is it the same as last time ??
	jr	z,drv802	;if yes...branch so that head is not
;				;.. unloaded.  A select is still done to
;				;.. allow for a density change.

	or	a		;is it 0 ??
	jr	z,drv802	;if 0 (ie. if no drive currently selected)
;				;.. then branch

	in	a,(cylpt)	;get current "cylinder"
	out	(datpt),a	;set up to seek to the cylinder we are
;				;.. already on
	ld	a,(curstp)	;get stepping rate for old drive
	or	cmdsku		;Combine with command to seek and unload
;				;.. the head.  Since we are seeking to the
;				;.. cylinder we are already on, the effect
;				;.. of this command is simply to unload
;				;.. the head.
	call	iogo		;issue command and wait for interrupt

;				;We have now deselected the old drive.
;				;.. The head was unloaded on the old drive
;				;.. so that the 1797 knows the head is
;				;.. unloaded.  We will next select the
;				;.. requested drive.
drv802:
	ld	b,(iy+selmsk)	;get select mask for this device
	ld	(ix+dgslmsk),b	;update mask in DGB
	ld	a,(iy+modenum)	;get mode number to find density
	or	a
	jr	z,drv805	;if single density...branch
	ld	a,denmsk	;.. else set double density

drv805:
	or	b		;combine mask with density
	out	(fdspt),a	;select the new drive
	ld	a,(iy+steprt)	;get step rate for later
	ld	(curstp),a	;save
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
	in	a,(sdslpt)
	and	0ffh-ssmsk
	out	(sdslpt),a	;force side 0 to be selected

hom802:
	res	pdfhome,(iy+pdflgs)	;show no home done
	xor	a
	ld	(iy+pdcyl),a
	ld	(iy+pdcyl+1),a	;show current cylinder number as 0
	ld	a,(curstp)	;get drive stepping rate
	or	cmdhom		;combine with home command
	call	iogo		;issue command and wait for finish
	and	snrdy+sskerr+scrcer	;everything go ok ??
	ld	a,1		;preset <A> to show error
	ret	nz		;exit if error

	set	pdfhome,(iy+pdflgs)	;show home has been done
	bit	pdfsid,(iy+pdflgs)	;has number of sides been
;					;.. determined ??
	jr	nz,hom809	;if yes...branch

	in	a,(sdslpt)	;get info on number of sides
	and	scmsk		;check for number of sides
	jr	z,hom807	;if only one side...branch
;				;.. the MODE command has set 
;				;.. up the control blocks ok
	ld	h,(iy+_1stldb+1)
	ld	l,(iy+_1stldb)	;get address of first LDB
;				;.. (Logical Disk Block)
	ld	de,_dph
	add	hl,de		;point to address for DPH
;				;.. (Disk Parameter Header)
	ld	e,(hl)
	inc	hl
	ld	d,(hl)		;point to DPH
	ld	hl,10
	add	hl,de		;point to address of DPB
;				;.. (Disk Parameter Block)
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	ld	bc,15
	ex	de,hl
	add	hl,bc		;<HL> = address of DPB for 2 sided floppy
	ex	de,hl
	ld	(hl),d
	dec	hl
	ld	(hl),e		;put updated DPB address back in DPH

	inc	(iy+pdhedc)	;show that this drive has 2 sides

hom807:
	set	pdfsid,(iy+pdflgs)	;set bit to show we have
;					;.. determined number of sides

hom809:
	xor	a		;clear <A> to show no error
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
	ld	a,(curstp)	;get stepping rate
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
	in	a,(statpt)	;status from 1797
	and	snrdy		;check for not ready
	ld	a,1		;preset for error return
	ret	nz		;

	call	rw820		;get buffer address and length for DMA
;				;.. initialize retry counters
	ld	(rdmem),de	;put buffer address in DMA command string
	ld	(rdbln),hl	;put adjusted buffer length in DMA
;				;.. command string

	jr	red804

red802:
	call	hom802		;home drive without changing side select
	call	cyl800		;seek once more to proper cylinder

red804:
;	Set sector register in 1797
;	Set up DMA and bank select port

	ld	b,ldmard	;length of commands for DMA controller
	ld	hl,dmared	;point to commands
	call	rw830		;set sector register in 1797, select bank
;				;.. for DMA, initialize DMA chip

	ld	a,(sidenm)	;get side select flag
	or	cmdred		;combine with read command
	call	iogo		;issue command and wait for interrupt
	ld	b,a		;save status
	in	a,(dmapt)	;read status from DMA controller
	and	dmaeob		;look only at "end of block" indicator
	ld	c,a		;save
	ld	a,b		;status from 1797
	and	snrdy+srnf+scrcer+sldata+sbusy
;				;check for errors
	or	c		;combine with end of block indicator
;				;.. (0 = eob)
	ret	z		;return if no errors

	xor	a		;show that this is a read
;				;<B> = status
	call	rw800		;record temporary error
;				;.. and clear previous command

	ld	hl,mictr
	dec	(hl)		;decrement minor loop control
	jr	nz,red804

	ld	(hl),mirty	;reset minor retry counter

	ld	hl,mactr
	dec	(hl)		;decrement major loop control
	jr	nz,red802

	jr	rw810		;reduce temporary counter and bump
;				;permanent error counter
;				;show permanent error
;				;exit to routine which called this one

;----------------------------------------------------------------------
;
;	Write routine for Shugart 800 floppy disk drives.
;	At entry: <IX> points to BPB
;		  <IY> points to PDB
;	Returns:  <A> = 0, if no error
;		  <A> = 1, if error
;	An error return code is given only if the attempt at error
;	recovery is unsuccessfull.
;
;----------------------------------------------------------------------

wrt800:
	ld	a,cmdnul	;null command so that we can read
	out	(cmdpt),a	;.. command status
	in	a,(statpt)	;status from 1797
	and	snrdy+swrtp	;check for not ready, write protect
	ld	a,1		;preset for error return
	ret	nz		;no point in retrying either error
;				;.. also don't want to count either

	call	rw820		;get buffer address and length for DMA
;				;.. initialize retry counters
	ld	(wrtmem),de	;put buffer address in DMA command string
	ld	(wrtbln),hl	;put adjusted buffer length in DMA
;				;.. command string

	jr	wrt804

wrt802:
	call	hom802		;home drive without changing side select
	call	cyl800		;seek once more to proper cylinder

wrt804:
;	Set sector register in 1797
;	Set up DMA and bank select port

	ld	b,ldmawr	;length of commands for DMA controller
	ld	hl,dmawrt	;point to commands
	call	rw830		;set sector register in 1797, select bank
;				;.. for DMA, initialize DMA chip

	ld	a,(sidenm)	;get side select flag
	or	cmdwrt		;combine with write command
	call	iogo		;issue command and wait for interrupt
	ld	b,a		;save status
	and	snrdy+swrtf+srnf+scrcer+sldata+sbusy
;				;check for errors
	ret	z		;return if no errors

	ld	a,1		;show that this is a write
;				;<B> = status
	call	rw800		;record temporary error
;				;.. and clear previous command

	ld	hl,mictr
	dec	(hl)		;decrement minor loop control
	jr	nz,wrt804

	ld	(hl),mirty	;reset minor retry counter

	ld	hl,mactr
	dec	(hl)		;decrement major loop control
	jr	nz,wrt802

	jr	rw810		;reduce temporary counter and bump
;				;permanent error counter
;				;show permanent error
;				;exit to routine which called this one

;----------------------------------------------------------------------
;
;	Various common read/write routines
;
;----------------------------------------------------------------------

;	Record temporary error, then issue null command and read
;	status to clear any previous command

rw800:
	call	rwc200		;record temporary error
	ld	a,cmdnul	;command to clear 1797
	out	(cmdpt),a	;issue command
	in	a,(statpt)	;get status
	ret			;exit

;	Place permanent error code in <A>
;	Reduce temporary error counter by the number of temporary errors
;	in one permanent error.  Then bump permanent error counter.

rw810:
	ld	de,marty*mirty	;total temporary errors in a permanent error
	jp	rwc300		;put a 1 in <A>, record permanent error

;	Put buffer address in <DE>, put adjusted buffer length in <HL>.
;	These values are for the DMA command string.
;	Initialize retry counters in case of I/O errors

rw820:
	call	rwc100		;on return: <BC> = number of bytes preceding
;				;		   buffer
;				;	    <DE> = buffer address
;				;	    <HL> = physical sector length - 1

	ld	a,mirty		;minor retry counter (number of reads or
;				;.. writes for each home)
	ld	(mictr),a	;set counter workarea
	ld	a,marty		;major retry counter (number of homes + 1)
	ld	(mactr),a	;set counter workarea
	ret			;exit

;	Set sector in sector register of 1797
;	Select the proper memory bank for DMA
;	Initialize the DMA chip for data transfer
;	At entry: <B>  = length of DMA commands
;		  <HL> = address of DMA commands
;		  <IX> = address of BPB
;		  <IY> = address of PDB
rw830:
	ld	a,(iy+pdsec)	;current sector
	out	(secpt),a	;set 1797

	in	a,(bankpt)	;get current bank select info
	and	00111111b	;zero out current DMA bank number
	or	(ix+bpmsk)	;combine with mask from BPB
	out	(bankpt),a	;set DMA bank number

	ld	c,dmapt		;port for DMA controller
	otir			;set up DMA
	ret			;exit

;----------------------------------------------------------------------
;
;	Common routine to be sure flag bit is not set,
;	to issue command and to wait for completion.
;	Status is saved and also returned in <A>.
;
;----------------------------------------------------------------------

fliogo:
iogo:
	ld	hl,fioflg	;point to floppy I/O flag
	ld	(hl),0		;show I/O is pending
	out	(cmdpt),a	;give floppy disk command

	ld	a,6		;set 3 second time limit

iogo2:
	ld	(time0),a
	ld	bc,45455	;enough loops for .5 second

iogo3:
	dec	bc
	ld	a,b
	or	c		;check for end of .5 second
	jr	z,iogo4		;if .5 second up...branch

	xor	a		;check for completion
	cp	(hl)
	jr	z,iogo3		;loop until I/O complete

	ld	a,(fdstat)	;retrieve status from I/O operation
	ret			;exit

iogo4:
	ld	a,(time0)
	dec	a
	jr	nz,iogo2	;repeat loop until total time has elapsed

	ld	a,snrdy+srnf	;simulate not ready and record not found
;				;.. (or not ready and seek error)
	ret			;exit


curstp:	db	0		;stepping rate for currently selected drive
sidenm:	db	0		;side number for floppy disk drive

mactr:	db	0		;major retry counter workarea
mictr:	db	0		;minor retry counter workarea

time0:	db	0		;I/O timer work area

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
	db	085H		;byte transfer
	db	datpt		;port B (ie. device) address
	db	0CFH		;load starting address for both ports
;				;.. clear byte counter
	db	005H		;transfer port A to port B
	db	0CFH		;load starting address for both ports
;				;.. clear byte counter
	db	087H		;enable DMA controller
ldmawr	equ	$-dmawrt	;length of write commands for DMA controller
	
