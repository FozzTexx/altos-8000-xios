	include "ldb.equ"
	include "pdb.equ"
	include "dgb.equ"
	include "bptbpb.equ"
	include "xios.equ"

dskpio	equ	008H		;port to monitor for disk interrupts
fdbit	equ	6		;interrupt indication from floppy disk
hdbit	equ	7		;interrupt indication from hard disk
	ifndef	fdstpt
fdstpt	equ	004H		;port for status from floppy disk
	endif
	ifndef	hdstpt
hdstpt	equ	023H		;port for status from hard disk
	endif

;----------------------------------------------------------------------
;
;	All floppy and hard disk interrupts are vectored here.  All
;	registers and status bits must be preserved so that the
;	interrupted task can be properly restarted.
;
;----------------------------------------------------------------------

dskint:
	push	af
	push	hl		;save registers

dskin2:
	in	a,(dskpio)	;read PIO to see who caused interrupt
	bit	hdbit,a		;check for hard disk interrupt
	jr	z,dskin5	;if not hard disk...branch

	in	a,(hdstpt)	;get status and clear interrupt from
;				;.. hard disk controller
	ld	(hdstat),a	;save status
	ld	hl,hioflg	;point to hard disk I/O flag
	jr	dskin8

dskin5:
	bit	fdbit,a		;check for floppy disk interrupt
	jr	z,intxit	;if not floppy disk...branch
	in	a,(fdstpt)	;get status and clear interrupt from
;				;.. floppy disk controller
	ld	(fdstat),a	;save status
	ld	hl,fioflg	;point to floppy I/O flag

dskin8:
	ld	(hl),0ffh	;change flag to show I/O complete
	jr	dskin2		;be sure both devices have their interrupts
;				;cleared

intxit:
	pop	hl
	pop	af		;restore registers
	ei			;allow interrupts
	reti			;exit and reset daisy chain

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

	;; call	msgdump
	;; db	'not0FFH$'
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

	;; call	msgdump
	;; db	'devavail$'
	ld	(newpdb),iy	;save PDB address for later
	ld	(newldb),ix	;also save pointer to LDB
	ld	h,(ix+_dph+1)
	ld	l,(ix+_dph)	;put DPH address in <HL>

	bit	pdfhome,(iy+pdflgs)	;has it been homed yet ??
	ret	nz		;if already homed...exit

	;; call	msgdump
	;; db	'homed$'
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
	;; call	msgdump
	;; db	'selerr$'
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
	;; call	msgdump
	;; db	'drhome$'
	jp	(hl)		;go do home and return to routine
;				;.. that called this routine.
;				;.. <IY> points to PDB
;				;Note: This routine returns 0 if home ok
;				;      and 1 if error on home.

	newpage
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

SECTRAN:
	;; call	regdump
	;; ld	hl,dbgmsg
	;; call	putmsg
	;; call	puthex2
	ex	de,hl
	ld	a,h
	or	l		;find out if translate table exists
	jr	z,sectr5	;if not...branch
	add	hl,bc		;point to translated sector number
	ld	l,(hl)		;get (one byte) sector value
	ld	h,0
	;; ld	c,' '
	;; call	dbgout
	;; ld	b,h
	;; ld	c,l
	;; call	puthex2
	;; push	hl
	;; ld	hl,newln
	;; call	putmsg
	;; pop	hl
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

	newpage
;----------------------------------------------------------------------
;
;	READ entry point for CBIOS
;	Returns: <A> = 0 if no errors occured
;		 <A> = 1 if errors
;
;----------------------------------------------------------------------

read:
	;; ld	hl,readmsg
	;; call	putmsg
	;; ld	bc,(newcyl)
	;; call	puthex4
	;; ld	c,' '
	;; call	dbgout
	;; ld	bc,(newpsc)
	;; call	puthex4
	;; ld	hl,newln
	;; call	putmsg

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
	ld	(_dmabpb+_bppdb),hl	;release special BPB
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

;----------------------------------------------------------------------
;
;	WRITE entry point to CBIOS
;	<C> = 0, normal write
;	<C> = 1, write to directory sector
;	<C> = 2, write to first sector of new data block
;	Returns: <A> = 0 if no errors occured
;		 <A> = 1 if errors
;
;----------------------------------------------------------------------

write:
	ld	a,c
	ld	(sectype),a		;save code which shows sector type
	ld	iy,(newpdb)		;point to PDB
	bit	pdfbuf,(iy+pdflgs)	;is the buffer to be the caller's
;					;.. "DMA" area ??
	jr	z,writ10		;if not...branch

	call	rw110		;fill in special BPB,
;				;.. on return <IX> points to special BPB
	call	rw280		;<IX> must point to BPB.  As necessary,
;				;.. do drive select, seek,
;				;.. head select and sector select.
;				;.. Do the write.
;				;<A> = 0 if no error, else <A> = 1

	jr	read04		;same exit as for read

writ10:
	call	rw150		;Search buffer pool BPB's for match on
;				;.. PDB address and disk address.  If match
;				;.. occurs, move BPB to top of list.  Return
;				;.. 0 (match) or 1 (no match) in <A>.  Return
;				;.. pointer to BPB in <IX> if match.
	or	a
	jr	z,writ15	;skip physical I/O if match found
	call	rw170		;Purge last buffer pool BPB.  Write out buffer
;				;.. if necessary.  Init BPB with PDB address
;				;.. and disk address.  Put BPB address in <IX>.
	ld	a,(sectype)	;get type of sector to be written
	cp	2		;is this the first sector of a new block ??
	jr	z,writ13	;if yes...skip pre-read

	call	rw250		;<IX> must point to BPB.  As necessary,
;				;.. do drive select, seek,
;				;.. head select and sector select.
;				;.. Pre-read physical sector into buffer.
	or	a		;<A> = 0 if no error, else <A> = 1
	ret	nz		;exit with I/O error indicator if read not ok

writ13:
	call	rw150		;Move BPB to top of list.  Return pointer to
;				;BPB in <IX>

writ15:
	ld	a,1		;indicate direction of data transfer
;				;.. (ie. from caller's "DMA" area to buffer)
	call	rw190		;move sector to buffer
	set	bpfwrt,(ix+bpflgs)	;set bit to show physical write is
;					;.. needed.
	ld	a,(sectype)	;get sector type
	cp	1		;is it a directory sector ??
	jr	z,writ20	;if yes...branch
	xor	a		;clear <A> to show ok write
	ret			;exit

writ20:
	ld	bc,bpblen	;put length of 1 BPB in <BC>
	ld	hl,bpbend	;point to one byte beyond last BPB
	ld	ix,bpt		;point to buffer pool table
	ld	e,(ix+numbuf)	;number of buffers (and BPB's)

writ24:
	or	a		;turn off carry
	sbc	hl,bc		;point to previous BPB
	push	hl
	pop	ix		;put address in <IX>
	bit	bpfwrt,(ix+bpflgs)	;do we need to write this buffer ??
	jr	z,writ28		;if not...branch

	push	bc
	push	de
	push	hl
	call	rw280		;<IX> must point to BPB.  As necessary,
;				;.. do drive select, seek,
;				;.. head select and sector select.
;				;.. Do the write.
;				;return code in <A> is ignored except for
;				;.. write for first BPB in list (which will
;				;.. be last BPB to have its buffer written).
	pop	hl
	pop	de
	pop	bc

writ28:
	dec	e
	jr	nz,writ24
	ret			;exit with return code in <A>

	newpage
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

	if	mpm20
	call	swtuser
	endif
	in	a,(bankpt)	;get current memory and DMA bank select info
	and	00011000b	;get current CPU bank number since this is
;				;.. where caller's "DMA" area is
	rlca
	rlca
	rlca			;move bank number into position for DMA bank
;				;.. number
	ld	(ix+bpmsk),a	;save for later
	if	mpm20
	call	swtsys
	endif

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

	newpage
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
;	jr	rw220		;do sector select and exit to routine which
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
	;; call	msgdump
	;; db	'rw220$'
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

	newpage
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

sectype:	db	0		;code which shows the type of sector to be
;				;.. written

ldsb:				;Logical Disk Select Block
	dw	ldat		;Address of Logical Disk Address Table
	db	0		;Reserved
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

hdstat:	db	0		;hard disk status byte
hioflg:	db	0ffh		;initialize to show no I/O pending

rwcmd:	db	0		;command for common read/write routine
;	The following 2 variables must be kept together and in the order
;	shown.
rwptnc:
	db	dmapt		;DMA port address
rwlen:	db	0		;count of DMA commands

rwdmap:	dw	0		;address of DMA commands
