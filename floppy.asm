;----------------------------------------------------------------------
;
;	Various equates
;
;----------------------------------------------------------------------

fdspt	equ	008H		;Floppy disk select port
;	Various bits in above port
fhldmsk	equ	002H		;Shows whether or not head is loaded
fdenmsk	equ	001H		;Sets recording density

fsdslpt	equ	009H		;Side select port
;	Various bits in above port
fscmsk	equ	080H		;Side count bit
fssmsk	equ	020H		;Side select bit

;	ports for 1797

	ifndef	fdstpt
fdstpt	equ	004H		;1797 status register
	endif
fcmdpt	equ	004H		;1797 command register
fcylpt	equ	005H		;1797 cylinder register (The 1797
;				;.. booklet calls this a track register.)
fsecpt	equ	006H		;1797 sector register
fdatpt	equ	007H		;1797 data register

;	Commands for FD1797

fcmdnul	equ	0D0H		;Null command
fcmdsku	equ	010H		;Seek, unload head at beginning, no verify
fcmdskl	equ	01CH		;Seek, load head at beginning, verify
fcmdhom	equ	008H		;Restore (Home), load head at beginning,
;				;.. no verify
fcmdred	equ	088H		;Read, single record, IBM sector length,
;				;.. no 15 ms. delay -- side select determined
;				;.. dynamically
fcmdwrt	equ	0A8H		;Write, single record, IBM sector length,
;				;.. no 15 ms. delay, data mark -- side select
;				;.. determined dynamically

;	Status bit meanings - Type I  and II Commands

fsnrdy	equ	080H		;Not ready
fswrtp	equ	040H		;Write protect
fswrtf	equ	020H		;Write fault
fsrnf	equ	010H		;Record not found
fsskerr	equ	010H		;Seek error
fscrcer	equ	008H		;CRC error
fsldata	equ	004H		;Lost data
fsbusy	equ	001H		;Busy

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

	in	a,(fcylpt)	;get current "cylinder"
	out	(fdatpt),a	;set up to seek to the cylinder we are
;				;.. already on
	ld	a,(curstp)	;get stepping rate for old drive
	or	fcmdsku		;Combine with command to seek and unload
;				;.. the head.  Since we are seeking to the
;				;.. cylinder we are already on, the effect
;				;.. of this command is simply to unload
;				;.. the head.
	call	fiogo		;issue command and wait for interrupt

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
	ld	a,fdenmsk	;.. else set double density

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
	in	a,(fsdslpt)
	and	0ffh-fssmsk
	out	(fsdslpt),a	;force side 0 to be selected

hom802:
	res	pdfhome,(iy+pdflgs)	;show no home done
	xor	a
	ld	(iy+pdcyl),a
	ld	(iy+pdcyl+1),a	;show current cylinder number as 0
	ld	a,(curstp)	;get drive stepping rate
	or	fcmdhom		;combine with home command
	call	fiogo		;issue command and wait for finish
	and	fsnrdy+fsskerr+fscrcer	;everything go ok ??
	ld	a,1		;preset <A> to show error
	ret	nz		;exit if error

	set	pdfhome,(iy+pdflgs)	;show home has been done
	bit	pdfsid,(iy+pdflgs)	;has number of sides been
;					;.. determined ??
	jr	nz,hom809	;if yes...branch

	in	a,(fsdslpt)	;get info on number of sides
	and	fscmsk		;check for number of sides
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
	out	(fcylpt),a	;always set current cylinder (track) reg.
	cp	(ix+bpcyl)	;cylinder number from BPB
	ld	a,0		;preset for return with no errors
	ret	z		;return if equal

	ld	a,(ix+bpcyl)	;get new cylinder
	ld	(iy+pdcyl),a	;save in PDB
	out	(fdatpt),a	;send to controller
	ld	a,(curstp)	;get stepping rate
	or	fcmdskl		;seek command, load head combined with
;				;.. proper stepping rate
	call	fiogo		;issue command and wait for interrupt
	and	fsnrdy+fsskerr+fscrcer	;check for not ready, seek error
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
	in	a,(fsdslpt)	;get current info from side select port
	and	0ffh-fssmsk	;eliminate current side select value
	or	b		;combine with new side select
	out	(fsdslpt),a	;select proper side
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
	in	a,(fdstpt)	;status from 1797
	and	fsnrdy		;check for not ready
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
	or	fcmdred		;combine with read command
	call	fiogo		;issue command and wait for interrupt
	ld	b,a		;save status
	in	a,(dmapt)	;read status from DMA controller
	and	dmaeob		;look only at "end of block" indicator
	ld	c,a		;save
	ld	a,b		;status from 1797
	and	fsnrdy+fsrnf+fscrcer+fsldata+fsbusy
;				;check for errors
	or	c		;combine with end of block indicator
;				;.. (0 = eob)
;; 	push	af
;; 	in	a,(25h)
;; 	and	a,018h
;; 	ld	c,a
;; 	ld	a,(bankno)
;; 	;; cp	c
;; 	;; jr	z,zerobank
;; 	call	puthex2
;; 	ld	c,a
;; 	call	puthex2
;; 	ld	h,(ix+_buf+1)
;; 	ld	l,(ix+_buf)	;get address of buffer
;; 	ld	b,h
;; 	ld	c,l
;; 	call	puthex4
;; 	ld	c,'\r'
;; 	call	dbgout
;; 	ld	c,'\n'
;; 	call	dbgout
;; 	;; call	dumprec
;; zerobank:	
;; 	pop	af
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
	ld	a,fcmdnul	;null command so that we can read
	out	(fcmdpt),a	;.. command status
	in	a,(fdstpt)	;status from 1797
	and	fsnrdy+fswrtp	;check for not ready, write protect
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
	or	fcmdwrt		;combine with write command
	call	fiogo		;issue command and wait for interrupt
	ld	b,a		;save status
	and	fsnrdy+fswrtf+fsrnf+fscrcer+fsldata+fsbusy
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
	ld	a,fcmdnul	;command to clear 1797
	out	(fcmdpt),a	;issue command
	in	a,(fdstpt)	;get status
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
	out	(fsecpt),a	;set 1797

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

fiogo:
	ld	hl,fioflg	;point to floppy I/O flag
	ld	(hl),0		;show I/O is pending
	out	(fcmdpt),a	;give floppy disk command

	ld	a,6		;set 3 second time limit

fiogo2:
	ld	(time0),a
	ld	bc,45455	;enough loops for .5 second

fiogo3:
	dec	bc
	ld	a,b
	or	c		;check for end of .5 second
	jr	z,fiogo4	;if .5 second up...branch

	xor	a		;check for completion
	cp	(hl)
	jr	z,fiogo3	;loop until I/O complete

	ld	a,(fdstat)	;retrieve status from I/O operation
;	jp	regdump
	ret			;exit

fiogo4:
	ld	a,(time0)
	dec	a
	jr	nz,fiogo2	;repeat loop until total time has elapsed

	ld	a,fsnrdy+fsrnf	;simulate not ready and record not found
;				;.. (or not ready and seek error)
	ret			;exit

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
	ld	d,(ix+_buf+1)
	ld	e,(ix+_buf)	;get address of buffer
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

	newpage
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
	db	fdatpt		;port B (ie. device) address
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
	db	fdatpt		;port B (ie. device) address
	db	0CFH		;load starting address for both ports
;				;.. clear byte counter
	db	005H		;transfer port A to port B
	db	0CFH		;load starting address for both ports
;				;.. clear byte counter
	db	087H		;enable DMA controller
ldmawr	equ	$-dmawrt	;length of write commands for DMA controller

fdmt:				;Floppy Disk Mode Table
	dw	sdxlt		;Address of single density sector translate
;				;.. table
	dw	dpbm0		;Address of model DPB's for mode 0
	dw	dpbm1		;Address of model DPB's for mode 1
	dw	dpbm2		;Address of model DPB's for mode 2

sdxlt:				;Single density sector translate table
	db	01,07,13,19,25,05,11,17,23,03,09,15,21
	db	02,08,14,20,26,06,12,18,24,04,10,16,22

; Model DPB's

;	***** The following 2 DPB's must be kept together and in the order
;	***** shown.

;	diskdef	0,1,26,,1024,243,64,64,2,0
dpbm0:				;Single sided, single density
	dw	26		;Sec per track
	db	3		;Block shift
	db	7		;Block mask
	db	0		;Extent mask
	dw	242		;(Disk size) - 1
	dw	63		;(Directory max) - 1
	db	192		;Alloc0
	db	0		;Alloc1
	dw	16		;Check size
	dw	2		;Offset

;	diskdef	1,1,26,,4096,123,128,128,2
;				;Double sided, single density
	dw	26		;Sec per track
	db	5		;Block shift
	db	31		;Block mask
	db	3		;Extent mask
	dw	122		;(Disk size) - 1
	dw	127		;(Directory max) - 1
	db	128		;Alloc0
	db	0		;Alloc1
	dw	32		;Check size
	dw	2		;Offset

;	The translate table associated with the above DPB's is "sdxlt" shown
;	above.  Only the single density diskette will have a software "skew"
;	factor associated with it.  The remaining formats will have the
;	sectors skewed on the disk when the disk is formatted.


;	***** The following 2 DPB's must be kept together and in the order
;	***** shown.

;	diskdef	2,1,60,,4096,140,128,128,2
dpbm1:				;Single sided, double density, 512 byte sectors
	dw	60		;Sec per track
	db	5		;Block shift
	db	31		;Block mask
	db	3		;Extent mask
	dw	139		;(Disk size) - 1
	dw	127		;(Directory max) - 1
	db	128		;Alloc0
	db	0		;Alloc1
	dw	32		;Check size
	dw	2		;Offset

;	diskdef 3,1,60,,4096,285,256,256,2
;				;Double sided, double density, 512 byte sectors
	dw	60		;Sec per track
	db	5		;Block shift
	db	31		;Block mask
	db	1		;Extent mask
	dw	284		;(Disk size) - 1
	dw	255		;(Directory max) - 1
	db	192		;Alloc0
	db	0		;Alloc1
	dw	64		;Check size
	dw	2		;Offset

;	***** The following 2 DPB's must be kept together and in the order
;	***** shown.

;	diskdef 4,1,48,,2048,225,96,96,2,0
dpbm2:				;Single sided, double density, 128 byte sectors
;				;.. 1.4 compatibility
	dw	48		;Sec per track
	db	4		;Block shift
	db	15		;Block mask
	db	0		;Extent mask
	dw	224		;(Disk size) - 1
	dw	95		;(Directory max) - 1
	db	192		;Alloc0
	db	0		;Alloc1
	dw	24		;Check size
	dw	2		;Offset

;	diskdef	5,1,48,,4096,228,256,256,2
;				;Double sided, double density, 128 byte sectors
	dw	48		;Sec per track
	db	5		;Block shift
	db	31		;Block mask
	db	3		;Extent mask
	dw	227		;(Disk size) - 1
	dw	255		;(Directory max) - 1
	db	192		;Alloc0
	db	0		;Alloc1
	dw	64		;Check size
	dw	2		;Offset

	newpage
dph000:				;DPH for first floppy disk drive
	dw	sdxlt		;Address of sector translate table
	dw	0
	dw	0,0		;Work areas
	dw	dirbuf		;Address of directory buffer work area
	dw	dpbm0		;Address of DPB
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
	db	000h		;Mode number
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

;Floppy disk work areas
csv0:	ds	64		;Changed disks work area
alv0:	ds	36		;Allocation work area
csv1:	ds	64		;Changed disks work area
alv1:	ds	36		;Allocation work area
csv2:	ds	64		;Changed disks work area
alv2:	ds	36		;Allocation work area
csv3:	ds	64		;Changed disks work area
alv3:	ds	36		;Allocation work area
