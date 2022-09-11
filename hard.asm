;----------------------------------------------------------------------
;
;	Various equates
;
;----------------------------------------------------------------------

;	ports for the hard disk controller

hdrhdpt	equ	020H		;Drive/head port
hdatpt	equ	021H		;Data port
hpcylpt	equ	021H		;Previous cylinder port
hsecpt	equ	021H		;Sector port
hncylpt	equ	022H		;New cylinder port
hcmdpt	equ	023H		;Command port
	ifndef	hdstpt
hdstpt	equ	023H		;Status port
	endif

;	Commands for hard disk controller

hcmdhom	equ	020H		;Restore (Home)
hcmdsk	equ	010H		;Seek
hcmdwrt	equ	002H		;Write
hcmdred	equ	001H		;Read
hcmdnul	equ	000H		;Null command

;	Status bit meanings

hsrdy	equ	080H		;Ready
hswrtf	equ	040H		;Write fault
;	equ	020H		;Not used
hscrcer	equ	010H		;CRC error
hsrnf	equ	008H		;Record not found
hsbdsec	equ	004H		;Bad sector
hstc	equ	002H		;Task complete
hsbusy	equ	001H		;Busy

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

	out	(hdrhdpt),a	;.. else select new drive
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
	ld	a,hcmdhom	;home command
	call	hiogo		;issue command and wait for finish
	and	hsrdy+hstc+hsbusy
	cp	hsrdy+hstc	;everything go ok ??
	ld	a,1		;preset <A> to show error
	ret	nz		;exit if error

	set	pdfhome,(iy+pdflgs)	;show home has been done
	xor	a		;clear <A> to show no error
	out	(hncylpt),a	;low byte of cylinder address
	out	(hncylpt),a	;high byte of cylinder address
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
	out	(hpcylpt),a
	ld	a,h		;high byte
	out	(hpcylpt),a	;set old cylinder register
	ld	d,(ix+bpcyl+1)
	ld	e,(ix+bpcyl)	;cylinder number from BPB
	ld	a,e		;low byte
	out	(hncylpt),a
	ld	a,d		;high byte
	out	(hncylpt),a	;set new cylinder number
;				;Note: The new cylinder register must
;				;..    be reloaded with the current cylinder
;				;..    number in case a seek is not necessary.
	xor	a		;preset for return with no errors
;				;.. and reset carry
	sbc	hl,de		;compare cylinder numbers
	ret	z		;return if equal

	ld	(iy+pdcyl+1),d
	ld	(iy+pdcyl),e	;save new cylinder in PDB
	ld	a,hcmdsk	;seek command
	call	hiogo		;issue command and wait for interrupt
	and	hsrdy+hstc+hsbusy
	cp	hsrdy+hstc	;everything go ok ??
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
	out	(hdrhdpt),a	;select proper side
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
	out	(hsecpt),a	;inform controller
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
	ld	a,hcmdred
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

	jr	hrw100		;go to common I/O routine

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
	ld	a,hcmdwrt
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

	jr	hrw100		;go to common I/O routine

;----------------------------------------------------------------------
;
;	Various common read/write routines
;
;----------------------------------------------------------------------

hrw100:
	in	a,(statpt)	;get status
	and	hsrdy+hsbusy	;isolate interesting bits
	cp	hsrdy		;check for ready and not busy
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
	call	hiogo		;issue command and wait for interrupt
	ld	b,a		;save status
	and	hsrdy+hswrtf+hscrcer+hsrnf+hsbdsec+hstc+hsbusy
	cp	hsrdy+hstc	;check for errors
	ld	a,0		;preset for good return
	ret	z		;return if no errors

	ld	a,(rwcmd)	;get previous I/O command
	cp	hcmdred		;check for read command
	ld	a,0		;preset for read
	jr	z,rw106		;if read...branch

	inc	a		;else show write

rw106:
;				;<B> = status
	call	rwc200		;record temporary I/O error

	ld	a,b
	and	hswrtf		;check for write fault
	jr	z,rw108		;branch if not write fault

	xor	a
	out	(hdrhdpt),a	;deselect drive to clear write fault
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

	newpage
;----------------------------------------------------------------------
;
;	Common routine to be sure flag bit is not set,
;	to issue command and to wait for completion.
;	Status is saved and also returned in <A>.
;
;----------------------------------------------------------------------

hiogo:
	ld	c,a		;save command
	in	a,(statpt)	;get current status
	ld	b,a		;save status
	and	hsrdy+hsbusy	;isolate interesting bits
	cp	hsrdy		;only ready should be on
	ld	a,b		;restore status
	ret	nz		;return if not ready or if busy

	ld	a,c		;get command
	ld	hl,hioflg	;point to hard disk
	ld	(hl),0		;show I/O is pending
	out	(hcmdpt),a	;give hard disk command
	xor	a		;same as I/O flag

hiogo4:
	cp	(hl)		;look at I/O flag
	jr	z,hiogo4	;loop until I/O complete

	ld	a,(hdstat)	;retrieve status from I/O operation
	ret			;exit

;----------------------------------------------------------------------
;
;	Control blocks for disk I/O
;
;----------------------------------------------------------------------

;; mpt:				;Master Pointer Table
;; 	dw	ldsb		;Address of LDSB (Logical Disk Select Block)
;; 	dw	dgb0		;Address of first DGB (Device Group Block)
;; 	dw	bpt		;Address of BPT (Buffer Pool Table)
;; 	dw	fdmt		;Address of FDMT (Floppy Disk Mode Table)
;; 	dw	ptlb		;Address of PTLB (Printer/Terminal Lookup
;; ;				;.. Block)
;; 	dw	0		;Address of BSPIT (Bootstrap Parameter
;; ;				;.. Information Table).  Not used in CBIOS.
;; 	dw	0		;Address of "special" entry point in CBIOS.
;; ;				;.. The code at this entry point provides
;; ;				;.. support for certain diagnostic and utility
;; ;				;.. functions.

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

;DPBs for hard disk

;	diskdef	6,0,67,,16384,512,512,0,0
dpbhd1:				;8,388,608 bytes
	dw	68		;Sec per track
	db	7		;Block shift
	db	127		;Block mask
	db	7		;Extent mask
	dw	511		;(Disk size) - 1
	dw	511		;(Directory max) - 1
	db	128		;Alloc0
	db	0		;Alloc1
	dw	0		;Check size
	dw	0		;Offset

;	diskdef	7,0,67,,4096,74,128,0,0
dpbhd2:				;303,104 bytes
	dw	68		;Sec per track
	db	5		;Block shift
	db	31		;Block mask
	db	3		;Extent mask
	dw	73		;(Disk size) - 1
	dw	127		;(Directory max) - 1
	db	128		;Alloc0
	db	0		;Alloc1
	dw	0		;Check size
	dw	0		;Offset

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

dmabpb:				;This is the special BPB which is used to
;				;.. refer to the caller's "DMA" area
	dw	0		;Address of PDB for this BPB
	dw	0		;Address of buffer area
	dw	0		;Cylinder number
	db	0		;Head number
	db	0		;Physical sector number
	db	0		;Bank mask for bank containing caller
	db	0		;Flag bits

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

dirbuf:	ds	128		;Directory work area
	
;Hard disk work areas
csv4:	;ds	0		;Changed disks work area (not used)
alv4:	ds	64		;Allocation work area
csv5:	;ds	0		;Changed disks work area (not used)
alv5:	ds	16		;Allocation work area
csv6:	;ds	0		;Changed disks work area (not used)
alv6:	ds	64		;Allocation work area
csv7:	;ds	0		;Changed disks work area (not used)
alv7:	ds	16		;Allocation work area

;Buffers for blocking/de-blocking
buff1:	ds	515
buff2:	ds	515
