.z80
	title	"LDRB224H, Copyright 1981, Altos Computer Systems - 29 May 81"

;; 	name	('LDRB2H')

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
;	Version Date:	May 29, 1981
;		Add "special" entry point address to MPT.  At the present
;		time this entry point is not supported.
;
;	This module of the CP/M 2.24 Loader BIOS contains most of the
;	disk control blocks including the Master Pointer Table and
;	various buffers and workareas.
;
;----------------------------------------------------------------------

	;; public	mpt
	;; public	bpbpl,bpbend,bspit
	;; public	dgb0,pdb00,ldb000,dph000

	;; extrn	drv800,hom800,cyl800,hed800,sec800,red800

	page	62
;----------------------------------------------------------------------
;
;	Various equates
;
;----------------------------------------------------------------------

	ifndef	iobyte
iobyte	equ	003H		;Location of Intel IOBYTE
	endif
	ifndef	cdisk
cdisk	equ	004H		;Location of current drive number
	endif

	include	bptbpb.equ

;----------------------------------------------------------------------
;
;	Control blocks for disk I/O
;
;----------------------------------------------------------------------

mpt:				;Master Pointer Table
	dw	0		;Address of LDSB (Logical Disk Select Block)
	dw	dgb0		;Address of first DGB (Device Group Block)
	dw	bpt		;Address of BPT (Buffer Pool Table)
	dw	fdmt		;Address of FDMT (Floppy Disk Mode Table)
	dw	0		;Address of PTLB (Printer/Terminal Lookup
;				;.. Block)
	dw	bspit		;Address of BSPIT (Bootstrap Parameter
;				;.. Information Table)
	dw	0		;Address of "special" code to support
;				;.. certain diagnostic and utility functions

dgb0:				;Device Group Block for Shugart 800 floppy
;				;.. disk drives
	dw	0		;Address of next DGB
	dw	pdb00		;Address of first PDB in this Device Group
	db	0		;Select mask from currently selected device
;	db	0		;Reserved
;	dw	drv800		;Address of drive select routine
;	dw	hom800		;Address of home routine
;	dw	cyl800		;Address of seek routine
;	dw	hed800		;Address of head select routine
;	dw	sec800		;Address of physical sector select routine
;	dw	red800		;Address of physical sector read routine
;	dw	wrt800		;Address of physical sector write routine

pdb00:				;Physical Device Block for floppy drive #1
	dw	dgb0		;DGB for this PDB
	dw	0		;Address of next PDB
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
	db	26		;Number of physical sectors per track
	dw	0		;Current cylinder number
	db	0		;Current head (ie. side) number
	db	0		;Current physical sector number
;	dw	0		;Count of temporary I/O errors
;	dw	0		;Count of permanent I/O errors
;	db	0		;Flag bits for I/O errors
;	db	0		;Status from most recent I/O error
;	dw	0		;Cylinder number for most recent I/O error
;	db	0		;Head number for most recent I/O error
;	db	0		;Physical sector number for most recent
;				;.. I/O error

ldb000:				;Logical device block for first logical device
	dw	pdb00		;Address of PDB for this LDB
	dw	0		;Address of next LDB
	dw	dph000		;Address of DPH for this logical device
	dw	0		;Offset for logical track 0 from physical
;				;.. track 0

	newpage
dph000:				;DPH for first floppy disk drive
	dw	sdxlt		;Address of sector translate table
	dw	0
	dw	0,0		;Work areas
	dw	dirbuf		;Address of directory buffer work area
	dw	dpbm2		;Address of DPB
	dw	csv0		;Address of disk change work area
	dw	alv0		;Address of disk allocate work area

bspit:				;Bootstrap Parameter Info Table
;				;NOTE: All parameters in this table
;				;      may be modified by the setup program.
	db	0		;Parameter Bits
;				;bit 0, 0 = one sided floppy disk drives
;				;	1 = two sided floppy disk drives
;				;bit 1, 0 = serial port printer
;				;	1 = parallel port printer
;				;bit 2 - 7 reserved
	db	0		;Reserved for future parameter bits
	db	0		;Default mode for floppy disk drives
	db	0		;Reserved for future use
	db	047h,00dh	;Baud rate for Console #2
	db	047h,00dh	;Baud rate for Consoles #3 & #4
	db	047h,00dh	;Baud rate for Serial Printer #1
	db	047h,00dh	;Baud rate for Serial Printer #2

fdmt:				;Floppy Disk Mode Table
	dw	sdxlt		;Address of single density sector translate
;				;.. table
	dw	dpbm2		;Address of model DPB

sdxlt:				;Single density sector translate table
	db	01,07,13,19,25,05,11,17,23,03,09,15,21
	db	02,08,14,20,26,06,12,18,24,04,10,16,22


; Model DPB

;	The following DPB may be changed by the setup program for CP/M

dpbm2:				;Single sided, single density, 128 byte sectors
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

;	The translate table associated with the above DPB is "sdxlt" shown
;	above.  Only the single density diskette will have a software "skew"
;	factor associated with it.  The remaining formats will have the
;	sectors skewed on the disk when the disk is formatted.


bpt:				;Buffer Pool Table
	dw	0		;Address of special BPB
	db	0		;Reserved
	db	(bpbend-bpbpl)/bpblen	;Number of buffers in buffer pool
	dw	515		;length of one buffer in buffer pool
bpbpl:				;These are the BPB's for buffers in the
;				;.. buffer pool.
	dw	0		;Address of PDB for this BPB
	dw	buff1		;Address of buffer area
	dw	0		;Cylinder number
	db	0		;Head Number
	db	0		;Physical sector number
	db	0		;Bank mask for bank containing buffer
	db	0		;Flag bits
bpbend:

	newpage
dirbuf:	ds	128		;Directory work area

;Floppy disk work areas
csv0:	ds	64		;Changed disks work area
alv0:	ds	36		;Allocation work area

;Buffers for blocking/de-blocking
buff1:	ds	515
endbuf:

	;; end
