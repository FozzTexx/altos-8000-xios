;----------------
; Debug routines
;----------------
	
statpt	equ	01dh		;SIO status port for console
datapt	equ	01ch		;SIO data port for console
eom	equ	'$'		;end of message indicator
	
puthex4:
	push	bc
	ld	c,b
	call	puthex2
	pop	bc
puthex2:
	push	bc
	sra	c
	sra	c
	sra	c
	sra	c
	call	puthex
	pop	bc
puthex:
	push	af
	push	bc
	ld	a,c
	and	a,0Fh
	add	a,'0'
	cp	a,'9'+1
	jp	c,isdig
	add	a,'A'-'9'-1
isdig:	ld	c,a
	call	dbgout
	pop	bc
	pop	af
	ret

	;; Write character in C to console 0
dbgout:
	ld	a,010h		;reset EXT/STATUS interrupts
	out	(statpt),a	;issue command to Z80-SIO
	in	a,(statpt)	;read status
	and	00ch		;look for DCD (wired to DTR) and xmit ready
	cp	00ch		;both must be set
	jr	nz,dbgout	;if not...branch

	ld	a,c
	out	(datapt),a	;send data character on its way
	ret			;exit

putmsg:
	push	af
	push	bc
mnext:	ld	a,(hl)
	cp	a,eom
	jp	z,mdone
	ld	c,a
	call	dbgout
	inc	hl
	jp	mnext
mdone:	pop	bc
	pop	af
	ret

newln:	db	"\r\n$"
regmsg:	db	"\r\nAF: $  BC: $  DE: $  HL: $\r\nIX: $  IY: $  SP: $  RT: $"
rgsave:	dw	0
	dw	0

regdump:
	call	flagpr
	ld	(rgsave),hl	; Save HL to restore when done
	pop	hl		; Get return address
	push	hl		; put it back
	push	af		; Save AF to restore when done
	push	bc		; Save BC to restore when done
	push	de		; Save DE to restore when done

	;; Put all registers on stack to print them
	push	hl		; Push RT 7 - return address

	push	af		; Save flags before something mangles them
	pop	hl
	ld	(rgsave+2),hl

	ld	hl,0
	ccf
	add	hl,sp
	push	hl		; Push SP 6
	
	push	iy		; Push IY 5
	push	ix		; Push IX 4
	ld	hl,(rgsave)
	push	hl		; Push HL 3
	push	de		; Push DE 2
	push	bc		; Push BC 1
	ld	hl,(rgsave+2)
	push	hl		; Push AF 0

	;; Print everything saved to stack
	ld	d,8
	ld	hl,regmsg
regprnt:
	call	putmsg
	inc	hl		; Move past '$'
	pop	bc
	call	puthex4
	dec	d
	jr	nz,regprnt

	ld	hl,newln
	call	putmsg
	
	ld	hl,(rgsave)	; Restore HL
	pop	de		; Restore DE
	pop	bc		; Restore BC
	pop	af		; Restore AF
	ret

flgstr:	db	'\r\nFlags: $'
flgpos:	db	'sz0h0vncSZ1H1VNC'

flagpr:
	push	af
	push	hl
	push	de
	push	bc
	push	af		; Save flags to pop after printing message

	ld	hl,flgstr
	call	putmsg
	
	pop	bc
	ld	a,c		; Flags now in A
	ld	hl,flgpos
	ld	de,flgpos+8
	ld	b,8
nxflag:	
	sla	a
	jr	nc,flag0
	ex	de,hl
	ld	c,(hl)
	ex	de,hl
	jr	flag1
flag0:
	ld	c,(hl)
flag1:
	push	af		; dbgout destroys A
	call	dbgout
	pop	af
	inc	hl
	inc	de
	djnz	nxflag
	
	pop	bc
	pop	de
	pop	hl
	pop	af
	ret

msgdump:
	ld	(rgsave),hl	; Save HL to restore when done
	pop	hl		; Get return address
	call	putmsg		; Print string that is just after call
	inc	hl
	push	hl		; Return to instruction after string
	ld	hl,(rgsave)	; Restore HL
	jp	regdump
	
	;; Dump 128 byte sector at HL
	;; destroys af, bc, hl
dumprec:
	ld	b,128
nextdmp:
	ld	c,(hl)
	call	puthex2
	ld	c,' '
	call	dbgout
	ld	a,b
	and	a,0Fh
	cp	a,1
	jr	nz,noteol
	ld	c,'\r'
	call	dbgout
	ld	c,'\n'
	call	dbgout
noteol:
	inc	hl
	djnz	nextdmp
	ret
	
dbgmsg:	db	"debug $"

debug:
	di
	ld	hl,dbgmsg
	call	putmsg
	call	dbgout
	ld	hl,newln
	call	putmsg
	call	regdump
	jp	$
	
debuga:
	ld	c,"A"
	jp	debug
debugb:
	ld	c,"B"
	jp	debug
debugc:
	ld	c,"C"
	jp	debug
debugd:
	ld	c,"D"
	jp	debug
debuge:
	ld	c,"E"
	jp	debug
debugf:
	ld	c,"F"
	jp	debug
debugg:
	ld	c,"G"
	jp	debug
debugh:
	ld	c,"H"
	jp	debug
debugi:
	ld	c,"I"
	jp	debug
debugj:
	ld	c,"J"
	jp	debug
debugk:
	ld	c,"K"
	jp	debug
debugl:
	ld	c,"L"
	jp	debug
debugm:
	ld	c,"M"
	jp	debug
debugn:
	ld	c,"N"
	jp	debug
debugo:
	ld	c,"O"
	jp	debug
debugp:
	ld	c,"P"
	jp	debug
debugq:
	ld	c,"Q"
	jp	debug
debugr:
	ld	c,"R"
	jp	debug
debugs:
	ld	c,"S"
	jp	debug
debugt:
	ld	c,"T"
	jp	debug
debugu:
	ld	c,"U"
	jp	debug
debugv:
	ld	c,"V"
	jp	debug
debugw:
	ld	c,"W"
	jp	debug
debugx:
	ld	c,"X"
	jp	debug
debugy:
	ld	c,"Y"
	jp	debug
debugz:
	ld	c,"Z"
	jp	debug
