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
	push	bc
	push	af
	ld	a,c
	and	a,0Fh
	add	a,'0'
	cp	a,'9'+1
	jp	c,isdig
	add	a,'A'-'9'-1
isdig:	ld	c,a
	call	dbgout
	pop	af
	pop	bc
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
	ld	(rgsave),hl	; Save HL to restore when done
	pop	hl		; Get return address
	push	hl		; put it back
	push	bc		; Save BC to restore when done
	push	de		; Save DE to restore when done

	;; Put all registers on stack to print them
	push	hl		; Push return address

	push	af		; Save flags before something mangles them
	pop	hl
	ld	(rgsave+2),hl

	ld	hl,0		; Push SP
	ccf
	add	hl,sp
	push	hl
	
	ld	hl,(rgsave)	; Push saved HL
	push	hl
	push	iy
	push	ix
	push	hl
	push	de
	push	bc
	ld	hl,(rgsave+2)	; Push saved AF
	push	hl

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
	
	pop	de		; Restore DE
	pop	bc		; Restore BC
	ld	hl,(rgsave)	; Restore HL
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
