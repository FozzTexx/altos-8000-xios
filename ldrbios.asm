	include	ldrb224a.asm
	include	ldrb224b.asm
	include	ldrb224c.asm
	include	ldrb224d.asm
	include	ldrb224e.asm
	include	ldrb224h.asm

;; hello:
;; 	ld	hl,hlomsg
;; 	jp	putmsg

;; putbin16:
;; 	push	de
;; 	push	af
;; 	push	bc
;; 	ld	e,2
;; nextby:	ld	d,8
;; nextbt:	ld	a,'0'
;; 	sla	b
;; 	adc	a,0
;; 	ld	c,a
;; 	call	conout
;; 	dec	d
;; 	jr	nz,nextbt
;; 	pop	bc
;; 	push	bc
;; 	ld	b,c
;; 	dec	e
;; 	jr	nz,nextby
;; 	pop	bc
;; 	pop	af
;; 	pop	de
;; 	ret

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
	call	conout
	pop	af
	pop	bc
	ret

putmsg:
	push	af
	push	bc
mnext:	ld	a,(hl)
	cp	a,eom
	jp	z,mdone
	ld	c,a
	call	conout
	inc	hl
	jp	mnext
mdone:	pop	bc
	pop	af
	ret

;; hlomsg:
;; 	db	'Hello World\r\n'
;; 	db	eom

;; trkmsg:
;; 	db	'Track:  '
;; 	db	eom

secmsg:
	db	'Sector: '
	db	eom

;; selmsg:
;; 	db	'Sel Disk DPH Addr: '
;; 	db	eom

readmsg:
	db	'READ '
	db	eom

newln:	db	'\r\n'
	db	eom

dumprec:
	ld	b,128
nextdmp:
	ld	c,(hl)
	call	puthex2
	ld	c,' '
	call	conout
	ld	a,b
	and	a,0Fh
	cp	a,1
	jr	nz,noteol
	ld	c,'\r'
	call	conout
	ld	c,'\n'
	call	conout
noteol:
	inc	hl
	dec	b
	jr	nz,nextdmp
	ret
