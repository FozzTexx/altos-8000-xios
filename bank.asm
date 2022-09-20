	ifndef	bankpt
bankpt	equ	025h
	endif
dmamask	equ	0E0h

tbuf	equ	0100h
	
	;; Value to fill is in <A>, length is in <BC>, dest is in <DE>
	;;  destroys <BC> <DE> <HL> 
fillmem:
	ld	h,d
	ld	l,e
	ld	(hl),a
	inc	de
	dec	bc
	ldir
	ret
	
banktest:
	ld	hl,bnkmsg
	call	putmsg
	call	getbank
	ld	c,a
	call	puthex
	ld	hl,newln
	call	putmsg
	ld	hl,tbuf
	call	dumprec

	ld	a,055h
	ld	bc,64
	ld	de,tbuf
	call	fillmem

	ld	hl,tbuf
	call	dumprec
	ld	hl,newln
	call	putmsg

	ld	c,3
	call	selbank
	ld	a,0AAh
	ld	bc,32
	ld	de,tbuf
	call	fillmem

	ld	c,2
	call	selbank
	ld	a,088h
	ld	bc,16
	ld	de,tbuf
	call	fillmem

	ld	c,1
	call	selbank
	ld	a,077h
	ld	bc,32
	ld	de,tbuf
	call	fillmem

	ld	hl,bnkmsg
	call	putmsg
	ld	c,2
	call	selbank
	call	getbank
	ld	c,a
	call	puthex
	ld	hl,newln
	call	putmsg
	ld	hl,tbuf
	call	dumprec
	
	ld	hl,bnkmsg
	call	putmsg
	ld	c,1
	call	selbank
	call	getbank
	ld	c,a
	call	puthex
	ld	hl,newln
	call	putmsg
	ld	hl,tbuf
	call	dumprec
	
	ld	hl,bnkmsg
	call	putmsg
	ld	c,3
	call	selbank
	call	getbank
	ld	c,a
	call	puthex
	ld	hl,newln
	call	putmsg
	ld	hl,tbuf
	call	dumprec

	jp	$
	ret

selmsg:	db	'Selecting '
bnkmsg:	db	'Bank: $'

	;; Returns selected bank in <A>
	;;  
getbank:
	in	a,(bankpt)
	rra
	rra
	rra
	and	3
	ret

	;; Bank to select is in <C>
	;;  destroys <A> <B>
selbank:
	;; cp	4
	;; jr	c,$

	;; push	hl
	;; ld	hl,selmsg
	;; call	putmsg
	;; call	puthex
	;; ld	hl,newln
	;; call	putmsg
	;; pop	hl

	;; ld	a,c
	;; add	a,03Ch
	;; ld	c,a		; Yes this mangles C but the bank is still in the lower bits
	;; call	dbgout
	
	in	a,(bankpt)
	and	dmamsk		; Preserve DMA bank and write protect
	ld	b,a
	ld	a,c
	rla
	rla
	rla
	and	018h
	or	b
	out	(bankpt),a
	ret
