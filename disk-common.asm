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

dirbuf:	ds	128		;Directory work area
