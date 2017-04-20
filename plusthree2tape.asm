; +3 DISC - TAPE CONVERTER:			
; V1.2:			
; SCREEN RESIDENT (org=20480):			
; 1 April 1993:			

	MODULE	plusthree2tape
			
	defc	catbuf = 54000
			
			
	org	20480	
			
start:	xor	a	
	call	cls	
	xor	a	
	call	8859	
	inc	hl	
	inc	de	
	ld	(hl),0	
	ld	b,1	
	ldir		
	ld	a,2	
	call	5633	
	ld	a,23	
	call	pager	
	ld	de,15616	
	ld	hl,55296-256	
	ld	(23606),hl	
	inc	h	
	ex	de,hl	
	ld	bc,768	
font1:	ld	a,(hl)	
	rra		
	or	(hl)	
	ld	(de),a	
	inc	hl	
	inc	de	
	dec	bc	
	ld	a,b	
	or	c	
	jr	nz,font1	
	ld	a,16	
	call	pager	
	ld	hl,fsel	
	ld	de,fsel+1	
	ld	bc,26	
	ld	(hl),b	
	ldir		
	ld	a,23	
	call	pager	
	ld	hl,catbuf	
	ld	de,catbuf+1	
	ld	(hl),b	
	ld	bc,844	
	Ldir		
	ld	a,16	
	call	pager	
	call	box	
	defb	5,6,4,20,32	
	call	messag	
	defb	22,6,7	
	defm	"PRESS SPACE TO CAT"	
	defb	22,7,7	
	defm	"OR ENTER FOR HELP!"	
	defb	255	
pspa:	call	654	
	ld	a,e	
	cp	21h	
	jp	z,help	
	cp	32	
	jr	nz,pspa	
	xor	a	
	ld	iy,334	
	call	dodos	
	ld	a,(23417)	
	ld	iy,301	
	call	dodos	
	ld	a,(23417)	
	ld	iy,337	
	call	dodos	
	ld	bc,16385	
	ld	de,catbuf	
	ld	hl,cfile	
	ld	iy,286	
	call	dodos	
	jp	nc,derror	
	ld	a,b	
	dec	a	
	jr	nz,isfil	
	call	box	
	defb	8,8,3,16,79	
	call	messag	
	defb	22,9,9	
	defm	"NO FILES FOUND"	
	defb	255	
	jp	derro1	
isfil:	dec	a	
	ld	(files),a	
	ld	hl,catbuf+13	
	ld	(ptcfch+1),hl	
	call	pcat	
	xor	a	
	ld	(curfil),a	
	ld	(temfil),a	
	ld	(select),a	
menu:	ld	a,48	
	call	cls1	
	ld	a,(curfil)	
	and	31	
	inc	a	
	ld	hl,22512	
	ld	de,16	
menu1:	add	hl,de	
	dec	a	
	jr	nz,menu1	
	ld	bc,4109	
menu2:	ld	(hl),c	
	inc	hl	
	djnz	menu2	
menu3:	xor	a	
	ld	(23560),a	
menu4:	ld	a,(23560)	
	and	a	
	jr	z,menu4	
	cp	13	
	jp	z,sloop	
	cp	32	
	jr	z,selone	
	cp	14	
	jp	z,start	
	cp	8	
	jr	c,menu3	
	cp	12	
	jr	nc,menu3	
	ld	hl,curfil	
	ld	b,(hl)	
	ld	hl,temfil	
	ld	(hl),b	
	sub	8	
	jr	nz,right	
	dec	(hl)	
right:	dec	a	
	jr	nz,down	
	inc	(hl)	
down:	dec	a	
	jr	nz,up	
	inc	(hl)	
	inc	(hl)	
up:	dec	a	
	jr	nz,chek	
	dec	(hl)	
	dec	(hl)	
chek:	ld	a,(hl)	
	and	a	
	jp	m,menu	
	ld	a,(files)	
	cp	(hl)	
	jr	c,menu	
	ld	a,(curfil)	
	xor	(hl)	
	and	32	
	ld	a,(hl)	
	ld	(curfil),a	
	jr	z,menu	
	bit	5,a	
	ld	hl,catbuf+13	
	jr	z,chek1	
	ld	hl,catbuf+429	
chek1:	ld	(ptcfch+1),hl	
	call	pcat	
	jr	menu	
			
selone:	ld	a,(select)	
	inc	a	
	cp	27	
	jp	z,menu	
	ld	(select),a	
	ld	c,a	
	ld	b,0	
	ld	hl,fsel-1	
	add	hl,bc	
	ld	a,(curfil)	
	inc	a	
	ld	(hl),a	
	call	pcat2	
	jp	menu	
			
pcat:	ld	a,48	
	call	cls	
pcat2:	call	messag	
	defb	22,0,0,255	
ptcfch:	ld	hl,catbuf+13	
	ld	b,32	
ptcf:	ld	a,23	
	call	pager	
	ld	a,(hl)	
	and	a	
	ret	z	
	push	bc	
	push	hl	
	ld	a,32	
	sub	b	
	ld	b,a	
	ld	a,(curfil)	
	and	32	
	or	b	
	ld	b,a	
	inc	b	
	ld	c,1	
	ld	ix,fsel	
ptcf2:	ld	a,(ix)	
	and	a	
	jr	z,nose1	
	cp	b	
	jr	nz,nosel	
	ld	a,c	
	add	64	
	ld	(self),a	
	ld	a,b	
	push	af	
	call	messag	
	defb	20,1	
self:	defb	0	
	defb	20,0,255	
	pop	af	
	jr	mok	
nosel:	inc	ix	
	inc	c	
	jr	ptcf2	
nose1:	ld	a,32	
	call	print	
mok:	ld	a,32	
	call	print	
	pop	hl	
	call	fnex	
	push	hl	
	call	fnprn	
	pop	hl	
	ld	a,23	
	call	pager	
	ld	a,9	
	cp	(hl)	
	ld	a,32	
	call	nc,print	
	ld	a,23	
	call	pager	
	ld	c,(hl)	
	inc	hl	
	ld	b,(hl)	
	push	hl	
	push	af	
	call	11563	
	call	11747	
	pop	af	
	pop	hl	
	inc	hl	
	pop	bc	
	djnz	ptcf	
	ld	a,16	
	call	pager	
	ret		
			
help:	ld	a,87	
	call	cls	
	ld	hl,helpta	
help1:	ld	a,(hl)	
	cp	255	
	jr	z,helpfi	
	push	hl	
	call	print	
	pop	hl	
	inc	hl	
	jr	help1	
helpfi:	call	messag	
	defb	22,15,25,255	
	ld	a,(pauset)	
	ld	c,a	
	ld	b,0	
	ld	a,23	
	call	pager	
	call	11563	
	call	11747	
	ld	a,16	
	call	pager	
helplo:	call	654	
	ld	a,e	
	cp	32	
	jp	z,start	
	ld	b,1	
	cp	25h	
	jr	z,inc	
	ld	b,255	
	cp	26h	
	jr	z,inc	
	jr	helplo	
			
inc:	ld	a,(pauset)	
	add	a,b	
	cp	255	
	jr	z,helplo	
	cp	6	
	jr	z,helplo	
	ld	(pauset),a	
	halt		
	halt		
	halt		
	jr	helpfi	
			
			
			
;Top 2/3 Screen Cls:			
;a=attribute byte:			
			
cls:	ld	hl,16384	
	ld	de,16385	
	ld	bc,4095	
	ld	(hl),l	
	ldir		
cls1:	ld	hl,22528	
	ld	de,22529	
	ld	bc,511	
	ld	(hl),a	
	ld	(23695),a	
	ld	(23693),a	
	ldir		
	ret		
			
			
; Modify File name to tape name:			
; hl=disc file name addr:			
; de=tape file name addr:			
			
			
; Call a +3 DOS routine:			
; iy=address of DOS routine:			
			
dodos:	push	af	
	push	bc	
	ld	a,7	
	ld	bc,32765	
	di		
	ld	(23388),a	
	out	(c),a	
	ei		
	pop	bc	
	pop	af	
	call	jumpiy	
	push	af	
	push	bc	
	ld	a,16	
	ld	bc,32765	
	di		
	ld	(23388),a	
	out	(c),a	
	ld	iy,23610	
	ei		
	pop	bc	
	pop	af	
	ret		
jumpiy:	jp	(iy)	
			
;Pager, enter:			
;a=page+16:			
			
pager:	di		
	push	af	
	push	bc	
	ld	bc,32765	
	out	(c),a	
	ld	(23388),a	
	pop	bc	
	pop	af	
	ei		
	ret		
			
			
;After enter on menu:			
			
sloop:	ld	hl,fsel	
	ld	a,(select)	
	and	a	
	jp	z,start	
	ld	b,a	
sloop1:	push	bc	
	push	hl	
	ld	a,(hl)	
	dec	a	
	call	lands	
	pop	hl	
	pop	bc	
	inc	hl	
	djnz	sloop1	
	jp	start	
			
;Convert catalogue data to filen:			
			
			
fnex:	push	hl	
	ld	a,23	
	call	pager	
	ld	hl,store	
	push	hl	
	ld	de,store+1	
	ld	bc,13	
	ld	(hl),32	
	ldir		
	pop	de	
	pop	hl	
	ld	b,8	
fnex1:	ld	a,(hl)	
	ld	(de),a	
	inc	hl	
	inc	de	
	djnz	fnex1	
	ld	a,46	
	ld	(de),a	
	inc	de	
	ld	b,3	
fnex2:	ld	a,(hl)	
	res	7,a	
	ld	(de),a	
	inc	hl	
	inc	de	
	djnz	fnex2	
	ld	a,255	
	ld	(de),a	
	ld	a,16	
	call	pager	
	ret		
			
;Open a file header:			
;Load into memory:			
;Save to tape:			
;a=file no in catalogue:			
			
lands:	inc	a	
	ld	bc,13	
	ld	hl,catbuf	
lands1:	add	hl,bc	
	dec	a	
	jr	nz,lands1	
	call	fnex	
	ld	a,7	
	call	cls	
	call	box	
	defb	7,8,4,16,79	
	call	messag	
	defb	22,8,9	
	defm	"LOADING FILE:"
	defb	22,9,9,255	
	call	fnprn	
	ld	bc,1	
	ld	de,1	
	ld	hl,store	
	ld	iy,262	
	call	dodos	
	jp	nc,derror	
	ld	b,0	
	ld	iy,271	
	call	dodos	
	jp	nc,derror	
copyhd:	ld	a,23	
	call	pager	
	push	ix	
	pop	hl	
	ld	a,(hl)	
	ld	(thead),a	
	ld	de,tlen	
	inc	hl	
	ld	bc,6	
	ldir		
	ld	a,16	
	call	pager	
	ld	a,(thead)	
	cp	4	
	jr	c,fok	
	call	box	
	defb	10,8,3,16,87	
	call	messag	
	defb	22,11,9	
	defm	"NOT A DOS FILE"	
	defb	255	
	ld	b,100	
fnok1:	halt		
	djnz	fnok1	
	ret		
fok:	ld	hl,24000	
	ld	(laddr),hl	
	ld	bc,0	
	ld	de,(tlen)	
	ld	hl,(laddr)	
	ld	iy,274	
	call	dodos	
	jp	nc,derror	
	ld	b,0	
	ld	iy,265	
	call	dodos	
	ld	hl,store	
	ld	de,tname	
adjust:	ld	bc,2573	
lback:	ld	a,(hl)	
	dec	c	
	jr	z,adok	
	inc	hl	
	cp	32	
	jr	nz,lok	
	jr	lback	
lok:	ld	(de),a	
	inc	de	
	djnz	lback	
	jr	adout	
adok:	ld	a,32	
adok1:	ld	(de),a	
	inc	de	
	djnz	adok1	
adout:	call	box	
	defb	9,9,4,14,48	
	call	messag	
	defb	22,10,10	
	defm	"SAVING FILE:"	
	defb	22,11,10,255	
	ld	hl,tname	
	ld	de,store	
	ld	bc,10	
	ldir		
	ld	a,255	
	ld	(de),a	
	call	fnprn	
			
;Saving to Tape:			
	ld	hl,(23613)	
	ld	(oldsp),hl	
	ld	hl,serror	
	push	hl	
	ld	(23613),sp	
	call	tpause	
	ld	ix,thead	
	ld	de,17	
	xor	a	
	call	4C2h	
	call	tpause	
	ld	ix,(laddr)	
	ld	de,(tlen)	
	ld	a,255	
	call	4C2h	
	pop	hl	
	ld	hl,(oldsp)	
	ld	(23613),hl	
	ret		
			
tpause:	ld	a,(pauset)	
	and	a	
	ret	z	
	ld	l,a	
	ld	h,0	
	ld	bc,tpauta	
	add	hl,bc	
	ld	b,(hl)	
tpaus1:	halt		
	djnz	tpaus1	
	ret		
tpauta:	defb	0,50,100,150,200,250	
pauset:	defb	0	
			
			
			
;Disc Error:			
;Jumps back to reCAT disc:			
			
derror:	call	box	
	defb	8,10,3,12,87	
	call	messag	
	defb	22,9,11	
	defm	"DISC ERROR"	
	defb	255	
derro1:	ld	b,100	
derr1:	halt		
	djnz	derr1	
	jp	start	
			
;Save Error (eg Break):			
			
serror:	ld	hl,(oldsp)	
	ld	(23613),hl	
	call	box	
	defb	8,10,3,12,87	
	call	messag	
	defb	22,9,11	
	defm	"TAPE ERROR"	
	defb	255	
	jr	derro1	
			
;Draw attr box:			
;Data stored after call:			
			
box:	pop	ix	
	ld	d,(ix)	
	ld	a,(ix+2)	
box1:	ex	af,af'	
	ld	e,(ix+1)	
	ld	c,(ix+3)	
box2:	call	xypos	
	ld	b,8	
box3:	ld	(hl),0	
	inc	h	
	djnz	box3	
	dec	h	
	ld	a,h	
	rrca		
	rrca		
	rrca		
	and	3	
	or	58h	
	ld	h,a	
	ld	a,(ix+4)	
	ld	(hl),a	
	inc	e	
	dec	c	
	jr	nz,box2	
	inc	d	
	ex	af,af'	
	dec	a	
	jr	nz,box1	
	ld	a,(ix+4)	
	ld	(23695),a	
	ld	(23693),a	
	ld	bc,5	
	add	ix,bc	
	jp	(ix)	
			
;Message Printer:			
;Text Stored after CALL:			
			
fnprn:	ld	hl,store	
fnprn1:	ld	a,(hl)	
	cp	255	
	ret	z	
	push	hl	
	call	print	
	pop	hl	
	inc	hl	
	jr	fnprn1	
			
messag:	pop	hl	
messa1:	ld	a,(hl)	
	cp	255	
	jr	z,mout	
	push	hl	
	call	print	
	pop	hl	
	inc	hl	
	jr	messa1	
mout:	inc	hl	
	jp	(hl)	
			
;Print Routine rst 16 substitute:			
			
print:	push	af	
	ld	a,23	
	call	pager	
	pop	af	
	rst	16	
	push	af	
	ld	a,16	
	call	pager	
	pop	af	
	ret		
			
;Calculate Scr addr from coords:			
;d=y coord:			
;e=x coord:			
			
xypos:	ld	a,d	
	and	7	
	rrca		
	rrca		
	rrca		
	add	e	
	ld	l,a	
	ld	a,d	
	and	248	
	or	64	
	ld	h,a	
	ret		
			
thead:	defb	0	
tname:	defb	32,32,32,32,32,32,32	
	defb	32,32,32	
tlen:	defw	0	
tst:	defw	0	
tblen:	defw	0	
			
laddr:	defw	0	
files:	defb	0	
curfil:	defb	0	
temfil:	defb	0	
select:	defb	0	
oldsp:	defw	0	
cfile:	defm	"*.*"	
	defb	255	
	defw	0,0,0,0,0	
store:	defs	15	
fsel:	defs	26	
	defw	0,0	
helpta:	
	defb	22,0,0
	defb	17,2,16,7
	defm	"     TAPER  V1.2 (01/04/93)     "
	defb	17,1
	defm	"    +3 DISC - TAPE CONVERTER    "
	defb	17,3
	defm	"CONTROLS (ON CATALOGUE SCREEN): "
	defb	17, 4
	defm	"CURSORS - MOVE HIGHLIGHTER      "
	defm	"SPACE   - SELECT FILE           "
	defm	"ENTER   - CONVERT FILES         "
	defm	"EXTEND  - RECATALOGUE DISC      "
	defb	17,3
	defm	"CONTROLS (NOW):                 "
	defb	17,4
	defm	"Q     - INCREASE PAUSE TIME     "
	defm	"A     - DECREASE PAUSE TIME     "
	defm	"SPACE - EXIT THIS HELP PAGE     "
	defm	17,5,16,0
	defm	"                                "
	defm	"http://github.com/suborb        "
	defm	"                                "
	defb	17, 7
	defm	"FIRST PUBLISHED IN OUTLET       "
	defb	17,2,16,7
	defm	"PAUSE TIME BETWEEN SAVES 0 SECS "
	defb	255

zz:
