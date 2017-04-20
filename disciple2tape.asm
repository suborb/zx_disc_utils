; DISCiPLE - TAPE CONVERTER:			
; V1.0:			
; SCREEN RESIDENT (org=20480):			
; 25 June 1993:			
	
	MODULE	disciple2tape
		
	defc	catto = 32768
	defc	catbuf = 32768 - 1024
			
			
	org	20480	
intro:	xor	a	
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
	call	box	
	defb	5,3,3,24,31	
	call	messag	
	defb	22,6,4	
	defm	"(P)LUS D or (D)ISCiPLE"	
	defb	255	
chekd:	call	654	
	ld	a,e	
	cp	22h	
	jr	z,plusd	
	cp	16h	
	jr	nz,chekd	
	ld	a,187	
	ld	hl,7126	
	jr	chekd1	
plusd:	ld	a,231	
	ld	hl,15318	
chekd1:	ld	(page1+1),a	
	ld	(page2+1),a	
	ld	(paddr+1),hl	
start:	ld	hl,fsel	
	ld	de,fsel+1	
	ld	bc,26	
	ld	(hl),b	
	ldir		
	xor	a	
	call	cls	
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
pokout:	jp	z,help	
	cp	32	
	jr	nz,pspa	
	xor	a	
	ld	hl,catbuf	
	ld	de,catbuf+1	
	ld	(hl),a	
	ld	bc,880	
	ldir		
	call	cat	
	and	a	
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
	xor	a	
	ld	(select),a	
	ld	(curfil),a	
	ld	(temfil),a	
	ld	(topofw),a	
	call	pcat	
menu:	ld	a,48	
	call	cls1	
	ld	hl,topofw	
	ld	a,(curfil)	
	sub	(hl)	
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
	jp	z,selone	
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
	ld	a,(topofw)	
	dec	a	
	cp	(hl)	
	jp	m,ntopow	
	dec	a	
	jp	m,menu	
	ld	(topofw),a	
	ex	af,af'	
	ld	a,(hl)	
	ld	(curfil),a	
	ex	af,af'	
	call	dscrol	
	ld	a,(topofw)	
	ld	(temfil),a	
	ld	hl,0	
	ld	(xypos),hl	
	call	pfilen	
	jp	menu	
ntopow:			
	ld	a,(hl)	
	ld	(curfil),a	
	ld	a,(topofw)	
	add	31	
	cp	(hl)	
	jp	p,menu	
	sub	31	
	inc	a	
	inc	a	
	ld	(topofw),a	
	call	uscrol	
	ld	a,(topofw)	
	add	30	
	ld	(temfil),a	
	ld	hl,15	
	ld	(xypos),hl	
	call	pfilen	
	jp	menu	
			
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
pcat2:	ld	a,(topofw)	
	add	32	
	ld	(pcat3+1),a	
	sub	32	
	ld	hl,0	
pcat1:	ld	(xypos),hl	
	ld	(temfil),a	
	push	af	
	push	hl	
	call	pfilen	
	pop	hl	
	pop	af	
	inc	l	
	add	2	
pcat3:	cp	33	
	jr	nz,pcat1	
	ret		
			
			
pfilen:	push	af	
	ld	a,(temfil)	
	and	254	
	ld	(temfil),a	
	call	messag	
	defb	22	
xypos:	defb	0,0,255	
	pop	af	
	inc	a	
	ld	de,11	
	ld	hl,catbuf-11	
pfile1:	add	hl,de	
	dec	a	
	jr	nz,pfile1	
	ld	b,2	
ptcf:	ld	a,(hl)	
	and	a	
	ret	z	
			
	push	bc	
	push	hl	
	ld	a,2	
	sub	b	
	ld	b,a	
	ld	a,(temfil)	
	add	a,b	
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
	call	messag	
	defb	32,32,255	
	pop	hl	
	push	hl	
	ld	l,(hl)	
	ld	h,0	
	dec	l	
	ld	bc,lettab	
	add	hl,bc	
	ld	a,(hl)	
	call	print	
	ld	a,32	
	call	print	
	pop	hl	
	inc	hl	
	pop	bc	
	djnz	ptcf	
	ret		
lettab:	defm	"BD$C  S"	
			
help:	ld	a,87	
	call	cls	
	ld	hl,helpta	
help1:	ld	a,(hl)	
	cp	255	
	jr	z,helplo	
	push	hl	
	call	print	
	pop	hl	
	inc	hl	
	jr	help1	
helplo:	call	654	
	ld	a,e	
	cp	32	
	jp	z,start	
	jr	helplo	
			
			
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
			
;Scroll Top 2/3 down:			
			
dscrol:	ld	a,64	
	ld	(count),a	
	ld	b,120	
dscro1:	push	bc	
	ld	a,(count)	
	ld	b,a	
	ld	c,0	
	ld	a,191	
	call	8876	
	push	hl	
	ld	a,(count)	
	add	8	
	ld	b,a	
	ld	c,0	
	ld	a,191	
	call	8876	
	pop	de	
	ld	bc,32	
	ldir		
	ld	a,(count)	
	inc	a	
	ld	(count),a	
	pop	bc	
	djnz	dscro1	
	call	messag	
	defb	22,0,0,23,31,32,255	
	ret		
			
;Scroll top 2/3 up:			
			
uscrol:	ld	a,191	
	ld	(count),a	
	ld	b,120	
uscro1:	push	bc	
	ld	a,(count)	
	ld	b,a	
	ld	c,0	
	ld	a,191	
	call	8876	
	push	hl	
	ld	a,(count)	
	sub	8	
	ld	b,a	
	ld	c,0	
	ld	a,191	
	call	8876	
	pop	de	
	ld	bc,32	
	ldir		
	ld	a,(count)	
	dec	a	
	ld	(count),a	
	pop	bc	
	djnz	uscro1	
	call	messag	
	defb	22,15,0,23,31,32,255	
	ret		
			
;Catalogues a DISCiPLE disc,:			
;Returns no of files in a:			
			
cat:	ld	hl,catto	
	ld	d,0	
cat1:	ld	e,1	
cat2:	push	de	
	push	hl	
	rst	8	
	defb	63	
	pop	de	
page1:	in	a,(187)	
paddr:	ld	hl,7126	
	ld	bc,512	
	ldir		
	ex	de,hl	
page2:	out	(187),a	
	pop	de	
	ld	a,11	
	inc	e	
	cp	e	
	jr	nz,cat2	
	ld	a,4	
	inc	d	
	cp	d	
	jr	nz,cat1	
	exx		
	ld	b,0	
	exx		
	ld	hl,catto	
	ld	de,catbuf	
	ld	a,80	
sort1:	ex	af,af'	
	push	hl	
	ld	a,(hl)	
	and	a	
	jr	z,nxtfil	
	cp	7	
	jr	z,isscr	
	cp	5	
	jr	nc,nxtfil	
isscr:	inc	hl	
	ld	bc,10	
	ldir		
	ld	(de),a	
	exx		
	inc	b	
	exx		
	inc	de	
nxtfil:	pop	hl	
	inc	h	
	ex	af,af'	
	dec	a	
	jr	nz,sort1	
	exx		
	ld	a,b	
	exx		
	ret		
			
;Moves filename from directory:			
;To store:			
			
fnex:	ld	de,store	
	ld	bc,10	
	ldir		
	ld	a,255	
	ld	(de),a	
	ld	a,(hl)	
	nop		
	ld	(dcopy),a	
	ret		
			
;After enter on menu:			
			
sloop:	ld	a,(select)	
	and	a	
	jp	z,start	
	ld	a,7	
	call	cls	
	call	box	
	defb	6,7,4,18,96	
	call	messag	
	defb	22,7,8	
	defm	"PRESS REC & PLAY"
	defb	22,8,8	
	defm	"....THEN ANY KEY"	
	defb	255	
lands3:	call	654	
	ld	a,e	
	inc	a	
	jr	z,lands3	
	ld	hl,0	
	ld	(pokout),hl	
	ld	(pokout+1),hl	
	ld	hl,fsel	
	ld	de,catsto	
	ld	b,a	
sloop1:	push	bc	
	push	hl	
	ld	a,(hl)	
	ld	bc,11	
	ld	hl,catbuf-11	
sloop2:	add	hl,bc	
	dec	a	
	jr	nz,sloop2	
	ld	bc,11	
	ldir		
	pop	hl	
	pop	bc	
	inc	hl	
	djnz	sloop1	
	ld	a,(select)	
	ld	b,a	
	xor	a	
sloop3:	push	bc	
	push	af	
	call	lands	
	pop	af	
	inc	a	
	pop	bc	
	djnz	sloop3	
	jp	start	
			
			
;Open a file header:			
;Load into memory:			
;Save to tape:			
;a=file no in catalogue:			
			
lands:	inc	a	
	ld	bc,11	
	ld	hl,catsto-11	
lands1:	add	hl,bc	
	dec	a	
	jr	nz,lands1	
	call	fnex	
	ld	hl,store	
	ld	de,dname	
	ld	bc,10	
	ldir		
	ld	(dcopy),a	
	ld	a,7	
	call	cls	
	call	box	
	defb	7,8,4,16,79	
	call	messag	
	defb	22,8,9	
	defm	"LOADING FILE:"	
	defb	22,9,9,255	
	call	fnprn	
	ld	ix,dhead	
	rst	8	
	defb	3Bh	
	jp	c,derror	
	ld	de,hd00	
	push	de	
	ld	b,9	
ghead:	rst	8	
	defb	3Ch	
	ld	(de),a	
	inc	de	
	djnz	ghead	
	pop	hl	
			
	ld	a,(hl)	
	ld	(thead),a	
	ld	de,tlen	
	inc	hl	
	ld	bc,6	
	ldir		
	and	a	
	jr	nz,basnot	
	ld	hl,(hd11)	
	ld	(tst),hl	
basnot:			
	ld	a,(thead)	
	cp	4	
	jr	c,fok	
	call	box	
	defb	10,8,3,17,87	
	call	messag	
	defb	22,11,9	
	defm	"NOT CONVERTIBLE"	
	defb	255	
	ld	b,100	
fnok1:	halt		
	djnz	fnok1	
	ret		
fok:	ld	de,24000	
	ld	bc,(hd0b)	
	rst	8	
	defb	3Dh	
	jp	c,derror	
	ld	hl,dname	
	ld	de,tname	
	ld	bc,10	
	ldir		
			
adout:	call	box	
	defb	9,9,4,14,48	
	call	messag	
	defb	22,10,10	
	defm	"SAVING FILE:"	
	defb	22,11,10,255	
	call	fnprn	
			
;Saving to Tape:			
	ld	hl,(23613)	
	ld	(oldsp),hl	
	ld	hl,serror	
	push	hl	
	ld	(23613),sp	
	ld	ix,thead	
	ld	de,17	
	xor	a	
	call	4C2h	
	ld	ix,24000	
	ld	de,(tlen)	
	ld	a,255	
	call	4C2h	
	pop	hl	
	ld	hl,(oldsp)	
	ld	(23613),hl	
	ret		
			
;Disc Error:			
;Jumps back to reCAT disc:			
			
derror:	call	box	
	defb	8,8,3,16,87	
	call	messag	
	defb	22,9,9	
	defm	"DISCiPLE ERROR"	
	defb	255	
derbak:	pop	af	
	pop	af	
	pop	af	
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
	jr	derbak	
			
;Draw attr box:			
;Data stored after call:			
			
box:	pop	ix	
	ld	d,(ix)	
	ld	a,(ix+2)	
box1:	ex	af,af'	
	ld	e,(ix+1)	
	ld	c,(ix+3)	
box2:	ld	a,d	
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
			
print:	rst	16	
	ret		
			
			
dhead:	defb	1	
	defb	0	
	defb	0	
	defb	'd'	
dcopy:	defb	0	
dname:	defb	32,32,32,32,32	
	defb	32,32,32,32,32	
hd00:	defb	0	
hd0b:	defw	0	
hd0d:	defw	0	
hdof:	defw	0	
hd11:	defw	0	
			
			
thead:	defb	0	
tname:	defb	32,32,32,32,32,32,32	
	defb	32,32,32	
tlen:	defw	0	
tst:	defw	0	
tblen:	defw	0	
			
files:	defb	0	
curfil:	defb	0	
temfil:	defb	0	
select:	defb	0	
topofw:	defb	0	
count:	defb	0	
oldsp:	defw	0	
store:	defs	11	
fsel:	defs	26	
	defw	0,0	
catsto:			
helpta:			
	defb	22,0,0
	defb	17,2,16,7
	defm	" D/DISCiPLE TAPER V1.1(14/07/93)"
	defb	17,1
	defm	"+D/DISCiPLE DISC->TAPE CONVERTER"
	defb	17,3
	defm	"CONTROLS (ON CATALOGUE SCREEN): "
	defb	17,4
	defm	"CURSORS - MOVE HIGHLIGHTER"
	defb	6
	defm	"SPACE   - SELECT FILE"
	defb	6
	defm	"ENTER   - CONVERT FILES"
	defb	6
	defm	"EXTEND  - RECATALOGUE DISC"
	defb	6
	defb	17,3
	defm	"CONTROLS (NOW):"
	defb	6,6,17,4
	defm	"SPACE - EXIT THIS HELP PAGE"
	defb	6,17,5,16,0
	defb	6,6
	defm	"http://www.github.com/suborb"
	defb	6
	defb	6,6
	defb	17,7
	defm	"FIRST PUBLISHED IN OUTLET"
	defb	6,17,2,16,7
	defm	"NB. AFTER PRESSING ENTER TO"
	defb	6
	defm	"CONVERT FILES THIS HELP PAGE"
	defb	6
	defm	"WILL BE DELETED."
	defb	6,255
			
