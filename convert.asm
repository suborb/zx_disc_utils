; Read +D/DISCiPLE DISCS ON +3:			
; V1.0:			
; SCREEN RESIDENT (org=20480):			
; 30 July 1993:			
; 31 July 1993:			
; 2 August 1993:			
; Convert 7:			

	MODULE	convert

	defc	catto = 32768
	defc	catbuf = 32768 - 1536
	defc	ldaddr = 24000
			
	org	20480	
			
intro:	xor	a	
	call	cls	
	call	8859	
	inc	hl	
	inc	de	
	ld	(hl),b	
	ld	c,255	
	ldir		
	ld	a,2	
	call	5633	
	call	box	
	defb	4,5,6,22,79	
	call	messag	
	defb	22,5,8	
	defm	"+3 CONVERT  V1.0"	
	defb	22,6,6	
	defm	"(C) 31.7.93 D MORRIS"	
	defb	22,7,6	
	defm	"USE:CURSORS,SP,ENTER"
	defb	22,8,6,20,1	
	defm	"OUTLET FREEWARE (PD)"
	defb	20,0,255	
pspa4:	call	654	
	inc	e	
	jr	z,pspa4	
setup:	ld	a,65	
	ld	iy,301	
	call	dodos	
	jr	nc,setup	
	xor	a	
	ld	iy,334	
	call	dodos	
	ld	a,10	
	ld	(xdpb+19),a	
	ld	a,8	
	ld	(23658),a	
	ld	hl,out	
	push	hl	
	ld	(start+1),sp	
start:	ld	sp,0	
	ld	a,201	
	ld	(dcheat),a	
	ld	b,0	
	ld	iy,268	
	call	dodos	
	ld	a,216	
	ld	(dcheat),a	
	ld	hl,fsel	
	ld	de,fsel+1	
	ld	bc,51	
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
	defm	"OR ENTER TO EXIT!!"	
	defb	255	
pspa:	call	654	
	ld	a,e	
	cp	21h	
	ret	z	
	cp	32	
	jr	nz,pspa	
	call	idisc	
	xor	a	
	ld	hl,catbuf	
	ld	de,catbuf+1	
	ld	(hl),a	
	ld	bc,1040	
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
	call	pak	
	jp	start	
			
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
menu4:	ld	a,(curfil)	
	ld	a,(23560)	
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
	ld	de,12	
	ld	hl,catbuf-12	
selon1:	add	hl,de	
	dec	a	
	jr	nz,selon1	
	dec	de	
	add	hl,de	
	ex	de,hl	
	ld	a,(select)	
	ld	c,a	
	ld	b,0	
	ld	hl,sectab-1	
	add	hl,bc	
	ld	a,(de)	
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
	ld	de,12	
	ld	hl,catbuf-12	
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
	rst	16	
mok:	ld	a,32	
	rst	16	
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
	rst	16	
	ld	a,32	
	rst	16	
	pop	hl	
	inc	hl	
	inc	hl	
	pop	bc	
	djnz	ptcf	
	ret		
lettab:	defm	"BD$C  S"	
			
;Print insert disc message:			
			
idisc:	call	box	
	defb	5,6,4,20,31	
	call	messag	
	defb	22,6,7	
	defm	"INSERT +"	
icheat:	defm	"D 3.5\"DISC"	
	defb	22,7,7	
	defm	"THEN PRESS ENTER!!"	
	defb	255	
idisc1:	call	654	
	ld	a,e	
	cp	21h	
	jr	nz,idisc1	
	ret		
			
;Press a key routine:			
			
pak:	ld	a,250	
	dec	a	
	jr	z,pakout	
	ld	(pak+1),a	
	halt		
	call	654	
	inc	e	
	jr	z,pak	
pakout:	ld	a,250	
	ld	(pak+1),a	
	ret		
			
;Out routine:			
			
out:	ld	a,7	
	ld	(23693),a	
	ld	(23695),a	
	xor	a	
	call	8859	
	jp	3435	
			
			
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
	ldir		
	ld	(23693),a	
	ld	(23695),a	
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
;on a +3 uses iy=355:			
;Returns no of files in a:			
			
cat:	ld	hl,catto	
	ld	d,0	
cat1:	ld	e,1	
cat2:	push	de	
	push	hl	
	call	ros	
			
	pop	hl	
	ld	bc,512	
	add	hl,bc	
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
	inc	de	
	ex	af,af'	
	push	af	
	ld	c,a	
	ld	a,81	
	sub	c	
	ld	(de),a	
	pop	af	
	ex	af,af'	
			
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
			
;Locate a file directory entry:			
;Entry :  b=file number (1=base):			
;Exit  :  d=track:			
;         e=sector (+D):			
;carry if in second half of sec:			
			
locate:	ld	c,0	
	ld	hl,0	
	ld	(disp),hl	
	ld	d,0	
	ld	e,2	
locat1:	ld	a,b	
	dec	a	
	and	a	
	jr	z,locout	
	ld	a,c	
	xor	1	
	ld	c,a	
	inc	e	
	ld	a,e	
	cp	22	
	jr	c,locat2	
	inc	d	
	ld	e,2	
locat2:	dec	b	
	jr	locat1	
locout:	srl	e	
	srl	c	
	ret	nc	
	ld	hl,256	
	ld	(disp),hl	
	ret		
			
;Moves filename from directory:			
;To store:			
			
fnex:	ld	de,store	
	ld	bc,10	
	ldir		
	ld	a,255	
	ld	(de),a	
	ld	a,(hl)	
	ret		
			
;Read a sector:			
;Entry:   d=track:			
;         e=+D sector ((+3)+1):			
;        hl=address to load to:			
			
ros:	bit	7,d	
	jr	z,ros1	
	res	7,d	
	ld	a,159	
	sub	d	
	ld	d,a	
ros1:	ld	bc,1	
	ld	ix,xdpb	
	ld	iy,355	
	dec	e	
	call	dodos	
	ret		
			
;+3 DOS call routine:			
			
dodos:	di		
	push	af	
	push	bc	
	ld	a,(23388)	
	or	7	
	res	4,a	
	ld	bc,32765	
	out	(c),a	
	ld	(23388),a	
	pop	bc	
	pop	af	
	ei		
	call	cjump	
	di		
	push	af	
	push	bc	
	ld	a,(23388)	
	and	248	
	set	4,a	
	ld	bc,32765	
	out	(c),a	
	ld	(23388),a	
	pop	bc	
	pop	af	
	ld	iy,23610	
	ei		
dcheat:	ret		
	ld	(errorn+1),a	
	call	box	
	defb	11,8,3,16,87	
	call	messag	
	defb	22,12,9	
	defm	"DISC ERROR #"	
	defb	255	
errorn:	ld	bc,0	
	call	11563	
	call	11747	
	call	pak	
	and	a	
	jp	start	
cjump:	jp	(iy)	
			
;After enter on menu:			
			
sloop:	ld	a,(select)	
	and	a	
	jp	z,start	
			
	ld	a,(select)	
	ld	b,a	
	xor	a	
sloop3:	push	bc	
	push	af	
	ld	l,a	
	ld	h,0	
	ld	bc,sectab	
	add	hl,bc	
	ld	b,(hl)	
	call	locate	
	call	lands	
	call	save	
	pop	af	
	inc	a	
	pop	bc	
	djnz	sloop3	
	jp	start	
			
;Save the file:			
			
			
save:	ld	hl,dname	
	ld	de,dname+1	
	ld	bc,11	
	ld	(hl),32	
	ldir		
	ld	a,46	
	ld	(point),a	
	ld	hl,02020h	
	ld	(dsuff),hl	
	ld	(dsuff+1),hl	
	ld	a,62	
	ld	(dend),a	
			
;Adjust filename here:			
	ld	hl,store	
	ld	de,dname	
	ld	c,10	
	ld	b,8	
	call	nsort1	
nsort5:	ld	de,dsuff	
	ld	b,3	
nsort2:	call	nsort1	
	ld	a,1	
	ld	(count),a	
	call	box	
	defb	7,8,4,16,48	
	call	messag	
	defb	22,8,9	
	defm	"ENTER FILENAME:"	
	defb	255	
	call	finput	
	ld	a,255	
	ld	(dend),a	
;Save file:			
dsave:	ld	hl,dname	
	ld	de,259	
	ld	bc,2	
	ld	iy,262	
	call	dodos	
	ld	iy,header	
	call	dodos	
	ld	bc,0	
	ld	de,(dlen)	
	ld	hl,ldaddr+9	
	ld	iy,277	
	call	dodos	
	ld	b,0	
	ld	iy,265	
	call	dodos	
	ret		
sback:	jp	save	
			
header:	ld	b,0	
	call	271	
	push	ix	
	pop	de	
	ld	hl,dhead	
	ld	bc,7	
	ldir		
	ret		
			
;Sort out filename:			
			
nsort1:	ld	a,(hl)	
	cp	127	
	jr	nc,nsort7	
	cp	32	
	jr	nc,nsort3	
	dec	c	
	ret	z	
nsort7:	ld	a,' '	
nsort3:	inc	hl	
	cp	'.'	
	ret	z	
	ld	(de),a	
	inc	de	
	djnz	nsort1	
	ret		
			
;File name input routine:			
			
finput:	xor	a	
	ld	(23560),a	
	ld	de,dcont	
	ld	bc,17	
	call	8252	
	ld	a,(count)	
	ld	l,a	
	ld	h,0	
	ld	de,22825	
	add	hl,de	
	ld	(hl),6	
finpu2:	ld	a,(23560)	
	and	a	
	jr	z,finpu2	
	cp	13	
	ret	z	
	cp	8	
	jr	z,left	
	cp	9	
	jr	z,righ	
	cp	12	
	jr	z,delete	
	cp	32	
	jr	c,finput	
	cp	46	
	jr	z,finput	
	cp	128	
	jr	nc,finput	
	ld	c,a	
	ld	a,(count)	
	ld	l,a	
	ld	h,0	
	ld	de,dname-1	
	add	hl,de	
	ld	(hl),c	
	jr	righ	
			
delete:	ld	a,(count)	
	ld	l,a	
	ld	h,0	
	ld	de,dname-1	
	add	hl,de	
	ld	(hl),32	
			
left:	ld	a,(count)	
	dec	a	
	and	a	
	jr	z,finput	
	ld	(count),a	
	cp	9	
	jr	nz,finput	
	dec	a	
	ld	(count),a	
	jr	finput	
			
righ:	ld	a,(count)	
	inc	a	
	cp	13	
	jr	z,finput	
	ld	(count),a	
	cp	9	
	jr	nz,finput	
	inc	a	
	ld	(count),a	
	jp	finput	
			
			
;Open a file header:			
;Load into memory:			
;Save to +3 disc:			
;a=file no in +3 catalogue:			
			
lands:	ld	hl,32768	
	push	hl	
	call	ros	
	pop	hl	
	ld	bc,(disp)	
	add	hl,bc	
	push	hl	
	inc	hl	
	call	fnex	
	ld	hl,store	
	ld	de,dname	
	ld	bc,10	
	ldir		
	ld	a,7	
	call	cls	
	call	box	
	defb	7,8,4,16,79	
	call	messag	
	defb	22,8,9	
	defm	"LOADING FILE:"	
	defb	22,9,9,255	
	call	fnprn	
	pop	ix	
	ld	l,(ix+12)	
	ld	h,(ix+11)	
	ld	d,(ix+13)	
	ld	e,(ix+14)	
	push	hl	
	push	de	
			
;Set up disc header:			
	ld	bc,200	
	add	ix,bc	
	ld	a,(ix+11)	
	ld	(dhead),a	
	ld	l,(ix+12)	
	ld	h,(ix+13)	
	ld	(dlen),hl	
	ld	e,(ix+14)	
	ld	d,(ix+15)	
	ld	hl,65535	
	and	a	
	jr	nz,nobas	
	ld	e,(ix+18)	
	ld	d,(ix+19)	
	ld	l,(ix+16)	
	ld	h,(ix+17)	
nobas:	ld	(dstart),de	
	ld	(doffse),hl	
			
	ld	hl,ldaddr	
	pop	de	
	pop	bc	
load1:	push	bc	
	push	hl	
	call	ros	
	pop	hl	
	ld	bc,510	
	add	hl,bc	
	ld	d,(hl)	
	inc	hl	
	ld	e,(hl)	
	dec	hl	
	pop	bc	
	ld	a,d	
	or	e	
	ret	z	
	dec	bc	
	ld	a,b	
	or	c	
	jr	nz,load1	
	ret		
			
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
	ld	(23693),a	
	ld	(23695),a	
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
	rst	16	
	pop	hl	
	inc	hl	
	jr	fnprn1	
			
messag:	pop	hl	
messa1:	ld	a,(hl)	
	cp	255	
	jr	z,mout	
	push	hl	
	rst	16	
	pop	hl	
	inc	hl	
	jr	messa1	
mout:	inc	hl	
	jp	(hl)	
			
dcont:	defb	22,9,9	
	defb	'<'	
dname:	defm	"        "	
point:	defb	46	
dsuff:	defm	"   "	
dend:	defb	255	
			
dhead:	defb	1	
dlen:	defw	0	
dstart:	defw	0	
doffse:	defw	0	
			
disp:	defw	0	
			
files:	defb	0	
curfil:	defb	0	
temfil:	defb	0	
select:	defb	0	
topofw:	defb	0	
count:	defb	0	
store:	defs	11,0	
			
			
xdpb:	defb	36,0,4,15,0,100,1	
	defb	127,0,192,0,32,0,1,0	
	defb	2,3,130,80,10,1,0,2	
	defb	42,82,96,0	
			
	defw	0,0	
fsel:	defs	26,0	
sectab:	defs	26,0	
zz:			
			
