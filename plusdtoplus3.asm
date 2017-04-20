;+D/+3 file converter:			
;12/8/94:			
;Written for BOGIE of E3:			
;13/8/94:			
;Added converters for filenames:			
;and BASIC syntax:			

	MODULE	discconv
			

	defc	font = 49152

;Table for occupied dir posn:			

	defc	catto = 32768
	defc	tembuf = 32768 - 1024
	defc	catbuf = 49152 + 768
	defc	ldaddr = 24000

	org	18432
			
intro:	xor	a	
	call	cls	
	call	8859	
	inc	hl	
	inc	de	
	ld	(hl),b	
	ld	bc,511	
	ldir		
	ld	a,2	
	call	5633	
	ld	a,8	
	ld	(23658),a	
			
setup:	ld	a,23	
	call	pager	
;Set up the font in page 7:			
	ld	hl,15616	
	ld	de,font-256	
	ld	(23606),de	
	inc	d	
	ld	bc,768	
setup1:	ld	a,(hl)	
	rrca		
	or	(hl)	
	ld	(de),a	
	inc	hl	
	inc	de	
	dec	bc	
	ld	a,b	
	or	c	
	jr	nz,setup1	
	xor	a	
	call	cls	
	call	box	
	defb	0,4,6,25,120	
	call	messag	
	defb	22,1,5	
	defm	"DISCCONV v1.0 (18.8.94)"
	defb	22,2,6	
	defm	"(C) 1994 D.J.Morris"
	defb	22,3,5,16,2	
	defm	"WRITTEN FOR BOGIE OF E3"
	defb	22,4,10,16,1	
	defm	"PRESS ANY KEY"	
	defb	255	
	call	gkey	
	ld	b,10	
setup2:	halt		
	djnz	setup2	
	ld	hl,65535	
	call	pag16	
	ld	(hl),255	
	call	pag23	
	ld	(hl),0	
	call	pag16	
	ld	a,(hl)	
	and	a	
	jr	nz,setupo	
	call	box	
	defb	2,8,5,21,23	
	call	messag	
	defb	22,3,9	
	defm	"IN 48K LOCKED MODE!"	
	defb	22,4,8	
	defm	"- +3 DOS UNUSABLE.."	
	defb	22,5,9	
	defm	"PRESS KEY FOR BASIC"
	defb	255	
	call	gkey	
	jp	out	
			
;Choose between +D/DISCiPLE:			
setupo:	call	pag23	
	xor	a	
	call	cls	
	call	box	
	defb	2,4,3,24,31	
	call	messag	
	defb	22,3,5	
	defm	"(P)LUS D or (D)ISCiPLE"
	defb	255	
chekd:	call	gkey	
	cp	'P'	
	jr	z,plusd	
	cp	'D'	
	jr	nz,chekd	
	ld	a,187	
	ld	hl,7126	
	jr	chekd1	
plusd:	ld	a,231	
	ld	hl,15318	
chekd1:	ld	(page1+1),a	
	ld	(page2+1),a	
	ld	(paddr+1),hl	
			
;Now ask whether to reinit DOS:			
	xor	a	
	call	cls	
	call	box	
	defb	2,4,3,26,31	
	call	messag	
	defb	22,3,5	
	defm	"REINITALISE +3 DOS?(Y/N)"	
	defb	255	
setup3:	call	gkey	
	cp	'N'	
	jr	z,start0	
	cp	'Y'	
	jr	nz,setup3	
	xor	a	
	call	pager	
	di		
	ld	bc,8189	
	ld	a,(23399)	
	push	af	
	push	bc	
	res	4,a	
	out	(c),a	
	ld	hl,189	
	ld	de,23296	
	ld	bc,82	
	ldir		
	pop	bc	
	pop	af	
	out	(c),a	
	ei		
	ld	iy,256	
	call	dodos	
start0:	xor	a	
	ld	iy,334	
	call	dodos	
	ld	a,255	
	ld	iy,301	
	call	dodos	
	ld	(drive),a	
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
	xor	a	
	ld	hl,fsel	
	ld	b,26	
start1:	ld	(hl),a	
	inc	hl	
	djnz	start1	
	call	cls	
start2:	call	box	
	defb	1,8,6,20,32	
start3:	call	messag	
	defb	22,2,9	
	defm	"1. CONVERT < +3"	
	defb	22,3,9	
	defm	"2. CONVERT > +3"	
	defb	22,4,9	
	defm	"3. CHANGE DRIVE("	
	defb	255	
	ld	a,(drive)	
	rst	16	
	call	messag	
	defb	')'	
	defb	22,5,9	
	defm	"4. EXIT TO BASIC"	
	defb	255	
pspa:	call	gkey	
	cp	'1'	
	jr	z,p3ent	
	cp	'2'	
	jr	z,pdent	
	cp	'3'	
	jr	nz,pspa1	
	ld	a,(drive)	
	inc	a	
	cp	'C'	
	jr	nz,pspa0	
	ld	a,'A'	
pspa0:	ld	(drive),a	
	ld	iy,301	
	call	dodos	
	jp	start3	
pspa1:	cp	'4'	
	ret	z	
	jr	pspa	
			
p3ent:	ld	hl,p3tab	
	jr	pdent1	
pdent:	ld	hl,pdtab	
pdent1:	ld	(depsto),hl	
			
cat:	call	idisc	
	ld	b,0 ;catalogue	
	call	gtrout	
;a=number of files:			
	and	a	
	jr	nz,isfil	
	xor	a	
	call	cls	
	call	box	
	defb	2,8,3,16,79	
	call	messag	
	defb	22,3,9	
	defm	"NO FILES FOUND"
	defb	255	
	call	pak	
	jp	start	
			
isfil:	dec	a	
	ld	(files),a	
isfil1:	xor	a	
	ld	(select),a	
	ld	(curfil),a	
	ld	(temfil),a	
	ld	(topofw),a	
	call	cls	
	call	pcat	
			
;Menu routine:			
			
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
			
menu3:	call	gkey	
	cp	13	
	jp	z,conver	
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
	add	15	
	cp	(hl)	
	jp	p,menu	
	sub	15	
	inc	a	
	inc	a	
	ld	(topofw),a	
	call	uscrol	
	ld	a,(topofw)	
	add	14	
	ld	(temfil),a	
	ld	hl,7	
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
	add	15	
	ld	(pcat3+1),a	
	sub	15	
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
pcat3:	cp	15	
	jr	c,pcat1	
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
	ld	b,1	
	call	gtrout	
	ret		
			
;Scroll Top 2/3 down:			
			
dscrol:	ld	a,128	
	ld	(count),a	
	ld	b,56	
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
	defb	22,0,0,6,6,255	
	ret		
			
;Scroll top 2/3 up:			
			
uscrol:	ld	a,191	
	ld	(count),a	
	ld	b,56	
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
	defb	22,7,0,6,6,255	
	ret		
			
;Print insert disc message:			
			
idisc:	xor	a	
	call	cls	
	call	box	
	defb	2,6,4,21,31	
	call	messag	
	defb	22,3,7	
	defm	"ENSURE DISCS ARE IN"
	defb	22,4,7	
	defm	"DRIVES, PRESS ENTER"	
	defb	255	
idisc1:	call	gkey	
	cp	13	
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
			
;Get a key routine:			
			
gkey:	xor	a	
	ld	(23560),a	
gkey1:	ld	a,(23560)	
	and	a	
	jr	z,gkey1	
	ret		
			
;Out routine:			
			
out:	ld	hl,10072	
	exx		
	di		
	ld	a,16	
	ld	(23388),a	
	ld	bc,32765	
	out	(c),a	
	ei		
	ld	a,7	
	ld	(23693),a	
	ld	(23695),a	
	ld	hl,15360	
	ld	(23606),hl	
	xor	a	
	call	8859	
	jp	3435	
			
;Print a number:			
			
numpr:	call	11563	
	call	11747	
	ret		
			
			
;Top 2/3 Screen Cls:			
;a=attribute byte:			
			
cls:	ld	hl,16384	
	ld	de,16385	
	ld	bc,2047	
	ld	(hl),l	
	ldir		
cls1:	ld	hl,22528	
	ld	de,22529	
	ld	bc,255	
	ld	(hl),a	
	ldir		
	ld	(23693),a	
	ld	(23695),a	
	ret		
			
;After enter on menu:			
			
conver:	ld	a,(select)	
	and	a	
	jp	z,start	
	ld	b,a	
	ld	hl,fsel	
sloop1:	push	bc	
	push	hl	
	ld	a,(hl)	
	ld	b,2	
	call	gtrout	
	ld	b,3	
	call	c,gtrout	
	pop	hl	
	pop	bc	
	inc	hl	
	djnz	sloop1	
	jp	start	
			
			
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
			
;Find if a file is marked for:			
;conversion, if so print:			
;the letter:			
			
fifma:	ld	a,2	
	sub	b	
	ld	b,a	
	ld	a,(temfil)	
	add	a,b	
	ld	b,a	
	inc	b	
	ld	c,1	
	ld	ix,fsel	
fifma1:	ld	a,(ix)	
	and	a	
	jr	z,fifma4	
	cp	b	
	jr	nz,fifma3	
	ld	a,c	
	add	64	
	ld	(fifma2),a	
	ld	a,b	
	push	af	
	call	messag	
	defb	20,1	
fifma2:	defb	0	
	defb	20,0,255	
	pop	af	
	ret		
fifma3:	inc	ix	
	inc	c	
	jr	fifma1	
fifma4:	ld	a,32	
	rst	16	
	ret		
			
;Paging routine:			
			
pag16:	ld	a,16	
	jr	pager	
pag23:	ld	a,23	
pager:	di		
	push	bc	
	ld	bc,32765	
	out	(c),a	
	ld	(23388),a	
	pop	bc	
	ei		
	ret		
			
			
;+3 DOS call routine:			
			
dodos:	di		
	push	af	
	push	bc	
	ld	a,7	
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
	ld	bc,32765	
	ld	a,23	
	out	(c),a	
	ld	(23388),a	
	pop	bc	
	pop	af	
	ld	iy,23610	
	ei		
dcheat:	ret		
error:	ld	(errorn+1),a	
	xor	a	
	call	cls	
	call	box	
	defb	3,8,3,15,87	
	call	messag	
	defb	22,4,9	
erro2:	defm	"+3 ERROR #"	
	defb	255	
errorn:	ld	bc,0	
	call	numpr	
	ld	a,'3'	
	ld	(erro2+1),a	
error1:	call	pak	
	and	a	
	jp	start	
cjump:	jp	(iy)	
			
;+D disc error:			
			
pderro:	ld	de,(oldsp)	
	ld	(23613),de	
	ex	af,af'	
	ld	a,'D'	
	ld	(erro2+1),a	
	ex	af,af'	
	jp	error	
			
messag:	pop	hl	
messa1:	ld	a,(hl)	
	cp	255	
	jr	z,mout	
	rst	16	
	inc	hl	
	jr	messa1	
mout:	inc	hl	
	jp	(hl)	
			
string:	ld	a,b	
	or	c	
	ret	z	
	ld	a,(de)	
	rst	16	
	inc	de	
	dec	bc	
	jr	string	
			
;Switch on/off error trapping:			
			
pderon:	pop	hl	
	ld	de,(23613)	
	ld	(oldsp),de	
	ld	de,pderro	
	push	de	
	ld	(23613),sp	
	call	pag16	
	jp	(hl)	
			
pderof:	pop	hl	
	pop	de	
	ld	de,(oldsp)	
	ld	(23613),de	
	call	pag23	
	jp	(hl)	
			
;Routines to do with loading:			
;and saving:			
;Dependent routines for +3/+D:			
			
;Call a dependent routine,:			
;Entry: a=routine number:			
;Exit: as per routine:			
			
			
gtrout:	ld	l,b	
	ld	h,0	
	add	hl,hl	
	ld	de,(depsto)	
	add	hl,de	
	ld	e,(hl)	
	inc	hl	
	ld	d,(hl)	
	ex	de,hl	
	call	cjump+1	
	ret		
			
;Table for +3 values:			
			
p3tab:	defw	p3cat	
	defw	p3name	
	defw	p3load	
	defw	pdsave	
			
pdtab:	defw	pdcat	
	defw	pdname	
	defw	pdload	
	defw	p3save	
			
			
;Catalogue a +3 disc:			
			
p3cat:	ld	hl,catbuf	
	ld	de,catbuf+1	
	ld	(hl),0	
	ld	bc,880	
	ldir		
	ld	hl,cfilen	
	ld	de,catbuf	
	ld	b,64	
	ld	c,1	
	ld	iy,286	
	call	dodos	
	ld	a,b	
	dec	a	
	ret		
			
;Print a +3 filename in cat scrn:			
;Entry: a=catalogue number:			
			
p3name:	ld	de,13	
	ld	hl,catbuf	
p3nam1:	add	hl,de	
	dec	a	
	jr	nz,p3nam1	
	ld	b,2	
p3nam2:	ld	a,(hl)	
	and	a	
	ret	z	
	push	bc	
	push	hl	
	call	fifma	
	ld	a,32	
	rst	16	
	pop	hl	
	call	fnex	
	push	hl	
	call	fnprn	
	pop	hl	
	ld	a,9	
	cp	(hl)	
	ld	a,32	
	call	nc,16	
	ld	c,(hl)	
	inc	hl	
	ld	b,(hl)	
	ld	a,c	
	cp	99	
	jr	c,p3nam3	
	ld	bc,99	
p3nam3:	push	hl	
	push	af	
	call	numpr	
	pop	af	
	pop	hl	
	inc	hl	
	pop	bc	
	djnz	p3nam2	
	ret		
			
;Routine to load a plus 3 file:			
;Entry: a=file no in catalogue:			
			
p3load:	inc	a	
	ld	bc,13	
	ld	hl,catbuf-13	
p3ld1:	add	hl,bc	
	dec	a	
	jr	nz,p3ld1	
	call	fnex	
	call	datal	
	ld	bc,1	
	ld	de,1	
	ld	hl,filen	
	ld	iy,262	
	call	dodos	
	ld	b,0	
	ld	iy,271	
	call	dodos	
;Copy the file header:			
	push	ix	
	pop	hl	
	ld	de,ftype	
	ld	bc,7	
	ldir		
	ld	hl,65535	
	ld	(astart),hl	
screeh:	ld	a,(ftype)	
	and	a	
	jr	nz,nobas	
	ld	hl,(fstart)	
	ld	(astart),hl	
nobas:	call	chekty	
	ret	nc	
	ld	bc,0	
	ld	de,(flenth)	
	ld	hl,ldaddr	
	ld	iy,274	
	call	dodos	
	ld	b,0	
	ld	iy,265	
	call	dodos	
	scf		
	ret		
			
;Save a file to +3 disc:			
			
p3save:	call	p3conv	
	call	data	
	ld	hl,filen	
	ld	de,259	
	ld	bc,2	
	ld	iy,262	
	call	dodos	
	ld	iy,header	
	call	dodos	
	ld	bc,0	
	ld	de,(flenth)	
	ld	hl,ldaddr	
	ld	iy,277	
	call	dodos	
	ld	b,0	
	ld	iy,265	
	call	dodos	
	ret		
			
header:	ld	b,0	
	call	271	
	push	ix	
	pop	de	
	ld	hl,ftype	
	ld	bc,7	
	ldir		
	ret		
			
p3conv:	ld	hl,iname	
	ld	de,iname+1	
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
	ld	hl,filen	
	ld	de,iname	
	ld	c,10	
	ld	b,8	
	call	nsort1	
nsort5:	ld	de,dsuff	
	ld	b,3	
nsort2:	call	nsort1	
	ld	a,1	
	ld	(count),a	
	call	box	
	defb	2,8,4,16,48	
	call	messag	
	defb	22,3,9	
	defm	"ENTER FILENAME:"	
	defb	255	
	call	finput	
	ld	a,255	
	ld	(dend),a	
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
	ld	de,22665    ;22825	
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
	ld	de,iname-1	
	add	hl,de	
	ld	(hl),c	
	jr	righ	
			
delete:	ld	a,(count)	
	ld	l,a	
	ld	h,0	
	ld	de,iname-1	
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
			
;+D/DISCiPLE ROUTINES...:			
			
;Catalogue a +D/DISCiPLE disc:			
;Exit: a=no of files:			
			
pdcat:	call	pderon	
	ld	hl,tembuf	
	ld	de,tembuf+1	
	ld	bc,20480+1024	
	ld	(hl),0	
	ldir		
	ld	hl,catto	
	ld	d,0	
pdcat1:	ld	e,1	
pdcat2:	push	de	
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
	jr	nz,pdcat2	
	ld	a,4	
	inc	d	
	cp	d	
	jr	nz,pdcat1	
;Okay, have now read the 1st:			
;4 tracks in now sort it..:			
	defw	0,0,0,0	
	exx		
	ld	b,0	
	exx		
	ld	hl,catto	
	ld	de,tembuf	
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
	call	pderof	
	exx		
	ld	a,b	
	exx		
	ld	hl,tembuf	
	ld	de,catbuf	
	ld	bc,1024	
	ldir		
	ret		
			
;Print a +D name (in catalogue):			
			
pdname:	ld	de,11	
	ld	hl,catbuf-11	
pdnam1:	add	hl,de	
	dec	a	
	jr	nz,pdnam1	
	ld	b,2	
pdnam2:	ld	a,(hl)	
	and	a	
	ret	z	
	push	bc	
	push	hl	
	call	fifma	
	ld	a,32	
	rst	16	
	pop	hl	
	call	pdfnex	
	push	hl	
	call	fnprn	
	ld	a,32	
	rst	16	
	ld	a,32	
	rst	16	
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
	pop	bc	
	djnz	pdnam2	
	ret		
lettab:	defm	"BD$C  S"	
			
;Save a file to +D disc:			
;-based on info in store:			
			
pdsave:	call	pdconv	
	call	data	
	call	pderon	
	ld	ix,dhead	
	rst	8	
	defb	35h	
	ld	de,ldaddr	
	ld	bc,(flenth)	
	rst	8	
	defb	37h	
	rst	8	
	defb	38h	
	call	pderof	
	ret		
			
			
;Load a +D/DISCiPLE error:			
;Entry: a=catalogue no.:			
			
pdload:	ld	bc,11	
	ld	hl,catbuf-11	
pdloa1:	add	hl,bc	
	dec	a	
	jr	nz,pdloa1	
	call	pdfnex	
	ld	hl,filen	
	ld	de,dname	
	ld	bc,10	
	ldir		
	ld	(ddesc),a	
	ld	a,7	
	call	cls	
	call	datal	
			
;Read in the header:			
	call	pderon	
	ld	ix,dhead	
	rst	8	
	defb	3Bh	
	ld	de,ftype	
	ld	b,9	
ghead:	rst	8	
	defb	3Ch	
	ld	(de),a	
	inc	de	
	djnz	ghead	
	call	pderof	
	ld	a,(ftype)	
	and	a	
	jr	nz,basnot	
	ld	hl,(astart)	
	ld	(fstart),hl	
basnot:	call	chekty	
	ret	nc	
	call	pderon	
	ld	de,ldaddr	
	ld	bc,(flenth)	
	rst	8	
	defb	3Dh	
	call	pderof	
	scf		
	ret		
			
chekty:	ld	a,(ftype)	
	cp	4	
	ret	c	
	call	box	
	defb	3,8,3,17,87	
	call	messag	
	defb	22,4,9	
	defm	"NOT CONVERTIBLE"	
	defb	255	
	call	pak	
	and	a	
	ret		
			
;Adjust filename to +D standard:			
;Convert the BASIC too..:			
			
pdconv:	ld	hl,filen	
	ld	de,dname	
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
adout:	ld	hl,dname	
;Check to remove trailing '.':			
	ld	b,10	
adout1:	ld	a,(hl)	
	cp	'.'	
	jp	z,adout3	
	inc	hl	
	djnz	adout1	
			
adout2:	ld	a,(ftype)	
	inc	a	
	ld	(ddesc),a	
	cp	1	
	jp	nz,adout4	
;Basic changer:			
	call	box	
	defb	3,8,4,15,87	
	call	messag	
	defb	22,4,9	
	defm	"CONVERT DRIVE"	
	defb	22,5,9	
	defm	"SYNTAX? (Y/N)"	
	defb	255	
chekb:	call	gkey	
	and	223	
	cp	'N'	
	jr	z,adout2	
	cp	'Y'	
	jr	nz,chekb	
	ld	ix,ldaddr	
	ld	de,(progl)	
			
chekb2:	ld	c,(ix+2)	
	ld	b,(ix+3)	
	inc	ix	
	inc	ix	
	inc	ix	
	inc	ix	
	dec	de	
	dec	de	
	dec	de	
	dec	de	
	call	search	
	inc	ix	
	dec	de	
	ld	a,d	
	or	e	
	jr	nz,chekb2	
adout4:	ld	hl,dname	
	ld	de,filen	
	ld	bc,10	
	ldir		
	ld	a,255	
	ld	(de),a	
	scf		
	ret		
			
adout3:	inc	hl	
	ld	a,(hl)	
	cp	32	
	jp	nz,adout2	
	dec	hl	
	ld	(hl),32	
	jp	adout2	
			
;Search routine for BASIC:			
;conversion:			
			
search:	ld	a,(ix)	
	cp	13	
	ret	z	
	cp	234	
	ret	z	
	cp	239	
	jr	z,found	
	cp	248	
	jr	z,found	
	cp	213	
	jr	z,found	
	cp	214	
	jr	z,found	
	inc	ix	
searc1:	dec	de	
	dec	bc	
	ld	a,b	
	or	c	
	jr	nz,search	
	ret		
			
found:	inc	ix	
	ld	a,(ix)	
	cp	32	
	jr	nz,searc1	
	dec	de	
	dec	bc	
	inc	ix	
	ld	a,(ix)	
	cp	32	
	jr	nz,searc1	
	dec	de	
	dec	bc	
			
	inc	ix	
	dec	de	
	dec	bc	
	ld	a,(ix)	
	and	223	
	cp	'A'	
	jr	c,found1	
	cp	'Z'	
	jr	c,searc1	
;At this point all is ok!:			
found1:	and	a	
	jr	nz,found2	
	ld	(ix),';'	
found2:	ld	(ix-1),'*'	
	ld	(ix-2),'d'	
	inc	ix	
	jr	searc1	
			
;Convert catalogue name to:			
;print format in store:			
;Entry: hl=cat entry addr:			
			
fnex:	ld	de,filen	
	ld	b,8	
	call	fnex1	
	ld	a,46	
	ld	(de),a	
	inc	de	
	ld	b,3	
	call	fnex1	
	ld	a,255	
	ld	(de),a	
	ret		
			
fnex1:	ld	a,(hl)	
	and	127	
	ld	(de),a	
	inc	de	
	inc	hl	
	djnz	fnex1	
	ret		
			
;Plus D next file name:			
;hl=addy in catalogue:			
			
pdfnex:	ld	de,filen	
	ld	bc,10	
	ldir		
	ld	a,255	
	ld	(de),a	
	ld	a,(hl)	
	ret		
			
;Message Printer:			
;Text Stored after CALL:			
			
fnprn:	ld	hl,filen	
fnprn1:	ld	a,(hl)	
	bit	7,a	
	jr	z,fnprn2	
	cp	255	
	ret	z	
	push	hl	
	call	messag	
	defb	20,1,255	
	pop	hl	
	ld	a,(hl)	
	rst	16	
	push	hl	
	call	messag	
	defb	20,0,255	
	pop	hl	
	inc	hl	
	jr	fnprn1	
fnprn2:	rst	16	
	inc	hl	
	jr	fnprn1	
			
;Print data boxs:			
			
datal:	ld	a,7	
	call	cls	
	call	box	
	defb	2,8,4,15,79	
	call	messag	
	defb	22,3,9	
	defm	"LOADING FILE:"	
	defb	22,4,9,255	
	call	fnprn	
	ret		
			
data:	xor	a	
	call	cls	
	call	box	
	defb	0,9,7,14,48	
	call	messag	
	defb	22,1,10	
	defm	"SAVING FILE:"	
	defb	22,2,10,16,1,255	
	call	fnprn	
	call	messag	
	defb	22,3,10,16,0	
	defb	255	
	ld	a,(ftype)	
	ld	l,a	
	ld	h,0	
	add	hl,hl	
	add	hl,hl	
	add	hl,hl	
	ld	de,filet1	
	add	hl,de	
	call	fnprn1	
	ld	a,(ftype)	
	and	a	
	jr	nz,intyp1	
	call	messag	
	defb	8,'(',255	
	ld	bc,(astart)	
	call	numpr	
	call	messag	
	defb	')',22,4,10	
	defm	"VAR: "	
	defb	255	
	ld	hl,(flenth)	
	ld	de,(progl)	
	and	a	
	sbc	hl,de	
	ld	b,h	
	ld	c,l	
	call	numpr	
			
intyp3:	call	messag	
	defb	22,5,10	
	defm	"LENGTH:"	
	defb	255	
	ld	bc,(flenth)	
	call	numpr	
	ret		
intyp1:	cp	3	
	jr	z,intyp2	
	call	messag	
	defb	22,4,10	
	defm	"CHAR:"	
	defb	255	
	ld	a,(fstart+1)	
	and	31	
	or	64	
	rst	16	
	jr	intyp3	
intyp2:	call	messag	
	defb	22,4,10	
	defm	"START:"	
	defb	255	
	ld	bc,(fstart)	
	call	numpr	
	jr	intyp3	
			
			
;Data information:			
			
filet1:	defm	"BASIC  "	
	defb	255	
filet2:	defm	"#.ARRAY"	
	defb	255	
filet3:	defm	"$.ARRAY"	
	defb	255	
filet4:	defm	"CODE   "	
	defb	255	
			
;Edit filename print thingy..:			
			
dcont:	defb	22,4,9	
	defb	'<'	
iname:	defm	"        "	
point:	defb	46	
dsuff:	defm	"   "	
dend:	defb	255	
			
;Catalogue wild card:			
			
cfilen:	defm	"*.*"	
	defb	255	
			
filen:	defs	15,0	
			
;DISCiPLE Header:			
			
dhead:	defb	1	
	defb	0	
	defb	0	
	defb	'd'	
ddesc:	defb	0	
dname:	defb	32,32,32,32,32,32,32	
	defb	32,32,32	
ftype:	defb	0	
flenth:	defw	0	
fstart:	defw	0	
progl:	defw	0	
astart:	defw	65535	
			
;Address of dependent rout table:			
depsto:	defw	0	
			
oldsp:	defw	0	
			
drive:	defb	0	
			
			
files:	defb	0	
curfil:	defb	0	
temfil:	defb	0	
select:	defb	0	
topofw:	defb	0	
count:	defb	0	
fsel:	defs	28,0	
