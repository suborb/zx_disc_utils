

TARGETS = plusthree2tape.tap convert.tap plusthree2disciple.tap plusdtoplus3.tap disciple2tape.tap convert.tap
 
all: $(TARGETS)

plusthree2tape.tap: plusthree2tape.bin
	appmake +zx -b $^ --org  20480 --clearaddr=23999 -o $@

convert.tap: convert.bin
	appmake +zx -b $^ --org  20480 --clearaddr=23999 -o $@

plusthree2disciple.tap: plusthree2disciple.bin
	appmake +zx -b $^ --org  20480 --clearaddr=23999 -o $@

plusdtoplus3.tap: plusdtoplus3.bin
	appmake +zx -b $^ --org  18432 --clearaddr=23999 -o $@

disciple2tape.tap: disciple2tape.bin
	appmake +zx -b $^ --org  20480 --clearaddr=23999 -o $@

convert.tap: convert.bin
	appmake +zx -b $^ --org  20480 --clearaddr=23999 -o $@

%.bin:	%.asm
	z80asm -b -o$@ $^


clean:
	rm -f *.bin $(TARGETS) *.o *.err
	
