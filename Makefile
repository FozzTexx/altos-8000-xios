AS=./asl-err-wrap -cpu z80
%.p: %.asm
	$(AS) -L $(basename $<).lst $<
%.spr: %.asm
	$(AS) -L $(basename $<).lst --spr $<
%.hex: %.p
	p2hex -F Intel -M 2 $<

default: ldrbios.hex xios.spr

xios.spr: xios.asm

ldrbios.p: ldrbios.asm \
	   ldrb224a.asm ldrb224b.asm ldrb224c.asm ldrb224d.asm ldrb224e.asm ldrb224h.asm

ldrbios.hex: ldrbios.p
