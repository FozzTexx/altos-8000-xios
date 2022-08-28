AS=./asl-err-wrap -cpu z80
%.p: %.asm
	$(AS) -L $(basename $<).lst $<
%.hex: %.p
	p2hex $<

default: ldrbios.hex xios.spr

xios.p: xios.asm

xios.spr: xios.p

ldrbios.p: ldrbios.asm

ldrbios.hex: ldrbios.p
