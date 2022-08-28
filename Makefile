AS=./asl-err-wrap -cpu z80

default: xios.bin

xios.bin: xios.asm
	$(AS) -L $(basename $<).lst $<
