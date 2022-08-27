AS=z80asm

default: xios.bin

xios.bin: xios.asm
	$(AS) $<
