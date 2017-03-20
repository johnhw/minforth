#
# Makefile for minforth -- Raspberry Pi JonesFORTH Operating System
#
# remove the prefix if compiling on an RPI
PREFIX= arm-none-eabi-
AS=	$(PREFIX)as
CC=	$(PREFIX)gcc -Wall -O2 -nostdlib -nostartfiles -ffreestanding -Os
LD=	$(PREFIX)ld

KOBJS=	jonesforth.o 

all: kernel.img


jonesforth.o: jonesforth.s
	$(AS) jonesforth.s -o jonesforth.o

kernel.img: loadmap $(KOBJS)
	$(PREFIX)objcopy -I binary -O elf32-littlearm -B arm --rename-section .data=.rodata,alloc,load,readonly,data,contents jonesforth.f jonesforthf.o
	$(LD) $(KOBJS) jonesforthf.o  -T loadmap -o minforth.elf -Map minforth.map
	$(PREFIX)objdump -D minforth.elf > minforth.list
	$(PREFIX)objcopy minforth.elf -O ihex minforth.hex
	$(PREFIX)objcopy --only-keep-debug minforth.elf kernel.sym
	$(PREFIX)objcopy minforth.elf -O binary kernel.img
	hexdump -v -e '16/1 " %.2X""\n"' kernel.img > kernel.hex
	echo " END" >> kernel.hex

.c.o:
	$(CC) -c $<

clean:
	rm -f *.o
	rm -f *.bin
	rm -f *.hex
	rm -f *.elf
	rm -f *.list
	rm -f *.img
	rm -f *~ core
