#
# Makefile for minforth -- Raspberry Pi JonesFORTH Operating System
#

AS=	as
CC=	gcc -Wall -O2 -nostdlib -nostartfiles -ffreestanding -Os
LD=	ld

KOBJS=	start.o jonesforth.o 

all: kernel.img

start.o: start.s
	$(AS) start.s -o start.o

jonesforth.o: jonesforth.s
	$(AS) jonesforth.s -o jonesforth.o

kernel.img: loadmap $(KOBJS)
	objcopy -I binary -O elf32-littlearm -B arm --rename-section .data=.rodata,alloc,load,readonly,data,contents jonesforth.f jonesforthf.o
	$(LD) $(KOBJS) jonesforthf.o  -T loadmap -o minforth.elf -Map minforth.map
	objdump -D minforth.elf > minforth.list
	objcopy minforth.elf -O ihex minforth.hex
	objcopy --only-keep-debug minforth.elf kernel.sym
	objcopy minforth.elf -O binary kernel.img
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
