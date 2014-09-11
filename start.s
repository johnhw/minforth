@@
@@ minFORTH -- Raspberry Pi JonesFORTH Operating System
@@
@@ This code is loaded at 0x00008000 on the Raspberry Pi ARM processor
@@ and is the first code that runs to boot the O/S kernel.
@@
@@ View this file with hard tabs every 8 positions.
@@	|	|	.	|	.	.	.	.  max width ->
@@      |       |       .       |       .       .       .       .  max width ->
@@ If your tabs are set correctly, the lines above should be aligned.
@@


@@ Entry point
@ _start is the bootstrap entry point
	.text
	.align 2
	.global _start
    
    reset:    
        @ relocation code
        sub	r1, pc, #8	@ Where are we?
        mov	sp, r1		@ Bootstrap stack immediately before _start        
        ldr	r0, =0x8000	@ Absolute address of kernel memory
        cmp	r0, r1		@ Are we loaded where we expect to be?
        beq	no_relocate		@ Then, jump to kernel entry-point
        mov	lr, r0		@ Otherwise, relocate ourselves
        ldr	r2, =0x7F00	@ Copy (32k - 256) bytes
    1:	ldmia	r1!, {r3-r10}	@ Read 8 words
        stmia	r0!, {r3-r10}	@ Write 8 words
        subs	r2, #32		@ Decrement len
        bgt	1b		@ More to copy?
        bx lr       @ Return to our relocated selves!
no_relocate:        
        ldr r0, =0x8000     @ load the start address
        b jonesforth
               
    
    
           