@@
@@ pijFORTHos -- Raspberry Pi JonesFORTH Operating System
@@
@@ This code is loaded at 0x00008000 on the Raspberry Pi ARM processor
@@ and is the first code that runs to boot the O/S kernel.
@@
@@ View this file with hard tabs every 8 positions.
@@	|	|	.	|	.	.	.	.  max width ->
@@      |       |       .       |       .       .       .       .  max width ->
@@ If your tabs are set correctly, the lines above should be aligned.
@@

@ GET and SET macros
.macro SET32 addr=0, var=0
    ldr r0, =\addr
    ldr r1, =\var
    str r1, [r0]
.endm

.macro GET32 addr=0
    ldr r1, =\addr
    ldr r0, [r1]
.endm

@@ UART constants
.set GPFSEL1, 0x20200004
.set GPSET0, 0x2020001C
.set GPCLR0, 0x20200028
.set GPPUD, 0x20200094
.set GPPUDCLK0, 0x20200098

.set AUX_ENABLES, 0x20215004
.set AUX_MU_IO_REG, 0x20215040
.set AUX_MU_IER_REG, 0x20215044
.set AUX_MU_IIR_REG, 0x20215048
.set AUX_MU_LCR_REG, 0x2021504C
.set AUX_MU_MCR_REG, 0x20215050
.set AUX_MU_LSR_REG, 0x20215054
.set AUX_MU_MSR_REG, 0x20215058
.set AUX_MU_SCRATCH, 0x2021505C
.set AUX_MU_CNTL_REG, 0x20215060
.set AUX_MU_STAT_REG, 0x20215064
.set AUX_MU_BAUD_REG, 0x20215068

.set IRQ_BASIC, 0x2000B200
.set IRQ_PEND1, 0x2000B204
.set IRQ_PEND2, 0x2000B208
.set IRQ_FIQ_CONTROL, 0x2000B210
.set IRQ_ENABLE1, 0x2000B210
.set IRQ_ENABLE2, 0x2000B214
.set IRQ_ENABLE_BASIC, 0x2000B218
.set IRQ_DISABLE1, 0x2000B21C
.set IRQ_DISABLE2, 0x2000B220
.set IRQ_DISABLE_BASIC, 0x2000B224    


@@ Entry point
@ _start is the bootstrap entry point
	.text
	.align 2
	.global _start
    
    reset:    
        @ relocation code
        sub	r1, pc, #8	@ Where are we?
        mov	sp, r1		@ Bootstrap stack immediately before _start
        ldr	lr, =halt	@ Halt on "return"
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
        SET32 IRQ_DISABLE1, (1<<29)         @ disable interrupts and clear the buffer
        SET32 rxhead, 0
        SET32 rxtail, 0
        
        @ Enable interrupts
        mov r0,#0xD2
        msr cpsr_c,r0
        
        mov r0,#0xD1
        msr cpsr_c,r0
        
        mov r0,#0xD3
        msr cpsr_c,r0
        
        @ copy the IRQ table into 0-1F
        ldr	r1, =_irq_table	@ IRQ table		
        ldr r0, =0
        ldr	r2, =0x100	@ Copy 256 bytes
    2:	ldmia	r1!, {r3-r10}	@ Read 8 words
        stmia	r0!, {r3-r10}	@ Write 8 words
        subs	r2, #32		@ Decrement len
        bgt	2b		@ More to copy?
         
    @ Initialise the UART to operate in IRQ mode
    .globl uart_init    
    uart_init:
        SET32 IRQ_DISABLE1, (1<<29)
        SET32 AUX_ENABLES, 1
        SET32 AUX_MU_IER_REG,0
        SET32 AUX_MU_CNTL_REG,0
        SET32 AUX_MU_LCR_REG, 3
        SET32 AUX_MU_MCR_REG, 0
        SET32 AUX_MU_IER_REG, 5
        SET32 AUX_MU_IIR_REG, 0xC6
        SET32 AUX_MU_BAUD_REG, 270
        
        GET32 GPFSEL1
        and r0, #~(7<<12)
        orr r0, #(2<<12)
        and r0, #~(7<<15)
        orr r0, #(2<<15)
        ldr r1, =GPFSEL1
        str r0, [r1]
            
        SET32 GPPUD, 0    
        ldr r0, =150
    SPIN1:
        subs	r0, #1	
        bge	SPIN1			
        SET32 GPPUDCLK0, ((1<<14)|(1<<15))
        ldr r0, =150
    SPIN2:
        subs	r0, #1	
        bge	SPIN2		
        SET32 GPPUDCLK0, 0      
        SET32 AUX_MU_CNTL_REG,3
        SET32 IRQ_ENABLE1,(1<<29)
    enable_irq:
        mrs r0,cpsr
        bic r0,r0,#0x80
        msr cpsr_c,r0
        
        ldr r0, =boot_string
        bl uart_puts        
    wait_enter:
        @ print the welcome, and wait for enter before continuing
        bl _getchar              
        cmp r0, #10
        bne wait_enter
        ldr r0, =welcome_string
        bl uart_puts
        
        ldr r0, =0x8000     @ load the start address
        b jonesforth
               
    halt:
        b	halt		@ Full stop
    
@ the IRQ table. This will be loaded at 0 and consists of 
@ a series of jump commands (actually ldr pc,) to jump to
@ respective handler. We are only interested in the irq_handler
    _irq_table:   
        ldr pc,reset_handler
        ldr pc,undefined_handler
        ldr pc,swi_handler
        ldr pc,prefetch_handler
        ldr pc,data_handler
        ldr pc,unused_handler
        ldr pc,irq_handler
        ldr pc,fiq_handler
        reset_handler: .word reset
        undefined_handler: .word halt
        swi_handler: .word halt
        prefetch_handler: .word halt
        data_handler: .word halt
        unused_handler: .word halt
        irq_handler: .word irq
        fiq_handler: .word halt     
           
@ Perform a hard reset by setting the watchdog and waiting for it to trip
    .globl wdog_reset
    wdog_reset:
        .set PM_RSTC, 0x2010001c
        .set PM_WDOG, 0x20100024
        .set PM_PASSWORD, 0x5a000000
        .set PM_RSTC_WRCFG_FULL_RESET, 0x00000020    
        SET32 PM_WDOG, PM_PASSWORD|1
        SET32 PM_RSTC, PM_PASSWORD|PM_RSTC_WRCFG_FULL_RESET    
    hang: b hang
    
   
@@ UART reading and writing functions   
   
@ Write one character to the UART    
    .globl uart_putc
    uart_putc:
        ldr r1, =AUX_MU_LSR_REG
    uart_wait:
        ldr r2, [r1]
        ands r2, #0x20    
        beq uart_wait
        ldr r1, =AUX_MU_IO_REG
        str r0, [r1]
        bx lr

@ write a zero-teriminated string to the UART
    .globl uart_puts
    uart_puts:
        mov r1, r0
    puts_loop:    
        ldrb r0, [r1]
        add r1, #1       
        cmp r0, #0 
        beq puts_exit
        push {r1,lr}    
        bl uart_putc
        pop {r1,lr}    
        b puts_loop
    puts_exit:
        bx lr
        
@ read a single character from the UART buffer
        .globl uart_getc    
    uart_getc:
        ldr r0, =rxhead
        ldr r1, =rxtail        
    uart_getc_wait:    
        ldr r3, [r0]
        ldr r2, [r1]        @ while rxtail==rxhead
        cmp r2, r3    
        beq uart_getc_wait            
        ldr r3, =rxbuffer   @ c=rxbuffer[rxtail]            
        ldrb r0, [r3,r2]
        add r2, #1          @ rxtail = (rxtail+1)&RXBUFMASK
        lsl     r2, r2, #20
        lsr     r2, r2, #20        
        str r2, [r1]
        bx lr               @ return c (in r0)
    
    
@ return 0 if no character ready, 1 if there is
    .globl uart_ready
    uart_ready:
        ldr r0, =rxhead
        ldr r1, =rxtail        
        ldr r0, [r0]
        ldr r1, [r1]        @ rxtail==rxhead?
        cmp r0, r1
        ldreq r0, =0
        ldrne r0, =-1
        bx lr
        
@ Write on character to the UART, translating newlines    
    .global putchar
    putchar:
        push {lr}        
        cmp r0, #10    
        bne regularchar
        ldr r0, =13             @ if '\n' write '\r\n'
        bl uart_putc
        ldr r0, =10
        bl uart_putc
        b putchar_exit
    regularchar:
        bl uart_putc
    putchar_exit:
        pop {lr}
        bx lr

@ Get one character, no echo
    .global _getchar
    _getchar:
        push {lr}
        bl uart_getc    
        cmp r0, #13
        ldreq r0, =10
        pop {lr}
        bx lr
    
        
@ IRQ handler    
@ Just writes characters from the UART into a circular buffer, which can
@ then be read by uart_getc()
    irq:        
        push {r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,lr}
        @ clear the monitor flag
        @ clear the post irq jump
    irq_start:
        GET32 AUX_MU_IIR_REG
        mov r1, r0
        ands r1, #1             @ check if we have done all the interrupts
        bne irq_end
        ands r0, #6
        cmp r0, #4
        bne irq_start           @ loop, if it's not UART interrupt
        GET32 AUX_MU_IO_REG
        ldr r1, =rxbuffer        
        ldr r2, =rxhead
        ldr r3, [r2]
        uxtb r0, r0
        strb r0, [r1,r3]        @ rxbuffer[rxhead] = rc & 0xff
        add r3, #1
        lsl     r3, r3, #20
        lsr     r3, r3, #20        
        str r3, [r2]            @ rxhead = (rxhead+1) & mask
        ldr r2, =key_state
        ldr r2, [r2]
        cmp r2, #0x18           @ ctrl-x
        bne not_escaped
        cmp r0, #0x03
        beq wdog_reset        
    not_escaped:
        ldr r2, =key_state
        strb r0, [r2]
    irq_end:
        pop  {r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,lr}        
        subs pc,lr,#4    

@ previous key read; use this to determine if we should reset (on CTRL-X CTRL-C)    
key_state: .word 0
        
.data
    .align 5
    welcome_string:
        .ascii "\x1B[44m minFORTH 0.4 READY\x1B[40m\r\n\0"
    boot_string:
        .ascii "<PRESS ENTER>\r\n\0"
       
.bss
    .align 5                @ align to cache-line size
    .set RXBUFMASK, 0xFFF
        
    .globl rxtail, rxhead, rxbuffer
    rxtail: .word  0
    rxhead: .word  0
    
    rxbuffer:
            .space RXBUFMASK+1    
