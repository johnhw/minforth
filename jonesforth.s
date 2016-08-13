@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@
@ pijFORTHos -- Raspberry Pi JonesFORTH Operating System
@
@ A bare-metal FORTH operating system for Raspberry Pi
@ Copyright (C) 2014 Dale Schumacher and Tristan Slominski
@
@ based on Jones' Forth port for ARM EABI
@ Copyright (C) 2013 M2IHP'13 class
@
@ Original x86 and FORTH code: Richard W.M. Jones <rich@annexia.org>
@
@ See AUTHORS for the full list of contributors.
@
@ The extensive comments from Jones' x86 version have been removed.  You should
@ check them out, they are really detailed, well written and pedagogical.
@ The original sources (with full comments) are in the /annexia/ directory.
@
@ DIVMOD routine taken from the ARM Software Development Toolkit User Guide 2.50
@
@ This program is free software: you can redistribute it and/or modify it under
@ the terms of the GNU Lesser General Public License as published by the Free
@ Software Foundation, either version 3 of the License, or (at your option) any
@ later version.
@
@ This program is distributed in the hope that it will be useful, but WITHOUT
@ ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
@ FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
@ details.
@
@ You should have received a copy of the GNU Lesser General Public License
@ along with this program.  If not, see <http://www.gnu.org/licenses/>.
@
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


@ Reserve three special registers:
@ DSP (r13) points to the top of the data stack
@ RSP (r11) points to the top of the return stack
@ FIP (r10) points to the next FORTH word that will be executed
@ Note: r12 is often considered a "scratch" register

DSP     .req    r13
RSP     .req    r11
FIP     .req    r10

@ Define macros to push and pop from the data and return stacks

        .macro PUSHRSP reg
        str     \reg, [RSP, #-4]!
        .endm
        
        .macro POPRSP reg
        ldr     \reg, [RSP], #4
        .endm

        .macro PUSHDSP reg
        str     \reg, [DSP, #-4]!
        .endm

        .macro POPDSP reg
        ldr     \reg, [DSP], #4
        .endm

        .macro PUSH2 reg
        stmdb   \reg!, {r0-r1}          @ ( -- r1 r0 )
        .endm

        .macro POP2 reg
        ldmia   \reg!, {r0-r1}          @ ( r1 r0 -- )
        .endm        

@ align a register to a 4 byte boundary        
.macro ALGN reg
        add     \reg, #3
        and     \reg, #~3
        .endm
        

@ _NEXT is the assembly subroutine that is called
@ at the end of every FORTH word execution.
@ The NEXT macro is defined to simply call _NEXT
        .macro NEXT
        b _NEXT
        .endm

@ jonesforth is the entry point for the FORTH environment
        .text
        .align 2                        @ alignment 2^n (2^2 = 4 byte alignment)
        .global _start
    
reset:    
        @ relocation code
        sub	r1, pc, #8	        @ Where are we?
        mov	sp, r1		        @ Bootstrap stack immediately before _start        
        ldr	r0, =0x8000	        @ Absolute address of kernel memory
        cmp	r0, r1		        @ Are we loaded where we expect to be?
        beq	no_relocate		    @ Then, jump to kernel entry-point
        mov	lr, r0		        @ Otherwise, relocate ourselves
        ldr	r2, =0x7F00	        @ Copy (32k - 256) bytes
    1:	ldmia	r1!, {r3-r10}	@ Read 8 words
        stmia	r0!, {r3-r10}	@ Write 8 words
        subs	r2, #32		    @ Decrement len
        bgt	1b		            @ More to copy?
        bx lr                   @ Return to our relocated selves!
no_relocate:        
        ldr r0, =0x8000         @ load the start address

jonesforth:
        ldr r0, =var_S0
        str DSP, [r0]                   @ Save the original stack position in S0
        ldr RSP, =return_stack_top      @ Set the initial return stack position
        ldr r0, =data_segment           @ Get the initial data segment address
        ldr r1, =var_HERE               @ Initialize HERE to point at
        str r0, [r1]                    @   the beginning of data segment
        ldr FIP, =cold_start            @ Make the FIP point to cold_start
        NEXT                            @ Start the interpreter

@ _DOCOL is the assembly subroutine that is called
@ at the start of every FORTH word execution, which:
@   0. expects the CFA of a FORTH word in r0
@   1. saves the old FIP on the return stack
@   2. makes FIP point to the DFA (first codeword)
@   3. uses _NEXT to start interpreting the word
_DOCOL:
        PUSHRSP FIP
        add FIP, r0, #4

@ _NEXT is the assembly subroutine that is called
@ at the end of every FORTH word execution, which:
@   1. finds the CFA of the FORTH word to execute
@      by dereferencing the FIP
@   2. increments FIP
@   3. begins executing the routine pointed to
@      by the CFA, with the CFA in r0
_NEXT:
        ldr r0, [FIP], #4
        ldr r1, [r0]
        bx r1

@ cold_start is used to bootstrap the interpreter, 
@ the first word executed is QUIT
        .section .rodata
cold_start:
        .int QUIT


@@ Now we define a set of helper macros that are syntactic sugar
@@ to ease the declaration of FORTH words, Native words, FORTH variables
@@ and FORTH constants.

@ define the word flags
        .set F_IMM, 0x80
        .set F_HID, 0x20
        .set F_LEN, 0x1f

@ link is used to chain the words in the dictionary as they are defined
        .set link, 0

@ defword macro helps defining new FORTH words in assembly
        .macro defword name, namelen, flags=0, label
        .section .rodata
        .align 2
        .global name_\label
name_\label :
        .int link               @ link
        .set link,name_\label
        .byte \flags+\namelen   @ flags + length byte
        .ascii "\name"          @ the name
        .align 2                @ padding to next 4 byte boundary
        .global \label
\label :
        .int _DOCOL             @ codeword - the interpreter
        @ list of word pointers follow
        .endm

@ defcode macro helps defining new native words in assembly
        .macro defcode name, namelen, flags=0, label
        .section .rodata
        .align 2
        .globl name_\label
name_\label :
        .int link               @ link
        .set link,name_\label
        .byte \flags+\namelen   @ flags + length byte
        .ascii "\name"          @ the name
        .align 2                @ padding to next 4 byte boundary
        .global \label
\label :
        .int code_\label        @ codeword
        .text
        .global code_\label
code_\label :                   @ assembler code follows
        .endm

@ simplify definitions of 2-operand operators
@ operands will appear in r0 and r1
@ ( ), r1 = a, r0 = b
.macro defop name, namelen, flags=0, label
        defcode \name,\namelen,\flags,\label
        POP2DSP

@ EXIT is the last codeword of a FORTH word.
@ It restores the FIP and returns to the caller using NEXT.
@ (See _DOCOL)
defcode "EXIT",4,,EXIT
        POPRSP FIP
        NEXT

@ defvar macro helps defining FORTH variables in assembly
        .macro defvar name, namelen, flags=0, label, initial=0
        defcode \name,\namelen,\flags,\label
        ldr r0, =var_\name
        PUSHDSP r0
        NEXT
        .data
        .align 2
        .global var_\name
var_\name :
        .int \initial
        .endm

@ push the given register onto the stack and call NEXT
.macro PUSHNEXT, r
    PUSHDSP \r
    NEXT
.endm        

@ The built-in variables are:
@  STATE           Is the interpreter executing code (0) or compiling a word (non-zero)?
        defvar "STATE",5,,STATE
@  HERE            Points to the next free byte of memory.  When compiling, compiled words go here.
        defvar "HERE",4,,HERE
@  LATEST          Points to the latest (most recently defined) word in the dictionary.
        defvar "LATEST",6,,LATEST,name_EXECUTE  @ The last word defined in assembly is EXECUTE
@  S0              Stores the address of the top of the parameter stack.
        defvar "S0",2,,S0

@ defconst macro helps defining FORTH constants in assembly
        .macro defconst name, namelen, flags=0, label, value
        defcode \name,\namelen,\flags,\label
        ldr r0, =\value
        PUSHDSP r0
        NEXT
        .endm

@ The built-in constants are:
@  R0              The address of the top of the return stack.
        defconst "R0",2,,R0,return_stack_top
@  DOCOL           Pointer to _DOCOL.
        defconst "DOCOL",5,,DOCOL,_DOCOL
@  F_IMMED         The IMMEDIATE flag's actual value.
        defconst "F_IMMED",7,,F_IMMED,F_IMM
@  F_HIDDEN        The HIDDEN flag's actual value.
        defconst "F_HIDDEN",8,,F_HIDDEN,F_HID
@  F_LENMASK       The length mask in the flags/len byte.
        defconst "F_LENMASK",9,,F_LENMASK,F_LEN
@ 1 itself (since we can't parse numbers yet!)        
        defconst "1",1,,ONE,1        

@ + ( a b -- a+b )
defop "+",1,,ADD        
        add r0, r0, r1
        PUSHNEXT r0
        
@ - ( a b -- a-b )
defcode "-",1,,SUB        
        sub r0, r1, r0
        PUSHNEXT r0
        
@ LSHIFT ( a b -- a<<b )
defop "LSHIFT",6,,LSHIFT
        mov r0, r1, LSL r0
        PUSHNEXT r0
        
@ RSHIFT ( a b -- a>>b )
defop "RSHIFT",6,,RSHIFT
        mov r0, r1, LSR r0
        PUSHNEXT r0
        
@ * ( a b -- a*b )
defop "*",1,,MUL
        mul r2, r1, r0
        PUSHNEXT r2
        
@ = ( a b -- p ) where p is 1 when a and b are equal (0 otherwise)
defop "=",1,,EQ
        cmp r1, r0
        mvneq r0, #0
        movne r0, #0
        PUSHNEXT r0
        
@ < ( a b -- p ) where p = a < b
defop "<",1,,LT
        cmp r1, r0
        mvnlt r0, #0
        movge r0, #0
        PUSHNEXT r0
        
@ > ( a b -- p ) where p = a < b
defop ">",1,,GT
        cmp r1, r0
        mvngt r0, #0
        movle r0, #0
        PUSHNEXT r0
        
@ AND ( a b -- a&b ) bitwise and
defop "AND",3,,AND
        and r0, r1, r0
        PUSHNEXT r0
        
@ OR ( a b -- a|b ) bitwise or
defop "OR",2,,OR
        orr r0, r1, r0
        PUSHNEXT r0
        
@ XOR ( a b -- a^b ) bitwise xor
defop "XOR",3,,XOR
        eor r0, r1, r0
        PUSHNEXT r0                    


@ ! ( value address -- ) write value at address
defop "!",1,,STORE
        str r1, [r0]
        NEXT

@ @ ( address -- value ) reads value from address
defcode "@",1,,FETCH
        POPDSP r1
        ldr r0, [r1]
        PUSHNEXT r0        

@ C! ( c addr -- ) write byte c at addr
defop "C!",2,,STOREBYTE        
        strb r1, [r0]
        NEXT

@ C@ ( addr -- c ) read byte from addr
defcode "C@",2,,FETCHBYTE
        POPDSP r1
        ldrb r0, [r1]
        PUSHNEXT r0
        
@ >R ( a -- ) move the top element from the data stack to the return stack
defcode ">R",2,,TOR
        POPDSP r0
        PUSHRSP r0
        NEXT

@ R> ( -- a ) move the top element from the return stack to the data stack
defcode "R>",2,,FROMR
        POPRSP r0
        PUSHNEXT r0
        
@ RSP@, RSP!, DSP@, DSP! manipulate the return and data stack pointers

defcode "RSP@",4,,RSPFETCH
        PUSHNEXT RSP
        
defcode "RSP!",4,,RSPSTORE
        POPDSP RSP
        NEXT

defcode "DSP@",4,,DSPFETCH
        mov r0, DSP
        PUSHNEXT r0        

defcode "DSP!",4,,DSPSTORE
        POPDSP r0
        mov DSP, r0
        NEXT
        
defcode "FIP@",4,,FIPFETCH
        mov r0, FIP
        PUSHNEXT r0
        
defcode "FIP!",4,,FIPSTORE
        POPDSP r0
        mov FIP, r0
        NEXT
        
@ MEMKEY ( -- c ) Read the next character from the built in source buffer
defcode "MEMKEY",6,,MEMKEY
        bl srcchar
        PUSHDSP r0        
        NEXT        
@ read one character from the pre-loaded source 
_MEMKEY:
    ldr r1, =srcptr     @ r1 = srcptr
    ldr r2, [r1]        @ r2 = *srcptr
    ldrb r0, [r2]       @ r0 = **srcptr     
    add r2, #1          @ *srcptr++
    str r2, [r1]        @ [writeback]
    bx lr               @ return
.data
    .align 2
    .global srcptr
    srcptr: .int _binary_jonesforth_f_start
            
        
@ WORD ( -- addr length ) reads next word from stdin
@ skips spaces, control-characters and comments, limited to 32 characters
defcode "MEMWORD",7,,MEMWORD
        bl _WORD
        PUSHDSP r0               @ address
        PUSHDSP r1               @ length
        NEXT
_WORD:
        stmfd   sp!, {r6,lr}     @ preserve r6 and lr
1:
        bl _MEMKEY               @ read a character        
        cmp r0, #' '
        ble 1b                   @ skip blank character

        ldr     r6, =word_buffer
2:
        strb r0, [r6], #1        @ store character in word buffer
        bl _MEMKEY               @ read more characters until a space is found
        cmp r0, #' '
        bgt 2b

        ldr r0, =word_buffer    @ r0, address of word
        sub r1, r6, r0          @ r1, length of word

        ldmfd sp!, {r6,lr}      @ restore r6 and lr
        bx lr

@ word_buffer for WORD
        .data
        .align 5                @ align to cache-line size
word_buffer:
        .space 32               @ FIXME: what about overflow!?
        .int 0
word_length:
        .space 1

@ FIND ( addr length -- dictionary_address )
@ Tries to find a word in the dictionary and returns its address.
@ If the word is not found, NULL is returned.
defcode "FIND",4,,FIND
        POPDSP r1       @ length
        POPDSP r0       @ addr
        bl _FIND
        PUSHNEXT r0        

_FIND:
        stmfd   sp!, {r5,r6,r8,r9}      @ save callee save registers
        ldr r2, =var_LATEST
        ldr r3, [r2]                    @ get the last defined word address
1:
        cmp r3, #0                      @ did we check all the words ?
        beq 4f                          @ then exit
        ldrb r2, [r3, #4]               @ read the length field
        and r2, r2, #(F_HID|F_LEN)      @ keep only length + hidden bits
        cmp r2, r1                      @ do the lengths match ?
                                        @ (note that if a word is hidden,
                                        @  the test will be always negative)
        bne 3f                          @ branch if they do not match
                                        @ Now we compare strings characters
        mov r5, r0                      @ r5 contains searched string
        mov r6, r3                      @ r6 contains dict string
        add r6, r6, #5                  @ (we skip link and length fields)
                                        @ r2 contains the length
2:
        ldrb r8, [r5], #1               @ compare character per character
        ldrb r9, [r6], #1
        cmp r8,r9
        bne 3f                          @ if they do not match, branch to 3
        subs r2,r2,#1                   @ decrement length
        bne 2b                          @ loop
                                        @ here, strings are equal
        b 4f                            @ branch to 4

3:
        ldr r3, [r3]                    @ Mismatch, follow link to the next
        b 1b                            @ dictionary word
4:
        mov r0, r3                      @ move result to r0
        ldmfd   sp!, {r5,r6,r8,r9}      @ restore callee save registers
        bx lr

@ >CFA ( dictionary_address -- executable_address )
@ Transform a dictionary address into a code field address
defcode ">CFA",4,,TCFA
        POPDSP r0
        bl _TCFA
        PUSHNEXT r0
        
_TCFA:
        add r0,r0,#4            @ skip link field
        ldrb r1, [r0], #1       @ load and skip the length field
        and r1,r1,#F_LEN        @ keep only the length
        add r0,r0,r1            @ skip the name field
        ALGN r0
        bx lr

@ CREATE ( address length -- ) Creates a new dictionary entry
@ in the data segment.
defcode "CREATE",6,,CREATE
        POPDSP r1       @ length of the word to insert into the dictionnary
        POPDSP r0       @ address of the word to insert into the dictionnary

        stmfd   sp!, {r4, r5,r6,r7,r8}      @ save callee save registers
        ldr r2,=var_HERE
        ldr r3,[r2]     @ load into r3 and r8 the location of the header
        mov r8,r3

        ldr r4,=var_LATEST
        ldr r5,[r4]     @ load into r5 the link pointer
        str r5,[r3]     @ store link here -> last
        add r3,r3,#4    @ skip link adress
        strb r1,[r3]    @ store the length of the word
        add r3,r3,#1    @ skip the length adress
        mov r7,#0       @ initialize the incrementation

1:
        cmp r7,r1       @ if the word is completley read
        beq 2f
        ldrb r6,[r0,r7] @ read and store a character
        strb r6,[r3,r7]
        add r7,r7,#1    @ ready to read the next character
        b 1b
2:
        add r3,r3,r7            @ skip the word
        ALGN r3                 @ align to next 4 byte boundary
        str r8,[r4]             @ update LATEST and HERE
        str r3,[r2]
        ldmfd   sp!, {r4, r5,r6,r7,r8}      @ restore callee save registers
        NEXT

        
@ , ( n -- ) writes the top element from the stack at HERE
defcode ",",1,,COMMA
        POPDSP r0
        bl _COMMA
        NEXT
_COMMA:
        ldr     r1, =var_HERE
        ldr     r2, [r1]        @ read HERE
        str     r0, [r2], #4    @ write value and increment address
        str     r2, [r1]        @ update HERE
        bx      lr

@ [ ( -- ) Change interpreter state to Immediate mode
defcode "[[",2,F_IMM,LBRAC
        ldr     r0, =var_STATE
        mov     r1, #0                  @ FALSE
        str     r1, [r0]
        NEXT

@ ] ( -- ) Change interpreter state to Compilation mode
defcode "]]",2,,RBRAC
        ldr     r0, =var_STATE
        mvn     r1, #0                  @ TRUE
        str     r1, [r0]
        NEXT

defcode "SOURCE",6,,SOURCE
        ldr r0, =_binary_jonesforth_f_start
        PUSHDSP r0
        ldr r0, =_binary_jonesforth_f_size
        PUSHNEXT r0
        
defword "KEY",3,,KEY
        .int MEMKEY
        .int EXIT
        
defword "WORD",4,,WORD
        .int MEMWORD
        .int EXIT
        
@ ' ( -- ) returns the codeword address of next read word
@ only works in compile mode. Implementation is identical to LIT.
defcode "'",1,,TICK
        ldr r0, [FIP], #4
        PUSHNEXT r0        
                
@ : word ( -- ) Define a new FORTH word
@ : : WORD CREATE DOCOL , ] ;
defword ":",1,,COLON
        .int WORD                       @ Get the name of the new word
        .int CREATE                     @ CREATE the dictionary entry / header
        .int DOCOL, COMMA               @ Append DOCOL (the codeword).
        .int RBRAC                      @ Go into compile mode.
        .int EXIT                       @ Return from the function.

@ : ; IMMEDIATE LIT EXIT , [ ;
defword ";",1,F_IMM,SEMICOLON
        .int TICK, EXIT, COMMA           @ Append EXIT (so the word will return).
        .int LBRAC                      @ Go back to IMMEDIATE mode.
        .int EXIT                       @ Return from the function.

       
@ BRANCH ( -- ) changes FIP by offset which is found in the next codeword
defcode "BRANCH",6,,BRANCH
        ldr r0, [FIP]
        add FIP, FIP, r0
        NEXT

@ 0BRANCH ( p -- ) branch if the top of the stack is zero
defcode "0BRANCH",7,,ZBRANCH
        POPDSP r0
        cmp r0, #0              @ if the top of the stack is zero
        beq code_BRANCH         @ then branch
        add FIP, FIP, #4        @ else, skip the offset
        NEXT

@ LITS as LIT but for strings
defcode "LITS",4,,LITS
        ldr r0, [FIP], #4       @ read length
        PUSHDSP FIP             @ push address
        PUSHDSP r0              @ push string
        add FIP, FIP, r0        @ skip the string
        ALGN FIP        
        NEXT
    

@ QUIT ( -- ) the first word to be executed
defword "QUIT", 4,, QUIT
        .int INTERPRET                  @ Interpret a word                
        .int BRANCH,-8                  @ LOOP FOREVER

@ INTERPRET, reads a word from stdin and executes or compiles it.
@ No need to backup callee save registers here,
@ since we are the top level routine!
defcode "INTERPRET",9,,INTERPRET
7:  
        bl _WORD                        @ read a word from stdin
        mov r4, r0                      @ store it in r4,r5
        mov r5, r1

        bl _FIND                        @ find its dictionary entry
        cmp r0, #0                      @ if not found go to 6
        beq 6f

    @ Here the entry is found
        ldrb r6, [r0, #4]               @ read length and flags field
        bl _TCFA                        @ find code field address
        tst r6, #F_IMM                  @ if the word is immediate
        bne 4f                          @ branch to 4 (execute)
        b 2f                            @ otherwise, branch to 2

2:  @ Compiling or Executing
        ldr r1, =var_STATE              @ Are we compiling or executing ?
        ldr r1, [r1]
        cmp r1, #0
        beq 4f                          @ Go to 4 if in interpret mode

    @ Here in compile mode
        bl _COMMA                       @ Call comma to compile the codeword
        NEXT

4:  @ Executing
        ldr r1, [r0]                    @ (it's important here that
        bx r1                           @  FIP address in r0, since _DOCOL
                                        @  assumes it)
6:  @ Parse error        
    @ just ignore it; must *NEVER* happen before we've established the new interpreter
        NEXT
                
@ EXECUTE ( xt -- ) jump to the address on the stack
@-- WARNING! THIS MUST BE THE LAST WORD DEFINED IN ASSEMBLY (see LATEST) --@
defcode "EXECUTE",7,,EXECUTE
        POPDSP r0
        ldr r1, [r0]
        bx r1
        
@ Reserve space for the return stack (1Kb)
        .bss
        .align 5                @ align to cache-line size
        .set RETURN_STACK_SIZE, 0x400
return_stack:
        .space RETURN_STACK_SIZE
return_stack_top:

@ Reserve space for new words and data structures (16Mb)
        .bss
        .align 5                @ align to cache-line size
        .set DATA_SEGMENT_SIZE, 0x1000000
data_segment:
        .space DATA_SEGMENT_SIZE
data_segment_top:
