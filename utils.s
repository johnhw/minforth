@ CMOVE ( source dest length -- ) copy length bytes from source to dest
defcode "CMOVE",5,,CMOVE
        POP3 DSP                @ ( ), r2 = source, r1 = dest, r0 = length
        cmp r2, r1              @ account for potential overlap
        bge 2f                  @ copy forward if s >= d, backward otherwise
        sub r3, r0, #1          @ (length - 1)
        add r2, r3              @ end of source
        add r1, r3              @ end of dest
1:
        cmp r0, #0              @ while length > 0
        ble 3f
        ldrb r3, [r2], #-1      @    read character from source
        strb r3, [r1], #-1      @    and write it to dest (decrement both pointers)
        sub r0, r0, #1          @    decrement length
        b 1b
2:
        cmp r0, #0              @ while length > 0
        ble 3f
        ldrb r3, [r2], #1       @    read character from source
        strb r3, [r1], #1       @    and write it to dest (increment both pointers)
        sub r0, r0, #1          @    decrement length
        b 2b
3:
        NEXT

        

defcode "HASH",4,,HASH
    POPDSP r1
    POPDSP r0
    bl murmur_hash
    PUSHDSP r0
    NEXT
        
@ Compute the murmur3 hash of a string.
@ string in r0, len in r1, return in r0
.globl murmur_hash    
murmur_hash:
    ldr r6, =0xcc9e2d51
    ldr r7, =0x1b873593
    ldr r8, =0xe6546b64
    ldr r5, =0              @ hash    
    push {r1}
    
block_loop:    
        ldr r2, [r0]        @ k = blocks[i]        
        add r0, #4
        sub r1, #4
        mul r2, r6          @ *= c1
        mov r2, r2, ROR #32-15     @ rotl(k, 15)
        mul r2, r7          @ *=c2
        eor r5, r2          @ hash ^ = k
        mov r5, r5, ROR #32-13     @ hash = rotl(hash, 13)
        ldr r4, =5          @ hash = hash * m + n
        mul r5, r4
        add r5, r8        
        cmp r1, #3          @ loop if not finished
        bgt block_loop
            
    @ r1 is left over characters    
    cmp r1, #0
    beq finalise            @ can finalise if len%4==0    
    ldr r2, [r0]        
    cmp r1, #3    
    andeq r2, #0x00ffffff   @ hash the tail
    cmp r1, #2
    ldr r3, =0x0000ffff
    andeq r2, r3    
    cmp r1, #1
    andeq r2, #0x000000ff               
    mul r2, r6      @ *= c1
    mov r2, r2, ROR #32-15     @ rotl(k, 15)
    mul r2, r7      @ *=c2
    eor r5, r2      @ hash ^ = k
    
finalise:        
    pop {r1}
    eor r5, r1
    eor r5, r5, LSR #16
    ldr r0, =0x085ebca6b
    mul r5, r0
    eor r5, r5, LSR #13
    ldr r0, =0xc2b2ae35
    mul r5, r0
    eor r5, r5, LSR #16    
    mov r0, r5              @ return hash
    bx lr   


@ hash a 32 bit integer
defcode "HASHINT",7,,HASHINT
        ldr r1, =4
        POPDSP r5
        bl finalise
        PUSHDSP r0
        NEXT


@ DIVMOD computes the unsigned integer division and remainder
@ The implementation is based upon the algorithm extracted from 'ARM Software
@ Development Toolkit User Guide v2.50' published by ARM in 1997-1998
@ The algorithm is split in two steps: search the biggest divisor b^(2^n)
@ lesser than a and then subtract it and all b^(2^i) (for i from 0 to n)
@ to a.
@ ( a b -- r q ) where a = q * b + r
defcode "/MOD",4,,DIVMOD
        POPDSP  r1                      @ Get b
        POPDSP  r0                      @ Get a
        bl _DIVMOD
        PUSHDSP r0                      @ Put r
        PUSHDSP r2                      @ Put q
        NEXT

@ on entry r0=numerator r1=denominator
@ on exit r0=remainder r1=denominator r2=quotient
_DIVMOD:                        @ Integer Divide/Modulus
        mov     r3, r1                  @ Put b in tmp

        cmp     r3, r0, LSR #1
1:      movls   r3, r3, LSL #1          @ Double tmp
        cmp     r3, r0, LSR #1
        bls     1b                      @ Jump until 2 * tmp > a

        mov     r2, #0                  @ Initialize q

2:      cmp     r0, r3                  @ If a - tmp > 0
        subcs   r0, r0, r3              @ a <= a - tmp
        adc     r2, r2, r2              @ Increment q
        mov     r3, r3, LSR #1          @ Halve tmp
        cmp     r3, r1                  @ Jump until tmp < b
        bhs     2b

        bx lr        