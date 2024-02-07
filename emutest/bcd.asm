
start:
    movea #1, sp    ; test 1: nbcd 
    moveq #$8, d0
    nbcd d0         ; 100 - 8 = 92
    cmp.b #$92, d0
    bne failed

    addx d1, d1     ; clear x flag 
    moveq #$23, d1
    moveq #$15, d0
    sbcd d0, d1     ; 23 - 15 = 8
    movea #2, sp    ;  test 2: sbcd C flag
    bcs failed      ; assert C clear
    movea #3, sp    ;  test 3: sbcd Z flag
    beq failed      ; assert Z clear
    cmp.b #$8, d1   ; == 8?
    movea #4, sp    ;  test 4: sbcd result 
    bne failed

    addx d0, d0     ; clear x flag 
    moveq #$15, d0
    moveq #$23, d1
    sbcd d1, d0     ; 15 - 23 = 92 (wrapping arithmetic, equivalent to -8)
    movea #5, sp    ;  test 5: sbcd resulting in carry
    bcc failed      ; assert carry set 
    movea #6, sp    ;  test 6: Z flag 
    beq failed      ; assert Z clear
    cmp.b #$92, d0  ; == 92? 
    movea #7, sp    ;  test 7: sbcd result 
    bne failed

    movea #8, sp    ; test 8: long sbcd 
    moveq #0, d0    ; clear X flag
    addx d0, d0
    lea (minuend + 4), a0
    lea (subtrahend + 4), a1
    sbcd -(a1), -(a0)
    sbcd -(a1), -(a0)
    sbcd -(a1), -(a0)
    sbcd -(a1), -(a0)
    move.l #$0045_3973, d0
    move.l (minuend), d1
    cmp.l d1, d0
    bne failed

    movea #9, sp    ; test 9: abcd
    moveq #0, d0
    addx d0, d0     ; clear X flag
    moveq #$15, d0
    moveq #$17, d1
    abcd d0, d1
    cmp.b #$32, d1
    bne failed

    movea #10, sp   ; test 10: abcd long 
    moveq #0, d0    ; clear X flag
    addx d0, d0
    lea (add_A + 4), a0
    lea (add_B + 4), a1
    abcd -(a1), -(a0)
    abcd -(a1), -(a0)
    abcd -(a1), -(a0)
    abcd -(a1), -(a0)
    move.l (a0), d0
    cmp.l #$0155_4127, d0
    bne failed

    moveq #0, d0    ; clear X flag
    addx d0, d0
    moveq #$73, d0
    moveq #$27, d1
    abcd d0, d1
    movea #11, sp   ; test 11: abcd with carry
    bcc failed
    movea #12, sp   ; test 12: abcd == 0
    bne failed

    ; passed 
    movea #0, sp
failed:
inf: bra inf

minuend:
    dc.b $01, $00, $40, $50 ; decimal: 1004050
subtrahend:
    dc.b $00, $55, $00, $77 ; decimal:  550077
                            ; expect:   453973

add_A:
    dc.b $01, $00, $40, $50 ; decimal: 1004050
add_B:
    dc.b $00, $55, $00, $77 ; decimal:  550077
                            ; expect:  1554127


