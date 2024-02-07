
start:
    movea #1, sp    ; test 1: sbcd 
    moveq #$23, d1
    moveq #$15, d0
    sbcd d0, d1
    cmp.b #$8, d1
    bne failed

    movea #2, sp    ; test 2: nbcd 
    addx d0, d0     ; clear x flag 
    nbcd d1
    cmp.b #$92, d1
    bne failed

    ; passed 
    movea #0, sp
failed:
inf: bra inf


