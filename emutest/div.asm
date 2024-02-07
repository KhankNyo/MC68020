
start:
    movea #1, sp
    moveq #5, d0
    moveq #2, d1
    divs.w d1, d0
    cmp.l #$0001_0002, d0
    bne failed

    movea #2, sp
    moveq #-8, d0
    moveq #3, d1
    divs.w d1, d0
    cmp.l #$fffe_fffe, d0
    bne failed

    movea #3, sp
    moveq #5, d0
    moveq #-2, d1
    divs.w d1, d0
    cmp.l #$0001_fffe, d0
    bne failed

    movea #4, sp
    move.l #$10000, D0
    moveq #1, d1
    divu.w d1, d0
    bvc failed          ; assert V set 

    movea #5, sp
    move.l #$1000, d0
    moveq #-1, d1
    divu.w d1, d0
    bne failed          ; assert Z set

    movea #0, sp
failed:
inf: bra inf


