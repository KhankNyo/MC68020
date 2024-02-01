
start:
    movea #1, sp
    move.w #$ff, d0
    ext.w d0
    cmp.w #-1, d0
    bne failed

    movea #2, sp
    ext.l d0
    cmp.l #-1, d0
    bne failed

    movea #0, sp
failed:
inf: bra inf

