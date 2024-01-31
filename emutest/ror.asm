
start:
    movea #1, sp
    moveq #%01010101, d0
    ror.b #1, d0
    cmp.l #%10101010, d0
    bne.b failed

    movea #2, sp
    ror.b #2, d0
    cmp.l #%10101010, d0
    bne.b failed

    movea #0, sp
failed:
inf: bra inf
