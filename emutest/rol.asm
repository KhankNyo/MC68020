
start:
    movea #1, sp
    move.l #$AAAAAAAA, d0
    rol.l #1, d0
    cmp.l #$55555555, d0
    bne.b failed

    movea #2, sp
    rol.l #3, d0
    cmp #$AAAAAAAA, d0
    bne.b failed

    movea #0, sp
failed:
inf: bra inf
