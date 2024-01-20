
start:
    movea.w #result + 4, a0
    movea.w #data + 4, a1

    addx.b -(a1), -(a0)
    addx.b -(a1), -(a0)
    addx.b -(a1), -(a0)
    addx.b -(a1), -(a0)

    ; result == $0100 0000?
    move.l result, d0
    add.l #-$1000000, d0
    beq passed

; failed
    moveq #-1, d0
    bra inf
passed:
    moveq #1, d0
inf: bra inf

data: db 0, $ff, $ff, $ff
result: db 0, 0, 0, $1

