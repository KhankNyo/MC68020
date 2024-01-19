
start:
    move.l #$deadbeef, d0
    move.l d0, d1
    move.w d1, d2
    move.b d2, d3

    movea #data, a0
    move (a0)+, d1
    move (a0)+, d2


inf: bra inf 

data:
    dw $dead, $beef
    

