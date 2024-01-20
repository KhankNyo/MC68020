
start:
    move.l #$baddf00d, d0
    move.l d0, d1
    move.w d1, d2
    move.b d2, d3

    movea #data, a0
    move (a0)+, d1
    move (a0)+, d2

    movea.w #-1, a1             ; sign extend
    move.w ([addr, PC]), d4     ; D4 should be $dead

inf: bra inf 

data:
    dw $dead, $beef
addr:
    dl data
    

