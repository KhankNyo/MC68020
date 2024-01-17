
start:
    move #1, D0
    move #2, (A0)
    move #3, -(A0)
    move #4, (A0)+
    move #5, (1234, A0)
    move #6, (-1, A0)
    move #7, (1, A0, D0)
    move #8, (1234, A0, D0.l*4)
    move #9, (1234, A0, D0)
    move #10, ([1234, A0], D0, 12345678)
    move #11, ([1234, A0], D0, 12345678)
    move #12, ([12, A0, D0], 1234)
place:
    move #13, ([12345678, A0, D0], 12345678)
    move place, D0
PCmem:
    move ([place, PC],D0,1), ([-2,A2,D1],10)

    movea d0, a1
    ; movea.b d0, a1
    nop
    nop
    nop
    move.b d0, d1
    move.w d0, d1
    move.l d0, d1
    movea d0, a1
    

