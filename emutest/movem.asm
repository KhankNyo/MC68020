

start:
    moveq #1, d0
    moveq #2, d1
    moveq #3, d2
    moveq #4, d3
    moveq #5, d4
    moveq #6, d5
    moveq #7, d6
    moveq #8, d7
    movea #9, a0
    movea #10, a1
    movea #11, a2
    movea #12, a3
    movea #13, a4
    movea #14, a5
    movea #15, a6

    movea #stack_start, sp
savelist = D0-D7/A0-A7
    movem.l #savelist, -(sp)

    moveq #-1, d0
    moveq #-1, d1
    moveq #-1, d2
    moveq #-1, d3
    moveq #-1, d4
    moveq #-1, d5
    moveq #-1, d6
    moveq #-1, d7
    movea #-1, a0
    movea #-1, a1
    movea #-1, a2
    movea #-1, a3
    movea #-1, a4
    movea #-1, a5
    movea #-1, a6
    movem.l (sp)+, savelist
inf: bra inf
stack_end: 
resv $100
stack_start:

