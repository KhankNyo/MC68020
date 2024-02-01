
start:
    movea #1, sp ; reg indirect
    lea (sp), a0
    cmpa.l a0, sp
    bne failed

    movea #2, sp ; reg + offset16
    movea #0, a1
    lea (failed, a1), a0
    adda.l #failed, a1
    cmpa.l a1, a0
    bne failed

    movea #3, sp ; reg + index + offset8
    movea #array16, a0
    moveq #1, d1
    lea (2, a0, d0.l*2), a0
    move.w (a0), d0
    cmp (array16 + 1*2), d0
    bne failed

    movea #4, sp ; abs word 
    lea (array16 + 1*2).w, a0
    move (a0), d0
    cmp #1, d0
    bne failed

    movea #5, sp ; abs long 
    lea (array16 + 5*2).l, a0
    move (a0), d0
    cmp #5, d0
    bne failed

    movea #6, sp ; PC + offset32
    lea (array16 + 3*2, PC), a0
    move (a0), d0
    cmp #3, d0
    bne failed

    movea #7, sp ; PC + index + offset32
    moveq #6, d0
    lea (array16, PC, d0*2), a0
    move (a0), d0
    cmp #6, d0
    bne failed

    movea #8, sp ; mem indirect postindexed PC
    moveq #2, d0
    lea ([arrptr, PC], d0*2, 2), a0
    move (a0), d0
    cmp #3, d0
    bne failed

    movea #9, sp ; mem indirect preindexed PC
    moveq #1, d0
    lea ([arrptr, PC, d0*4]), a0 ; a0 contains arrptr
    movea.l (a0), a0               ; loads &array16   
    move (4*2, a0), d0           ; d0 = array16[4]
    cmp #4, d0
    bne failed

    movea #10, sp ; mem indirect postindexed
    moveq #1, d0
    lea (arrptr).w, a0
    lea ([a0], d0*2), a0
    move (a0), d0
    cmp #1, d0
    bne failed

    movea #11, sp ; mem indirect preindexed
    moveq #-1, d0
    lea (arrptr + 4).w, a0         ; loads &arrptr[1]
    lea ([a0, d0*4], 7*2), a0      ; a0 contains &array16[7]
    move (a0), d0
    cmp #7, d0
    bne failed

    movea #12, sp ; reg + index + offset8
    moveq #5, d0
    lea (array16).w, a0
    lea (a0, d0*2), a0
    move (a0), d0
    cmp #5, d0
    bne failed


    movea #0, sp
failed:
inf: bra inf

array16:
    dc.w 0, 1, 2, 3, 4, 5, 6, 7
arrptr:
    dc.l array16, arrptr

