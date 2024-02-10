
start:
    movea #1, sp            ; test 1: neg-neg
    moveq #-4, d0
    muls.w (mem, PC), d0
    cmp.l #$00000004, d0
    bne failed              ; assert result 
    bvs failed              ; assert overflow clear (should never be on)

    movea #2, sp            ; test 2: neg-pos
    moveq #3, d0
    muls.w (mem, PC), d0
    cmp.l #-3, d0
    bne failed

    movea #3, sp            ; test 3: unsigned mul 
    moveq #-1, d0
    mulu.w (mem + 2, PC), d0
    cmp.l #$ffff * $ffff, d0
    bne failed

    ; passed 
    movea #0, sp
failed: 
inf: bra inf

mem: 
    dc.w -1, $ffff


