
mem = $55AA
off = 10
start:
    negx.l d4
    clr.l (mem)
    neg.l (mem)
    not.b (mem)

    ext.l d5
    extb.l d4
    ext.w d3
    nbcd.b (mem)

    swap d4
    pea.l ([mem, PC], d6.l*2, off)
    tas.b ([SP], mem)
    tst ([mem, PC], d2)

    jsr longfunc
    jmp start
longfunc:
self: jmp self