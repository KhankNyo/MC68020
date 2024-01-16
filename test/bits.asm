
m68kBits:
    btst.l #1, d1
    bchg.l #1, d2
    bclr.l #1, d3
    bset.l #1, d4
    btst.l d5, d1
    bchg.l d6, d2
    bclr.l d7, d3
    bset.l d1, d4

    btst.b #2, rel ([20, D2.w*4], 20)


