

int_size = 4
start:
    sub.l d1, d2
    suba.l d1, a2
    subx d2, d3
    subx -(a1), -(a2)
    suba (a1, d4.l*4), a0
    sub (-4, a1, d3*int_size), d5


