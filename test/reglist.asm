

rlist = D0-D3/A0/A3/A4-SP
start:
    movem {rlist}, (SP)
    movem -(SP), {rlist}
dw 0, rlist
dw 0, -rlist

