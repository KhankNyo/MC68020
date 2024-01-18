

mem_location = $0420
start:
    eor d5, (SP)
    cmpa.l (mem_location, A3), SP
    cmpm (A4)+, (SP)+
    cmp d2, d3
    cmp.l ([mem_location, PC], d4.l*4), d7

