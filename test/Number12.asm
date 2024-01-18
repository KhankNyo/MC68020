

mem_location = $deadbeef
ptr_location = $baadf00d
start:
    mulu.w (SP), d2
    muls.w (mem_location, SP), d3
    and.l d3, (mem_location, SP, a4)
    and.b ([ptr_location, PC], SP*4, mem_location), d2
    abcd.b d3, d4
    abcd.b -(SP), -(A3)
    exg.l d3, d5
    exg.l d7, SP
    exg.l SP, A4

