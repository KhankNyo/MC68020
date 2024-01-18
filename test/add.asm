

mem_location = $AA55AA55
start:
    add SP, d3
    add #-1, d4
    add (SP), d5
    adda #-4, SP
    adda ([SP], d7, mem_location), a2
    addx d3, d4
    addx -(SP), -(A6)
    


