

value = 1 + 2
value2 = $10 + %0110
value3 = 100_000 + $FF_FF + %0000_1111_0000_1010
value4 = 12.1 + .2 + 1.
start:
    add d0, d1
    sub d2, d3
    bsr subroutine
    trap
    trapcc


subroutine:
    rts
