

mem_location = $6969
start:
    divu (mem_location), d2
    divs (mem_location), d1
    sbcd.b d2, d1
    sbcd.b -(a2), -(a1)
    or.l d1, d2
    or.w ([mem_location, PC]), d3
    or.b d4, ([mem_location, sp])
