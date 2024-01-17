
start:
    addq #1, d0
    dbne d0, start

    blt forward
    addq #1, d0
    addq #1, d0
forward:
    addq #1, d0
    bgt.b smallforward
    addq #1, d0
    addq #1, d0

smallforward:
    addq #2, d0
    bgt.w wordforward
    addq #2, d0
    addq #2, d0
wordforward:
    addq #3, d0


loop: bra loop
    bra.w infront
infront:
    addq #4, d0
    addq #8, d0
    
