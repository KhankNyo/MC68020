
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
    
