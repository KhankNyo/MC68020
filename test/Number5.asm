
label:
    addq #1, d0
    dblt d0, label
    addq #1, d0
    addq #1, (label)
    subq #1, ([A0], 1)
    sge.b (label + 4)
    dblt d0, label

