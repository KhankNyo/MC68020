
start:
    st d1
    sf d1
    shi d1
    sls d1

    bcc start
    bcs start
    bne start
    beq start

    dbvc d1, start
    dbvs d1, start
    dbpl d1, start
    dbmi d1, start

    sge d1
    slt d1
    sgt d1
    sle d1

