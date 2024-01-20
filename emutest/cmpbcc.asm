
start:
    ; basic cmp test: equal 
    cmpa #0, a0
    movea #1, sp
    bne.w failed    ; assert Z set
    movea #2, sp
    bcs.w failed    ; assert C clear
    movea #3, sp
    bvs.w failed    ; assert V clear
    movea #4, sp
    bmi.w failed    ; assert N clear
    movea #5, sp
    bhi.w failed    ; assert LS
    movea #6, sp
    blt.b failed    ; assert GE
    movea #7, sp
    bgt.b failed    ; assert LE 

    ; basic cmp test: less than
    cmpa #1, a0
    beq.b failed    ; assert Z clear
    movea #8, sp
    bcs.b failed    ; assert C clear
    movea #9, sp
    bvs.b failed    ; assert V clear
    movea #10, sp
    bpl.b failed    ; assert N set
    movea #11, sp
    bls.b failed    ; assert HI
    movea #12, sp
    bge.b failed    ; assert LT
    movea #13, sp
    bgt.b failed    ; assert LE 

    ; overflow test 
    moveq #-128, d0 ; 0x80 
    subq.b #1, d0   ; 0x7F
    movea #14, sp
    bcs.b failed    ; assert C clear
    movea #15, sp
    bvc.b failed    ; assert V set 
    movea #16, sp
    bmi.b failed    ; assert N clear

    ; cmp test: less or equal
    moveq #0, d0
    cmp d0, d0      ; <=, >=, ==
    movea #17, sp
    bgt.b failed    ; assert not <
    movea #18, sp
    blt.b failed    ; assert not >
    movea #19, sp
    bne.b failed    ; assert not != 
    movea #20, sp
    ble.b skip0     ; assert <=
        bra.b failed
skip0:
    movea #21, sp
    bge.b skip1     ; assert >=
        bra.b failed
skip1:
    movea #22, sp
    beq.b skip2       ; assert == 
        bra.b failed
skip2:


    movea #0, sp    ; passed, 0 in sp
failed:
inf: bra inf




