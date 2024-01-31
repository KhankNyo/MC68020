
start:
    movea #1, sp
    moveq #%01001100, d0
    lsr.l #2, d0
    bvs.b failed            ; assert V clear 
    bcs.b failed            ; assert C clear 
    bmi.b failed            ; assert N clear
    beq.b failed            ; assert Z clear
    cmp.l #%00010011, d0    ; assert d0 == 0b00010011
    bne.b failed

    movea #2, sp
    moveq #1, d0
    lsr #1, d0
    bcc.b failed            ; assert C set 
    bne.b failed            ; assert N set

    movea #0, sp
failed:
inf: bra inf

