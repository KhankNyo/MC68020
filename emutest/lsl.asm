
start:
    movea #1, sp
    move.l #$80000000, d0
    lsl.l #1, d0
    bvs.b failed            ; assert V clear
    bne.b failed            ; assert Z set
    bcc.b failed            ; assert C set
    bmi.b failed            ; assert N clear
    addx d0, d0
    beq.b failed            ; assert X set 

    movea #2, sp
    move.l #$00000000, d0
    lsl.l #1, d0
    bmi.b failed            ; assert N clear

    movea #0, sp
failed:
inf: bra inf

