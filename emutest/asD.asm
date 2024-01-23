
start:
    ; sign shift test
    moveq #-1, d0        ; shift test
    movea #1, sp
    asr.b #8, d0
        beq.b failed ; assert Z clear
        bcc.b failed ; assert C set
        bvs.b failed ; assert V clear 
        bpl.b failed ; assert N set 
        moveq #0, d2
        addx d2, d2
        beq.b failed ; assert X set, see line above 
    movea #2, sp
    asr.w d0, d0        ; oversized shift test
        beq.b failed ; assert Z clear
        bcc.b failed ; assert C set
        bvs.b failed ; assert V clear 
        bpl.b failed ; assert N set 
        moveq #0, d2
        addx d2, d2
        beq.b failed ; assert X set, see line above 
    movea #3, sp        ; shift by 0 test
    moveq #0, d3     ; clears C and X
    asr.l d3, d0
        beq.b failed ; assert Z clear
        bcs.b failed ; assert C clear
        bvs.b failed ; assert V clear 
        bpl.b failed ; assert N set 
        addx d2, d2
        beq.b failed ; assert x set, see line above 

    movea #4, sp        ; shift arith left test
    moveq #-128, d0     ; 0xFFFFFF80 -> 0xFFFFFF00
    asl.b #1, d0
        bvc failed   ; assert V set
        bcc failed   ; assert C set
        bne failed   ; assert Z set
        bmi failed   ; assert N clear

    movea #5, sp
    moveq #-1, d0


    movea #0, sp     ; passed
failed:
inf: bra inf


mem:
    

