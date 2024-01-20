
start:
    ; test sub instruction
    moveq #1, d0
    sub.b #1, d0
    movea #1, sp
    bne.b failed            ; asserts that z flag is set 

    ; test subx instruction
    movea #2, sp
    movea.w #data + 4*4, a0
    movea.w #result + 4*4, a1

    moveq #0, d0            ; sets Z flag 
    subx.l -(a0), -(a1)
    subx.l -(a0), -(a1)
    subx.l -(a0), -(a1)
    subx.l -(a0), -(a1)
    movea #3, sp
        bcc.b failed        ; asserts C flag clear
    movea #4, sp
        beq.b failed        ; asserts Z flag clear

    ; test suba and result of subx
    movea.w #result, a0
    movea.w #5, sp          ; test codes: 5, 6, 7, 8
    moveq #-1, d0
    moveq #4, d1
loophead:
    movea.l (a0)+, a1
    suba.w d0, a1           ; used .w but whole register should be affected
    move.l a1, d2           ; use data reg for flag testing 
        bne.b failed        ; a1 != -1? failed
    adda #1, sp
    sub.b #1, d1
    bne loophead

    movea.w #0, sp          ; 0 in sp, passed
failed:
inf: bra inf


dw 0, 0
data:   dl 0, 0, 0, 1
result: dl 0, 0, 0, 0
    
