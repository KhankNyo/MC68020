
start:
    movea #1, sp    ; test code 1: flags
l0:
    addq.b #1, (counter0).w
    bne l0          ; counter0: 0 .. 255 
    bcc.b failed    ; assert C set 
    bvs.b failed    ; assert V clear
    bmi.b failed    ; assert N clear 

    movea #2, sp    ; test code 2: byte overflowed
    ; check bytes behind the counter 
    move.l (counter0 - 3).w, d0
    bne.b failed    ; overflowed? 

l1:
    subq.w #5, (counter1).w
    bne l1          ; counter1: 10 .. 0
    ; no test code here, it'll pass if it finishes the loop correctly 

    movea #3, sp    ; test code 3: subq
    subq.l #1, (counter2).w
    bne.b failed

    moveq #0, d1    ; for later use 
    movea #4, sp
    moveq #-1, d0   ; d0 = $FFFFFFFF
    addq.b #1, d0   ; should only affect low byte, d0 = $FFFFFF00
        bne.b failed; assert Z set 
        bvs.b failed; assert V clear
        bcc.b failed; assert C set
    addx.b d0, d1   ; get X flag
        beq.b failed; assert X set 
    move.l d0, d0   ; test d0
        beq.b failed    ; d0 == 0? 

    movea #0, sp    ; passed
failed:
inf: bra inf

dw 0, 0 ; buffer space 
counter0: db $fe
counter1: dw 10
counter2: dl 1

