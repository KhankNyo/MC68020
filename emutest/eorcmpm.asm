
start:
    moveq #-5, d0
    eor d0, d0          ; x86 zeroing idiom (not optimal in M68k)
    movea #1, sp
        bnz.b failed    ; assert Z set
    movea.w #str1, a0
    movea.w #str2, a1

    ; while (*str1 && *str2 && *str1++ == *str2++) i++;
    moveq #-1, d1
strlencmp:                  ; get maximum length of equal parts between 2 strings
    move.b (a0), d0
    bez.b done              ; *str1 == 0?
    move.b (a1), d0
    bez.b done              ; *str2 == 0?
    cmpm.b (a0)+, (a1)+
    bne.b done              ; *str1++ != *str2++?
        addq.l #1, d1
    bra strlencmp
done:
    movea #2, sp
    cmp #12, d1             ; strlencmp(str1, str2) != 12?
    bne.b failed

    movea #0, sp    ; passed 
failed:
inf: bra inf

str1: db 'Hello, world\n', 0
str2: db 'Hello, world\n', 0

