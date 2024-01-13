

addi #1, D1                         ; data reg 
addi #2, (A1)                       ; reg ind 
addi #3, (A1)+                      ; inc 
addi #3, -(A1)                      ; dec
addi #4, ($ff, A1)                  ; (D16, An)
addi #5, ($f, A1, D2.w*2)           ; (D8, An, Xn)
addi #6, ($ffffffff, A1, D3.w*2)    ; (Bd, An, Xn)
addi #7, ($ff)                      ; (Addr).W
addi #7, ($ffffffff)                ; (Addr).L

addi #8, (A1, D4.w*4)
addi #9, ([A1])
addi #10, ([A1, D5.w*4])
addi #11, ([A1, D6.w*4], $ff)
addi #12, ([A1, D7.w*4], $ffffffff)
addi #13, ([$AA, A1, A1.w*4], $ffffffff)
addi #14, ([$AAAAAAAA, A1, A1.w*4], $ffffffff)

addi #15, ([$BB, A1], D7.l*4, $ff)
