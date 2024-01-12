

addi #1, D1                         ; data reg 
addi #2, (A1)                       ; reg ind 
addi #3, (A1)+                      ; inc 
addi #3, -(A1)                      ; dec
addi #4, ($ff, A1)                  ; (D16, An)
addi #5, ($f, A1, D2.w*2)           ; (D8, An, Xn)
addi #6, ($ffffffff, A1, D3.w*2)    ; (Bd, An, Xn)
addi #7, ($ff)                      ; (Addr).W
addi #7, ($ffffffff)                ; (Addr).L
