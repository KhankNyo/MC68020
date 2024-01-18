
start:
    blt out
inf_loop: bra inf_loop
out: bsr subroutine

halt: bra halt
    

subroutine:
    bra subroutine
