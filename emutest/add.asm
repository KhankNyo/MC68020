

stack_size = $10
start:
    movea.w #inf + 2, sp
    adda #stack_size, sp
    move.w #$ffff, d0
    add.b ([ptr, PC]), d0
inf: bra inf
 
resv stack_size
; stack here 
stack_start:
data_sect:
ptr:
    dl precious_data
precious_data:
    db $01


