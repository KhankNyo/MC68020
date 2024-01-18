

mem_location = $4000
start:
    asr.b d1, d2
    asl.l #5, d2
    lsl d2
    lsr d1
    ror (SP)
    roxl ([SP], d0.w*4, mem_location)

