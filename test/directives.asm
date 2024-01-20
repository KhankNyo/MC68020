
db 1, 2, 3, 4, 5, 6, 7, 8
dw 1, 2, 3, 4
dl 1, 2
dq 1

org $100
db 'hello, world!', $a, $0
org $150
start:
    BRA start

