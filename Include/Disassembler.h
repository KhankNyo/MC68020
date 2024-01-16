#ifndef MC68020_DISASSEMBLER_H
#define MC68020_DISASSEMBLER_H


#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

void MC68020Disassemble(const uint8_t *Buffer, size_t BufferSize, 
    FILE *OutStream, uint32_t VirtualStartAddr, bool LittleEndian
);

#endif /* MC68020_DISASSEMBLER_H */

