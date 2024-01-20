#ifndef MC68020_DISASSEMBLER_H
#define MC68020_DISASSEMBLER_H


#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

typedef struct SmallStr 
{
    char Data[128];
} SmallStr;
uint32_t MC68020DisassembleSingleInstruction(const void *Buffer, size_t BufferSizeBytes, 
    uint32_t VirtualAddr, bool LittleEndian, SmallStr *Out
);
void MC68020Disassemble(const void *Buffer, size_t BufferSizeBytes, 
    uint32_t VirtualAddr, bool LittleEndian, FILE *OutStream
);

#endif /* MC68020_DISASSEMBLER_H */

