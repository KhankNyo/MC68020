#ifndef MC68020_EMULATOR_H
#define MC68020_EMULATOR_H

#include "Common.h"

typedef struct MC68020 MC68020;
typedef void (*MC68020WriteFn)(MC68020 *, uint32_t Addr, uint32_t Data, unsigned Size);
typedef uint32_t (*MC68020ReadFn)(MC68020 *, uint32_t Addr, unsigned Size);

struct MC68020 
{
    int32_t R[16];
    uint32_t PC;
    uint16_t SR;
    uint16_t Opcode;

    uint8_t *Memory;
    uint32_t MemorySize;
    MC68020ReadFn Read;
    MC68020WriteFn Write;
};

MC68020 MC68020Init(uint8_t *Memory, uint32_t MemorySize, bool IsLittleEndian);
void MC68020Execute(MC68020 *M68k);



#endif /* MC68020_EMULATOR_H */

