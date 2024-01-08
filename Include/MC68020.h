#ifndef MC68020_H
#define MC68020_H

#include <stdint.h>


#define MC68020_REGCOUNT 16
typedef struct MC68020 MC68020;

typedef uint32_t (*MC68020Read)(MC68020 *M68k, uint32_t Addr, unsigned Size);
typedef void (*MC68020Write)(MC68020 *M68k, uint32_t Addr, uint32_t Data, unsigned Size);

typedef union MC68020Register  
{
    uint8_t Byte[4];
    uint16_t Word[2];
    uint32_t Long;
    uint32_t Addr;
} MC68020Register;

struct MC68020
{
    union {
        MC68020Register X[MC68020_REGCOUNT];
        struct {
            MC68020Register D[MC68020_REGCOUNT/2];
            MC68020Register A[MC68020_REGCOUNT/2];
        } Type;
    } Reg;
    uint32_t PC;
    uint32_t Addr;
    MC68020Register Data;

    uint32_t MemorySize;
    uint8_t *Memory;

    MC68020Read Read;
    MC68020Write Write;
};

MC68020 MC68020Init(uint8_t *Memory, size_t MemorySize);
void MC68020Execute(MC68020 *M68k);

#endif /* MC68020_H*/

