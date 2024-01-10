
#include <stdio.h>
#include "MC68020.h"


static uint8_t sMemory[10] = {
    0
};

int main(void)
{
    MC68020 m68k = MC68020Init(sMemory, sizeof sMemory, false);
    char input = 0;
    while (input = getc(stdin), 'q' != input)
    {
        MC68020Execute(&m68k);
    }
    return 0;
}

