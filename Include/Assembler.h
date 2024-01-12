#ifndef MC68020_ASSEMBLER_H
#define MC68020_ASSEMBLER_H


#include <stdio.h>
#include <stdbool.h>

typedef struct MC68020MachineCode 
{
    unsigned char *Buffer;
    size_t Size;
    size_t Capacity;
} MC68020MachineCode;

/* Source must be null-terminated,
 * Buffer will be NULL if an error was encountered,
 * if ErrorStream is NULL, no error will be emitted */
MC68020MachineCode MC68020Assemble(
        const char *SourceName, const char *Source, 
        bool LittleEndian, FILE *ErrorStream
);



#endif /* MC68020_ASSEMBLER_H */

