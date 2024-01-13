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


/* this function must act exactly like realloc:
 *  returns a pointer to a newly allocated buffer or NULL:
 *      (NULL, size); size != 0
 *  returns NULL and free the given pointer:
 *      (ptr, 0); ptr can be NULL
 *  returns a new buffer with the given size or NULL:
 *      (ptr, size); size != 0, ptr != NULL
 * */
typedef void *(*AllocatorFn)(void *, size_t);

/* Allocator is used to allocate the Buffer of MC68020MachineCode structure
 * Source must be null-terminated,
 * Buffer will be NULL if an error was encountered,
 * if ErrorStream is NULL, no error will be emitted */
MC68020MachineCode MC68020Assemble(AllocatorFn Allocator,
        const char *SourceName, const char *Source, 
        bool LittleEndian, FILE *ErrorStream
);



#endif /* MC68020_ASSEMBLER_H */

