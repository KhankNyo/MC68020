
#include <stdio.h>
#include <stdlib.h>

#include "MC68020.h"
#include "Assembler.h"


static char *ReadFile(const char *FileName)
{
    FILE *SourceFile = fopen(FileName, "rb");
    if (NULL == SourceFile)
    {
        perror(FileName);
        exit(1);
    }

    /* get file size */
    fseek(SourceFile, 0, SEEK_END);
    size_t FileSize = ftell(SourceFile);
    fseek(SourceFile, 0, SEEK_SET);

    char *Source = malloc(FileSize + 1);
    if (NULL == Source)
    {
        fprintf(stderr, "malloc failed\n");
        exit(1);
    }

    size_t ReadSize = fread(Source, 1, FileSize, SourceFile);
    if (ReadSize != FileSize)
    {
        fprintf(stderr, "cannot read file properly (read %zu, expected %zu)\n", ReadSize, FileSize);
        exit(1);
    }
    fclose(SourceFile);

    Source[FileSize] = '\0';
    return Source;
}


int main(int argc, char **argv)
{
    if (argc < 2)
    {
        puts("Expected file name.");
        return 1;
    }
    const char *FileName = argv[1];
    char *Source = ReadFile(FileName);
    MC68020MachineCode Memory = MC68020Assemble(FileName, Source, stderr);
    if (NULL == Memory.Buffer)
    {
        return 1;
    }
    free(Source);

    MC68020 m68k = MC68020Init(Memory.Buffer, Memory.Capacity, false);
    char input = 0;
    while (input = getc(stdin), 'q' != input)
    {
        MC68020Execute(&m68k);
    }
    return 0;
}

