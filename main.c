
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "MC68020Isa.h"
#include "Emulator.h"
#include "Assembler.h"
#include "Disassembler.h"


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


static int GetInput(void)
{
    printf("\npress enter to cont ...");
    int Input = getc(stdin);
    return Input;
}


static void UpdateDebugPC(const void *Memory, size_t MemSize, 
    uint32_t PC, bool LittleEndian)
{
#define INS_BUF_SIZE 8
    static SmallStr InstructionBuffer[INS_BUF_SIZE] = { 0 };
    static uint32_t InstructionAddr[INS_BUF_SIZE] = { 0 };
    bool PCOutOfRange = !IN_RANGE(InstructionAddr[0], PC, InstructionAddr[7]);
    if (PC == 0 || PCOutOfRange)
    {
        /* update all of the instruction in the buffer */
        const uint8_t *At = Memory;
        uint32_t InsAddr = PC;
        for (unsigned i = 0; i < STATIC_ARRAY_SIZE(InstructionBuffer); i++)
        {
            int64_t SizeLeft = (int64_t)MemSize - (int64_t)PC;
            if (SizeLeft < 1)
            {
                InstructionBuffer[i] = (SmallStr) { "---" };
            }
            else
            {
                InstructionAddr[i] = InsAddr;
                InsAddr += MC68020DisassembleSingleInstruction(
                    At + InsAddr, SizeLeft, 
                    InsAddr, LittleEndian, &InstructionBuffer[i]
                );
            }
        }
    }

    /* print the instruction buffer */
    for (unsigned i = 0; i < STATIC_ARRAY_SIZE(InstructionBuffer); i++)
    {
        const char *Pointer = PC == InstructionAddr[i]? " > ": "   ";
        printf("%8x:%s%s\n", InstructionAddr[i], Pointer, InstructionBuffer[i].Data);
    }
#undef INS_BUF_SIZE
}

static void DumpState(const MC68020 *M68k)
{
    UpdateDebugPC(M68k->Memory, M68k->MemorySize, M68k->PC, false);

    /* print flags, upper means active */
    char FlagLut[] = "xnzvc";
    for (int i = FLAG_C; i <= FLAG_X; i++)
    {
        if (M68k->SR & (1 << i))
            FlagLut[FLAG_X - i] = TO_UPPER(FlagLut[FLAG_X - i]);
    }
    printf("\nSR: %04x: %s", M68k->SR, FlagLut);

    for (int i = 0; i < (int)STATIC_ARRAY_SIZE(M68k->R); i++) 
    {
        char RegName = i >= 8? 'A': 'D';
        if (i % 4 == 0)
            printf("\n  ");
        printf("%c%d|  0x%08x  |", RegName, i % 8, M68k->R[i]);
    }
}

static void LogData(const char *Mode, const MC68020 *M68k, uint32_t Addr, uint32_t Data, unsigned DataSize)
{
    printf("\n%s: [%08x]"
            "\n    data: %u"
            "\n    raw:  0x%08x"
            "\n    size: %d\n", 
            Mode, Addr, 
                Data, 
                Data, 
                DataSize
    );
    GetInput();
}

static uint32_t MemRead(MC68020 *M68k, uint32_t Addr, unsigned Size)
{
    uint32_t Data = 0;
    if (Addr + Size > M68k->MemorySize && Addr < M68k->MemorySize)
        return 0;

    for (int i = Size - 1; i >= 0; i--)
        Data |= M68k->Memory[Addr++] << i*8;
    //LogData("Reading", M68k, Addr - Size, Data, Size);
    return Data;
}

static void MemWrite(MC68020 *M68k, uint32_t Addr, uint32_t Data, uint32_t Size)
{
    if (Addr + Size > M68k->MemorySize && Addr < M68k->MemorySize)
        return;

    for (int i = Size - 1; i >= 0; i--)
        M68k->Memory[Addr++] = Data >> i*8;
    //LogData("Writing", M68k, Addr - Size, Data, Size);
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
    bool LittleEndian = false;

    MC68020MachineCode Memory = MC68020Assemble(realloc, FileName, Source, LittleEndian, stderr);
    free(Source);
    if (NULL == Memory.Buffer)
    {
        return 1;
    }
    MC68020Disassemble(Memory.Buffer, Memory.Size, 0, LittleEndian, stdout);


    MC68020 M68k = MC68020Init(Memory.Buffer, Memory.Size, LittleEndian);
    M68k.Read = MemRead;
    M68k.Write = MemWrite;
    while ('q' != GetInput())
    {
        MC68020Execute(&M68k);
        DumpState(&M68k);
    }

    free(Memory.Buffer);
    return 0;
}

