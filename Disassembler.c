
#include "Common.h"
#include "Disassembler.h"



static uint32_t DisassemblerLittleEndianRead(const uint8_t *Buffer, size_t Size)
{
    uint32_t Value = 0;
    for (unsigned i = 0; i < Size; i++)
    {
        Value |= (uint32_t)*Buffer++ << 8*i;
    }
    return Value;
}

static uint32_t DisassemblerBigEndianRead(const uint8_t *Buffer, size_t Size)
{
    uint32_t Value = 0;
    for (int i = Size - 1; i >= 0; i--)
    {
        Value |= (uint32_t)*Buffer++ << 8*i;
    }
    return Value;
}



typedef struct DisasmBuffer 
{
    const uint8_t *Buffer;
    size_t BufferSize;
    uint32_t VirtualStartAddr;
    uint32_t i;
} DisasmBuffer;

typedef struct SmallStr 
{
    char Data[64];
} SmallStr;
#define SmallStrFmt(SmlStr, ...) snprintf((SmlStr).Data, sizeof(SmallStr), __VA_ARGS__)

static const char sRegisterName[16][4] = {
    "D0", "D1", "D2", "D3", "D4", "D5", "D6", "D7", 
    "A0", "A1", "A2", "A3", "A4", "A5", "A6", "A7"
};



static uint32_t (*sReadFn)(const uint8_t *Buffer, size_t Size) = NULL;
static uint32_t CheckAndRead(DisasmBuffer *Dis, size_t Size)
{
    Dis->i += Size;
    if (Dis->i <= Dis->BufferSize)
    {
        return sReadFn(Dis->Buffer + Dis->i - Size, Size);
    }
    return 0;
}

static unsigned DisasmDecodeExtensionSize(unsigned Sz)
{
    switch (Sz & 0x3)
    {
    case 3: return 4;
    case 2: return 2;
    default:
    case 1: 
    case 0: return 0;
    }
}

static unsigned DisasmDecodeSize(unsigned Sz)
{
    switch (Sz & 0x3)
    {
    case 2: return 4;
    case 1: return 2;
    case 0: return 1;
    }
    return 0;
}

static SmallStr DisasmMemoryIndirect(DisasmBuffer *Dis, 
        const char *BaseRegister, unsigned ExtensionWord)
{
    SmallStr Ret;
    unsigned IndexReg = ExtensionWord >> 12;
    bool IndexLong = (ExtensionWord & 0x0800) != 0;
    unsigned Scale = 1 << ((ExtensionWord >> 9) & 0x3);
    if (ExtensionWord & 0x0100) /* full extension word */
    {
        unsigned IS = ExtensionWord & 0x7;
        int32_t BaseDisplacement = 0;
        /* base not suppressed */
        if (0 == (ExtensionWord & 0x0080))
        {
            unsigned BaseSize = DisasmDecodeExtensionSize(ExtensionWord >> 4);
            BaseDisplacement = CheckAndRead(Dis, BaseSize);
            if (2 == BaseSize)
                BaseDisplacement = SEX(32, 16)BaseDisplacement;
        }

        /* index suppressed? */
        if (ExtensionWord & 0x0040)
        {
            switch (IS)
            {
            case 0: /* no mem indirect */
            {
                SmallStrFmt(Ret, "(%d, %s)", 
                    BaseDisplacement,
                    BaseRegister
                );
            } break;
            case 1: /* null outer */
            {
                SmallStrFmt(Ret, "([%d, %s])", 
                    BaseDisplacement,
                    BaseRegister
                );
            } break;
            case 2: /* word outer */
            {
                int32_t OuterDisplacement = SEX(32, 16)CheckAndRead(Dis, 2);
                SmallStrFmt(Ret, "([%d, %s], %d)", 
                    BaseDisplacement,
                    BaseRegister,
                    OuterDisplacement
                );
            } break;
            case 3: /* long outer */
            {
                int32_t OuterDisplacement = CheckAndRead(Dis, 4);
                SmallStrFmt(Ret, "([%d, %s], %d)", 
                    BaseDisplacement,
                    BaseRegister,
                    OuterDisplacement
                );
            } break;
            default: 
            {
                SmallStrFmt(Ret, "???");
            } break;
            }
        }
        else
        {
            switch (IS)
            {
            case 0: /* no mem indirect */
            {
                SmallStrFmt(Ret, "(%d, %s, %s.%c*%u)", 
                    BaseDisplacement,
                    BaseRegister,
                    sRegisterName[IndexReg], 
                    IndexLong? 'l' : 'w', Scale
                );
            } break;
            case 1: /* pre index, null outer */
            {
                SmallStrFmt(Ret, "([%d, %s, %s.%c*%u])", 
                    BaseDisplacement,
                    BaseRegister,
                    sRegisterName[IndexReg], 
                    IndexLong? 'l' : 'w', Scale
                );
            } break;
            case 2: /* pre index, word outer */
            {
                int32_t OuterDisplacement = SEX(32, 16)CheckAndRead(Dis, 2);
                SmallStrFmt(Ret, "([%d, %s, %s.%c*%u], %d)", 
                    BaseDisplacement,
                    BaseRegister,
                    sRegisterName[IndexReg], 
                    IndexLong? 'l' : 'w', Scale, 
                    OuterDisplacement
                );
            } break;
            case 3: /* pre index, long outer */
            {
                int32_t OuterDisplacement = CheckAndRead(Dis, 4);
                SmallStrFmt(Ret, "([%d, %s, %s.%c*%u], %d)", 
                    BaseDisplacement,
                    BaseRegister,
                    sRegisterName[IndexReg], 
                    IndexLong? 'l' : 'w', Scale, 
                    OuterDisplacement
                );
            } break;
            case 4: /* reserved */
            {
                SmallStrFmt(Ret, "???");
            } break;
            case 5: /* post index, null outer */
            {
                SmallStrFmt(Ret, "([%d, %s], %s.%c*%u)", 
                    BaseDisplacement,
                    BaseRegister,
                    sRegisterName[IndexReg], 
                    IndexLong? 'l' : 'w', Scale
                );
            } break;
            case 6: /* post index, word outer */
            {
                int32_t OuterDisplacement = SEX(32, 16)CheckAndRead(Dis, 2);
                SmallStrFmt(Ret, "([%d, %s], %s.%c*%u, %d)", 
                    BaseDisplacement,
                    BaseRegister,
                    sRegisterName[IndexReg], 
                    IndexLong? 'l' : 'w', Scale, 
                    OuterDisplacement
                );
            } break;
            default:
            case 7: /* post index, long outer */
            {
                int32_t OuterDisplacement = CheckAndRead(Dis, 4);
                SmallStrFmt(Ret, "([%d, %s], %s.%c*%u, %d)", 
                    BaseDisplacement,
                    BaseRegister,
                    sRegisterName[IndexReg], 
                    IndexLong? 'l' : 'w', Scale, 
                    OuterDisplacement
                );
            } break;
            }
        }
    }
    else /* brief extension word */
    {
        int32_t Displacement = (int32_t)(int8_t)(ExtensionWord & 0xff);
        SmallStrFmt(Ret, "(%d, %s, %s.%c*%u)", 
            Displacement, 
            BaseRegister,
            sRegisterName[IndexReg], IndexLong? 'l': 'w',
            Scale
        );
    }
    return Ret;
}


static SmallStr DisasmModeReg(DisasmBuffer *Dis, unsigned Mode, unsigned Reg, unsigned OperandSize)
{
    SmallStr Ret;
    Mode &= 0x7;
    Reg &= 0x7;
    switch (Mode)
    {
    case 0: SmallStrFmt(Ret, "%s", sRegisterName[Reg]); break;
    case 1: SmallStrFmt(Ret, "%s", sRegisterName[Reg + 8]); break;
    case 2: SmallStrFmt(Ret, "(%s)", sRegisterName[Reg + 8]); break;
    case 3: SmallStrFmt(Ret, "(%s)+", sRegisterName[Reg + 8]); break;
    case 4: SmallStrFmt(Ret, "-(%s)", sRegisterName[Reg + 8]); break;
    case 5: 
    {
        int32_t Displacement = SEX(32, 16)CheckAndRead(Dis, 2);
        SmallStrFmt(Ret, "(%d, %s)", Displacement, sRegisterName[Reg + 8]);
    } break;
    case 6:
        return DisasmMemoryIndirect(Dis, sRegisterName[Reg + 8], CheckAndRead(Dis, 2));
    default:
    case 7:
    {
        switch (Reg)
        {
        case 0:
        {
            int32_t Address = SEX(32, 16)CheckAndRead(Dis, 2);
            SmallStrFmt(Ret, "($%x).w", Address);
        } break;
        case 1:
        {
            int32_t Address = CheckAndRead(Dis, 4);
            SmallStrFmt(Ret, "($%x).l", Address);
        } break;
        case 2:
        {
            int32_t Displacement = SEX(32, 16)CheckAndRead(Dis, 2);
            SmallStrFmt(Ret, "(%d, PC)", Displacement);
        } break;
        case 3:
            return DisasmMemoryIndirect(Dis, "PC", CheckAndRead(Dis, 2));
        case 4:
        {
            unsigned Size = uMax(OperandSize, 2);
            int32_t Immediate = CheckAndRead(Dis, Size);
            if (2 == Size)
                Immediate = SEX(32, 16)Immediate;
            SmallStrFmt(Ret, "#%d", Immediate);
        } break;
        default:
        {
            SmallStrFmt(Ret, "???");
        } break;
        }
    } break;
    }
    return Ret;
}

static const char *DisasmRealSize(unsigned DataSize)
{
    switch (DataSize)
    {
    case 1: return ".b";
    case 2: return ".w"; 
    case 4: return ".l";
    default: return "";
    }
}

static const char *DisasmEncodedSize(unsigned SizeEncoding)
{
    switch (SizeEncoding)
    {
    case 0: return ".b";
    case 1: return ".w";
    case 2: return ".l";
    default: return ".?";
    }
}

static SmallStr DisasmMove(DisasmBuffer *Dis, uint32_t Opcode, uint32_t DataSize)
{
    SmallStr Src = DisasmModeReg(Dis, Opcode >> 3, Opcode >> 0, DataSize);
    SmallStr Dst = DisasmModeReg(Dis, Opcode >> 6, Opcode >> 9, DataSize);
    const char *Size = DisasmRealSize(DataSize);
    const char *Mnemonic = 01 == ((Opcode >> 6) & 0x7)?
        "movea" : "move";
    SmallStr Ret;
    SmallStrFmt(Ret, "%s%s %s, %s", Mnemonic, Size, Src.Data, Dst.Data);
    return Ret;
}


static void PrintAddr(FILE *f, uint32_t Addr)
{
    fprintf(f, "\n%08x:  ", Addr);
}

static void PrintBytesAndMnemonic(FILE *f, const DisasmBuffer *Dis, 
    uint32_t OpcodeIndex, unsigned BytesPerLine, const char *Instruction)
{
    uint32_t FirstLine = uMin(OpcodeIndex + BytesPerLine, Dis->i);
    uint32_t i = OpcodeIndex;
    for (; i < FirstLine; i++)
    {
        fprintf(f, "%02x ", Dis->Buffer[i]);
    }

    fprintf(f, "%*s    %s", 3*(BytesPerLine - (i - OpcodeIndex)), "", 
        Instruction
    );

    uint32_t BytesCurrentLine = 0;
    for (; i < Dis->i; i++)
    {
        if (BytesCurrentLine % BytesPerLine == 0)
        {
            PrintAddr(f, i + Dis->VirtualStartAddr);
            BytesCurrentLine = 0;
        }
        BytesCurrentLine++;
        fprintf(f, "%02x ", Dis->Buffer[i]);
    }
}

static const char *DisasmConditionalCode(unsigned CC)
{
    static const char ConditionalCodeLut[16][4] = {
        "t", "f", "hi", "ls",
        "cc", "cs", "ne", "eq",
        "vc", "vs", "pl", "mi",
        "ge", "lt", "gt", "le"
    };
    return ConditionalCodeLut[CC & 0xF];
}


static SmallStr DisasmPreDecOrDn(const char *Mnemonic, uint16_t Opcode)
{
    SmallStr Ret;
    unsigned LeftReg = (Opcode >> 9) & 07, 
             RightReg = (Opcode & 07);
    if (Opcode & 0x0008) /* -(An) */
    {
        SmallStrFmt(Ret, "%s -(%s), -(%s)", 
            Mnemonic, 
            sRegisterName[RightReg + 8], sRegisterName[LeftReg + 8]
        );
    }
    else /* Dn */
    {
        SmallStrFmt(Ret, "%s %s, %s", 
            Mnemonic, 
            sRegisterName[RightReg], sRegisterName[LeftReg]
        );
    }
    return Ret;
}

void MC68020Disassemble(const uint8_t *Buffer, size_t BufferSize,
        FILE *f, uint32_t VirtualStartAddr, bool LittleEndian)
{
    DisasmBuffer Dis = {
        .i = 0,
        .Buffer = Buffer, 
        .BufferSize = BufferSize,
        .VirtualStartAddr = VirtualStartAddr,
    };
    sReadFn = LittleEndian? DisassemblerLittleEndianRead : DisassemblerBigEndianRead;

    while (Dis.i + 1 < BufferSize)
    {
        uint32_t OpcodeAddr = Dis.i;
        PrintAddr(f, OpcodeAddr + VirtualStartAddr);
        uint16_t Opcode = CheckAndRead(&Dis, 2);

        SmallStr Instruction = { 0 };
        switch (Opcode >> 12)
        {
        case 0:
        {
            unsigned Mode = 07 & (Opcode >> 3);
            unsigned Reg = Opcode & 07;
            if (Opcode & 0x0100) /* bit with register argument */
            {
                const char *Mnemonic = "???";
                unsigned Size = (Mode & 0x7) == 0? 4: 1; /* long if Dn, else byte */
                unsigned Op = Opcode >> 6;

                switch (Op & 0x3)
                {
                case 0: /* btst */ Mnemonic = "btst"; break;
                case 1: /* bchg */ Mnemonic = "bchg"; break;
                case 2: /* bclr */ Mnemonic = "bclr"; break;
                case 3: /* bset */ Mnemonic = "bset"; break;
                }
                if (01 == Mode) /* no An */
                    Mnemonic = "???";

                SmallStr Arg = DisasmModeReg(&Dis, Mode, Reg, Size);
                const char *ShiftRegister = sRegisterName[(Opcode >> 9) & 0x7];
                SmallStrFmt(Instruction, "%s%s %s, %s", Mnemonic, DisasmRealSize(Size), ShiftRegister, Arg.Data);
            }
            else
            {
                unsigned Op = Opcode >> 9;
                Op &= 7;
                if (4 == Op) /* bit immediate */
                {
                    uint8_t ShiftByte = CheckAndRead(&Dis, 2);
                    unsigned Size = (Mode & 0x7) == 0? 4: 1; /* long if Dn, else byte */
                    const char *Mnemonic = "???";
                    unsigned BitOp = Opcode >> 6;

                    switch (BitOp & 0x3)
                    {
                    case 0: /* btst */ Mnemonic = "btst"; break;
                    case 1: /* bchg */ Mnemonic = "bchg"; break;
                    case 2: /* bclr */ Mnemonic = "bclr"; break;
                    case 3: /* bset */ Mnemonic = "bset"; break;
                    }
                    if (01 == Mode || (07 == Mode && 04 == Reg))
                        Mnemonic = "???";

                    SmallStr Arg = DisasmModeReg(&Dis, Mode, Reg, Size);
                    SmallStrFmt(Instruction, "%s%s #%u, %s", Mnemonic, DisasmRealSize(Size), ShiftByte, Arg.Data);
                }
                else
                {
                    unsigned Size = Opcode >> 6;
                    Size &= 0x3;

                    Mode &= 07;
                    SmallStr Operand;
                    uint32_t Immediate;
                    if (07 == Mode && 04 == Reg)
                    {
                        Operand = Size == 0
                            ? (SmallStr){"CCR"} 
                            : (SmallStr){"SR"};
                        Immediate = CheckAndRead(&Dis, 2);
                    }
                    else
                    {
                        Immediate = CheckAndRead(&Dis, uMax(DisasmDecodeSize(Size), 2));
                        Operand = DisasmModeReg(&Dis, Mode, Reg, DisasmDecodeSize(Size));
                    }
                    const char *Mnemonic = "???";
                    switch ((Opcode >> 9) & 0x7)
                    {
                    case 0: /* ori  */ Mnemonic = "ori"; break;
                    case 1: /* andi */ Mnemonic = "andi"; break;
                    case 2: /* subi */ Mnemonic = "subi"; break;
                    case 3: /* addi */ Mnemonic = "addi"; break;
                    case 5: /* eori */ Mnemonic = "eori"; break;
                    case 6: /* cmpi */ Mnemonic = "cmpi"; break;
                    case 7: /* unkown */ break;
                    }
                    SmallStrFmt(Instruction, "%s%s %d, %s", 
                        Mnemonic, DisasmEncodedSize(Size), Immediate, Operand.Data
                    );
                }
            }
        } break;
        case 1: Instruction = DisasmMove(&Dis, Opcode, sizeof(uint8_t)); break;
        case 2: Instruction = DisasmMove(&Dis, Opcode, sizeof(uint32_t)); break;
        case 3: Instruction = DisasmMove(&Dis, Opcode, sizeof(uint16_t)); break;
        case 4:
        {
        } break;
        case 5:
        {
            unsigned Mode = (Opcode >> 3) & 07;
            unsigned Reg = Opcode & 07;
            unsigned Size = Opcode >> 6;
            Size &= 03;
            if ((Opcode & 0xF0F8) == 0x50C8) /* DBcc */
            {
                int32_t BranchOffset = SEX(32, 16)CheckAndRead(&Dis, 2);
                uint32_t Location = VirtualStartAddr + OpcodeAddr + 2 + BranchOffset;
                const char *Cond = DisasmConditionalCode(Opcode >> 8);
                SmallStrFmt(Instruction, "db%s %s, %x", Cond, sRegisterName[Reg], Location);
            }
            else if (03 == Size) /* Scc */
            {
                const char *Cond = DisasmConditionalCode(Opcode >> 8);
                SmallStr Arg = DisasmModeReg(&Dis, Mode, Reg, 1);
                SmallStrFmt(Instruction, "s%s.b %s", Cond, Arg.Data);
            }
            else 
            {
                const char *Mnemonic = Opcode & 0x0100? "subq" : "addq";
                unsigned Data = 07 & (Opcode >> 9);
                if (Data == 0)
                    Data = 8;
                SmallStr Arg = DisasmModeReg(&Dis, Mode, Reg, DisasmDecodeSize(Size));
                SmallStrFmt(Instruction, "%s%s %s, %u", Mnemonic, DisasmEncodedSize(Size), Arg.Data, Data);
            }
        } break;
        case 6: /* BSR, Bcc, BRA */
        {
            int32_t Offset = SEX(32, 8)(Opcode & 0xFF);
            if (0 == Offset)
                Offset = CheckAndRead(&Dis, 2);
            else if (-1 == Offset)
                Offset = CheckAndRead(&Dis, 4);
            uint32_t Location = OpcodeAddr + 2 + VirtualStartAddr + Offset;

            if ((Opcode & 0x0F00) == 0x0100) /* BSR */
            {
                SmallStrFmt(Instruction, "bsr $%x", Location);
            }
            else if (0 == (Opcode & 0x0F00)) /* BRA */
            {
                SmallStrFmt(Instruction, "bra $%x", Location);
            }
            else /* bcc */
            {
                const char *ConditionalCode = DisasmConditionalCode(Opcode >> 8);
                SmallStrFmt(Instruction, "b%s $%x", ConditionalCode, Location);
            }
        } break;
        case 7: /* MOVEQ */
        {
            int32_t Immediate = SEX(32, 8)(Opcode & 0xFF);
            const char *Mnemonic = Opcode & 0x0100?
                "???" : "moveq";
            unsigned Dst = (Opcode >> 9) & 07;
            SmallStrFmt(Instruction, "%s.l %d, %s", Mnemonic, Immediate, sRegisterName[Dst]);
        } break;
        case 8:
        {
            unsigned LeftReg = (Opcode >> 9) & 07;
            if ((Opcode & 0xC0) == 0xC0) /* DIVU/S */
            {
                unsigned Mode = Opcode >> 3, Reg = Opcode;
                const char *Mnemonic = Opcode & 0x0100? "divs": "divu";
                SmallStr Src = DisasmModeReg(&Dis, Mode, Reg, 2);
                SmallStrFmt(Instruction, "%s.w %s, %s", Mnemonic, Src.Data, sRegisterName[LeftReg]);
            }
            else if ((Opcode & 0x01F0) == 0x0100) /* SBCD */
            {
                Instruction = DisasmPreDecOrDn("sbcd.b", Opcode);
            }
            else /* OR */
            {
                unsigned Mode = Opcode >> 3, 
                         Reg = Opcode, 
                         Size = 03 & (Opcode >> 6);
                SmallStr Ea = DisasmModeReg(&Dis, Mode, Reg, DisasmDecodeSize(Size));
                const char *Dst = sRegisterName[LeftReg];
                const char *Src = Ea.Data;
                if (Opcode & 0x0100) /* direction bit: ea | d -> ea */
                {
                    Dst = Ea.Data;
                    Src = sRegisterName[LeftReg];
                }

                SmallStrFmt(Instruction, "or%s %s, %s", DisasmEncodedSize(Size), Src, Dst);
            }
        } break;
        case 13: /* ADD, ADDA, ADDX */
        case 9: /* SUB, SUBA, SUBX */
        {
            unsigned Mode = Opcode >> 3, 
                     Reg = Opcode;
            unsigned LeftReg = (Opcode >> 9) & 07;
            unsigned Size = (Opcode >> 6) & 03;

            const char *Op = (Opcode >> 12) == 9? "sub": "add";
            if (03 == Size) /* A */
            {
                unsigned Size = Opcode & 0x0100? 4: 1;
                SmallStr Src = DisasmModeReg(&Dis, Mode, Reg, Size);
                const char *Dst = sRegisterName[LeftReg + 8];
                const char SizeChr = Size == 4? 'l': 'w';
                SmallStrFmt(Instruction, "%sa.%c %s, %s", Op, SizeChr, Src.Data, Dst);
            }
            else if ((Opcode & 0x0130) == 0x0100) /* X */
            {
                SmallStr Mnemonic;
                SmallStrFmt(Mnemonic, "%sx%s", Op, DisasmEncodedSize(Size));
                Instruction = DisasmPreDecOrDn(Mnemonic.Data, Opcode);
            }
            else /* normal */
            {
                SmallStr Ea = DisasmModeReg(&Dis, Mode, Reg, DisasmDecodeSize(Size));
                const char *Dst = sRegisterName[LeftReg];
                const char *Src = Ea.Data;
                if (Opcode & 0x0100)
                {
                    Dst = Ea.Data;
                    Src = sRegisterName[LeftReg];
                }
                SmallStrFmt(Instruction, "%s%s %s, %s", Op, DisasmEncodedSize(Size), Src, Dst);
            }
        } break;
        case 10:
        {
            SmallStrFmt(Instruction, "???");
        } break;
        case 11: /* EOR, CMPM, CMP, CMPA */
        {
            unsigned Size = 03 & (Opcode >> 6);
            unsigned LeftReg = 07 & (Opcode >> 9), 
                     Reg = 07 & Opcode,
                     Mode = 07 & (Opcode >> 3);
            if (03 == Size) /* cmpa */
            {
                Size = Opcode & 0x0100? 4: 2;
                SmallStr Src = DisasmModeReg(&Dis, Mode, Reg, Size);
                SmallStrFmt(Instruction, "cmpa%s %s, %s", DisasmRealSize(Size), Src.Data, sRegisterName[LeftReg + 8]);
            }
            else if (Opcode & 0x0100) /* eor, cmpm */
            {
                if (01 == Mode) /* cmpm */
                {
                    SmallStrFmt(Instruction, "cmpm%s (%s)+, (%s)+", 
                        DisasmEncodedSize(Size), sRegisterName[Reg + 8], sRegisterName[LeftReg + 8]
                    );
                }
                else /* eor */
                {
                    SmallStr Src = DisasmModeReg(&Dis, Mode, Reg, DisasmDecodeSize(Size));
                    SmallStrFmt(Instruction, "eor%s %s, %s", DisasmEncodedSize(Size), sRegisterName[LeftReg], Src.Data);
                }
            }
            else /* cmp */
            {
                SmallStr Src = DisasmModeReg(&Dis, Mode, Reg, DisasmDecodeSize(Size));
                SmallStrFmt(Instruction, "cmp%s %s, %s", DisasmEncodedSize(Size), Src.Data, sRegisterName[LeftReg]);
            }
        } break;
        case 12: /* MULU/S, ABCD, EXG, AND */
        {
            unsigned LeftReg = (Opcode >> 9) & 07,
                     Mode = (Opcode >> 3) & 07,
                     Reg = Opcode & 07,
                     Size = (Opcode >> 6) & 03;
            if (03 == Size) /* MUL */
            {
                const char *Mnemonic = (Opcode & 0x0100)? "muls": "mulu";
                SmallStr Src = DisasmModeReg(&Dis, Mode, Reg, 2);
                SmallStrFmt(Instruction, "%s.w %s, %s", Mnemonic, Src.Data, sRegisterName[LeftReg]);
            }
            else if (Opcode & 0x0100) /* AND Dn, <ea>; ABCD; EXG */
            {
                if (Mode <= 1) /* ABCD, EXG */
                {
                    switch (Size)
                    {
                    case 0: /* ABCD */
                    {
                        Instruction = DisasmPreDecOrDn("abcd.b", Opcode);
                    } break;
                    case 1: /* exg Dx, Dy; exg Ax, Ay */
                    {
                        const char *Src = Opcode & 0x0008? 
                            sRegisterName[LeftReg + 8] : sRegisterName[LeftReg];
                        const char *Dst = Opcode & 0x0008? 
                            sRegisterName[Reg + 8] : sRegisterName[Reg];
                        SmallStrFmt(Instruction, "exg.l %s, %s", Src, Dst);
                    } break;
                    case 2: /* exg Dx, Ay */
                    {
                        SmallStrFmt(Instruction, "exg.l %s, %s", sRegisterName[LeftReg], sRegisterName[Reg + 8]);
                    } break;
                    default:
                    {
                        Instruction = (SmallStr){ "???" };
                    } break;
                    }
                }
                else /* AND Dn, <ea> */
                {
                    SmallStr Ea = DisasmModeReg(&Dis, Mode, Reg, DisasmDecodeSize(Size));
                    const char *Dst = Ea.Data;
                    const char *Src = sRegisterName[LeftReg];
                    SmallStrFmt(Instruction, "and%s %s, %s", DisasmEncodedSize(Size), Src, Dst);
                }
            }
            else /* AND <ea>, Dn */
            {
                SmallStr Ea = DisasmModeReg(&Dis, Mode, Reg, DisasmDecodeSize(Size));
                const char *Dst = sRegisterName[LeftReg];
                const char *Src = Ea.Data;
                SmallStrFmt(Instruction, "and%s %s, %s", DisasmEncodedSize(Size), Src, Dst);
            }
        } break;
        default:
        {
            SmallStrFmt(Instruction, "???");
        } break;
        }

        PrintBytesAndMnemonic(f, &Dis, OpcodeAddr, 4, Instruction.Data);
    }
}

