

#include "MC68020Isa.h"
#include "Emulator.h"


typedef enum LocationType 
{
    LOC_ADDR,
    LOC_REG,
    LOC_IMM,
} LocationType;

typedef struct DataLocation
{
    LocationType Type;
    union {
        uint32_t EffectiveAddr;
        unsigned RegisterIndex;
        uint32_t Immediate;
    } As;
} DataLocation;

#define SET_FLAG(pM68k, flFlag, u32Data) \
    ((pM68k)->SR = ((pM68k)->SR & ~(1ul << flFlag)) | ((uint16_t)(0 != (u32Data)) << flFlag))
#define GET_TRUTHY_FLAG(pM68k, flFlag)\
    ((pM68k)->SR & (1 << flFlag))
#define GET_FLAG(pM68k, flFlag) \
    (((pM68k)->SR & (1 << flFlag)) != 0)
#define FETCH_OPCODE(pM68k) ((pM68k)->Opcode = FetchImmediate(pM68k, 2))
    


static uint32_t DefaultReadLittleEndian(MC68020 *M68k, uint32_t Addr, unsigned Size)
{
    uint32_t Data = 0;
    if (Addr + Size > M68k->MemorySize && Addr < M68k->MemorySize)
        return 0;

    for (unsigned i = 0; i < Size; i++)
    {
        Data |= (uint32_t)M68k->Memory[Addr + i] << i*8;
    }
    return Data;
}

static uint32_t DefaultReadBigEndian(MC68020 *M68k, uint32_t Addr, unsigned Size)
{
    uint32_t Data = 0;
    if (Addr + Size > M68k->MemorySize && Addr < M68k->MemorySize)
        return 0;

    for (int i = Size - 1; i >= 0; i--)
    {
        Data |= (uint32_t)M68k->Memory[Addr + i] << i*8;
    }
    return Data;
}

static void DefaultWriteLittleEndian(MC68020 *M68k, uint32_t Addr, uint32_t Data, unsigned Size)
{
    if (Addr + Size > M68k->MemorySize && Addr < M68k->MemorySize)
        return;
    for (unsigned i = 0; i < Size; i++)
    {
        M68k->Memory[Addr + i] = Data >> i*i;
    }
}

static void DefaultWriteBigEndian(MC68020 *M68k, uint32_t Addr, uint32_t Data, unsigned Size)
{
    if (Addr + Size > M68k->MemorySize && Addr < M68k->MemorySize)
        return;
    for (int i = Size - 1; i >= 0; i--)
    {
        M68k->Memory[Addr + i] = Data >> i*8;
    }
}


MC68020 MC68020Init(uint8_t *Memory, uint32_t MemorySize, bool IsLittleEndian)
{
    MC68020 M68k = {
        .Memory = Memory,
        .MemorySize = MemorySize,
        .Read = IsLittleEndian? DefaultReadLittleEndian: DefaultReadBigEndian,
        .Write = IsLittleEndian? DefaultWriteLittleEndian: DefaultWriteBigEndian,
    };
    return M68k;
}

static uint32_t FetchImmediate(MC68020 *M68k, unsigned Size)
{
    uint32_t Data = M68k->Read(M68k, M68k->PC, Size);
    M68k->PC += Size;
    return Data;
}

static int32_t FetchExtensionImmediate(MC68020 *M68k, unsigned EncodedSize)
{
    switch (EncodedSize & 03)
    {
    default:
    case 0:
    case 1: return 0;
    case 2: return SEX(32, 16)FetchImmediate(M68k, 2);
    case 3: return (int32_t)FetchImmediate(M68k, 4);
    }
}

static DataLocation GetExtensionLocation(MC68020 *M68k, uint32_t BaseRegister)
{
    DataLocation Location = {
        .Type = LOC_ADDR,
    };
    unsigned Extension = FetchImmediate(M68k, 2);
    if (Extension & 0x0100) /* Full extension? */
    {
        int32_t Index = 0;
        if (0 == (Extension & 0x0040)) /* Index is not suppressed? */
        {
            Index = M68k->R[Extension >> 12];
            if (Extension & 0x0800) /* Index word? */
                Index = SEX(32, 16)Index;
            Index <<= (Extension >> 9) & 03;
        }

        int32_t BaseDisplacement = 0;
        if (0 == (Extension & 0x0080)) /* Base is not suppressed? */
            BaseDisplacement = FetchExtensionImmediate(M68k, Extension >> 4);

        /* calculate effective addr */
        Location.As.EffectiveAddr = Extension & 04?
            BaseDisplacement + BaseRegister + Index 
            : BaseDisplacement + BaseRegister;

        if ((Extension & 03) >= 1) /* Memory indirect? */
        {
            Location.As.EffectiveAddr = M68k->Read(M68k, Location.As.EffectiveAddr, 4);
            Location.As.EffectiveAddr += FetchExtensionImmediate(M68k, Extension);
        }
    }
    else
    {
        int32_t IndexRegister = M68k->R[Extension >> 12];
        if (Extension & 0x0800) /* Index word? */
            IndexRegister = SEX(32, 16)IndexRegister;
        IndexRegister <<= (Extension >> 9) & 03;

        int32_t Displacement = SEX(32, 8)Extension;
        Location.As.EffectiveAddr = BaseRegister + IndexRegister + Displacement;
    }
    return Location;
}

static DataLocation GetLocation(MC68020 *M68k, unsigned Mode, unsigned Reg, unsigned Size)
{
    DataLocation Location = {
        .Type = LOC_ADDR,
    };
    Mode &= 07;
    Reg &= 07;
    switch ((MC68020Mode)Mode)
    {
    case MODE_DN: /* Dn */
    {
        Location.Type = LOC_REG;
        Location.As.RegisterIndex = Reg;
    } break;
    case MODE_AN: /* An */
    {
        Location.Type = LOC_REG;
        Location.As.RegisterIndex = Reg + 8;
    } break;
    case MODE_IND: /* (An) */
    {
        Location.As.EffectiveAddr = M68k->R[Reg + 8];
    } break;
    case MODE_POSTINC: /* (An)+ */
    {
        Location.As.EffectiveAddr = M68k->R[Reg + 8];
        M68k->R[Reg + 8] += Size;
    } break;
    case MODE_PREDEC: /* -(An) */
    {
        M68k->R[Reg + 8] -= Size;
        Location.As.EffectiveAddr = M68k->R[Reg + 8];
    } break;
    case MODE_IND_I16: /* (D16, An) */
    {
        int32_t Displacement = SEX(32, 16)FetchImmediate(M68k, 2);
        Location.As.EffectiveAddr = M68k->R[Reg + 8] + Displacement;
    } break;
    case MODE_INDEX: /* Index */
    {
        Location = GetExtensionLocation(M68k, M68k->R[Reg + 8]);
    } break;
    case MODE_SPECIAL: /* PC rel, imm and abs */
    {
        uint32_t PC = M68k->PC;
        switch (Reg)
        {
        /* abs */
        case 0: Location.As.EffectiveAddr = FetchImmediate(M68k, 2); break;
        case 1: Location.As.EffectiveAddr = FetchImmediate(M68k, 4); break;
        /* (D16, PC) */
        case 2:
        {
            int32_t Displacement = SEX(32, 16)FetchImmediate(M68k, 2);
            Location.As.EffectiveAddr = PC + Displacement;
        } break;
        /* PC index */
        case 3:
        {
            Location = GetExtensionLocation(M68k, PC);
        } break;
        /* imm */
        case 4:
        {
            Location.Type = LOC_IMM;
            Location.As.Immediate = FetchImmediate(M68k, 1 == Size? 2: Size);
        } break;
        }
    } break;
    }
    return Location;
}

static uint32_t GetDataFromReg(MC68020 *M68k, unsigned RegisterIndex, unsigned Size)
{
    RegisterIndex &= 0xF;
    uint32_t Data = M68k->R[RegisterIndex];
    if (RegisterIndex >= 8 && Size == 2)
        return SEX(32, 16)Data;
    return MASK(Data, Size);
}

static uint32_t GetDataFromLocation(MC68020 *M68k, DataLocation Location, unsigned Size)
{
    switch (Location.Type)
    {
    case LOC_REG: 
    {
        return GetDataFromReg(M68k, Location.As.RegisterIndex, Size);
    } break;
    case LOC_ADDR: 
    {
        return MASK(M68k->Read(M68k, Location.As.EffectiveAddr, Size), Size);
    } break;
    case LOC_IMM:
    {
        return MASK(Location.As.Immediate, Size);
    } break;
    }
    return 0;
}

static uint32_t GetData(MC68020 *M68k, unsigned Mode, unsigned Reg, unsigned Size)
{
    DataLocation Location = GetLocation(M68k, Mode, Reg, Size);
    return GetDataFromLocation(M68k, Location, Size);
}

static void WriteDataToReg(MC68020 *M68k, unsigned RegisterIndex, uint32_t Data, unsigned Size)
{
    RegisterIndex &= 0xF;
    uint32_t Dst = M68k->R[RegisterIndex];
    if (RegisterIndex >= 8) /* store whole register is dst is an addr reg */
    {
        if (Size == 2)
            Data = SEX(32, 16)Data;
        M68k->R[RegisterIndex] = Data;
    }
    else /* mix old content with new if it's a data reg */
    {
        M68k->R[RegisterIndex] = 
            (Dst & ~((1ull << Size*8) - 1)) | MASK(Data, Size);
    }
}

static void WriteData(MC68020 *M68k, DataLocation Location, uint32_t Data, unsigned Size)
{
    switch (Location.Type)
    {
    case LOC_REG:
    {
        WriteDataToReg(M68k, Location.As.RegisterIndex, Data, Size);
    } break;
    case LOC_ADDR:
    {
        M68k->Write(M68k, Location.As.EffectiveAddr, Data, Size);
    } break;
    case LOC_IMM:
    {
        /* TODO: illegal instruction */
    } break;
    }
}

static void TestCommonDataFlags(MC68020 *M68k, uint32_t Data, unsigned Size)
{
    SET_FLAG(M68k, FLAG_C, 0);
    SET_FLAG(M68k, FLAG_V, 0);
    SET_FLAG(M68k, FLAG_Z, MASK(Data, Size) == 0);
    SET_FLAG(M68k, FLAG_N, (Data >> (Size*8 - 1)) & 0x1); /* check sign bit */
}

static void TestCommonAdditionFlags(MC68020 *M68k, 
    bool ZFlagValue, uint64_t Result, uint32_t A, uint32_t B, unsigned Size)
{
    unsigned SignIndex = Size*8 - 1;
    A = (A >> SignIndex) & 0x1;
    B = (B >> SignIndex) & 0x1;
    unsigned ResultSign = (Result >> SignIndex) & 0x1;

    SET_FLAG(M68k, FLAG_N, ResultSign);
    SET_FLAG(M68k, FLAG_Z, ZFlagValue);

    /* A and B has the same sign, but both differ from result => overflow */
    SET_FLAG(M68k, FLAG_V, (A && B && !ResultSign) || (!A && !B && ResultSign));

    bool Carry = Result > (uint64_t)MASK(-1ll, Size);
    SET_FLAG(M68k, FLAG_X, Carry);
    SET_FLAG(M68k, FLAG_C, Carry);
}

#define TestCommonSubtractionFlags(pM68k, bZFlagValue, u64Result, u32A, u32B, uSize)\
    TestCommonAdditionFlags(pM68k, bZFlagValue, u64Result, u32A, -(u32B), uSize)


static bool CondIsTrue(MC68020 *M68k, MC68020ConditionalCode ConditionalCode)
{
    switch (ConditionalCode)
    {
    case CC_T: return true;
    case CC_F: return false;
    case CC_HI: return 0 == (M68k->SR & 0x5); /* C and Z */
    case CC_LS: return 0 != (M68k->SR & 0x5); 
    case CC_CC: return !GET_TRUTHY_FLAG(M68k, FLAG_C);
    case CC_CS: return GET_TRUTHY_FLAG(M68k, FLAG_C);
    case CC_NE: return !GET_TRUTHY_FLAG(M68k, FLAG_Z);
    case CC_EQ: return GET_TRUTHY_FLAG(M68k, FLAG_Z);
    case CC_PL: return !GET_TRUTHY_FLAG(M68k, FLAG_N);
    case CC_MI: return GET_TRUTHY_FLAG(M68k, FLAG_N);
    case CC_VC: return !GET_TRUTHY_FLAG(M68k, FLAG_V);
    case CC_VS: return GET_TRUTHY_FLAG(M68k, FLAG_V);
    case CC_GE: return GET_FLAG(M68k, FLAG_N) == GET_FLAG(M68k, FLAG_V);
    case CC_LT: return GET_FLAG(M68k, FLAG_N) != GET_FLAG(M68k, FLAG_V);
    case CC_GT: return !GET_FLAG(M68k, FLAG_Z) && (GET_FLAG(M68k, FLAG_N) == GET_FLAG(M68k, FLAG_V));
    case CC_LE: return GET_FLAG(M68k, FLAG_Z) || (GET_FLAG(M68k, FLAG_N) != GET_FLAG(M68k, FLAG_V));
    }
    return false;
}


static void Move(MC68020 *M68k, uint16_t Opcode, unsigned Size)
{
    unsigned SrcMode = Opcode >> 3,
             SrcReg = Opcode,
             DstMode = Opcode >> 6,
             DstReg = Opcode >> 9;
    uint32_t Data = GetData(M68k, SrcMode, SrcReg, Size);
    DataLocation Location = GetLocation(M68k, DstMode, DstReg, Size);
    WriteData(M68k, Location, Data, Size);

    if (MODE_AN != (07 & DstMode))
    {
        TestCommonDataFlags(M68k, Data, Size);
    }
}

static void Push(MC68020 *M68k, uint32_t Data)
{
    M68k->R[15] -= 4;
    M68k->Write(M68k, M68k->R[15], Data, 4);
}

static uint32_t Pop(MC68020 *M68k)
{
    uint32_t Data = M68k->Read(M68k, M68k->R[15], 4);
    M68k->R[15] += 4;
    return Data;
}


void MC68020Execute(MC68020 *M68k)
{
    uint16_t Opcode = FETCH_OPCODE(M68k);
    switch (Opcode >> 12)
    {
    case 1: Move(M68k, Opcode, 1); break;
    case 2: Move(M68k, Opcode, 4); break;
    case 3: Move(M68k, Opcode, 2); break;
    case 6: /* BRA, BSR, Bcc */
    {
        int32_t PC = M68k->PC;
        int32_t Offset = SEX(32, 8)(Opcode);
        if (-1 == Offset)
            Offset = FetchImmediate(M68k, 4);
        else if (0 == Offset)
            Offset = SEX(32, 16)FetchImmediate(M68k, 2);

        MC68020ConditionalCode Cond = 0xF & (Opcode >> 8);
        if (CC_F == Cond) /* BSR */
        {
            Push(M68k, M68k->PC);
            M68k->PC = PC + Offset;
        }
        else if (CondIsTrue(M68k, Cond))
        {
            M68k->PC = PC + Offset;
        }
    } break;
    case 7: /* MOVEQ */
    {
        int32_t Immediate = SEX(32, 8)(Opcode);
        unsigned Dn = 07 & (Opcode >> 9);
        M68k->R[Dn] = Immediate;
        TestCommonDataFlags(M68k, Immediate, 4);
    } break;
    case 9: /* SUB, SUBA, SUBX */
    {
        unsigned Mode = 07 & (Opcode >> 3),
                 Reg = 07 & (Opcode & 07),
                 EncodedSize = 03 & (Opcode >> 6),
                 LeftReg = 07 & (Opcode >> 9);
        if (EncodedSize == 03) /* SUBA */
        {
            unsigned Size = 
                Opcode & 0x0100? 
                4: 2;
            int32_t Src = GetData(M68k, Mode, Reg, Size);
            uint32_t *Dst = &M68k->R[LeftReg + 8];

            /* writeback */
            *Dst = (2 == Size)?
                SEX(32, 16)*Dst - SEX(32, 16)Src 
                : (int32_t)*Dst - Src;
        }
        else if ((Opcode & 0x0130) == 0x0100) /* SUBX */
        {
            unsigned Size = 1 << EncodedSize;
            if (Opcode & 0x8) /* (-An) mode */
            {
                uint32_t B = GetData(M68k, MODE_PREDEC, Reg + 8, Size);
                DataLocation Dst = GetLocation(M68k, MODE_PREDEC, LeftReg + 8, Size);
                uint32_t A = GetDataFromLocation(M68k, Dst, Size);
                /* C is awesome */
                uint64_t Result = (uint64_t)A - (uint64_t)B - (uint64_t)GET_FLAG(M68k, FLAG_X);

                WriteData(M68k, Dst, Result, Size);
                TestCommonSubtractionFlags(M68k, 
                    GET_TRUTHY_FLAG(M68k, FLAG_Z) && 0 == MASK(Result, Size), 
                    Result, A, B, Size
                );
            }
            else /* Dn mode */
            {
                uint32_t A = GetDataFromReg(M68k, LeftReg, Size),
                         B = GetDataFromReg(M68k, Reg, Size);
                uint64_t Result = (uint64_t)A - (uint64_t)B - (uint64_t)GET_FLAG(M68k, FLAG_X);
                WriteDataToReg(M68k, LeftReg, Result, Size);
                TestCommonSubtractionFlags(M68k, 
                    0 == MASK(Result, Size),
                    Result, A, B, Size
                );
            }
        }
        else /* SUB */
        {
            unsigned Size = 1 << EncodedSize;
            DataLocation SrcLocation = GetLocation(M68k, Mode, Reg, Size);
            uint32_t Subtrahend, Minuend;
            uint64_t Result;
            if (Opcode & 0x0100) /* <ea> -= Reg */
            {
                Minuend = GetDataFromLocation(M68k, SrcLocation, Size);
                Subtrahend = GetDataFromReg(M68k, LeftReg, Size);
                Result = (uint64_t)Minuend - (uint64_t)Subtrahend;
                WriteData(M68k, SrcLocation, Result, Size);
            }
            else /* Reg -= <ea> */
            {
                Minuend = GetDataFromReg(M68k, LeftReg, Size);
                Subtrahend = GetDataFromLocation(M68k, SrcLocation, Size);
                Result = (uint64_t)Minuend - (uint64_t)Subtrahend;
                WriteDataToReg(M68k, LeftReg, Result, Size);
            }
            TestCommonSubtractionFlags(M68k, 
                0 == MASK(Result, Size), 
                Result, Minuend, Subtrahend, Size
            );
        }
    } break;
    case 13: /* ADD, ADDX, ADDA */
    {
        unsigned Mode = 07 & (Opcode >> 3),
                 Reg = 07 & (Opcode & 07),
                 EncodedSize = 03 & (Opcode >> 6),
                 LeftReg = 07 & (Opcode >> 9);
        if (EncodedSize == 03) /* ADDA */
        {
            unsigned Size = 
                Opcode & 0x0100? 
                4: 2;
            int32_t Src = GetData(M68k, Mode, Reg, Size);
            uint32_t *Dst = &M68k->R[LeftReg + 8];

            /* writeback */
            *Dst = (2 == Size)?
                SEX(32, 16)*Dst + SEX(32, 16)Src 
                : (int32_t)*Dst + Src;
        }
        else if ((Opcode & 0x0130) == 0x0100) /* ADDX */
        {
            unsigned Size = 1 << EncodedSize;
            if (Opcode & 0x8) /* (-An) mode */
            {
                uint32_t B = GetData(M68k, MODE_PREDEC, Reg + 8, Size);
                DataLocation Dst = GetLocation(M68k, MODE_PREDEC, LeftReg + 8, Size);
                uint32_t A = GetDataFromLocation(M68k, Dst, Size);
                uint64_t Result = (uint64_t)A + (uint64_t)B + (uint64_t)GET_FLAG(M68k, FLAG_X);

                WriteData(M68k, Dst, Result, Size);
                TestCommonAdditionFlags(M68k, 
                    GET_TRUTHY_FLAG(M68k, FLAG_Z) && 0 == MASK(Result, Size), 
                    Result, A, B, Size
                );
            }
            else /* Dn mode */
            {
                uint32_t A = GetDataFromReg(M68k, LeftReg, Size),
                         B = GetDataFromReg(M68k, Reg, Size);
                uint64_t Result = (uint64_t)A + (uint64_t)B + (uint64_t)GET_FLAG(M68k, FLAG_X);
                WriteDataToReg(M68k, LeftReg, Result, Size);
                TestCommonAdditionFlags(M68k, 
                    0 == MASK(Result, Size),
                    Result, A, B, Size
                );
            }
        }
        else /* ADD */
        {
            unsigned Size = 1 << EncodedSize;
            DataLocation SrcLocation = GetLocation(M68k, Mode, Reg, Size);
            uint32_t A = GetDataFromLocation(M68k, SrcLocation, Size),
                     B = GetDataFromReg(M68k, LeftReg, Size);
            uint64_t Result = (uint64_t)A + (uint64_t)B;

            /* writeback */
            if (Opcode & 0x0100) /* A + B -> SrcLocation */
                WriteData(M68k, SrcLocation, Result, Size);
            else /* A + B -> LeftReg */
                WriteDataToReg(M68k, LeftReg, Result, Size);
            TestCommonAdditionFlags(M68k, 
                0 == MASK(Result, Size), 
                Result, A, B, Size
            );
        }
    } break;
    default: break;
    }
}


