
#include "Common.h"
#include "MC68020.h"
#include "MC68020Isa.h"


#if HOST_IS_BIG_ENDIAN
#  define LOW_BYTE 0
#  define LOW_WORD 0
#elif HOST_IS_LITTLE_ENDIAN
#  define LOW_BYTE 3
#  define LOW_WORD 1
#endif 


typedef enum LocationType
{
    LOC_REG,
    LOC_ADDR, 
} LocationType;

typedef struct MC68020DataLocation 
{
    uint16_t Type, DataSize;
    union {
        uint32_t RegisterIndex;
        uint32_t EffectiveAddress;
    } As;
} MC68020DataLocation;




static uint32_t DefaultReadBigEndian(MC68020 *M68k, uint32_t Addr, unsigned Size)
{
    uint32_t Data = 0;
    Addr += Size;
    if (Addr < M68k->MemorySize)
    {
        for (unsigned i = 0; i < Size; i++)
        {
            Data |= (uint32_t)M68k->Memory[--Addr] << i*8;
        }
    }
    return Data;
}

static uint32_t DefaultReadLittleEndian(MC68020 *M68k, uint32_t Addr, unsigned Size)
{
    uint32_t Data = 0;
    if (Addr + Size < M68k->MemorySize)
    {
        for (uint32_t i = 0; i < Size; i++)
        {
            Data |= (uint32_t)M68k->Memory[Addr++] << i*8;
        }
    }
    return Data;
}

static void DefaultWriteBigEndian(MC68020 *M68k, uint32_t Addr, uint32_t Data, unsigned Size)
{
    Addr += Size;
    if (Addr < M68k->MemorySize)
    {
        for (unsigned i = 0; i < Size; i++)
        {
            M68k->Memory[--Addr] |= Data >> (Size - i);
        }
    }
}

static void DefaultWriteLittleEndian(MC68020 *M68k, uint32_t Addr, uint32_t Data, unsigned Size)
{
    if (Addr + Size < M68k->MemorySize)
    {
        for (unsigned i = 0; i < Size; i++)
        {
            M68k->Memory[Addr++] |= Data >> i*8;
        }
    }
}


MC68020 MC68020Init(uint8_t *Memory, size_t MemorySize, bool BigEndian)
{
    MC68020 M68k = {
        .Memory = Memory,
        .MemorySize = MemorySize,
    };
    if (BigEndian)
    {
        M68k.Read = DefaultReadBigEndian;
        M68k.Write = DefaultWriteBigEndian;
    }
    else
    {
        M68k.Read = DefaultReadLittleEndian;
        M68k.Write = DefaultWriteLittleEndian;
    }
    return M68k;
}


/* set Addr and DataSize field before call */
static uint32_t ReadData(MC68020 *M68k)
{
    return M68k->Data.Long = M68k->Read(M68k, M68k->Addr, M68k->DataSize);
}

static uint32_t ReadDataFrom(MC68020 *M68k, MC68020DataLocation Location)
{
    switch ((LocationType)Location.Type)
    {
    case LOC_REG:
    {
        if (Location.As.RegisterIndex >= 8 && Location.DataSize == 2) /* Addr Register */
        {
            M68k->Data.Long = SEX(32, 16)M68k->Reg.X[Location.As.RegisterIndex].Word[LOW_WORD];
        }
        else switch (Location.DataSize)
        {
        case 1: M68k->Data.Byte[LOW_BYTE] = M68k->Reg.X[Location.As.RegisterIndex].Byte[LOW_BYTE]; break;
        case 2: M68k->Data.Word[LOW_WORD] = M68k->Reg.X[Location.As.RegisterIndex].Word[LOW_WORD]; break;
        case 4: M68k->Data.Long = M68k->Reg.X[Location.As.RegisterIndex].Long; break;
        }
    } break;
    case LOC_ADDR:
    {
        M68k->Addr = Location.As.EffectiveAddress;
        M68k->DataSize = Location.DataSize;
        ReadData(M68k);
    } break;
    }

    return M68k->Data.Long;
}

static uint32_t ReadAddr(MC68020 *M68k)
{
    unsigned OldSize = M68k->DataSize;
    M68k->DataSize = 4;
    M68k->Addr = ReadData(M68k);
    M68k->DataSize = OldSize;
    return M68k->Addr;
}


/* set Addr, Data.Long and DataSize fields before call */
static void WriteData(MC68020 *M68k, uint32_t Data)
{
    M68k->Data.Long = Data;
    M68k->Write(M68k, M68k->Addr, Data, M68k->DataSize);
}

static void WriteDataTo(MC68020 *M68k, uint32_t Data, MC68020DataLocation Location)
{
    M68k->Data.Long = Data;
    switch (Location.Type)
    {
    case LOC_REG:
    {
        if (Location.As.RegisterIndex >= 8 && Location.DataSize == 2)
        {
            M68k->Reg.X[Location.As.RegisterIndex].Long = SEX(32, 16)Data;
        }
        else switch (M68k->DataSize)
        {
        case 1: M68k->Reg.X[Location.As.RegisterIndex].Byte[LOW_BYTE] = Data; break;
        case 2: M68k->Reg.X[Location.As.RegisterIndex].Word[LOW_WORD] = Data; break;
        case 4: M68k->Reg.X[Location.As.RegisterIndex].Long = Data; break;
        default: break;
        }
    } break;
    case LOC_ADDR:
    {
        M68k->Addr = Location.As.EffectiveAddress;
        M68k->DataSize = Location.DataSize;
        WriteData(M68k, Data);
    } break;
    }
}

static uint32_t FetchImmediate(MC68020 *M68k, unsigned Size)
{
    uint32_t OldAddr = M68k->Addr, OldSize = M68k->DataSize;

    M68k->Addr = M68k->PC;
    M68k->DataSize = Size;
    uint32_t LongImmediate = ReadData(M68k);
    M68k->PC += Size;

    M68k->DataSize = OldSize;
    M68k->Addr = OldAddr;
    return LongImmediate;
}


static unsigned TranslateSize(unsigned Encoding)
{
    static const unsigned Lut[8] = {
        1, /* 000 */
        2, /* 001 */
        4, /* 010 */
        2, /* 011 */ 
        1, /* 100 */ 
        2, /* 101 */ 
        4, /* 110 */ 
        4, /* 111 */ 
    };
    return Lut[Encoding % sizeof Lut];
}

/* -1 in RegisterIndex for PC */
#define REGIDX_PC ((unsigned)-1)
static MC68020DataLocation IndexedIndirectAddressingMode(MC68020 *M68k, 
        unsigned DataSize, unsigned RegisterIndex)
{
    MC68020DataLocation Location = {
        .DataSize = DataSize,
        .Type = LOC_ADDR,
    };
    unsigned EaField = FetchImmediate(M68k, 2);
    unsigned IndexRegister = GET_EA_IDX(EaField);
    int32_t Index = M68k->Reg.X[IndexRegister].Long;
    if (!EA_IDX_IS_LONG(EaField))
    {
        Index = SEX(32, 16)(Index & 0xFFFF);
    }
    Index <<= GET_EA_SCALE(EaField);
    uint32_t An = 0;
    if (REGIDX_PC != RegisterIndex)
    {
        An = M68k->Reg.Type.A[RegisterIndex].Long;
    }


    if (EA_FIELD_8BIT_DISPLACEMENT(EaField))
    {
        int32_t Displacement = (int8_t)(EaField & 0xFF);
        Location.As.EffectiveAddress = An + Index + Displacement;
    }
    else
    {
        int32_t Base = 0;
        if (!(EaField & (1 << 7))) /* Base displacement not supressed */
        {
            unsigned BaseSize = (EaField >> 4) & 0x3;
            if (BaseSize > 1)
            {
                Base = FetchImmediate(M68k, TranslateSize(BaseSize));
            }
        }

        if ((EaField >> 6) & 0x1) /* index suppressed */
        {
            Index = 0;
        }

        if (0 == (EaField & 0x3)) /* no memory indirection, (Bd, An, Xn) */
        {
            Location.As.EffectiveAddress = Base + An + Index;
        }
        else 
        {
            uint32_t OuterDisplacement = 0;
            unsigned OuterDisplacementSize = EaField & 0x3;
            if (OuterDisplacementSize > 1)
            {
                OuterDisplacement = FetchImmediate(M68k, TranslateSize(OuterDisplacementSize));
            }

            if (EA_POST_INDEX(EaField))
            {
                M68k->Addr = Base + An;
                Location.As.EffectiveAddress = ReadAddr(M68k) + Index + OuterDisplacement;
            }
            else
            {
                M68k->Addr = Base + An + Index;
                Location.As.EffectiveAddress = ReadAddr(M68k) + OuterDisplacement;
            }
        }
    }

    if (REGIDX_PC != RegisterIndex)
    {
        Location.As.EffectiveAddress += M68k->PC;
    }
    return Location;
}


static MC68020DataLocation GetLocation(MC68020 *M68k, unsigned DataSize, unsigned RegisterIndex, unsigned Mode)
{
    MC68020DataLocation Location = { 
        .Type = LOC_ADDR,
        .DataSize = DataSize 
    };
    switch ((MC68020EAMode)Mode)
    {
    case EA_DN:
    {
        Location.Type = LOC_REG,
        Location.As.RegisterIndex = RegisterIndex;
    } break;
    case EA_AN:
    {
        Location.Type = LOC_REG,
        Location.As.RegisterIndex = 8 + RegisterIndex;
    } break;
    case EA_ADDR:
    {
        Location.As.EffectiveAddress = M68k->Reg.Type.A[RegisterIndex].Long;
    } break;
    case EA_ADDR_INC:
    {
        Location.As.EffectiveAddress = M68k->Reg.Type.A[RegisterIndex].Long;
        M68k->Reg.Type.A[RegisterIndex].Long += DataSize;
    } break;
    case EA_ADDR_DEC:
    {
        Location.As.EffectiveAddress = M68k->Reg.Type.A[RegisterIndex].Long;
        M68k->Reg.Type.A[RegisterIndex].Long -= DataSize;
    } break;
    case EA_ADDR_I16:
    {
        int32_t Displacement = SEX(32, 16)FetchImmediate(M68k, 2);
        Location.As.EffectiveAddress = M68k->Reg.Type.A[RegisterIndex].Long + Displacement;
    } break;
    case EA_INDEX:
    {
        return IndexedIndirectAddressingMode(M68k, DataSize, RegisterIndex);
    } break;
    case EA_SPECIAL:
    {
        switch (RegisterIndex)
        {
        case 0x0: /* Abs word */
        {
            Location.As.EffectiveAddress = FetchImmediate(M68k, 2);
        } break;
        case 0x1: /* Abs long */
        {
            Location.As.EffectiveAddress = FetchImmediate(M68k, 4);
        } break;
        case 0x2: /* (ImmWord, PC) */
        {
            int32_t Displacement = SEX(32, 16)FetchImmediate(M68k, 2);
            Location.As.EffectiveAddress = M68k->PC + Displacement;
        } break;
        case 0x3:
        {
            return IndexedIndirectAddressingMode(M68k, DataSize, REGIDX_PC);
        } break;
        case 0x4: /* Immediate */
        {
            Location.As.EffectiveAddress = M68k->PC;
            M68k->PC += DataSize;
        } break;
        case 0x5:
        case 0x6:
        case 0x7:
        {
            /* none */
        } break;
        }
    } break;
    }

    return Location;
}

/* set DataSize before call */
static uint32_t GetData(MC68020 *M68k, unsigned DataSize, unsigned RegisterIndex, unsigned Mode)
{
    MC68020DataLocation Location = GetLocation(M68k, DataSize, RegisterIndex, Mode);
    switch ((LocationType)Location.Type)
    {
    case LOC_REG:
    {
        if (EA_AN == Mode && DataSize == 2)  /* addr register, sign extend */
        {
            return M68k->Data.Long = SEX(32, 16)M68k->Reg.X[Location.As.RegisterIndex].SWord[LOW_WORD];
        }
        else
        {
            M68k->Data = M68k->Reg.X[Location.As.RegisterIndex];
        }
    } break;
    case LOC_ADDR:
    {
        M68k->Addr = Location.As.EffectiveAddress;
        M68k->DataSize = DataSize;
        M68k->Data.Long = ReadData(M68k);
    } break;   
    }
    return MASK(M68k->Data.Long, DataSize);
}

static bool GetFlag(MC68020 *M68k, MC68020Flags Flag)
{
    return (M68k->SR.Raw & (1ul << Flag)) != 0;
}

static void SetFlag(MC68020 *M68k, MC68020Flags Flag, bool Condition)
{
    M68k->SR.Raw |= (uint32_t)(0 != Condition) << Flag;
}

static void TestBasicFlagsOnData(MC68020 *M68k)
{
    SetFlag(M68k, FLAG_Z, 0 == MASK(M68k->Data.Long, M68k->DataSize));
    SetFlag(M68k, FLAG_N, M68k->Data.Long & (1ull << (M68k->DataSize*8 - 1)));
    SetFlag(M68k, FLAG_C, 0);
    SetFlag(M68k, FLAG_V, 0);
}

static void TestCommonAdditionFlags(MC68020 *M68k, uint32_t Result, uint32_t A, uint32_t B)
{
    unsigned SignIndex = M68k->DataSize * 8 - 1;
    A = (A >> SignIndex) & 0x1;
    B = (B >> SignIndex) & 0x1;
    Result = (Result >> SignIndex) & 0x1;

    /* A and B has the same sign, but both differ from result => overflow */
    SetFlag(M68k, FLAG_V, (A & B & !Result) | (!A & !B & Result));

    /* (A and B are negative) or (result is positive while A or B are negative) => carry */
    bool Carry = (A & B) | ((!Result) & (A | B)) ;
    SetFlag(M68k, FLAG_X, Carry);
    SetFlag(M68k, FLAG_C, Carry);
}

static void TestCommonSubtractionFlags(MC68020 *M68k, uint32_t Result, uint32_t A, uint32_t B)
{
    unsigned SignIndex = M68k->DataSize * 8 - 1;
    A = (A >> SignIndex) & 0x1;
    B = (B >> SignIndex) & 0x1;
    Result = (Result >> SignIndex) & 0x1;

    /* Result and minuend has the same sign, but different from subtrahend => overflow */
    SetFlag(M68k, FLAG_V, (Result & B & !A) | (!Result & !B & A));

    /* minuend is negative and subtrahend is positive
     * or result is negative and subtrahend is positive 
     * or minuend and result are both negative */
    bool Carry = (B & !A) | (Result & !A) | (B & Result);
    SetFlag(M68k, FLAG_X, Carry);
    SetFlag(M68k, FLAG_C, Carry);
}

static bool ConditionIsTrue(MC68020 *M68k, MC68020ConditionalCode ConditionalCode)
{
    switch (ConditionalCode)
    {
    case CC_T: return true;
    case CC_F: return false;
    case CC_HI: return 0 == (M68k->SR.Raw & 0x5);
    case CC_LS: return 0 != (M68k->SR.Raw & 0x5);
    case CC_CC: return !GetFlag(M68k, FLAG_C);
    case CC_CS: return GetFlag(M68k, FLAG_C);
    case CC_NE: return !GetFlag(M68k, FLAG_Z);
    case CC_EQ: return GetFlag(M68k, FLAG_Z);
    case CC_PL: return !GetFlag(M68k, FLAG_N);
    case CC_MI: return GetFlag(M68k, FLAG_N);
    case CC_VC: return !GetFlag(M68k, FLAG_V);
    case CC_VS: return GetFlag(M68k, FLAG_V);
    case CC_GE: return GetFlag(M68k, FLAG_N) == GetFlag(M68k, FLAG_V);
    case CC_LT: return GetFlag(M68k, FLAG_N) != GetFlag(M68k, FLAG_V);
    case CC_GT: return !GetFlag(M68k, FLAG_Z) && (GetFlag(M68k, FLAG_N) == GetFlag(M68k, FLAG_V));
    case CC_LE: return GetFlag(M68k, FLAG_Z) || (GetFlag(M68k, FLAG_N) != GetFlag(M68k, FLAG_V));
    }
    return false;
}






static void Move(MC68020 *M68k, unsigned Size, uint16_t Opcode)
{
    unsigned SrcIndex = Opcode & 0x7, DstIndex = GET_DN(Opcode);
    MC68020EAMode SrcMode = (Opcode >> 3) & 0x7, DstMode = (Opcode >> 6) & 0x7;

    GetData(M68k, Size, SrcIndex, SrcMode);
    if (EA_AN != DstMode)
    {
        TestBasicFlagsOnData(M68k);
    }

    MC68020DataLocation Location = GetLocation(M68k, Size, DstIndex, DstMode);
    WriteDataTo(M68k, M68k->Data.Long, Location);
}

static void Push(MC68020 *M68k, uint32_t Value)
{
    M68k->DataSize = 4;
    M68k->Addr = M68k->Reg.Type.A[7].Long -= 4;
    WriteData(M68k, Value);
}



void MC68020Execute(MC68020 *M68k)
{
#define ARITH_OP(Op, DataType) do {\
    A = ReadDataFrom(M68k, Location);\
    B = M68k->Reg.Type.D[LeftRegisterIndex] DataType;\
    Result = A Op B;\
    M68k->Reg.Type.D[LeftRegisterIndex] DataType = Result;\
} while (0)

#define AARITH_OP(Op, TypeCast, DataType) do {\
    A = ReadDataFrom(M68k, Location);\
    B = M68k->Reg.Type.A[LeftRegisterIndex] DataType;\
    Result = TypeCast A Op TypeCast B;\
    M68k->Reg.Type.A[LeftRegisterIndex].Long = Result;\
} while (0)

#define XARITH_OP(Op, DataType) do {\
    A = ReadDataFrom(M68k, Location);\
    B = M68k->Reg.Type.D[LeftRegisterIndex] DataType;\
    Result = A Op B;\
    if (Mode == EA_DN) {\
        Result Op##= GetFlag(M68k, FLAG_X);\
        M68k->Reg.Type.D[LeftRegisterIndex] DataType = Result;\
    } else {\
        WriteData(M68k, Result);\
    }\
} while (0)

#define IMM_OP(Op) do {\
    unsigned RegisterIndex = Opcode & 0x7;\
    \
    MC68020DataLocation Location = GetLocation(M68k, M68k->DataSize, RegisterIndex, GET_MODE(Opcode));\
    Dst = ReadData(M68k);\
    Result = Dst Op Immediate;\
    \
    WriteDataTo(M68k, Result, Location);\
} while (0)

    uint16_t Opcode = FetchImmediate(M68k, sizeof(Opcode));
    /* TODO: illegal instructions */
    switch (Opcode >> 12)
    {
    case 0x0: /* ORI, ANDI, SUBI, ADDI, EORI, CMPI, BTST, BCHG, BCLR, BSET, MOVEP */
    {
        M68k->DataSize = TranslateSize(GET_DATASIZE(Opcode));
        if (0x1 & (Opcode >> 8)) /* BTST, BCHG, BCLR, BSET, MOVEP */
        {
        }
        else /* ORI, ANDI, SUBI, ADDI, EORI, CMPI, BTST, BCHG, BCLR, BSET */
        {
            uint32_t Immediate = SIZE_BYTE == M68k->DataSize
                ? 0xFF & FetchImmediate(M68k, 2)
                : FetchImmediate(M68k, M68k->DataSize);
            uint32_t Result = 0, Dst = 0;
            switch (0x7 & (Opcode >> 9))
            {
            case 0x0: /* ORI */
            {
                if (IS_CCR_INSTRUCTION(Opcode))
                {
                    M68k->SR.Type.User |= CCR_IMPL_BITS & Immediate;
                }
                else if (IS_SR_INSTRUCTION(Opcode))
                {
                    M68k->SR.Raw |= SR_IMPL_BITS & Immediate;
                }
                else
                {
                    IMM_OP(|);
                    TestBasicFlagsOnData(M68k);
                }
            } break;
            case 0x1: /* ANDI */ 
            {
                if (IS_CCR_INSTRUCTION(Opcode))
                {
                    M68k->SR.Type.User &= CCR_IMPL_BITS & Immediate;
                }
                else if (IS_SR_INSTRUCTION(Opcode))
                {
                    M68k->SR.Raw &= SR_IMPL_BITS & Immediate;
                }
                else
                {
                    IMM_OP(&);
                    TestBasicFlagsOnData(M68k);
                }
            } break;
            case 0x2: /* SUBI */
            {
                IMM_OP(-);
                TestCommonSubtractionFlags(M68k, Result, Dst, Immediate);
                SetFlag(M68k, FLAG_Z, 0 == MASK(Result, M68k->DataSize));
            } break;
            case 0x3: /* ADDI */
            {
                IMM_OP(+);
                TestCommonAdditionFlags(M68k, Result, Dst, Immediate);
                SetFlag(M68k, FLAG_Z, 0 == MASK(Result, M68k->DataSize));
            } break;
            case 0x4: /* BTST, BCHG, BCLR, BSET */
            {
            } break;
            case 0x5: /* EORI */
            {
                if (IS_CCR_INSTRUCTION(Opcode))
                {
                    M68k->SR.Type.User ^= CCR_IMPL_BITS & Immediate;
                }
                else if (IS_SR_INSTRUCTION(Opcode))
                {
                    M68k->SR.Raw ^= SR_IMPL_BITS & Immediate;
                }
                else
                {
                    IMM_OP(&);
                    TestBasicFlagsOnData(M68k);
                }
            } break;
            case 0x6: /* CMPI */
            {
            } break;
            }
        }
    } break;
    /* fuck this encoding */
    case 0x1: /* MOVEB */           Move(M68k, 1, Opcode); break;
    case 0x2: /* MOVEL, MOVEAL */   Move(M68k, 4, Opcode); break;
    case 0x3: /* MOVEW, MOVEAW */   Move(M68k, 2, Opcode); break;
    case 0x4: /* */
    {
    } break;
    case 0x5: /* ADDQ, SUBQ, Scc, DBcc, */
    {
        unsigned EncodedDataSize = GET_DATASIZE(Opcode);
        MC68020EAMode Mode = GET_MODE(Opcode);
        if (0x3 == EncodedDataSize) /* Scc or DBcc */
        {
            if (0x1 == Mode) /* DBcc */
            {
                uint32_t PC = M68k->PC;
                int32_t BranchOffset = SEX(32, 16)FetchImmediate(M68k, 2);
                if (!ConditionIsTrue(M68k, GET_CC(Opcode)))
                {
                    int16_t *Dn = &M68k->Reg.Type.D[Opcode & 0x7].SWord[LOW_WORD];
                    *Dn -= 1;
                    if (-1 != *Dn)
                    {
                        M68k->PC = PC + BranchOffset;
                    }
                }
            }
            else /* Scc */
            {
                MC68020DataLocation Location = GetLocation(M68k, 1, Opcode & 0x7, Mode);
                WriteDataTo(M68k, ConditionIsTrue(M68k, GET_CC(Opcode)), Location);
            }
        }
        else /* SUBQ or ADDQ */
        {
            M68k->DataSize = TranslateSize(EncodedDataSize);
            MC68020DataLocation Location = GetLocation(M68k, M68k->DataSize, Opcode & 0x7, Mode);
            uint32_t Value = ReadDataFrom(M68k, Location);
            unsigned QuickImmediate = (Opcode >> 9) & 0x7;
            uint32_t Result;

            if (GET_DIRECTION(Opcode)) /* SUBQ */
            {
                Result = Value + QuickImmediate;
                TestCommonSubtractionFlags(M68k, Result, Value, QuickImmediate);
            }
            else /* ADDQ */
            {
                Result = Value - QuickImmediate;
                TestCommonAdditionFlags(M68k, Result, Value, QuickImmediate);
            }

            WriteDataTo(M68k, Result, Location);
            SetFlag(M68k, FLAG_Z, 0 == MASK(Result, M68k->DataSize));
        }
    } break;
    case 0x6: /* Bcc, BSR, BRA */
    {
        MC68020ConditionalCode ConditionalCode = GET_CC(Opcode);
        uint32_t PC = M68k->PC;
        int32_t BranchOffset = SEX(32, 8)(Opcode & 0xFF);
        if (0 == BranchOffset) /* 16 bit offset instead */
        {
            BranchOffset = SEX(32, 16)FetchImmediate(M68k, 2);
        }
        else if (-1 == BranchOffset) /* 32 bit offset instead */
        {
            BranchOffset = FetchImmediate(M68k, 4);
        }

        if (CC_F == ConditionalCode) /* BSR */
        {
            Push(M68k, M68k->PC);
            M68k->PC = PC + BranchOffset;
        }
        else if (ConditionIsTrue(M68k, ConditionalCode))
        {
            M68k->PC = PC + BranchOffset;
        }
    } break;
    case 0x9: /* SUB, SUBA, SUBX */
    {
        uint32_t A = 0, B = 0, Result = 0;
        if ((Opcode & 0x0131) == 0x0101) /* SUBX -(An), -(An) */ 
        {
            M68k->DataSize = TranslateSize(GET_DATASIZE(Opcode));
            
            unsigned x = GET_DN(Opcode), y = Opcode & 0x7;
            /* dst */
            M68k->Addr = M68k->Reg.Type.A[y].Long;
            M68k->Reg.Type.A[y].Long -= M68k->DataSize;
            A = ReadData(M68k);
            
            /* src */
            M68k->Addr = M68k->Reg.Type.A[x].Long;
            M68k->Reg.Type.A[x].Long -= M68k->DataSize;
            B = ReadData(M68k);

            /* op */
            Result = A - B - GetFlag(M68k, FLAG_X);
            TestCommonSubtractionFlags(M68k, Result, A, B);
            SetFlag(M68k, FLAG_Z, 0 == MASK(Result, M68k->DataSize) && GetFlag(M68k, FLAG_Z));
            
            WriteData(M68k, Result);
        } 
        else /* SUB, SUBA, SUBX */
        { 
            MC68020EAMode Mode = GET_MODE(Opcode);
            unsigned Size = GET_DATASIZE(Opcode);
            M68k->DataSize = TranslateSize(Size);
            
            unsigned LeftRegisterIndex = GET_DN(Opcode);
            unsigned RightRegisterIndex = Opcode & 0x7;
            MC68020DataLocation Location = GetLocation(M68k, M68k->DataSize, RightRegisterIndex, Mode);
            
            switch (Size) 
            {
            case SIZE_BYTE:         ARITH_OP(-, .Byte[LOW_BYTE]); break;
            case SIZE_WORD:         ARITH_OP(-, .Word[LOW_WORD]); break;
            case SIZE_LONG:         ARITH_OP(-, .Long); break;
            case SIZE_ADDR_WORD:    AARITH_OP(-, SEX(32, 16), .Word[LOW_WORD]); break;
            case SIZE_X_BYTE:       XARITH_OP(-, .Byte[LOW_BYTE]); break;
            case SIZE_X_WORD:       XARITH_OP(-, .Word[LOW_WORD]); break;
            case SIZE_X_LONG:       XARITH_OP(-, .Long); break;
            case SIZE_ADDR_LONG:    AARITH_OP(-, (int32_t), .Long); break;
            }

            /* only set flags if dst is not an addr reg */
            if ((Size != SIZE_ADDR_WORD) && (Size != SIZE_ADDR_LONG)) 
            {
                TestCommonSubtractionFlags(M68k, Result, A, B);
                SetFlag(M68k, FLAG_Z, 0 == MASK(Result, M68k->DataSize));
            }
        }
    } break;
    case 0xA: /* None, all illegal */
    {
    } break;
    case 0xB: /* EOR, CMPM, CMP, CMPA */
    {
    } break;
    case 0xC: /* MULU, MULS, ABCD, EXG */
    {
    } break;
    case 0xD: /* ADD, ADDA, ADDX */
    {
        uint32_t A = 0, B = 0, Result = 0;
        if ((Opcode & 0x0131) == 0x0101) /* ADDX -(An), -(An) */ 
        {
            M68k->DataSize = TranslateSize(GET_DATASIZE(Opcode));
            
            /* dst */
            unsigned x = GET_DN(Opcode), y = Opcode & 0x7;
            M68k->Addr = M68k->Reg.Type.A[y].Long;
            M68k->Reg.Type.A[y].Long -= M68k->DataSize;
            A = ReadData(M68k);
            
            /* src */
            M68k->Addr = M68k->Reg.Type.A[x].Long;
            M68k->Reg.Type.A[x].Long -= M68k->DataSize;
            B = ReadData(M68k);

            /* op */
            Result = A + B;
            TestCommonAdditionFlags(M68k, Result, A, B);
            SetFlag(M68k, FLAG_Z, 0 == MASK(Result, M68k->DataSize) && GetFlag(M68k, FLAG_Z));
            
            WriteData(M68k, Result);
        } 
        else /* ADD, ADDA, ADDX */
        {
            MC68020EAMode Mode = GET_MODE(Opcode);
            unsigned Size = GET_DATASIZE(Opcode);
            M68k->DataSize = TranslateSize(Size);
            
            unsigned LeftRegisterIndex = GET_DN(Opcode);
            unsigned RightRegisterIndex = Opcode & 0x7;
            MC68020DataLocation Location = GetLocation(M68k, M68k->DataSize, RightRegisterIndex, Mode);
            
            switch (Size) 
            {
            case SIZE_BYTE:         ARITH_OP(+, .Byte[LOW_BYTE]); break;
            case SIZE_WORD:         ARITH_OP(+, .Word[LOW_WORD]); break;
            case SIZE_LONG:         ARITH_OP(+, .Long); break;
            case SIZE_ADDR_WORD:    AARITH_OP(+, SEX(32, 16), .Word[LOW_WORD]); break;
            case SIZE_X_BYTE:       XARITH_OP(+, .Byte[LOW_BYTE]); break;
            case SIZE_X_WORD:       XARITH_OP(+, .Word[LOW_WORD]); break;
            case SIZE_X_LONG:       XARITH_OP(+, .Long); break;
            case SIZE_ADDR_LONG:    AARITH_OP(+, (int32_t), .Long); break;
            }
            /* only set flags if dst is not an addr reg */
            if ((Size != SIZE_ADDR_WORD) && (Size != SIZE_ADDR_LONG)) 
            {
                TestCommonAdditionFlags(M68k, Result, A, B);
                SetFlag(M68k, FLAG_Z, 0 == MASK(Result, M68k->DataSize));
            }
        }
    } break;
    }

#undef ARITH_OP
#undef IMM_OP
}





