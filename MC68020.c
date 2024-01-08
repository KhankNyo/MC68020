
#include "MC68020.h"
#include "MC68020Isa.h"

#define SEX(To, From) (int##To##_t)(int##From##_t)


static uint32_t DefaultRead(MC68020 *M68k, uint32_t Addr, unsigned Size)
{
    uint32_t Data = 0;
    Addr += Size;
    for (unsigned i = 0; i < Size; i++)
    {
        if (--Addr < M68k->MemorySize)
        {
            Data |= (uint32_t)M68k->Memory[Addr] << i*8;
        }
    }
    return Data;
}

static void DefaultWrite(MC68020 *M68k, uint32_t Addr, uint32_t Data, unsigned Size)
{
    for (unsigned i = 0; i < Size; i++)
    {
        if (--Addr < M68k->MemorySize)
        {
            M68k->Memory[Addr] |= Data >> (Size - i);
        }
    }
}

MC68020 MC68020Init(uint8_t *Memory, size_t MemorySize)
{
    MC68020 M68k = {
        .Reg.X = { 0 },
        .PC = 0,
        .Addr = 0,
        .Data = { 0 },

        .Memory = Memory,
        .MemorySize = MemorySize,

        .Read = DefaultRead,
        .Write = DefaultWrite,
    };
    return M68k;
}


static uint16_t FetchWord(MC68020 *M68k)
{
    uint16_t Opcode = M68k->Read(M68k, M68k->PC, sizeof(Opcode));
    M68k->PC += sizeof(Opcode);
    return Opcode;
}

static MC68020Register *GetLocation(MC68020 *M68k, unsigned Size, unsigned Field)
{
    unsigned n = Field & 0x7;
    switch ((Field >> 3) & 0x7)
    {
    case EA_DN:
    {
        M68k->Data = M68k->Reg.Type.D[n];
    } break;
    case EA_AN:
    {
        M68k->Data = M68k->Reg.Type.A[n];
    } break;
    case EA_ADDR:
    {
        M68k->Data.Long = M68k->Read(M68k, M68k->Reg.Type.A[n].Long, Size);
    } break;
    case EA_ADDR_INC:
    {
        M68k->Data.Long = M68k->Read(M68k, M68k->Reg.Type.A[n].Long, Size);
        M68k->Reg.Type.A[n].Long += Size;
    } break;
    case EA_ADDR_DEC:
    {
        M68k->Data.Long = M68k->Read(M68k, M68k->Reg.Type.A[n].Long, Size);
        M68k->Reg.Type.A[n].Long -= Size;
    } break;
    case EA_ADDR_I16:
    {
        int32_t Displacement = (int16_t)FetchWord(M68k);
        uint32_t EffectiveAddress = M68k->Reg.Type.A[n].Long + Displacement;
        M68k->Data.Long = M68k->Read(M68k, EffectiveAddress, Size);
    } break;
    case EA_INDEX:
    {
        unsigned EaField = FetchWord(M68k);
        int32_t Index = EA_IDX_IS_LONG(EaField) 
                ? (int32_t)M68k->Reg.X[GET_EA_IDX(EaField)].Long
                : SEX(32, 16)M68k->Reg.X[GET_EA_IDX(EaField)].Word[0];
        unsigned Scale = GET_EA_SCALE(EaField);

        if (EA_FIELD_IS_COMPLICATED(EaField))
        {
            int32_t Base = 0;
            if (!(EaField & (1 << 7))) /* Base not suppressed */
            {
                unsigned BaseSize = (EaField >> 4) & 0x3;
                switch (BaseSize)
                {
                case 0: /* reserved */ break;
                case 1: /* null */ break;
                case 2: {
                    Base = (int32_t)FetchWord(M68k);
                } break;
                case 3:
                {
                    Base = (int32_t)(FetchWord(M68k) << 16);
                    Base |= FetchWord(M68k);
                } break;
                }
            }

            /* TODO: the docs said only ind have post and pre, not mem ind in the addr mode page, 
             * but the page for add ins said only mem ind has post and pre */
            uint32_t EffectiveAddress = M68k->Reg.Type.A[n].Long + Base;
            switch (GET_MEMIND(EaField))
            {
            case MEMIND_IDX_ONLY: 
            {
                EffectiveAddress += (Index << Scale);
            } break;
            case MEMIND_MEM_PREI_NULL:
            {
                EffectiveAddress += (Index << Scale) + 0;
                EffectiveAddress = M68k->Read(M68k, EffectiveAddress, 4);
            } break;
            case MEMIND_MEM_PREI_WORD:
            {
                int32_t Displacement = (int16_t)FetchWord(M68k);
                EffectiveAddress += (Index << Scale);
                EffectiveAddress = M68k->Read(M68k, EffectiveAddress, 4) + Displacement;
            } break;
            case MEMIND_MEM_PREI_LONG:
            {
                int32_t Displacement = (int32_t)FetchWord(M68k) << 16;
                Displacement |= FetchWord(M68k);
                EffectiveAddress += (Index << Scale);
                EffectiveAddress = M68k->Read(M68k, EffectiveAddress, 4) + Displacement;
            } break;
            case MEMIND_MEM_POSTI_NULL:
            {
                EffectiveAddress += Base + 0;
                EffectiveAddress = M68k->Read(M68k, EffectiveAddress, 4) + (Index << Scale);
            } break;
            case MEMIND_MEM_POSTI_WORD:
            {
                int32_t Displacement = (int16_t)FetchWord(M68k);
                EffectiveAddress = M68k->Read(M68k, EffectiveAddress, 4) + (Index << Scale) + Displacement;
            } break;
            case MEMIND_MEM_POSTI_LONG:
            {
                int32_t Displacement = (int32_t)FetchWord(M68k) << 16;
                Displacement |= FetchWord(M68k);
                EffectiveAddress = M68k->Read(M68k, EffectiveAddress, 4) + (Index << Scale) + Displacement;
            } break;
            case MEMIND_IND_NULL:
            {
                EffectiveAddress += Index << Scale;
            } break;
            case MEMIND_IND_WORD:
            {
                int32_t Displacement = (int16_t)FetchWord(M68k);
                EffectiveAddress += Displacement;
            } break;
            case MEMIND_IND_LONG:
            {
                int32_t Displacement = (int32_t)FetchWord(M68k) << 16;
                Displacement |= FetchWord(M68k);
                EffectiveAddress += Displacement;
            } break;
            case MEMIND_RESV1:
            case MEMIND_RESV2:
            case MEMIND_NONE:  break;
            }
        }
        else
        {
            int32_t Displacement = (int8_t)(EaField & 0xFF);
            uint32_t EffectiveAddress = M68k->Reg.Type.A[n].Long + (Index << Scale) + Displacement;
            M68k->Data.Long = M68k->Read(M68k, EffectiveAddress, Size);
        }
    } break;
    case EA_SPECIAL:
    {
    } break;
    }

    return &M68k->Data;
}

static MC68020Register GetData(MC68020 *M68k, unsigned Size, unsigned Field)
{
    return *GetLocation(M68k, Size, Field);
}

void MC68020Execute(MC68020 *M68k)
{
#define ARITH_OP(Op)\
    do {\
        switch (GET_OPMODE(Opcode)) {\
        case OPMODE_BYTE: {\
            M68k->Reg.Type.D[GET_DN(Opcode)].Byte[0] Op GetData(M68k, 1, GET_EA_FIELD(Opcode)).Byte[0];\
        } break;\
        case OPMODE_WORD: {\
            M68k->Reg.Type.D[GET_DN(Opcode)].Word[0] Op GetData(M68k, 2, GET_EA_FIELD(Opcode)).Word[0];\
        } break;\
        case OPMODE_LONG: {\
            M68k->Reg.Type.D[GET_DN(Opcode)].Long Op GetData(M68k, 4, GET_EA_FIELD(Opcode)).Long;\
        } break;\
        case OPMODE_REV_BYTE: {\
            GetLocation(M68k, 1, GET_EA_FIELD(Opcode))->Byte[0] Op M68k->Reg.Type.D[GET_DN(Opcode)].Byte[0];\
        } break;\
        case OPMODE_REV_WORD: {\
            GetLocation(M68k, 2, GET_EA_FIELD(Opcode))->Word[0] Op M68k->Reg.Type.D[GET_DN(Opcode)].Word[0];\
        } break;\
        case OPMODE_REV_LONG: {\
            GetLocation(M68k, 4, GET_EA_FIELD(Opcode))->Long Op M68k->Reg.Type.D[GET_DN(Opcode)].Long;\
        } break;\
        }\
    } while (0)

    unsigned Opcode = FetchWord(M68k);
    switch (GET_OP(Opcode))
    {
    case MC68020_ADD:
    {
        ARITH_OP(+=);
    } break;

    }

#undef ARITH_OP
}





