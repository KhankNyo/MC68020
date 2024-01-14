
#include "Common.h"
#include "Assembler.h"

#include <stdarg.h>
#include <stdlib.h>



typedef enum TokenType 
{
    TOKEN_ERROR,
    TOKEN_EOF,

    TOKEN_IDENTIFIER,
    TOKEN_LIT_INT,
    TOKEN_LIT_FLT,
    TOKEN_LIT_STR,

    /* expr */
    TOKEN_EQUAL, TOKEN_LESS_EQUAL, TOKEN_GREATER_EQUAL, 
    TOKEN_LESS, TOKEN_GREATER, 
    TOKEN_LSHIFT, TOKEN_RSHIFT, TOKEN_RSHIFT_ARITH,
    TOKEN_PLUS, TOKEN_MINUS, TOKEN_STAR, TOKEN_SLASH, TOKEN_PERCENT,
    TOKEN_AMPERSAND, TOKEN_BAR, TOKEN_CARET, TOKEN_EQUAL_EQUAL, TOKEN_BANG_EQUAL,
    TOKEN_TILDE,

    /* syntax */
    TOKEN_LPAREN, TOKEN_RPAREN,
    TOKEN_LCURLY, TOKEN_RCURLY,
    TOKEN_LBRACE, TOKEN_RBRACE,
    TOKEN_COLON,
    TOKEN_POUND,
    TOKEN_COMMA,
    TOKEN_DOT,

    /* reg */
    TOKEN_ADDR_REG,
    TOKEN_DATA_REG,
    TOKEN_CCR,
    TOKEN_SR,
    TOKEN_REL, /* PC */

    /* instructions */
    TOKEN_ABCD,
    TOKEN_ADD,
    TOKEN_ADDA,
    TOKEN_ADDI,
    TOKEN_ADDQ,
    TOKEN_ADDX,
    TOKEN_ANDI,
    TOKEN_AND,
    TOKEN_ASR, 
    TOKEN_ASL,

    TOKEN_Bcc,
    TOKEN_BCHG,
    TOKEN_BCLR,
    TOKEN_BFCHG,
    TOKEN_BFCLR,
    TOKEN_BFEXTS,
    TOKEN_BFEXTU,
    TOKEN_BFFFO,
    TOKEN_BFINS,
    TOKEN_BFSET,
    TOKEN_BFTST,
    TOKEN_BKPT,
    TOKEN_BRA,
    TOKEN_BSET,
    TOKEN_BSR,
    TOKEN_BTST,

    TOKEN_CALLM,
    TOKEN_CAS,
    TOKEN_CAS2,
    TOKEN_CHK,
    TOKEN_CHK2,
    TOKEN_CLR,
    TOKEN_CMP,
    TOKEN_CMP2,
    TOKEN_CMPA,
    TOKEN_CMPI,
    TOKEN_CMPM,
    /* TODO: cp instructions */

    TOKEN_DBcc,
    TOKEN_DIVS,
    TOKEN_DIVU,
    TOKEN_DIVSL,
    TOKEN_DIVUL,

    TOKEN_EOR,
    TOKEN_EORI,
    TOKEN_EXT,
    TOKEN_EXTB,
    TOKEN_EXG,

    TOKEN_ILLEGAL,
    
    TOKEN_JMP,
    TOKEN_JSR,

    TOKEN_LEA,
    TOKEN_LINK,
    TOKEN_LSL,
    TOKEN_LSR,

    TOKEN_MOVE,
    TOKEN_MOVEA,
    TOKEN_MOVEM,
    TOKEN_MOVEP,
    TOKEN_MOVEQ,
    TOKEN_MOVEC,
    TOKEN_MOVES,
    TOKEN_MULS,
    TOKEN_MULU,

    TOKEN_NBCD,
    TOKEN_NEG,
    TOKEN_NEGX,
    TOKEN_NOP,
    TOKEN_NOT,

    TOKEN_OR,
    TOKEN_ORI,

    TOKEN_PACK,
    TOKEN_PEA,

    TOKEN_RESET,
    TOKEN_ROL, 
    TOKEN_ROR,
    TOKEN_ROXL,
    TOKEN_ROXR,
    TOKEN_RTD,
    TOKEN_RTE,
    TOKEN_RTM,
    TOKEN_RTR,
    TOKEN_RTS,

    TOKEN_SBCD,
    TOKEN_Scc,
    TOKEN_STOP,
    TOKEN_SUB,
    TOKEN_SUBA,
    TOKEN_SUBI,
    TOKEN_SUBQ,
    TOKEN_SUBX,
    TOKEN_SWAP,

    TOKEN_TAS,
    TOKEN_TRAP,
    TOKEN_TRAPcc,
    TOKEN_TRAPV,
    TOKEN_TST,

    TOKEN_UNLK,
    TOKEN_UNPK,

    TOKEN_COUNT
} TokenType;

typedef struct StringView 
{
    const char *Ptr;
    size_t Len;
} StringView;

#define STRVIEW_FMT "%.*s"
#define STRVIEW_FMT_ARG(StrView) (int)(StrView).Len, (StrView).Ptr

typedef union TokenData
{
    uint64_t Int;
    double Flt;
    StringView String;
    unsigned ConditionalCode;
} TokenData;

typedef struct Value 
{
    bool IsFloat;
    union {
        double Flt;
        uint64_t Int;
    } As;
} Value;


typedef enum ArgumentType 
{
    /* '?' means optional. 
     * When an item is optional, 
     * the comma in front of or after it is also optional,
     * ex:
     *  [Bd?, An]:
     *      valid: [An]; [Bd, An]
     *  ([Bd?], An, Xn?)
     *      valid: ([], An); ([Bd], An); ([Bd], An, Xn); ([], An, Xn)
     * */
    ARG_INVALID = 0,
    ARG_IMMEDIATE,  /* #Imm */
    ARG_ADDR,       /* Abs Addr */
    ARG_DATA_REG,   /* Dn */
    ARG_ADDR_REG,   /* An */
    ARG_IND_REG,    /* (An) */
    ARG_IND_PREDEC, /* -(An) */
    ARG_IND_POSTINC,/* (An)+ */
    ARG_IND_I16,    /* (I16, An) */

    ARG_IDX_I8,     /* (I8?, An, Xn) */
    ARG_IDX_BD,     /* (Bd, An, Xn?) */
    ARG_MEM_PRE,    /* ([Bd?, An, Xn], Od?) */
    ARG_MEM_POST,   /* ([Bd?, An], Xn, Od?) */
    ARG_MEM,        /* ([Bd?, An], Od?) */

    ARG_PC_IDX_I8,  /* rel (I8?, Xn) */
    ARG_PC_I16,     /* rel (I16) */
    ARG_PC_BD,      /* rel (Bd, Xn?) */
    ARG_PC_MEM_PRE, /* rel ([Bd, Xn], Od?) */
    ARG_PC_MEM_POST,/* rel ([Bd], Xn, Od?) */
    ARG_PC_MEM,     /* rel ([Bd], Od?) */
} ArgumentType;
typedef struct XnReg /* n >= 8 for Address reg */ 
{
    uint8_t Size, n, Scale;
} XnReg;
typedef struct Argument 
{
    ArgumentType Type;
    union {
        uint32_t An, Dn;
        uint32_t Addr, Immediate;
        struct {
            uint32_t An;
        } PreDec, PostInc;
        struct {
            int32_t Displacement;
            uint32_t An;
            XnReg X;
        } Ind;
        struct {
            uint32_t An;
            int8_t I8;
            XnReg X;
        } IdxI8;
        struct {
            int32_t Bd;
            int32_t Od;
            uint8_t An;
            XnReg X;
        } Mem, Idx;
        union {
            uint16_t I16;
            struct {
                XnReg X;
                int8_t I8;
            } Idx;
            struct {
                int32_t Bd, Od;
                XnReg X;
            } Mem;
        } PC;
    } As;
} Argument;
#define ARGUMENT(Typ, ...) (Argument) {.Type = Typ, __VA_ARGS__}
#define ADDRM_USE_PC(ArgType) IN_RANGE(ARG_PC_IDX_I8, ArgType, ARG_PC_MEM)
#define NO_REG 17


typedef struct EaEncoding 
{
    uint8_t ModeReg;
    uint8_t RegMode;
    uint16_t Extension;
    bool HasImmediate;
    union {
        uint32_t Immediate;
        uint32_t BaseDisplacement;
    } u;
    uint32_t OuterDisplacement;
} EaEncoding;
#define NO_EXTENSION 0xFFFF

typedef struct Token 
{
    StringView Lexeme;
    int Line, Offset;
    TokenType Type;
    TokenData Data;
} Token;

typedef struct Expression
{
    StringView Str;
    int Line, Offset;
} Expression;

typedef struct M68kAssembler
{
    AllocatorFn Allocator;
    void (*Emit)(uint8_t *, uint64_t, unsigned);
    bool Error, CriticalError, Panic;
    const char *SourceName;
    FILE *ErrorStream;

    int LineCount;
    const char *LineStart, *StartPtr, *CurrPtr;
    Token CurrentToken, NextToken;

    uint32_t PC;
    MC68020MachineCode MachineCode;

    uint32_t IdenCount;
    uint32_t IsFloat[1048/32];
    uint32_t IsLabel[1048/32];
    StringView Idens[1048];
    union {
        uint64_t Int;
        double Flt;
    } IdenData[1048];

    uint32_t ExprCount;
    Expression Expr[1048];
} M68kAssembler;



#define TO_UPPER(Ch) ((Ch) & ~(1 << 5))
#define IN_RANGE(lower, n, upper) ((lower) <= (n) && (n) <= (upper))
#define IN_I8(n) IN_RANGE((int64_t)INT8_MIN, (int64_t)(n), (int64_t)INT8_MAX)
#define IN_I16(n) IN_RANGE((int64_t)INT16_MIN, (int64_t)(n), (int64_t)INT16_MAX)
#define IN_I32(n) IN_RANGE((int64_t)INT32_MIN, (int64_t)(n), (int64_t)INT32_MAX)
#define INS_COUNT (TOKEN_UNPK - TOKEN_ABCD)
#define INS_BASE TOKEN_ABCD


static bool IsAlpha(char Ch)
{
    return IN_RANGE('a', Ch, 'z')
        || IN_RANGE('A', Ch, 'Z')
        || '_' == Ch;
}
static bool IsNumber(char Ch)
{
    return IN_RANGE('0', Ch, '9');
}
static bool IsAlphaNum(char Ch)
{
    return IsNumber(Ch) || IsAlpha(Ch);
}


static bool IsAtEnd(M68kAssembler *Assembler)
{
    return '\0' == *Assembler->CurrPtr;
}

static char PeekChar(M68kAssembler *Assembler)
{
    if (IsAtEnd(Assembler))
        return '\0';
    return Assembler->CurrPtr[0];
}

static bool ConsumeIfNextCharIs(M68kAssembler *Assembler, char Ch)
{
    if (Ch == PeekChar(Assembler))
    {
        Assembler->CurrPtr++;
        return true;
    }
    return false;
}

static char ConsumeSpace(M68kAssembler *Assembler)
{
    while (!IsAtEnd(Assembler))
    {
        switch (*Assembler->CurrPtr)
        {
        case ' ':
        case '\t':
        case '\r':
        {
            Assembler->CurrPtr++;
        } break;
        case '\n':
        {
            Assembler->CurrPtr++;
            Assembler->LineStart = Assembler->CurrPtr;
            Assembler->LineCount++;
        } break;
        case ';': /* ; comment */
        {
            do {
                Assembler->CurrPtr++;
            } while (!IsAtEnd(Assembler) && '\n' != *Assembler->CurrPtr);
        } break;
        default: goto Out;
        }
    }
Out:
    Assembler->StartPtr = Assembler->CurrPtr++;
    return *Assembler->StartPtr;
}

static Token MakeTokenWithData(M68kAssembler *Assembler, TokenType Type, TokenData Data)
{
    Token Tok = {
        .Type = Type,
        .Lexeme = {
            .Ptr = Assembler->StartPtr,
            .Len = Assembler->CurrPtr - Assembler->StartPtr,
        },
        .Line = Assembler->LineCount,
        .Offset = Assembler->StartPtr - Assembler->LineStart + 1,
        .Data = Data,
    };
    Assembler->StartPtr = Assembler->CurrPtr;
    return Tok;
}
#define MakeTokenWith(Type, ...) \
    MakeTokenWithData(Assembler, Type, (TokenData) {\
        __VA_ARGS__\
    })
#define MakeToken(Type) MakeTokenWithData(Assembler, Type, (TokenData) { 0 })

static Token ErrorTokenStr(M68kAssembler *Assembler, const char *Msg, size_t MsgLen)
{
    return MakeTokenWith(TOKEN_ERROR, 
        .String.Ptr = Msg,
        .String.Len = MsgLen
    );
}
#define ErrorToken(LiteralMsg) ErrorTokenStr(Assembler, LiteralMsg, sizeof LiteralMsg)


static Token ConsumeFloatDecimalPlace(M68kAssembler *Assembler, uint64_t Integer)
{
    double Double = 0;
    unsigned DecimalPlaces = 0;
    while (IsNumber(*Assembler->CurrPtr) || '_' == *Assembler->CurrPtr)
    {
        char Char = *Assembler->CurrPtr++;
        if ('_' == Char) 
            continue;

        Double *= 10;
        Double += Char - '0';
        DecimalPlaces += 10;
    }
    if (IsAlpha(*Assembler->CurrPtr))
        return ErrorToken("Invalid character in floating-point number.");

    return MakeTokenWith(TOKEN_LIT_FLT,
        .Flt = DecimalPlaces? 
            Integer + (Double / DecimalPlaces) 
            : Integer
    );
}

static Token ConsumeNumber(M68kAssembler *Assembler, char First)
{
    int64_t Integer = First - '0';
    while (IsNumber(*Assembler->CurrPtr) || '_' == *Assembler->CurrPtr)
    {
        char Char = *Assembler->CurrPtr++;
        if ('_' == Char) 
            continue;

        Integer *= 10;
        Integer += Char - '0';
    }
    if (IsAlpha(*Assembler->CurrPtr))
        return ErrorToken("Invalid character in integer.");

    if (ConsumeIfNextCharIs(Assembler, '.'))
    {
        return ConsumeFloatDecimalPlace(Assembler, Integer);
    }
    else
    {
        return MakeTokenWith(TOKEN_LIT_INT, 
            .Int = Integer
        );
    }
}

static Token ConsumeHex(M68kAssembler *Assembler)
{
    uint64_t Hex = 0;
    char Char = *Assembler->CurrPtr;
    while (!IsAtEnd(Assembler) && 
        ('_' == Char || IsNumber(Char) || IN_RANGE('A', TO_UPPER(Char), 'F')))
    {
        if ('_' != Char)
        {
            Hex *= 16;
            Hex += IsNumber(Char)?
                Char - '0'
                : TO_UPPER(Char) - 'A' + 10;
        }
        Char = *(++Assembler->CurrPtr);
    }
    if (IsAlpha(*Assembler->CurrPtr))
        return ErrorToken("Invalid character in hexadecimal number.");

    return MakeTokenWith(TOKEN_LIT_INT,
        .Int = Hex
    );
}

static Token ConsumeBinary(M68kAssembler *Assembler)
{
    uint64_t Binary = 0;
    char Char = *Assembler->CurrPtr;
    while (!IsAtEnd(Assembler) &&
          ('_' == Char || '1' == Char || '0' == Char))
    {
        if ('_' != Char)
        {
            Binary *= 2;
            Binary += '1' == Char;
        }
        Char = *(++Assembler->CurrPtr);
    }
    if (IsAlphaNum(*Assembler->CurrPtr))
        return ErrorToken("Invalid character in binary number.");

    return MakeTokenWith(TOKEN_LIT_INT,
        .Int = Binary
    );
}


#define INVALID_CONDITIONAL_CODE 16
static unsigned GetConditionalCodeFromMnemonic(char UpperSecond, char UpperThird)
{
    unsigned ConditionalCode = 16;
    switch (UpperSecond)
    {
    case 'T': ConditionalCode = 1; break;
    case 'F': ConditionalCode = 2; break;
    case 'H':
    {
        if ('I' == UpperThird)
            ConditionalCode = 2;
    } break;
    case 'L':
    {
        if ('S' == UpperThird) /* LS */
            ConditionalCode = 3;
        else if ('T' == UpperThird) /* LT */
            ConditionalCode = 13;
        else if ('E' == UpperThird) /* LE */
            ConditionalCode = 15;
    } break;
    case 'C':
    {
        if ('C' == UpperThird) /* CC */
            ConditionalCode = 4;
        else if ('S' == UpperThird) /* CS */
            ConditionalCode = 5;
    } break;
    case 'N':
    {
        if ('E' == UpperThird) /* NE */
            ConditionalCode = 6;
    } break;
    case 'E':
    {
        if ('Q' == UpperThird) /* EQ */
            ConditionalCode = 7;
    } break;
    case 'V':
    {
        if ('S' == UpperThird) /* VS */
            ConditionalCode = 9;
        else if ('C' == UpperThird) /* VC */
            ConditionalCode = 8;
    } break;
    case 'P':
    {
        if ('L' == UpperThird) /* PL */
            ConditionalCode = 10;
    } break;
    case 'M':
    {
        if ('I' == UpperThird) /* MI */
            ConditionalCode = 11;
    } break;
    case 'G':
    {
        if ('E' == UpperThird) /* GE */
            ConditionalCode = 12;
        else if ('T' == UpperThird) /* GT */
            ConditionalCode = 14;
    } break;
    }
    return ConditionalCode;
}


static bool StrSliceEquNoCase(const char *A, const char *B, size_t Len)
{
    while (Len != 0)
    {
        if (TO_UPPER(*A) != TO_UPPER(*B))
            return false;
        A++;
        B++;
        Len--;
    }
    return true;
}

static Token ConsumeIdentifier(M68kAssembler *Assembler, char FirstLetter)
{
    typedef struct Keyword 
    {
        const char Str[16 - 2];
        uint8_t Len;
        uint8_t Type;
    } Keyword;
#define KEYWORD(kw) \
    { .Str = { #kw }, .Len = sizeof(#kw) - 1, .Type = TOKEN_ ## kw }
#define INS(mnemonic) KEYWORD(mnemonic)
    static const Keyword Keywords['Z'][16] = {
        ['A'] = {
            INS(ABCD),
            INS(ADD),
            INS(ADDA),
            INS(ADDI),
            INS(ADDQ),
            INS(ADDX),
            INS(AND),
            INS(ANDI),
            INS(ASR), 
            INS(ASL),
        },
        ['B'] = {
            INS(Bcc),
            INS(BCHG),
            INS(BCLR),
            INS(BFCHG),
            INS(BFCLR),
            INS(BFEXTS),
            INS(BFEXTU),
            INS(BFFFO),
            INS(BFINS),
            INS(BFSET),
            INS(BFTST),
            INS(BKPT),
            INS(BRA),
            INS(BSET),
            INS(BSR),
            INS(BTST),
        },
        ['C'] = {
            INS(CALLM),
            INS(CAS),
            INS(CAS2),
            INS(CHK),
            INS(CHK2),
            INS(CLR),
            INS(CMP),
            INS(CMP2),
            INS(CMPA),
            INS(CMPI),
            INS(CMPM),
            KEYWORD(CCR),
            KEYWORD(SR),
        },
        ['D'] = {
            INS(DBcc),
            INS(DIVS),
            INS(DIVU),
            INS(DIVSL),
            INS(DIVUL),
        },
        ['E'] = {
            INS(EOR),
            INS(EORI),
            INS(EXT),
            INS(EXTB),
            INS(EXG),
        },
        ['I'] = {
            INS(ILLEGAL),
        },
        ['J'] = {
            INS(JMP),
            INS(JSR),
        },
        ['L'] = {
            INS(LEA),
            INS(LINK),
            INS(LSL),
            INS(LSR),
        },
        ['M'] = {
            INS(MOVE),
            INS(MOVEA),
            INS(MOVEM),
            INS(MOVEP),
            INS(MOVEQ),
            INS(MOVEC),
            INS(MOVES),
            INS(MULS),
            INS(MULU),
        },
        ['N'] = {
            INS(NBCD),
            INS(NEG),
            INS(NEGX),
            INS(NOP),
            INS(NOT),
        },
        ['O'] = {
            INS(OR),
            INS(ORI),
        },
        ['P'] = {
            INS(PACK),
            INS(PEA),
        },
        ['R'] = {
            INS(RESET),
            INS(ROL), 
            INS(ROR),
            INS(ROXL),
            INS(ROXR),
            INS(RTD),
            INS(RTE),
            INS(RTM),
            INS(RTR),
            INS(RTS),
            KEYWORD(REL),
        },
        ['S'] = {
            INS(SBCD),
            INS(Scc),
            INS(STOP),
            INS(SUB),
            INS(SUBA),
            INS(SUBI),
            INS(SUBQ),
            INS(SUBX),
            INS(SWAP),
        },
        ['T'] = {
            INS(TAS),
            INS(TRAP),
            INS(TRAPcc),
            INS(TRAPV),
            INS(TST),
        },
        ['U'] = {
            INS(UNLK),
            INS(UNPK),
        },
    };
#undef INS
#undef KEYWORD


    char UpperFirst = TO_UPPER(FirstLetter);
    char SecondLetter = PeekChar(Assembler);
    if (('A' == UpperFirst || 'D' == UpperFirst) 
    && IN_RANGE('0', SecondLetter, '8'))
    {
        Assembler->CurrPtr++;
        Token Register = MakeToken('A' == UpperFirst
                ? TOKEN_ADDR_REG
                : TOKEN_DATA_REG
        );
        Register.Data.Int = SecondLetter - '0';
        return Register;
    }


    /* consume the identifier */
    size_t Len = 1;
    while (!IsAtEnd(Assembler) && IsAlphaNum(*Assembler->CurrPtr))
    {
        Assembler->CurrPtr++;
        Len++;
    }



    /* determine its type */
    char UpperSecond = Len > 1? TO_UPPER(Assembler->StartPtr[1]) : '\0';
    char UpperThird = Len > 2? TO_UPPER(Assembler->StartPtr[2]) : '\0';
    /* Bcc? */
    if ('B' == UpperFirst && IN_RANGE(2, Len, 3))
    {
        unsigned ConditionalCode = GetConditionalCodeFromMnemonic(UpperSecond, UpperThird);
        if (INVALID_CONDITIONAL_CODE != ConditionalCode)
        {
            return MakeTokenWith(TOKEN_Bcc, 
                .ConditionalCode = ConditionalCode
            );
        }
    }
    /* DBcc? */
    if ('D' == UpperFirst && IN_RANGE(3, Len, 4) && 'B' == UpperSecond)
    {
        char UpperFourth = TO_UPPER(Assembler->StartPtr[3]);
        unsigned ConditionalCode = GetConditionalCodeFromMnemonic(UpperThird, UpperFourth);
        if (INVALID_CONDITIONAL_CODE != ConditionalCode)
        {
            return MakeTokenWith(TOKEN_DBcc, 
                .ConditionalCode = ConditionalCode
            );
        }
    }
    /* SP, Scc? */
    if ('S' == UpperFirst && IN_RANGE(2, Len, 3))
    {
        if ('P' == UpperSecond && 2 == Len)
        {
            return MakeTokenWith(TOKEN_ADDR_REG, 
                .Int = 7 /* SP, A7 */
            );
        }
        unsigned ConditionalCode = GetConditionalCodeFromMnemonic(UpperSecond, UpperThird);
        if (INVALID_CONDITIONAL_CODE != ConditionalCode)
        {
            return MakeTokenWith(TOKEN_Scc, 
                .ConditionalCode = ConditionalCode
            );
        }
    }
    /* TRAPcc? */
    if ('T' == UpperFirst && IN_RANGE(5, Len, 6) && StrSliceEquNoCase(Assembler->StartPtr + 1, "RAP", 3))
    {
        unsigned ConditionalCode = GetConditionalCodeFromMnemonic(
                TO_UPPER(Assembler->StartPtr[4]), TO_UPPER(Assembler->StartPtr[5])
        );
        if (INVALID_CONDITIONAL_CODE != ConditionalCode)
        {
            return MakeTokenWith(TOKEN_TRAPcc, 
                .ConditionalCode = ConditionalCode
            );
        }
    }
    if ('_' != FirstLetter)
    {
        const Keyword *PossibleKeywords = Keywords[(uint8_t)UpperFirst];
        for (unsigned i = 0; PossibleKeywords[i].Len && i < STATIC_ARRAY_SIZE(Keywords[0]); i++)
        {
            if (Len == PossibleKeywords[i].Len 
            && StrSliceEquNoCase(PossibleKeywords[i].Str, Assembler->StartPtr, Len))
            {
                return MakeToken(PossibleKeywords[i].Type);
            }
        }
    }
    return MakeToken(TOKEN_IDENTIFIER);
}



static Token Tokenize(M68kAssembler *Assembler)
{
    char Current = ConsumeSpace(Assembler);
    if (IsAlpha(Current))
    {
        return ConsumeIdentifier(Assembler, Current);
    }
    if (IsNumber(Current))
    {
        return ConsumeNumber(Assembler, Current);
    }
    switch (Current)
    {
    case '\0': return MakeToken(TOKEN_EOF);
    case '$': return ConsumeHex(Assembler);
    case '+': return MakeToken(TOKEN_PLUS);
    case '-': return MakeToken(TOKEN_MINUS);
    case '/': return MakeToken(TOKEN_SLASH);
    case '*': return MakeToken(TOKEN_STAR);
    case '&': return MakeToken(TOKEN_AMPERSAND);
    case '|': return MakeToken(TOKEN_BAR);
    case '#': return MakeToken(TOKEN_POUND);
    case ',': return MakeToken(TOKEN_COMMA);
    case '^': return MakeToken(TOKEN_CARET);
    case '~': return MakeToken(TOKEN_TILDE);
    case '.': 
    {
        if (IsNumber(PeekChar(Assembler)))
            return ConsumeFloatDecimalPlace(Assembler, 0);
        return MakeToken(TOKEN_DOT);
    } break;
    case '=': 
    {
        if (ConsumeIfNextCharIs(Assembler, '='))
            return MakeToken(TOKEN_EQUAL_EQUAL);
        return MakeToken(TOKEN_EQUAL);
    } break;
    case '!': 
    {
        if (ConsumeIfNextCharIs(Assembler, '='))
            return MakeToken(TOKEN_BANG_EQUAL);
    } break;
    case '%':
    {
        if (IsNumber(PeekChar(Assembler)))
            return ConsumeBinary(Assembler);
        return MakeToken(TOKEN_PERCENT);
    } break;
    case ':': return MakeToken(TOKEN_COLON);
    case '<':
    {
        if (ConsumeIfNextCharIs(Assembler, '<'))
            return MakeToken(TOKEN_LSHIFT);
        if (ConsumeIfNextCharIs(Assembler, '='))
            return MakeToken(TOKEN_LESS_EQUAL);
        return MakeToken(TOKEN_LESS);
    } break;
    case '>':
    {
        if (ConsumeIfNextCharIs(Assembler, '-'))
            return MakeToken(TOKEN_RSHIFT_ARITH);
        if (ConsumeIfNextCharIs(Assembler, '>'))
            return MakeToken(TOKEN_RSHIFT);
        if (ConsumeIfNextCharIs(Assembler, '='))
            return MakeToken(TOKEN_GREATER_EQUAL);
        return MakeToken(TOKEN_GREATER);
    } break;
    case '(': return MakeToken(TOKEN_LPAREN);
    case ')': return MakeToken(TOKEN_RPAREN);
    case '{': return MakeToken(TOKEN_LCURLY);
    case '}': return MakeToken(TOKEN_RCURLY);
    case '[': return MakeToken(TOKEN_LBRACE);
    case ']': return MakeToken(TOKEN_RBRACE);
    }

    return ErrorToken("Invalid character.");
}





static void Highlight(FILE *ErrorStream, StringView Offender, StringView Line)
{
    const char *p = Line.Ptr;
    while (p++ < Offender.Ptr)
    {
        fputc(' ', ErrorStream);
    }
    while (p++ <= Offender.Ptr + Offender.Len)
    {
        fputc('^', ErrorStream);
    }
}

static void Display(FILE *ErrorStream, int LineNumber, 
        const char *MsgType, const char *FileName, 
        StringView Offender, StringView Line, 
        const char *Fmt, va_list Args)
{
    if (NULL != ErrorStream)
    {
        int Offset = Offender.Ptr - Line.Ptr + 1;
        fprintf(ErrorStream, 
                "\n%s [Line %d, %d]:\n"
                "   | "STRVIEW_FMT"\n"
                "   | ", 
                FileName, LineNumber, Offset,
                STRVIEW_FMT_ARG(Line)
        );
        Highlight(ErrorStream, Offender, Line);
        fprintf(ErrorStream, "\n   | %s: ", MsgType);
        vfprintf(ErrorStream, Fmt, Args);
        fputc('\n', ErrorStream);
    }
}

static size_t LineLen(const char *s)
{
    size_t i = 0;
    while (s[i] && s[i] != '\n' && s[i] != '\r')
        i++;
    return i;
}

static void WarnAtToken(M68kAssembler *Assembler, const Token *Tok, const char *Fmt, ...)
{
    if (Assembler->Panic)
        return;

    va_list Args;
    va_start(Args, Fmt);

    StringView Line = {.Ptr = Tok->Lexeme.Ptr - Tok->Offset + 1};
    Line.Len = LineLen(Line.Ptr);
    Display(Assembler->ErrorStream, Assembler->LineCount, 
            "Warning", Assembler->SourceName, 
            Tok->Lexeme, Line, 
            Fmt, Args
    );

    va_end(Args);
}


static void ErrorAtArgs(M68kAssembler *Assembler, StringView Offender, StringView Line, const char *Fmt, va_list Args)
{
    if (Assembler->Panic)
        return;

    Display(Assembler->ErrorStream, Assembler->LineCount, 
            "Error", Assembler->SourceName, 
            Offender, Line, 
            Fmt, Args
    );
    Assembler->Error = true;
    Assembler->Panic = true;
}

static void ErrorAtTokenArgs(M68kAssembler *Assembler, const Token *Tok, const char *Fmt, va_list Args)
{
    StringView Line = {.Ptr = Tok->Lexeme.Ptr - Tok->Offset + 1};
    Line.Len = LineLen(Line.Ptr);
    ErrorAtArgs(Assembler, Tok->Lexeme, Line, Fmt, Args);
}

static void Error(M68kAssembler *Assembler, const char *Fmt, ...)
{
    va_list Args;
    va_start(Args, Fmt);
    ErrorAtTokenArgs(Assembler, &Assembler->CurrentToken, Fmt, Args);
    va_end(Args);
}

static void EmptyError(M68kAssembler *Assembler, const char *Fmt, ...)
{
    va_list Args;
    va_start(Args, Fmt);
    
    Assembler->Error = true;
    Assembler->Panic = true;
    if (NULL != Assembler->ErrorStream)
    {
        vfprintf(Assembler->ErrorStream, Fmt, Args);
        fputc('\n', Assembler->ErrorStream);
    }

    va_end(Args);
}

static void ErrorAtToken(M68kAssembler *Assembler, const Token *Tok, const char *Fmt, ...)
{
    va_list Args;
    va_start(Args, Fmt);
    ErrorAtTokenArgs(Assembler, Tok, Fmt, Args);
    va_end(Args);
}

static void ErrorAtExpr(M68kAssembler *Assembler, const char *Fmt, ...)
{
    va_list Args;
    va_start(Args, Fmt);
    const Expression *Current = &Assembler->Expr[Assembler->ExprCount - 1];
    if (0 == Assembler->ExprCount)
    {
        UNREACHABLE("Must have an expression before %s", __func__);
    }

    StringView Line = {.Ptr = Current->Str.Ptr - Current->Offset + 1};
    Line.Len = LineLen(Line.Ptr);
    ErrorAtArgs(Assembler, Current->Str, Line, Fmt, Args);
    va_end(Args);
}

static const char *LookupAddrModeName(ArgumentType AddressingMode)
{
    static const char *Lut[] = {
        [ARG_INVALID]       = "Invalid",
        [ARG_IMMEDIATE]     = "Immediate",  /* #Imm */
        [ARG_ADDR]          = "Absolute",       /* Abs Addr */
        [ARG_DATA_REG]      = "Data Register",   /* Dn */
        [ARG_ADDR_REG]      = "Address Register",   /* An */
        [ARG_IND_REG]       = "Register Indirect",    /* (An) */
        [ARG_IND_PREDEC]    = "Predecrement Indirect", /* -(An) */
        [ARG_IND_POSTINC]   = "Postincrement Indirect",/* (An)+ */

        [ARG_IND_I16]       = "Indirect",    /* (I16, An) */
        [ARG_IDX_I8]        = "Indexed Indirect",     /* (I8, An, Xn) */
        [ARG_IDX_BD]        = "Indexed Indirect",     /* (Bd, An, Xn) */
        [ARG_MEM_PRE]       = "Preindexed Memory Indirect",    /* ([Bd, An, Xn], Od) */
        [ARG_MEM_POST]      = "Postindexed Memory Indirect",   /* ([Bd, An], Xn, Od) */
        [ARG_MEM]           = "Memory Indirect",

        [ARG_PC_I16]        = "PC-Relative",
        [ARG_PC_IDX_I8]     = "Indexed PC-Relative",
        [ARG_PC_BD]         = "Indexed PC-Relative",
        [ARG_PC_MEM_POST]   = "Postindexed PC-Relative Indirect",
        [ARG_PC_MEM_PRE]    = "Preindexed PC-Relative Indirect",
        [ARG_PC_MEM]        = "PC-Relative Indirect",
    };
    return Lut[AddressingMode];
}

static void ErrorInvalidAddrMode(M68kAssembler *Assembler, 
        const Token *Instruction, ArgumentType AddrMode, const char *Location)
{
    if (NULL == Location)
    {
        ErrorAtToken(Assembler, Instruction, STRVIEW_FMT" does not have %s addressing mode.",
            STRVIEW_FMT_ARG(Instruction->Lexeme),
            LookupAddrModeName(AddrMode)
        );
    }
    else
    {
        ErrorAtToken(Assembler, Instruction, STRVIEW_FMT" does not have %s addressing mode %s.",
            STRVIEW_FMT_ARG(Instruction->Lexeme),
            LookupAddrModeName(AddrMode),
            Location
        );
    }
}









#define NoMoreToken(Assembler) (TOKEN_EOF == (Assembler)->NextToken.Type)


static TokenType ConsumeToken(M68kAssembler *Assembler)
{
    Assembler->CurrentToken = Assembler->NextToken;
    Assembler->NextToken = Tokenize(Assembler);
    if (TOKEN_ERROR == Assembler->NextToken.Type)
    {
        ErrorAtToken(Assembler, &Assembler->NextToken, STRVIEW_FMT, STRVIEW_FMT_ARG(Assembler->NextToken.Data.String));
    }
    return Assembler->CurrentToken.Type;
}

#define NextTokenIs(Typ) (Assembler->NextToken.Type == (Typ))
#define NextTokenIsIndexRegister(Assembler) (NextTokenIs(TOKEN_ADDR_REG) || NextTokenIs(TOKEN_DATA_REG))

static bool ConsumeIfNextTokenIs(M68kAssembler *Assembler, TokenType Type)
{
    if (Assembler->NextToken.Type == Type)
    {
        ConsumeToken(Assembler);
        return true;
    }
    return false;
}



/* returns true if the next token's type is the same as Type
 * else does not consume the token */
static bool ConsumeOrError(M68kAssembler *Assembler, TokenType Type, const char *Fmt, ...)
{
    if (!ConsumeIfNextTokenIs(Assembler, Type))
    {
        va_list Args;
        va_start(Args, Fmt);
        ErrorAtTokenArgs(Assembler, &Assembler->NextToken, Fmt, Args);
        va_end(Args);
        return false;
    }
    return true;
}



static bool Find(M68kAssembler *Assembler, StringView Identifier, Value *Out)
{
    for (unsigned i = 0; i < Assembler->IdenCount; i++)
    {
        if (Identifier.Len == Assembler->Idens[i].Len 
        && StrSliceEquNoCase(Identifier.Ptr, Assembler->Idens[i].Ptr, Identifier.Len))
        {
            if (Assembler->IsFloat[i / 32] & (1ul << (i % 32)))
            {
                Out->IsFloat = true;
                Out->As.Flt = Assembler->IdenData[i].Flt;
            }
            else
                Out->As.Int = Assembler->IdenData[i].Int;
            return true;
        }
    }
    return false;
}


static Value ConstExpr(M68kAssembler *Assembler);

static Value Factor(M68kAssembler *Assembler)
{
    switch (ConsumeToken(Assembler))
    {
    case TOKEN_LIT_INT: return (Value) { .As.Int = Assembler->CurrentToken.Data.Int };
    case TOKEN_LIT_FLT: return (Value) { .As.Flt = Assembler->CurrentToken.Data.Flt, .IsFloat = true };
    case TOKEN_IDENTIFIER:
    {
        Value Val = { 0 };
        if (!Find(Assembler, Assembler->CurrentToken.Lexeme, &Val))
        {
            Error(Assembler, "Undefined symbol '"STRVIEW_FMT"'.", STRVIEW_FMT_ARG(Assembler->CurrentToken.Lexeme));
        }
        return Val;
    } break;

    case TOKEN_MINUS:
    {
        Value Val = Factor(Assembler);
        if (Val.IsFloat)
            Val.As.Flt = -Val.As.Flt;
        else
            Val.As.Int = -Val.As.Int;
        return Val;
    } break;
    case TOKEN_PLUS:
    {
        return Factor(Assembler);
    } break;
    case TOKEN_TILDE:
    {
        Value Val = Factor(Assembler);
        if (Val.IsFloat)
            Error(Assembler, "'~' is not applicable to floating-point number.");
        Val.As.Int = ~Val.As.Int;
        return Val;
    } break;
    case TOKEN_LPAREN:
    {
        Value Val = ConstExpr(Assembler);
        ConsumeOrError(Assembler, TOKEN_RPAREN, "Expected ')' after expression.");
        return Val;
    } break;
    default:
    {
        Error(Assembler, "Expected expression.");
    } break;
    }
    return (Value) { 0 };
}


#define BIN_OP(VLeft, Op, Val) do {\
    Value Right = Val;\
    if ((VLeft).IsFloat) {\
        double Flt = (Right).IsFloat ? (Right).As.Flt : (Right).As.Int;\
        (VLeft).As.Flt = (VLeft).As.Flt Op Flt;\
    } else if ((Right).IsFloat) {\
        double Dst = (VLeft).As.Int;\
        (VLeft).As.Flt = Dst Op (Right).As.Flt;\
    } else {\
        (VLeft).As.Int = (VLeft).As.Int Op (Right).As.Int;\
    }\
} while (0)

#define INT_ONLY(VLeft, Op, Val, ...) do {\
    Value Right = Val;\
    if ((VLeft).IsFloat || Right.IsFloat) {\
        Error(Assembler, __VA_ARGS__);\
        return (VLeft);\
    }\
    (VLeft).As.Int Op##= Right.As.Int;\
} while (0)

static Value ExprMulDiv(M68kAssembler *Assembler)
{
    Value Left = Factor(Assembler);
    while (!Assembler->Panic && !NoMoreToken(Assembler))
    {
        if (ConsumeIfNextTokenIs(Assembler, TOKEN_STAR))
        {
            BIN_OP(Left, *, Factor(Assembler));
        }
        else if (ConsumeIfNextTokenIs(Assembler, TOKEN_SLASH))
        {
            Value R = Factor(Assembler);
            if (R.As.Int == 0)
            {
                Error(Assembler, "Division by 0");
                break;
            }
            BIN_OP(Left, /, R);
        }
        else if (ConsumeIfNextTokenIs(Assembler, TOKEN_PERCENT))
        {
            Value R = Factor(Assembler);
            if (R.As.Int == 0)
            {
                Error(Assembler, "Division by 0");
                break;
            }
            INT_ONLY(Left, %, R, "Cannot perform modulo on floating-point number.");
        }
        else break;
    }
    return Left;
}

static Value ExprAddSub(M68kAssembler *Assembler)
{
    Value Left = ExprMulDiv(Assembler);
    while (!Assembler->Panic && !NoMoreToken(Assembler))
    {
        if (ConsumeIfNextTokenIs(Assembler, TOKEN_PLUS))
            BIN_OP(Left, +, ExprMulDiv(Assembler));
        else if (ConsumeIfNextTokenIs(Assembler, TOKEN_MINUS))
            BIN_OP(Left, -, ExprMulDiv(Assembler));
        else break;
    }
    return Left;
}

static Value ExprBitwise(M68kAssembler *Assembler)
{
    Value Left = ExprAddSub(Assembler);
    while (!Assembler->Panic && !NoMoreToken(Assembler))
    {
        if (ConsumeIfNextTokenIs(Assembler, TOKEN_LSHIFT))
        {
            INT_ONLY(Left, <<, ExprAddSub(Assembler), "Cannot perform '<<' on floating-point number.");
        }
        else if (ConsumeIfNextTokenIs(Assembler, TOKEN_RSHIFT))
        {
            INT_ONLY(Left, >>, ExprAddSub(Assembler), "Cannot perform '>>' on floating-point number.");
        }
        else if (ConsumeIfNextTokenIs(Assembler, TOKEN_RSHIFT_ARITH))
        {
            Value Right = ExprAddSub(Assembler);
            if (Left.IsFloat || Right.IsFloat)
            {
                Error(Assembler, "Cannot perform '>-' on floating-point number.");
                return Left;
            }
            uint64_t Sign = Left.As.Int & (1ull << 63);
            Left.As.Int = Left.As.Int >> Right.As.Int;
            /* sign extend */
            if (Sign)
            {
                Left.As.Int |= ~((Sign >> Right.As.Int) - 1);
            }
        }
        else break;
    }
    return Left;
}

static Value ExprEquality(M68kAssembler *Assembler)
{
    Value Left = ExprBitwise(Assembler);
    while (!Assembler->Panic && !NoMoreToken(Assembler))
    {
        if (ConsumeIfNextTokenIs(Assembler, TOKEN_EQUAL_EQUAL))
            BIN_OP(Left, !=, ExprBitwise(Assembler));
        else if (ConsumeIfNextTokenIs(Assembler, TOKEN_BANG_EQUAL))
            BIN_OP(Left, ==, ExprBitwise(Assembler));
        else break;
    }
    return Left;
}

static Value ExprAnd(M68kAssembler *Assembler)
{
    Value Left = ExprEquality(Assembler);
    while (!Assembler->Panic && !NoMoreToken(Assembler) && ConsumeIfNextTokenIs(Assembler, TOKEN_AMPERSAND))
    {
        INT_ONLY(Left, &, ExprEquality(Assembler), "Cannot perform '&' on floating-point number.");
    }
    return Left;
}

static Value ExprXor(M68kAssembler *Assembler)
{
    Value Left = ExprAnd(Assembler);
    while (!Assembler->Panic && !NoMoreToken(Assembler) && ConsumeIfNextTokenIs(Assembler, TOKEN_CARET))
    {
        INT_ONLY(Left, ^, ExprAnd(Assembler), "Cannot perform '^' on floating-point number.");
    }
    return Left;
}

static Value ExprOr(M68kAssembler *Assembler)
{
    Value Left = ExprXor(Assembler);
    while (!Assembler->Panic && !NoMoreToken(Assembler) && ConsumeIfNextTokenIs(Assembler, TOKEN_BAR))
    {
        INT_ONLY(Left, |, ExprXor(Assembler), "Cannot perform '|' on floating-point number.");
    }
    return Left;
}

static Value ConstExpr(M68kAssembler *Assembler)
{
    if (Assembler->ExprCount > STATIC_ARRAY_SIZE(Assembler->Expr))
    {
        UNREACHABLE("TODO: make expr array dynamic %s", __func__);
    }
    Expression Expr = {
        .Str = Assembler->NextToken.Lexeme,
        .Line = Assembler->LineCount,
        .Offset = Assembler->NextToken.Offset,
    };

    Value Val = ExprOr(Assembler);

    Expr.Str.Len = 
        Assembler->CurrentToken.Lexeme.Ptr + Assembler->CurrentToken.Lexeme.Len 
        - Expr.Str.Ptr;
    Assembler->Expr[Assembler->ExprCount++] = Expr;
    return Val;
}

static uint32_t IntExpr(M68kAssembler *Assembler, const char *ExprName)
{
    Value Expr = ConstExpr(Assembler);
    if (Expr.IsFloat)
    {
        ErrorAtExpr(Assembler, "%s cannot be a floating-point number.", ExprName);
    }
    return Expr.As.Int;
}


static void PushSymbol(M68kAssembler *Assembler, const Token *Sym, Value Val, bool IsLabel)
{
    if (Assembler->IdenCount >= STATIC_ARRAY_SIZE(Assembler->Idens))
        UNREACHABLE("TODO: make Symbol table dynamic");

    unsigned i = Assembler->IdenCount++;
    Assembler->Idens[i] = Sym->Lexeme;
    if (Val.IsFloat)
    {
        Assembler->IsFloat[i/32] |= 1ul << (i % 32);
        Assembler->IdenData[i].Flt = Val.As.Flt;
    }
    else 
    {
        Assembler->IdenData[i].Int = Val.As.Int;
    }

    if (IsLabel)
    {
        Assembler->IsLabel[i/32] |= 1ul << (i % 32);
    }
}

static void DeclStmt(M68kAssembler *Assembler)
{
    Token Identifier = Assembler->CurrentToken;
    Value Val = { 0 };

    if (ConsumeIfNextTokenIs(Assembler, TOKEN_EQUAL))
    {
        Val = ConstExpr(Assembler);
        PushSymbol(Assembler, &Identifier, Val, false);
    }
    else if (ConsumeIfNextTokenIs(Assembler, TOKEN_COLON))
    {
        Val = (Value){
            .IsFloat = false, 
            .As.Int = Assembler->MachineCode.Size 
        };
        PushSymbol(Assembler, &Identifier, Val, true);
    }
    else
    {
        Error(Assembler, "Expected '=' or ':' after identifier.");
    }
}




static unsigned ConsumeSizeSpecifier(M68kAssembler *Assembler)
{
    unsigned Size = 0;
    ConsumeOrError(Assembler, TOKEN_IDENTIFIER, "Expected operand size specifier.");
    Token Letter = Assembler->CurrentToken;
    if (Letter.Lexeme.Len != 1)
        goto UnknownSize;

    switch (TO_UPPER(Letter.Lexeme.Ptr[0]))
    {
    case 'L': Size = 4; break;
    case 'W': Size = 2; break;
    case 'B': Size = 1; break;
    UnknownSize:
    default: Error(Assembler, "Expected operand size specifier to be 'b', 'w' or 'l'.");
    }
    return Size;
}

/* Next token is TOKEN_ADDR_REG or TOKEN_DATA_REG */
static XnReg ConsumeIndexRegister(M68kAssembler *Assembler)
{
    XnReg X = { 0 };
    if (ConsumeIfNextTokenIs(Assembler, TOKEN_ADDR_REG))
    {
        X.n = Assembler->CurrentToken.Data.Int + 8;
    }
    else if (ConsumeOrError(Assembler, TOKEN_DATA_REG, "Expected register name."))
    {
        X.n = Assembler->CurrentToken.Data.Int;
    }

    if (ConsumeIfNextTokenIs(Assembler, TOKEN_DOT))
    {
        X.Size = ConsumeSizeSpecifier(Assembler);
        if (X.Size == 1)
        {
            Error(Assembler, "Cannot use byte size specifier for index register.");
        }
    }

    if (ConsumeIfNextTokenIs(Assembler, TOKEN_STAR))
    {
        unsigned Scale = IntExpr(Assembler, "Index scalar");

        if (Scale != 1 && Scale != 2 
        && Scale != 4 && Scale != 8)
        {
            ErrorAtExpr(Assembler, "Scale factor must be 1, 2, 4, or 8.");
        }
        X.Scale = CountBits(Scale - 1);
    }
    return X;
}


/* '[' is the current token */
static Argument ConsumeMemoryIndirect(M68kAssembler *Assembler)
{
    int32_t BaseDisplacement = 0;
    if (!ConsumeIfNextTokenIs(Assembler, TOKEN_ADDR_REG)) /* [Expr, An ...] */
    {
        BaseDisplacement = IntExpr(Assembler, "Base Displacement");
        ConsumeOrError(Assembler, TOKEN_COMMA, "Expected ',' after expression.");
        ConsumeOrError(Assembler, TOKEN_ADDR_REG, "Expected address register.");
    }

    unsigned An = Assembler->CurrentToken.Data.Int;
    XnReg Xn = { .n = NO_REG };
    ArgumentType Type = ARG_MEM;

    if (ConsumeIfNextTokenIs(Assembler, TOKEN_COMMA)) /* [Expr, An, Xn] */
    {
        Xn = ConsumeIndexRegister(Assembler);
        Type = ARG_MEM_PRE;
    }

    return ARGUMENT(
        Type,
        .As.Mem = {
            .An = An,
            .X = Xn,
            .Bd = BaseDisplacement,
        }
    );
}



/* '(' is the current token */
static Argument IndirectAddressingMode(M68kAssembler *Assembler)
{
    if (ConsumeIfNextTokenIs(Assembler, TOKEN_LBRACE)) /* ([...] ...) */
    {
        /* ([...], ...) */
        Argument Arg = ConsumeMemoryIndirect(Assembler);
        ConsumeOrError(Assembler, TOKEN_RBRACE, "Expected ']' after memory indirection.");

        if (ConsumeIfNextTokenIs(Assembler, TOKEN_COMMA)) /* ([], ...) */
        {
            if (NextTokenIsIndexRegister(Assembler)) /* ([], Xn ...) */
            {
                if (Arg.Type == ARG_MEM_PRE)
                {
                    ErrorAtToken(Assembler, &Assembler->NextToken, 
                        "Too many index field for Memory Indirect addressing mode."
                    );
                }

                Arg.Type = ARG_MEM_POST;
                Arg.As.Mem.X = ConsumeIndexRegister(Assembler);
                if (!ConsumeIfNextTokenIs(Assembler, TOKEN_COMMA))
                    goto NoOuterDisplacement;
            }

            /* ([], Xn, Expr) */
            Arg.As.Mem.Od = IntExpr(Assembler, "Outer Displacement");
        }
NoOuterDisplacement:
        ConsumeOrError(Assembler, TOKEN_RPAREN, "Expected ')'.");
        return Arg;
    }
    else if (ConsumeIfNextTokenIs(Assembler, TOKEN_ADDR_REG)) /* (An ...)... */
    {
        unsigned An = Assembler->CurrentToken.Data.Int;
        if (ConsumeIfNextTokenIs(Assembler, TOKEN_COMMA)) /* (An, Xn) */
        {
            XnReg Xn = ConsumeIndexRegister(Assembler);
            ConsumeOrError(Assembler, TOKEN_RPAREN, "Expected ')' after index field.");
            return ARGUMENT(
                ARG_IDX_I8,
                .As.IdxI8 = {
                    .An = An,
                    .X = Xn,
                    .I8 = 0,
                }
            );
        }
        ConsumeOrError(Assembler, TOKEN_RPAREN, "Expected ')' or ',' after address register."); /* (An)... */
        if (ConsumeIfNextTokenIs(Assembler, TOKEN_PLUS)) /* (An)+ */
        {
            return ARGUMENT(
                ARG_IND_POSTINC,
                .As.PostInc.An = An
            );
        }

        /* (An) */
        return ARGUMENT(
            ARG_IND_REG,
            .As.Ind.An = An
        );
    }
    else /* (Expr ...) */
    {
        int32_t Displacement = IntExpr(Assembler, "Displacement");

        if (ConsumeIfNextTokenIs(Assembler, TOKEN_RPAREN)) /* (Expr) */
        {
            return ARGUMENT(
                ARG_ADDR,
                .As.Addr = Displacement
            );
        } 

        /* (Expr, An ...) */
        ConsumeOrError(Assembler, TOKEN_COMMA, "Expected ')' or ',' after expression.");
        ConsumeOrError(Assembler, TOKEN_ADDR_REG, "Expected address register.");
        unsigned An = Assembler->CurrentToken.Data.Int;
        if (ConsumeIfNextTokenIs(Assembler, TOKEN_COMMA)) /* (Expr, An, Xn) */
        {
            XnReg Xn = ConsumeIndexRegister(Assembler);
            ConsumeOrError(Assembler, TOKEN_RPAREN, "Expected ')' after index.");
            if (IN_I8(Displacement)) /* (I8, An, Xn) */
            {
                return ARGUMENT(
                    ARG_IDX_I8,
                    .As.IdxI8 = {
                        .An = An,
                        .X = Xn,
                        .I8 = Displacement
                    }
                );
            }
            /* (Bd, An, Xn) */
            return ARGUMENT(
                ARG_IDX_BD,
                .As.Idx = {
                    .Bd = Displacement,
                    .An = An,
                    .X = Xn
                }
            );
        }
        /* (Expr, An) */
        ConsumeOrError(Assembler, TOKEN_RPAREN, "Expected ')' or ','.");
        if (IN_I16(Displacement)) /* (I16, An) */
        {
            return ARGUMENT(
                ARG_IND_I16,
                .As.Ind = {
                    .An = An,
                    .X.n = NO_REG,
                    .Displacement = Displacement
                },
            );
        }
        /* (Bd, An) */
        return ARGUMENT(
            ARG_IDX_BD,
            .As.Idx = {
                .An = An,
                .X.n = NO_REG,
                .Bd = Displacement
            }
        );
    }
}

static Argument PCRelativeAddressingMode(M68kAssembler *Assembler, uint32_t PC)
{
    Argument Arg = { 0 };
    ConsumeOrError(Assembler, TOKEN_LPAREN, "Expected '(' after rel.");

    if (ConsumeIfNextTokenIs(Assembler, TOKEN_LBRACE)) /* rel ([...] ...) */
    {
        Arg.Type = ARG_PC_MEM;
        Arg.As.PC.Mem.Bd = IntExpr(Assembler, "Base Displacement") - PC;
        if (ConsumeIfNextTokenIs(Assembler, TOKEN_COMMA))
        {
            Arg.Type = ARG_PC_MEM_PRE;
            Arg.As.PC.Mem.X = ConsumeIndexRegister(Assembler);
        }
        ConsumeOrError(Assembler, TOKEN_RBRACE, "Expected ']'.");

        if (ConsumeIfNextTokenIs(Assembler, TOKEN_COMMA))
        {
            if (NextTokenIsIndexRegister(Assembler))
            {
                if (Arg.Type == ARG_PC_MEM_PRE)
                {
                    Error(Assembler, "Too many index registers for PC-relative Memory Indirect addressing mode.");
                }
                Arg.Type = ARG_PC_MEM_POST;
                Arg.As.PC.Mem.X = ConsumeIndexRegister(Assembler);
                if (!ConsumeIfNextTokenIs(Assembler, TOKEN_COMMA))
                    goto NoOuterDisplacement;
            }

            Arg.As.PC.Mem.Od = IntExpr(Assembler, "Outer Displacement");
        }
NoOuterDisplacement:
        /* C is stupid */;
    }
    else if (NextTokenIsIndexRegister(Assembler)) /* rel (Xn) */
    {
        Arg = ARGUMENT(
            ARG_PC_IDX_I8,
            .As.PC.Idx = {
                .I8 = 0,
                .X = ConsumeIndexRegister(Assembler)
            }
        );
    }
    else
    {
        int32_t BaseDisplacement = IntExpr(Assembler, "Base Displacement") - PC;
        if (ConsumeIfNextTokenIs(Assembler, TOKEN_COMMA)) /* (Bd, Xn) */
        {
            XnReg Xn = ConsumeIndexRegister(Assembler);
            if (IN_I8(BaseDisplacement)) /* (I8, Xn) */
            {
                Arg = ARGUMENT(
                    ARG_PC_IDX_I8, 
                    .As.PC.Idx = {
                        .X = Xn,
                        .I8 = BaseDisplacement
                    }
                );
            }
            else /* (Bd, Xn) */
            {
                Arg = ARGUMENT(
                    ARG_PC_BD, 
                    .As.PC.Mem = {
                        .Bd = BaseDisplacement,
                        .X = Xn,
                    }
                );
            }
        }
        else if (IN_I16(BaseDisplacement)) /* (I16) */
        {
            Arg = ARGUMENT(
                ARG_PC_I16,
                .As.PC.I16 = BaseDisplacement
            );
        }
        else /* (Bd) */
        {
            Arg = ARGUMENT(
                ARG_PC_BD,
                .As.PC.Mem = {
                    .Bd = BaseDisplacement,
                    .X.n = NO_REG,
                }
            );
        }
    }

    ConsumeOrError(Assembler, TOKEN_RPAREN, "Expected ')' after addressing mode.");
    return Arg;
}




static Argument ConsumeEa(M68kAssembler *Assembler, unsigned InstructionSize)
{
    switch (ConsumeToken(Assembler))
    {
    default:                /* Addr */
    {
        return ARGUMENT(
            ARG_ADDR,
            .As.Addr = IntExpr(Assembler, "Address")
        );
    } break;
    case TOKEN_POUND:       /* #Imm */
    {
        return ARGUMENT(
            ARG_IMMEDIATE,
            .As.Immediate = IntExpr(Assembler, "Immediate") 
        );
    } break;
    case TOKEN_ADDR_REG:    /* An */
    {
        return ARGUMENT(
            ARG_ADDR_REG,
            .As.An = Assembler->CurrentToken.Data.Int
        );
    } break;
    case TOKEN_DATA_REG:    /* Dn */
    {
        return ARGUMENT(
            ARG_DATA_REG,
            .As.Dn = Assembler->CurrentToken.Data.Int
        );
    } break;
    case TOKEN_LPAREN:      /* (...) */
    {
        return IndirectAddressingMode(Assembler);
    } break;
    case TOKEN_MINUS:
    {
        ConsumeOrError(Assembler, TOKEN_LPAREN, "Expected '(' after '-'.");
        ConsumeOrError(Assembler, TOKEN_ADDR_REG, "Expected address register.");
        ConsumeOrError(Assembler, TOKEN_RPAREN, "Expected ')' after register.");
        return ARGUMENT(
            ARG_IND_PREDEC,
            .As.PreDec.An = Assembler->CurrentToken.Data.Int
        );
    } break;
    case TOKEN_REL:         /* rel (...) */
    {
        return PCRelativeAddressingMode(Assembler, Assembler->PC + InstructionSize);
    } break;
    }
}


static unsigned ConsumeAddrReg(M68kAssembler *Assembler)
{
    ConsumeOrError(Assembler, TOKEN_ADDR_REG, "Expected address register.");
    return Assembler->CurrentToken.Data.Int;
}

static unsigned ConsumeDataReg(M68kAssembler *Assembler)
{
    ConsumeOrError(Assembler, TOKEN_DATA_REG, "Expected data register.");
    return Assembler->CurrentToken.Data.Int;
}

static Argument ConsumeImmediate(M68kAssembler *Assembler)
{
    ConsumeOrError(Assembler, TOKEN_POUND, "Expected '#'.");
    return ARGUMENT(
        ARG_IMMEDIATE,
        .As.Immediate = IntExpr(Assembler, "Immediate")
    );
}


static unsigned EncodeExtensionSize(uint32_t Displacement)
{
    if (0 == Displacement)
        return 1;
    if (IN_I16((int32_t)Displacement))
        return 2;
    return 3;
}

static EaEncoding EncodeEa(Argument Arg)
{
#define ENCODE_FULL_EXTENSION(Xn, WL, SCALE, BS, IS, BD_SIZE, I)\
    ( ((uint32_t)((Xn)        & 0xF) << 12)\
    | ((uint32_t)((WL)        & 0x1) << 11)\
    | ((uint32_t)((SCALE)     & 0x3) << 9)\
    | 0x100\
    | ((uint32_t)((BS)        & 0x1) << 7)\
    | ((uint32_t)((IS)        & 0x1) << 6)\
    | ((uint32_t)((BD_SIZE)   & 0x3) << 4)\
    | ((uint32_t)(I)          & 0x7)) 
#define ENCODE_BRIEF_EXTENSION(Xn, WL, SCALE, Imm8)\
    ( ((uint32_t)((Xn)        & 0xF) << 12)\
    | ((uint32_t)((WL)        & 0x1) << 11)\
    | ((uint32_t)((SCALE)     & 0x3) << 9)\
    | 0x000\
    | ((uint32_t)(Imm8)       & 0xFF)) 

    EaEncoding Encoding = {
        .Extension = NO_EXTENSION,
    };
    switch (Arg.Type)
    {
    case ARG_DATA_REG:      Encoding.ModeReg = 000 + Arg.As.Dn; break;
    case ARG_ADDR_REG:      Encoding.ModeReg = 010 + Arg.As.An; break;
    case ARG_IND_REG:       Encoding.ModeReg = 020 + Arg.As.Ind.An; break;
    case ARG_IND_POSTINC:   Encoding.ModeReg = 030 + Arg.As.PostInc.An; break;
    case ARG_IND_PREDEC:    Encoding.ModeReg = 040 + Arg.As.PreDec.An; break;

    case ARG_IND_I16:       
    {
        Encoding.ModeReg = 050 + Arg.As.Ind.An; 
        Encoding.HasImmediate = true;
        Encoding.u.Immediate = Arg.As.Ind.Displacement;
    } break;
    case ARG_MEM:
    {
        Encoding.Extension = ENCODE_FULL_EXTENSION(
            0, 
            0, 
            0,
            Arg.As.Mem.Bd == 0,
            1,
            EncodeExtensionSize(Arg.As.Mem.Bd),
            EncodeExtensionSize(Arg.As.Mem.Od)
        );
        Encoding.u.BaseDisplacement = Arg.As.Mem.Bd;
        Encoding.OuterDisplacement = Arg.As.Mem.Od;
        Encoding.ModeReg = 060 + Arg.As.Mem.An;
    } break;
    case ARG_MEM_PRE:
    case ARG_MEM_POST:
    {
        unsigned IsPost = (ARG_MEM_POST == Arg.Type)? 04 : 0;
        Encoding.Extension = ENCODE_FULL_EXTENSION(
            Arg.As.Mem.X.n,
            Arg.As.Mem.X.Size == 4,
            Arg.As.Mem.X.Scale,
            Arg.As.Mem.Bd == 0,
            Arg.As.Mem.X.n == NO_REG,
            EncodeExtensionSize(Arg.As.Mem.Bd),
            IsPost + EncodeExtensionSize(Arg.As.Mem.Od)
        );
        Encoding.u.BaseDisplacement = Arg.As.Mem.Bd;
        Encoding.OuterDisplacement = Arg.As.Mem.Od;
        Encoding.ModeReg = 060 + Arg.As.Mem.An;
    } break;
    case ARG_IDX_BD:
    {
        Encoding.Extension = ENCODE_FULL_EXTENSION(
            Arg.As.Idx.X.n,
            Arg.As.Idx.X.Size == 4,
            Arg.As.Idx.X.Scale,
            Arg.As.Idx.Bd == 0,
            Arg.As.Idx.X.n == NO_REG,
            EncodeExtensionSize(Arg.As.Idx.Bd),
            0
        );
        Encoding.u.BaseDisplacement = Arg.As.Idx.Bd;
        Encoding.OuterDisplacement = Arg.As.Idx.Od;
        Encoding.ModeReg = 060 + Arg.As.Idx.An;
    } break;
    case ARG_IDX_I8:        
    {
        Encoding.Extension = ENCODE_BRIEF_EXTENSION(
            Arg.As.IdxI8.X.n,
            Arg.As.IdxI8.X.Size == 4,
            Arg.As.IdxI8.X.Scale,
            Arg.As.IdxI8.I8
        );
        Encoding.ModeReg = 060 + Arg.As.IdxI8.An;
    } break;

    case ARG_ADDR:          
    {
        Encoding.ModeReg = 070 + !IN_I16(Arg.As.Addr);
        Encoding.HasImmediate = true;
        Encoding.u.Immediate = Arg.As.Addr;
    } break;
    case ARG_IMMEDIATE:
    {
        Encoding.ModeReg = 074;
        Encoding.HasImmediate = true;
        Encoding.u.Immediate = Arg.As.Immediate;
    } break;
    case ARG_PC_I16:
    {
        Encoding.ModeReg = 072;
        Encoding.HasImmediate = true;
        Encoding.u.Immediate = Arg.As.PC.I16;
    } break;
    case ARG_PC_IDX_I8:
    {
        Encoding.Extension = ENCODE_BRIEF_EXTENSION(
            Arg.As.PC.Idx.X.n,
            Arg.As.PC.Idx.X.Scale,
            Arg.As.PC.Idx.X.Size == 4,
            Arg.As.PC.Idx.I8
        );
        Encoding.ModeReg = 073;
    } break;
    case ARG_PC_BD:
    {
        Encoding.Extension = ENCODE_FULL_EXTENSION(
            Arg.As.PC.Mem.X.n,
            Arg.As.PC.Mem.X.Scale,
            Arg.As.PC.Mem.X.Size == 4,
            Arg.As.PC.Mem.Bd == 0,
            Arg.As.PC.Mem.X.n == NO_REG,
            EncodeExtensionSize(Arg.As.PC.Mem.Bd),
            0
        );
        Encoding.ModeReg = 073;
        Encoding.u.BaseDisplacement = Arg.As.PC.Mem.Bd;
        Encoding.OuterDisplacement = Arg.As.PC.Mem.Od;
    } break;
    case ARG_PC_MEM:
    {
        Encoding.Extension = ENCODE_FULL_EXTENSION(
            0, 0, 0,
            Arg.As.PC.Mem.Bd == 0,
            1,
            EncodeExtensionSize(Arg.As.PC.Mem.Bd),
            0
        );
        Encoding.ModeReg = 073;
        Encoding.u.BaseDisplacement = Arg.As.PC.Mem.Bd;
        Encoding.OuterDisplacement = Arg.As.PC.Mem.Od;
    } break;
    case ARG_PC_MEM_PRE:
    case ARG_PC_MEM_POST:
    {
        unsigned IsPost = (ARG_PC_MEM_POST == Arg.Type)? 04 : 0;
        Encoding.Extension = ENCODE_FULL_EXTENSION(
            Arg.As.PC.Mem.X.n,
            Arg.As.PC.Mem.X.Scale,
            Arg.As.PC.Mem.X.Size == 4,
            Arg.As.PC.Mem.Bd == 0,
            Arg.As.PC.Mem.X.n == NO_REG,
            EncodeExtensionSize(Arg.As.PC.Mem.Bd),
            IsPost + EncodeExtensionSize(Arg.As.PC.Mem.Od)
        );
        Encoding.ModeReg = 073;
        Encoding.u.BaseDisplacement = Arg.As.PC.Mem.Bd;
        Encoding.OuterDisplacement = Arg.As.PC.Mem.Od;
    } break;

    case ARG_INVALID: break;
    }

    Encoding.RegMode = (Encoding.ModeReg << 3) | (Encoding.ModeReg >> 3);
    return Encoding;
#undef ENCODE_BRIEF_EXTENSION
#undef ENCODE_FULL_EXTENSION
}






static void LittleEndianEmitter(uint8_t *Buffer, uint64_t Data, unsigned Size)
{
    for (unsigned i = 0; i < Size; i++)
    {
        *Buffer++ = Data >> i*8;
    }
}

static void BigEndianEmitter(uint8_t *Buffer, uint64_t Data, unsigned Size)
{
    for (int i = Size - 1; i >= 0; i--)
    {
        *Buffer++ = Data >> i*8;
    }
}

static void Emit(M68kAssembler *Assembler, uint64_t Data, unsigned Size)
{
    if (Assembler->CriticalError)
        return;

    if (Assembler->MachineCode.Size + Size >= Assembler->MachineCode.Capacity)
    {
        Assembler->MachineCode.Capacity = (Assembler->MachineCode.Capacity + Size)*2;
        Assembler->MachineCode.Buffer = 
            Assembler->Allocator(Assembler->MachineCode.Buffer, Assembler->MachineCode.Capacity);
        if (NULL == Assembler->MachineCode.Buffer)
        {
            Assembler->CriticalError = true;
            EmptyError(Assembler, "Allocator failed.");
            return;
        }
    }

    Assembler->Emit(&Assembler->MachineCode.Buffer[Assembler->MachineCode.Size], Data, Size);
    Assembler->MachineCode.Size += Size;
    Assembler->PC += Size;
}

static void EmitEaExtension(M68kAssembler *Assembler, EaEncoding Encoding)
{
    if (Encoding.HasImmediate)
    {
        Emit(Assembler, Encoding.u.Immediate, IN_I16(Encoding.u.Immediate) ? 2 : 4);
    }

    if (Encoding.Extension == NO_EXTENSION)
        return;

    Emit(Assembler, Encoding.Extension, 2);
    if (Encoding.u.BaseDisplacement)
    {
        Emit(Assembler, Encoding.u.BaseDisplacement, IN_I16(Encoding.u.BaseDisplacement) ? 2 : 4);
    }
    if (Encoding.OuterDisplacement)
    {
        Emit(Assembler, Encoding.OuterDisplacement, IN_I16(Encoding.OuterDisplacement) ? 2 : 4);
    }
}

static void Unpanic(M68kAssembler *Assembler)
{
    Assembler->Panic = false;
    while (!NoMoreToken(Assembler) && Assembler->CurrentToken.Line == Assembler->NextToken.Line)
    {
        ConsumeToken(Assembler);
    }
}

static unsigned ConsumeSize(M68kAssembler *Assembler)
{
    unsigned Size = 2; /* default size */
    if (ConsumeIfNextTokenIs(Assembler, TOKEN_DOT))
        Size = ConsumeSizeSpecifier(Assembler);
    return Size;
}

static void IgnoreSize(M68kAssembler *Assembler, const Token *Instruction)
{
    if (ConsumeIfNextTokenIs(Assembler, TOKEN_DOT))
    {
        ConsumeSizeSpecifier(Assembler);
        WarnAtToken(Assembler, 
                Instruction, ""STRVIEW_FMT" ignores '"STRVIEW_FMT"' size specifier.", 
                STRVIEW_FMT_ARG(Instruction->Lexeme),
                STRVIEW_FMT_ARG(Assembler->CurrentToken.Lexeme)
        );
    }
}

/* index corresponding to register */
static uint16_t ConsumeRegisterList(M68kAssembler *Assembler, bool PreDec)
{
    uint16_t List = 0;
    ConsumeOrError(Assembler, TOKEN_LCURLY, "Expected '{' before register list.");
    do {
        unsigned Index = 0;
        if (ConsumeIfNextTokenIs(Assembler, TOKEN_ADDR_REG))
        {
            Index = Assembler->CurrentToken.Data.Int + 8;
        }
        else if (ConsumeIfNextTokenIs(Assembler, TOKEN_DATA_REG))
        {
            Index = Assembler->CurrentToken.Data.Int;
        }
        else
        {
            Error(Assembler, "Expected address or data register.");
        }

        List |= PreDec? 
            1 << (15 - Index)
            : 1 << Index;
    } while (ConsumeIfNextTokenIs(Assembler, TOKEN_COMMA));
    ConsumeOrError(Assembler, TOKEN_RCURLY, "Expected '}' after register list.");
    return List;
}

static void ConsumeStatement(M68kAssembler *Assembler)
{
#define INS(Mnemonic) (TOKEN_##Mnemonic - INS_BASE) 
    static const uint16_t OpcodeLut[INS_COUNT] = {
        [INS(ADDI)] = 03 << 9, 
        [INS(ANDI)] = 01 << 9,
        [INS(CMPI)] = 06 << 9,
        [INS(EORI)] = 05 << 9,
        [INS(ORI)]  = 00 << 9,
        [INS(SUBI)] = 02 << 9,

        [INS(BTST)] = 0 << 6,
        [INS(BCHG)] = 1 << 6,
        [INS(BCLR)] = 2 << 6,
        [INS(BSET)] = 3 << 6,

        [INS(ADD)] = 0xD000,
        [INS(SUB)] = 0x9000,
        [INS(OR)]  = 0x8000,
        [INS(AND)] = 0xC000,

        [INS(ADDA)] = 0xD << 12,
        [INS(SUBA)] = 0x9 << 12,
        [INS(ADDX)] = 0xD100,
        [INS(SUBX)] = 0x9100,

        [INS(MULS)] = 0xC1C0,
        [INS(DIVS)] = 0x81C0,
        [INS(MULU)] = 0xC0C0,
        [INS(DIVU)] = 0x80C0,
    };
#undef INS
#define LOOKUP_OPC(Ins) OpcodeLut[(Ins) - INS_BASE]
#define CONSUME_COMMA() ConsumeOrError(Assembler, TOKEN_COMMA, "Expected ',' after immediate.")
#define ENCODE_MOVE_SIZE(Size) \
    (Size == 4? 2 << 12 \
     : Size == 2? 3 << 12 \
     : 1 << 12)
#define ENCODE_SIZE(Size)\
    (CountBits(Size - 1))
#define NO_BYTE_SIZE(Size) if (1 == Size) Error(Assembler, "Invalid size specifier for "STRVIEW_FMT, STRVIEW_FMT_ARG(Instruction.Lexeme))

    TokenType Type = ConsumeToken(Assembler);
    Token Instruction = Assembler->CurrentToken;
    switch (Type)
    {
    default:
    {
        Error(Assembler, "Expected instruction, variable, label, or directive.");
    } break;
    case TOKEN_IDENTIFIER:
    {
        DeclStmt(Assembler);
    } break;
    case TOKEN_ADDI:
    case TOKEN_ANDI:
    case TOKEN_CMPI:
    case TOKEN_EORI:
    case TOKEN_ORI:
    case TOKEN_SUBI:
    {
        unsigned Size = ConsumeSize(Assembler);
        Argument Src = ConsumeImmediate(Assembler);
        CONSUME_COMMA();
        Argument Dst = ConsumeEa(Assembler, 4);

        if (ARG_IMMEDIATE == Dst.Type || ARG_ADDR_REG == Dst.Type || ADDRM_USE_PC(Dst.Type))
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, Dst.Type, "destination");
        }

        EaEncoding Ea = EncodeEa(Dst);
        Emit(Assembler, LOOKUP_OPC(Type)  /* opcode */
            | (ENCODE_SIZE(Size) << 6)    /* size */
            | Ea.ModeReg,                 /* ea */
            2
        );
        Emit(Assembler, Src.As.Immediate, Size == 1? 2 : Size);
        EmitEaExtension(Assembler, Ea);
    } break;
    case TOKEN_ADDQ:
    case TOKEN_SUBQ:
    {
        unsigned Size = ConsumeSize(Assembler);
        Argument Src = ConsumeImmediate(Assembler);
        CONSUME_COMMA();
        Argument Dst = ConsumeEa(Assembler, 2);

        if (ARG_IMMEDIATE == Dst.Type || ADDRM_USE_PC(Dst.Type))
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, Dst.Type, "destination");
        }
        if (Src.As.Immediate > 8)
        {
            ErrorAtToken(Assembler, &Instruction, 
                "Immediate is too big for '"STRVIEW_FMT"'.", STRVIEW_FMT_ARG(Instruction.Lexeme)
            );
        }

        EaEncoding Ea = EncodeEa(Dst);
        Emit(Assembler, 
            0x5000
            | ((Src.As.Immediate & 07) << 9)
            | ((TOKEN_SUBQ == Type) << 8)
            | (ENCODE_SIZE(Size) << 6)
            | (Ea.ModeReg), 
            2
        );
        EmitEaExtension(Assembler, Ea);
    } break;
    case TOKEN_BTST:
    case TOKEN_BCHG:
    case TOKEN_BCLR:
    case TOKEN_BSET:
    {
        IgnoreSize(Assembler, &Instruction);
        Argument Src = ConsumeEa(Assembler, 0);
        CONSUME_COMMA();
        if (Src.Type == ARG_IMMEDIATE)
        {
            Argument Dst = ConsumeEa(Assembler, 4);
            EaEncoding DstEa = EncodeEa(Dst);
            Emit(Assembler, 
                0x0800
                | LOOKUP_OPC(Type)
                | DstEa.ModeReg, 
                2
            );
            Emit(Assembler, Src.As.Immediate, 2);
            EmitEaExtension(Assembler, DstEa);
        }
        else if (Src.Type == ARG_DATA_REG)
        {
            Argument Dst = ConsumeEa(Assembler, 2);
            EaEncoding DstEa = EncodeEa(Dst);
            Emit(Assembler, 
                0x0100
                | ((uint32_t)Src.As.Dn << 9)
                | LOOKUP_OPC(Type)
                | DstEa.ModeReg,
                2
            );
            EmitEaExtension(Assembler, DstEa);
        }
        else
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, Src.Type, "source");
        }
    } break;
    case TOKEN_MOVE:
    {
        unsigned Size = ConsumeSize(Assembler);
        Argument Src = ConsumeEa(Assembler, 2);
        CONSUME_COMMA();
        Argument Dst = ConsumeEa(Assembler, 2);

        if (Dst.Type == ARG_ADDR_REG || ADDRM_USE_PC(Dst.Type) || ARG_IMMEDIATE == Dst.Type)
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, Dst.Type, "destination");
        }
        EaEncoding SrcEa = EncodeEa(Src);
        EaEncoding DstEa = EncodeEa(Dst);
        uint32_t SizeEncoding = ENCODE_MOVE_SIZE(Size);

        Emit(Assembler, 
            SizeEncoding
            | (DstEa.RegMode << 6)
            | (SrcEa.ModeReg),
            2
        );
        EmitEaExtension(Assembler, DstEa);
        EmitEaExtension(Assembler, SrcEa);
    } break;
    case TOKEN_MOVEA:
    {
        unsigned Size = ConsumeSize(Assembler);
        NO_BYTE_SIZE(Size);
        Argument Src = ConsumeEa(Assembler, 2);
        CONSUME_COMMA();
        unsigned Dst = ConsumeAddrReg(Assembler);

        uint32_t SizeEncoding = ENCODE_MOVE_SIZE(Size);
        EaEncoding Ea = EncodeEa(Src);

        Emit(Assembler,
            SizeEncoding
            | (Dst << 9)
            | (1 << 6) /* Addr Mode */
            | Ea.ModeReg,
            2
        );
        EmitEaExtension(Assembler, Ea);
    } break;
    case TOKEN_MOVEQ:
    {
        IgnoreSize(Assembler, &Instruction);
        Argument Src = ConsumeImmediate(Assembler);
        CONSUME_COMMA();
        unsigned Dst = ConsumeDataReg(Assembler);
        Emit(Assembler, 
            0x7000 
            | (Dst << 9)
            | (0xFF & Src.As.Immediate),
            2
        );
    } break;
    case TOKEN_MOVEP:
    {
        unsigned Size = ConsumeSize(Assembler);
        NO_BYTE_SIZE(Size);
        Argument Src = ConsumeEa(Assembler, 4);
        CONSUME_COMMA();
        Argument Dst = ConsumeEa(Assembler, 4);

        unsigned OpMode = 04;
        unsigned DataReg = 0, AddrReg = 0;
        uint16_t Displacement = 0;
        ArgumentType ExpectedSrcType = 0;
        if (Dst.Type == ARG_DATA_REG)
        {
            ExpectedSrcType = ARG_IND_I16;
            DataReg = Dst.As.Dn;
            AddrReg = Src.As.Ind.An;
            Displacement = Src.As.Ind.Displacement;
        }
        else if (Dst.Type == ARG_IND_I16)
        {
            OpMode += 2;
            ExpectedSrcType = ARG_DATA_REG;
            DataReg = Src.As.Dn;
            AddrReg = Dst.As.Ind.An;
            Displacement = Dst.As.Ind.Displacement;
        }
        else
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, Dst.Type, "destination");
        }
        if (ExpectedSrcType && ExpectedSrcType != Src.Type)
            ErrorInvalidAddrMode(Assembler, &Instruction, Src.Type, "source");

        OpMode += Size == 4;
        Emit(Assembler, 
            0x0008
            | (DataReg << 9)
            | (OpMode << 6)
            | (AddrReg),
            2
        );
        Emit(Assembler, Displacement, 2);
    } break;
    case TOKEN_MOVEM:
    {
        unsigned Size = ConsumeSize(Assembler);
        NO_BYTE_SIZE(Size);

        uint16_t Opcode, List;
        EaEncoding Ea;
        if (NextTokenIs(TOKEN_LCURLY)) /* mem to reg, postinc */
        {
            List = ConsumeRegisterList(Assembler, false);
            CONSUME_COMMA();
            Argument Dst = ConsumeEa(Assembler, 4);
            Ea = EncodeEa(Dst);
            Opcode = 0x4C80;

            if (ARG_DATA_REG == Dst.Type || ARG_ADDR_REG == Dst.Type 
            || ARG_IND_PREDEC == Dst.Type || ARG_IMMEDIATE == Dst.Type
            || ADDRM_USE_PC(Dst.Type))
            {
                ErrorInvalidAddrMode(Assembler, &Instruction, Dst.Type, "destination");
            }
        }
        else /* reg to mem predec */
        {
            Argument Src = ConsumeEa(Assembler, 4);
            Ea = EncodeEa(Src);
            CONSUME_COMMA();
            List = ConsumeRegisterList(Assembler, Src.Type == ARG_IND_PREDEC);
            Opcode = 0x4880;

            if (ARG_DATA_REG == Src.Type || ARG_ADDR_REG == Src.Type 
            || ARG_IND_POSTINC == Src.Type || ARG_IMMEDIATE == Src.Type)
            {
                ErrorInvalidAddrMode(Assembler, &Instruction, Src.Type, "source");
            }
        }

        Emit(Assembler, 
            Opcode
            | ((Size == 4) << 6)
            | Ea.ModeReg, 
            2
        );
        Emit(Assembler, List, sizeof List);
    } break;
    case TOKEN_ADD:
    case TOKEN_SUB:
    case TOKEN_OR:
    case TOKEN_AND:
    {
        unsigned Size = ConsumeSize(Assembler);
        Argument Src = ConsumeEa(Assembler, 2);
        CONSUME_COMMA();
        Argument Dst = ConsumeEa(Assembler, 2);
        uint32_t SizeEncoding = ENCODE_SIZE(Size);

        EaEncoding Ea = { 0 };
        /* check if dst is data reg first, 
         * this is important because order matters (addx encoding) */
        if (Dst.Type == ARG_DATA_REG) 
        {
            if ((TOKEN_OR == Type || TOKEN_AND == Type)
            && (Src.Type == ARG_ADDR_REG))
            {
                ErrorInvalidAddrMode(Assembler, &Instruction, Src.Type, "source");
            }

            Ea = EncodeEa(Src);
            Emit(Assembler,
                LOOKUP_OPC(Type) | 0x0000
                | ((uint32_t)Dst.As.Dn << 9)
                | (SizeEncoding << 6)
                | (Ea.ModeReg),
                2
            );
        }
        else if (Src.Type == ARG_DATA_REG)
        {
            if (Dst.Type == ARG_DATA_REG || Dst.Type == ARG_ADDR_REG 
            || Dst.Type == ARG_IMMEDIATE || ADDRM_USE_PC(Dst.Type))
            {
                ErrorInvalidAddrMode(Assembler, &Instruction, Dst.Type, "destination");
            }

            Ea = EncodeEa(Dst);
            Emit(Assembler, 
                LOOKUP_OPC(Type) | 0x0100 /* direction bit */
                | ((uint32_t)Src.As.Dn << 9)
                | (SizeEncoding << 6)
                | (Ea.ModeReg),
                2
            );
        }
        else
        {
            Error(Assembler, STRVIEW_FMT" expects at least 1 data register argument.", 
                STRVIEW_FMT_ARG(Instruction.Lexeme)
            );
        }
        EmitEaExtension(Assembler, Ea);
    } break;
    case TOKEN_MULS:
    case TOKEN_MULU:
    case TOKEN_DIVS:
    case TOKEN_DIVU:
    case TOKEN_DIVSL:
    case TOKEN_DIVUL:
    {
        unsigned Size = ConsumeSize(Assembler);
        NO_BYTE_SIZE(Size);
        Argument Src;
        if (4 == Size)
        {
            unsigned IsDiv = TOKEN_MULS != Type && TOKEN_MULU != Type;
            Src = ConsumeEa(Assembler, 4);
            EaEncoding Ea = EncodeEa(Src);
            CONSUME_COMMA();
            unsigned Da = ConsumeDataReg(Assembler);

            if (ConsumeIfNextTokenIs(Assembler, TOKEN_COLON)) /* 64 bit division/multiplication */
            {
                /* these instructions don't want to use both Da and Db */
                unsigned Use64Bits = TOKEN_DIVSL != Type && TOKEN_DIVUL != Type;
                unsigned Db = ConsumeDataReg(Assembler);

                Emit(Assembler, 
                    ((uint32_t)(0x4C00 /* works for both mul and div */
                    | (IsDiv << 6) 
                    | Ea.ModeReg) 
                     << 16)
                    | (Db << 12)
                    | (Use64Bits << 10) /* 64 bit division/multiplication */
                    | Da, 
                    4
                );
            }
            else
            {
                Emit(Assembler, 
                    ((uint32_t)(0x4C00
                    | (IsDiv << 6)
                    | Ea.ModeReg) 
                     << 16)
                    | (0 << 12)
                    | (0 << 10)
                    | Da, 
                    4
                );
            }
            EmitEaExtension(Assembler, Ea);
        }
        else 
        {
            if (TOKEN_DIVSL == Type || TOKEN_DIVUL == Type)
            {
                Error(Assembler, "Must use '.l' size specifier for "STRVIEW_FMT".", 
                    STRVIEW_FMT_ARG(Instruction.Lexeme)
                );
            }
            Src = ConsumeEa(Assembler, 2);
            EaEncoding Ea = EncodeEa(Src);
            CONSUME_COMMA(); 
            unsigned Dn = ConsumeDataReg(Assembler);

            Emit(Assembler, 
                LOOKUP_OPC(Type)
                | (Dn << 9)
                | Ea.ModeReg,
                2
            );
            EmitEaExtension(Assembler, Ea);           
        }

        if (ARG_ADDR_REG == Src.Type)
            ErrorInvalidAddrMode(Assembler, &Instruction, Src.Type, "source");
    } break;
    case TOKEN_EOR:
    {
        unsigned Size = ConsumeSize(Assembler);
        unsigned Src = ConsumeDataReg(Assembler);
        CONSUME_COMMA();
        Argument Dst = ConsumeEa(Assembler, 2);

        if (Dst.Type == ARG_DATA_REG || Dst.Type == ARG_IMMEDIATE || ADDRM_USE_PC(Dst.Type))
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, Dst.Type, "destination");
        }
        EaEncoding Ea = EncodeEa(Dst);
        Emit(Assembler, 
            0xB100
            | (Src << 9)
            | (ENCODE_SIZE(Size) << 6)
            | Ea.ModeReg,
            2
        );
        EmitEaExtension(Assembler, Ea);
    } break;
    case TOKEN_ADDA:
    case TOKEN_SUBA:
    {
        unsigned Size = ConsumeSize(Assembler);
        NO_BYTE_SIZE(Size);
        Argument Src = ConsumeEa(Assembler, 2);
        CONSUME_COMMA();
        unsigned Dst = ConsumeAddrReg(Assembler);

        EaEncoding Ea = EncodeEa(Src);
        Emit(Assembler,
            LOOKUP_OPC(Type)
            | (Dst << 9)
            | ((Size == 4) << 8)
            | (Ea.ModeReg),
            2
        );
        EmitEaExtension(Assembler, Ea);
    } break;
    case TOKEN_SUBX:
    case TOKEN_ADDX:
    {
        unsigned Size = ConsumeSize(Assembler);
        Argument Src = ConsumeEa(Assembler, 2);
        CONSUME_COMMA();
        Argument Dst = ConsumeEa(Assembler, 2);
        if (Dst.Type != Src.Type)
        {
            Error(Assembler, 
                STRVIEW_FMT" expects the same addressing mode for both destination and source.", 
                STRVIEW_FMT_ARG(Instruction.Lexeme)
            );
        }

        if (Dst.Type == ARG_DATA_REG)
        {
            Emit(Assembler, 
                LOOKUP_OPC(Type)
                | ((uint32_t)Dst.As.Dn << 9)
                | (ENCODE_SIZE(Size) << 6)
                | (0 << 3)
                | (Src.As.Dn), 
                2
            );
        }
        else if (Dst.Type == ARG_IND_PREDEC)
        {
            Emit(Assembler, 
                LOOKUP_OPC(Type)
                | ((uint32_t)Dst.As.PreDec.An << 9)
                | (ENCODE_SIZE(Size) << 6)
                | (1 << 3) /* R/M bit */
                | (Src.As.PreDec.An),
                2
            );
        }
        else
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, Dst.Type, "both source and destination");
        }
    } break;
    case TOKEN_CMPM:
    {
        unsigned Size = ConsumeSize(Assembler);
        Argument Src = ConsumeEa(Assembler, 2);
        CONSUME_COMMA();
        Argument Dst = ConsumeEa(Assembler, 2);

        if (Dst.Type != Src.Type)
        {
            Error(Assembler, 
                STRVIEW_FMT" expects the same addressing mode for both destination and source.", 
                STRVIEW_FMT_ARG(Instruction.Lexeme)
            );
        }
        if (Dst.Type != ARG_IND_POSTINC)
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, Dst.Type, "both source and destination");
        }
        Emit(Assembler, 
            0xB104
            | ((uint32_t)Dst.As.PostInc.An << 9)
            | (ENCODE_SIZE(Size) << 6)
            | (Src.As.PostInc.An),
            2
        );
    } break;
    case TOKEN_CMP:
    {
        unsigned Size = ConsumeSize(Assembler);
        Argument Src = ConsumeEa(Assembler, 2);
        CONSUME_COMMA();
        unsigned Dst = ConsumeDataReg(Assembler);

        EaEncoding Ea = EncodeEa(Src);
        Emit(Assembler, 
            0xB000
            | (Dst << 9)
            | (ENCODE_SIZE(Size) << 6)
            | Ea.ModeReg,
            2
        );
        EmitEaExtension(Assembler, Ea);
    } break;
    case TOKEN_CMPA:
    {
        unsigned Size = ConsumeSize(Assembler);
        Argument Src = ConsumeEa(Assembler, 2);
        CONSUME_COMMA();
        unsigned Dst = ConsumeAddrReg(Assembler);

        EaEncoding Ea = EncodeEa(Src);
        Emit(Assembler, 
            0xB0C0
            | (Dst << 9)
            | ((Size == 4) << 8)
            | Ea.ModeReg,
            2
        );
        EmitEaExtension(Assembler, Ea);
    } break;
    case TOKEN_EXG:
    {
        IgnoreSize(Assembler, &Instruction);
        Argument A = ConsumeEa(Assembler, 0);
        CONSUME_COMMA();
        Argument B = ConsumeEa(Assembler, 0);

        unsigned FirstArg = 0, SecondArg = 0;
        unsigned OpMode = 0;
        if (A.Type == ARG_DATA_REG)
            FirstArg = A.As.Dn;
        else if (A.Type == ARG_ADDR_REG)
            FirstArg = A.As.An;
        else ErrorInvalidAddrMode(Assembler, &Instruction, A.Type, "its first argument");

        if (B.Type == ARG_DATA_REG)
        {
            SecondArg = B.As.Dn;
            /* Important: EXG only support: 
             *    (DataReg, DataReg)
             *    (AddrReg, AddrReg)
             *    (DataReg, AddrReg)
             * but here we encountered (AddrReg, DataReg) form, 
             * so switch it to (DataReg, AddrReg) form */
            if (A.Type != B.Type) 
            {
                OpMode = 021;
                SecondArg = FirstArg;
                FirstArg = B.As.Dn;
            }
            else /* (D, D) form */
            {
                OpMode = 010;
            }
        }
        else if (B.Type == ARG_ADDR_REG)
        {
            SecondArg = B.As.An;
            OpMode = A.Type == B.Type?
                011: 021;
        }
        else ErrorInvalidAddrMode(Assembler, &Instruction, A.Type, "its second argument");

        Emit(Assembler, 
            0xC100
            | (FirstArg << 9)
            | (OpMode << 3)
            | (SecondArg),
            2
        );
    } break;
    case TOKEN_EXT:
    case TOKEN_EXTB:
    {
        unsigned ExtendTo = ConsumeSize(Assembler);
        if (ExtendTo == 1)
            Error(Assembler, "Cannot sign extend to byte.");
        unsigned Dn = ConsumeDataReg(Assembler);
        unsigned OpMode = 
            (((ExtendTo == 4) && (TOKEN_EXTB == Type)) << 2)
            | (1 << 1) 
            | (ExtendTo == 4);

        Emit(Assembler, 
            0x4800
            | (OpMode << 6)
            | Dn, 
            2
        );
    } break;
    case TOKEN_LEA:
    {
        IgnoreSize(Assembler, &Instruction);
        Argument Addr = ConsumeEa(Assembler, 2);
        CONSUME_COMMA();
        unsigned An = ConsumeAddrReg(Assembler);

        if (ARG_IND_PREDEC == Addr.Type || ARG_IND_POSTINC == Addr.Type 
        || ARG_DATA_REG == Addr.Type || ARG_ADDR_REG == Addr.Type 
        || ARG_IMMEDIATE == Addr.Type)
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, Addr.Type, "effective address");
        }

        EaEncoding Ea = EncodeEa(Addr);
        Emit(Assembler, 
            0x41C0 
            | (An << 9)
            | Ea.ModeReg, 
            2
        );
        EmitEaExtension(Assembler, Ea);
    } break;
    case TOKEN_SWAP:
    {
        IgnoreSize(Assembler, &Instruction);
        unsigned Dn = ConsumeDataReg(Assembler);
        Emit(Assembler, 
            0x4840 | Dn, 
            2
        );
    } break;
    case TOKEN_Bcc:
    case TOKEN_BRA:
    case TOKEN_BSR:
    {
        UNREACHABLE("TODO: branch ins properly");
    } break;
    case TOKEN_DBcc:
    {
        UNREACHABLE("TODO: branch ins properly");

        IgnoreSize(Assembler, &Instruction);
        unsigned Reg = ConsumeDataReg(Assembler);
        uint32_t Offset = IntExpr(Assembler, "Branch target") - (Assembler->PC + 2);
        Emit(Assembler, 
            0x5068
            | ((uint32_t)Instruction.Data.ConditionalCode << 8)
            | Reg, 2
        );
        Emit(Assembler, Offset, 2);
    } break;
    }

    if (Assembler->Panic)
    {
        Unpanic(Assembler);
    }
#undef LOOKUP_OPC
#undef CONSUME_COMMA
}



static M68kAssembler AssemblerInit(AllocatorFn Allocator, 
        const char *SourceName, const char *Source, bool LittleEndian, FILE *ErrorStream)
{
    M68kAssembler Assembler = {
        .Emit = LittleEndian? LittleEndianEmitter : BigEndianEmitter,
        .LineStart = Source,
        .StartPtr = Source,
        .CurrPtr = Source,
        .ErrorStream = ErrorStream,

        .Error = false,
        .CriticalError = false,
        .Panic = false,

        .SourceName = SourceName,
        .LineCount = 1,
        .Allocator = Allocator,
        .MachineCode = (MC68020MachineCode) { 
            .Size = 0,
            .Capacity = 256,
            .Buffer = Allocator(NULL, 256)
        },

        .NextToken = (Token){ 0 },
    };
    ConsumeToken(&Assembler);
    return Assembler;
}

static void AssemblerDeinit(M68kAssembler *Assembler)
{
    (void)Assembler;
}

MC68020MachineCode MC68020Assemble(AllocatorFn Allocator,
        const char *SourceName, const char *Source, bool LittleEndian, FILE *ErrorStream)
{
    M68kAssembler Assembler = AssemblerInit(Allocator, SourceName, Source, LittleEndian, ErrorStream);
    if (NULL == Assembler.MachineCode.Buffer)
    {
        EmptyError(&Assembler, "Allocator failed.");
        goto Done;
    }

    while (!NoMoreToken(&Assembler) && !Assembler.CriticalError)
    {
        ConsumeStatement(&Assembler);
    }

    if (Assembler.Error || Assembler.CriticalError)
    {
        Allocator(Assembler.MachineCode.Buffer, 0);
        Assembler.MachineCode = (MC68020MachineCode) { 0 };
    }
Done:
    AssemblerDeinit(&Assembler);
    return Assembler.MachineCode;
}


