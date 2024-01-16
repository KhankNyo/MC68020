
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
typedef struct Expression
{
    StringView Str;
    int Line, Offset;
} Expression;
typedef struct Argument 
{
    ArgumentType Type;
    Expression Expr;
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
    uint8_t RegMode;
    uint8_t Size;
    uint16_t ModeReg;
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
    StringView Idens[1048];
    int IdenLine[1048];
    union {
        uint64_t Int;
        double Flt;
    } IdenData[1048];

    uint32_t ExprCount;
    Expression Expr[1048];

    struct {
        StringView Str;
        uint32_t ExprIndex;
    } UndefSyms[1024];
} M68kAssembler;



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
    if (!IsAtEnd(Assembler) && Ch == PeekChar(Assembler))
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
    if (!IsAtEnd(Assembler))
    {
        Assembler->StartPtr = Assembler->CurrPtr++;
    }
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



    /* determine type */
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


static void ErrorAtArgs(M68kAssembler *Assembler, StringView Offender, int LineNumber, StringView Line, 
        const char *Fmt, va_list Args)
{
    if (Assembler->Panic)
        return;

    Display(Assembler->ErrorStream, LineNumber, 
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
    ErrorAtArgs(Assembler, Tok->Lexeme, Tok->Line, Line, Fmt, Args);
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

static void ErrorAtExpr(M68kAssembler *Assembler, const Expression *Expr, const char *Fmt, ...)
{
    va_list Args;
    va_start(Args, Fmt);

    StringView Line = {.Ptr = Expr->Str.Ptr - Expr->Offset + 1};
    Line.Len = LineLen(Line.Ptr);
    ErrorAtArgs(Assembler, Expr->Str, Expr->Line, Line, Fmt, Args);

    va_end(Args);
}

static void ErrorAtLastExpr(M68kAssembler *Assembler, const char *Fmt, ...)
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
    ErrorAtArgs(Assembler, Current->Str, Current->Line, Line, Fmt, Args);
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
        const Token *Instruction, Argument Arg, const char *Location)
{
    if (NULL == Location)
    {
        ErrorAtExpr(Assembler, &Arg.Expr, STRVIEW_FMT" does not have %s addressing mode.",
            STRVIEW_FMT_ARG(Instruction->Lexeme),
            LookupAddrModeName(Arg.Type)
        );
    }
    else
    {
        ErrorAtExpr(Assembler, &Arg.Expr, STRVIEW_FMT" does not have %s addressing mode as %s.",
            STRVIEW_FMT_ARG(Instruction->Lexeme),
            LookupAddrModeName(Arg.Type),
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
            {
                Out->As.Int = Assembler->IdenData[i].Int;
            }
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

static uint32_t StrictIntExpr(M68kAssembler *Assembler, const char *ExprName)
{
    unsigned PrevUndefSymCount = Assembler->UndefSymCount;
    Value Expr = ConstExpr(Assembler);
    if (Expr.IsFloat)
    {
        ErrorAtLastExpr(Assembler, "%s cannot be a floating-point number.", ExprName);
    }
    if (PrevUndefSymCount != Assembler->UndefSymCount)
    {
        Error(Assembler, "Undefined label in %s is not allowed.", ExprName);
    }
    return Expr.As.Int;
}

static uint32_t IntExpr(M68kAssembler *Assembler, const char *ExprName)
{
    unsigned PrevUndefSymCount = Assembler->UndefSymCount;
    Value Expr = ConstExpr(Assembler);
    if (Expr.IsFloat)
    {
        ErrorAtLastExpr(Assembler, "%s cannot be a floating-point number.", ExprName);
    }
    if (PrevUndefSymCount != Assembler->UndefSymCount)
    {
        for (unsigned i = PrevUndefSymCount; PrevUndefSymCount < Assembler->UndefSymCount; i++)
        {
            Assembler->UndefSyms[i].ExprIndex = Assembler->ExprCount - 1;
        }
    }
    return Expr.As.Int;
}


static void PushSymbol(M68kAssembler *Assembler, const Token *Sym, Value Val)
{
    if (Assembler->IdenCount >= STATIC_ARRAY_SIZE(Assembler->Idens))
        UNREACHABLE("TODO: make Symbol table dynamic");

    unsigned n = Assembler->IdenCount;
    /* search for the label */
    for (unsigned i = 0; i < n; i++)
    {
        /* symbol already exists, redefinition error */
        if (Sym->Lexeme.Len == Assembler->Idens[i].Len
        && StrSliceEquNoCase(Sym->Lexeme.Ptr, Assembler->Idens[i].Ptr, Sym->Lexeme.Len))
        {
            ErrorAtToken(Assembler, Sym, "Symbol is already defined on line %d.", 
                Assembler->IdenLine[i]
            );
            return;
        }
    }

    Assembler->IdenCount++;
    Assembler->Idens[n] = Sym->Lexeme;
    if (Val.IsFloat)
    {
        Assembler->IsFloat[n/32] |= 1ul << (n % 32);
        Assembler->IdenData[n].Flt = Val.As.Flt;
    }
    else 
    {
        Assembler->IdenData[n].Int = Val.As.Int;
    }
    Assembler->IdenLine[n] = Sym->Line;
}


static void DeclStmt(M68kAssembler *Assembler)
{
    Token Identifier = Assembler->CurrentToken;
    Value Val = { 0 };

    if (ConsumeIfNextTokenIs(Assembler, TOKEN_EQUAL))
    {
        Val = ConstExpr(Assembler);
        PushSymbol(Assembler, &Identifier, Val);
    }
    else if (ConsumeIfNextTokenIs(Assembler, TOKEN_COLON))
    {
        Val = (Value){
            .IsFloat = false, 
            .As.Int = Assembler->PC,
        };
        PushSymbol(Assembler, &Identifier, Val);
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
    else if (ConsumeOrError(Assembler, TOKEN_DATA_REG, "Expected data or address register."))
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
        unsigned Scale = StrictIntExpr(Assembler, "Index scalar");

        if (Scale != 1 && Scale != 2 
        && Scale != 4 && Scale != 8)
        {
            ErrorAtLastExpr(Assembler, "Scale factor must be 1, 2, 4, or 8.");
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
                    ErrorAtToken(Assembler, &Assembler->NextToken, 
                        "Too many index registers for PC-relative Memory Indirect addressing mode."
                    );
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
    Expression Expr = {
        .Str = Assembler->NextToken.Lexeme,
        .Line = Assembler->NextToken.Line,
        .Offset = Assembler->NextToken.Offset,
    };
    Argument Arg = { 0 };
    if (ConsumeIfNextTokenIs(Assembler, TOKEN_POUND))
    {
        Arg = ARGUMENT(ARG_IMMEDIATE,
            .As.Immediate = IntExpr(Assembler, "Immediate") 
        );
    }
    else if (ConsumeIfNextTokenIs(Assembler, TOKEN_ADDR_REG))
    {
        Arg = ARGUMENT(ARG_ADDR_REG,
            .As.An = Assembler->CurrentToken.Data.Int
        );
    }
    else if (ConsumeIfNextTokenIs(Assembler, TOKEN_DATA_REG))
    {
        Arg = ARGUMENT(ARG_DATA_REG,
            .As.Dn = Assembler->CurrentToken.Data.Int
        );
    }
    else if (ConsumeIfNextTokenIs(Assembler, TOKEN_LPAREN))
    {
        Arg = IndirectAddressingMode(Assembler);
    }
    else if (ConsumeIfNextTokenIs(Assembler, TOKEN_MINUS))
    {
        ConsumeOrError(Assembler, TOKEN_LPAREN, "Expected '(' after '-'.");
        ConsumeOrError(Assembler, TOKEN_ADDR_REG, "Expected address register.");
        ConsumeOrError(Assembler, TOKEN_RPAREN, "Expected ')' after register.");
        Arg = ARGUMENT(ARG_IND_PREDEC,
            .As.PreDec.An = Assembler->CurrentToken.Data.Int
        );
    }
    else if (ConsumeIfNextTokenIs(Assembler, TOKEN_REL))
    {
        Arg = PCRelativeAddressingMode(Assembler, Assembler->PC + InstructionSize);
    }
    else
    {
        Arg = ARGUMENT(ARG_ADDR,
            .As.Addr = IntExpr(Assembler, "Address")
        );
    }
    Expr.Str.Len = 
        Assembler->CurrentToken.Lexeme.Ptr + Assembler->CurrentToken.Lexeme.Len - Expr.Str.Ptr;
    Arg.Expr = Expr;
    return Arg;
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

static uint32_t ConsumeImmediate(M68kAssembler *Assembler)
{
    ConsumeOrError(Assembler, TOKEN_POUND, "Expected '#'.");
    return IntExpr(Assembler, "Immediate");
}


static unsigned EncodeExtensionSize(uint32_t Displacement)
{
    if (0 == Displacement)
        return 1;
    if (IN_I16((int32_t)Displacement))
        return 2;
    return 3;
}

static EaEncoding EncodeEa(Argument Arg, unsigned OperandSize)
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
#define SIZE_OF(Imm) \
    (Imm) == 0? 0\
    : IN_I16((int32_t)(Imm))? 2\
    : 4


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
        Encoding.Size = 2;
        Encoding.u.Immediate = Arg.As.Ind.Displacement;
    } break;
    case ARG_MEM:
    {
        Encoding.Size = 2 + SIZE_OF(Arg.As.Mem.Bd) + SIZE_OF(Arg.As.Mem.Od);
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
        Encoding.Size = 2 + SIZE_OF(Arg.As.Mem.Bd) + SIZE_OF(Arg.As.Mem.Od);
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
        Encoding.Size = 2 + SIZE_OF(Arg.As.Idx.Bd);
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
        Encoding.Size = 2;
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
        Encoding.Size = IN_I16(Arg.As.Addr)? 2 : 4;
        Encoding.ModeReg = 070 + (Encoding.Size == 4);
        Encoding.HasImmediate = true;
        Encoding.u.Immediate = Arg.As.Addr;
    } break;
    case ARG_IMMEDIATE:
    {
        Encoding.Size = OperandSize == 1? 2 : OperandSize;
        Encoding.ModeReg = 074;
        Encoding.HasImmediate = true;
        Encoding.u.Immediate = Arg.As.Immediate;
    } break;
    case ARG_PC_I16:
    {
        Encoding.Size = 2;
        Encoding.ModeReg = 072;
        Encoding.HasImmediate = true;
        Encoding.u.Immediate = Arg.As.PC.I16;
    } break;
    case ARG_PC_IDX_I8:
    {
        Encoding.Size = 2;
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
        Encoding.Size = 2 + SIZE_OF(Arg.As.PC.Mem.Bd);
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
        Encoding.Size = 2 + SIZE_OF(Arg.As.Mem.Bd) + SIZE_OF(Arg.As.Mem.Od);
        Encoding.Extension = ENCODE_FULL_EXTENSION(
            0, 0, 0,
            Arg.As.PC.Mem.Bd == 0,
            1,
            EncodeExtensionSize(Arg.As.PC.Mem.Bd),
            EncodeExtensionSize(Arg.As.PC.Mem.Od)
        );
        Encoding.ModeReg = 073;
        Encoding.u.BaseDisplacement = Arg.As.PC.Mem.Bd;
        Encoding.OuterDisplacement = Arg.As.PC.Mem.Od;
    } break;
    case ARG_PC_MEM_PRE:
    case ARG_PC_MEM_POST:
    {
        Encoding.Size = 2 + SIZE_OF(Arg.As.Mem.Bd) + SIZE_OF(Arg.As.Mem.Od);
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
        Emit(Assembler, Encoding.u.Immediate, IN_I16((int32_t)Encoding.u.Immediate) ? 2 : 4);
    }

    if (Encoding.Extension == NO_EXTENSION)
        return;

    Emit(Assembler, Encoding.Extension, 2);
    if (Encoding.u.BaseDisplacement)
    {
        Emit(Assembler, Encoding.u.BaseDisplacement, IN_I16((int32_t)Encoding.u.BaseDisplacement) ? 2 : 4);
    }
    if (Encoding.OuterDisplacement)
    {
        Emit(Assembler, Encoding.OuterDisplacement, IN_I16((int32_t)Encoding.OuterDisplacement) ? 2 : 4);
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
    static const uint32_t OpcodeLut[INS_COUNT] = {
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

        [INS(CMP2)] = 0x00C00000,
        [INS(CHK2)] = 0x00C00800,

        [INS(NEGX)] = 0x4000,
        [INS(CLR)] = 0x4200,
        [INS(NEG)] = 0x4400,
        [INS(NOT)] = 0x4600,

        [INS(JSR)] = 0x4E80,
        [INS(JMP)] = 0x4EC0,
        [INS(PEA)] = 0x4840,

        [INS(ILLEGAL)] = 0x4AFC,
        [INS(RESET)]= 0x4E70,
        [INS(NOP)]  = 0x4E71,
        [INS(STOP)] = 0x4E72,
        [INS(RTE)]  = 0x4E73,
        [INS(RTS)]  = 0x4E75,
        [INS(TRAPV)]= 0x4E76,
        [INS(RTR)]  = 0x4E77,
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
        uint32_t Src = ConsumeImmediate(Assembler);
        CONSUME_COMMA();
        Argument Dst = ConsumeEa(Assembler, 4);

        if (ARG_IMMEDIATE == Dst.Type || ARG_ADDR_REG == Dst.Type || ADDRM_USE_PC(Dst.Type))
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, Dst, "destination");
        }

        EaEncoding Ea = EncodeEa(Dst, Size);
        Emit(Assembler, LOOKUP_OPC(Type)  /* opcode */
            | (ENCODE_SIZE(Size) << 6)    /* size */
            | Ea.ModeReg,                 /* ea */
            2
        );
        Emit(Assembler, Src, Size == 1? 2 : Size);
        EmitEaExtension(Assembler, Ea);
    } break;
    case TOKEN_ADDQ:
    case TOKEN_SUBQ:
    {
        unsigned Size = ConsumeSize(Assembler);
        uint32_t Immediate = ConsumeImmediate(Assembler);
        CONSUME_COMMA();
        Argument Dst = ConsumeEa(Assembler, 2);

        if (ARG_IMMEDIATE == Dst.Type || ADDRM_USE_PC(Dst.Type))
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, Dst, "destination");
        }
        if (!IN_RANGE(1, Immediate, 8))
        {
            ErrorAtToken(Assembler, &Instruction, 
                STRVIEW_FMT" expects immediate to be in the range of 1 to 8.", 
                STRVIEW_FMT_ARG(Instruction.Lexeme)
            );
        }
        Immediate &= 0x7;

        EaEncoding Ea = EncodeEa(Dst, Size);
        Emit(Assembler, 
            0x5000
            | (Immediate << 9)
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
        unsigned Size = ConsumeSize(Assembler);
        if (2 == Size)
        {
            Error(Assembler, "Expected size specifier for "STRVIEW_FMT" to be '.l' or '.b'.", 
                STRVIEW_FMT_ARG(Instruction.Lexeme)
            );
        }
        Argument Src = ConsumeEa(Assembler, 0);
        CONSUME_COMMA();
        Argument Dst = ConsumeEa(Assembler, Src.Type == ARG_IMMEDIATE? 4: 2);
        EaEncoding DstEa = EncodeEa(Dst, Size);

        if (4 == Size && Dst.Type != ARG_DATA_REG)
        {
            ErrorAtExpr(Assembler, &Dst.Expr, 
                "'.l' size specifier is only valid with register as destination for "STRVIEW_FMT".",
                STRVIEW_FMT_ARG(Instruction.Lexeme)
            );
        }
        else if (1 == Size && Dst.Type == ARG_DATA_REG)
        {
            ErrorAtExpr(Assembler, &Dst.Expr, 
                "'.b' size specifier cannot be used with register as destination for "STRVIEW_FMT".",
                STRVIEW_FMT_ARG(Instruction.Lexeme)
            );
        }
        bool InstructionModifiesDst = TOKEN_BTST != Type;
        if (ARG_ADDR_REG == Dst.Type 
        || (InstructionModifiesDst && (ADDRM_USE_PC(Dst.Type) || ARG_IMMEDIATE == Dst.Type))
        || (Src.Type == ARG_IMMEDIATE && Dst.Type == ARG_IMMEDIATE))
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, Dst, "destination");
        }

        if (Src.Type == ARG_IMMEDIATE)
        {
            Emit(Assembler, 
                0x0800
                | LOOKUP_OPC(Type)
                | DstEa.ModeReg, 
                2
            );
            Emit(Assembler, 0xFF & Src.As.Immediate, 2);
            EmitEaExtension(Assembler, DstEa);
        }
        else if (Src.Type == ARG_DATA_REG)
        {
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
            ErrorInvalidAddrMode(Assembler, &Instruction, Src, "source");
        }
    } break;
    case TOKEN_MOVE:
    {
        unsigned Size = ConsumeSize(Assembler);
        Argument Src = ConsumeEa(Assembler, 2);
        EaEncoding SrcEa = EncodeEa(Src, Size);
        CONSUME_COMMA();
        Argument Dst = ConsumeEa(Assembler, 2 + SrcEa.Size);
        EaEncoding DstEa = EncodeEa(Dst, Size);

        if (Dst.Type == ARG_ADDR_REG || ADDRM_USE_PC(Dst.Type) || ARG_IMMEDIATE == Dst.Type)
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, Dst, "destination");
        }
        uint32_t SizeEncoding = ENCODE_MOVE_SIZE(Size);

        Emit(Assembler, 
            SizeEncoding
            | (DstEa.RegMode << 6)
            | (SrcEa.ModeReg),
            2
        );
        /* important: dst encoding is after source 
         * (obviously cuz it has to fetch the data before moving it, duh) */
        EmitEaExtension(Assembler, SrcEa);
        EmitEaExtension(Assembler, DstEa);
    } break;
    case TOKEN_MOVEA:
    {
        unsigned Size = ConsumeSize(Assembler);
        NO_BYTE_SIZE(Size);
        Argument Src = ConsumeEa(Assembler, 2);
        CONSUME_COMMA();
        unsigned Dst = ConsumeAddrReg(Assembler);

        uint32_t SizeEncoding = ENCODE_MOVE_SIZE(Size);
        EaEncoding Ea = EncodeEa(Src, Size);

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
        uint32_t Immediate = ConsumeImmediate(Assembler);
        CONSUME_COMMA();
        unsigned Dst = ConsumeDataReg(Assembler);
        Emit(Assembler, 
            0x7000 
            | (Dst << 9)
            | (0xFF & Immediate),
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
            ErrorInvalidAddrMode(Assembler, &Instruction, Dst, "destination");
        }
        if (ExpectedSrcType && ExpectedSrcType != Src.Type)
            ErrorInvalidAddrMode(Assembler, &Instruction, Src, "source");

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
            Ea = EncodeEa(Dst, Size);
            Opcode = 0x4C80;

            if (ARG_DATA_REG == Dst.Type || ARG_ADDR_REG == Dst.Type 
            || ARG_IND_PREDEC == Dst.Type || ARG_IMMEDIATE == Dst.Type
            || ADDRM_USE_PC(Dst.Type))
            {
                ErrorInvalidAddrMode(Assembler, &Instruction, Dst, "destination");
            }
        }
        else /* reg to mem predec */
        {
            Argument Src = ConsumeEa(Assembler, 4);
            Ea = EncodeEa(Src, Size);
            CONSUME_COMMA();
            List = ConsumeRegisterList(Assembler, Src.Type == ARG_IND_PREDEC);
            Opcode = 0x4880;

            if (ARG_DATA_REG == Src.Type || ARG_ADDR_REG == Src.Type 
            || ARG_IND_POSTINC == Src.Type || ARG_IMMEDIATE == Src.Type)
            {
                ErrorInvalidAddrMode(Assembler, &Instruction, Src, "source");
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
                ErrorInvalidAddrMode(Assembler, &Instruction, Src, "source");
            }

            Ea = EncodeEa(Src, Size);
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
                ErrorInvalidAddrMode(Assembler, &Instruction, Dst, "destination");
            }

            Ea = EncodeEa(Dst, Size);
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
            EaEncoding Ea = EncodeEa(Src, Size);
            CONSUME_COMMA();
            unsigned Da = ConsumeDataReg(Assembler);

            if (ConsumeIfNextTokenIs(Assembler, TOKEN_COLON)) /* 64 bit division/multiplication */
            {
                /* these instructions don't want to use both Da and Db */
                unsigned Use64B= TOKEN_DIVSL != Type && TOKEN_DIVUL != Type;
                unsigned Db = ConsumeDataReg(Assembler);

                Emit(Assembler, 
                    ((uint32_t)(0x4C00 /* works for both mul and div */
                    | (IsDiv << 6) 
                    | Ea.ModeReg) 
                     << 16)
                    | (Db << 12)
                    | (Use64B<< 10) /* 64 bit division/multiplication */
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
        else /* 2 == size */
        {
            if (TOKEN_DIVSL == Type || TOKEN_DIVUL == Type)
            {
                Error(Assembler, "Must use '.l' size specifier for "STRVIEW_FMT".", 
                    STRVIEW_FMT_ARG(Instruction.Lexeme)
                );
            }
            Src = ConsumeEa(Assembler, 2);
            EaEncoding Ea = EncodeEa(Src, Size);
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
            ErrorInvalidAddrMode(Assembler, &Instruction, Src, "source");
    } break;
    case TOKEN_EOR:
    {
        unsigned Size = ConsumeSize(Assembler);
        unsigned Src = ConsumeDataReg(Assembler);
        CONSUME_COMMA();
        Argument Dst = ConsumeEa(Assembler, 2);

        if (Dst.Type == ARG_DATA_REG || Dst.Type == ARG_IMMEDIATE || ADDRM_USE_PC(Dst.Type))
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, Dst, "destination");
        }
        EaEncoding Ea = EncodeEa(Dst, Size);
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

        EaEncoding Ea = EncodeEa(Src, Size);
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
                | (0 << 3) /* R/M bit, use register */
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
                | (1 << 3) 
                | (Src.As.PreDec.An),
                2
            );
        }
        else
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, Dst, "both source and destination");
        }
    } break;
    case TOKEN_NEGX:
    case TOKEN_NEG:
    case TOKEN_CLR:
    case TOKEN_NOT:
    {
        unsigned Size = ConsumeSize(Assembler);
        Argument Arg = ConsumeEa(Assembler, 2);

        if (ARG_ADDR_REG == Arg.Type || ARG_IMMEDIATE == Arg.Type 
        || ADDRM_USE_PC(Arg.Type))
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, Arg, "argument");
        }

        EaEncoding Ea = EncodeEa(Arg, Size);
        Emit(Assembler, 
            LOOKUP_OPC(Type) 
            | (ENCODE_SIZE(Size) << 6)
            | Ea.ModeReg, 
            2
        );
        EmitEaExtension(Assembler, Ea);
    } break;
    case TOKEN_TAS:
    {
        IgnoreSize(Assembler, &Instruction);
        Argument Arg = ConsumeEa(Assembler, 2);

        if (ARG_ADDR_REG == Arg.Type || ARG_IMMEDIATE == Arg.Type 
        || ADDRM_USE_PC(Arg.Type))
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, Arg, "argument");
        }

        EaEncoding Ea = EncodeEa(Arg, 1);
        Emit(Assembler, 
            0x4AC0
            | Ea.ModeReg, 
            2
        );
        EmitEaExtension(Assembler, Ea);
    } break;
    case TOKEN_TST:
    {
        unsigned Size = ConsumeSize(Assembler);
        Argument Arg = ConsumeEa(Assembler, 2);
        EaEncoding Ea = EncodeEa(Arg, Size);
        if (ARG_ADDR_REG == Arg.Type)
        {
            NO_BYTE_SIZE(Size);
        }
        Emit(Assembler, 
            0x4A00
            | (ENCODE_SIZE(Size) << 6)
            | Ea.ModeReg,
            2
        );
        EmitEaExtension(Assembler, Ea);
    } break;
    case TOKEN_CHK:
    {
        unsigned Size = ConsumeSize(Assembler);
        NO_BYTE_SIZE(Size);
        Argument Bound = ConsumeEa(Assembler, 2);
        CONSUME_COMMA();
        uint32_t Dn = ConsumeDataReg(Assembler);

        if (ARG_ADDR_REG == Bound.Type)
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, Bound, "first argument");
        }

        EaEncoding Ea = EncodeEa(Bound, Size);
        Emit(Assembler, 
            0x4100
            | (Dn << 9) 
            | ((Size == 2) << 7)
            | Ea.ModeReg, 
            2
        );
        EmitEaExtension(Assembler, Ea);
    } break;
    case TOKEN_CHK2:
    case TOKEN_CMP2:
    {
        unsigned Size = ConsumeSize(Assembler);
        Argument Bound = ConsumeEa(Assembler, 4);
        CONSUME_COMMA();
        Argument Rn = ConsumeEa(Assembler, 4);

        if (ARG_DATA_REG == Bound.Type || ARG_ADDR_REG == Bound.Type
        || ARG_IMMEDIATE == Bound.Type 
        || ARG_IND_PREDEC == Bound.Type || ARG_IND_POSTINC == Bound.Type)
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, Bound, "source");
        }
        uint32_t Reg = 0;
        if (Rn.Type == ARG_DATA_REG)
            Reg = Rn.As.Dn;
        else if (Rn.Type == ARG_ADDR_REG)
            Reg = Rn.As.An;
        else ErrorInvalidAddrMode(Assembler, &Instruction, Rn, "second argument");

        EaEncoding Ea = EncodeEa(Bound, Size);
        Emit(Assembler, 
            ((ENCODE_SIZE(Size) << 9)
            | (Ea.ModeReg))
             << 16
            | (Reg << 12)
            | LOOKUP_OPC(Type), 
            4
        );
        EmitEaExtension(Assembler, Ea);
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
            ErrorInvalidAddrMode(Assembler, &Instruction, Dst, "both source and destination");
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

        EaEncoding Ea = EncodeEa(Src, Size);
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
        NO_BYTE_SIZE(Size);
        Argument Src = ConsumeEa(Assembler, 2);
        CONSUME_COMMA();
        unsigned Dst = ConsumeAddrReg(Assembler);

        EaEncoding Ea = EncodeEa(Src, Size);
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
        else ErrorInvalidAddrMode(Assembler, &Instruction, A, "first argument");

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
        else ErrorInvalidAddrMode(Assembler, &Instruction, A, "second argument");

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
            ErrorInvalidAddrMode(Assembler, &Instruction, Addr, "effective address");
        }

        EaEncoding Ea = EncodeEa(Addr, 0); /* lea doesn't have imm encoding, this is ok */
        Emit(Assembler, 
            0x41C0 
            | (An << 9)
            | Ea.ModeReg, 
            2
        );
        EmitEaExtension(Assembler, Ea);
    } break;
    case TOKEN_JSR:
    case TOKEN_JMP:
    case TOKEN_PEA:
    {
        IgnoreSize(Assembler, &Instruction);
        Argument Addr = ConsumeEa(Assembler, 2);
 
        if (ARG_IND_PREDEC == Addr.Type || ARG_IND_POSTINC == Addr.Type 
        || ARG_DATA_REG == Addr.Type || ARG_ADDR_REG == Addr.Type 
        || ARG_IMMEDIATE == Addr.Type)
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, Addr, "effective address");
        }

        EaEncoding Ea = EncodeEa(Addr, 4);
        Emit(Assembler, 
            LOOKUP_OPC(Type)
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
    case TOKEN_Scc:
    {
        IgnoreSize(Assembler, &Instruction);
        Argument Arg = ConsumeEa(Assembler, 2);
        if (ARG_ADDR_REG == Arg.Type || ADDRM_USE_PC(Arg.Type) || ARG_IMMEDIATE == Arg.Type)
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, Arg, "argument");
        }
        EaEncoding Ea = EncodeEa(Arg, 1);
        Emit(Assembler, 
            0x50C0
            | ((uint32_t)Instruction.Data.Int << 8)
            | Ea.ModeReg, 
            2
        );
        EmitEaExtension(Assembler, Ea);
    } break;
    case TOKEN_ASR:
    case TOKEN_LSR:
    case TOKEN_ROXR:
    case TOKEN_ROR:
    case TOKEN_ASL:
    case TOKEN_LSL:
    case TOKEN_ROXL:
    case TOKEN_ROL:
    {
        unsigned DirectionLeft = 
            (TOKEN_ASL == Type 
            || TOKEN_LSL == Type 
            || TOKEN_ROXL == Type 
            || TOKEN_ROL == Type) 
            << 8;
        unsigned Size = ConsumeSize(Assembler);
        Argument First = ConsumeEa(Assembler, 2);

        if (ARG_DATA_REG == First.Type)
        {
            unsigned CountReg = First.As.Dn;
            CONSUME_COMMA();
            unsigned Reg = ConsumeDataReg(Assembler);

            Emit(Assembler, 
                0xE020
                | (CountReg << 9)
                | DirectionLeft
                | (ENCODE_SIZE(Size) << 6)
                | Reg, 
                2
            );
        }
        else if (ARG_IMMEDIATE == First.Type)
        {
            unsigned ShiftCount = First.As.Immediate;
            if (!IN_RANGE(1, ShiftCount, 8))
            {
                ErrorAtLastExpr(Assembler, "Shift count must be in range of 1 to 8.");
            }
            ShiftCount &= 0x7;
            CONSUME_COMMA();
            unsigned Reg = ConsumeDataReg(Assembler);

            Emit(Assembler, 
                0xE000
                | (ShiftCount << 9)
                | DirectionLeft
                | (ENCODE_SIZE(Size) << 6)
                | Reg,
                2
            );
        }
        else
        {
            if (ARG_ADDR_REG == First.Type || ARG_IMMEDIATE == First.Type || ADDRM_USE_PC(First.Type))
            {
                ErrorInvalidAddrMode(Assembler, &Instruction, First, "argument");
            }
            if (1 == Size || 4 == Size)
            {
                Error(Assembler, STRVIEW_FMT" can only shift memory word.", 
                    STRVIEW_FMT_ARG(Instruction.Lexeme)
                );
            }
            EaEncoding Ea = EncodeEa(First, 1);
            Emit(Assembler, 
                0xE0C0
                | DirectionLeft 
                | Ea.ModeReg,
                2 
            );
            EmitEaExtension(Assembler, Ea);
        }
    } break;
    case TOKEN_NBCD:
    {
        IgnoreSize(Assembler, &Instruction);
        Argument Arg = ConsumeEa(Assembler, 2);
        if (ARG_ADDR_REG == Arg.Type || ARG_IMMEDIATE == Arg.Type || ADDRM_USE_PC(Arg.Type))
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, Arg, "argument");
        }
        EaEncoding Ea = EncodeEa(Arg, 1);
        Emit(Assembler, 
            0x4800 | Ea.ModeReg, 
            2
        );
        EmitEaExtension(Assembler, Ea);
    } break;
    case TOKEN_ABCD:
    case TOKEN_SBCD:
    {
        IgnoreSize(Assembler, &Instruction);
        Argument Y = ConsumeEa(Assembler, 2);
        CONSUME_COMMA();
        Argument X = ConsumeEa(Assembler, 2);

        if (Y.Type != X.Type)
        {
            ErrorAtToken(Assembler, &Instruction, STRVIEW_FMT" expects both arguments to use the same addressing mode.",
                STRVIEW_FMT_ARG(Instruction.Lexeme)
            );
        }
        unsigned Opcode = 0;
        if (X.Type == ARG_DATA_REG)
        {
            Opcode = 0xC100 
                | ((uint32_t)X.As.Dn << 9) 
                | Y.As.Dn;
        }
        else if (X.Type == ARG_IND_PREDEC)
        {
            Opcode = 0xC108
                | ((uint32_t)X.As.PreDec.An << 9)
                | Y.As.PreDec.An;
        }
        else
        {
            ErrorInvalidAddrMode(Assembler, &Instruction, X, "both arguments");
        }

        Emit(Assembler, Opcode, 2);
    } break;
    case TOKEN_Bcc:
    case TOKEN_BRA:
    case TOKEN_BSR:
    {
        unsigned Size = 0;
        if (ConsumeIfNextTokenIs(Assembler, TOKEN_DOT))
        {
            Size = ConsumeSizeSpecifier(Assembler);
        }
        int32_t Offset = IntExpr(Assembler, "Branch target") - (Assembler->PC + 2);
        uint32_t Cond = Instruction.Data.ConditionalCode;

        if (TOKEN_BRA == Type)
            Cond = 0;
        else if (TOKEN_BSR == Type)
            Cond = 1;
        Cond <<= 8;

        if (0 == Offset || -1 == Offset)
        {
            Emit(Assembler, 0x6000 | Cond, 2);
            Emit(Assembler, Offset, 2);
        }
        else if ((IN_I8(Offset) && !Size) || Size == 1)
        {
            Emit(Assembler, 0x6000 | Cond | (Offset & 0xFF), 2);
        }
        else if ((IN_I16(Offset) && !Size) || Size == 2)
        {
            Emit(Assembler, 0x6000 | Cond, 2);
            Emit(Assembler, Offset, 2);
        }
        else
        {
            Emit(Assembler, 0x60FF | Cond, 2);
            Emit(Assembler, Offset, 4);
        }
    } break;
    case TOKEN_DBcc:
    {
        IgnoreSize(Assembler, &Instruction);
        unsigned Reg = ConsumeDataReg(Assembler);
        CONSUME_COMMA();
        uint32_t Offset = IntExpr(Assembler, "Branch target") - (Assembler->PC + 2);
        Emit(Assembler, 
            0x50C8
            | ((uint32_t)Instruction.Data.ConditionalCode << 8)
            | Reg, 2
        );
        Emit(Assembler, Offset, 2);
    } break;
    case TOKEN_RESET:
    case TOKEN_NOP:
    case TOKEN_STOP:
    case TOKEN_RTE:
    case TOKEN_RTS:
    case TOKEN_TRAPV:
    case TOKEN_RTR:
    {
        IgnoreSize(Assembler, &Instruction);
        Emit(Assembler, LOOKUP_OPC(Type), 2);
        if (TOKEN_STOP == Type)
        {
            uint32_t Imm = ConsumeImmediate(Assembler);
            Emit(Assembler, Imm, 2);
        }
    } break;
    case TOKEN_TRAP:
    {
        IgnoreSize(Assembler, &Instruction);
        unsigned Vector = ConsumeImmediate(Assembler);
        Emit(Assembler, 0x4E40 | Vector, 2);
    } break;
    case TOKEN_LINK:
    {
        unsigned Size = ConsumeSize(Assembler);
        NO_BYTE_SIZE(Size);
        unsigned An = ConsumeAddrReg(Assembler);
        CONSUME_COMMA();
        uint32_t Immediate = ConsumeImmediate(Assembler);

        uint16_t Opcode = 4 == Size? 0x4808: 0x4E50;
        Emit(Assembler, Opcode | An, 2);
        Emit(Assembler, Immediate, Size);
    } break;
    case TOKEN_UNLK:
    {
        unsigned An = ConsumeAddrReg(Assembler);
        Emit(Assembler, 0x4E5C | An, 2);
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


