
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

static struct 
{
    AllocatorFn Allocator;
    void (*Emit)(uint8_t *, uint64_t, unsigned);
    bool Error, CriticalError, Panic;
    const char *SourceName;
    FILE *ErrorStream;

    int LineCount;
    const char *LineStart, *StartPtr, *CurrPtr;
    Token CurrentToken, NextToken;

    MC68020MachineCode MachineCode;

    size_t IdenCount;
    StringView Idens[1048];
    uint32_t IsFloat[1048/32];
    union {
        uint64_t Int;
        double Flt;
    } IdenData[1048];

    size_t ExprCount;
    Expression Expr[1048];
} Assembler;



#define TO_UPPER(Ch) ((Ch) & ~(1 << 5))
#define IN_RANGE(lower, n, upper) ((lower) <= (n) && (n) <= (upper))
#define IN_I8(n) IN_RANGE(INT8_MIN, (int64_t)(n), INT8_MAX)
#define IN_I16(n) IN_RANGE(INT16_MIN, (int64_t)(n), INT16_MAX)
#define IN_I32(n) IN_RANGE(INT32_MIN, (int64_t)(n), INT32_MAX)
#define INS_COUNT (TOKEN_UNPK - TOKEN_ABCD)

static bool IsAtEnd(void)
{
    return '\0' == *Assembler.CurrPtr;
}

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

static char PeekChar(void)
{
    if (IsAtEnd())
        return '\0';
    return Assembler.CurrPtr[0];
}

static bool ConsumeIfNextCharIs(char Ch)
{
    if (Ch == PeekChar())
    {
        Assembler.CurrPtr++;
        return true;
    }
    return false;
}

static char ConsumeSpace(void)
{
    while (!IsAtEnd())
    {
        switch (*Assembler.CurrPtr)
        {
        case ' ':
        case '\t':
        case '\r':
        {
            Assembler.CurrPtr++;
        } break;
        case '\n':
        {
            Assembler.CurrPtr++;
            Assembler.LineStart = Assembler.CurrPtr;
            Assembler.LineCount++;
        } break;
        case ';': /* ; comment */
        {
            do {
                Assembler.CurrPtr++;
            } while (!IsAtEnd() && '\n' != *Assembler.CurrPtr);
        } break;
        default: goto Out;
        }
    }
Out:
    Assembler.StartPtr = Assembler.CurrPtr++;
    return *Assembler.StartPtr;
}

static Token MakeTokenWithData(TokenType Type, TokenData Data)
{
    Token Tok = {
        .Type = Type,
        .Lexeme = {
            .Ptr = Assembler.StartPtr,
            .Len = Assembler.CurrPtr - Assembler.StartPtr,
        },
        .Line = Assembler.LineCount,
        .Offset = Assembler.StartPtr - Assembler.LineStart + 1,
        .Data = Data,
    };
    Assembler.StartPtr = Assembler.CurrPtr;
    return Tok;
}
#define MakeTokenWith(Type, ...) \
    MakeTokenWithData(Type, (TokenData) {\
        __VA_ARGS__\
    })
#define MakeToken(Type) MakeTokenWithData(Type, (TokenData) { 0 })

static Token ErrorTokenStr(const char *Msg, size_t MsgLen)
{
    return MakeTokenWith(TOKEN_ERROR, 
        .String.Ptr = Msg,
        .String.Len = MsgLen
    );
}
#define ErrorToken(LiteralMsg) ErrorTokenStr(LiteralMsg, sizeof LiteralMsg)


static Token ConsumeFloatDecimalPlace(uint64_t Integer)
{
    double Double = 0;
    unsigned DecimalPlaces = 0;
    while (IsNumber(*Assembler.CurrPtr) || '_' == *Assembler.CurrPtr)
    {
        char Char = *Assembler.CurrPtr++;
        if ('_' == Char) 
            continue;

        Double *= 10;
        Double += Char - '0';
        DecimalPlaces += 10;
    }
    if (IsAlpha(*Assembler.CurrPtr))
        return ErrorToken("Invalid character in floating-point number.");

    return MakeTokenWith(TOKEN_LIT_FLT,
        .Flt = DecimalPlaces? 
            Integer + (Double / DecimalPlaces) 
            : Integer
    );
}

static Token ConsumeNumber(char First)
{
    int64_t Integer = First - '0';
    while (IsNumber(*Assembler.CurrPtr) || '_' == *Assembler.CurrPtr)
    {
        char Char = *Assembler.CurrPtr++;
        if ('_' == Char) 
            continue;

        Integer *= 10;
        Integer += Char - '0';
    }
    if (IsAlpha(*Assembler.CurrPtr))
        return ErrorToken("Invalid character in integer.");

    if (ConsumeIfNextCharIs('.'))
    {
        return ConsumeFloatDecimalPlace(Integer);
    }
    else
    {
        return MakeTokenWith(TOKEN_LIT_INT, 
            .Int = Integer
        );
    }
}

static Token ConsumeHex(void)
{
    uint64_t Hex = 0;
    char Char = *Assembler.CurrPtr;
    while (!IsAtEnd() && 
        ('_' == Char || IsNumber(Char) || IN_RANGE('A', TO_UPPER(Char), 'F')))
    {
        if ('_' != Char)
        {
            Hex *= 16;
            Hex += IsNumber(Char)?
                Char - '0'
                : TO_UPPER(Char) - 'A' + 10;
        }
        Char = *(++Assembler.CurrPtr);
    }
    if (IsAlpha(*Assembler.CurrPtr))
        return ErrorToken("Invalid character in hexadecimal number.");

    return MakeTokenWith(TOKEN_LIT_INT,
        .Int = Hex
    );
}

static Token ConsumeBinary(void)
{
    uint64_t Binary = 0;
    char Char = *Assembler.CurrPtr;
    while (!IsAtEnd() &&
          ('_' == Char || '1' == Char || '0' == Char))
    {
        if ('_' != Char)
        {
            Binary *= 2;
            Binary += '1' == Char;
        }
        Char = *(++Assembler.CurrPtr);
    }
    if (IsAlphaNum(*Assembler.CurrPtr))
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

static Token ConsumeIdentifier(char FirstLetter)
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


    char UpperFirst = TO_UPPER(FirstLetter);
    char SecondLetter = PeekChar();
    if (('A' == UpperFirst || 'D' == UpperFirst) 
    && IN_RANGE('0', SecondLetter, '8'))
    {
        Assembler.CurrPtr++;
        Token Register = MakeToken('A' == UpperFirst
                ? TOKEN_ADDR_REG
                : TOKEN_DATA_REG
        );
        Register.Data.Int = SecondLetter - '0';
        return Register;
    }


    /* consume the identifier */
    size_t Len = 1;
    while (!IsAtEnd() && IsAlphaNum(*Assembler.CurrPtr))
    {
        Assembler.CurrPtr++;
        Len++;
    }



    /* determine its type */
    char UpperSecond = Len > 1? TO_UPPER(Assembler.StartPtr[1]) : '\0';
    char UpperThird = Len > 2? TO_UPPER(Assembler.StartPtr[2]) : '\0';
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
        char UpperFourth = TO_UPPER(Assembler.StartPtr[3]);
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
        if ('P' == UpperSecond)
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
    if ('T' == UpperFirst && IN_RANGE(5, Len, 6) && StrSliceEquNoCase(Assembler.StartPtr + 1, "RAP", 3))
    {
        unsigned ConditionalCode = GetConditionalCodeFromMnemonic(
                TO_UPPER(Assembler.StartPtr[4]), TO_UPPER(Assembler.StartPtr[5])
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
            && StrSliceEquNoCase(PossibleKeywords[i].Str, Assembler.StartPtr, Len))
            {
                return MakeToken(PossibleKeywords[i].Type);
            }
        }
    }
    return MakeToken(TOKEN_IDENTIFIER);
}



static Token Tokenize(void)
{
    char Current = ConsumeSpace();
    if (IsAlpha(Current))
    {
        return ConsumeIdentifier(Current);
    }
    if (IsNumber(Current))
    {
        return ConsumeNumber(Current);
    }
    switch (Current)
    {
    case '\0': return MakeToken(TOKEN_EOF);
    case '$': return ConsumeHex();
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
        if (IsNumber(PeekChar()))
            return ConsumeFloatDecimalPlace(0);
        return MakeToken(TOKEN_DOT);
    } break;
    case '=': 
    {
        if (ConsumeIfNextCharIs('='))
            return MakeToken(TOKEN_EQUAL_EQUAL);
        return MakeToken(TOKEN_EQUAL);
    } break;
    case '!': 
    {
        if (ConsumeIfNextCharIs('='))
            return MakeToken(TOKEN_BANG_EQUAL);
    } break;
    case '%':
    {
        if (IsNumber(PeekChar()))
            return ConsumeBinary();
        return MakeToken(TOKEN_PERCENT);
    } break;
    case ':': return MakeToken(TOKEN_COLON);
    case '<':
    {
        if (ConsumeIfNextCharIs('<'))
            return MakeToken(TOKEN_LSHIFT);
        if (ConsumeIfNextCharIs('='))
            return MakeToken(TOKEN_LESS_EQUAL);
        return MakeToken(TOKEN_LESS);
    } break;
    case '>':
    {
        if (ConsumeIfNextCharIs('-'))
            return MakeToken(TOKEN_RSHIFT_ARITH);
        if (ConsumeIfNextCharIs('>'))
            return MakeToken(TOKEN_RSHIFT);
        if (ConsumeIfNextCharIs('='))
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




static bool NoMoreToken(void)
{
    return TOKEN_EOF == Assembler.CurrentToken.Type;
}

static TokenType ConsumeToken(void)
{
    Assembler.CurrentToken = Assembler.NextToken;
    Assembler.NextToken = Tokenize();
    return Assembler.CurrentToken.Type;
}

#define NextTokenIs(Typ) (Assembler.NextToken.Type == (Typ))
#define NextTokenIsIndexRegister() (NextTokenIs(TOKEN_ADDR_REG) || NextTokenIs(TOKEN_DATA_REG))

static bool ConsumeIfNextTokenIs(TokenType Type)
{
    if (Assembler.NextToken.Type == Type)
    {
        ConsumeToken();
        return true;
    }
    return false;
}



static void Highlight(StringView Offender, StringView Line)
{
    const char *p = Line.Ptr;
    while (p++ < Offender.Ptr)
    {
        fputc(' ', Assembler.ErrorStream);
    }
    while (p++ <= Offender.Ptr + Offender.Len)
    {
        fputc('^', Assembler.ErrorStream);
    }
}

static void ErrorAtArgs(StringView Offender, StringView Line, const char *Fmt, va_list Args)
{
    if (Assembler.Panic)
        return;

    if (NULL != Assembler.ErrorStream)
    {
        int Offset = Offender.Ptr - Line.Ptr + 1;
        fprintf(Assembler.ErrorStream, 
                "\n%s [Line %d, %d]:\n"
                " | '"STRVIEW_FMT"'\n"
                " |  ", 
                Assembler.SourceName, Assembler.LineCount, Offset,
                STRVIEW_FMT_ARG(Line)
        );
        Highlight(Offender, Line);
        fprintf(Assembler.ErrorStream, "\n | Error: ");
        vfprintf(Assembler.ErrorStream, Fmt, Args);
        fputc('\n', Assembler.ErrorStream);
    }

    Assembler.Error = true;
    Assembler.Panic = true;
}

static size_t LineLen(const char *s)
{
    size_t i = 0;
    while (s[i] && s[i] != '\n' && s[i] != '\r')
        i++;
    return i;
}

static void ErrorAtTokenArgs(const Token *Tok, const char *Fmt, va_list Args)
{
    StringView Line = {.Ptr = Tok->Lexeme.Ptr - Tok->Offset + 1};
    Line.Len = LineLen(Line.Ptr);
    ErrorAtArgs(Tok->Lexeme, Line, Fmt, Args);
}

static void Error(const char *Fmt, ...)
{
    va_list Args;
    va_start(Args, Fmt);
    ErrorAtTokenArgs(&Assembler.CurrentToken, Fmt, Args);
    va_end(Args);
}

static void EmptyError(const char *Fmt, ...)
{
    va_list Args;
    va_start(Args, Fmt);
    
    Assembler.Error = true;
    Assembler.Panic = true;
    if (NULL != Assembler.ErrorStream)
    {
        vfprintf(Assembler.ErrorStream, Fmt, Args);
        fputc('\n', Assembler.ErrorStream);
    }

    va_end(Args);
}

static void ErrorAtToken(const Token *Tok, const char *Fmt, ...)
{
    va_list Args;
    va_start(Args, Fmt);
    ErrorAtTokenArgs(Tok, Fmt, Args);
    va_end(Args);
}

static void ErrorAtExpr(const char *Fmt, ...)
{
    va_list Args;
    va_start(Args, Fmt);
    const Expression *Current = &Assembler.Expr[Assembler.ExprCount - 1];
    if (0 == Assembler.ExprCount)
    {
        UNREACHABLE("Must have an expression before %s", __func__);
    }

    StringView Line = {.Ptr = Current->Str.Ptr - Current->Offset + 1};
    Line.Len = LineLen(Line.Ptr);
    ErrorAtArgs(Current->Str, Line, Fmt, Args);
    va_end(Args);
}


/* returns true if the next token's type is the same as Type
 * else does not consume the token */
static bool ConsumeOrError(TokenType Type, const char *Fmt, ...)
{
    if (!ConsumeIfNextTokenIs(Type))
    {
        va_list Args;
        va_start(Args, Fmt);
        ErrorAtTokenArgs(&Assembler.NextToken, Fmt, Args);
        va_end(Args);
        return false;
    }
    return true;
}



static bool Find(StringView Identifier, Value *Out)
{
    for (unsigned i = 0; i < Assembler.IdenCount; i++)
    {
        if (Identifier.Len == Assembler.Idens[i].Len 
        && StrSliceEquNoCase(Identifier.Ptr, Assembler.Idens[i].Ptr, Identifier.Len))
        {
            if (Assembler.IsFloat[i / 32] & (1ul << (i % 32)))
            {
                Out->IsFloat = true;
                Out->As.Flt = Assembler.IdenData[i].Flt;
            }
            else
                Out->As.Int = Assembler.IdenData[i].Int;
            return true;
        }
    }
    return false;
}


static Value ConstExpr(void);

static Value Factor(void)
{
    switch (ConsumeToken())
    {
    case TOKEN_LIT_INT: return (Value) { .As.Int = Assembler.CurrentToken.Data.Int };
    case TOKEN_LIT_FLT: return (Value) { .As.Flt = Assembler.CurrentToken.Data.Flt, .IsFloat = true };
    case TOKEN_IDENTIFIER:
    {
        Value Val = { 0 };
        if (!Find(Assembler.CurrentToken.Lexeme, &Val))
        {
            Error("Undefined symbol '"STRVIEW_FMT"'.", STRVIEW_FMT_ARG(Assembler.CurrentToken.Lexeme));
        }
        return Val;
    } break;

    case TOKEN_MINUS:
    {
        Value Val = Factor();
        if (Val.IsFloat)
            Val.As.Flt = -Val.As.Flt;
        else
            Val.As.Int = -Val.As.Int;
        return Val;
    } break;
    case TOKEN_PLUS:
    {
        return Factor();
    } break;
    case TOKEN_TILDE:
    {
        Value Val = Factor();
        if (Val.IsFloat)
            Error("'~' is not applicable to floating-point number.");
        Val.As.Int = ~Val.As.Int;
        return Val;
    } break;
    case TOKEN_LPAREN:
    {
        Value Val = ConstExpr();
        ConsumeOrError(TOKEN_RPAREN, "Expected ')' after expression.");
        return Val;
    } break;
    default:
    {
        Error("Expected expression.");
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
        Error(__VA_ARGS__);\
        return (VLeft);\
    }\
    (VLeft).As.Int Op##= Right.As.Int;\
} while (0)

static Value ExprMulDiv(void)
{
    Value Left = Factor();
    while (!Assembler.Panic && !NoMoreToken())
    {
        if (ConsumeIfNextTokenIs(TOKEN_STAR))
        {
            BIN_OP(Left, *, Factor());
        }
        else if (ConsumeIfNextTokenIs(TOKEN_SLASH))
        {
            Value R = Factor();
            if (R.As.Int == 0)
            {
                Error("Division by 0");
                break;
            }
            BIN_OP(Left, /, R);
        }
        else if (ConsumeIfNextTokenIs(TOKEN_PERCENT))
        {
            Value R = Factor();
            if (R.As.Int == 0)
            {
                Error("Division by 0");
                break;
            }
            INT_ONLY(Left, %, R, "Cannot perform modulo on floating-point number.");
        }
        else break;
    }
    return Left;
}

static Value ExprAddSub(void)
{
    Value Left = ExprMulDiv();
    while (!Assembler.Panic && !NoMoreToken())
    {
        if (ConsumeIfNextTokenIs(TOKEN_PLUS))
            BIN_OP(Left, +, ExprMulDiv());
        else if (ConsumeIfNextTokenIs(TOKEN_MINUS))
            BIN_OP(Left, -, ExprMulDiv());
        else break;
    }
    return Left;
}

static Value ExprBitwise(void)
{
    Value Left = ExprAddSub();
    while (!Assembler.Panic && !NoMoreToken())
    {
        if (ConsumeIfNextTokenIs(TOKEN_LSHIFT))
        {
            INT_ONLY(Left, <<, ExprAddSub(), "Cannot perform '<<' on floating-point number.");
        }
        else if (ConsumeIfNextTokenIs(TOKEN_RSHIFT))
        {
            INT_ONLY(Left, >>, ExprAddSub(), "Cannot perform '>>' on floating-point number.");
        }
        else if (ConsumeIfNextTokenIs(TOKEN_RSHIFT_ARITH))
        {
            Value Right = ExprAddSub();
            if (Left.IsFloat || Right.IsFloat)
            {
                Error("Cannot perform '>-' on floating-point number.");
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

static Value ExprEquality(void)
{
    Value Left = ExprBitwise();
    while (!Assembler.Panic && !NoMoreToken())
    {
        if (ConsumeIfNextTokenIs(TOKEN_EQUAL_EQUAL))
            BIN_OP(Left, !=, ExprBitwise());
        else if (ConsumeIfNextTokenIs(TOKEN_BANG_EQUAL))
            BIN_OP(Left, ==, ExprBitwise());
        else break;
    }
    return Left;
}

static Value ExprAnd(void)
{
    Value Left = ExprEquality();
    while (!Assembler.Panic && !NoMoreToken() && ConsumeIfNextTokenIs(TOKEN_AMPERSAND))
    {
        INT_ONLY(Left, &, ExprEquality(), "Cannot perform '&' on floating-point number.");
    }
    return Left;
}

static Value ExprXor(void)
{
    Value Left = ExprAnd();
    while (!Assembler.Panic && !NoMoreToken() && ConsumeIfNextTokenIs(TOKEN_CARET))
    {
        INT_ONLY(Left, ^, ExprAnd(), "Cannot perform '^' on floating-point number.");
    }
    return Left;
}

static Value ExprOr(void)
{
    Value Left = ExprXor();
    while (!Assembler.Panic && !NoMoreToken() && ConsumeIfNextTokenIs(TOKEN_BAR))
    {
        INT_ONLY(Left, |, ExprXor(), "Cannot perform '|' on floating-point number.");
    }
    return Left;
}

static Value ConstExpr(void)
{
    if (Assembler.ExprCount > STATIC_ARRAY_SIZE(Assembler.Expr))
    {
        UNREACHABLE("TODO: make expr array dynamic %s", __func__);
    }
    Expression Expr = {
        .Str = Assembler.NextToken.Lexeme,
        .Line = Assembler.LineCount,
        .Offset = Assembler.NextToken.Offset,
    };

    Value Val = ExprOr();

    Expr.Str.Len = 
        Assembler.CurrentToken.Lexeme.Ptr + Assembler.CurrentToken.Lexeme.Len 
        - Expr.Str.Ptr;
    Assembler.Expr[Assembler.ExprCount++] = Expr;
    return Val;
}

static uint32_t IntExpr(const char *ExprName)
{
    Value Expr = ConstExpr();
    if (Expr.IsFloat)
    {
        ErrorAtExpr("%s cannot be a floating-point number.", ExprName);
    }
    return Expr.As.Int;
}


static void PushSymbol(const Token *Sym, Value Val)
{
    if (Assembler.IdenCount >= STATIC_ARRAY_SIZE(Assembler.Idens))
        UNREACHABLE("TODO: make Symbol table synamic");

    unsigned i = Assembler.IdenCount++;
    Assembler.Idens[i] = Sym->Lexeme;
    if (Val.IsFloat)
    {
        Assembler.IsFloat[i/32] |= 1ul << (i % 32);
        Assembler.IdenData[i].Flt = Val.As.Flt;
    }
    else 
    {
        Assembler.IdenData[i].Int = Val.As.Int;
    }
}

static void DeclStmt(void)
{
    Token Identifier = Assembler.CurrentToken;
    Value Val = { 0 };

    if (ConsumeIfNextTokenIs(TOKEN_EQUAL))
    {
        Val = ConstExpr();
    }
    else if (ConsumeIfNextTokenIs(TOKEN_COLON))
    {
        Val = (Value){
            .IsFloat = false, 
            .As.Int = Assembler.MachineCode.Size 
        };
    }
    else
    {
        Error("Expected '=' or ':' after identifier.");
    }
    PushSymbol(&Identifier, Val);
}




static unsigned ConsumeSizeSpecifier(void)
{
    unsigned Size = 0;
    ConsumeOrError(TOKEN_IDENTIFIER, "Expected operand size specifier.");
    Token Letter = Assembler.CurrentToken;
    if (Letter.Lexeme.Len != 1)
        goto UnknownSize;

    switch (TO_UPPER(Letter.Lexeme.Ptr[0]))
    {
    case 'L': Size = 4; break;
    case 'W': Size = 2; break;
    case 'B': Size = 1; break;
    UnknownSize:
    default: Error("Expected operand size specifier to be 'b', 'w' or 'l'.");
    }
    return Size;
}

/* Next token is TOKEN_ADDR_REG or TOKEN_DATA_REG */
static XnReg ConsumeIndexRegister(void)
{
    XnReg X = { 0 };
    if (ConsumeIfNextTokenIs(TOKEN_ADDR_REG))
    {
        X.n = Assembler.CurrentToken.Data.Int + 8;
    }
    else if (ConsumeOrError(TOKEN_DATA_REG, "Expected register name."))
    {
        X.n = Assembler.CurrentToken.Data.Int;
    }

    if (ConsumeIfNextTokenIs(TOKEN_DOT))
    {
        X.Size = ConsumeSizeSpecifier();
        if (X.Size == 1)
        {
            Error("Cannot use byte size specifier for index register.");
        }
    }

    if (ConsumeIfNextTokenIs(TOKEN_STAR))
    {
        unsigned Scale = IntExpr("Index scalar");

        if (Scale != 1 && Scale != 2 
        && Scale != 4 && Scale != 8)
        {
            ErrorAtExpr("Scale factor must be 1, 2, 4, or 8.");
        }
        X.Scale = CountBits(Scale - 1);
    }
    return X;
}


/* '[' is the current token */
static Argument ConsumeMemoryIndirect(void)
{
    uint32_t BaseDisplacement = 0;
    if (!ConsumeIfNextTokenIs(TOKEN_ADDR_REG)) /* [Expr, An ...] */
    {
        BaseDisplacement = IntExpr("Base Displacement");
        ConsumeOrError(TOKEN_COMMA, "Expected ',' after expression.");
        ConsumeOrError(TOKEN_ADDR_REG, "Expected address register.");
    }

    unsigned An = Assembler.CurrentToken.Data.Int;
    XnReg Xn = { .n = NO_REG };
    ArgumentType Type = ARG_MEM;

    if (ConsumeIfNextTokenIs(TOKEN_COMMA)) /* [Expr, An, Xn] */
    {
        Xn = ConsumeIndexRegister();
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
static Argument IndirectAddressingMode(void)
{
    if (ConsumeIfNextTokenIs(TOKEN_LBRACE)) /* ([...] ...) */
    {
        /* ([...], ...) */
        Argument Arg = ConsumeMemoryIndirect();
        ConsumeOrError(TOKEN_RBRACE, "Expected ']' after memory indirection.");

        if (ConsumeIfNextTokenIs(TOKEN_COMMA)) /* ([], ...) */
        {
            if (NextTokenIsIndexRegister()) /* ([], Xn ...) */
            {
                if (Arg.Type == ARG_MEM_PRE)
                {
                    ErrorAtToken(&Assembler.NextToken, 
                        "Too many index field for Memory Indirect addressing mode."
                    );
                }

                Arg.Type = ARG_MEM_POST;
                Arg.As.Mem.X = ConsumeIndexRegister();
                if (!ConsumeIfNextTokenIs(TOKEN_COMMA))
                    goto NoOuterDisplacement;
            }

            /* ([], Xn, Expr) */
            Arg.As.Mem.Od = IntExpr("Outer Displacement");
        }
NoOuterDisplacement:
        ConsumeOrError(TOKEN_RPAREN, "Expected ')'.");
        return Arg;
    }
    else if (ConsumeIfNextTokenIs(TOKEN_ADDR_REG)) /* (An ...)... */
    {
        unsigned An = Assembler.CurrentToken.Data.Int;
        if (ConsumeIfNextTokenIs(TOKEN_COMMA)) /* (An, Xn) */
        {
            XnReg Xn = ConsumeIndexRegister();
            ConsumeOrError(TOKEN_RPAREN, "Expected ')' after index field.");
            return ARGUMENT(
                ARG_IDX_I8,
                .As.IdxI8 = {
                    .An = An,
                    .X = Xn,
                    .I8 = 0,
                }
            );
        }
        ConsumeOrError(TOKEN_RPAREN, "Expected ')' or ',' after address register."); /* (An)... */
        if (ConsumeIfNextTokenIs(TOKEN_PLUS)) /* (An)+ */
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
        uint32_t Displacement = IntExpr("Displacement");

        if (ConsumeIfNextTokenIs(TOKEN_RPAREN)) /* (Expr) */
        {
            return ARGUMENT(
                ARG_ADDR,
                .As.Addr = Displacement
            );
        } 

        /* (Expr, An ...) */
        ConsumeOrError(TOKEN_COMMA, "Expected ')' or ',' after expression.");
        ConsumeOrError(TOKEN_ADDR_REG, "Expected address register.");
        unsigned An = Assembler.CurrentToken.Data.Int;
        if (ConsumeIfNextTokenIs(TOKEN_COMMA)) /* (Expr, An, Xn) */
        {
            XnReg Xn = ConsumeIndexRegister();
            ConsumeOrError(TOKEN_RPAREN, "Expected ')' after index.");
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
        ConsumeOrError(TOKEN_RPAREN, "Expected ')' or ','.");
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

static Argument PCRelativeAddressingMode(void)
{
    Argument Arg = { 0 };
    ConsumeOrError(TOKEN_LPAREN, "Expected '(' after rel.");

    if (ConsumeIfNextTokenIs(TOKEN_LBRACE)) /* rel ([...] ...) */
    {
        Arg.Type = ARG_PC_MEM;
        Arg.As.PC.Mem.Bd = IntExpr("Base Displacement");
        if (ConsumeIfNextTokenIs(TOKEN_COMMA))
        {
            Arg.Type = ARG_PC_MEM_PRE;
            Arg.As.PC.Mem.X = ConsumeIndexRegister();
        }
        ConsumeOrError(TOKEN_RBRACE, "Expected ']'.");

        if (ConsumeIfNextTokenIs(TOKEN_COMMA))
        {
            if (NextTokenIsIndexRegister())
            {
                if (Arg.Type == ARG_PC_MEM_PRE)
                {
                    Error("Too many index registers for PC-relative Memory Indirect addressing mode.");
                }
                Arg.Type = ARG_PC_MEM_POST;
                Arg.As.PC.Mem.X = ConsumeIndexRegister();
                if (!ConsumeIfNextTokenIs(TOKEN_COMMA))
                    goto NoOuterDisplacement;
            }

            Arg.As.PC.Mem.Od = IntExpr("Outer Displacement");
        }
NoOuterDisplacement:
        /* C is stupid */;
    }
    else if (NextTokenIsIndexRegister()) /* rel (Xn) */
    {
        Arg = ARGUMENT(
            ARG_PC_IDX_I8,
            .As.PC.Idx = {
                .I8 = 0,
                .X = ConsumeIndexRegister()
            }
        );
    }
    else
    {
        uint32_t BaseDisplacement = IntExpr("Base Displacement");
        if (ConsumeIfNextTokenIs(TOKEN_COMMA)) /* (Bd, Xn) */
        {
            XnReg Xn = ConsumeIndexRegister();
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

    ConsumeOrError(TOKEN_RPAREN, "Expected ')' after addressing mode.");
    return Arg;
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


static Argument ConsumeInstructionArgument(unsigned Size)
{
    switch (ConsumeToken())
    {
    default:                /* Addr */
    {
        return ARGUMENT(
            ARG_ADDR,
            .As.Addr = IntExpr("Address")
        );
    } break;
    case TOKEN_POUND:       /* #Imm */
    {
        return ARGUMENT(
            ARG_IMMEDIATE,
            .As.Immediate = IntExpr("Immediate") 
        );
    } break;
    case TOKEN_ADDR_REG:    /* An */
    {
        if (1 == Size)
        {
            Error("Byte size specifier is not applicable to address register.");
        }
        return ARGUMENT(
            ARG_ADDR_REG,
            .As.An = Assembler.CurrentToken.Data.Int
        );
    } break;
    case TOKEN_DATA_REG:    /* Dn */
    {
        return ARGUMENT(
            ARG_DATA_REG,
            .As.Dn = Assembler.CurrentToken.Data.Int
        );
    } break;
    case TOKEN_LPAREN:      /* (...) */
    {
        return IndirectAddressingMode();
    } break;
    case TOKEN_MINUS:
    {
        ConsumeOrError(TOKEN_LPAREN, "Expected '(' after '-'.");
        ConsumeOrError(TOKEN_ADDR_REG, "Expected address register.");
        return ARGUMENT(
            ARG_IND_PREDEC,
            .As.PreDec.An = Assembler.CurrentToken.Data.Int
        );
        ConsumeOrError(TOKEN_RPAREN, "Expected ')' after register.");
    } break;
    case TOKEN_REL:         /* rel (...) */
    {
        return PCRelativeAddressingMode();
    } break;
    }
}

static Argument ConsumeImmediate(void)
{
    ConsumeOrError(TOKEN_POUND, "Expected '#'.");
    return ARGUMENT(
        ARG_IMMEDIATE,
        .As.Immediate = IntExpr("Immediate")
    );
}


static unsigned EncodeExtensionSize(uint32_t Displacement)
{
    if (0 == Displacement)
        return 1;
    if (IN_I16(Displacement))
        return 2;
    return 3;
}

static EaEncoding EncodeEa(Argument Arg, unsigned Size)
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
    case ARG_MEM_POST:
    {
        Encoding.Extension = ENCODE_FULL_EXTENSION(
            Arg.As.Mem.X.n,
            Arg.As.Mem.X.Size == 4,
            Arg.As.Mem.X.Scale,
            Arg.As.Mem.Bd == 0,
            Arg.As.Mem.X.n == NO_REG,
            EncodeExtensionSize(Arg.As.Mem.Bd),
            EncodeExtensionSize(Arg.As.Mem.Od)
        );
        Encoding.u.BaseDisplacement = Arg.As.Mem.Bd;
        Encoding.OuterDisplacement = Arg.As.Mem.Od;
        Encoding.ModeReg = 060 + Arg.As.Mem.An;
    } break;
    case ARG_MEM_PRE:
    {
        Encoding.Extension = ENCODE_FULL_EXTENSION(
            Arg.As.Mem.X.n,
            Arg.As.Mem.X.Size == 4,
            Arg.As.Mem.X.Scale,
            Arg.As.Mem.Bd == 0,
            Arg.As.Mem.X.n == NO_REG,
            EncodeExtensionSize(Arg.As.Mem.Bd),
            EncodeExtensionSize(Arg.As.Mem.Od)
        );
        Encoding.u.BaseDisplacement = Arg.As.Mem.Bd;
        Encoding.OuterDisplacement = Arg.As.Mem.Od;
        Encoding.ModeReg = 060 + Arg.As.Mem.An;
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
            04 + EncodeExtensionSize(Arg.As.Mem.Od)
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

    case ARG_INVALID: DIE(); break;
    }
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

static void Emit(uint64_t Data, unsigned Size)
{
    if (Assembler.CriticalError)
        return;

    if (Assembler.MachineCode.Size + Size >= Assembler.MachineCode.Capacity)
    {
        Assembler.MachineCode.Capacity = (Assembler.MachineCode.Capacity + Size)*2;
        Assembler.MachineCode.Buffer = 
            Assembler.Allocator(Assembler.MachineCode.Buffer, Assembler.MachineCode.Capacity);
        if (NULL == Assembler.MachineCode.Buffer)
        {
            Assembler.CriticalError = true;
            EmptyError("Allocator failed.");
            return;
        }
    }
    Assembler.Emit(&Assembler.MachineCode.Buffer[Assembler.MachineCode.Size], Data, Size);
    Assembler.MachineCode.Size += Size;
}

static void EmitEaExtension(EaEncoding Encoding)
{
    if (Encoding.HasImmediate)
    {
        Emit(Encoding.u.Immediate, IN_I16(Encoding.u.Immediate) ? 2 : 4);
    }

    if (Encoding.Extension == NO_EXTENSION)
        return;

    Emit(Encoding.Extension, 2);
    if (Encoding.u.BaseDisplacement)
    {
        Emit(Encoding.u.BaseDisplacement, IN_I16(Encoding.u.BaseDisplacement) ? 2 : 4);
    }
    if (Encoding.OuterDisplacement)
    {
        Emit(Encoding.OuterDisplacement, IN_I16(Encoding.OuterDisplacement) ? 2 : 4);
    }
}

static void Unpanic(void)
{
    Assembler.Panic = false;
    while (!NoMoreToken() && Assembler.CurrentToken.Line == Assembler.NextToken.Line)
    {
        ConsumeToken();
    }
}

static unsigned ConsumeSize(void)
{
    unsigned Size = 2; /* default size */
    if (ConsumeIfNextTokenIs(TOKEN_DOT))
        Size = ConsumeSizeSpecifier();
    return Size;
}

static void ConsumeStatement(void)
{
    TokenType Type = ConsumeToken();
    Token Current = Assembler.CurrentToken;
    switch (Type)
    {
    case TOKEN_IDENTIFIER:
    {
        DeclStmt();
    } break;
    case TOKEN_ADDI:
    case TOKEN_ANDI:
    case TOKEN_CMPI:
    case TOKEN_EORI:
    case TOKEN_ORI:
    case TOKEN_SUBI:
    {
        static const unsigned OpcodeLut[TOKEN_SUBI - TOKEN_ADDI] = {
            03 << 1, /* ADDI */
            01 << 1, /* ANDI */
            06 << 1, /* CMPI */
            05 << 1, /* EORI */
            00 << 1, /* ORI */
            02 << 1, /* SUBI */
            /* 04 is bit instructions */
        };
        unsigned Size = ConsumeSize();
        Argument Src = ConsumeImmediate();
        ConsumeOrError(TOKEN_COMMA, "Expected ',' after immediate.");
        Argument Dst = ConsumeInstructionArgument(Size);

        switch (Dst.Type)
        {
        case ARG_IMMEDIATE:
        case ARG_ADDR_REG:
        {
            ErrorAtToken(&Current, "'%s' addressing mode is not valid for '"STRVIEW_FMT"'.", 
                    LookupAddrModeName(Dst.Type),
                    STRVIEW_FMT_ARG(Current.Lexeme)
            );
        } break;
        default:
        {
            EaEncoding Ea = EncodeEa(Dst, Size);
            Emit((OpcodeLut[Type - TOKEN_ADDI] << 8)
                | (CountBits(Size - 1) << 6)
                | Ea.ModeReg,
                2
            );
            Emit(Src.As.Immediate, Size);
            EmitEaExtension(Ea);
        } break;
        }
    } break;
    }
    if (Assembler.Panic)
    {
        Unpanic();
    }
}

MC68020MachineCode MC68020Assemble(AllocatorFn Allocator,
        const char *SourceName, const char *Source, bool LittleEndian, FILE *ErrorStream)
{
    Assembler.Emit = LittleEndian? LittleEndianEmitter : BigEndianEmitter;
    Assembler.LineStart = Source;
    Assembler.StartPtr = Source;
    Assembler.CurrPtr = Source;
    Assembler.ErrorStream = ErrorStream;
    Assembler.Error = false;
    Assembler.CriticalError = false;
    Assembler.Panic = false;
    Assembler.SourceName = SourceName;
    Assembler.LineCount = 1;
    Assembler.IdenCount = 0;
    Assembler.ExprCount = 0;
    Assembler.Allocator = Allocator;
    Assembler.MachineCode = (MC68020MachineCode) { 
        .Size = 0,
        .Capacity = 256,
        .Buffer = Allocator(NULL, 256)
    };
    if (NULL == Assembler.MachineCode.Buffer)
    {
        EmptyError("Allocator failed.");
        return Assembler.MachineCode;
    }

    Assembler.NextToken = (Token) { 0 };
    ConsumeToken();
    while (!NoMoreToken() && !Assembler.CriticalError)
    {
        ConsumeStatement();
    }

    if (Assembler.Error || Assembler.CriticalError)
    {
        Allocator(Assembler.MachineCode.Buffer, 0);
        Assembler.MachineCode = (MC68020MachineCode) { 0 };
    }
    return Assembler.MachineCode;
}



