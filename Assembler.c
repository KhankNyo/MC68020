
#include "Common.h"
#include "Assembler.h"
#include <stdarg.h>



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
    TOKEN_PC,
    TOKEN_SP, 

    /* instructions */
    TOKEN_ABCD,
    TOKEN_ADD,
    TOKEN_ADDA,
    TOKEN_ADDI,
    TOKEN_ADDQ,
    TOKEN_ADDX,
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

typedef struct Token 
{
    StringView Lexeme;
    TokenType Type;
    int Line, Offset;
    TokenData Data;
} Token;

static struct 
{
    bool Error, Panic;
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
} Assembler;



#define TO_UPPER(Ch) ((Ch) & ~(1 << 5))
#define IN_RANGE(lower, n, upper) ((lower) <= (n) && (n) <= (upper))
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
            KEYWORD(PC),
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
        },
        ['S'] = {
            KEYWORD(SP),
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
    if ('B' == UpperFirst && 3 == Len)
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
    if ('D' == UpperFirst && 4 == Len && 'B' == UpperSecond)
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
    /* Scc? */
    if ('S' == UpperFirst && 3 == Len)
    {
        unsigned ConditionalCode = GetConditionalCodeFromMnemonic(UpperSecond, UpperThird);
        if (INVALID_CONDITIONAL_CODE != ConditionalCode)
        {
            return MakeTokenWith(TOKEN_Scc, 
                .ConditionalCode = ConditionalCode
            );
        }
    }
    /* TRAPcc? */
    if ('T' == UpperFirst && 6 == Len && StrSliceEquNoCase(Assembler.StartPtr + 1, "RAP", 3))
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

static void ConsumeToken(void)
{
    Assembler.CurrentToken = Assembler.NextToken;
    Assembler.NextToken = Tokenize();
}

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

static void ErrorArgs(const char *Fmt, va_list Args)
{
    if (Assembler.Panic)
        return;

    StringView OffendingExpr = Assembler.CurrentToken.Lexeme;
    StringView Line = {
        .Ptr = OffendingExpr.Ptr - Assembler.CurrentToken.Offset + 1,
        .Len = OffendingExpr.Len + Assembler.CurrentToken.Offset - 1,
    };
    fprintf(Assembler.ErrorStream, 
            "\n%s [Line %d, %d]:\n"
            " | '"STRVIEW_FMT"'\n"
            " |  ", 
            Assembler.SourceName, Assembler.LineCount, Assembler.CurrentToken.Offset,
            STRVIEW_FMT_ARG(Line)
    );
    Highlight(OffendingExpr, Line);
    fprintf(Assembler.ErrorStream, "\n | Error: ");
    vfprintf(Assembler.ErrorStream, Fmt, Args);
    fputc('\n', Assembler.ErrorStream);

    Assembler.Error = true;
    Assembler.Panic = true;
}

static void Error(const char *Fmt, ...)
{
    va_list Args;
    va_start(Args, Fmt);
    ErrorArgs(Fmt, Args);
    va_end(Args);
}

static bool ConsumeOrError(TokenType Type, const char *Fmt, ...)
{
    if (!ConsumeIfNextTokenIs(Type))
    {
        va_list Args;
        va_start(Args, Fmt);
        ErrorArgs(Fmt, Args);
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
    ConsumeToken();
    switch (Assembler.CurrentToken.Type)
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
        return Factor();
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
    return ExprOr();
}


static void PushSymbol(const Token *Sym, Value Val)
{
    if (Assembler.IdenCount < STATIC_ARRAY_SIZE(Assembler.Idens))
    {
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


static void ConsumeStatement(void)
{
    ConsumeToken();
    if (TOKEN_IDENTIFIER == Assembler.CurrentToken.Type)
    {
        DeclStmt();
    }
    else while (!NoMoreToken() && Assembler.CurrentToken.Line == Assembler.NextToken.Line)
    {
        ConsumeToken();
    }
    Assembler.Panic = false;
}

MC68020MachineCode MC68020Assemble(const char *SourceName, const char *Source, FILE *ErrorStream)
{
    Assembler.LineStart = Source;
    Assembler.StartPtr = Source;
    Assembler.CurrPtr = Source;
    Assembler.ErrorStream = ErrorStream;
    Assembler.Error = false;
    Assembler.Panic = false;
    Assembler.MachineCode = (MC68020MachineCode) { 0 };
    Assembler.SourceName = SourceName;
    Assembler.LineCount = 1;
    Assembler.IdenCount = 0;

    Assembler.NextToken = (Token) { 0 };
    ConsumeToken();
    while (!NoMoreToken())
    {
        ConsumeStatement();
    }
    for (size_t i = 0; i < Assembler.IdenCount; i++)
    {
        printf(STRVIEW_FMT" = ", STRVIEW_FMT_ARG(Assembler.Idens[i]));
        if (Assembler.IsFloat[i / 32] & (1ul << (i % 32)))
            printf("%f\n", Assembler.IdenData[i].Flt);
        else
            printf("%lld\n", Assembler.IdenData[i].Int);
    }
    return Assembler.MachineCode;
}



