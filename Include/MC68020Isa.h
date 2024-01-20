#ifndef MC68020_ISA_H
#define MC68020_ISA_H 


typedef enum MC68020Ins
{
    MC68020_MOVEB = 0x1,
    MC68020_MOVEL,
    MC68020_MOVEW,
    MC68020_ADD = 0xD,
    MC68020_SUB = 0x9,

} MC68020Ins;

typedef enum MC68020EAMode 
{
    EA_DN = 0,
    EA_AN,
    EA_ADDR,
    EA_ADDR_INC,
    EA_ADDR_DEC,
    EA_ADDR_I16,
    EA_INDEX, 
    EA_SPECIAL,
} MC68020EAMode;

typedef enum MC68020Mode 
{
    MODE_DN = 0,
    MODE_AN,
    MODE_IND, 
    MODE_POSTINC,
    MODE_PREDEC,
    MODE_IND_I16,
    MODE_INDEX,
    MODE_SPECIAL,
} MC68020Mode;

typedef enum MC68020Size
{
    SIZE_BYTE = 0, 
    SIZE_WORD, 
    SIZE_LONG, 
    SIZE_ADDR_WORD,
    SIZE_X_BYTE, 
    SIZE_X_WORD, 
    SIZE_X_LONG, 
    SIZE_ADDR_LONG, 
} MC68020Size;

typedef enum MC68020Flags
{
    FLAG_C = 0,
    FLAG_V, 
    FLAG_Z, 
    FLAG_N,
    FLAG_X,
} MC68020Flags;

typedef enum MC68020ConditionalCode 
{
    CC_T = 0, /* why the fuck is true 0??? */
    CC_F,   /* False */
    CC_HI,  /* Higher */
    CC_LS,  /* Lower or Same */
    CC_CC,  /* Carry Clear */
    CC_CS,  /* Carry Set */
    CC_NE,  /* Not Equal */
    CC_EQ,  /* Equal */
    CC_VC,  /* oVerflow Clear */
    CC_VS,  /* oVerflow Set */
    CC_PL,  /* Plus */
    CC_MI,  /* Minus */
    CC_GE,  /* Greater or Equal */
    CC_LT,  /* Less Than */
    CC_GT,  /* Greater Than */
    CC_LE,  /* Less than or Equal */
} MC68020ConditionalCode;

typedef enum MC68020Direction 
{
    DIRECTION_TO_REG = 0,
    DIRECTION_TO_EA,

    DIRECTION_LEFT = 0,
    DIRECTION_RIGHT,
} MC68020Direction;

#define GET_DATASIZE(Opcode)    (((Opcode) >> 6) & 0x7)
#define GET_DIRECTION(Opcode)   (((Opcode) >> 8) & 0x1)
#define GET_DN(Opcode)          (((Opcode) >> 9) & 0x7)
#define GET_MODE(Opcode)        (((Opcode) >> 3) & 0x7)
#define GET_CC(Opcode)          (MC68020ConditionalCode)(((Opcode) >> 8) & 0xF)

#define EA_FIELD_8BIT_DISPLACEMENT(EaField) (0 == ((EaField) & (1 << 8)))
#define EA_IDX_IS_LONG(EaField) ((EaField) & (1 << 11))
#define GET_EA_IDX(EaField)     ((EaField) >> 12)
#define GET_EA_FIELD(Opcode)    (MC68020EAMode)((Opcode) & 0x3F)
#define GET_EA_SCALE(EaField)   (((EaField) >> 9) & 0x3)
#define GET_EA_OUTER_DISPLACEMENT_TYPE(EaField) (MC68020OuterDisplacementType)((EaField) & 0x3)
#define EA_POST_INDEX(EaField)  ((EaField) & 0x4)

#define IS_CCR_INSTRUCTION(Opcode) (0x3E == ((Opcode) & 0xFF))
#define IS_SR_INSTRUCTION(Opcode) (0x7E == ((Opcode) & 0xFF))

#define CCR_IMPL_BITS 0x1Fu
#define SR_IMPL_BITS 0xF71Fu



#endif /* MC68020_ISA_H */

