#ifndef MC68020_ISA_H
#define MC68020_ISA_H 


typedef enum MC68020Ins
{
    MC68020_ADD = 0xD,
} MC68020Ins;

typedef enum MC68020EA 
{
    EA_DN = 0,
    EA_AN,
    EA_ADDR,
    EA_ADDR_INC,
    EA_ADDR_DEC,
    EA_ADDR_I16,
    EA_ADDR_XI8,
    EA_INDEX, 
    EA_SPECIAL,
} MC68020EA;

typedef enum MC68020OpMode
{
    OPMODE_BYTE = 0, 
    OPMODE_WORD, 
    OPMODE_LONG, 
    OPMODE_REV_BYTE = 0x4,
    OPMODE_REV_WORD, 
    OPMODE_REV_LONG, 
} MC68020OpMode;

typedef enum MC68020MemInd 
{
    MEMIND_IDX_ONLY = 0,
    MEMIND_MEM_PREI_NULL,
    MEMIND_MEM_PREI_WORD,
    MEMIND_MEM_PREI_LONG,
    MEMIND_RESV1,
    MEMIND_MEM_POSTI_NULL,
    MEMIND_MEM_POSTI_WORD,
    MEMIND_MEM_POSTI_LONG,
    MEMIND_NONE,
    MEMIND_IND_NULL,
    MEMIND_IND_WORD,
    MEMIND_IND_LONG,
    MEMIND_RESV2,
} MC68020MemInd;


#define GET_OP(Opcode)          (MC68020Ins)((Opcode) >> 12)
#define GET_OPMODE(Opcode)      (MC68020OpMode)(((Opcode) >> 6) & 0x3)
#define GET_DN(Opcode)          (((Opcode) >> 9) & 0x7)

#define EA_FIELD_IS_COMPLICATED(EaField) ((EaField) & (1 << 8))
#define EA_IDX_IS_LONG(EaField) ((EaField) & (1 << 11))
#define GET_EA_IDX(EaField)     ((EaField) >> 12)
#define GET_EA_FIELD(Opcode)    (MC68020EA)((Opcode) & 0x3F)
#define GET_EA_SCALE(EaField)   (((EaField) >> 9) & 0x3)

#define GET_MEMIND(EaField)     (MC68020MemInd)((EaField) & 0x47)




#endif /* MC68020_ISA_H */

