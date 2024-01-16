#ifndef COMMON_H
#define COMMON_H

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

#define MASK(Value, Size) \
    ((Value) & ((1ull << (Size)) - 1))
#define SEX(To, From) (int##To##_t)(int##From##_t)
#define STATIC_ARRAY_SIZE(Array) (sizeof(Array) / sizeof((Array)[0]))

#define strfy_1(Expression) #Expression
#define STRFY(Expression) strfy_1(Expression)
#define DIE() (*(volatile char *)0 = 0)
#define UNREACHABLE(...) do {\
    fprintf(stderr, __FILE__": Unreachable on line "STRFY(__LINE__)":\n    "\
            __VA_ARGS__\
    );\
    fputc('\n', stderr);\
    DIE();\
} while (0)


#if defined(__LITTLE_ENDIAN__)
#  define HOST_IS_LITTLE_ENDIAN 1
#elif defined(__BIG_ENDIAN__)
#  define HOST_IS_BIG_ENDIAN 1
#elif !defined(__LITTLE_ENDIAN__) && !defined(__BIG_ENDIAN__)
#  if (defined(__BYTE_ORDER__)  && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__) || \
    (defined(__BYTE_ORDER) && __BYTE_ORDER == __BIG_ENDIAN) || \
    (defined(_BYTE_ORDER) && _BYTE_ORDER == _BIG_ENDIAN) || \
    (defined(BYTE_ORDER) && BYTE_ORDER == BIG_ENDIAN) || \
    (defined(__sun) && defined(__SVR4) && defined(_BIG_ENDIAN)) || \
    defined(__ARMEB__) || defined(__THUMBEB__) || defined(__AARCH64EB__) || \
    defined(_MIBSEB) || defined(__MIBSEB) || defined(__MIBSEB__) || \
    defined(_M_PPC)
#    define HOST_IS_BIG_ENDIAN 1
#  elif (defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__) || /* gcc */\
    (defined(__BYTE_ORDER) && __BYTE_ORDER == __LITTLE_ENDIAN) /* linux header */ || \
    (defined(_BYTE_ORDER) && _BYTE_ORDER == _LITTLE_ENDIAN) || \
    (defined(BYTE_ORDER) && BYTE_ORDER == LITTLE_ENDIAN) /* mingw header */ ||  \
    (defined(__sun) && defined(__SVR4) && defined(_LITTLE_ENDIAN)) || /* solaris */ \
    defined(__ARMEL__) || defined(__THUMBEL__) || defined(__AARCH64EL__) || \
    defined(_MIPSEL) || defined(__MIPSEL) || defined(__MIPSEL__) || \
    defined(_M_IX86) || defined(_M_X64) || defined(_M_IA64) || /* msvc for intel processors */ \
    defined(_M_ARM) /* msvc code on arm executes in little endian mode */
#    define HOST_IS_LITTLE_ENDIAN 1
#  endif
#else
#  error "fuck your c compiler"
#endif


#if HOST_IS_LITTLE_ENDIAN
#  define HOST_IS_BIG_ENDIAN 0
#else
#  define HOST_IS_LITTLE_ENDIAN 0
#endif


#define TO_UPPER(Ch) ((Ch) & ~(1 << 5))
#define IN_RANGE(lower, n, upper) ((lower) <= (n) && (n) <= (upper))
#define IN_I8(n) IN_RANGE((int64_t)INT8_MIN, (int64_t)(n), (int64_t)INT8_MAX)
#define IN_I16(n) IN_RANGE((int64_t)INT16_MIN, (int64_t)(n), (int64_t)INT16_MAX)
#define IN_I32(n) IN_RANGE((int64_t)INT32_MIN, (int64_t)(n), (int64_t)INT32_MAX)


static inline unsigned CountBits(uint64_t n)
{
    unsigned i = 0;
    while (n)
    {
        n &= n - 1;
        i++;
    }
    return i;
}

static inline uint64_t uMax(uint64_t a, uint64_t b)
{ return a > b? a: b; }
static inline int64_t iMax(int64_t a, int64_t b)
{ return a > b? a: b; }
static inline uint64_t uMin(uint64_t a, uint64_t b)
{ return a > b? b: a; }
static inline int64_t iMin(int64_t a, int64_t b)
{ return a > b? b: a; }


#endif /* COMMON_H */

