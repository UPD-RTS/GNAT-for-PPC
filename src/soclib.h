/****************************************************************************
 *                                                                          *
 *                GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  *
 *                                                                          *
 *                                SOCLIB                                    *
 *                                                                          *
 *                             C Header File                                *
 *                                                                          *
 *          Copyright (C) 2012-2013 Università degli Studi di Padova        *
 *                                                                          *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 *                                                                          *
 *                                                                          * 
 *                                                                          * 
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 ****************************************************************************/

/* This file offers SocLib specific defintions. 
 * Should be safely removed.
 */

#ifndef __SOCLIB_H__
#define __SOCLIB_H__

/*
typedef unsigned short        uint8_t;
typedef unsigned short        uint16_t;
typedef unsigned int          uint32_t;
typedef unsigned long long    uint64_t;

typedef short                 int8_t;
typedef short                 int16_t;
typedef signed long long      int64_t;

typedef unsigned int          size_t;
typedef unsigned long int     intptr_t;
*/

#define get_cp2(x, sel)                                                                 \
({unsigned int __cp2_x;                                                         \
__asm__("mfc2 %0, $"#x", "#sel:"=r"(__cp2_x));  \
__cp2_x;})

#define DO_MEMORY_SYNC __asm__ volatile("" ::: "memory")
#define MAX(x,y) ( ( (x) >= (y) ) ? (x) : (y) )
/*Digits after radix point of a double to be printed */
#define FP_DIGITS (10)
#define SIZE_OF_BUF MAX( 1024,(FP_DIGITS + 16))

#define	TTY_BASE	0xC0800000 
#define	TTY_SIZE	0x00000040
#define base(x) (void*)(x##_BASE)

//#define putchar __inline_putchar

#define NaNdMaskF               (0x0007FFFFFFFFFFFFLLU)
#define SNaNdMaskE              (0x7FF8000000000000LLU)
#define PINFId                  (0x7FF0000000000000LLU)
#define NINFId                  (0xFFF0000000000000LLU)
#define SignMaskd               (0x8000000000000000LLU)

/* Prototypes for ansi compatibility*/
/*
int Is_QNaNd(uint64_t i64);
int Is_NaNd(uint64_t i64);
int Is_PINFd(uint64_t i64);
int Is_NINFd(uint64_t i64);
int Is_INFd(uint64_t i64);
int Is_Negatived(uint64_t i64);
*/

#define dcr_get(x)					\
({unsigned int __val;				\
__asm__("mfdcr %0, "#x:"=r"(__val));\
__val;})


/*
enum SoclibTtyRegisters {
    TTY_WRITE = 0,
    TTY_STATUS = 1,
    TTY_READ  = 2,
    TTY_SPAN  = 4,
};
*/

//void soclib_io_write8(void *comp_base, size_t reg, uint8_t val);

/*
static __inline__ int Is_SNaNd(uint64_t i64)
{
	return ((((i64 & SNaNdMaskE)== 0x7FF0000000000000LLU) && (i64 & NaNdMaskF) ) ? 1 :0);
}

static __inline__ int Is_QNaNd(uint64_t i64)
{
	return ((((i64 & SNaNdMaskE)== 0x7FF8000000000000LLU) ) ? 1 : 0);
}

static __inline__ int Is_NaNd(uint64_t i64)
{
	return ((Is_SNaNd(i64) || Is_QNaNd(i64)) ? 1 : 0);
}

static __inline__ int Is_PINFd(uint64_t i64)
{
	return ((i64 == PINFId) ? 1 : 0 );
}
static __inline__ int Is_NINFd(uint64_t i64)
{
	return ((i64 == NINFId) ? 1 : 0 );
}
static __inline__ int Is_INFd(uint64_t i64)
{
	return ((Is_PINFd(i64) || Is_NINFd(i64)) ? 1 : 0);
}
static __inline__ int Is_Negatived(uint64_t i64)
{
	return ((i64 & SignMaskd) ? 1 : 0 );
}
*/

//int putchar(const int x);

/*
static __inline__ void soclib_io_set(void *comp_base, size_t reg, uint32_t val)
{
        volatile uint32_t *addr = (uint32_t *)comp_base;

    reg <<= 2;
    __asm__("stwbrx %0, %1, %2":: "b"(val), "b"(addr), "b"(reg) : "memory" );
}

static  __inline__ void trap(){
	__asm__ volatile("trap"); 
}
*/
// __inline__ void exit(int level)
/*{


#define	SIMHELPER_BASE			0xD0800000
#define	SIMHELPER_END_WITH_RETVAL	1

soclib_io_set(
		
                base(SIMHELPER),
                SIMHELPER_END_WITH_RETVAL,
                level);
		trap();	
		while (1);	
}*/



#endif // SOCLIB

