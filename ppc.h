/****************************************************************************
 *                                                                          *
 *                GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  *
 *                                                                          *
 *                                  PPC                                     *
 *                                                                          *
 *                             C Header File                                *
 *                                                                          *
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

/* This file defines sizes and offsets that allow accessing specific bits
   in hardware registers and saving and restoring the hardware status
   to/from the stack or the thread descriptor. */

/* This file is specific for PowerPC */

#ifndef __PPC_ORK_H__
#define __PPC_ORK_H__

/* Machine STate Register (MSR) offsets */
#define MSR_DR  (1 << 4) 
#define MSR_IR  (1 << 5)

#define MSR_IP  (1 << 6)   /* Exception prefix */
#define MSR_FP  (1 << 13)  /* Floating Point enable */
#define MSR_FE0 (1 << 11)  /* Floating Exception mode 0 */
#define MSR_FE1 (1 << 8)   /* Floating Exception mode 1 */

#define MSR_PR  (1 << 14)
#define MSR_EE  (1 << 15)

/* Hardware Implementation Dependent register 0 (HID0) offsets */

#define HID0 1008

#define HID0_NAP (1 << 22)
#define HID0_SLEEP (1 << 21)

#define HID0_ICE (1 << 15)
#define HID0_DCE (1 << 14)
#define HID0_ILOCK (1 << 13)
#define HID0_DLOCK (1 << 12)
#define HID0_ICFI (1 << 11)
#define HID0_DCFI (1 << 10)

#define HID0_INIT_ICACHE (HID0_ICE | HID0_ICFI)
#define HID0_INIT_DCACHE (HID0_DCE | HID0_DCFI)

/* Thread context offsets */

#define SP_OFFSET     0x00
#define LR_OFFSET     0x04
#define CR_OFFSET     0x08
#define GPR_2_OFFSET  0x0c

#define GPR13_OFFSET  0x10
#define GPR14_OFFSET  0x14
#define GPR15_OFFSET  0x18
#define GPR16_OFFSET  0x1c
#define GPR17_OFFSET  0x20
#define GPR18_OFFSET  0x24
#define GPR19_OFFSET  0x28
#define GPR20_OFFSET  0x2c
#define GPR21_OFFSET  0x30
#define GPR22_OFFSET  0x34
#define GPR23_OFFSET  0x38
#define GPR24_OFFSET  0x3c
#define GPR25_OFFSET  0x40
#define GPR26_OFFSET  0x44
#define GPR27_OFFSET  0x48
#define GPR28_OFFSET  0x4c
#define GPR29_OFFSET  0x50
#define GPR30_OFFSET  0x54
#define GPR31_OFFSET  0x58
#define pad0_OFFSET   0x5c

#define FPSCR_OFFSET  0x60
#define FPSCRv_OFFSET 0x64
#define FPR14_OFFSET  0x68
#define FPR15_OFFSET  0x70
#define FPR16_OFFSET  0x78
#define FPR17_OFFSET  0x80
#define FPR18_OFFSET  0x88
#define FPR19_OFFSET  0x90
#define FPR20_OFFSET  0x98
#define FPR21_OFFSET  0xa0
#define FPR22_OFFSET  0xa8
#define FPR23_OFFSET  0xb0 
#define FPR24_OFFSET  0xb8
#define FPR25_OFFSET  0xc0
#define FPR26_OFFSET  0xc8
#define FPR27_OFFSET  0xd0
#define FPR28_OFFSET  0xd8
#define FPR29_OFFSET  0xe0
#define FPR30_OFFSET  0xe8
#define FPR31_OFFSET  0xf0

#define pad1_OFFSET   0xf8
#define pad2_OFFSET   0xfc
#define BACK_OFFSET   0x100
#define LR_U_OFFSET   0x104

#endif

