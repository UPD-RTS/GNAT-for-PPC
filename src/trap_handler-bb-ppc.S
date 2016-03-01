/****************************************************************************
 *                                                                          *
 *                GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  *
 *                                                                          *
 *                          T R A P _ H A N D L E R                         *
 *                                                                          *
 *                               Assembly File                              *
 *                                                                          *
 *      Copyright (C) 1999-2002 Universidad Politecnica de Madrid           *
 *             Copyright (C) 2003-2005 The European Space Agency            *
 *                    Copyright (C) 2003-2010 AdaCore                       *
 *           Copyright (C) 2012 Universita' degli studi di Padova           *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNARL was developed by the GNARL team at Florida State University.       *
 * Extensive contributions were provided by Ada Core Technologies, Inc.     *
 * The  executive  was developed  by the  Real-Time  Systems  Group  at the *
 * Technical University of Madrid.                                          *
 *                                                                          *
 ****************************************************************************/

/* This file is a PowerPC "prep" specific implementation */

/* TODO: As a possible optimisation save the FP registers only when
 * actually needed (see SPARC implementation) */

#include "ppc.h"

#define FRAME_SIZE 200
        /* r4 is available, r1 is set, r3 contains cr,
           sprg3 contains r3 and sprg2 contains r2.  */
#define SAVE_REGS                                \
        /** EM: Set the FP bit in MSR  */        \
        mfmsr   %r4;                             \
        ori     %r4, %r4, MSR_FP;                \
        mtmsr   %r4;                             \
        /* Establish new frame.  */              \
        mflr    %r4;                             \
        stw     %r4,FRAME_SIZE+4(%r1);  /* lr */ \
        mfctr   %r4;                             \
        stw     %r4,64(%r1);  /* ctr */          \
        mfxer   %r4;                             \
        stw     %r4,68(%r1);  /* xer */          \
        stw     %r3,8(%r1);   /* cr */           \
        stw     %r0,12(%r1);  /* r0 */           \
        stw     %r2,16(%r1);  /* r2 */           \
        mfsprg  %r0,3;                           \
        stw     %r0,20(%r1);  /* r3 */           \
        mfsprg  %r0,2;                           \
        stw     %r0,24(%r1);  /* r4 */           \
        stw     %r5,28(%r1);  /* r5 */           \
        stw     %r6,32(%r1);  /* r6 */           \
        stw     %r7,36(%r1);  /* r7 */           \
        stw     %r8,40(%r1);  /* r8 */           \
        stw     %r9,44(%r1);  /* r9 */           \
        stw     %r10,48(%r1); /* r10 */          \
        stw     %r11,52(%r1); /* r11 */          \
        stw     %r12,56(%r1); /* r12 */          \
        stw     %r13,60(%r1); /* r13 */          \
        mfsrr0  %r3;                             \
        stw     %r3,72(%r1);  /* srr0 */         \
        mfsrr1  %r4;                             \
        stw     %r4,76(%r1);  /* srr1 */         \
        /* EM: Include FP registers form FPR0 to FPR13 plus FPSCR  */ \
        stfs    %f0, 88(%r1);  /* fpr0 */ \
        stfs    %f1, 96(%r1);  /* fpr1 */ \
        stfs    %f2, 104(%r1); /* fpr2 */ \
        stfs    %f3, 112(%r1); /* fpr3 */ \
        stfs    %f4, 120(%r1); /* fpr4 */ \
        stfs    %f5, 128(%r1); /* fpr5 */ \
        stfs    %f6, 136(%r1); /* fpr6 */ \
        stfs    %f7, 144(%r1); /* fpr7 */  \
        stfs    %f8, 152(%r1); /* fpr8 */ \
        stfs    %f9, 160(%r1); /* fpr9 */ \
        stfs    %f10,168(%r1); /* fpr10 */ \
        stfs    %f11,176(%r1); /* fpr11 */ \
        stfs    %f12,184(%r1); /* fpr12 */ \
        stfs    %f13,192(%r1); /* fpr13 */ \
        /* EM: Assuming FPSCR need to be saved (not sure about it) */ \
        mffs    %f13;                                     \
        stfs    %f13,80(%r1)  /* fpscr_pad + fpscr_val */ \

/*
	 SRR0 <- PC (trapped pc)
	 SRR1 <- MSR
	 SPRG 0 <- Trap Type (TT)
  */

	.text
	.align 4

	.global common_handler
common_handler:
        mtsprg  3,%r3
        mtsprg  2,%r4
        // E mfcr    %r3
        // E mfsrr1  %r4
        // E andi.   %r4,%r4,MSR_PR
        // E beq     1f  
        // b 1f
 /* Coming from user land, switch to system stack */
        mr      %r4,%r1
        //mfsprg  %r1,1
/*
        lis %r2,(interrupt_stack_address)@h 
        ori %r2,%r2,(interrupt_stack_address)@l
        lwz %r1, 0(%r2);
*/
        stwu    %r4,-FRAME_SIZE(%r1)
// b       2f
        /* Coming from super , continue on kernel stack */
/* 1:      stwu    %r1,-FRAME_SIZE(%r1) */
2:      SAVE_REGS
 
 /* Use user_vector_table as baseline to compute the position
  * in the user vector table and get the handler address
  * and call it! 
   */
        /* Load the interrupt ID form SPRG0 (see boot.S) */
        mfsprg %r2, 0;
        /* The handler address is stored in position
         * "user_vector_table" + %r2*4 */
 // DEBUG: r6 holds the Interrupt_ID
 mr %r6, %r2;
        mulli %r2, %r2, 4;
 // DEBUG: r7 holds the Interrupt offset
        mr %r7, %r2;
 lis %r3,(user_vector_table)@h 
        ori %r3,%r3,(user_vector_table)@l
        add %r3, %r3, %r2;
 /* So r5 holds the Interrupt position in vector_table 
  * TODO: Rewrite to simply use %r3
         */
        mr %r5, %r3;
 /* load the interrupt handler's address in %r8 */
        lwz %r8, 0(%r5);
 /* set the ctr */
        mtctr %r8;
 /* Load Interrupt_ID into %r3 as parameter for the Interrupt wrapper */
        mfsprg %r3, 0;
 /* Branch to the interrupt wrapper */
        bctrl

 bl context_switch_needed
        cmpwi   cr7, %r3, 0
 beq cr7, restore_frame
 bl      context_switch


restore_frame:
        /* Restore stack frame and rfi */
        lwz     %r13,60(%r1)
        lwz     %r12,56(%r1)
        lwz     %r11,52(%r1)
        lwz     %r10,48(%r1)
        lwz     %r9,44(%r1)
        lwz     %r8,40(%r1)
        lwz     %r7,36(%r1)
        lwz     %r6,32(%r1)
        lwz     %r5,28(%r1)
        lwz     %r4,24(%r1)
        lwz     %r3,20(%r1)
        lwz     %r2,16(%r1)
        /* EM: Assuming FPSCR need to be restored (not sure about it) */
        lfs    %f0, 80(%r1)  /* fpscr_pad + fpscr_val */
        mtfsf  0xff, %f0;                                     
        lfs    %f0, 88(%r1);  /* fpr0 */ 
        lfs    %f1, 96(%r1);  /* fpr1 */
        lfs    %f2, 104(%r1); /* fpr2 */
        lfs    %f3, 112(%r1); /* fpr3 */
        lfs    %f4, 120(%r1); /* fpr4 */
        lfs    %f5, 128(%r1); /* fpr5 */
        lfs    %f6, 136(%r1); /* fpr6 */
        lfs    %f7, 144(%r1); /* fpr7 */ 
        lfs    %f8, 152(%r1); /* fpr8 */
        lfs    %f9, 160(%r1); /* fpr9 */
        lfs    %f10,168(%r1); /* fpr10 */
        lfs    %f11,176(%r1); /* fpr11 */
        lfs    %f12,184(%r1); /* fpr12 */
        lfs    %f13,192(%r1); /* fpr13 */

        lwz     %r0,204(%r1) 
        mtlr    %r0
        lwz     %r0,64(%r1)
        mtctr   %r0
        lwz     %r0,68(%r1)
        mtxer   %r0
        lwz     %r0,72(%r1)
        mtsrr0  %r0
        lwz     %r0,76(%r1)
        mtsrr1  %r0
        andi.   %r0,%r0,MSR_PR
        beq     1f
        addi    %r0,%r1,FRAME_SIZE
	mtsprg  1, %r0
1:      lwz     %r0,8(%r1)
        mtcr    %r0
        lwz     %r0,12(%r1)
        lwz     %r1,0(%r1)
        b 	arch_rfi
	//rfi

/* TODO: Label used to debug assembly code: it can be safely removed
	.global user_vector_table
user_vector_table:
        .long 0x45567
*/

