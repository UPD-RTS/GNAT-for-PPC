/****************************************************************************
 *                                                                          *
 *                GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  *
 *                                                                          *
 *                      C O N T E X T _ S W I T C H                         *
 *                                                                          *
 *                              Assembly File                               *
 *                                                                          *
 *      Copyright (C) 2012-2013 Università degli Studi di Padova            *
 *      Copyright (C) 1999-2002 Universidad Politecnica de Madrid           *
 *             Copyright (C) 2003-2005 The European Space Agency            *
 *                     Copyright (C) 2003-2005 AdaCore                      *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 * MA 02111-1307, USA.                                                      *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 * GNARL was developed by the GNARL team at Florida State University.       *
 * Extensive contributions were provided by Ada Core Technologies, Inc.     *
 * The  executive  was developed  by the  Real-Time  Systems  Group  at the *
 * Technical University of Madrid.                                          *
 *                                                                          *
 ****************************************************************************/

/* This file is a PowerPC specific implementation for the PROARTIS Sim sim- *
 * ulator developed by BSC within the scope of the PROARTIS project.        */

#include "ppc.h"

/* procedure context_switch.
   This routine performs a context switch between the two tasks that are
   specified in running_thread and first_thread. */

	.text
	.align 4

/** 
 *      Include volatile FP registers form FPR14 to FPR31 (64bit each) plus FPSCR.
 *      The thread context has been modified accordingly.
 */
        .text
        .globl context_switch
context_switch:

	/*mfmsr   %r4;                             \
        xori     %r4, %r4, MSR_EE;                \
        mtmsr   %r4;     */

	/* Volatile registers were already saved on interrupt */
        /* Get the address of the task descriptor of the currently running
	 * task for saving its state.  Put it in %r3            
         */
	lis 	%r4,(running_thread)@h	
        ori 	%r4,%r4,(running_thread)@l 
        /* Dereference ppointer */
        lwz	%r3, 0(%r4)
        
	/* Save stack register */
	stw 	%r1, SP_OFFSET(%r3)
	/* Save link register */
        mflr 	%r4
        stw  	%r4, LR_OFFSET(%r3)
	/* Save condition register */
        mfcr 	%r4
        stw  	%r4, CR_OFFSET(%r3)
        stw  	%r2, GPR_2_OFFSET(%r3)	


        /* Save registers.  */
        stw     %r13, GPR13_OFFSET(%r3)	
        stw     %r14, GPR14_OFFSET(%r3)	
        stw     %r15, GPR15_OFFSET(%r3)	
        stw     %r16, GPR16_OFFSET(%r3)	
        stw     %r17, GPR17_OFFSET(%r3)	
        stw     %r18, GPR18_OFFSET(%r3)	
        stw     %r19, GPR19_OFFSET(%r3)	
        stw     %r20, GPR20_OFFSET(%r3)	
        stw     %r21, GPR21_OFFSET(%r3)	
        stw     %r22, GPR22_OFFSET(%r3)	
        stw     %r23, GPR23_OFFSET(%r3)
        stw     %r24, GPR24_OFFSET(%r3)	
        stw     %r25, GPR25_OFFSET(%r3)	
        stw     %r26, GPR26_OFFSET(%r3)	
        stw     %r27, GPR27_OFFSET(%r3)	
        stw     %r28, GPR28_OFFSET(%r3)	
        stw     %r29, GPR29_OFFSET(%r3)	
        stw     %r30, GPR30_OFFSET(%r3)	
        stw     %r31, GPR31_OFFSET(%r3)	
        /* Assuming FPSCR need to be saved (not sure) */
        mffs    %f14;
        stfs    %f14, FPSCR_OFFSET(%r3)	
        stfs    %f14, FPR14_OFFSET(%r3)	
        stfs    %f15, FPR15_OFFSET(%r3)	
        stfs    %f16, FPR16_OFFSET(%r3)	
        stfs    %f17, FPR17_OFFSET(%r3)	
        stfs    %f18, FPR18_OFFSET(%r3)	
        stfs    %f19, FPR19_OFFSET(%r3)	
        stfs    %f20, FPR20_OFFSET(%r3)	
        stfs    %f21, FPR21_OFFSET(%r3)	
        stfs    %f22, FPR22_OFFSET(%r3)	
        stfs    %f23, FPR23_OFFSET(%r3)	
        stfs    %f24, FPR24_OFFSET(%r3)	
        stfs    %f25, FPR25_OFFSET(%r3)	
        stfs    %f26, FPR26_OFFSET(%r3)	
        stfs    %f27, FPR27_OFFSET(%r3)	
        stfs    %f28, FPR28_OFFSET(%r3)	
        stfs    %f29, FPR29_OFFSET(%r3)	
        stfs    %f30, FPR30_OFFSET(%r3)	
        stfs    %f31, FPR31_OFFSET(%r3)	

	/* Switch stack. */
	lis 	%r4,(running_thread)@h	
        ori 	%r4,%r4,(running_thread)@l 
        /* Dereference pointer */
        mr 	%r6, %r4
        lwz 	%r7, 0(%r4)
        //lwz	%r3, 0(%r4)
        /* Get the first_thread in r3 */
        lis 	%r3, (first_thread)@h	
        ori 	%r3, %r3, (first_thread)@l
        /* Dereference ppointer */
        lwz	%r5, 0(%r3)
	/* Update running thread pointer */
        stw	%r5, 0(%r4)	
       
        /* Restore registers.  */
        /* Assuming FPSCR need to be restored (not sure) */
        lfs     %f14, FPSCR_OFFSET(%r5) /* fpscr_pad + fpscr_val */ 
        mtfsf   0xff, %f14           
        lfs     %f14, FPR14_OFFSET(%r5) /* fpr14 */ 
        lfs     %f15, FPR15_OFFSET(%r5) /* fpr15 */
        lfs     %f16, FPR16_OFFSET(%r5) /* fpr16 */
        lfs     %f17, FPR17_OFFSET(%r5) /* fpr17 */
        lfs     %f18, FPR18_OFFSET(%r5) /* fpr18 */
        lfs     %f19, FPR19_OFFSET(%r5) /* fpr19 */
        lfs     %f20, FPR20_OFFSET(%r5) /* fpr20 */
        lfs     %f21, FPR21_OFFSET(%r5) /* fpr21 */ 
        lfs     %f22, FPR22_OFFSET(%r5) /* fpr22 */
        lfs     %f23, FPR23_OFFSET(%r5) /* fpr23 */
        lfs     %f24, FPR24_OFFSET(%r5) /* fpr24 */
        lfs     %f25, FPR25_OFFSET(%r5) /* fpr25 */
        lfs     %f26, FPR26_OFFSET(%r5) /* fpr26 */
        lfs     %f27, FPR27_OFFSET(%r5) /* fpr27 */
        lfs     %f28, FPR28_OFFSET(%r5) /* fpr28 */
        lfs     %f29, FPR29_OFFSET(%r5) /* fpr29 */
        lfs     %f30, FPR30_OFFSET(%r5) /* fpr30 */
        lfs     %f31, FPR31_OFFSET(%r5) /* fpr31 */

        lwz     %r31, GPR31_OFFSET(%r5)
        lwz     %r30, GPR30_OFFSET(%r5)
        lwz     %r29, GPR29_OFFSET(%r5)
        lwz     %r28, GPR28_OFFSET(%r5)
        lwz     %r27, GPR27_OFFSET(%r5)
        lwz     %r26, GPR26_OFFSET(%r5)
        lwz     %r25, GPR25_OFFSET(%r5)
        lwz     %r24, GPR24_OFFSET(%r5)
        lwz     %r23, GPR23_OFFSET(%r5)
        lwz     %r22, GPR22_OFFSET(%r5)
        lwz     %r21, GPR21_OFFSET(%r5)
        lwz     %r20, GPR20_OFFSET(%r5)
        lwz     %r19, GPR19_OFFSET(%r5)
        lwz     %r18, GPR18_OFFSET(%r5)
        lwz     %r17, GPR17_OFFSET(%r5)
        lwz     %r16, GPR16_OFFSET(%r5) 
        lwz     %r15, GPR15_OFFSET(%r5) 
        lwz     %r14, GPR14_OFFSET(%r5) 
        lwz     %r13, GPR13_OFFSET(%r5) 
        lwz     %r3,  GPR_2_OFFSET(%r5) // Parameter
        lwz     %r0,  CR_OFFSET(%r5)
        mtcr    %r0
        lwz     %r1,  SP_OFFSET(%r5)
        lwz     %r0,  LR_OFFSET(%r5)
        mtlr    %r0
	//mr	%r28, %r0 //AB debug
        blr
	//b failed

        .size context_switch, . - context_switch
