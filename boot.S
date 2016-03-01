/****************************************************************************
 *                                                                          *
 *                GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  *
 *                                                                          *
 *                                  BOOT                                    *
 *                                                                          *
 *                              Assembly File                               *
 *                                                                          *
 *        Copyright (C) 2012-2013 Università degli Studi di Padova          *	
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
/****************************************************************************
 * This file provides the startup code for executing the ORK kernel on top  *
 * of a custom PowerPC platform, as provided by the PROARTIS Sim simulator. *
 * The statically defined  interrupt vector  has  been adapted to adhere to *
 * the ORK interrupt handling mechanism (based on a common_handler).        *
 * The startup code is the same as that of the system reset handler.        *
 * Note that not all exceptions are actually managed at Ada level:  the ex- *
 * ception handling code for  those exceptions that have been not been con- *
 * sidered to be relevant so far are directly handled at this level through *
 * dummy handlers. However those handlers provide a minimal debugging supp- *
 * ort by saving some relevant information before entering an  unterminated *
 * loop in reserved_0000, reserved_2F00.                                    *
 * When an interrupt is raised, the ciontext of the executing task must be  *
 * saved. It includes the so-called non volatile general purpose and float- *
 * ing point registers, as opposed to the volatile ones that are explicitly *
 * on context swicth.                                                       *
 ****************************************************************************/

        .section ".start", "ax"
#define FRAME_SIZE 200
        /* r4 is available, r1 is set, r3 contains cr,
           sprg3 contains r3 and sprg2 contains r2.  */
#define SAVE_REGS                                \
        /** Set the FP bit in MSR  */        \
        mfmsr   %r4;                             \
        ori     %r4, %r4, MSR_FP;                \
        mtmsr   %r4;                             \
        /* Set up a new frame.  */              \
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
        /* Include FP registers form FPR0 to FPR13 plus FPSCR  */ \
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
        /* Assuming FPSCR need to be saved (not sure ) */ \
        mffs    %f13;                                     \
        stfs    %f13,80(%r1)  /* fpscr_pad + fpscr_val */ \

base = .
        
reserved_0000:
        /* Reserved for implementation specific exceptions      */
        /* Modified to save info on SRR0 and SRR1               */
        /* Set %r4 to ctr to save information in case of fault  */
        mfctr   %r4
        bl failed
        
        /* Restore registers and return from interrupt */
        .globl arch_rfi
arch_rfi:
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
        /* Again assuming FPSCR need to be restored (not sure) */
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
        rfi

        /* Common label used in for intercepting illegal execution privileges */
        /* Modified to save info on SRR0 and SRR1                             */
	.global failed
failed: 
        /* b failed */
        /* %r4 holds the EA of the instruction that branched here */
        /* Save %r4 to SRR0 */
        /* Save MSR to SRR1 */
        mtsrr0  %r4;
        mfmsr   %r3;
        mtsrr1  %r3;
	li	%r3,-1;
//	b 	exit;
        b reserved_2F00;

        . = base + 0x100
        .global _reset
        .global start, _start

system_reset_0100:
_reset:
_start:
start:
        /* Clear BAT */
        ori %r0, %r0, 0;
        /* Using a loop seems to not working 
        li %r1, 16;
        mtctr %r1;
        li %r1, 528;
init_bat:
        mtspr %r1, %r0;
        addi %r1, %r1, 1;
        bdnz init_bat;*/

        /* Just clear the Vs and Vp bits in the upper BAT registers */
        mtspr 528, %r0;
        mtspr 530, %r0; 
        mtspr 532, %r0;
        mtspr 534, %r0;
        mtspr 536, %r0;
        mtspr 538, %r0;
        mtspr 540, %r0;
        mtspr 542, %r0;

        /* Invalidate all segment registers */
        /* Temporary solution a long as only sr1 is currently used */
        /* %r0 is zero */
        //isync  *** NOT SUPPORTED YET ***
        mtsr 0, %r0;
        mtsr 1, %r0;
        mtsr 2, %r0;
        mtsr 3, %r0;
        mtsr 4, %r0;
        mtsr 5, %r0;
        mtsr 6, %r0;
        mtsr 7, %r0;
        mtsr 8, %r0;
        mtsr 9, %r0;
        mtsr 10,%r0;
        mtsr 11, %r0;
        mtsr 12, %r0;
        mtsr 13, %r0;
        mtsr 14, %r0;
        mtsr 15, %r0;
        //isync  *** NOT SUPPORTED YET ***

        /* Init HID0 register to enable Icache and Dcache */
        /* Enabling and invalidating both */
        xor %r1, %r1, %r1;       
        ori %r1, %r1, HID0_ICE; /* set ICE */
        ori %r1, %r1, HID0_ICFI; /* set ICFI */
        //isync;  *** NOT SUPPORTED YET ***
        mtspr HID0, %r1; /* Enable ICache and Invalidate Icache */
        //isync;  *** NOT SUPPORTED YET ***
        ori %r1, %r1, HID0_DCE; /* set DCE */
        ori %r1, %r1, HID0_DCFI; /* set DCFI */
        //isync;  *** NOT SUPPORTED YET ***
        mtspr HID0, %r1; /* Enable Dcache and Invalidate Dcache */
        //isync;  *** NOT SUPPORTED YET ***

        /* Setup stack */
        //lis %r1,(_end)@h		
        //ori %r1,%r1,(_end)@l

        /* Clear back chain */
        //xor %r0,%r0,%r0
        //stw %r0,4(%r1)

        /* Minimal setup for the MSR register */
        /* TODO Extend for real hardware */
        mfmsr %r3
        ori %r3, %r3, MSR_IP
        ori %r3, %r3, MSR_FP
	mtmsr %r3
         
	/* clear .bss */
	b clear_bss


end_clear_bss:
	
        /* Call C function */
        bl main //pok_boot
1:      b  1b

        . = base + 0x200
machine_check_0200:
        /* fails */
        /* SRRO <- EA of some instruction that was executing of about to be */
        /*         In QEMU implementation it holds the NIP                  */
        /* SRR1 <- MSR[RI] if recoverable, imp-dependent otherwise          */
        b reserved_2F00

        . = base + 0x300
dsi_0300:
        mtsprg  3,%r3
        mtsprg  2,%r4
        mfcr    %r3
        mfsrr1  %r4
        andi.   %r4,%r4,MSR_PR
        /* DSI musn't appear in supervisor mode.  */
        /* EM: Set %r4 to ctr to save information in case of fault */
        mfctr   %r4
        beq     failed
        /* DSI musn't appear in supervisor mode.  */

        /* Switch to kernel stack.  */
        mr      %r4,%r1
        mfsprg  %r1,1
        stwu    %r4,-FRAME_SIZE(%r1)

        SAVE_REGS
        mfdar   %r3
        mfdsisr %r4

        /* bl      pok_arch_dsi_int  */
        /* Jump instead to a common handler */
        b       arch_rfi

        . = base + 0x400
isi_0400:
        mtsprg  3,%r3
        mtsprg  2,%r4
        mfcr    %r3
        mfsrr1  %r4
        andi.   %r4,%r4,MSR_PR
        /* EM: Set %r4 to ctr to save information in case of fault */
        mfctr   %r4
        beq     failed 
        /* ISI musn't appear in supervisor mode.  */
        /* beql     failed */
        /* ISI musn't appear in supervisor mode.  */

        /* Switch to kernel stack.  */
        mr      %r4,%r1
        mfsprg  %r1,1
        stwu    %r4,-FRAME_SIZE(%r1)

        SAVE_REGS

        /* bl      pok_arch_isi_int */
        /* Jump instead to a common handler */
        b       arch_rfi

        . = base + 0x500
ext_interrupt_0500:
        /* SRRO <- EA of the instruction that would have been executed next */
        /*         In QEMU implementation it holds the NIP-4                */
        /* SRR1 <- MSR[16-31]                                               */
        b reserved_2F00

        . = base + 0x600
alignment_0600:
        /* SRRO <- EA of the instruction that caused the exception          */
        /*         In QEMU implementation it holds the NIP                  */
        /* SRR1 <- MSR[16-31]                                               */
        /* DSISR<- instruction encoding                                  !Q */
        /* DAR  <- EA of data access computed by the faulty instruction  !Q */
        b reserved_2F00

        . = base + 0x700
program_0700:
        /* SRRO <- (a) EA of the instruction that caused the exception      */
        /*         (b) of the instruction that would have been executed next*/
        /*         In QEMU implementation it holds the NIP                  */
        /* SRR1 <- [11] floating-point enabled exceptions                   */
        /*         [12] illegal instruction exception                       */
        /*         [13] privileged instruction exception                    */
        /*         [14] trap exception                                      */
        /*         [15] SRR0 contains next instruction (a)                  */
        /*         MSR[16-31]                                               */
        b reserved_2F00

        . = base + 0x800
fp_unavailable_0800:
        /* Raised on an attempt to execute a fp instruction when MSR[FP]=0  */ 
        /* SRRO <- (a) EA of the instruction that caused the exception      */
        /*         In QEMU implementation it holds the NIP                  */
        /* SRR1 <- MSR[16-31]                                               */
        b reserved_2F00

        . = base + 0x900
decrementer_0900:
        /* So far this is the only exception to be handled within ORK */
        /* PC saved in srr0,
           MSR saved in srr1.  */
        /* Can use sprg 3 and 2 */
        mtsprg  3,%r3
        mtsprg  2,%r4
        mfcr    %r3
        mfsrr1  %r4
        andi.   %r4,%r4,MSR_PR
        beq     1f  
	/* Coming from user land, switch to system stack (currently infesible) */
        mr      %r4,%r1
        mfsprg  %r1,1
        stwu    %r4,-FRAME_SIZE(%r1)
	b       2f
        /* Coming from super , continue on kernel stack */
1:      stwu    %r1,-FRAME_SIZE(%r1)
2:      SAVE_REGS
	/* bl      pok_arch_decr_int */
        li %r4,9;
        mtsprg 0, %r4;
        bl common_handler
        /* Jump to a common handler (Alarm) */
        b       arch_rfi

        . = base + 0xa00
reserved_0A00:
        /* Reserved for implementation specific exceptions                 */
        b reserved_2F00

        . = base + 0xb00
reserved_0B00:
        /* Reserved for implementation specific exceptions                 */
        b reserved_2F00

        . = base + 0xc00
system_call_0C00:
        mtsprg  3,%r3
        mtsprg  2,%r4
        mfcr    %r3
        mfsrr1  %r4
        andi.   %r4,%r4,MSR_PR
        /* EM: Set %r4 to ctr to save information in case of fault */
        mfctr   %r4
        beq     failed
        /* SC musn't appear in supervisor mode.  */
        /* beql     failed */
        /* SC musn't appear in supervisor mode.  */

        /* Switch to kernel stack.  */
        mr      %r4,%r1
        mfsprg  %r1,1
        stwu    %r4,-FRAME_SIZE(%r1)
        
        SAVE_REGS
        lwz     %r3,20(%r1)
        lwz     %r4,24(%r1)
        lwz     %r5,28(%r1)
        lwz     %r6,32(%r1)
        lwz     %r7,36(%r1)
        lwz     %r8,40(%r1)

        /* Enable interrupt during syscalls.  */
        /* mfmsr   %r0
        ori     %r0,%r0,MSR_EE
        mtmsr   %r0 */

        /* bl      pok_arch_sc_int */
        /* Jump instead to a common handler */
        stw     %r3,20(%r1)
        
        b       arch_rfi

        . = base + 0xd00
trace_0D00:
        /* SRRO <- EA of the instruction that would have been executed next */
        /*         In QEMU implementation it holds the NIP-4                */
        /* SRR1 <- MSR[16-31]                                               */
        b reserved_2F00

        . = base + 0xe00
reserved_0E00:
        /* Should be Floating_point Assist exception  (optional)            */
        b reserved_2F00

        . = base + 0xf00
reserved_0F00:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_1000:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_1100:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_1200:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_1300:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00


         .p2align 8
reserved_1400:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_1500:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_1600:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_1700:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_1800:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_1900:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_1A00:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_1B00:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_1C00:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_1D00:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_1E00:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_1F00:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_2000:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_2100:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_2200:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_2300:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_2400:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_2500:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_2600:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_2700:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_2800:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_2900:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_2A00:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_2B00:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8
reserved_2C00:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00

         .p2align 8

	
reserved_2D00:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00 

         .p2align 8
reserved_2E00:
        /* Reserved for implementation specific exceptions                  */
        b reserved_2F00


        .p2align 8

reserved_2F00:
        /* Set a breakpoint to the following label to intercept all unhandled exceptions */
unhandled_exception:
        /* Reserved for implementation specific exceptions                  */
       //bl failed
       b reserved_2F00

         .p2align 8

reserved_end:



clear_bss:

	/* clear .sbss */
	xor %r0,%r0,%r0
        lis %r3,(__sbss_start-4)@h
        ori %r3,%r3,(__sbss_start-4)@l
        lis %r5,__sbss_end@h
        ori %r5,%r5,__sbss_end@l
        cmplw %r3,%r5
        bge 2f
1:      stwu %r0,4(%r3)
        cmplw %r3,%r5
        blt 1b
2:

	/* clear .sbss2 */
	xor %r0,%r0,%r0
 	lis %r3,__sbss2_start@h
        ori %r3,%r3,__sbss2_start@l
        lis %r5,__sbss2_end@h
        ori %r5,%r5,__sbss2_end@l
        cmplw %r3,%r5 /* check if sbss2 section has dimension not null*/
        bge 2f

        lis %r3,(__sbss2_start-4)@h
        ori %r3,%r3,(__sbss2_start-4)@l
        cmplw %r3,%r5
        bge 2f
1:      stwu %r0,4(%r3)
        cmplw %r3,%r5
        blt 1b
2:
	/* clear .bss */
        lis %r3,(__bss_start-4)@h
        ori %r3,%r3,(__bss_start-4)@l
        lis %r5,__bss_end@h
        ori %r5,%r5,__bss_end@l
        cmplw %r3,%r5
        bge 2f
1:      stwu %r0,4(%r3)
        cmplw %r3,%r5
        blt 1b
2:
	b end_clear_bss


        .section ".reset", "ax"
reset_FFFC:
        b system_reset_0100
