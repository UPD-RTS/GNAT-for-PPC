/****************************************************************************
 *                                                                          *
 *                GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  *
 *                                                                          *
 *                    I N T E R R U P T _ M A S K I N G                     *
 *                                                                          *
 *                             Assembly File                                *
 *                                                                          *
 *      Copyright (C) 1999-2002 Universidad Politecnica de Madrid           *
 *             Copyright (C) 2003-2005 The European Space Agency            *
 *                     Copyright (C) 2003-2005 AdaCore                      *
 *          Copyright (C) 2012-2013 Università degli Studi di Padova        *
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

/* This file is a PowerPC specific implementation */

/* Interrupt enabling/disabling primitives.
   These routines are responsible for changing the EE field in the MSR
   for enabling and disabling interrupts (asynchronous traps). */

#include "ppc.h"

	.text
	.align 4

/* procedure disable_interrupts */

	.global disable_interrupts
	.type   disable_interrupts,@function
disable_interrupts:
	xori	%r4, %r4, %r4;
	mfmsr   %r4;                             
	ori     %r4, %r4, MSR_EE;
        xori    %r4, %r4, MSR_EE;
        mtmsr   %r4;
        blr

	.size disable_interrupts, . - disable_interrupts

/* procedure enable_interrupts (interrupt_level).
   The first and only argument (passed through r3) could determine the
   interrupt level above which interrupts will be acknowledged. 
   However, a single level is currently supported/required,
 */

	.global enable_interrupts
	.type   enable_interrupts,@function
enable_interrupts:
	xori	%r4, %r4, %r4;
	mfmsr   %r4;                             \
        ori     %r4, %r4, MSR_EE;                \
        mtmsr   %r4;
        blr

	.size enable_interrupts, . - enable_interrupts
