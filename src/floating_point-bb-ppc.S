/****************************************************************************
 *                                                                          *
 *                GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  *
 *                                                                          *
 *                         F L O A T I N G _ P O I N T                      *
 *                                                                          *
 *                               Assembly File                              *
 *                                                                          *
 *      Copyright (C) 1999-2002 Universidad Politecnica de Madrid           *
 *             Copyright (C) 2003-2005 The European Space Agency            *
 *                     Copyright (C) 2003-2005 AdaCore                      *
 *           Copyright (C) 2012-2013 Università degli Studi di Padova       *
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

/* This file is a PPC specific implementation */

#include "ppc.h"

	.text
	.align 4
	.global initialize_floating_point

/* Procedure that installs the trap handler in charge of executing floating
   point context switches. 
   For the time being we just need to enable the FP bit in MSR
 */

initialize_floating_point:

	/* Set the FP bit in MSR */
	mfmsr   %r4;                             \
        ori     %r4, %r4, MSR_FP;                \
        mtmsr   %r4;
	
/* We assume that fp registers are always included in context switches */
fp_context_switch:

	blr				


/* We assume that fp registers are always included in context switches */
fp_context_switch_done:

	blr				
	

/* __gnat_init_float is a routine used for reseting the floating-point
   processor to the default state needed to get correct Ada semantics
  for the target. */

	.global __gnat_init_float
__gnat_init_float:

	blr				
/*
	nop				! Nothing to be done on this target
	retl
	nop
*/
