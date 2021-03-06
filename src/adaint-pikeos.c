/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                         A D A I N T -  P I K E O S                       *
 *                                                                          *
 *               Copyright (C) 2009, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *
 * Boston, MA 02110-1301, USA.                                              *
 *                                                                          *
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/
/* Simple helper to create a new thread.  */
#include <p4.h>

P4_e_t
__gnat_p4_thread_create (P4_thr_t num, P4_prio_t prio, void *code,
			 void *arg, void *stack_base,
			 unsigned long stack_size)
{
  struct P4_thread_create_str tc;
  P4_regs_t context;
  P4_e_t res;

#if 0
  vm_cprintf ("thread_create: num=%u, prio=%u, code=%p, stack=%p, size=%lu\n",
	      num, prio, code, stack_base, stack_size);
#endif

  /* Initialize context.  */
  res = p4_thread_arg
    (&context, code, P4_STACK (stack_base, stack_size),
     P4_THREAD_ARG_FPU | P4_THREAD_ARG_VEC | P4_THREAD_ARG_DEBUG,
     1, &arg);
  if (res != P4_E_OK)
    {
      vm_cprintf ("p4_thread_arg: failure (%u)\n", res);
      return res;
    }

  tc.name = NULL;
  tc.context = &context;
  tc.prio = prio;
  tc.tp_id = P4_TIMEPART_INHERIT;
  tc.shortexh = P4_UID_INHERIT;
  tc.fullexh = P4_UID_INHERIT;
  tc.ipc_mask = P4_UID_ALL;
  tc.ev_mask = P4_UID_ALL;

  res = p4_thread_create (num, &tc, P4_THREAD_CREATE_READY);
  if (res != P4_E_OK)
    vm_cprintf ("p4_thread_create failed: %u\n", res);
  return res;
}
