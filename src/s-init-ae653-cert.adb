------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                             S Y S T E M . I N I T                        --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--          Copyright (C) 2003-2010, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is the Level A cert version for VxWorks 6, VxWorks 653, and MILS

--  This file should be kept synchronized with 2sinit.ads, 2sinit.adb,
--  s-init-xi-sparc.adb, and init.c. All these files implement the required
--  functionality for different targets.

with Interfaces.C;
with System.OS_Versions;

package body System.Init is

   use Interfaces.C, System.OS_Versions;

   Stack_Limit_Hook : System.Address;
   pragma Export (C, Stack_Limit_Hook, "__gnat_set_stack_limit_hook");
   --  Used when stack limit checking is the stack overflow checking method

   ------------------------
   -- Signal Definitions --
   ------------------------

   NSIG : constant := 32;
   --  Number of signals on the target OS

   type Signal is new int range 0 .. Interfaces.C."-" (NSIG, 1);

   SIGILL  : constant :=  4; --  illegal instruction (not reset)
   SIGFPE  : constant :=  8; --  floating point exception
   SIGBUS  : constant := 10; --  bus error
   SIGSEGV : constant := 11; --  segmentation violation

   type sigset_t is mod 2 ** (if OS = VxWorks_Cert
                              then Long_Long_Integer'Size
                              else Long_Integer'Size);
   SIG_SETMASK : constant := 3;
   SA_ONSTACK  : constant := 16#0004#;

   type struct_sigaction is record
      sa_handler : System.Address;
      sa_mask    : sigset_t;
      sa_flags   : int;
   end record;
   pragma Convention (C, struct_sigaction);
   type struct_sigaction_ptr is access all struct_sigaction;

   function sigdelset (set : access sigset_t; sig : Signal) return int;
   pragma Import (C, sigdelset, "sigdelset");

   function sigemptyset (set : access sigset_t) return int;
   pragma Import (C, sigemptyset, "sigemptyset");

   function sigaction
     (sig  : Signal;
      act  : struct_sigaction_ptr;
      oact : struct_sigaction_ptr) return int;
   pragma Import (C, sigaction, "sigaction");

   type sigset_t_ptr is access all sigset_t;

   function pthread_sigmask
     (how  : int;
      set  : sigset_t_ptr;
      oset : sigset_t_ptr) return int;
   pragma Import (C, pthread_sigmask, "sigprocmask");

   -----------------------------
   -- Binder Generated Values --
   -----------------------------

   Gl_Leap_Seconds_Support : Integer := 0;
   pragma Export (C, Gl_Leap_Seconds_Support, "__gl_leap_seconds_support");

   Gl_Main_Priority : Integer := -1;
   pragma Export (C, Gl_Main_Priority, "__gl_main_priority");

   Gl_Main_CPU : Integer := -1;
   pragma Export (C, Gl_Main_CPU, "__gl_main_cpu");

   Gl_Time_Slice_Val : Integer := -1;
   pragma Export (C, Gl_Time_Slice_Val, "__gl_time_slice_val");

   Gl_Wc_Encoding : Character := 'n';
   pragma Export (C, Gl_Wc_Encoding, "__gl_wc_encoding");

   Gl_Locking_Policy : Character := ' ';
   pragma Export (C, Gl_Locking_Policy, "__gl_locking_policy");

   Gl_Queuing_Policy : Character := ' ';
   pragma Export (C, Gl_Queuing_Policy, "__gl_queuing_policy");

   Gl_Task_Dispatching_Policy : Character := ' ';
   pragma Export (C, Gl_Task_Dispatching_Policy,
                     "__gl_task_dispatching_policy");

   Gl_Priority_Specific_Dispatching : Address := Null_Address;
   pragma Export (C, Gl_Priority_Specific_Dispatching,
                     "__gl_priority_specific_dispatching");

   Gl_Num_Specific_Dispatching : Integer := 0;
   pragma Export (C, Gl_Num_Specific_Dispatching,
                  "__gl_num_specific_dispatching");

   Gl_Restrictions : Address := Null_Address;
   pragma Export (C, Gl_Restrictions, "__gl_restrictions");

   Gl_Interrupt_States : Address := Null_Address;
   pragma Export (C, Gl_Interrupt_States, "__gl_interrupt_states");

   Gl_Num_Interrupt_States : Integer := 0;
   pragma Export (C, Gl_Num_Interrupt_States, "__gl_num_interrupt_states");

   Gl_Unreserve_All_Interrupts : Integer := 0;
   pragma Export (C, Gl_Unreserve_All_Interrupts,
                  "__gl_unreserve_all_interrupts");

   Gl_Exception_Tracebacks : Integer := 0;
   pragma Export (C, Gl_Exception_Tracebacks, "__gl_exception_tracebacks");

   Gl_Zero_Cost_Exceptions : Integer := 0;
   pragma Export (C, Gl_Zero_Cost_Exceptions, "__gl_zero_cost_exceptions");

   Gl_Detect_Blocking : Integer := 0;
   pragma Export (C, Gl_Detect_Blocking, "__gl_detect_blocking");

   Gl_Default_Stack_Size : Integer := 0;
   pragma Export (C, Gl_Default_Stack_Size, "__gl_default_stack_size");

   Handler_Installed : Integer := 0;
   pragma Export (C, Handler_Installed, "__gnat_handler_installed");
   --  Indication of whether synchronous signal handlers have already been
   --  installed by a previous call to Install_Handler.

   ----------------------
   -- Local Procedures --
   ----------------------

   procedure Clear_Exception_Count;
   pragma Import (C, Clear_Exception_Count, "__gnat_clear_exception_count");
   --  Clear count of nested hardware exceptions when handling signal, as
   --  required for vThreads.

   procedure GNAT_Error_Handler (Sig : Signal);
   pragma No_Return (GNAT_Error_Handler);
   --  Common procedure that is executed when a SIGFPE, SIGILL, SIGSEGV, or
   --  SIGBUS is captured.

   ------------------------
   -- GNAT_Error_Handler --
   ------------------------

   procedure GNAT_Error_Handler (Sig : Signal) is
      Mask   : aliased sigset_t;
      Result : int;
      pragma Unreferenced (Result);

   begin
      --  VxWorks will always mask out the signal during the signal handler and
      --  will reenable it on a longjmp. GNAT does not generate a longjmp to
      --  return from a signal handler so the signal will still be masked
      --  unless we unmask it.

      Result := pthread_sigmask (SIG_SETMASK, null, Mask'Unchecked_Access);
      Result := sigdelset (Mask'Unchecked_Access, Sig);
      Result := pthread_sigmask (SIG_SETMASK, Mask'Unchecked_Access, null);
      Clear_Exception_Count;

      --  The VxWorks MILS VxWorks 5 Guest OS does not support stack probing
      --  via guard pages, so its use of signals differs from the other VxWorks
      --  variants. Warnings are off because of constant condition values.

      pragma Warnings (Off);
      case Sig is
         when SIGFPE =>
            raise Constraint_Error with "SIGFPE";

         when SIGILL =>
            if OS = VxWorks_MILS then
               raise Storage_Error with "possible stack overflow, SIGILL";
            elsif OS = VxWorks_653 then
               raise Constraint_Error with "Floating point overflow or SIGILL";
            else
               raise Constraint_Error with "SIGILL";
            end if;

         when SIGSEGV =>
            if OS = VxWorks_Cert then
               raise Storage_Error with "possible stack overflow, SIGSEGV";
            else
               raise Storage_Error with "SIGSEGV";
            end if;

         when SIGBUS =>
            if OS = VxWorks_MILS then
               raise Program_Error with "SIGBUS";
            else
               raise Storage_Error with "possible stack overflow, SIGBUS";
            end if;

         when others =>
            raise Program_Error with "unexpected signal";
      end case;
      pragma Warnings (On);
   end GNAT_Error_Handler;

   ---------------------
   -- Install_Handler --
   ---------------------

   procedure Install_Handler is
      Mask          : aliased sigset_t;
      Signal_Action : aliased struct_sigaction;

      Result : Interfaces.C.int;
      pragma Unreferenced (Result);

   begin
      --  Set up signal handler to map synchronous signals to appropriate
      --  exceptions. Make sure that the handler isn't interrupted by
      --  another signal that might cause a scheduling event!

      Signal_Action.sa_handler := GNAT_Error_Handler'Address;
      Signal_Action.sa_flags := SA_ONSTACK;
      Result := sigemptyset (Mask'Unchecked_Access);
      Signal_Action.sa_mask := Mask;

      Result := sigaction
        (Signal (SIGFPE), Signal_Action'Unchecked_Access, null);

      Result := sigaction
        (Signal (SIGILL), Signal_Action'Unchecked_Access, null);

      Result := sigaction
        (Signal (SIGSEGV), Signal_Action'Unchecked_Access, null);

      Result := sigaction
        (Signal (SIGBUS), Signal_Action'Unchecked_Access, null);

      Handler_Installed := 1;
   end Install_Handler;

end System.Init;
