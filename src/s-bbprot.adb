------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   S Y S T E M . B B . P R O T E C T I O N                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2011, AdaCore                     --
--           Copyright (C) 2011 Universidad Politecnica de Madrid           --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
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
-- The porting of GNARL to bare board  targets was initially  developed  by --
-- the Real-Time Systems Group at the Technical University of Madrid.       --
--                                                                          --
------------------------------------------------------------------------------

pragma Restrictions (No_Elaboration_Code);

with System.BB.Peripherals;
--  Used for Power_Down_Mode

with System.BB.CPU_Primitives;
with System.BB.Threads;
with System.BB.Interrupts;

with System.BB.Threads.Queues;
pragma Elaborate (System.BB.Threads.Queues);
--  Used for Next_Running
--           Running_Thread

with System.BB.Time;
--  Used for Clock

--  with System.BB.Time.Execution_Time_Support;
--  Used for Execution_Time_Adjust
--           Timer_Execution_Time_Adjust
--           GB_Execution_Time_Adjust
with System.BB.Serial_Output;
use System.BB.Serial_Output;

with Ada.Unchecked_Conversion;

package body System.BB.Protection is

   --  function To_Integer is new
   --    Ada.Unchecked_Conversion
   --        (System.BB.Threads.Thread_Id, System.Address);

   -----------------------
   -- Local subprograms --
   -----------------------

   function Context_Switch_Needed return Boolean;
   pragma Inline (Context_Switch_Needed);
   pragma Export (Asm, Context_Switch_Needed, "context_switch_needed");
   --  This function returns True if the task (or interrupt handler) that is
   --  executing is no longer the highest priority one. This function can also
   --  be called by the interrupt handlers' epilogue.

   ------------------
   -- Enter_Kernel --
   ------------------

   procedure Enter_Kernel is
   begin
      --  Interrupts are disabled to avoid concurrency problems when modifying
      --  kernel data. This way, external interrupts cannot be raised.

      CPU_Primitives.Disable_Interrupts;
   end Enter_Kernel;

   ---------------------------
   -- Context_Switch_Needed --
   ---------------------------

   function Context_Switch_Needed return Boolean is
      use type System.BB.Threads.Thread_Id;
      B   : Boolean;
      Now : System.BB.Time.Time;

   begin
      --  Put_Line ("CS (?) " & To_Integer (Threads.Queues.Running_Thread)'Img
      --                  & To_Integer (Threads.Queues.First_Thread)'Img);
      --  A context switch is needed when there is a higher priority task ready
      --  to execute. It means that First_Thread is not null and it is not
      --  equal to the task currently executing (Running_Thread).

      pragma Assert (Threads.Queues.First_Thread   /= Threads.Null_Thread_Id
                       and then
                     Threads.Queues.Running_Thread /= Threads.Null_Thread_Id);

      B := Threads.Queues.First_Thread /= Threads.Queues.Running_Thread;

      --  if a context switch is needed we have to update execution-time data
      if B then
         Threads.Queues.Nr_Context_Switches :=
           Threads.Queues.Nr_Context_Switches + 1; --  AB
         Put_Line ("# CONTEXT SWITCH: " &
                     Integer'Image (Threads.Queues.Nr_Context_Switches));
         Now := System.BB.Time.Clock;
         System.BB.Threads.
           Timer_Execution_Time_Adjust (Now);
         System.BB.Threads.
           GB_Execution_Time_Adjust (Now);
         System.BB.Threads.
           Execution_Time_Adjust (Now);
      end if;

      return B;
   end Context_Switch_Needed;

   ------------------
   -- Leave_Kernel --
   ------------------

   procedure Leave_Kernel is
      use type System.BB.Threads.Thread_Id;
      use type System.BB.Threads.Thread_States;

   begin
      --  Interrupts are always disabled when entering here

      --  If there is nothing to execute (no tasks or interrupt handlers) then
      --  we just wait until there is something to do. It means that we need to
      --  wait until there is any thread ready to execute. Interrupts are
      --  handled just after enabling interrupts.

      if Threads.Queues.First_Thread = Threads.Null_Thread_Id then
         --  There is no task ready to execute so we need to wait until there
         --  is one, unless we are currently handling an interrupt.

         --  In the meantime, we put the task temporarily in the ready queue
         --  so interrupt handling is performed normally. Note that the task
         --  is inserted in the queue but its state is not Runnable.

         Threads.Queues.Insert (Threads.Queues.Running_Thread);

         --  Wait until a task has been made ready to execute (including the
         --  one that has been temporarily added to the ready queue).

         while Threads.Queues.First_Thread = Threads.Queues.Running_Thread
           and then Threads.Queues.Running_Thread.State /= Threads.Runnable
           and then Threads.Queues.Running_Thread.Next = Threads.Null_Thread_Id
         loop
            --  Allow all external interrupts for a while

            CPU_Primitives.Enable_Interrupts (0);
            --  Peripherals.Power_Down_Mode;
            CPU_Primitives.Disable_Interrupts;
         end loop;

         --  A task has been made ready to execute. We remove the one that was
         --  temporarily inserted in the ready queue, if needed.

         if Threads.Queues.Running_Thread.State /= Threads.Runnable then
            Threads.Queues.Extract (Threads.Queues.Running_Thread);
         end if;
      end if;

      --  We need to check whether a context switch is needed

      if Context_Switch_Needed then
         --  Perform a context switch because the currently executing thread
         --  is blocked or it is no longer the one with the highest priority.
         --  Put_Line ("oldTh: " &
         --     To_Integer (Threads.Queues.Running_Thread)'Img);
         CPU_Primitives.Context_Switch;
         --  Put_Line ("newTh: " &
         --     To_Integer (Threads.Queues.Running_Thread)'Img);
      end if;

      --  Now we need to set the hardware interrupt masking level equal to the
      --  software priority of the task that is executing.

      if Threads.Queues.Running_Thread.Active_Priority in
        Interrupt_Priority'Range
      then
         --  We need to mask some interrupts because we are executing at a
         --  hardware interrupt priority.

         System.BB.CPU_Primitives.Enable_Interrupts
           (Threads.Queues.Running_Thread.Active_Priority -
            System.Interrupt_Priority'First + 1);

      else
         --  We are neither within an interrupt handler nor within task that
         --  has a priority in the range of Interrupt_Priority, so that no
         --  interrupt should be masked.

         System.BB.CPU_Primitives.Enable_Interrupts (0);
      end if;
   end Leave_Kernel;

end System.BB.Protection;
