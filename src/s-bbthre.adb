------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                       S Y S T E M . B B . T H R E A D S                  --
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

with System.Storage_Elements;
with System.BB.Parameters;
with System.BB.Peripherals;
with System.BB.Protection;
with System.BB.Threads.Queues;

with Ada.Unchecked_Conversion;

package body System.BB.Threads is

   use System.BB.CPU_Primitives;
   use System.BB.Time;
   use System.BB.Parameters;

   use type System.Address;
   use type System.Parameters.Size_Type;
   use type System.Storage_Elements.Storage_Offset;

   --------------
   -- Get_ATCB --
   --------------

   function Get_ATCB return System.Address is
   begin
      return System.BB.Threads.Queues.Running_Thread.ATCB;
   end Get_ATCB;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (Id : Thread_Id) return System.Any_Priority is
   begin
      --  This function does not need to be protected by Enter_Kernel and
      --  Leave_Kernel, because the Active_Priority value is only updated
      --  by Set_Priority (atomically). Moreover, Active_Priority is
      --  marked as Volatile.

      return Id.Active_Priority;
   end Get_Priority;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Environment_Thread : Thread_Id;
      Main_Priority      : System.Any_Priority)
   is
   begin
      --  Perform some basic hardware initialization (clock, timer, and
      --  interrupt handlers).

      Peripherals.Initialize_Board;
      Interrupts.Initialize_Interrupts;
      Time.Initialize_Timers;

      --  Initialize internal queues and the environment task

      Protection.Enter_Kernel;

      --  The environment thread executes the main procedure of the program

      --  The active priority is initially equal to the base priority

      Environment_Thread.Base_Priority   := Main_Priority;
      Environment_Thread.Active_Priority := Main_Priority;

      --  The currently executing thread (and the only one) is the
      --  environment thread.

      Queues.Running_Thread := Environment_Thread;
      Queues.First_Thread   := Environment_Thread;

      Environment_Thread.Next := Null_Thread_Id;

      --  Store stack information

      Environment_Thread.Top_Of_Stack := Top_Of_Environment_Stack'Address;

      Environment_Thread.Bottom_Of_Stack :=
        Bottom_Of_Environment_Stack'Address;

      --  The initial state is Runnable

      Environment_Thread.State := Runnable;

      --  No wakeup has been yet signaled

      Environment_Thread.Wakeup_Signaled := False;

      --  Initialize alarm status

      Environment_Thread.Alarm_Time :=
        System.BB.Time.Time'Last;
      Environment_Thread.Next_Alarm := Null_Thread_Id;
      --  Initialize execution-time status

      Environment_Thread.Time_Init_Execution := Time.Time'First;
      Environment_Thread.Execution_Time      := Time.Time'First;
      Environment_Thread.Time_Remaining      := Time.Time'Last;
      Environment_Thread.Is_Timer_Alarm      := False;
      Environment_Thread.Is_GB_Alarm         := False;
      Environment_Thread.TM_Integer          := 0;
      Environment_Thread.Time_Diff           := 0;
      Environment_Thread.Handler             := null;
      Environment_Thread.GB_Id               := 0;
      Environment_Thread.GB_Index            := 0;
      Environment_Thread.Time_Diff_GB        := 0;
      Environment_Thread.Handler_GB          := null;

      --  Enable use of the floating point unit in a multitasking environment

      Initialize_Floating_Point;

      Protection.Leave_Kernel;
   end Initialize;

   --------------
   -- Set_ATCB --
   --------------

   procedure Set_ATCB (ATCB : System.Address) is
   begin
      --  Set_ATCB is only called in the initialization of the task, and
      --  just by the owner of the thread, so there is no need of explicit
      --  kernel protection when calling this function.

      System.BB.Threads.Queues.Running_Thread.ATCB := ATCB;
   end Set_ATCB;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority (Priority  : System.Any_Priority) is
   begin
      Protection.Enter_Kernel;

      --  The Ravenscar profile does not allow dynamic priority changes. Tasks
      --  change their priority only when they inherit the ceiling priority of
      --  a PO (Ceiling Locking policy). Hence, the task must be running when
      --  changing the priority. It is not possible to change the priority of
      --  another thread within the Ravenscar profile, so that is why
      --  Running_Thread is used.

      --  Priority changes are only possible as a result of inheriting the
      --  ceiling priority of a protected object. Hence, it can never be set
      --  a priority which is lower than the base priority of the thread.

      pragma Assert (Priority >= Queues.Running_Thread.Base_Priority);

      Queues.Change_Priority (Queues.Running_Thread, Priority);

      Protection.Leave_Kernel;
   end Set_Priority;

   -----------
   -- Sleep --
   -----------

   procedure Sleep is
      Self_Id : constant Thread_Id := Queues.Running_Thread;
   begin
      Protection.Enter_Kernel;

      if Self_Id.Wakeup_Signaled then

         --  Another thread has already executed a Wakeup on this thread so
         --  that we just consume the token and continue execution.

         Self_Id.Wakeup_Signaled := False;

      else
         --  Update status

         Self_Id.State := Suspended;

         --  Extract from the list of ready threads

         Queues.Extract (Self_Id);

         --  The currently executing thread is now blocked, and it will leave
         --  the CPU when executing the Leave_Kernel procedure.

      end if;

      Protection.Leave_Kernel;

      --  Now the thread has been awaken again and it is executing
   end Sleep;

   -------------------
   -- Thread_Create --
   -------------------

   procedure Thread_Create
     (Id            : Thread_Id;
      Code          : System.Address;
      Arg           : System.Address;
      Priority      : System.Any_Priority;
      Stack_Address : System.Address;
      Stack_Size    : System.Parameters.Size_Type)
   is
   begin

      Protection.Enter_Kernel;

      --  Set the base and active priority

      Id.Base_Priority   := Priority;
      Id.Active_Priority := Priority;

      --  Insert task inside the ready list (as last within its priority)

      Queues.Insert (Id);

      --  Store stack information

      Id.Top_Of_Stack :=
        ((Stack_Address +
          System.Storage_Elements.Storage_Offset (Stack_Size)) /
         Standard'Maximum_Alignment) *
        Standard'Maximum_Alignment;

      Id.Bottom_Of_Stack := Stack_Address;

      --  The initial state is Runnable

      Id.State := Runnable;

      --  No wakeup has been yet signaled

      Id.Wakeup_Signaled := False;

      --  Initialize the saved registers, including the program counter and
      --  stack pointer. The thread will execute the Thread_Caller procedure
      --  and the stack pointer points to the top of the stack assigned to the
      --  thread.

      Initialize_Context (Id.Context'Access, Code, Arg, Id.Top_Of_Stack);

      --  Initialize alarm status

      Id.Alarm_Time := System.BB.Time.Time'Last;
      Id.Next_Alarm := Null_Thread_Id;

      --  Initialize execution-time data status
      Id.Time_Init_Execution := System.BB.Time.Time'First;
      Id.Execution_Time      := System.BB.Time.Time'First;
      Id.Time_Remaining      := System.BB.Time.Time'Last;
      Id.Is_Timer_Alarm      := False;
      Id.Is_GB_Alarm         := False;
      Id.TM_Integer          := 0;
      Id.Time_Diff           := 0;
      Id.Handler             := null;
      Id.GB_Id               := 0;
      Id.GB_Index            := 0;
      Id.Time_Diff_GB        := 0;
      Id.Handler_GB          := null;

      Protection.Leave_Kernel;
   end Thread_Create;

   -----------------
   -- Thread_Self --
   -----------------

   function Thread_Self return Thread_Id is
   begin
      --  Return the thread that is currently executing

      return Queues.Running_Thread;
   end Thread_Self;

   ------------
   -- Wakeup --
   ------------

   procedure Wakeup (Id : Thread_Id) is
   begin
      Protection.Enter_Kernel;

      if Id.State = Suspended then

         --  The thread is already waiting so that we awake it

         --  Update status

         Id.State := Runnable;

         --  Insert the thread at the tail of its active priority so that the
         --  thread will resume execution.

         Queues.Insert (Id);

      else
         --  The thread is not yet waiting so that we just signal that the
         --  Wakeup command has been executed.

         Id.Wakeup_Signaled := True;
      end if;

      Protection.Leave_Kernel;
   end Wakeup;

   --------------------------------
   --  Execution_Time functions  --
   --------------------------------

   function Get_Timer_Id  return Integer is
   begin
      Global_TM_Pointer := Global_TM_Pointer + 1;
      return Global_TM_Pointer - 1;
   end Get_Timer_Id;

   function Get_GB_Id  return Integer is
   begin
      Global_GB_Pointer := Global_GB_Pointer + 1;
      return Global_GB_Pointer - 1;
   end Get_GB_Id;

   ------------------------------
   --  Timing_Event functions  --
   ------------------------------

   function Get_TE_Id  return Integer is
   begin
      Global_TE_Pointer := Global_TE_Pointer + 1;
      return Global_TE_Pointer - 1;
   end Get_TE_Id;

   ------------------------------
   --  Execution_Time_Support  --
   ------------------------------

   type Clock_Periods is mod 2 ** 40;
   for Clock_Periods'Size use 40;
   --  Values of this type represent number of times that the clock finishes
   --  its countdown.

   type Split_Time is
      record
         MSP : Clock_Periods;
         LSP : System.BB.Peripherals.Timer_Interval;
      end record;

   for Split_Time use
      record
         MSP at 0 range 0 .. 39;
         LSP at 5 range 0 .. 23;
      end record;

   for Split_Time'Size use 64;
   --  The type Split_Time represents a 64-bit time value, but it gives
   --  access to the two 40/24 bits parts separately.

   function To_Split_Time is new Ada.Unchecked_Conversion
     (System.BB.Time.Time, Split_Time);
   --  Function to change the view from Time (unsigned 64-bit) to
   --  Split_Time (a record with two unsigned 40/24 fields).

   --  function To_Time_Span is new Ada.Unchecked_Conversion
   --    (Ada.Real_Time.Time_Span, System.BB.Time.Time_Span);
   --  Function to change the view between Time_Span types

   -----------------------------
   --  Execution_Time_Adjust  --
   -----------------------------

   procedure Execution_Time_Adjust (Now : System.BB.Time.Time) is
      --  Update the Execution_Time data

      Elapsed_Time   : System.BB.Time.Time;
      Next           : System.BB.Threads.Thread_Id;
      Running        : System.BB.Threads.Thread_Id;

      --  use type System.BB.Threads.Thread_Id;

   begin
      Running := System.BB.Threads.Queues.Running_Thread;
      Next := System.BB.Threads.Queues.First_Thread;

      if Next.Active_Priority > 0 then

         --  if there are ready tasks in the queue

         if Running.Time_Init_Execution /= System.BB.Time.Time'Last then
            Elapsed_Time            := Now - Running.Time_Init_Execution;
            Running.Execution_Time  :=
              Running.Execution_Time + Elapsed_Time;
         end if;

         Next.Time_Init_Execution := Now;
         Running.Time_Init_Execution := System.BB.Time.Time'Last;

      else

         --  if there is not any ready task in the queue

         if Running.Time_Init_Execution /= System.BB.Time.Time'Last then
               Elapsed_Time := Now - Running.Time_Init_Execution;
            Running.Execution_Time := Running.Execution_Time + Elapsed_Time;
            Running.Time_Init_Execution := System.BB.Time.Time'Last;
         end if;

      end if;
      System.BB.Threads.Queues.Update_Alarms;

   end Execution_Time_Adjust;

   -----------------------------------
   --  Timer_Execution_Time_Adjust  --
   -----------------------------------

   procedure Timer_Execution_Time_Adjust (Now : System.BB.Time.Time) is
      --  Update data relatives of Execution_Time.Timers

      Elapsed_Time    : System.BB.Time.Time;
      Next            : System.BB.Threads.Thread_Id;
      Running         : System.BB.Threads.Thread_Id;
      Is_First_Alarm  : Boolean;

      --  use type System.BB.Threads.Thread_Id;
      use type System.BB.Peripherals.Timer_Interval;

   begin
      Next := System.BB.Threads.Queues.First_Thread;
      Running := System.BB.Threads.Queues.Running_Thread;

      if Next.Active_Priority > 0 then

         --  if there are ready tasks in the queue

         if Running.Time_Init_Execution /= System.BB.Time.Time'Last then
            Elapsed_Time            := Now - Running.Time_Init_Execution;

            if Running.Time_Remaining /= System.BB.Time.Time'Last then
               if Running.Time_Remaining <
                 (Elapsed_Time - Running.Time_Diff) then

                  null;

               else

                  --  Update the value of the time remaining of the timer

                  Running.Time_Remaining :=
                    Running.Time_Remaining - Elapsed_Time
                      + Running.Time_Diff;
                  Running.Time_Diff := 0;

               end if;

            end if;

            Running.Is_Timer_Alarm := False;
            System.BB.Threads.Queues.TM_Alarm := System.BB.Time.Time'Last;

            if Next.Time_Remaining /= System.BB.Time.Time'Last then

               --  Insert alarm if needed
               System.BB.Threads.Queues.Insert_TM_Alarm
                 (Now + Next.Time_Remaining, Is_First_Alarm);
               Insert_Alarm_If_Needed (Is_First_Alarm,
                                       Next.Time_Remaining);

            end if;

         else

            Next.Is_Timer_Alarm := False;
            System.BB.Threads.Queues.TM_Alarm := System.BB.Time.Time'Last;
            if Next.Time_Remaining /= System.BB.Time.Time'Last then

               --  Insert alarm if needed
               System.BB.Threads.Queues.Insert_TM_Alarm
                 (Now + Next.Time_Remaining, Is_First_Alarm);
               Insert_Alarm_If_Needed (Is_First_Alarm,
                                       Next.Time_Remaining);

            end if;

         end if;

      else
         Running.Is_Timer_Alarm := False;
         System.BB.Threads.Queues.TM_Alarm := System.BB.Time.Time'Last;
         --  If there is not any ready task in the queue
         if Running.Time_Remaining /= System.BB.Time.Time'Last then

            Elapsed_Time := Now - Running.Time_Init_Execution;

            if Running.Time_Remaining < (Elapsed_Time - Running.Time_Diff) then

               null;

            else

               --  Update the value of the time remaining of the timer
               Running.Time_Remaining :=
                    Running.Time_Remaining - Elapsed_Time
                      + Running.Time_Diff;
               Running.Time_Diff := 0;

            end if;

         end if;

      end if;

   end Timer_Execution_Time_Adjust;

   --------------------------------
   --  GB_Execution_Time_Adjust  --
   --------------------------------

   procedure GB_Execution_Time_Adjust (Now : System.BB.Time.Time) is
      --  Update data relatives of Execution_Time.Group_Budgets

      Elapsed_Time    : System.BB.Time.Time_Span;
      Next            : System.BB.Threads.Thread_Id;
      Running         : System.BB.Threads.Thread_Id;
      Is_First_Alarm  : Boolean;
      New_Budget      : System.BB.Time.Time_Span;

   begin

      Next := System.BB.Threads.Queues.First_Thread;
      Running := System.BB.Threads.Queues.Running_Thread;

      if Next.Active_Priority > 0 then

         if Running.Time_Init_Execution /= System.BB.Time.Time'Last and
           Running.GB_Id /= 0 then

            --  if the running task belongs to a group

            Running.Is_GB_Alarm := False;
            System.BB.Threads.Queues.GB_Alarm := System.BB.Time.Time'Last;
            if System.BB.Threads.Budget_Array (Running.GB_Id)
              > 0 then
               Elapsed_Time            :=
                 System.BB.Time.Time_Span
                   (Now - Running.Time_Init_Execution
                    - Running.Time_Diff_GB);

               New_Budget :=
                 System.BB.Threads.Budget_Array
                   (Running.GB_Id) - Elapsed_Time;

               if New_Budget < 0 then

                  null;

               else

                  System.BB.Threads.Budget_Array (Running.GB_Id)
                    := New_Budget;

               end if;
            end if;
         end if;

         if Next.GB_Id /= 0 and then
           System.BB.Threads.Budget_Array (Next.GB_Id) > 0 then

            --  insert alarm if needed

            System.BB.Threads.Queues.Insert_GB_Alarm
              (Now + System.BB.Threads.Budget_Array (Next.GB_Id),
               Is_First_Alarm);

            Insert_Alarm_If_Needed
              (Is_First_Alarm,
               System.BB.Time.Time
                 (System.BB.Threads.Budget_Array (Next.GB_Id)));

         end if;

      else

         if Running.GB_Id /= 0
           and Running.Time_Init_Execution /= System.BB.Time.Time'Last then
            Running.Is_GB_Alarm := False;
            System.BB.Threads.Queues.GB_Alarm := System.BB.Time.Time'Last;

            Elapsed_Time := System.BB.Time.Time_Span
              (Now - Running.Time_Init_Execution - Running.Time_Diff_GB);

            if System.BB.Threads.Budget_Array (Running.GB_Id)
              > 0 then

               --  Update the value of the time remaining of the timer
               System.BB.Threads.Budget_Array (Running.GB_Id)  :=
                 (System.BB.Threads.Budget_Array (Running.GB_Id)
                  - Elapsed_Time);
               Running.Time_Diff_GB := 0;

            end if;

         end if;

      end if;

   end GB_Execution_Time_Adjust;

   -----------------------
   --  Execute_Handler  --
   -----------------------

   procedure Execute_Handler (T : System.BB.Threads.Thread_Id) is
      --  Executes the correct handler when a timer expires

   begin
      T.Handler.all (T.TM_Integer);
      T.Time_Remaining := System.BB.Time.Time'Last;
      System.BB.Threads.Queues.TM_Alarm := System.BB.Time.Time'Last;
   end Execute_Handler;

   --------------------------
   --  Execute_GB_Handler  --
   --------------------------

   procedure Execute_GB_Handler (T : System.BB.Threads.Thread_Id) is
      --  Executes the correct handler when a group_budget expires

   begin
      T.Handler_GB.all (T.GB_Id);
      System.BB.Threads.Budget_Array (T.GB_Id) :=
        System.BB.Time.Time_Span_Zero;
      --  To_Time_Span (Ada.Real_Time.Time_Span_Zero);
      System.BB.Threads.Queues.GB_Alarm := System.BB.Time.Time'Last;
   end Execute_GB_Handler;

   ------------------------------
   --  Insert_Alarm_If_Needed  --
   ------------------------------

   procedure Insert_Alarm_If_Needed (Is_First_Alarm : Boolean;
                                     Time : System.BB.Time.Time) is

      Time_Difference : Split_Time;
      use type System.BB.Peripherals.Timer_Interval;

      Now : System.BB.Time.Time;
   begin

      --  if the alarm time is closer than a clock period then we need to
      --  program the timer.

      if Is_First_Alarm then
         --  Time_Difference := To_Split_Time (Time);
         --  JAPP
         Now := System.BB.Time.Clock;
         Time_Difference := To_Split_Time (Time - Now);
         --  JAPP

         --  Check whether the alarm time is within a clock period

         if Time_Difference.MSP = 0 then

            --  If so, set a new alarm and cancel the previous one if
            --  needed. The timer that we are setting is always in the
            --  future because we have previously checked that the the
            --  value of the alarm is strictly greater than the
            --  selected clock value.

            pragma Assert (Time_Difference.LSP > 0);

            if System.BB.Time.Get_Pending_Alarm then
               System.BB.Peripherals.Cancel_And_Set_Alarm
                 (Time_Difference.LSP);
            else
               System.BB.Peripherals.Set_Alarm (Time_Difference.LSP);
               System.BB.Time.Turn_True_Pending_Alarm;
            end if;
         end if;
      end if;
   end Insert_Alarm_If_Needed;

end System.BB.Threads;
