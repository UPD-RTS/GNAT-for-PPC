------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                       S Y S T E M . B B . T H R E A D S                  --
--                                                                          --
--                                  S p e c                                 --
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

--  Package that implements basic tasking functionalities

pragma Restrictions (No_Elaboration_Code);

with System;
with System.Parameters;
with System.BB.CPU_Primitives;
with System.BB.Time;
with System.BB.Interrupts;

package System.BB.Threads is
   pragma Preelaborate;

   --------------------------
   -- Basic thread support --
   --------------------------

   type Thread_Descriptor;
   --  This type contains the information about a thread

   type Thread_Id is access all Thread_Descriptor;
   --  Type used as thread identifier

   Null_Thread_Id : constant Thread_Id := null;
   pragma Export (C, Null_Thread_Id, "system_bb_null_thread_id");
   --  Identifier used to define an invalid value for a thread identifier

   type Thread_States is (Runnable, Suspended, Delayed);
   --  These are the three possible states for a thread under the Ravenscar
   --  profile restrictions: Runnable (not blocked, and it may also be
   --  executing), Suspended (waiting on an entry call), and Delayed (waiting
   --  on a delay until statement).

   type Exec_Handler is access procedure (I : Integer);

   type Thread_Descriptor is record
      Context : aliased System.BB.CPU_Primitives.Context_Buffer;
      --  Location where the hardware registers (stack pointer, program
      --  counter, ...) are stored. This field supports context switches among
      --  threads.

      ATCB : System.Address;
      --  Address of the Ada Task Control Block corresponding to the Ada task
      --  that executes on this thread.

      Base_Priority : System.Any_Priority;
      --  Base priority of the thread

      Active_Priority : System.Any_Priority;
      pragma Volatile (Active_Priority);
      --  Active priority that differs from the base priority due to dynamic
      --  priority changes required by the Ceiling Priority Protocol. This
      --  field is marked as Volatile for a fast implementation of
      --  Get_Priority.

      Top_Of_Stack : System.Address;
      --  Address of the top of the stack that is used by the thread

      Bottom_Of_Stack : System.Address;
      --  Address of the bottom of the stack that is used by the thread

      Next : Thread_Id;
      --  Points to the ready thread that is in the next position for
      --  execution.

      Alarm_Time : System.BB.Time.Time;
      --  Time (absolute) when the alarm for this thread expires

      Next_Alarm : Thread_Id;
      --  Next thread in the alarm queue. The queue is ordered by expiration
      --  times. The first place is occupied by the thread which must be
      --  first awaken.

      State : Thread_States;
      --  Encodes some basic information about the state of a thread

      Wakeup_Signaled : Boolean;
      --  Variable which reflects whether another thread has performed a
      --  Wakeup operation on the thread.

      Time_Init_Execution : System.BB.Time.Time;
      --  Time when task has received the CPU
      Execution_Time      : System.BB.Time.Time;
      --  Execution Time of the task
      Time_Remaining      : System.BB.Time.Time;
      --  Time remaining of timer associated to the task
      Is_Timer_Alarm      : Boolean;
      --  Flag that indicates if it has been put an alarm associated to the
      --  timer associated to the task
      Is_GB_Alarm         : Boolean;
      --  Flag that indicates if it has been put an alarm associated to the
      --  timer associated to the group of the task
      TM_Integer          : Integer;
      --  Index to the array with the handlers associated to the timers
      Time_Diff           : System.BB.Time.Time;
      --  Difference of time between time of the init of the execution of the
      --  task and the asignation of the timer to the task
      Time_Diff_GB        : System.BB.Time.Time;
      --  Difference of time between time of the init of the execution of the
      --  task and the asignation of the timer to the group of the task
      Handler             : Exec_Handler;
      --  Handler associated to the Execution_Time Timer of the task
      GB_Id               : Integer;
      --  Identifier associated to the Group_Budget of the task
      GB_Index            : Integer;
      --  Place of the array associated to the Group_Budget where the task
      --  data is stored
      Handler_GB          : Exec_Handler;
      --  Handler associated to the Group_Budget of the task

      --  AB: LIMITED PREEMPTION
      NPR_Length          : Integer;
      CS_Length           : Integer;
      WCET_Budget         : Integer;

      Remaining_WCET      : Integer;
      --  Extraction_Pending  : Boolean;
   end record;

   for Thread_Descriptor use
      record
         Context at 0 range 0 ..
           (System.BB.CPU_Primitives.Context_Buffer_Size - 1);
      end record;
   --  It is important that the Context field is placed at the beginning of
   --  the record, because this assumption is using for implementing context
   --  switching.

   procedure Initialize
     (Environment_Thread : Thread_Id;
      Main_Priority      : System.Any_Priority);
   --  Procedure to initialize the board and the data structures related to
   --  the low level tasking system. This procedure must be called before any
   --  other tasking operation.

   procedure Thread_Create
     (Id            : Thread_Id;
      Code          : System.Address;
      Arg           : System.Address;
      Priority      : System.Any_Priority;
      Stack_Address : System.Address;
      Stack_Size    : System.Parameters.Size_Type;
      Task_Args     : System.Address); --  AB
   pragma Export (C, Thread_Create, "system_bb_thread_create");
   --  Create a new thread
   --
   --  The new thread executes the code at address Code and using Args
   --  as argument. Priority is the base priority of the new
   --  thread. The new thread is provided with a stack of size
   --  Stack_Size that has been preallocated at Stack_Address.
   --
   --  A procedure to destroy threads is not available because that is not
   --  allowed by the Ravenscar profile.

   function Thread_Self return Thread_Id;
   pragma Inline (Thread_Self);
   pragma Export (C, Thread_Self, "system_bb_thread_self");
   --  Return the thread identifier of the calling thread

   ----------------
   -- Scheduling --
   ----------------

   procedure Set_Priority (Priority : System.Any_Priority);
   pragma Inline (Set_Priority);
   pragma Export (C, Set_Priority, "system_bb_set_priority");
   --  Set the active priority of the executing thread to the given value

   function Get_Priority  (Id : Thread_Id) return System.Any_Priority;
   pragma Inline (Get_Priority);
   pragma Export (C, Get_Priority, "system_bb_get_priority");
   --  Get the current active priority of any thread

   procedure Sleep;
   pragma Export (C, Sleep, "system_bb_sleep");
   --  The calling thread is unconditionally suspended

   procedure Wakeup (Id : Thread_Id);
   pragma Export (C, Wakeup, "system_bb_wakeup");
   --  Thread Id becomes ready (the thread must be previously suspended)

   ----------
   -- ATCB --
   ----------

   procedure Set_ATCB (ATCB : System.Address);
   pragma Inline (Set_ATCB);
   --  This procedure sets the ATCB passed as argument for the
   --  currently running thread.

   function Get_ATCB return System.Address;
   pragma Inline (Get_ATCB);
   --  Returns the ATCB of the currently executing thread

   ---------------------------------
   --  Execution_Time functions  ---
   ---------------------------------
   type Handler is access protected procedure (I : Integer);

   Global_TM_Pointer : Integer := 1;

   Global_GB_Pointer : Integer := 1;
   Budget_Array : array (1 .. 255) of System.BB.Time.Time_Span;

   function Get_Timer_Id  return Integer;
   function Get_GB_Id return Integer;

   ---------------------------
   --  Timing_Events types  --
   ---------------------------

   Global_TE_Pointer : Integer := 1;
   function Get_TE_Id return Integer;

   type TE_Alarm_Queue;
   --  This type contains the time of alarms relative of Timing Events and
   --  the next alarm.

   type TE_Alarm_Queue_Id is access all TE_Alarm_Queue;
   --  Type used as TE_Alarm_Queue identifier

   Null_TE_Alarm_Queue_Id : constant TE_Alarm_Queue_Id := null;

   type TE_Alarm_Queue is record
      Previous_Alarm : TE_Alarm_Queue_Id   := Null_TE_Alarm_Queue_Id;
      Alarm_Time     : System.BB.Time.Time := System.BB.Time.Time'Last;
      TE_Id          : Integer             := 0;
      Next_Alarm     : TE_Alarm_Queue_Id   := Null_TE_Alarm_Queue_Id;
   end record;

   ------------------------------
   --  Execution_Time_Support  --
   ------------------------------

   procedure Execution_Time_Adjust (Now : System.BB.Time.Time);
   procedure Timer_Execution_Time_Adjust (Now : System.BB.Time.Time);
   procedure GB_Execution_Time_Adjust (Now : System.BB.Time.Time);

   procedure Execute_Handler (T : System.BB.Threads.Thread_Id);
   procedure Execute_GB_Handler (T : System.BB.Threads.Thread_Id);

   procedure Insert_Alarm_If_Needed (Is_First_Alarm : Boolean;
                                     Time : System.BB.Time.Time);

end System.BB.Threads;
