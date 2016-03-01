------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . T H R E A D S . Q U E U E S            --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2004 The European Space Agency            --
--                     Copyright (C) 2003-2010, AdaCore                     --
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

with System.BB.Time;

package System.BB.Threads.Queues is
   pragma Preelaborate;

   ----------------
   -- Ready list --
   ----------------

   procedure Insert (Thread : Thread_Id);
   --  pragma Inline (Insert);
   --  Insert the thread into the ready queue. The thread is always
   --  inserted at the tail of its active priority because these are
   --  the semantics of FIFO_Within_Priorities dispatching policy when
   --  a task becomes ready to execute.

   procedure Extract (Thread : Thread_Id);
   --  pragma Inline (Extract);
   --  Remove the thread from the ready queue

   procedure Change_Priority
     (Thread   : Thread_Id;
      Priority : System.Any_Priority);
   pragma Inline (Change_Priority);
   --  Move the thread to a new priority within the ready queue

   procedure Yield (Thread : Thread_Id);
   pragma Export (C, Yield, "system_bb_yield");
   --  Move the thread to the tail of its current priority

   Running_Thread : Thread_Id := Null_Thread_Id;
   pragma Volatile (Running_Thread);
   pragma Export (Asm, Running_Thread, "running_thread");
   --  Identifier of the thread that is currently executing in the
   --  CPU. This shared variable is used by the debugger to know which is
   --  the currently running thread. This variable is exported to be
   --  visible in the assembly code to allow its value to be used when
   --  necessary (by the low-level routines).

   First_Thread : Thread_Id := Null_Thread_Id;
   pragma Volatile (First_Thread);
   pragma Export (Asm, First_Thread, "first_thread");
   --  Pointer to the first thread of the priority queue. This is the thread
   --  that will be next to execute in the CPU (if not already executing).
   --  This variable is exported to be visible in the assembly code to allow
   --  its value to be used when necessary (by the low-level routines).

   ----------------
   -- Alarm list --
   ----------------

   procedure Insert_Alarm
     (T        : System.BB.Time.Time;
      Thread   : Thread_Id;
      Is_First : out Boolean);
   --  pragma Inline (Insert_Alarm);
   --  This function inserts the Thread inside the alarm queue ordered by
   --  Time. If the alarm is the next to be served then the functions
   --  returns true in the Is_First argument, and false if not.

   function Extract_First_Alarm return Thread_Id;
   pragma Inline (Extract_First_Alarm);
   --  Extract the first element in the alarm queue and return its
   --  identifier.

   function Get_Next_Alarm_Time return System.BB.Time.Time;
   pragma Inline (Get_Next_Alarm_Time);
   --  Return the time when the next alarm should be set. This function
   --  does not modify the queue.

   Nr_Context_Switches : Integer   := 0;
   function Get_Context_Switches return Integer;

   -------------------------
   --  Temporal Services  --
   -------------------------

   TM_Alarm : System.BB.Time.Time := System.BB.Time.Time'Last;
   --  Time of the alarm of the timer associated to the running task. If
   --  this one have not any timer, this time will be Time'Last.
   GB_Alarm : System.BB.Time.Time := System.BB.Time.Time'Last;
   --  Time of the alarm of the budget associated to the group of the running
   --  task. If this one does not belong to any group, it will be Time'Last.

   Is_Delay    : Boolean := False;
   --  Indicates if the first alarm to expire belongs to a delay until
   --  instruction.

   Is_TE_Alarm : Boolean := False;
   --  Indicates if the first alarm to expire belongs to Timing_Event queue
   TE_Handler  : Exec_Handler :=  null;
   --  Handler of the Timing_Event expired

   function Get_Next_Delay_Time return System.BB.Time.Time;
   pragma Inline (Get_Next_Delay_Time);
   --  Return the time when the next delay alarm should be set. This function
   --  does not modify the queue.

   procedure Get_TE_Id (Id : out Integer);
   pragma Inline (Get_TE_Id);
   --  Return the Id of the Timing_Event referred to the Timer expired and
   --  extract it of the queue.

   procedure Insert_TM_Alarm (At_Time  : System.BB.Time.Time;
                              Is_First : out Boolean);
   pragma Inline (Insert_TM_Alarm);
   --  This procedure inserts the Timer alarm in software mechanism suitable
   --  for it

   procedure Insert_GB_Alarm (At_Time  : System.BB.Time.Time;
                              Is_First : out Boolean);
   pragma Inline (Insert_GB_Alarm);
   --  This procedure inserts the Group-Budget alarm in software mechanism
   --  suitable for it

   procedure Insert_TE_Alarm (At_Time  : System.BB.Time.Time;
                              TE_Alarm : TE_Alarm_Queue_Id);
   pragma Inline (Insert_TE_Alarm);
   --  This procedure inserts the Timing_Event alarm inside the queue
   --  ordered by Time.

   procedure Update_Alarms;
   pragma Inline (Update_Alarms);
   --  Tests if the first alarm to expire was due to the Timing_Events,
   --  delays or execution-time timers.

   procedure Extract_TE_Alarm (TE_Alarm : TE_Alarm_Queue_Id);
   pragma Inline (Extract_TE_Alarm);
   --  Extract the alarm set by TE_Alarm, because this Timing_Event is
   --  going to be replenished

   procedure Extract_First_TE_Alarm;
   pragma Inline (Extract_First_TE_Alarm);

end System.BB.Threads.Queues;
