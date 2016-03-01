------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                        S Y S T E M . B B . T I M E .                     --
--                 E X E C U T I O N _ T I M E _ S U P P O R T              --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--           Copyright (C) 2007 Universidad Politecnica de Madrid           --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
-- The porting of GNARL to bare board  targets was initially  developed  by --
-- the Real-Time Systems Group at the Technical University of Madrid.       --
--                                                                          --
------------------------------------------------------------------------------

with System.BB.Protection;
--  Used for Leave_Kernel

--  with System.BB.Threads;
--  Used for Thread_Id

with System.BB.Threads.Queues;
--  Used for Running_Thread
--           First_Thread

with System.BB.Time;
--  Used for Time
--           Time_Span
--           Clock
use type System.BB.Time.Time;

with Ada.Task_Identification;
--  Used for Task_Id

with System.BB.Peripherals;
pragma Elaborate (System.BB.Peripherals);
--  Used for Timer_Interval
--           Cancel_And_Set_Alarm
--           Set_Alarm

with Unchecked_Conversion;

--  with Ada.Real_Time;

package body System.BB.Time.Execution_Time_Support is

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

   function To_Split_Time is new Unchecked_Conversion
     (System.BB.Time.Time, Split_Time);
   --  Function to change the view from Time (unsigned 64-bit) to
   --  Split_Time (a record with two unsigned 40/24 fields).

   --  function To_Time_Span is new Unchecked_Conversion
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

      use type System.BB.Threads.Thread_Id;

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

      use type System.BB.Threads.Thread_Id;
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

end System.BB.Time.Execution_Time_Support;
