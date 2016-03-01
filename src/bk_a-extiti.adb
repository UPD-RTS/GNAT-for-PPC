------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--             A D A . E X E C U T I O N _ T I M E . T I M E R S            --
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

with System.BB.Time;
--  Used for Clock
--           Time_Span

with System.BB.Protection;
--  Used for Enter_Kernel
--           Leave_Kernel

with System.BB.Peripherals;
pragma Elaborate (System.BB.Peripherals);
--  Used for Timer_Interval
--           Cancel_And_Set_Alarm
--           Set_Alarm

with Ada.Unchecked_Conversion;

package body Ada.Execution_Time.Timers is

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
   --  Split_Time (a record with two unsigned 40/24 bits fields).

   function To_Time_Span is new Ada.Unchecked_Conversion
     (Ada.Real_Time.Time_Span, System.BB.Time.Time_Span);
   --  Function to change the view between standard and internal Time_Span
   --  types

   function To_Time is new Ada.Unchecked_Conversion
     (Ada.Real_Time.Time_Span, System.BB.Time.Time);
   --  Function to change the view from Time_Span (signed 63-bit) to
   --  Time (unsigned 64-bit).

   function To_Time is new Ada.Unchecked_Conversion
     (Ada.Execution_Time.CPU_Time, System.BB.Time.Time);
   --  Function to change the view from CPU_Time (unsigned 64-bit) to
   --  Time (unsigned 64-bit).

   function To_Time_Span is new Ada.Unchecked_Conversion
     (System.BB.Time.Time, Ada.Real_Time.Time_Span);
   --  Function to change the view from Time (unsigned 64-bit) to
   --  Time_Span (signed 63-bit).

   ------------------
   --  Set_Handler --
   ------------------

   procedure Set_Handler (TM      : in out Timer;
                          In_Time : Ada.Real_Time.Time_Span;
                          Handler : Timer_Handler) is
      Now             : System.BB.Time.Time;
      Is_First_Alarm  : Boolean := False;
      Handler_Exec    : constant System.BB.Threads.Exec_Handler
                         := Execute_Handler'Access;
      Quote           : System.BB.Time.Time;

      use type Ada.Real_Time.Time_Span;
      use type System.BB.Time.Time;
      use type System.BB.Peripherals.Timer_Interval;
      use type System.BB.Threads.Thread_Id;

   begin
      --  The access to the queues must be protected
      System.BB.Protection.Enter_Kernel;

      Now := System.BB.Time.Clock;

      --  If it is the first time that the timer has set, we have to give it an
      --  identifier
      if TM.TM_Id = 0 then

         TM.TM_Id := System.BB.Threads.Get_Timer_Id;
         --  The timer id must be in ATCB too.
         TM.Thread.TM_Integer := TM.TM_Id;

         --  Exec_Handler is the universal handler that allow us to execute the
         --  timer handlers
         TM.Thread.Handler := Handler_Exec;
      end if;

      --  The time data are assigned to Timer
      if In_Time > Ada.Real_Time.Time_Span_Zero then

         Quote := To_Time (In_Time);
         Array_Handlers (TM.Thread.TM_Integer) := Handler;

         if Handler /=  null then

            --  if the time interval is positive and the handler not null, we
            --  have to insert the alarm in the queue.
            TM.Is_Set := True;
            TM.Thread.Time_Remaining := Quote;
            if TM.Thread = System.BB.Threads.Queues.Running_Thread then

               --  The difference between the init of the execution and the set
               --  of the handler must be stored
               TM.Thread.Time_Diff :=
                 Now - TM.Thread.Time_Init_Execution;
               --  Insert an alarm if it is needed
               System.BB.Threads.Queues.Insert_TM_Alarm
                 (Now + To_Time_Span (In_Time), Is_First_Alarm);
               Insert_Alarm_If_Needed (Is_First_Alarm, To_Time (In_Time));
            end if;
         else

            TM.Is_Set := False;
            TM.Thread.Time_Remaining := System.BB.Time.Time'Last;
            if TM.Thread = System.BB.Threads.Queues.Running_Thread then
               System.BB.Threads.Queues.TM_Alarm := System.BB.Time.Time'Last;
            end if;
         end if;

         System.BB.Threads.Queues.Update_Alarms;
      else

         if Handler /= null then

            --  if the handler is not null and the interval is non-positive, we
            --  execute it.
            Handler.all (TM);
         end if;

         TM.Is_Set := False;
         TM.Thread.Time_Remaining := System.BB.Time.Time'Last;

         if TM.Thread = System.BB.Threads.Queues.Running_Thread then
            System.BB.Threads.Queues.TM_Alarm := System.BB.Time.Time'Last;
         end if;
      end if;

      --  Timer data is introduced in the array to its later use
      Array_Timers (TM.Thread.TM_Integer).Thread := TM.Thread;
      Array_Timers (TM.Thread.TM_Integer).Is_Set := TM.Is_Set;
      Array_Timers (TM.Thread.TM_Integer).TM_Id  := TM.TM_Id;
      System.BB.Protection.Leave_Kernel;

   end Set_Handler;

   ------------------
   --  Set_Handler --
   ------------------

   procedure Set_Handler (TM      : in out Timer;
                          At_Time : CPU_Time;
                          Handler : Timer_Handler) is
      Now             : System.BB.Time.Time;
      Is_First_Alarm  : Boolean := False;
      Handler_Exec    : constant System.BB.Threads.Exec_Handler
                          := Execute_Handler'Access;

      use type System.BB.Time.Time;
      use type System.BB.Peripherals.Timer_Interval;
      use type System.BB.Threads.Thread_Id;

   begin
      --  The access to the queues must be protected
      System.BB.Protection.Enter_Kernel;
      Now := System.BB.Time.Clock;

      --  If it is the first time that the timer has set, we have to give it an
      --  identifier
      if TM.TM_Id = 0 then
         TM.TM_Id := System.BB.Threads.Get_Timer_Id;
         TM.Thread.TM_Integer := TM.TM_Id;
         TM.Thread.Handler := Handler_Exec;
      end if;

      if To_Time (At_Time) > Now then
         if Handler /= null then
            Array_Handlers (TM.Thread.TM_Integer) := Handler;
            TM.Is_Set := True;

            --  The elapsed must be taken into account in alarm time
            --  calculations
            TM.Thread.Time_Remaining :=
              System.BB.Time.Time (At_Time) - TM.Thread.Execution_Time;
            if TM.Thread = System.BB.Threads.Queues.Running_Thread then
               --  The difference between the init of the execution and the
               --  set of the handler must be stored
               TM.Thread.Time_Diff :=
                 Now - TM.Thread.Time_Init_Execution;
               --  Insert an alarm if it is needed
               System.BB.Threads.Queues.Insert_TM_Alarm
                 (System.BB.Time.Time (At_Time), Is_First_Alarm);
               Insert_Alarm_If_Needed (Is_First_Alarm,
                                       System.BB.Time.Time (At_Time) - Now);
            end if;
         else
            --  Update alarm queue data.
            TM.Is_Set := False;
            TM.Thread.Time_Remaining := System.BB.Time.Time'Last;
            if TM.Thread = System.BB.Threads.Queues.Running_Thread then
               System.BB.Threads.Queues.TM_Alarm := System.BB.Time.Time'Last;
            end if;
         end if;
         System.BB.Threads.Queues.Update_Alarms;
      else

         if Handler /= null then
            Handler.all (TM);
         end if;
         TM.Is_Set := False;
         TM.Thread.Time_Remaining := System.BB.Time.Time'Last;
         if TM.Thread = System.BB.Threads.Queues.Running_Thread then
            System.BB.Threads.Queues.TM_Alarm := System.BB.Time.Time'Last;
         end if;
      end if;

      --  Timer data is introduced in the array to its later use
      Array_Timers (TM.Thread.TM_Integer).Thread := TM.Thread;
      Array_Timers (TM.Thread.TM_Integer).Is_Set := TM.Is_Set;
      Array_Timers (TM.Thread.TM_Integer).TM_Id  := TM.TM_Id;

      System.BB.Protection.Leave_Kernel;
   end Set_Handler;

   ----------------------
   --  Current_Handler --
   ----------------------

   function Current_Handler (TM : Timer) return Timer_Handler is
   begin
      return Array_Handlers (TM.Thread.TM_Integer);
   end Current_Handler;

   ---------------------
   --  Cancel_Handler --
   ---------------------

   procedure Cancel_Handler (TM        : in out Timer;
                             Cancelled : out Boolean) is
   begin
      TM.Thread.Time_Remaining := System.BB.Time.Time'Last;
      if TM.Is_Set then
         Cancelled  := True;
         TM.Is_Set  := False;
      else
         Cancelled  := False;
      end if;
      if TM.Thread.Is_Timer_Alarm then
         System.BB.Threads.Queues.TM_Alarm := System.BB.Time.Time'Last;
         System.BB.Threads.Queues.Update_Alarms;
      end if;
   end Cancel_Handler;

   ----------------------
   --  Time_Remaining  --
   ----------------------

   function Time_Remaining (TM : Timer) return Ada.Real_Time.Time_Span is
      use type System.BB.Threads.Thread_Id;
      use type System.BB.Time.Time;
   begin
      if TM.Is_Set then
         if TM.Thread = System.BB.Threads.Queues.Running_Thread then
            return To_Time_Span
              (TM.Thread.Time_Remaining -
                 (System.BB.Time.Clock - TM.Thread.Time_Init_Execution));
         else
            return To_Time_Span (TM.Thread.Time_Remaining);
         end if;
      else
         return Ada.Real_Time.Time_Span_Zero;
      end if;
   end Time_Remaining;

   -----------------------
   --  Execute_Handler  --
   -----------------------

   procedure Execute_Handler (Id : Integer) is
   begin
      Array_Handlers (Id).all (Array_Timers (Id));
   exception
      when others => null;
   end Execute_Handler;

   ------------------------------
   --  Insert_Alarm_If_Needed  --
   ------------------------------

   procedure Insert_Alarm_If_Needed (Is_First_Alarm : Boolean;
                                     Time : System.BB.Time.Time) is
      Time_Difference : Split_Time;
      use type System.BB.Peripherals.Timer_Interval;
   begin
      --  if the alarm time is closer than a clock period then we need to
      --  program the timer.

      if Is_First_Alarm then
--           My_IO2.Put_Line ("304");
         Time_Difference := To_Split_Time
           (Time);

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

end Ada.Execution_Time.Timers;
