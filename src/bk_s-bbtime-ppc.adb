------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                         S Y S T E M . B B . T I M E                      --
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

with Ada.Unchecked_Conversion;

--  with System.BB.Peripherals;
--  pragma Elaborate (System.BB.Peripherals);
--  Used for Read_Clock
--           To_Vector

--  with System.BB.Peripherals.Registers;
--  pragma Elaborate (System.BB.Peripherals.Registers);
--  Used for Timer_1_Control

with System.BB.Interrupts;
--  pragma Elaborate (System.BB.Interrupts);
--  Used for Attach_Handler

with System.BB.Threads;
--  pragma Elaborate (System.BB.Threads);
--  Used for Thread_Id
--           Thread_Self

with System.BB.Protection;
--  Used for Enter_Kernel
--           Leave_Kernel

with System.BB.Threads.Queues;
--  Used for Insert
--           Extract
--           Yield
--           Insert_Alarm
--           Extract_First_Alarm
--           Get_Next_Alarm_Time

--  with System.BB.Time.Execution_Time_Support;
--  Used for Execution_Time_Adjusts
--           Timer_Execution_Time_Adjusts
--           Execute_Handler

with System.Machine_Code;
--  UNIPD: Used for Clock

package body System.BB.Time is

   use type Peripherals.Timer_Interval;

   --  We use two timers:
   --     A Periodic Timer for the clock
   --     An Alarm Timer for delays

   -----------------------
   -- Local definitions --
   -----------------------

   type Clock_Periods is mod 2 ** Peripherals.Timer_Interval'Size;
   for Clock_Periods'Size use Peripherals.Timer_Interval'Size;
   --  Values of this type represent number of times that the clock finishes
   --  its countdown.

   TBU_Clock : Clock_Periods := 0;
   --  TBU_Clock object holds the Most Significant Part of the Clock.
   --  It corresponds to TBU register in PowerPC implementations.
   --  The Lowest Significant Part of the Clock is held in the hardware
   --  clock register. Again, it corrsponds to the TBL register in
   --  PowerPC implementations.
   --  The range of the Lowest Significant Part of the
   --  Clock is System.BB.Parameters.Timer_Interval'Range.
   --
   --  UNIPD: The following does not appply to PowerPC
   --  pragma Atomic (TBU_Clock) is needed because TBU_Clock is updated
   --  by Clock_Handler.
   --  GNAT can not afford pragma Atomic for 64 bits objects with SPARC
   --  architecture.
   --  With the following pragma Volatile, SPARC/GNAT generates 64 bits
   --  instructions to read and write TBU_Clock. i.e., atomic access
   --  is guaranteed in an uniprocessor SPARC system.
   pragma Volatile (TBU_Clock);

   Pending_Alarm : Boolean := False;
   pragma Atomic (Pending_Alarm);
   --  Alarm Timer is used to set alarms between two periodic interrupts. But
   --  it is possible than due to calculations delay an alarm could expire
   --  after the clock interrupt. If so the Clock Handler does nothing with
   --  alarms, so that this flag shows if an alarm is pending.

   subtype TB is 2 ** 32;

   type Split_Time is
      record
         TBU : TB;
         TBL : TB;
      end record;

   for Split_Time use
      record
         TBU at 0 range 0 .. 31; --  32;
         TBL at 4 range 0 .. 31; --  32;
      end record;

   for Split_Time'Size use 64;
   --  The type Split_Time represents a 64-bit time value, but it gives
   --  access to the two 32 bits parts (TBU and TBL) separately.

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Alarm_Handler (Interrupt : Interrupts.Interrupt_ID);
   --  Handler for the alarm interrupt

   procedure Clock_Handler (Interrupt : Interrupts.Interrupt_ID);
   --  Handler for the clock interrupt

   function To_Time is new Ada.Unchecked_Conversion (Split_Time, Time);
   --  Function to change the view from Split_Time (a record with two
   --  unsigned 40/24 bits fields) to Time (unsigned 64-bit).

   function To_Split_Time is new Ada.Unchecked_Conversion (Time, Split_Time);
   --  Function to change the view from Time (unsigned 64-bit) to
   --  Split_Time (a record with two unsigned 40/24 bits fields).

   -------------------
   -- Alarm_Handler --
   -------------------

   procedure Alarm_Handler (Interrupt : Interrupts.Interrupt_ID) is
      Now             : Time;
      Time_Difference : Split_Time;
      Wakeup_Thread   : Threads.Thread_Id;
      Id_Aux          : Integer;
      use type Threads.Thread_States;
      use type Threads.Thread_Id;

   begin
      --  Make sure we are handling the right interrupt and there is an
      --  alarm pending.
      Threads.Queues.Update_Alarms;

      pragma Assert
        (Pending_Alarm and Interrupt = Peripherals.Timer_1);

      Peripherals.Clear_Alarm_Interrupt;

      Now := Clock;

      --  The access to the queues must be protected

      Protection.Enter_Kernel;
      if Threads.Queues.Get_Next_Alarm_Time < Clock then
         if Threads.Queues.Is_TE_Alarm then
            --  The expiring alarm belongs to the Timing_Event queue

            Threads.Queues.Get_TE_Id (Id_Aux);
            Threads.Queues.Extract_First_TE_Alarm;
            Threads.Queues.TE_Handler.all (Id_Aux);

         else
            if Threads.Queues.Running_Thread.Is_Timer_Alarm then

               --  The expiring alarm belongs to the timers queue

               System.BB.Threads.Execute_Handler
                 (Threads.Queues.Running_Thread);
            else
               if Threads.Queues.Running_Thread.Is_GB_Alarm then

                  --  The expiring alarm belongs to the budgets queue

                  System.BB.Threads.Execute_GB_Handler
                    (Threads.Queues.Running_Thread);
               else
                  if Threads.Queues.Is_Delay then
                     --  The expiring alarm belongs to delays queue
                     --  Extract all the threads whose delay has expired

                     while Threads.Queues.Get_Next_Delay_Time <= Now loop

                        --  Extract the task(s) that was waiting in the alarm
                        --  queue and insert it in the ready queue.

                        Wakeup_Thread := Threads.Queues.Extract_First_Alarm;

                        --  We awake tasks that are delay statement
                        pragma Assert (Wakeup_Thread.State = Threads.Delayed);
                        Wakeup_Thread.State := Threads.Runnable;
                        Threads.Queues.Insert (Wakeup_Thread);
                     end loop;
                  end if;
               end if;
            end if;
         end if;
      end if;

      --  Alarm queues have been modified
      Threads.Queues.Update_Alarms;

      if Threads.Queues.Get_Next_Alarm_Time <= Now then
         --  In the time elapsed in the handle of the alarm, new alarms may
         --  have expired. To avoid the lose of an alarm, we must test all
         --  the alarms would have expired until this instant and not have
         --  been handled.
         Inmediate_Alarm (Now);
      else
         Time_Difference :=
           To_Split_Time (Threads.Queues.Get_Next_Alarm_Time - Now);

         --  if next alarm time is closer than a clock period then we need to
         --  program the timer. Otherwise we just need to signal that the timer
         --  has not been programed.

         if Time_Difference.TBU = 0 then
            --  The timer that we are setting is always in the future because
            --  we have previously checked that the the value of the alarm is
            --  strictly greater than the selected clock value.

            pragma Assert (Time_Difference.TBL > 0);
            Pending_Alarm := True;

            Peripherals.Set_Alarm (Time_Difference.TBL);
         else
            Pending_Alarm := False;
         end if;
      end if;

      --  We have finished the modifications to the queues

      Protection.Leave_Kernel;
   end Alarm_Handler;

   --------------------------------
   -- Number_Of_Ticks_Per_Second --
   --------------------------------
   function Number_Of_Ticks_Per_Second return Time is
   begin
      return Ticks_Per_Second;
   end Number_Of_Ticks_Per_Second;

   -----------
   -- Clock --
   -----------

   function Clock return Time is
      Result : Split_Time;
      tempTBU : TB;
      use System.Machine_Code;
      use Peripherals;
   begin
      --  A clock interrupt has occurred after reading Result.TBU. Hence,
      --  Result must be adjusted.
      Asm ("1:"          & ASCII.LF & ASCII.HT &   --  Branch label
           "mftbu %0"    & ASCII.LF & ASCII.HT &   --  Get TBU
           "mftb  %1"    & ASCII.LF & ASCII.HT &   --  Get TBU
           "mftbu %2"    & ASCII.LF & ASCII.HT &   --  Get TBL
           "cmpw  %2,%0" & ASCII.LF & ASCII.HT &   --  Repeat if TBU changed
           "bne   1b",
           Outputs => (TB'Asm_Output ("=r", Result.TBU), --  %0=TBU
                       TB'Asm_Output ("=r", Result.TBL), --  %1=TBL
                       TB'Asm_Output ("=r", tempTBU)),   --  %2=tmpTBU
           Volatile => True
      );
      return To_Time (Result);
   end Clock;

   -------------------
   -- Clock_Handler --
   -------------------

   procedure Clock_Handler (Interrupt : Interrupts.Interrupt_ID) is
      Time_Difference : Split_Time;

   begin
      --  Check that we are in the right handler

      pragma Assert (Interrupt = Peripherals.Timer_2);

      Peripherals.Clear_Clock_Interrupt;

      --  The access to the queues must be protected

      Protection.Enter_Kernel;

      --  The clock timer has finished counting 2**24 hardware clock ticks

      TBU_Clock := TBU_Clock + 1;

      if not Pending_Alarm then

         Time_Difference := To_Split_Time
           (Threads.Queues.Get_Next_Alarm_Time -
              To_Time (Split_Time'(TBU_Clock, Peripherals.Read_Clock)));

         --  If next alarm time is closer than a clock period then we need
         --  to program the alarm.

         if Time_Difference.TBU = 0 then

            --  Program the alarm at least one tick later

            Peripherals.Set_Alarm
              (Peripherals.Timer_Interval'Max (Time_Difference.TBL, 1));

            Pending_Alarm := True;

            --  JAPP
            System.BB.Threads.Queues.Update_Alarms;
            --  JAPP
         end if;
      end if;

      Protection.Leave_Kernel;

   end Clock_Handler;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
      Now               : Time;
      Time_Difference   : Split_Time;
      Self              : Threads.Thread_Id;
      Inserted_As_First : Boolean;

   begin
      Now := Clock;

      Protection.Enter_Kernel;

      Self := Threads.Thread_Self;

      --  Test if the alarm time is in the future

      if T > Now then

         --  Extract the thread from the ready queue. When a thread wants
         --  to wait for an alarm it becomes blocked.

         Self.State := Threads.Delayed;

         Threads.Queues.Extract (Self);

         --  Insert Thread_Id in the alarm queue (ordered by time) and if
         --  it was inserted at head then check if Alarm Time is closer
         --  than the next clock interrupt.
         Threads.Queues.Insert_Alarm (T, Self, Inserted_As_First);

         if Inserted_As_First then

            --  JAPP

            if T <= System.BB.Threads.Queues.Get_Next_Alarm_Time then

               --  JAPP

               Time_Difference := To_Split_Time (T - Now);

               --  Check whether the alarm time is within a clock period

               if Time_Difference.TBU = 0 then

                  --  If so, set a new alarm and cancel the previous one if
                  --  needed. The timer that we are setting is always in the
                  --  future because we have previously checked that the the
                  --  value of the alarm is strictly greater than the selected
                  --  clock value.

                  pragma Assert (Time_Difference.TBL > 0);

                  if Pending_Alarm then
                     Peripherals.Cancel_And_Set_Alarm (Time_Difference.TBL);
                  else
                     Peripherals.Set_Alarm (Time_Difference.TBL);
                     Pending_Alarm := True;
                  end if;

                  --  JAPP
                  System.BB.Threads.Queues.Update_Alarms;
                  --  JAPP

               end if;
            end if;
         end if;

      else
         --  If alarm time is not in the future, the thread must yield the CPU

         Threads.Queues.Yield (Self);
      end if;

      Protection.Leave_Kernel;
   end Delay_Until;

   -----------------------
   -- Initialize_Timers --
   -----------------------

   procedure Initialize_Timers is
   begin
      --  Install clock handler

      Interrupts.Attach_Handler
        (Clock_Handler'Access, Peripherals.Timer_2);

      --  Install timer handler

      Interrupts.Attach_Handler
        (Alarm_Handler'Access, Peripherals.Timer_1);
   end Initialize_Timers;

   -----------
   --  "+"  --
   -----------

   function "+" (Left : Time; Right : Time_Span) return Time is
   begin
      return Left + Time (Right);
   end "+";

   -------------------------
   --  Get_Pending_Alarm  --
   -------------------------

   function Get_Pending_Alarm return Boolean is
   begin
      --  This function enable us to get the value of Pending_Alarm variable

      return Pending_Alarm;
   end Get_Pending_Alarm;

   -------------------------------
   --  Turn_True_Pending_Alarm  --
   -------------------------------

   procedure Turn_True_Pending_Alarm is
   begin
      --  This function enable us to turn Pending_Alarm variable to true

      Pending_Alarm := True;
   end Turn_True_Pending_Alarm;

   procedure Inmediate_Alarm (Now : in out System.BB.Time.Time) is
      Id_Aux        : Integer;
      Wakeup_Thread : Threads.Thread_Id;

      use type Threads.Thread_States;
   begin

      --  All alarms expired that have not been handled properly
      --  will be handled inside this loop.
      while Threads.Queues.Get_Next_Alarm_Time <= Now loop
         if Threads.Queues.Is_TE_Alarm then
            --  The timer expired belongs to the Timing_Event queue
            Threads.Queues.Get_TE_Id (Id_Aux);
            Threads.Queues.Extract_First_TE_Alarm;
            Threads.Queues.TE_Handler.all (Id_Aux);
         else
            if Threads.Queues.Running_Thread.Is_Timer_Alarm then

               System.BB.Threads.Execute_Handler
                 (Threads.Queues.Running_Thread);
            else
               if Threads.Queues.Running_Thread.Is_GB_Alarm then

                  System.BB.Threads.Execute_GB_Handler
                    (Threads.Queues.Running_Thread);
               else
                  if Threads.Queues.Is_Delay then
                     Wakeup_Thread := Threads.Queues.Extract_First_Alarm;
                     --  We awake tasks that are delay statement
                     pragma Assert (Wakeup_Thread.State = Threads.Delayed);
                     Wakeup_Thread.State := Threads.Runnable;
                     Threads.Queues.Insert (Wakeup_Thread);
                  end if;
               end if;
            end if;
         end if;
         System.BB.Threads.Queues.Update_Alarms;
         Now := Clock;
      end loop;
   end Inmediate_Alarm;

end System.BB.Time;
