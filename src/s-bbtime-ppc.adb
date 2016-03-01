------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                         S Y S T E M . B B . T I M E                      --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2004 The European Space Agency            --
--                     Copyright (C) 2003-2011, AdaCore                     --
--           Copyright (C) 2011 Universidad Politecnica de Madrid           --
--          Copyright (C) 2012-2013 Università degli Studi di Padova        --
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

--  This is the PPC version of this package.

--  pragma Restrictions (No_Elaboration_Code);

with Ada.Unchecked_Conversion;

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

--  Used for easy debugging (can be safely removed)
with System.BB.Serial_Output;
use System.BB.Serial_Output;

with System.Machine_Code;

package body System.BB.Time is

   use type Peripherals.Timer_Interval;

   --  As opposed to the SPARC version we use a single 64bit timer
   --  that is responsible for all the alarms (delays, timing events)

   -----------------------
   -- Local definitions --
   -----------------------

   --  type Clock_Periods is mod 2 ** 40;
   --  for Clock_Periods'Size use 40;
   --  Values of this type represent number of times that the clock finishes
   --  its countdown.

   --  MSP_Clock : constant Clock_Periods := 0;
   --  This is not a constant, this is for xtratum port

   --  MSP_Clock object holds the Most Significant Part of the Clock.
   --  The Lowest Significant Part of the Clock is held in the hardware
   --  clock register. The range of the Lowest Significant Part of the
   --  Clock is System.BB.Parameters.Timer_Interval'Range.
   --
   --  pragma Atomic (MSP_Clock) is needed because MSP_Clock is updated
   --  by Clock_Handler.
   --  GNAT can not afford pragma Atomic for 64 bits objects with SPARC
   --  architecture.
   --  With the following pragma Volatile, SPARC/GNAT generates 64 bits
   --  instructions to read and write MSP_Clock. i.e., atomic access
   --  is guaranteed in an uniprocessor SPARC system.
   --  pragma Volatile (MSP_Clock);

   Pending_Alarm : Boolean := False;
   pragma Atomic (Pending_Alarm);
   --  Alarm Timer is used to set alarms between two periodic interrupts.
   --  But it is possible than due to calculations delay an alarm could
   --  expire after the clock interrupt. If so the Clock Handler does
   --  nothing with alarms, so that this flag shows if an alarm is pending.
   --  Pending_Partition_Alarm : Boolean := False;
   --  pragma Atomic (Pending_Partition_Alarm);

   --  type Timer_Interval is range -2**63 .. 2**63-1;
   --  This type represents any interval that we can measure within a
   --  Clock_Interrupt_Period.

   function To_Timer_Interval is new Ada.Unchecked_Conversion
     (Time, Peripherals.Timer_Interval);

   function To_Time is new Ada.Unchecked_Conversion
     (Peripherals.Timer_Interval, Time);

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Alarm_Handler (Interrupt : Interrupts.Interrupt_ID);
   --  Handler for the alarm interrupt
   --  procedure Clock_Handler (Interrupt : Interrupts.Interrupt_ID);
   --  Handler for the clock interrupt

   -------------------
   -- Alarm_Handler --
   -------------------

   --  function To_Integer is new
   --    Ada.Unchecked_Conversion (Threads.Thread_Id, System.Address);
   Last_Read_TB : Peripherals.Timer_Interval := 0;

   procedure Alarm_Handler (Interrupt : Interrupts.Interrupt_ID) is
      Now             : Time;
      Time_Difference : Peripherals.Timer_Interval;
      Wakeup_Thread   : Threads.Thread_Id;
      Id_Aux          : Integer;
      use type Threads.Thread_States;
      use type Threads.Thread_Id;

   begin
      Last_Read_TB := (Peripherals.Read_Clock - Last_Read_TB) / 74000;
      Put_Line ("ALARM: " &
                  Peripherals.Timer_Interval'Image (Last_Read_TB));
      --  Make sure we are handling the right interrupt and there is an
      --  alarm pending.
      Threads.Queues.Update_Alarms;

      pragma Assert
        (Pending_Alarm and Interrupt = Peripherals.Timer_1);

      Peripherals.Clear_Alarm_Interrupt;

      Now := Clock;

      --  The access to the queues must be protected

      Protection.Enter_Kernel;
      if Threads.Queues.Get_Next_Alarm_Time < Now then
         if Threads.Queues.Is_TE_Alarm then
            --  Put_Line ("Alarm::Timing event");
            --  The expiring alarm belongs to the Timing_Event queue

            Threads.Queues.Get_TE_Id (Id_Aux);
            Threads.Queues.Extract_First_TE_Alarm;
            Threads.Queues.TE_Handler.all (Id_Aux);

         else
            --  Put ("Alarm::Delay ");
            if Threads.Queues.Is_Delay then
               --  The expiring alarm belongs to delays queue
               --  Extract all the threads whose delay has expired

               while Threads.Queues.Get_Next_Delay_Time <= Now loop
                  --  Put ("*(");
                  --  Extract the task(s) that was waiting in the alarm
                  --  queue and insert it in the ready queue.

                  Wakeup_Thread := Threads.Queues.Extract_First_Alarm;
                  --  Put (To_Integer (Wakeup_Thread)'Img & ")");
                  --  We awake tasks that are delay statement
                  pragma Assert (Wakeup_Thread.State = Threads.Delayed);
                  Wakeup_Thread.State := Threads.Runnable;
                  Threads.Queues.Insert (Wakeup_Thread);
               end loop;
            --  else
               --  Put_Line (" ------------  !!  ----------- ");
            end if;
            --  Put_Line (" .");
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
      elsif Threads.Queues.Get_Next_Alarm_Time /=
        System.BB.Time.Time'Last then

         Time_Difference := To_Timer_Interval
           (Threads.Queues.Get_Next_Alarm_Time - Now);

         --  if next alarm time is closer than a clock period then we need to
         --  program the timer. Otherwise we just need to signal that the timer
         --  has not been programed.

         Peripherals.Set_Alarm (Time_Difference);
      end if;
      --  We have finished the modifications to the queues
      Protection.Leave_Kernel;
   end Alarm_Handler;

   -------------------------------
   --  Partition_Alarm_Handler  --
   -------------------------------

--   procedure Partition_Alarm_Handler (Interrupt : Interrupts.Interrupt_ID) is
--      Now             : Time;
--      Time_Difference : Timer_Interval;

--   begin
      --  Make sure we are handling the right interrupt and there is an
      --  alarm pending.
--     Threads.Queues.Update_Alarms;

--      pragma Assert
--        (Pending_Partition_Alarm and Interrupt =
--           Peripherals.Xtratum_Timer_2);
--
--      Peripherals.Clear_Alarm_Interrupt;

--      Now := Partition_Clock;

      --  The access to the queues must be protected

--      Protection.Enter_Kernel;
--      if Threads.Queues.Get_Next_Partition_Alarm_Time < Now then
--         if Threads.Queues.Running_Thread.Is_Timer_Alarm then

            --  The expiring alarm belongs to the timers queue

--            System.BB.Threads.Execute_Handler
--              (Threads.Queues.Running_Thread);
--         else

            --  The expiring alarm must be GB_Alarm
--            pragma Assert (Threads.Queues.Running_Thread.Is_GB_Alarm);

--            System.BB.Threads.Execute_GB_Handler
--              (Threads.Queues.Running_Thread);

--         end if;
--      end if;

      --  Alarm queues have been modified
--      Threads.Queues.Update_Alarms;

--      if Threads.Queues.Get_Next_Partition_Alarm_Time <= Now then
         --  In the time elapsed in the handle of the alarm, new alarms may
         --  have expired. To avoid the lose of an alarm, we must test all
         --  the alarms would have expired until this instant and not have
         --  been handled.
--         Inmediate_Partition_Alarm (Now);
--      elsif Threads.Queues.Get_Next_Partition_Alarm_Time /=
--        System.BB.Time.Time'Last then

--        Time_Difference := To_Time_Interval
--           (Threads.Queues.Get_Next_Partition_Alarm_Time);

         --  if next alarm time is closer than a clock period then we need to
         --  program the timer. Otherwise we just need to signal that the timer
         --  has not been programed.
--        Peripherals.Set_Partition_Alarm (Time_Difference);
--     end if;

      --  We have finished the modifications to the queues
--      Protection.Leave_Kernel;
--   end Partition_Alarm_Handler;

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
      use Peripherals;
   begin
      return To_Time (System.BB.Peripherals.Read_Clock);
   end Clock;

   -----------------
   -- Delay_Until --
   -----------------
   procedure Delay_Until (T : Time) is
      Now               : Time;
      Time_Difference   : Peripherals.Timer_Interval;
      Self              : Threads.Thread_Id;
      Inserted_As_First : Boolean;

   begin
      Now := Clock;
      --  Put_Line ("  " & To_Timer_Interval (T)'Img & " - " &
      --          To_Timer_Interval (Now)'Img);
      Protection.Enter_Kernel;

      Self := Threads.Thread_Self;

      --  Test if the alarm time is in the future
      if T > Now then
         --  Extract the thread from the ready queue. When a thread wants
         --  to wait for an alarm it becomes blocked.
         --  Put_Line ("Delay_Unitl->Delayed");
         Self.State := Threads.Delayed;

         Threads.Queues.Extract (Self);

         --  Insert Thread_Id in the alarm queue (ordered by time) and if
         --  it was inserted at head then check if Alarm Time is closer
         --  than the next clock interrupt.
         Threads.Queues.Insert_Alarm (T, Self, Inserted_As_First);

         if Inserted_As_First and then
           T <= System.BB.Threads.Queues.Get_Next_Alarm_Time then

               Time_Difference := To_Timer_Interval (T - Now);

               --  Check whether the alarm time is within a clock period

               if Pending_Alarm then
                  Peripherals.Cancel_And_Set_Alarm (Time_Difference);
               else
                  Peripherals.Set_Alarm (Time_Difference);
                  Pending_Alarm := True;
               end if;

               System.BB.Threads.Queues.Update_Alarms;

         end if;
      else
         --  Put_Line ("Delay_Unitl->Yield");
         --  If alarm time is not in the future, the thread must
         --  yield the CPU
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

      --  Interrupts.Attach_Handler
      --    (Clock_Handler'Access, Peripherals.Xtratum_Interrupt);

      --  Install timer handler
      --  Interrupt for Alarm_Handler of System is
      --  Peripherals.Xtratum_System_Timer.
      Interrupts.Attach_Handler
        (Alarm_Handler'Access, Peripherals.Timer_1);
      --  Interrupts.Attach_Handler
      --  (Partition_Alarm_Handler'Access, Peripherals.Xtratum_Timer_2);
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

   -----------------------------------
   --  Get_Partition_Pending_Alarm  --
   -----------------------------------

--   function Get_Pending_Partition_Alarm return Boolean is
--   begin
      --  This function enable us to get the value of Pending_Partition_Alarm
      --  variable.

--      return Pending_Partition_Alarm;
--   end Get_Pending_Partition_Alarm;

   -------------------------------
   --  Turn_True_Pending_Alarm  --
   -------------------------------

   procedure Turn_True_Pending_Alarm is
   begin
      --  This function enable us to turn Pending_Alarm variable to true

      Pending_Alarm := True;
   end Turn_True_Pending_Alarm;

   -----------------------------------------
   --  Turn_True_Pending_Partition_Alarm  --
   -----------------------------------------

--   procedure Turn_True_Pending_Partition_Alarm is
--   begin
      --  This procedure enable us to turn Pending_Partition_Alarm to true

--      Pending_Partition_Alarm := True;
--   end Turn_True_Pending_Partition_Alarm;

   -----------------------
   --  Inmediate_Alarm  --
   -----------------------
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
            if Threads.Queues.Is_Delay then
               Wakeup_Thread := Threads.Queues.Extract_First_Alarm;
               --  We awake tasks that are delay statement
               pragma Assert (Wakeup_Thread.State = Threads.Delayed);
               Wakeup_Thread.State := Threads.Runnable;
               Threads.Queues.Insert (Wakeup_Thread);
            end if;
         end if;
         System.BB.Threads.Queues.Update_Alarms;
         Now := Clock;
      end loop;
   end Inmediate_Alarm;

   ---------------------------------
   --  Inmediate_Partition_Alarm  --
   ---------------------------------
--  procedure Inmediate_Partition_Alarm (Now : in out System.BB.Time.Time) is

--   begin

      --  All alarms expired that have not been handled properly
      --  will be handled inside this loop.
--      while Threads.Queues.Get_Next_Partition_Alarm_Time <= Now loop
--         if Threads.Queues.Running_Thread.Is_Timer_Alarm then

--            System.BB.Threads.Execute_Handler
--              (Threads.Queues.Running_Thread);
--         else
--            if Threads.Queues.Running_Thread.Is_GB_Alarm then

--               System.BB.Threads.Execute_GB_Handler
--                 (Threads.Queues.Running_Thread);
--            end if;
--         end if;
--         System.BB.Threads.Queues.Update_Alarms;
--         Now := Partition_Clock;
--      end loop;
--   end Inmediate_Partition_Alarm;

end System.BB.Time;
