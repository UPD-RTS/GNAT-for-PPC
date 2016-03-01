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
--           Get_First_Thread

with Interfaces;

--  with System.BB.Xtratum;
--  Used for time services of xtratum.

with System.BB.Serial_Output;
use System.BB.Serial_Output;

with System.Machine_Code; use System.Machine_Code;
with System.BB.CPU_Primitives;

package body System.BB.Time is

   use type Peripherals.Timer_Interval;

   --  We use two timers:
   --     A Periodic Timer for the clock
   --     An Alarm Timer for delays

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

   --  function To_Timer_Interval is new Ada.Unchecked_Conversion
   --    (Time, Peripherals.Timer_Interval);

   function To_Time is new Ada.Unchecked_Conversion
     (Peripherals.Timer_Interval, Time);

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Alarm_Handler (Interrupt : Interrupts.Interrupt_ID);
   --  Handler for the alarm interrupt
   --  procedure Clock_Handler (Interrupt : Interrupts.Interrupt_ID);
   --  Handler for the clock interrupt

   procedure Alarm_Handler_FP_Sched;
   procedure Alarm_Handler_Activation_Triggered_LP_Sched;
   procedure Alarm_Handler_FPP_LP_Sched;

   -------------------
   -- Alarm_Handler --
   -------------------

   --  function To_Integer is new
   --    Ada.Unchecked_Conversion (Threads.Thread_Id, System.Address);

   procedure Alarm_Handler (Interrupt : Interrupts.Interrupt_ID) is

--        use type Threads.Thread_States;
--        use type Threads.Thread_Id;
      use Threads.Queues;
--        use Interfaces;
--
--        Next_Candidate_PP : Integer;
--        Aux : Threads.Thread_Id;

   begin
      --  Put_Line ("KERNEL: Alarm Handler");
      --  Make sure we are handling the right interrupt

      pragma Assert (Interrupt = Peripherals.Timer_1);

      Peripherals.Clear_Alarm_Interrupt;

      --  case Alarms_Queue (Current_Alarm).Event_Type is
      --  case NPR_Alarm.Event_Type is
      case Preempt_Policy is

         when Fully_Preemptive =>
            Alarm_Handler_FP_Sched;

         when Fixed_Preemption_Points =>
            Alarm_Handler_FPP_LP_Sched;

         when Activation_Triggered_NPR | Model_NPR =>
            Alarm_Handler_Activation_Triggered_LP_Sched;

      end case;

   end Alarm_Handler;

   ----------------------------
   -- Alarm_Handler_FP_Sched --
   ----------------------------

   procedure Alarm_Handler_FP_Sched is
      use Threads.Queues;
   begin
--        Put_Line ("[SYSTEM.BB.TIME.ALARM_HANDLER_FP] " &
--                    "TASK ACTIVATION EVENT (ALARM " &
--                    Integer'Image (Current_Alarm) & ")");

      Time_From_Hyp_Start :=
        Alarms_Queue (Current_Alarm).Timestamp;
      if Time_From_Hyp_Start = 180 then
         Asm ("nop", Volatile => True);
      end if;

--        Put_Line ("[SYSTEM.BB.TIME.ALARM_HANDLER_FP]  " &
--                    "Current HYP time is " &
--                    Integer'Image (Time_From_Hyp_Start));

      --  The access to the queues must be protected

      Protection.Enter_Kernel;

      Threads.Queues.Update_Ready_Queue
        (Alarms_Queue (Current_Alarm));

      --  Threads.Queues.Print_Masks;

      --  Option A: Uncomment for original behaviour
      --  (next interrupt is found in alarm_queue)
      Threads.Queues.Next_Alarm := Activation;
      Peripherals.Set_Alarm
        (Alarms_Queue (Current_Alarm).Interval *
         Peripherals.Timer_Interval (Peripherals.Clock_Freq_Hz));

      Current_Alarm := Alarms_Queue (Current_Alarm).Next;

      --  We have finished the modifications to the queues
      Protection.Leave_Kernel;

   end Alarm_Handler_FP_Sched;

   -------------------------------------------------
   -- Alarm_Handler_Activation_Triggered_LP_Sched --
   -------------------------------------------------

   procedure Alarm_Handler_Activation_Triggered_LP_Sched is
      pragma Style_Checks (Off);
      use Threads;
      use Threads.Queues;
      use Interfaces;
      Current_TB : Peripherals.Timer_Interval;
      --  Current_TB : Float;
      --  Tmp_Root_Mask : Interfaces.Unsigned_32;
      --  Tmp_Child_Masks : Ready_Child_Bitmasks_Type;
      Bitmask_Index : Integer range 0 .. 7;
      Priority_Level : System.Any_Priority;
      Tmp_Timestamp : Integer;
--        Old_Time_From_Hyp : Integer;
--        Old_Alarm : Alarm;
   begin

      case Next_Alarm is
         when Activation =>

            Protection.Enter_Kernel;

            Current_TB := Peripherals.Read_Clock;
            Last_Read_TB := Current_TB;
            if --  First_Activation and
              Alarms_Queue (Current_Alarm).Timestamp = 0 then
               --  first activation
               --  First_Activation := False;
--                 Last_Read_TB := Current_TB;
               Time_From_Hyp_Start := 0;

            else
               Time_From_Hyp_Start := Alarms_Queue (Current_Alarm).Timestamp;
--                 Time_From_Hyp_Start := Time_From_Hyp_Start +
--                   Integer(Float'Ceiling(((Float(Current_TB - Last_Read_TB)) / Float(Clock_Freq_Hz * 10**3))));
            end if;

--              Put_Line ("[SYSTEM.BB.TIME.ALARM_HANDLER_ATLP]  " &
--                          "Current HYP time is " &
--                          Integer'Image (Time_From_Hyp_Start) &
--                          " corresponding to alarm " &
--                          Integer'Image (Alarms_Queue (Current_Alarm).Timestamp));

            if Time_From_Hyp_Start = 180 then
               Asm ("nop", Volatile => True);
            end if;


            --  in-place deBruijn
            Bitmask_Index := 7 -
              Queues.Get_Index_DeBruijn (Alarms_Queue (Current_Alarm).Alarm_Root_Bitmask);
            Priority_Level := (32 * Bitmask_Index) +
              (31 - Queues.Get_Index_DeBruijn
                 (Alarms_Queue (Current_Alarm).Alarm_Child_Bitmasks (Bitmask_Index)));

--              Put_Line ("Executing thread is " &
--                          System.Any_Priority'Image (Running_Thread.Base_Priority) &
--                          ", interrupting thread is " &
--                       System.Any_Priority'Image (Priority_Level));

            if Running_Thread.Base_Priority = 2 then
               --  no thread was executing
               Threads.Queues.Update_Ready_Queue (Alarms_Queue (Current_Alarm));

--                 Put_Line ("[SYSTEM.BB.TIME.ALARM_HANDLER_ATLP]  " &
--                             "Activation of user task ");
--                 Put_Line ("[SYSTEM.BB.TIME.ALARM_HANDLER_ATLP]  " &
--                             "Setting next alarm to now + " &
--                             Peripherals.Timer_Interval'Image (Alarms_Queue (Current_Alarm).Interval));

               Threads.Queues.Next_Alarm := Activation;
               Peripherals.Set_Alarm
                 (Alarms_Queue (Current_Alarm).Interval
                  * Peripherals.Timer_Interval (Peripherals.Clock_Freq_Hz));
               Current_Alarm := Alarms_Queue (Current_Alarm).Next;
               Protection.Leave_Kernel;

            elsif Priority_Level >
              Running_Thread.Base_Priority then
               --  activation of HP task
--                 Put_Line ("[SYSTEM.BB.TIME.ALARM_HANDLER_ATLP]  " &
--                             "Entering NPR of task " &
--                             System.Any_Priority'Image (Running_Thread.Base_Priority) &
--                             " at time " &
--                             Integer'Image (Time_From_Hyp_Start));
--                 Put_Line ("[SYSTEM.BB.TIME.ALARM_HANDLER_ATLP]  Setting alarm to now + " &
--                             Integer'Image (Running_Thread.NPR_Length));

               Next_Alarm := End_Floating_NPR;
               Peripherals.Set_Alarm
                 (Peripherals.Timer_Interval
                    (Running_Thread.NPR_Length * Peripherals.Clock_Freq_Hz));
               System.BB.CPU_Primitives.Enable_Interrupts (0);
            else
               --  activation of LP task
               Threads.Queues.Update_Ready_Queue (Alarms_Queue (Current_Alarm));
--
--                 Put_Line ("[SYSTEM.BB.TIME.ALARM_HANDLER_ATLP]  " &
--                             "Activation of a lower priority task");

               Next_Alarm := Activation;
               Peripherals.Set_Alarm
                 (Alarms_Queue (Current_Alarm).Interval
                  * Peripherals.Timer_Interval(Peripherals.Clock_Freq_Hz));
               Current_Alarm := Alarms_Queue (Current_Alarm).Next;
               System.BB.CPU_Primitives.Enable_Interrupts (0);
--                 Protection.Leave_Kernel;

            end if;

            --  Protection.Leave_Kernel;
--              System.BB.CPU_Primitives.Enable_Interrupts (0);

         when End_Floating_NPR =>

            Protection.Enter_Kernel;
--              Old_Time_From_Hyp := Time_From_Hyp_Start;
--              Tmp_Timestamp := Time_From_Hyp_Start;
--              Old_Alarm := Alarms_Queue (Current_Alarm);
            Current_TB := Peripherals.Read_Clock;

            Time_From_Hyp_Start := Time_From_Hyp_Start +
              (Integer (Current_TB - Last_Read_TB) / Peripherals.Clock_Freq_Hz);

            Last_Read_TB := Current_TB;

--              Put_Line ("[SYSTEM.BB.TIME.ALARM_HANDLER_ATLP]  " &
--                          "End of NPR region for task " &
--                          System.Any_Priority'Image (Running_Thread.Base_Priority) &
--                          " at time " &
--                          Integer'Image (Time_From_Hyp_Start));

            --  recover masked activations: JITTERY PART
            while Alarms_Queue (Current_Alarm).Timestamp <= Time_From_Hyp_Start loop
--                 Put_Line ("N" & Integer'Image (Alarms_Queue (Current_Alarm).Timestamp));
               Threads.Queues.Update_Ready_Queue (Alarms_Queue (Current_Alarm));
               Current_Alarm := Alarms_Queue (Current_Alarm).Next;
            end loop;

            Tmp_Timestamp := Alarms_Queue (Current_Alarm).Timestamp-Time_From_Hyp_Start;
            Threads.Queues.Next_Alarm := Activation;

--              Put_Line ("[SYSTEM.BB.TIME.ALARM_HANDLER_ATLP] " &
--                          "Setting next alarm to now + " & Integer'Image (Tmp_Timestamp));
            Peripherals.Set_Alarm (Peripherals.Timer_Interval
                                   (Tmp_Timestamp * Peripherals.Clock_Freq_Hz));


            Protection.Leave_Kernel;

         when End_WCET_Budget =>
            null;
         when others =>
            null;
      end case;

   end Alarm_Handler_Activation_Triggered_LP_Sched;

   --------------------------------
   -- Alarm_Handler_FPP_LP_Sched --
   --------------------------------

   procedure Alarm_Handler_FPP_LP_Sched is
      use Threads.Queues;
   begin

      case Next_Alarm is
         when Activation =>
            null;
            --                    Put_Line ("[SYSTEM.BB.TIME.ALARM_HANDLER] " &
            --                                "TASK ACTIVATION EVENT (ALARM " &
            --                          Integer'Image (Current_Alarm) & ")");
            --
            --                    Time_From_Hyp_Start :=
            --                      Alarms_Queue (Current_Alarm).Timestamp;
            --
            --                    Put_Line ("[SYSTEM.BB.TIME.ALARM_HANDLER]  " &
            --                                "Current HYP time is " &
            --                          Integer'Image (Time_From_Hyp_Start));
            --
            --                    --  The access to the queues must be protected
            --
            --                    Protection.Enter_Kernel;
            --
            --                    Threads.Queues.Update_Ready_Queue
            --                      (Alarms_Queue (Current_Alarm));
            --
            --                    Threads.Queues.Print_Masks;
            --
            --                    Aux := Get_First_Thread (True);
            --                    Aux.Remaining_WCET := Aux.WCET_Budget;
            --                    --  NEED TO REPLENISH WCET BUDGET? MAYBE IN DELAY_UNTIL
            --                    Put_Line ("[SYSTEM.BB.TIME.ALARM_HANDLER] " &
            --                          "First thread is " &
            --                          System.Any_Priority'Image
            --                          (Aux.Active_Priority)
            --                        & ": setting Alarm to "
            --                        & Integer'Image
            --                                (Aux.NPR_Length));
            --
            --                    Threads.Queues.Print_Masks;
            --
            --                    --  Option B: Uncomment for FPP behaviour (next interrupt
            --                    --  is at next NPR)
            --
            --                    --  NPR_Alarm.Event_Type := Fixed_Preemption_Point;
            --                    Threads.Queues.Next_Alarm := Fixed_Preemption_Point;
            --                    Peripherals.Set_Alarm
            --                      (Peripherals.Timer_Interval
            --                         (Aux.NPR_Length) * (Clock_Freq_Hz * 10**3));
            --
            --                    Current_Alarm := Alarms_Queue (Current_Alarm).Next;
            --
            --                    --  We have finished the modifications to the queues
            --                    Protection.Leave_Kernel;
         when Fixed_Preemption_Point =>
            null;
            --              Put_Line ("[SYSTEM.BB.TIME.ALARM_HANDLER] " &
            --            "PREEMPTION POINT EVENT");
            --
            --              Protection.Enter_Kernel;
            --              --  Here, Current_Alarm denotes next periodic activation,
            --              --  which is still to fire
            --
            --              --  update remaining ET and ready mask
            --              Current_Task.Remaining_WCET :=
            --                Current_Task.Remaining_WCET - Current_Task.NPR_Length;
            --              Time_From_Hyp_Start :=
            --                Time_From_Hyp_Start + Current_Task.NPR_Length;
            --
            --              Put_Line ("[SYSTEM.BB.TIME.ALARM_HANDLER] HYP time: " &
            --                       Integer'Image (Time_From_Hyp_Start));
            --
            --              Threads.Queues.Update_Ready_Queue (NPR_Alarm);
            --              Threads.Queues.Print_Masks;
            --
            --              --  check min(remaining ET, NPR length)
            --              if Current_Task.Remaining_WCET <=
            --                Current_Task.NPR_Length then
            --                 Put_Line ("[SYSTEM.BB.TIME.ALARM_HANDLER]" &
            --                             " Next event will be end wcet budget");
            --                 Next_Candidate_PP := Current_Task.Remaining_WCET;
            --                 --  NPR_Alarm.Event_Type := End_WCET_Budget;
            --                 Next_Alarm := End_WCET_Budget;
            --                 --  do nothing, will be managed by delay_until call
            --              else
            --                 Put_Line ("[SYSTEM.BB.TIME.ALARM_HANDLER]" &
            --                             " Next event will be end of NPR");
            --                 Next_Candidate_PP := Current_Task.NPR_Length;
            --                 --  NPR_Alarm.Event_Type := Fixed_Preemption_Point;
            --                 Next_Alarm := Fixed_Preemption_Point;
            --              end if;
            --
            --              --  next event is NPR end or task termination
            --              --  may want to serve interrupts or timing events
            --              --  TODO: if Next_Candidate_PP =
            --            Current_Task.all.NPR_Length then
            --              --  Protection.Enter_Kernel;
            --              --  create fake alarm for NPR end (do we need it?)
            --              --  NPR_Alarm.Interval := Current_Task.all.NPR_Length;
            --              --  NPR_Alarm.Next := Alarms_Queue (Current_Alarm);
            --              --  NPR_Alarm.Thread := Current_Task;
            --              --  NPR_Alarm.Timestamp;
            --
            --              while
            --                Alarms_Queue (Current_Alarm).Timestamp < Next_Candidate_PP
            --              loop
            --                 Put_Line ("[SYSTEM.BB.TIME.ALARM_HANDLER] Looping");
            --                 --  accumulate masked activations : JITTERY PART
            --                 NPR_Alarm.Alarm_Root_Bitmask :=
            --            NPR_Alarm.Alarm_Root_Bitmask or
            --                   Alarms_Queue (Current_Alarm).Alarm_Root_Bitmask;
            --                 for index in 0 .. 7 loop
            --                    NPR_Alarm.Alarm_Child_Bitmasks (index) :=
            --                      NPR_Alarm.Alarm_Child_Bitmasks (index) or
            --                      Alarms_Queue (Current_Alarm).
            --            Alarm_Child_Bitmasks (index);
            --                 end loop;
            --                 Current_Alarm := Alarms_Queue (Current_Alarm).Next;
            --              end loop;
            --
            --              --  program timer accordingly
            --              if Next_Candidate_PP = Current_Task.NPR_Length then
            --                 --  next alarm is PP
            --                 Put_Line ("[SYSTEM.BB.TIME.ALARM_HANDLER]" &
            --                             " Setting alarm for next NPR to " &
            --                             Integer'Image (Next_Candidate_PP));
            --                 Peripherals.Set_Alarm
            --                   (Peripherals.Timer_Interval (Next_Candidate_PP)
            --                    * (Clock_Freq_Hz * 10**3));
            --              else
            --                 --  next alarm will be of task activation type,
            --                 --  since current task will run out of ET budget
            --                 Put_Line ("[SYSTEM.BB.TIME.ALARM_HANDLER]" &
            --                             " Setting alarm for wcet end");
            --                 Peripherals.Set_Alarm
            --                   (Alarms_Queue (Current_Alarm).Interval
            --                    * (Clock_Freq_Hz * 10**3));
            --                 --  TODO : compute actual time interval from now to alarm
            --              end if;
            --
            --              Protection.Leave_Kernel;
         when End_WCET_Budget =>
            null;
            --  MANAGE OVERRUN
            --              --  Protection.Enter_Kernel;
            --
            --              Put_Line ("WARNING: WCET OVERRUN");
            --              Time_From_Hyp_Start :=
            --                Time_From_Hyp_Start + Current_Task.Remaining_WCET;
            --
            --              Running_Thread.Extraction_Pending := True;
            --
            --              if Alarms_Queue (Current_Alarm).Timestamp -
            --                Time_From_Hyp_Start = 0 then
            --
            --                 Threads.Queues.Update_Ready_Queue
            --                   (Alarms_Queue (Current_Alarm));
            --
            --                 Aux := Get_First_Thread (True);
            --
            --                 if Aux = Running_Thread then
            --                    Running_Thread.Extraction_Pending := False;
            --                 end if;
            --
            --                 Aux.Remaining_WCET := Aux.WCET_Budget;
            --                 --  NEED TO REPLENISH WCET BUDGET?
            --
            --                 --  Option B: Uncomment for FPP behaviour (next interrupt
            --                 --  is at next NPR)
            --                 Peripherals.Set_Alarm
            --                   (Peripherals.Timer_Interval
            --                      (Aux.NPR_Length) * (Clock_Freq_Hz * 10**3));
            --                 --  NPR_Alarm.Event_Type := Fixed_Preemption_Point;
            --                 Next_Alarm := Fixed_Preemption_Point;
            --
            --                 Current_Alarm := Alarms_Queue (Current_Alarm).Next;
            --
            --              else
            --                 --  NPR_Alarm.Event_Type := Activation;
            --                 Next_Alarm := Activation;
            --                 Peripherals.Set_Alarm
            --                (Peripherals.Timer_Interval (
            --                 (Alarms_Queue (Current_Alarm).Timestamp -
            --            Time_From_Hyp_Start)
            --                )
            --                    * (Clock_Freq_Hz * 10**3));
            --              end if;
            --
            --              --  Protection.Leave_Kernel;

         when others =>
            null;

      end case;

   end Alarm_Handler_FPP_LP_Sched;

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

   procedure Delay_Until_FP_Sched (T : Time);
   procedure Delay_Until_Activation_Triggered_LP_Sched (T : Time);
   procedure Delay_Until_FPP_LP_Sched (T : Time);

   -----------------
   -- Delay_Until --
   -----------------
   procedure Delay_Until (T : Time) is
      use Threads.Queues;
   begin
      case Preempt_Policy is
         when Fully_Preemptive =>
            Delay_Until_FP_Sched (T);
         when Activation_Triggered_NPR | Model_NPR =>
            Delay_Until_Activation_Triggered_LP_Sched (T);
         when Fixed_Preemption_Points =>
            Delay_Until_FPP_LP_Sched (T);
      end case;
   end Delay_Until;

   --------------------------
   -- Delay_Until_FP_Sched --
   --------------------------
   procedure Delay_Until_FP_Sched (T : Time) is
      Self              : Threads.Thread_Id;
   begin

      Protection.Enter_Kernel;

      Self := Threads.Thread_Self;
--        Put_Line ("[SYSTEM.BB.TIME.DELAY_UNTIL_FP] Delay until for thread at priority " &
--                    System.Any_Priority'Image (Self.Base_Priority));

      Self.State := Threads.Delayed;

      Threads.Queues.Extract (Self);

      Self.Alarm_Time := T;

      Protection.Leave_Kernel;
   end Delay_Until_FP_Sched;

   -----------------------------------------------
   -- Delay_Until_Activation_Triggered_LP_Sched --
   -----------------------------------------------
   procedure Delay_Until_Activation_Triggered_LP_Sched (T : Time) is
      use Threads.Queues;
      Self              : Threads.Thread_Id;
      Current_TB : Peripherals.Timer_Interval;
      Tmp_Timestamp : Integer;
--        Old_Time_From_Hyp : Integer;
--        Old_Alarm : Alarm;
   begin
      Protection.Enter_Kernel;

      Current_TB := Peripherals.Read_Clock;
--        Tmp_Timestamp := Time_From_Hyp_Start;
--        Old_Alarm := Alarms_Queue (Current_Alarm);

      Self := Threads.Thread_Self;
      Self.State := Threads.Delayed;
      Threads.Queues.Extract (Self);
      Self.Alarm_Time := T;

      Time_From_Hyp_Start := Time_From_Hyp_Start +
        (Integer (Current_TB - Last_Read_TB) / Peripherals.Clock_Freq_Hz);
      Last_Read_TB := Current_TB;

--        Put_Line ("[SYSTEM.BB.TIME.DELAY_UNTIL_ATLP] " &
--                    "Delay Until at time " &
--                    Integer'Image (Time_From_Hyp_Start) &
--                    " for task "&
--                    System.Any_Priority'Image (Self.Base_Priority));

      if Next_Alarm = End_Floating_NPR then


--           Put ("[SYSTEM.BB.TIME.DELAY_UNTIL_ATLP] Recovering from NPR region: ");
         --  in NPR, restore masked activations: JITTERY PART
            while Alarms_Queue (Current_Alarm).Timestamp <= Time_From_Hyp_Start loop
--                 Put_Line ("D"& Integer'Image (Alarms_Queue (Current_Alarm).Timestamp));
               Threads.Queues.Update_Ready_Queue (Alarms_Queue (Current_Alarm));
               Current_Alarm := Alarms_Queue (Current_Alarm).Next;
            end loop;

            Threads.Queues.Next_Alarm := Activation;
            Tmp_Timestamp := Alarms_Queue (Current_Alarm).Timestamp - Time_From_Hyp_Start;
--           Put_Line ("[SYSTEM.BB.TIME.DELAY_UNTIL_ATLP] " &
--                       "Setting next alarm to now (" & Integer'Image (Time_From_Hyp_Start) &") + " & Integer'Image (Tmp_Timestamp));

            Peripherals.Set_Alarm (Peripherals.Timer_Interval
              (Tmp_Timestamp * Peripherals.Clock_Freq_Hz));



      else
         --  not in NPR, nothing to be done
         --           Put_Line ("Simple delay_until");
         null;
      end if;

      Protection.Leave_Kernel;

   end Delay_Until_Activation_Triggered_LP_Sched;

   ------------------------------
   -- Delay_Until_FPP_LP_Sched --
   ------------------------------
   procedure Delay_Until_FPP_LP_Sched (T : Time) is
   begin
      null;
   end Delay_Until_FPP_LP_Sched;

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
