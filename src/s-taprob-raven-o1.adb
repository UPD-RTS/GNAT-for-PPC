------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--      S Y S T E M . T A S K I N G . P R O T E C T E D _ O B J E C T S     --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1992-2010, Free Software Foundation, Inc.         --
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
------------------------------------------------------------------------------

--  This is the Ravenscar version of this package
with System.Task_Primitives.Operations;
--  Used for Set_Priority
--           Get_Priority
--           Self

with System.BB.Threads.Queues;
with System.BB.Peripherals;
with System.BB.Protection;
with System.BB.Serial_Output; use System.BB.Serial_Output;
with System.BB.Time;
use System.BB;
with System.BB.CPU_Primitives;
with Interfaces;

package body System.Tasking.Protected_Objects is

   type CS_Type is (A, B, C);
   Critical_Section_Type : CS_Type;

   use System.Task_Primitives.Operations;

   ---------------------------
   -- Initialize_Protection --
   ---------------------------

   procedure Initialize_Protection
     (Object           : Protection_Access;
      Ceiling_Priority : Integer)
   is
      Init_Priority : Integer := Ceiling_Priority;

   begin
      if Init_Priority = Unspecified_Priority then
         Init_Priority := System.Priority'Last;
      end if;

      Object.Ceiling := System.Any_Priority (Init_Priority);
      Object.Caller_Priority := System.Any_Priority'First;
      Object.Owner := Null_Task;
   end Initialize_Protection;

   procedure Lock_FP (Object : Protection_Access);
   procedure Lock_Activation_Triggered_LP_Sched (Object : Protection_Access);
   procedure Lock_Model_LP_Sched (Object : Protection_Access);

   ----------
   -- Lock --
   ----------

   procedure Lock (Object : Protection_Access) is
      use Threads.Queues;
   begin
      case Preempt_Policy is
         when Fully_Preemptive =>
            Lock_FP (Object);
         when Activation_Triggered_NPR =>
            Lock_Activation_Triggered_LP_Sched (Object);
         when Model_NPR =>
            Lock_Model_LP_Sched (Object);
         when Fixed_Preemption_Points =>
            null;
      end case;
   end Lock;

   -------------
   -- Lock_FP --
   -------------

   procedure Lock_FP (Object : Protection_Access) is
      Self_Id         : constant Task_Id := Self;
      Caller_Priority : constant Any_Priority := Get_Priority (Self_Id);

   begin
      --  For this run time, pragma Detect_Blocking is always active. As
      --  described in ARM 9.5.1, par. 15, an external call on a protected
      --  subprogram with the same target object as that of the protected
      --  action that is currently in progress (i.e., if the caller is
      --  already the protected object's owner) is a potentially blocking
      --  operation, and hence Program_Error must be raised.

      --  AB: ince set_priority is protected no more
      System.BB.Protection.Enter_Kernel;

      if Object.Owner = Self_Id then
         raise Program_Error;
      end if;

      --  Check ceiling locking violation

      if Caller_Priority > Object.Ceiling then
         raise Program_Error;
      end if;

      Set_Priority (Self_Id, Object.Ceiling);

--        Put_Line ("Lock for task " &
--                    System.Any_Priority'Image (Caller_Priority));
      --  Update the protected object's owner

      Object.Owner := Self_Id;

      --  Store caller's active priority so that it can be later
      --  restored when finishing the protected action.

      Object.Caller_Priority := Caller_Priority;

      --  We are entering in a protected action, so that we increase the
      --  protected object nesting level.

      Self_Id.Common.Protected_Action_Nesting :=
        Self_Id.Common.Protected_Action_Nesting + 1;
      --  AB: ince set_priority is protected no more
      System.BB.Protection.Leave_Kernel;
   end Lock_FP;

   ----------------------------------------
   -- Lock_Activation_Triggered_LP_Sched --
   ----------------------------------------

   procedure Lock_Activation_Triggered_LP_Sched (Object : Protection_Access) is
      pragma Style_Checks (Off);
      use Threads;
      use Threads.Queues;
      use Peripherals;
      Self_Id         : constant Task_Id := Self;
      Caller_Priority : constant Any_Priority := Get_Priority (Self_Id);
      Current_TB : Peripherals.Timer_Interval;
      Now : Integer;
      Tmp_Timestamp : Integer;
   begin
      System.BB.Protection.Enter_Kernel;
      Current_TB := Peripherals.Read_Clock;

--        Put_Line ("[SYSTEM.TASKING.PROTECTED_OBJECTS.LOCK_ATLP] " &
--                    "Trying to enter CS at " &
--            Integer'Image (Time_From_Hyp_Start +
--            (Integer (Current_TB - Last_Read_TB) / Peripherals.Clock_Freq_Hz)));

      if Object.Owner = Self_Id then
         raise Program_Error;
      end if;

      --  Check ceiling locking violation

      if Caller_Priority > Object.Ceiling then
         raise Program_Error;
      end if;

      case Next_Alarm is
         when Activation =>
            --  outside NPR
            --  case B: begin NPR immediately

            --  update SW clock, since we are entering a NPR
            Time_From_Hyp_Start := Time_From_Hyp_Start +
              (Integer (Current_TB - Last_Read_TB) / Peripherals.Clock_Freq_Hz);
            Last_Read_TB := Current_TB;

--              Put_Line ("[SYSTEM.TASKING.PROTECTED_OBJECTS.LOCK_ATLP] (Time " &
--                          Integer'Image (Time_From_Hyp_Start) &
--                          ") Case B: Entering NPR of task " &
--                          System.Any_Priority'Image (Running_Thread.Base_Priority));

            Critical_Section_Type := B;
            Set_Priority (Self_Id, Object.Ceiling);
            --  Update the protected object's owner
            Object.Owner := Self_Id;
            --  Store caller's active priority so that it can be later
            --  restored when finishing the protected action.
            Object.Caller_Priority := Caller_Priority;
            --  We are entering in a protected action, so that we increase the
            --  protected object nesting level.
            Self_Id.Common.Protected_Action_Nesting :=
              Self_Id.Common.Protected_Action_Nesting + 1;

            Threads.Queues.Next_Alarm := End_Floating_NPR;
            Peripherals.Set_Alarm
              (Peripherals.Timer_Interval
                 (Running_Thread.NPR_Length * Peripherals.Clock_Freq_Hz));

         when End_Floating_NPR =>

            --  OTTIMIZZAZIONE RUNTIME: CHECK IF THERE'S TIME TO COMPLETE CS
            --  within NPR
            --  case C: start CS only if it ends before NPR end
            Now := Integer (Current_TB - Last_Read_TB) / Peripherals.Clock_Freq_Hz;
            --  ATTENTION: keep the "=" to avoid End_NPR to fire just before Unlock
            --  (still at the same time instant)
            if Time_From_Hyp_Start + Now + Running_Thread.CS_Length >=
              Time_From_Hyp_Start + Running_Thread.NPR_Length then

               --  update SW clock, since we are recovering from a NPR
               Time_From_Hyp_Start := Time_From_Hyp_Start + Now;
               Last_Read_TB := Current_TB;

--                 Put_Line ("[SYSTEM.TASKING.PROTECTED_OBJECTS.LOCK_ATLP] (Time " &
--                             Integer'Image (Time_From_Hyp_Start) &
--                             ") Case C: Time to finish CS of task " &
--                             System.Any_Priority'Image (Running_Thread.Base_Priority)
--                           & " is not enough -> Suspending");

               Critical_Section_Type := C;
               --  Enter_Kernel is redundant if preemption is disabled at the beginning of Lock
--                 System.BB.Protection.Enter_Kernel;
               --  don't enter critical section, there is no time to complete
--                 Thread_Self.State := Threads.Delayed;
--                 Threads.Queues.Extract (Thread_Self);
               --  Thread_Self.Alarm_Time := T;

               --  TODO: check if there are alarms to recover (might not be the case with MORE than one CS within
               --  the same NPR)
               --  in NPR, restore missed activations: JITTERY PART
               while Alarms_Queue (Current_Alarm).Timestamp <= Time_From_Hyp_Start loop
--                    Put_Line ("L" & Integer'Image (Alarms_Queue (Current_Alarm).Timestamp));
                  Threads.Queues.Update_Ready_Queue (Alarms_Queue (Current_Alarm));
                  Current_Alarm := Alarms_Queue (Current_Alarm).Next;
               end loop;

               Tmp_Timestamp := Alarms_Queue (Current_Alarm).Timestamp - Time_From_Hyp_Start;
               Threads.Queues.Next_Alarm := Activation;
               Peripherals.Set_Alarm
                 (Peripherals.Timer_Interval
                    (Tmp_Timestamp * Peripherals.Clock_Freq_Hz));
               System.BB.Protection.Leave_Kernel;
               --  RECURSIVE CALL: TRY TO ACQUIRE LOCK BACK WHEN SCHEDULED AGAIN
               Lock (Object);
            else

--                 Put_Line ("[SYSTEM.TASKING.PROTECTED_OBJECTS.LOCK_ATLP] (Time " &
--                     Integer'Image (Time_From_Hyp_Start +
--                     (Integer ((Current_TB - Last_Read_TB)) / Peripherals.Clock_Freq_Hz)) &
--                     ") Case A: Time to finish CS of task " &
--                     System.Any_Priority'Image (Running_Thread.Base_Priority) &
--                     " is enough -> Entering CS");

               --  case A: enter critical section safely
               Critical_Section_Type := A;
               Set_Priority (Self_Id, Object.Ceiling);
               --  Update the protected object's owner
               Object.Owner := Self_Id;
               --  Store caller's active priority so that it can be later
               --  restored when finishing the protected action.
               Object.Caller_Priority := Caller_Priority;
               --  We are entering in a protected action, so that we increase the
               --  protected object nesting level.
               Self_Id.Common.Protected_Action_Nesting :=
                 Self_Id.Common.Protected_Action_Nesting + 1;
            end if;

         when others =>
            raise Program_Error;
      end case;
      System.BB.CPU_Primitives.Enable_Interrupts (0);
      --  Leave_Kernel should be ok as well since task priority has been raised to ceiling

   end Lock_Activation_Triggered_LP_Sched;

   ----------------------------------------
   -- Lock_Model_LP_Sched --
   ----------------------------------------

   procedure Lock_Model_LP_Sched (Object : Protection_Access) is
      pragma Style_Checks (Off);
      use Threads;
      use Threads.Queues;
      use Peripherals;
      Self_Id         : constant Task_Id := Self;
      Caller_Priority : constant Any_Priority := Get_Priority (Self_Id);
      Current_TB : Peripherals.Timer_Interval;
      Now : Integer;
   begin
      System.BB.Protection.Enter_Kernel;
      Current_TB := Peripherals.Read_Clock;
      --
      --        Put_Line ("[SYSTEM.TASKING.PROTECTED_OBJECTS.LOCK_MOLP] " &
      --                    "Trying to enter CS at " &
      --            Integer'Image (Time_From_Hyp_Start +
      --            (Integer (Current_TB - Last_Read_TB) / Peripherals.Clock_Freq_Hz)));

      if Object.Owner = Self_Id then
         raise Program_Error;
      end if;

      --  Check ceiling locking violation

      if Caller_Priority > Object.Ceiling then
         raise Program_Error;
      end if;

      case Next_Alarm is
         when Activation =>
            --  outside NPR
            --  case B: begin NPR immediately

            --  update SW clock, since we are entering a NPR
            Time_From_Hyp_Start := Time_From_Hyp_Start +
              (Integer (Current_TB - Last_Read_TB) / Peripherals.Clock_Freq_Hz);
            Last_Read_TB := Current_TB;

--                          Put_Line ("[SYSTEM.TASKING.PROTECTED_OBJECTS.LOCK_MOLP] (Time " &
--                                      Integer'Image (Time_From_Hyp_Start) &
--                                      ") Case B: Entering NPR of task " &
--                                      System.Any_Priority'Image (Running_Thread.Base_Priority));

            Critical_Section_Type := B;
            Set_Priority (Self_Id, Object.Ceiling);
            --  Update the protected object's owner
            Object.Owner := Self_Id;
            --  Store caller's active priority so that it can be later
            --  restored when finishing the protected action.
            Object.Caller_Priority := Caller_Priority;
            --  We are entering in a protected action, so that we increase the
            --  protected object nesting level.
            Self_Id.Common.Protected_Action_Nesting :=
              Self_Id.Common.Protected_Action_Nesting + 1;

            Threads.Queues.Next_Alarm := End_Floating_NPR;

            --  choose max(cs,npr) since npr=q-b is always safe (and b>npr by assumption)
            if Running_Thread.CS_Length > Running_Thread.NPR_Length then
               Peripherals.Set_Alarm
                 (Peripherals.Timer_Interval
                    (Running_Thread.CS_Length * Peripherals.Clock_Freq_Hz));
            else
               Peripherals.Set_Alarm
                 (Peripherals.Timer_Interval
                    (Running_Thread.NPR_Length * Peripherals.Clock_Freq_Hz));
            end if;

         when End_Floating_NPR =>
            --  (SOLUZIONE MODELLO: (Q-B) IS ALWAYS SAFE)

            --  OTTIMIZZAZIONE RUNTIME: CHECK IF THERE'S TIME TO COMPLETE CS
            --  within NPR
            --  case C: start CS only if it ends before NPR end
            Now := Integer (Current_TB - Last_Read_TB) / Peripherals.Clock_Freq_Hz;
            --  ATTENTION: keep the "=" to avoid End_NPR to fire just before Unlock
            --  (still at the same time instant)
            if Time_From_Hyp_Start + Now + Running_Thread.CS_Length >=
              Time_From_Hyp_Start + Running_Thread.NPR_Length then

               --  update SW clock, since we are recovering from a NPR
--                 Time_From_Hyp_Start := Time_From_Hyp_Start + Now;
--                 Last_Read_TB := Current_TB;
--
--                                Put_Line ("[SYSTEM.TASKING.PROTECTED_OBJECTS.LOCK_MOLP] (Time " &
--                                            Integer'Image (Time_From_Hyp_Start) &
--                                            ") Case C: Time to finish CS of task " &
--                                            System.Any_Priority'Image (Running_Thread.Base_Priority)
--                                          & " is not enough -> Suspending");

               Critical_Section_Type := C;

               Set_Priority (Self_Id, Object.Ceiling);
               --  Update the protected object's owner
               Object.Owner := Self_Id;
               --  Store caller's active priority so that it can be later
               --  restored when finishing the protected action.
               Object.Caller_Priority := Caller_Priority;
               --  We are entering in a protected action, so that we increase the
               --  protected object nesting level.
               Self_Id.Common.Protected_Action_Nesting :=
                 Self_Id.Common.Protected_Action_Nesting + 1;

--                 Threads.Queues.Next_Alarm := Activation;
               Peripherals.Set_Alarm
                 (Peripherals.Timer_Interval
                    (180 * Peripherals.Clock_Freq_Hz));
            else

--                                Put_Line ("[SYSTEM.TASKING.PROTECTED_OBJECTS.LOCK_MOLP] (Time " &
--                                    Integer'Image (Time_From_Hyp_Start +
--                                    (Integer ((Current_TB - Last_Read_TB)) / Peripherals.Clock_Freq_Hz)) &
--                                    ") Case A: Time to finish CS of task " &
--                                    System.Any_Priority'Image (Running_Thread.Base_Priority) &
--                                    " is enough -> Entering CS");

               --  case A: enter critical section safely
               Critical_Section_Type := A;
               Set_Priority (Self_Id, Object.Ceiling);
               --  Update the protected object's owner
               Object.Owner := Self_Id;
               --  Store caller's active priority so that it can be later
               --  restored when finishing the protected action.
               Object.Caller_Priority := Caller_Priority;
               --  We are entering in a protected action, so that we increase the
               --  protected object nesting level.
               Self_Id.Common.Protected_Action_Nesting :=
                 Self_Id.Common.Protected_Action_Nesting + 1;
            end if;

         when others =>
            raise Program_Error;
      end case;
      System.BB.CPU_Primitives.Enable_Interrupts (0);
      --  Leave_Kernel should be ok as well since task priority has been raised to ceiling

   end Lock_Model_LP_Sched;

   procedure Unlock_FP (Object : Protection_Access);
   procedure Unlock_Activation_Triggered_LP_Sched (Object : Protection_Access);
   procedure Unlock_Model_LP_Sched (Object : Protection_Access);

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Object : Protection_Access) is
      use Threads.Queues;
   begin
      case Preempt_Policy is
         when Fully_Preemptive =>
            Unlock_FP (Object);
         when Activation_Triggered_NPR =>
            Unlock_Activation_Triggered_LP_Sched (Object);
         when Model_NPR =>
            Unlock_Model_LP_Sched (Object);
         when Fixed_Preemption_Points =>
            null;
      end case;
   end Unlock;

   ---------------
   -- Unlock_FP --
   ---------------

   procedure Unlock_FP (Object : Protection_Access) is
      Self_Id : constant Task_Id := Self;

   begin
      --  Calls to this procedure can only take place when being within a
      --  protected action and when the caller is the protected object's
      --  owner.

      --  AB: ince set_priority is protected no more
      System.BB.Protection.Enter_Kernel;

      pragma Assert (Self_Id.Common.Protected_Action_Nesting > 0
                     and then Object.Owner = Self_Id);

      --  Remove ownership of the protected object

      Object.Owner := Null_Task;

      --  We are exiting from a protected action, so that we decrease the
      --  protected object nesting level.

      Self_Id.Common.Protected_Action_Nesting :=
        Self_Id.Common.Protected_Action_Nesting - 1;

      Set_Priority (Self_Id, Object.Caller_Priority);
      --  AB: ince set_priority is protected no more
      System.BB.Protection.Leave_Kernel;
   end Unlock_FP;

   ------------------------------------------
   -- Unlock_Activation_Triggered_LP_Sched --
   ------------------------------------------

   procedure Unlock_Activation_Triggered_LP_Sched (Object : Protection_Access) is
      use Threads.Queues;
      use System.BB.Peripherals;
      use System.BB.Protection;
      Self_Id : constant Task_Id := Self;

      --  COMMENT FOR MODEL-SOLUTION
      Current_TB : Peripherals.Timer_Interval;
      Tmp_Timestamp : Integer;
      Bitmask_Index : Integer;
      Priority_Level : System.Any_Priority;
      Found_HP_Alarm : Boolean := False;

   begin

      System.BB.Protection.Enter_Kernel;
      Current_TB := Peripherals.Read_Clock;
--        Put_Line ("[SYSTEM.TASKING.PROTECTED_OBJECTS.UNLOCK_ATLP] (Time " &
--            Integer'Image (Time_From_Hyp_Start +
--            (Integer (Current_TB - Last_Read_TB) / Peripherals.Clock_Freq_Hz)) &
--            ") Unlock " & System.Any_Priority'Image (Running_Thread.Base_Priority));

      pragma Assert (Self_Id.Common.Protected_Action_Nesting > 0
                     and then Object.Owner = Self_Id);

      --  Remove ownership of the protected object

      Object.Owner := Null_Task;

      --  We are exiting from a protected action, so that we decrease the
      --  protected object nesting level.

      Self_Id.Common.Protected_Action_Nesting :=
        Self_Id.Common.Protected_Action_Nesting - 1;
      Set_Priority (Self_Id, Object.Caller_Priority);

      case Critical_Section_Type is
         when A =>
            null;
         when B =>
            --  update SW clock since we are recovering from a NPR
            Time_From_Hyp_Start := Time_From_Hyp_Start +
              (Integer (Current_TB - Last_Read_TB) / Peripherals.Clock_Freq_Hz);
            Last_Read_TB := Current_TB;
            --  COMMENT FOR MODEL SOLUTION

            --  OTTIMIZZAZIONE CASO B: all'uscita di CS prosegue nella NPR solo se ci
            --  sono attivazioni mascherate
            if Alarms_Queue (Current_Alarm).Timestamp <= Time_From_Hyp_Start then
               --  there are masked alarms
               while Alarms_Queue (Current_Alarm).Timestamp <= Time_From_Hyp_Start loop
--                    Put_Line ("B"& Integer'Image (Alarms_Queue (Current_Alarm).Timestamp));

                  --  ***************************************************************************
                  --  In-place deBruijn to see whether activated task is HP or LP
                  Bitmask_Index := 7 -
                    Get_Index_DeBruijn (Alarms_Queue (Current_Alarm).Alarm_Root_Bitmask);
                  Priority_Level := (32 * Bitmask_Index) +
                    (31 - Get_Index_DeBruijn
                       (Alarms_Queue (Current_Alarm).Alarm_Child_Bitmasks (Bitmask_Index)));
                  --  ***************************************************************************

                  if Priority_Level > Running_Thread.Base_Priority then
                     --  HP activation
                     if not Found_HP_Alarm then
                        --  first HP alarm found
                        --  set next timer according to virtual NPR
                        Threads.Queues.Next_Alarm := End_Floating_NPR;
                        Found_HP_Alarm := True;
--                          Put_Line ("[SYSTEM.TASKING.PROTECTED_OBJECTS.UNLOCK_ATLP] Masked (first) HP activation");
--                          Put_Line ("[SYSTEM.TASKING.PROTECTED_OBJECTS.UNLOCK_ATLP] " &
--                           "Setting next alarm to now + " & Integer'Image (Old_Alarm.Timestamp +
--                                Running_Thread.NPR_Length - Time_From_Hyp_Start));

                        Peripherals.Set_Alarm
                       (Peripherals.Timer_Interval
                          ((Alarms_Queue (Current_Alarm).Timestamp +
                             Running_Thread.NPR_Length - Time_From_Hyp_Start) * Peripherals.Clock_Freq_Hz));

                     else
                        --  other HP alarms
--                          Put_Line ("[SYSTEM.TASKING.PROTECTED_OBJECTS.UNLOCK_ATLP] Masked (other) HP activation");
                        null;
                     end if;

                  else
                     -- LP activation: go on
--                       Put_Line ("[SYSTEM.TASKING.PROTECTED_OBJECTS.UNLOCK_ATLP] Masked LP activation");

                     if not Found_HP_Alarm then
                     --  withdraw NPR
                        Threads.Queues.Next_Alarm := Activation;
                        Peripherals.Set_Alarm
                          (Peripherals.Timer_Interval
                             ((Alarms_Queue (Current_Alarm).Timestamp +
                                Integer(Alarms_Queue (Current_Alarm).Interval) - Time_From_Hyp_Start) * Peripherals.Clock_Freq_Hz));
                     end if;
                  end if;
                  Threads.Queues.Update_Ready_Queue (Alarms_Queue (Current_Alarm));
                  Current_Alarm := Alarms_Queue (Current_Alarm).Next;
               end loop;
            else
               --  no masked alarms
               --  withdraw NPR
--                 Put_Line ("[SYSTEM.TASKING.PROTECTED_OBJECTS.UNLOCK_ATLP] No Masked activation");
               Threads.Queues.Next_Alarm := Activation;
               Tmp_Timestamp := Alarms_Queue (Current_Alarm).Timestamp - Time_From_Hyp_Start;
--                 Put_Line ("[SYSTEM.TASKING.PROTECTED_OBJECTS.UNLOCK_ATLP] " &
--                             "Setting next alarm to now + " & Integer'Image (Tmp_Timestamp));
               Peripherals.Set_Alarm
                 (Peripherals.Timer_Interval
                    (Tmp_Timestamp * Peripherals.Clock_Freq_Hz));
            end if;
--              null;

         when C =>
            --  Case C never locks the resource, thus never unlocks it either
            pragma Assert (False);
      end case;

      System.BB.CPU_Primitives.Enable_Interrupts (0);

   end Unlock_Activation_Triggered_LP_Sched;

   ------------------------------------------
   -- Unlock_Model_LP_Sched --
   ------------------------------------------

   procedure Unlock_Model_LP_Sched (Object : Protection_Access) is
      use Threads.Queues;
      use System.BB.Peripherals;
      use System.BB.Protection;
      Self_Id : constant Task_Id := Self;

      --  COMMENT FOR MODEL-SOLUTION
      Current_TB : Peripherals.Timer_Interval;
      Tmp_Timestamp : Integer;

   begin

      System.BB.Protection.Enter_Kernel;
      Current_TB := Peripherals.Read_Clock;

--              Put_Line ("[SYSTEM.TASKING.PROTECTED_OBJECTS.UNLOCK_MOLP] (Time " &
--                  Integer'Image (Time_From_Hyp_Start +
--                  (Integer (Current_TB - Last_Read_TB) / Peripherals.Clock_Freq_Hz)) &
--                          ") Unlock ");


      pragma Assert (Self_Id.Common.Protected_Action_Nesting > 0
                     and then Object.Owner = Self_Id);

      --  Remove ownership of the protected object

      Object.Owner := Null_Task;

      --  We are exiting from a protected action, so that we decrease the
      --  protected object nesting level.

      Self_Id.Common.Protected_Action_Nesting :=
        Self_Id.Common.Protected_Action_Nesting - 1;

      Set_Priority (Self_Id, Object.Caller_Priority);

      case Critical_Section_Type is
         when A =>
            System.BB.CPU_Primitives.Enable_Interrupts (0);

         when B | C =>
            Time_From_Hyp_Start := Time_From_Hyp_Start +
              (Integer (Current_TB - Last_Read_TB) / Peripherals.Clock_Freq_Hz);
            Last_Read_TB := Current_TB;

            --  TODO: check if there are alarms to recover (might not be the case with MORE than one CS within
            --  the same NPR)
            --  in NPR, restore missed activations: JITTERY PART
            while Alarms_Queue (Current_Alarm).Timestamp <= Time_From_Hyp_Start loop
--                 Put_Line ("U" & Integer'Image (Alarms_Queue (Current_Alarm).Timestamp));
               Threads.Queues.Update_Ready_Queue (Alarms_Queue (Current_Alarm));
               Current_Alarm := Alarms_Queue (Current_Alarm).Next;
            end loop;

            Tmp_Timestamp := Alarms_Queue (Current_Alarm).Timestamp - Time_From_Hyp_Start;
            Threads.Queues.Next_Alarm := Activation;
            Peripherals.Set_Alarm
              (Peripherals.Timer_Interval
                 (Tmp_Timestamp * Peripherals.Clock_Freq_Hz));
            System.BB.Protection.Leave_Kernel;
      end case;

   end Unlock_Model_LP_Sched;

begin
   --  Ensure that tasking is initialized when using protected objects

   Tasking.Initialize;
end System.Tasking.Protected_Objects;
