------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . T H R E A D S . Q U E U E S            --
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

--  pragma Restrictions (No_Elaboration_Code);

with Interfaces; use Interfaces;

with Ada.Unchecked_Conversion;

with System.BB.Serial_Output;
use System.BB.Serial_Output;
with System.Machine_Code; use System.Machine_Code;

package body System.BB.Threads.Queues is

   use type System.BB.Time.Time;

   function To_Integer is new
      Ada.Unchecked_Conversion
         (System.BB.Threads.Thread_Id, System.Address);

   ----------------
   -- Local data --
   ----------------

   Head_Alarm_Id : Thread_Id := Null_Thread_Id;
   pragma Volatile (Head_Alarm_Id);
   --  Identifier of the thread that is in the first place of the alarm queue

   Head_TE_Alarm_Id : TE_Alarm_Queue_Id := Null_TE_Alarm_Queue_Id;
   pragma Volatile (Head_TE_Alarm_Id);

   ----------------------
   -- Local procedures --
   ----------------------

   Debruijn_Number : constant Unsigned_32 := 124511785;

   dBIndex : constant array (0 .. 31) of Integer :=
     (0, 1, 23, 2, 29, 24, 19, 3,
     30, 27, 25, 11, 20, 8, 4, 13,
     31, 22, 28, 18, 26, 10, 7, 12,
     21, 17, 9, 6, 16, 5, 15, 14);

   ------------------------
   -- Get_Index_DeBruijn --
   ------------------------

   function Get_Index_DeBruijn (Bitmask : Unsigned_32) return Integer
   is
      Local_Bitmask : Unsigned_32;
      Index : Integer := 0;
      Temp : Unsigned_64;

   begin
      Local_Bitmask := Bitmask and (-Bitmask);
      Temp := Unsigned_64 (Local_Bitmask * Debruijn_Number);
      Index := Integer (Shift_Right (Temp, 27));
      return dBIndex (Index);
   end Get_Index_DeBruijn;

   ----------------------
   -- Get_First_Thread --
   ----------------------

   function Get_First_Thread (Root_Bitmask     : Interfaces.Unsigned_32;
                              Child_Bitmasks   : Ready_Child_Bitmasks_Type;
                              Interrupt_Masked : Boolean)
                              return Thread_Id
   is
      Bitmask_Index : Integer range 0 .. 7;
      Priority_Level : System.Any_Priority;
      Tmp_Bitmask : Interfaces.Unsigned_32 := 0;
      Tmp_Root : Interfaces.Unsigned_32 := Root_Bitmask;
      Tmp_Child : Ready_Child_Bitmasks_Type := Child_Bitmasks;

   begin

      if Interrupt_Masked then
         --  Save interrupt priorities state in Tmp_Bitmask
         Tmp_Bitmask := Tmp_Child (7) and
           2#00000000000000000000001111111111#;

         --  Erase active interrupt priorities
         Tmp_Child (7) :=
           Tmp_Child (7) and 2#11111111111111111111110000000000#;

         if Tmp_Child (7) = 0 then
            Tmp_Root := Tmp_Root and
              not (2#00000000000000000000000000000001#);
         else
            --  do nothing, just to get constant-time behaviour
            Tmp_Root := Tmp_Root or
              2#00000000000000000000000000000001#;
         end if;
      end if;

      --  Get the index of the bitmask containing the first one (i.e. the bit
      --  corresponding to the priority level of the ready thread with the
      --  highest (active) priority

      Bitmask_Index := 7 - Get_Index_DeBruijn (Tmp_Root);

      --  Get the index of the bit corresponding to the priority level of the
      --  ready thread with the highest active priority

      Priority_Level := (32 * Bitmask_Index) +
        (31 - Get_Index_DeBruijn (Tmp_Child (Bitmask_Index)));

      --  tmp := Priorities_Threads_Mapping (Priority_Level);
--        Put_Line ("[SYSTEM.BB.THREADS.QUEUES.GET_FIRST_THREAD] " &
--          "DeBruijn has selected priority " &
--                    System.Any_Priority'Image (Priority_Level) &
--                    " corresponding to task " &
--                    System.Any_Priority'Image
--            (Priorities_Threads_Mapping
--               (Priority_Level).Base_Priority));

      --  AB
      pragma Assert (Priorities_Threads_Mapping (Priority_Level).all.
                       State = Threads.Delayed);
      Priorities_Threads_Mapping (Priority_Level).all.
        State := Threads.Runnable;

      if Interrupt_Masked then
         --  Restore active interrupt priorities
         Tmp_Child (7) :=
           Tmp_Child (7) or Tmp_Bitmask;

         if Tmp_Child (7) = 0 then
            Tmp_Root := Tmp_Root and
              not (2#00000000000000000000000000000001#);
         else
            --  do nothing, just to get constant-time behaviour
            Tmp_Root := Tmp_Root or
              2#00000000000000000000000000000001#;
         end if;
      end if;

      --  Return the thread that has active priority equal to Active_Priority
      return Priorities_Threads_Mapping (Priority_Level);
   end Get_First_Thread;

   ---------------------
   -- Change_Priority --
   ---------------------

   procedure Change_Priority
     (Thread   : Thread_Id;
      Priority : System.Any_Priority)
   is
      Bitmask_Index : Integer range 0 .. 7;
      Position : Integer range Any_Priority'Range;

   begin

      --  We can only change the priority of the thread that is
      --  currently executing.

      pragma Assert (Thread /= Running_Thread);
--        Put_Line ("[SYSTEM.BB.THREADS.QUEUES.CHANGE_PRIORITY] " &
--                    "Changing priority of thread " &
--                    System.Any_Priority'Image (Thread.Active_Priority) &
--                    " to" &
--                    System.Any_Priority'Image (Priority)
--                 );

      --  Get the index of the child bitmask corresponding to the current
      --  active priority level

      Bitmask_Index := Integer (Shift_Right (
        Unsigned_32 (Thread.Active_Priority), 5));

      --  Get the position of the current priority level in the child bitmask;
      --  note that position starts from the left

      Position := Thread.Active_Priority - (32 * Bitmask_Index);

      --  Set to zero the bit corresponding to the current priority level in
      --  the child bitmask

      Ready_Child_Bitmasks (Bitmask_Index) :=
        Ready_Child_Bitmasks (Bitmask_Index) and
          (not (Shift_Right (2#10000000000000000000000000000000#, Position)));

      --  If the child bitmask is now 0, i.e. no ready task is present anymore
      --  set 0 in the corresponing bit in the root-bitmask

      --  AG!!!!!!
--        Ready_Root_Bitmask := Ready_Root_Bitmask and
--          (not (Shift_Left (((Shift_Right
--        ((Ready_Child_Bitmasks (Bitmask_Index)
--            or ((not (Ready_Child_Bitmasks (Bitmask_Index))) + 1)), 31))+1)
--              and 1, 7 - Bitmask_Index)));

      --  AB rewriting
      if Ready_Child_Bitmasks (Bitmask_Index) = 0 then
         Ready_Root_Bitmask := Ready_Root_Bitmask and
           not (Shift_Right (2#00000000000000000000000010000000#,
                             Bitmask_Index));
      else
         --  do nothing, just to have constant-time behaviour
         Ready_Root_Bitmask := Ready_Root_Bitmask and
           not (Shift_Right (2#00000000000000000000000000000000#,
                             Bitmask_Index));
      end if;

      --  Update the priorities-threads mapping

      Priorities_Threads_Mapping (Thread.Active_Priority) := Null_Thread_Id;
      Priorities_Threads_Mapping (Priority) := Thread;

      --  Get the index of the child bitmask corresponding to the new
      --  priority level

      Bitmask_Index := Integer (Shift_Right (Unsigned_32 (Priority), 5));

      --  Get the position of the current priority level in the child bitmask;
      --  note that position starts from the left

      Position := Priority - (32 * Bitmask_Index);

      --  Set to one the bit corresponding to the new priority level in the
      --  child bitmask

      Ready_Child_Bitmasks (Bitmask_Index) :=
        Ready_Child_Bitmasks (Bitmask_Index) or
          Shift_Right (2#10000000000000000000000000000000#, Position);

      --  Now there is at least one bit set to one in the child bitmask so set
      --  to one the bit corresponding to the child bitmask in the root bitmask

      Ready_Root_Bitmask := Ready_Root_Bitmask or
        Shift_Right (2#00000000000000000000000010000000#, Bitmask_Index);

      --  Update the First_Thread value, getting the (possibly new) highest
      --  priority ready task

      First_Thread := Get_First_Thread (Ready_Root_Bitmask,
                                        Ready_Child_Bitmasks,
                                        False);

      --  Change the active priority of the thread

      Thread.Active_Priority := Priority;
   end Change_Priority;

   -------------
   -- Extract --
   -------------

   procedure Extract (Thread : Thread_Id) is
      Bitmask_Index : Integer range 0 .. 7;
      Position : Integer range Any_Priority'Range;
   begin
      --  The only thread that can be extracted from the ready list is
      --  the one that is currently executing (as a result of a delay
      --  or a protected operation).

      pragma Assert
        (Thread = Running_Thread
          and then Thread = First_Thread
         and then Thread.State /= Runnable);

--   Put_Line ("[SYSTEM.BB.THREADS.QUEUES.EXTRACT] Extracting at priority " &
--            System.Any_Priority'Image (Thread.Active_Priority) &
--            " corresponding to task " &
--            System.Any_Priority'Image
--            (Priorities_Threads_Mapping
--               (Thread.Active_Priority).Base_Priority));

      --  Get the child bitmask index

      Bitmask_Index := Integer (Shift_Right (
        Unsigned_32 (Thread.Active_Priority), 5));

      --  Get the position of the thread priority level in the child bitmask;
      --  note that position starts from the left

      Position := Thread.Active_Priority - (32 * Bitmask_Index);

      --  Set to zero the bit corresponding to the priority level in the child
      --  bitmask

      Ready_Child_Bitmasks (Bitmask_Index) :=
        Ready_Child_Bitmasks (Bitmask_Index) and
          (not (Shift_Right (2#10000000000000000000000000000000#, Position)));

      --  If the child-bitmask is now 0, i.e. no ready task is present anymore
      --  set 0 in the corresponing bit in the root-bitmask

      Ready_Root_Bitmask := Ready_Root_Bitmask and
        (not (Shift_Left (((Shift_Right ((Ready_Child_Bitmasks (Bitmask_Index)
          or ((not (Ready_Child_Bitmasks (Bitmask_Index))) + 1)), 31)) + 1)
            and 1, 7 - Bitmask_Index)));

      --  Update the First_Thread value, getting the (possibly new) highest
      --  priority ready task

      First_Thread := Get_First_Thread (Ready_Root_Bitmask,
                                        Ready_Child_Bitmasks,
                                        False);

   end Extract;

   -------------------------
   -- Extract_First_Alarm --
   -------------------------

   function Extract_First_Alarm return Thread_Id is
      Result : constant Thread_Id := Head_Alarm_Id;
   begin
      pragma Assert (Result.State = Delayed);

      Head_Alarm_Id := Head_Alarm_Id.Next_Alarm;
      Result.Alarm_Time := System.BB.Time.Time'Last;
      Result.Next_Alarm := Null_Thread_Id;
      return Result;
   end Extract_First_Alarm;

   -------------------------
   -- Get_Next_Alarm_Time --
   -------------------------

   function Get_Next_Alarm_Time return System.BB.Time.Time is
      Aux_Time : System.BB.Time.Time;
   begin
      --  Aux_Time will be the earliest alarm of delays queue and
      --  timing events queue
      if Head_Alarm_Id = Null_Thread_Id then
         if Head_TE_Alarm_Id = Null_TE_Alarm_Queue_Id then
            --  If alarm queues are empty then next alarm to raise
            --  will be Time'Last
            Aux_Time := System.BB.Time.Time'Last;
         else
            Aux_Time := Head_TE_Alarm_Id.Alarm_Time;
         end if;
      else
         if Head_TE_Alarm_Id = Null_TE_Alarm_Queue_Id then
            Aux_Time := Head_Alarm_Id.Alarm_Time;
         else
            if Head_TE_Alarm_Id.Alarm_Time < Head_Alarm_Id.Alarm_Time then
               Aux_Time := Head_TE_Alarm_Id.Alarm_Time;
            else
               Aux_Time := Head_Alarm_Id.Alarm_Time;
            end if;
         end if;
      end if;

      --  Compare Aux_Time with timers and group-budget alarms. The earliest of
      --  all of them will be the next alarm.
      if Aux_Time < TM_Alarm  and Aux_Time < GB_Alarm then
         return Aux_Time;
      else
         if TM_Alarm < GB_Alarm then
            return TM_Alarm;
         else
            return GB_Alarm;
         end if;
      end if;

   end Get_Next_Alarm_Time;

   -------------------------
   -- Get_Next_Delay_Time --
   -------------------------

   function Get_Next_Delay_Time return System.BB.Time.Time is

   begin
      if Head_Alarm_Id = Null_Thread_Id then
         --  If alarm queue is empty then next alarm to raise will be Time'Last

         return System.BB.Time.Time'Last;
      else
         return Head_Alarm_Id.Alarm_Time;
      end if;

   end Get_Next_Delay_Time;

   -------------------------
   -- Check_Deadline_Miss --
   -------------------------

   procedure Check_Deadline_Miss (The_Alaram : Alarm) is
      use Threads;
      use Threads.Queues;
--        use Interfaces;
      Tmp_Mask : Interfaces.Unsigned_32 := 0;
      Bitmask_Index : constant Integer :=
        Integer (Shift_Right (Unsigned_32 (Running_Thread.Base_Priority), 5));
      Position : constant Integer :=
        Running_Thread.Base_Priority - (32 * Bitmask_Index);
   begin

      --  ANY TASK
      for i in 0 .. 7 loop
         Tmp_Mask := Ready_Child_Bitmasks (i) and
           The_Alaram.Alarm_Child_Bitmasks (i);
         if Tmp_Mask > 0 then
            Asm ("nop", Volatile => True);
            Put_Line (Integer'Image (i) &
                        "-" & Interfaces.Unsigned_32'Image
                        (Ready_Child_Bitmasks (i)) &
                        "-" & Interfaces.Unsigned_32'Image
                        (The_Alaram.Alarm_Child_Bitmasks (i)));
            Put ("DEADLINE MISS (A) at time " &
                   Integer'Image (Time_From_Hyp_Start));
            Put_Line (" for task at priority " &
                Integer'Image ((i * 32) +
                (31 - Queues.Get_Index_DeBruijn (Tmp_Mask))));
         end if;
      end loop;

      --  CURRENT TASK (EXECUTING AT INTERRUPT PRIORITY)
      Tmp_Mask :=
        The_Alaram.Alarm_Child_Bitmasks (Bitmask_Index)
        and Shift_Right (2#10000000000000000000000000000000#, Position);
      if  Tmp_Mask > 0 then
         Asm ("nop", Volatile => True);
         Put ("DEADLINE MISS (B) at time " &
                Integer'Image (Time_From_Hyp_Start));
         Put_Line (" for task at priority " &
                     System.Any_Priority'Image (Running_Thread.Base_Priority));
      end if;

   end Check_Deadline_Miss;

   ------------------------
   -- Update_Ready_Queue --
   ------------------------

   procedure Update_Ready_Queue (time : Alarm) is
   begin
      Check_Deadline_Miss (time);
      for i in 0 .. 7 loop
         Ready_Child_Bitmasks (i) :=
           Ready_Child_Bitmasks (i) or time.Alarm_Child_Bitmasks (i);
      end loop;

      Ready_Root_Bitmask := Ready_Root_Bitmask or time.Alarm_Root_Bitmask;

      First_Thread := Get_First_Thread (Ready_Root_Bitmask,
                                        Ready_Child_Bitmasks,
                                        False);

      Idle_Is_Running := False;

   end Update_Ready_Queue;

   -----------------
   -- Print_Masks --
   -----------------

   procedure Print_Masks is
   begin
--        Put_Line ("[SYSTEM.BB.THREADS.QUEUES.PRINT_MASKS] " &
--                    "Ready_Root_Bitmask is " &
--                    Interfaces.Unsigned_32'Image (Ready_Root_Bitmask));
--        Put ("[SYSTEM.BB.THREADS.QUEUES.PRINT_MASKS] " &
--            "Ready_Child_Bitmasks are ");
      for i in 0 .. 7 loop
         Put (Interfaces.Unsigned_32'Image
              (Threads.Queues.Ready_Child_Bitmasks (i)) & " ");
      end loop;
--        Put_Line ("");
   end Print_Masks;

   function Get_Context_Switches return Integer is
   begin
      return Nr_Context_Switches;
   end Get_Context_Switches;

   ---------------------
   -- Insert_In_Queue --
   ---------------------

   procedure Insert_In_Queue (Thread : Thread_Id) is
      Bitmask_Index : Integer range 0 .. 7;
      Position : Integer range Any_Priority'Range;
   begin

--        Put_Line ("[SYSTEM.BB.THREADS.QUEUES.INSERT_IN_QUEUE] " &
--                    "Inserting in queue thread at priority " &
--                    System.Any_Priority'Image (Thread.Active_Priority));

      --  Get the index of the child bitmask containing the priority level of
      --  the thread
      --  AB: this is like ceil(priority/32)
      Bitmask_Index := Integer (Shift_Right (
        Unsigned_32 (Thread.Active_Priority), 5));

      --  Get the position of the thread priority level in the child bitmask;
      --  note that position starts from the left

      Position := Thread.Active_Priority - (32 * Bitmask_Index);

      --  Set to one the bit corresponding to the priority level in the child
      --  bitmask

      Ready_Child_Bitmasks (Bitmask_Index) :=
        Ready_Child_Bitmasks (Bitmask_Index) or
          Shift_Right (2#10000000000000000000000000000000#, Position);

      --  Now there is at least one bit set to one in the child bitmask so set
      --  to one the bit corresponding to the child bitmask in the root bitmask

      Ready_Root_Bitmask := Ready_Root_Bitmask or
        Shift_Right (2#00000000000000000000000010000000#, Bitmask_Index);

      --  Update the First_Thread value, getting the (possibly new) highest
      --  priority ready task

      First_Thread := Get_First_Thread (Ready_Root_Bitmask,
                                        Ready_Child_Bitmasks,
                                        False);
   end Insert_In_Queue;

   ------------
   -- Insert --
   ------------

   procedure Insert (Thread : Thread_Id) is
   begin
      --  Insert the thread in the ready queue; if the idle thread was running
      --  now it is not running anymore

      Insert_In_Queue (Thread);
      Idle_Is_Running := False;
   end Insert;

   ------------------
   -- Insert_Alarm --
   ------------------

   procedure Insert_Alarm
     (T        : System.BB.Time.Time;
      Thread   : Thread_Id;
      Is_First : out Boolean)
   is
      Alarm_Id_Aux : Thread_Id;

   begin
      --  We can only insert in the alarm queue threads whose state is Delayed

      pragma Assert (Thread.State = Delayed);

      --  Set the Alarm_Time within the thread descriptor

      Thread.Alarm_Time := T;

      if Head_Alarm_Id = Null_Thread_Id or else
        T < Head_Alarm_Id.Alarm_Time
      then
         --  The thread is inserted as first because either the queue is empty
         --  or the new alarm expires earlier.

         Thread.Next_Alarm := Head_Alarm_Id;
         Head_Alarm_Id := Thread;
         if T <= Get_Next_Alarm_Time then
            Is_First := True;
         else
            Is_First := False;
         end if;
      else
         --  Place in the middle

         Alarm_Id_Aux := Head_Alarm_Id;

         --  Find the minimum greater than T alarm within the alarm queue
         --  only if it is a delayed task and turn the alarms flags to false

         if Thread.State = Delayed then
            while Alarm_Id_Aux.Next_Alarm /= Null_Thread_Id and then
            Alarm_Id_Aux.Next_Alarm.Alarm_Time < T
            loop
               Alarm_Id_Aux := Alarm_Id_Aux.Next_Alarm;
            end loop;
            Thread.Next_Alarm := Alarm_Id_Aux.Next_Alarm;
            Alarm_Id_Aux.Next_Alarm := Thread;
            Is_First := False;
         end if;
      end if;

      --  Alarm queues have been modified, so the mechanism have to be
      --  updated.
      Update_Alarms;

   end Insert_Alarm;

   procedure Create_Alarms is
      pragma Style_Checks (Off);
      --        Current_Index : Integer := 0;
      Bitmask_Index : Integer;
      Position : Integer;
--        Previous_Alarm : Integer := -1;
      Current_Timestamp : Integer;
      The_Priority : Priority_Range;
   begin
      for Prio in Tasks_Periods'Range loop

         The_Priority := Prio;
         Current_Timestamp := Tasks_Phases (The_Priority);
         Bitmask_Index := Integer (Shift_Right (Unsigned_32 (The_Priority), 5));
      Position := Integer(The_Priority) - (32 * Bitmask_Index);

         while Current_Timestamp < Hyperperiod loop

--              Put_Line ("current timestamp = " & Integer'Image (Current_Timestamp));
--              Previous_Alarm := -1;
            --Put_Line ("i = " & Integer'Range (i));

               if Alarms_Queue (Current_Timestamp).Alarm_Root_Bitmask /= 0 then
               -- put in the same mask
--                    Put_Line ("Adding to mask ");
                  Alarms_Queue (Current_Timestamp).Alarm_Child_Bitmasks (Bitmask_Index) :=
                    Alarms_Queue (Current_Timestamp).Alarm_Child_Bitmasks (Bitmask_Index) or
                    Shift_Right (2#10000000000000000000000000000000#, Position);
                  Alarms_Queue (Current_Timestamp).Alarm_Root_Bitmask :=
                    Alarms_Queue (Current_Timestamp).Alarm_Root_Bitmask or
                 Shift_Right (2#00000000000000000000000010000000#, Bitmask_Index);
--                 Put_Line ("Resulting child mask: " &
--                             Interfaces.Unsigned_32'Image
--                             (Alarms_Queue (Current_Timestamp).Alarm_Child_Bitmasks (Bitmask_Index)));



               else
--                 Put_Line ("Creating node ");
                  --  create new node
--                    Alarms_Queue (Current_Index).Interval := 0;
                  Alarms_Queue (Current_Timestamp).Timestamp := Current_Timestamp;
--                    Alarms_Queue (i).Next := -1;
                  Alarms_Queue (Current_Timestamp).Thread := Tasks_Ids (The_Priority);
                  --  Alarms_Queue (i).Event_Type := Activation;


                  --Alarms_Queue (Previous_Alarm).Next := Current_Index;
                  --Alarms_Queue (Previous_Alarm).Interval := Peripherals.Timer_Interval(
                  --  Alarms_Queue (i).Timestamp - Alarms_Queue (Previous_Alarm).Timestamp);

                  --  AB
                  Alarms_Queue (Current_Timestamp).Alarm_Child_Bitmasks (Bitmask_Index) :=
              Shift_Right (2#10000000000000000000000000000000#, Position);
                  Alarms_Queue (Current_Timestamp).Alarm_Root_Bitmask :=
                 Shift_Right (2#00000000000000000000000010000000#, Bitmask_Index);
--                 Put_Line ("Resulting child mask: " &
--                             Interfaces.Unsigned_32'Image
--                             (Alarms_Queue (Current_Timestamp).Alarm_Child_Bitmasks (Bitmask_Index)));

                  --Print_Node (Current_Index);
                  --Current_Index := Current_Index + 1;

               end if;


            Current_Timestamp := Current_Timestamp + Tasks_Periods (The_Priority);
         end loop; --  while
    end loop;

      declare
         Index : Integer := 0;
         Previous_Index : Integer := 0;
         F_Alarm : Integer;
         L_Alarm : Integer;
         First_Alarm_Found : Boolean := False;
--           i : Integer;
         --Current_Timestamp : Integer  := Alarms_Queue (Index).Timestamp;
      begin
         while Index < Hyperperiod loop
            --              Current_Timestamp := Alarms_Queue (Index).Timestamp;
--              Put_Line ("Index " & Integer'Image (Index));
            if Alarms_Queue (Index).Alarm_Root_Bitmask /= 0 then
               if Alarms_Queue (Previous_Index).Alarm_Root_Bitmask /= 0 then
--                    Put_Line ("Previous bitmask is NOT 0");
--                    Put_Line ("Current Index is " & Integer'Image (Index));
--                    Put_Line ("Previous Index is " & Integer'Image (Previous_Index));
                  Alarms_Queue (Previous_Index).Next := Alarms_Queue (Index).Timestamp;
                  Alarms_Queue (Previous_Index).Interval :=
                    Peripherals.Timer_Interval(Alarms_Queue (Index).Timestamp -
                                                 Alarms_Queue (Previous_Index).Timestamp);

               end if;
               if not First_Alarm_Found then
                  First_Alarm_Found := True;
                  F_Alarm := Index;
               end if;

--                 Print_Node (Alarms_Queue (Index).Timestamp);
               Previous_Index := Index;
            end if;
            Index := Index + 1;

         end loop;
         L_Alarm := Previous_Index;
         Alarms_Queue (L_Alarm).Next := F_Alarm;
         Alarms_Queue (L_Alarm).Interval := Peripherals.Timer_Interval
           ((Hyperperiod + Alarms_Queue (F_Alarm).Timestamp) - Alarms_Queue (L_Alarm).Timestamp);
         --           Print_Node (Alarms_Queue (Previous_Index).Timestamp);
--           i := Alarms_Queue (F_Alarm).Timestamp;
--           while i <= L_Alarm loop
--              Print_Node (i);
--              i := i + Integer(Alarms_Queue (i).Interval);
--              Put_Line ("********************************");
--           end loop;
--           Threads.Queues.First_Alarm := F_Alarm;
--           Threads.Queues.Last_Alarm := L_Alarm;
         Threads.Queues.Current_Alarm := 0;

      end;

      --  last alarm in list
--        Alarms_Queue (Current_Index).Next := 0;
--        Alarms_Queue (Current_Index).Interval := Peripherals.Timer_Interval(
--          Hyperperiod - Alarms_Queue (Current_Index).Timestamp);

   end Create_Alarms;

   -----------------------
   -- Add_Initial_Alarm --
   -----------------------

--     procedure Add_Initial_Alarm (Thread : Thread_Id;
--                                  Priority : System.Any_Priority) is
--        --  Aux_Alarm : Alarm_Access_Type;
--        Bitmask_Index : constant Integer :=
--          Integer (Shift_Right (Unsigned_32 (Priority), 5));
--        Position : constant Integer := Priority - (32 * Bitmask_Index);
--     begin
--  --        Put_Line ("Add_Initial_Alarm procedure " &
--  --                    Thread.all.Base_Priority'Img);
--
--        --  if Current_Alarm = null then
--        if Current_Index = 0 then
--  --           Put_Line ("Current_Alarm is null, create the first initial node");
--           --  there are no initial alarms yet, add the first one
--           Alarms_Queue (Current_Index).Interval := 0;
--           Alarms_Queue (Current_Index).Timestamp :=
--             Threads.Queues.Tasks_Phases (Priority_Range(Priority));
--           Alarms_Queue (Current_Index).Next := -1;
--           Alarms_Queue (Current_Index).Thread := Thread;
--           --  Alarms_Queue (Current_Index).Event_Type := Activation;
--
--           --  AB
--           Alarms_Queue (Current_Index).
--                Alarm_Child_Bitmasks (Bitmask_Index) :=
--                Alarms_Queue (Current_Index).
--                Alarm_Child_Bitmasks (Bitmask_Index) or
--                Shift_Right (2#10000000000000000000000000000000#, Position);
--              Alarms_Queue (Current_Index).Alarm_Root_Bitmask :=
--                Alarms_Queue (Current_Index).Alarm_Root_Bitmask or
--             Shift_Right (2#00000000000000000000000010000000#, Bitmask_Index);
--
--           Current_Alarm := Current_Index;
--           Last_Initial_Alarm := Current_Alarm;
--           Current_Index := Current_Index + 1;
--           --  Current_Alarm := new Alarm'(Interval => 0,
--           --                            Timestamp => 0,
--           --                            Next => null,
--           --                            Thread => Thread);
--           --  Put_Line ("After creation");
--           --  Last_Initial_Alarm := Current_Alarm;
--        else
--  --           Put_Line ("Current_Alarm is not null, create a new initial node");
--           --  there is at least one initial alarm, add the new initial
--           --  alarm to the head of the list
--
--           --  Alarms_Queue (Current_Index).Interval := 0;
--           --  Alarms_Queue (Current_Index).Timestamp := 0;
--           --  Alarms_Queue (Current_Index).Next := Current_Alarm;
--           --  Alarms_Queue (Current_Index).Thread := Thread;
--           --  Alarms_Queue (Current_Index).Event_Type := Activation;
--
--           --  AB
--           Alarms_Queue (Current_Alarm).
--                Alarm_Child_Bitmasks (Bitmask_Index) :=
--                Alarms_Queue (Current_Alarm).
--                Alarm_Child_Bitmasks (Bitmask_Index) or
--                Shift_Right (2#10000000000000000000000000000000#, Position);
--
--           Alarms_Queue (Current_Alarm).Alarm_Root_Bitmask :=
--                Alarms_Queue (Current_Alarm).Alarm_Root_Bitmask or
--             Shift_Right (2#00000000000000000000000010000000#, Bitmask_Index);
--
--           --  Current_Alarm := Current_Index;
--           --  Current_Index := Current_Index + 1;
--        end if;
--        Put_Line ("End Add_Initial_Alarm procedure");
--     end Add_Initial_Alarm;

   ----------------
   -- Add_Alarms --
   ----------------

--     procedure Add_Alarms (Thread : Thread_Id;
--                          -- Phase : Integer;
--                           Period : Integer;
--                           Priority : System.Any_Priority) is
--        Index : Integer := Period;
--        Aux_Index : Integer;
--        --  Aux_Alarm : Alarm_Access_Type;
--        --  New_Alarm : Alarm_Access_Type;
--        Bitmask_Index : constant Integer :=
--          Integer (Shift_Right (Unsigned_32 (Priority), 5));
--        Position : constant Integer := Priority - (32 * Bitmask_Index);
--     begin
--        Put_Line ("Add_Alarms procedure");
--
--        while Index <= Hyperperiod loop
--           --  if First_Alarm = null or else Index < First_Alarm.Timestamp then
--           if First_Alarm = -1 or else
--              Index < Alarms_Queue (First_Alarm).Timestamp then
--              --  insert the node as first (after the initial nodes)
--              --  we have to update the interval of the last initial node
--              --  setting it with the timespamp of the new node
--              --  Last_Initial_Alarm.Interval :=
--              --    Peripherals.Timer_Interval (Index);
--              Alarms_Queue (Last_Initial_Alarm).Interval :=
--                 Peripherals.Timer_Interval (Index);
--
--              --  create the new first alarm
--              --  Aux_Alarm := new Alarm'(Interval => 0,
--              --                        Timestamp => Index,
--              --                        Next => null,
--              --                        Thread => Thread);
--              Alarms_Queue (Current_Index).Interval := 0;
--              Alarms_Queue (Current_Index).Timestamp := Index;
--              Alarms_Queue (Current_Index).Next := -1;
--              Alarms_Queue (Current_Index).Thread := Thread;
--              --  Alarms_Queue (Current_Index).Event_Type := Activation;
--              Alarms_Queue (Current_Index).
--                Alarm_Child_Bitmasks (Bitmask_Index) :=
--                Alarms_Queue (Current_Index).
--                Alarm_Child_Bitmasks (Bitmask_Index) or
--                Shift_Right (2#10000000000000000000000000000000#, Position);
--              Alarms_Queue (Current_Index).Alarm_Root_Bitmask :=
--                Alarms_Queue (Current_Index).Alarm_Root_Bitmask or
--                Shift_Right (2#00000000000000000000000010000000#, Bitmask_Index);
--
--              --  if First_Alarm = null then
--              if First_Alarm = -1 then
--                 --  Aux_Alarm.Interval := Peripherals.Timer_Interval (Index);
--                 Alarms_Queue (Current_Index).Interval :=
--                    Peripherals.Timer_Interval (Index);
--                 --  Aux_Alarm.Next := Aux_Alarm;
--                 Alarms_Queue (Current_Index).Next := Current_Index;
--                 --  Last_Alarm := Aux_Alarm;
--                 Last_Alarm := Current_Index;
--              else
--                 --  Aux_Alarm.Interval :=
--                 --  Peripherals.Timer_Interval (First_Alarm.Timestamp - Index);
--                 Alarms_Queue (Current_Index).Interval :=
--                    Peripherals.Timer_Interval (
--                      Alarms_Queue (First_Alarm).Timestamp - Index);
--                 --  Aux_Alarm.Next := First_Alarm;
--                 Alarms_Queue (Current_Index).Next := First_Alarm;
--              end if;
--              --  First_Alarm := Aux_Alarm;
--              First_Alarm := Current_Index;
--
--              --  the first alarm has changed so we have to update
--              --  the next field of the next alarm
--              --  Last_Alarm.Next := First_Alarm;
--              --  AB: CIRCULAR LIST
--              Alarms_Queue (Last_Alarm).Next := First_Alarm;
--              --  Last_Alarm.Interval :=
--              --  Peripherals.Timer_Interval (First_Alarm.Timestamp);
--              Alarms_Queue (Last_Alarm).Interval :=
--                 Peripherals.Timer_Interval (
--                    Alarms_Queue (First_Alarm).Timestamp);
--              --  Last_Initial_Alarm.Next := First_Alarm;
--              Alarms_Queue (Last_Initial_Alarm).Next := First_Alarm;
--
--              Current_Index := Current_Index + 1;
--
--           else
--              --  insert the node in the middle of the list
--              --  so we have to find the position where to add the new node
--              --  Aux_Alarm := First_Alarm;
--              Aux_Index := First_Alarm;
--              --  while Aux_Alarm.Next /= First_Alarm and
--              --      Aux_Alarm.Next.Timestamp < Index loop
--              while Alarms_Queue (Aux_Index).Next /= First_Alarm and
--                 Alarms_Queue (Alarms_Queue (Aux_Index).Next).Timestamp
--                    < Index loop
--                 --  Aux_Alarm := Aux_Alarm.Next;
--                 Aux_Index := Alarms_Queue (Aux_Index).Next;
--              end loop;
--
--              if Alarms_Queue (Alarms_Queue (Aux_Index).Next).Timestamp
--                = Index then
--                 --  no new node to insert: update next node masks
--                 Alarms_Queue (Alarms_Queue (Aux_Index).Next).
--                   Alarm_Child_Bitmasks (Bitmask_Index) :=
--                   Alarms_Queue (Alarms_Queue (Aux_Index).Next).
--                   Alarm_Child_Bitmasks (Bitmask_Index) or
--                   Shift_Right (2#10000000000000000000000000000000#, Position);
--                 Alarms_Queue (Alarms_Queue (Aux_Index).Next).
--                   Alarm_Root_Bitmask :=
--                     Alarms_Queue (Alarms_Queue (Aux_Index).Next).
--                   Alarm_Root_Bitmask or
--                   Shift_Right (2#00000000000000000000000010000000#,
--                                Bitmask_Index);
--              else
--              --  timestamp > index => create new node
--
--              --  the new node has to be inserted just after Aux,
--              --  so we have to update the Aux_Alarm interval
--              --  Aux_Alarm.Interval :=
--              --  Peripherals.Timer_Interval (Index - Aux_Alarm.Timestamp);
--
--                 Alarms_Queue (Aux_Index).Interval :=
--                   Peripherals.Timer_Interval (Index -
--                   Alarms_Queue (Aux_Index).Timestamp);
--
--              --  New_Alarm := new Alarm'(Interval => 0,
--              --                        Timestamp => Index,
--              --                        Next => Aux_Alarm.Next,
--              --                        Thread => Thread);
--                 Alarms_Queue (Current_Index).Interval := 0;
--                 Alarms_Queue (Current_Index).Timestamp := Index;
--                 Alarms_Queue (Current_Index).Next :=
--                   Alarms_Queue (Aux_Index).Next;
--                 Alarms_Queue (Current_Index).Thread := Thread;
--                 --  Alarms_Queue (Current_Index).Event_Type := Activation;
--                 Alarms_Queue (Current_Index).
--                   Alarm_Child_Bitmasks (Bitmask_Index) :=
--                   Alarms_Queue (Current_Index).
--                   Alarm_Child_Bitmasks (Bitmask_Index) or
--                   Shift_Right (2#10000000000000000000000000000000#, Position);
--                 Alarms_Queue (Current_Index).Alarm_Root_Bitmask :=
--                   Alarms_Queue (Current_Index).Alarm_Root_Bitmask or
--                   Shift_Right (2#00000000000000000000000010000000#,
--                                Bitmask_Index);
--
--              --  if Aux_Alarm.Next = First_Alarm then
--
--                 if Alarms_Queue (Aux_Index).Next = First_Alarm then
--                    --  we are inserting the new node as the last one
--                    --  New_Alarm.Interval :=
--                    --  Peripherals.Timer_Interval (First_Alarm.Timestamp);
--
--                    Alarms_Queue (Current_Index).Interval :=
--                      Peripherals.Timer_Interval (
--                      Alarms_Queue (First_Alarm).Timestamp);
--                    --  Last_Alarm := New_Alarm;
--                    Last_Alarm := Current_Index;
--                 else
--                    --  New_Alarm.Interval :=
--                    --  Peripherals.Timer_Interval (Aux_Alarm.Next.Timestamp -
--                    --  Index);
--                    Alarms_Queue (Current_Index).Interval :=
--                      Peripherals.Timer_Interval (
--                      Alarms_Queue (Alarms_Queue (Aux_Index).Next).Timestamp -
--                      Index);
--                 end if;
--              --  Aux_Alarm.Next := New_Alarm;
--                 Alarms_Queue (Aux_Index).Next := Current_Index;
--
--                 Current_Index := Current_Index + 1;
--              end if;
--           end if;
--
--           --
--           Index := Index + Period;
--        end loop;
--  --        Put_Line ("End Add_Alarms procedure");
--     end Add_Alarms;

   procedure Print_Node (Current_Alarm : Integer) is
      Next_Thread : System.BB.Threads.Thread_Id;
   begin
      Put_Line ("[NODE " &
         To_Integer (Alarms_Queue (Current_Alarm).Thread)'Img &
            "] Timestamp = " &
                  Integer'Image (Alarms_Queue (Current_Alarm).Timestamp));
      Put_Line ("[NODE " &
                  To_Integer (Alarms_Queue (Current_Alarm).Thread)'Img &
                  "] Index in Alarm_Queue = " &
                  Integer'Image (Current_Alarm));
      Put_Line ("[NODE " &
         To_Integer (Alarms_Queue (Current_Alarm).Thread)'Img &
            "] Interval = " & System.BB.Peripherals.Timer_Interval'Image (
          Alarms_Queue (Current_Alarm).Interval));

      Put ("[NODE " &
         To_Integer (Alarms_Queue (Current_Alarm).Thread)'Img &
           "] Masks = ");
      for i in 0 .. 7 loop
         Put (Unsigned_32'Image (
           Alarms_Queue (Current_Alarm).Alarm_Child_Bitmasks (i)));
      end loop;
      Put_Line ("");

      if Alarms_Queue (Current_Alarm).Next /= -1 then
         Next_Thread :=
            Alarms_Queue (Alarms_Queue (Current_Alarm).Next).Thread;
      else
         Next_Thread := System.BB.Threads.Null_Thread_Id;
      end if;
      Put_Line ("[NODE " &
         To_Integer (Alarms_Queue (Current_Alarm).Thread)'Img &
            "] Next = " & To_Integer (Next_Thread)'Img);
   end Print_Node;

   procedure Print_List_From (Start_Alarm : Integer) is
      Current_Alarm : Integer := Start_Alarm;
      First_Alarm_Printed : Boolean := False;
   begin
      Put_Line ("The alarm list is:");
      Put_Line ("== START ==");
      while Current_Alarm /= -1 loop
         exit when Current_Alarm = First_Alarm and First_Alarm_Printed;

         Print_Node (Current_Alarm);
         Put_Line ("+++++++++++++++++++++++");

         if Current_Alarm = First_Alarm then
            First_Alarm_Printed := True;
         end if;
         Current_Alarm := Alarms_Queue (Current_Alarm).Next;
      end loop;
      Put_Line ("== END ==");
   end Print_List_From;

   -----------
   -- Yield --
   -----------

   procedure Yield (Thread : Thread_Id) is
      Prio        : constant Integer := Thread.Active_Priority;
      Aux_Pointer : Thread_Id;

   begin
      pragma Assert (Thread = Running_Thread
                       and then Thread = First_Thread
                       and then Thread.State = Runnable);

      if Thread.Next /= Null_Thread_Id
        and then Thread.Next.Active_Priority = Prio
      then
         First_Thread := Thread.Next;

         --  Look for the Aux_Pointer to insert the thread just after it

         Aux_Pointer  := First_Thread;
         while Aux_Pointer.Next /= Null_Thread_Id and then
           Prio = Aux_Pointer.Next.Active_Priority
         loop
            Aux_Pointer := Aux_Pointer.Next;
         end loop;

         --  Insert the thread after the Aux_Pointer

         Thread.Next := Aux_Pointer.Next;
         Aux_Pointer.Next := Thread;
      end if;
   end Yield;

   ---------------------
   -- Insert_TE_Alarm --
   ---------------------

   procedure Insert_TE_Alarm (At_Time  : System.BB.Time.Time;
                              TE_Alarm : TE_Alarm_Queue_Id) is
      TE_Alarm_Aux : TE_Alarm_Queue_Id;

   begin
      --  We insert the alarm in the Timing_Events queue
      if Head_TE_Alarm_Id = Null_TE_Alarm_Queue_Id or else
        Head_TE_Alarm_Id.Alarm_Time > At_Time then

         --  The thread is inserted as first because either the queue is empty
         --  or the new alarm expires earlier.
         --  First, we have to extract the alarm of the timer if it would have
         --  been set.

         TE_Alarm.Next_Alarm := Head_TE_Alarm_Id;
         if Head_TE_Alarm_Id /= Null_TE_Alarm_Queue_Id then
            Head_TE_Alarm_Id.Previous_Alarm := TE_Alarm;
         end if;
         Head_TE_Alarm_Id := TE_Alarm;
         Update_Alarms;
      else
         --  Place in the middle
         TE_Alarm_Aux := Head_TE_Alarm_Id;
         --  Find the minimum greater than At_Time in the queue
         while TE_Alarm_Aux.Next_Alarm /= Null_TE_Alarm_Queue_Id and then
         TE_Alarm_Aux.Next_Alarm.Alarm_Time < At_Time
         loop
            TE_Alarm_Aux := TE_Alarm_Aux.Next_Alarm;
         end loop;

         TE_Alarm.Previous_Alarm := TE_Alarm_Aux;
         TE_Alarm.Next_Alarm := TE_Alarm_Aux.Next_Alarm;
         if TE_Alarm_Aux.Next_Alarm /= Null_TE_Alarm_Queue_Id then
            TE_Alarm_Aux.Next_Alarm.Previous_Alarm := TE_Alarm;
         end if;
         TE_Alarm_Aux.Next_Alarm := TE_Alarm;

      end if;

   end Insert_TE_Alarm;

   -----------------
   --  Get_TE_Id  --
   -----------------

   procedure Get_TE_Id (Id : out Integer) is

   begin
      --  if a timing events expires, the alarm handler needs the id
      --  of the expiring object, but it is not accesible from the
      --  routine. So we use this procedure.
      if Head_TE_Alarm_Id /= Null_TE_Alarm_Queue_Id then
         Id := Head_TE_Alarm_Id.TE_Id;
      end if;
   end Get_TE_Id;

   ---------------------
   --  Update_Alarms  --
   ---------------------

   procedure Update_Alarms is
      First_Alarm : System.BB.Time.Time;
   begin
      --  First of all, we must put all the flags to false
      First_Alarm := Get_Next_Alarm_Time;
      Is_TE_Alarm := False;
      Is_Delay := False;
      System.BB.Threads.Queues.Running_Thread.Is_Timer_Alarm := False;
      System.BB.Threads.Queues.Running_Thread.Is_GB_Alarm := False;

      --  Find the minimum alarm and turn the correct flag to true, in order
      --  to allow the alarm handler know what alarm is going to expire.
      if Head_Alarm_Id /= Null_Thread_Id and then
        Head_Alarm_Id.Alarm_Time = First_Alarm then
         Is_Delay := True;
--           System.BB.Serial_Output.Put ("IS_DELAY_ALARM");
      else
         if Head_TE_Alarm_Id /= Null_TE_Alarm_Queue_Id and then
           Head_TE_Alarm_Id.Alarm_Time = First_Alarm then
            Is_TE_Alarm := True;
--              System.BB.Serial_Output.Put ("IS_TE_ALARM");
         else
            if TM_Alarm = First_Alarm and
              TM_Alarm /= System.BB.Time.Time'Last then
               System.BB.Threads.Queues.Running_Thread.
                 Is_Timer_Alarm := True;
--                 System.BB.Serial_Output.Put ("IS_TM_ALARM");
            else
               if GB_Alarm = First_Alarm and
                 GB_Alarm /= System.BB.Time.Time'Last then
                  System.BB.Threads.Queues.Running_Thread.Is_GB_Alarm := True;
--                    System.BB.Serial_Output.Put_Line ("IS_GB_ALARM");
               else
--                    System.BB.Serial_Output.Put ("IS_NO_ALARM");
                  null;
               end if;
            end if;
         end if;
      end if;
   end Update_Alarms;

   ------------------------
   --  Extract_TE_Alarm  --
   ------------------------

   procedure Extract_TE_Alarm (TE_Alarm : TE_Alarm_Queue_Id) is
   begin
      if TE_Alarm.Previous_Alarm /=
        System.BB.Threads.Null_TE_Alarm_Queue_Id then
         TE_Alarm.Previous_Alarm.Next_Alarm :=
           TE_Alarm.Next_Alarm;
      else
         Head_TE_Alarm_Id := TE_Alarm.Next_Alarm;
         Update_Alarms;
      end if;
      if TE_Alarm.Next_Alarm /=
        System.BB.Threads.Null_TE_Alarm_Queue_Id then
         TE_Alarm.Next_Alarm.Previous_Alarm :=
           TE_Alarm.Previous_Alarm;
      end if;
   end Extract_TE_Alarm;

   ------------------------------
   --  Extract_First_TE_Alarm  --
   ------------------------------

   procedure Extract_First_TE_Alarm is

   begin
      if Head_TE_Alarm_Id.Next_Alarm /= Null_TE_Alarm_Queue_Id then
         Head_TE_Alarm_Id := Head_TE_Alarm_Id.Next_Alarm;
         Head_TE_Alarm_Id.Previous_Alarm := Null_TE_Alarm_Queue_Id;
      else
         Head_TE_Alarm_Id := Null_TE_Alarm_Queue_Id;
      end if;
   end Extract_First_TE_Alarm;

   -----------------------
   --  Insert_TM_Alarm  --
   -----------------------

   procedure Insert_TM_Alarm (At_Time  : System.BB.Time.Time;
                              Is_First : out Boolean) is
   begin
      --  We insert the timer alarm in the one-sized queue.
      TM_Alarm := At_Time;
      if At_Time <= Get_Next_Alarm_Time then
         Is_First := True;
      else
         Is_First := False;
      end if;
      Update_Alarms;

   end Insert_TM_Alarm;

   -----------------------
   --  Insert_GB_Alarm  --
   -----------------------

   procedure Insert_GB_Alarm (At_Time  : System.BB.Time.Time;
                              Is_First : out Boolean) is
   begin
      --  We insert the group-budget alarm in the one-sized queue.
      GB_Alarm := At_Time;
      if At_Time <= Get_Next_Alarm_Time then
         Is_First := True;
      else
         Is_First := False;
      end if;
      Update_Alarms;

   end Insert_GB_Alarm;

end System.BB.Threads.Queues;
