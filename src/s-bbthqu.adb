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

pragma Restrictions (No_Elaboration_Code);

with System.BB.Peripherals;
with System.BB.Serial_Output;
use System.BB.Serial_Output;
with Ada.Unchecked_Conversion;

package body System.BB.Threads.Queues is

   use type System.BB.Time.Time;

   ----------------
   -- Local data --
   ----------------

   Head_Alarm_Id : Thread_Id := Null_Thread_Id;
   pragma Volatile (Head_Alarm_Id);
   --  Identifier of the thread that is in the first place of the alarm queue

   Head_TE_Alarm_Id : TE_Alarm_Queue_Id := Null_TE_Alarm_Queue_Id;
   pragma Volatile (Head_TE_Alarm_Id);

   --  AB
   function Get_Context_Switches return Integer is
   begin
      return Nr_Context_Switches;
   end Get_Context_Switches;

   ---------------------
   -- Change_Priority --
   ---------------------

   procedure Change_Priority
     (Thread   : Thread_Id;
      Priority : System.Any_Priority)
   is
      Aux_Pointer : Thread_Id;

   begin
      --  We can only change the priority of the thread that is
      --  currently executing.

      pragma Assert (Thread = Running_Thread);

      --  Change the active priority. The base priority does not change

      Thread.Active_Priority := Priority;

      --  When raising the priority, it is not possible that there is
      --  another task with a higher priority (otherwise the other task
      --  would be running). Hence, there is no displacement required within
      --  the queue, because the thread is already in the first position.

      if Thread.Next /= Null_Thread_Id
        and then Priority < Thread.Next.Active_Priority
      then
         --  If we are here it is because the currently executing
         --  thread is lowering its priority, and there is a thread
         --  with a higher priority ready to execute.

         --  The running thread is no longer the highest priority thread

         First_Thread := Thread.Next;

         Aux_Pointer := First_Thread;

         --  FIFO_Within_Priorities dispatching policy. In ALRM D.2.2 it
         --  is said that when the active priority is lowered due to the
         --  loss of inherited priority (the only possible case within the
         --  Ravenscar profile) the task is added at the head of the ready
         --  queue for its new active priority. Next loop will look
         --  for the value of Aux_Pointer that contains the last thread with
         --  a higher priority (so that we insert the thread just after it).

         while Aux_Pointer.Next /= Null_Thread_Id
           and then Priority < Aux_Pointer.Next.Active_Priority
         loop
            Aux_Pointer := Aux_Pointer.Next;
         end loop;

         --  Insert the thread just after the Aux_Pointer

         Thread.Next := Aux_Pointer.Next;
         Aux_Pointer.Next := Thread;
      end if;
   end Change_Priority;

   -------------
   -- Extract --
   -------------

   procedure Extract (Thread : Thread_Id) is
   begin
      --  The only thread that can be extracted from the ready list is
      --  the one that is currently executing (as a result of a delay
      --  or a protected operation).

      pragma Assert
        (Thread = Running_Thread
          and then Thread = First_Thread
          and then Thread.State /= Runnable);

      First_Thread := Thread.Next;
      Thread.Next := Null_Thread_Id;
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

   ------------
   -- Insert --
   ------------
--   function To_Integer is new
--     Ada.Unchecked_Conversion (Thread_Id, System.Address);

   procedure Insert (Thread : Thread_Id) is
      Aux_Pointer : Thread_Id;

   begin
      --  If may be the case that we try to insert a task that is already in
      --  the queue. It can only happen if the task was not runnable and its
      --  context was being used for handling an interrupt. Hence, if the task
      --  is already in the queue we do nothing.

      --  Insert at the head of queue if there is no other thread with a higher
      --  priority.

      if First_Thread = Null_Thread_Id
        or else Thread.Active_Priority > First_Thread.Active_Priority
      then
         --  Put_Line ("First: wake (" & Thread.Active_Priority'Img &
         --            ") - first (" & First_Thread.Active_Priority'Img & ")");
         Thread.Next := First_Thread;
         First_Thread := Thread;

         --  Middle or tail insertion

      else
         --  Put_Line ("Not first: wake (" & Thread.Active_Priority'Img &
         --            ") - first (" & First_Thread.Active_Priority'Img & ")");
         Aux_Pointer := First_Thread;

         --  Look for the Aux_Pointer to insert the thread just after it

         while Aux_Pointer /= Thread
           and then Aux_Pointer.Next /= Null_Thread_Id
           and then Aux_Pointer.Next.Active_Priority >= Thread.Active_Priority
         loop
            Aux_Pointer := Aux_Pointer.Next;
         end loop;

         --  Insert the thread after the Aux_Pointer, if needed

         if Aux_Pointer /= Thread then
            Thread.Next := Aux_Pointer.Next;
            Aux_Pointer.Next := Thread;
         end if;
      end if;
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
      else
         if Head_TE_Alarm_Id /= Null_TE_Alarm_Queue_Id and then
           Head_TE_Alarm_Id.Alarm_Time = First_Alarm then
            Is_TE_Alarm := True;
         else
            if TM_Alarm = First_Alarm and
              TM_Alarm /= System.BB.Time.Time'Last then
               System.BB.Threads.Queues.Running_Thread.
                 Is_Timer_Alarm := True;
            else
               if GB_Alarm = First_Alarm and
                 GB_Alarm /= System.BB.Time.Time'Last then
                  System.BB.Threads.Queues.Running_Thread.Is_GB_Alarm := True;
               else
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
