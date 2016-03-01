------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--      A D A . E X E C U T I O N _ T I M E . G R O U P _ B U D G E T S     --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 2007 Universidad Politecnica de Madrid            --
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

with Ada.Unchecked_Conversion;

with System.BB.Threads.Queues;
--  Used for First_Thread
--           Insert_Alarm
--           Get_Next_Time_Alarm

with System.BB.Time;
--  Used for Clock

with System.BB.Peripherals;
--  Used for Set_Alarm
--           Cancel_And_Set_Alarm

with System.Task_Primitives.Operations.Execution_Time_Support;
--  Used for Convert_Id

package body Ada.Execution_Time.Group_Budgets is

   use type System.BB.Time.Time;
   use type System.BB.Time.Time_Span;
   use type System.BB.Threads.Thread_Id;
   use type Ada.Task_Identification.Task_Id;
   use type Ada.Real_Time.Time_Span;

   --  type Clock_Periods is mod 2 ** 40;
   --  for Clock_Periods'Size use 40;
   --  --Values of this type represent number of times that the clock finishes
   --  --  its countdown.

   --  type Split_Time is
   --     record
   --        MSP : Clock_Periods;
   --        LSP : System.BB.Peripherals.Timer_Interval;
   --     end record;

   --  for Split_Time use
   --     record
   --        MSP at 0 range 0 .. 39;
   --        LSP at 5 range 0 .. 23;
   --     end record;

   --  for Split_Time'Size use 64;
   --  --  The type Split_Time represents a 64-bit time value, but it gives
   --  --  access to the two 40/24 bits parts separately.

   --  function To_Split_Time is new Ada.Unchecked_Conversion
   --    (System.BB.Time.Time, Split_Time);
   --  --  Function to change the view from Time (unsigned 64-bit) to
   --  --  Split_Time (a record with two unsigned 40/24 bits fields).

   function To_Timer_Interval is new Ada.Unchecked_Conversion
     (System.BB.Time.Time, System.BB.Peripherals.Timer_Interval);

   function To_Time_Span is new Ada.Unchecked_Conversion
     (Ada.Real_Time.Time_Span, System.BB.Time.Time_Span);
   --  Function to change the view between standard and internal
   --  Time_Span types

   function To_Time_Span is new Ada.Unchecked_Conversion
     (System.BB.Time.Time_Span, Ada.Real_Time.Time_Span);
   --  Function to change the view between internal and standard
   --  Time_Span types

   function To_Time is new Ada.Unchecked_Conversion
     (System.BB.Time.Time_Span, System.BB.Time.Time);
   --  Function to change the view from Time_Span (signed 63-bit) to
   --  Time (unsigned 64-bits).

   ----------------
   --  Add_Task  --
   ----------------

   procedure Add_Task (GB : in out Group_Budget;
                       T  : Ada.Task_Identification.Task_Id) is

      Th           : System.BB.Threads.Thread_Id;
      Exec_Handler : constant System.BB.Threads.Exec_Handler :=
        Execute_Handler'Access;
      Is_First     : Boolean;
      Now          : System.BB.Time.Time;

   begin

      Now := System.BB.Time.Clock;
      --  Convert the Task_Id type in the thread information type of the
      --  kernel
      Th := System.Task_Primitives.Operations
          .Execution_Time_Support.Convert_Id (T);

      if Is_A_Group_Member (T) then
         raise Group_Budget_Error;
      else

         if T = Ada.Task_Identification.Current_Task then
            --  The difference between the init of the execution and the add of
            --  the thread must be stored, if the task was running.
            System.BB.Threads.Queues.Running_Thread.Time_Diff_GB :=
              Now -
                System.BB.Threads.Queues.Running_Thread.Time_Init_Execution;
         end if;

         if GB.GB_Id = 0 then

            --  If it is the first time that the timer has set, we have to give
            --  it an identifier and initialize the array for the budget time
            GB.GB_Id := System.BB.Threads.Get_GB_Id;
            System.BB.Threads.Budget_Array (GB.GB_Id) :=
              To_Time_Span (Ada.Real_Time.Time_Span_Zero);
         end if;

         --  Group_Budget data are updated

         GB.Num_Members := GB.Num_Members + 1;
         GB.Array_Threads (GB.Num_Members) := Th;
         GB.Array_Task_Id (GB.Num_Members) := T;
         Th.GB_Id := GB.GB_Id;
         Th.GB_Index := GB.Num_Members;
         Copy_GB (GB, GB.GB_Id);
         --  Exec_Handler is the universal handler. It is used to get to
         --  specific handlers
         Th.Handler_GB := Exec_Handler;
      end if;

      if GB.Is_Set_Handler then

         --  if the handler was set, we have to insert the alarm in the queue
         System.BB.Threads.Queues.Insert_GB_Alarm
           (Now + System.BB.Threads.Budget_Array (GB.GB_Id), Is_First);
         Insert_Alarm_If_Needed
           (Is_First, Now + System.BB.Threads.Budget_Array (GB.GB_Id));
         System.BB.Threads.Queues.Update_Alarms;
      end if;

   end Add_Task;

   -------------------
   --  Remove_Task  --
   -------------------

   procedure Remove_Task (GB : in out Group_Budget;
                          T  : Ada.Task_Identification.Task_Id) is

      Th     : System.BB.Threads.Thread_Id;
      Th_Aux : System.BB.Threads.Thread_Id;
      T_Aux  : Ada.Task_Identification.Task_Id;

   begin

      --  Convert the Task_Id type in the thread information type of the
      --  kernel

      Th := System.Task_Primitives.Operations
        .Execution_Time_Support.Convert_Id (T);

      if not Is_Member (GB, T) then
         raise Group_Budget_Error;
      else

         if GB.Num_Members > 1 then

            --  If it was not the only thread in the group
            --  we have to update Group_Budget data

            Th_Aux := GB.Array_Threads (GB.Num_Members);
            T_Aux := GB.Array_Task_Id (GB.Num_Members);
            GB.Array_Threads (Th.GB_Index) := Th_Aux;
            GB.Array_Task_Id (Th.GB_Index) := T_Aux;
            Th_Aux.GB_Index := Th.GB_Index;

         end if;

         Th.GB_Index := 0;
         Th.GB_Id := 0;

         --  GB_Id = 0 indicates that the thread is no longer in any group
         GB.Num_Members := GB.Num_Members - 1;
         Copy_GB (GB, GB.GB_Id);
      end if;

      if T = Ada.Task_Identification.Current_Task and
        GB.Is_Set_Handler then

         --  if the handler was set, we have to extract the alarm of the queue
         System.BB.Threads.Queues.GB_Alarm := System.BB.Time.Time'Last;
         System.BB.Threads.Queues.Update_Alarms;
      end if;

   end Remove_Task;

   -----------------
   --  Is_Member  --
   -----------------

   function Is_Member (GB : Group_Budget;
                       T  : Ada.Task_Identification.Task_Id) return Boolean is

      Th : System.BB.Threads.Thread_Id;

   begin

      --  Convert the Task_Id type in the thread information type of the
      --  kernel
      Th := System.Task_Primitives.Operations
        .Execution_Time_Support.Convert_Id (T);

      --  The GB_Id field in Thread_Id record indicates if the thread
      --  is in a group or not, and it must coincide with the identifier
      --  of the Group_Budget
      if Th.GB_Id = GB.GB_Id and GB.GB_Id /= 0 then
         return True;
      else
         return False;
      end if;

   end Is_Member;

   -------------------------
   --  Is_A_Group_Member  --
   -------------------------

   function Is_A_Group_Member
     (T : Ada.Task_Identification.Task_Id) return Boolean is

      Th : System.BB.Threads.Thread_Id;

   begin
      --  Convert the Task_Id type in the thread information type of the
      --  kernel
      Th := System.Task_Primitives.Operations
          .Execution_Time_Support.Convert_Id (T);

      --  The GB_Id field in Thread_Id record indicates if the thread
      --  is in a group or not
      if Th.GB_Id = 0 then
         return False;
      else
         return True;
      end if;

   end Is_A_Group_Member;

   ---------------
   --  Members  --
   ---------------

   function Members (GB : Group_Budget) return Task_Array is

      T   : Task_Array (1 .. GB.Num_Members);

   begin

      --  Every task added to the group is stored in a record
      --  of the Group_Budget
      for i in 1 .. GB.Num_Members loop
         T (i) := GB.Array_Task_Id (i);
      end loop;

      return T;

   end Members;

   -----------------
   --  Replenish  --
   -----------------

   procedure Replenish (GB : in out Group_Budget;
                        To : Ada.Real_Time.Time_Span) is

      Now            : System.BB.Time.Time;
      Is_First_Alarm : Boolean;

      use type System.BB.Peripherals.Timer_Interval;

   begin

      Now := System.BB.Time.Clock;

      if To < Ada.Real_Time.Time_Span_Zero then
         raise Group_Budget_Error;
      else

         if GB.GB_Id = 0 then

            --  If it is the first time that the timer has set, we have to give
            --  it an identifier
            GB.GB_Id := System.BB.Threads.Get_GB_Id;
            Copy_GB (GB, GB.GB_Id);
         end if;

         --  The value of the time is stored in an array to its later use.
         --  ys group data are update.

         System.BB.Threads.Budget_Array
           (GB.GB_Id) :=  To_Time_Span (To);

         --  if is a task of the group that execute replenish then
         if System.BB.Threads.Queues.Running_Thread.GB_Id = GB.GB_Id and
           GB.GB_Id /= 0 then

            --  En caso de que el llamante no sea una tarea (interrupcion
            --  o Time_Event) vemos si la tarea anterior que estaba ejecutando
            --  pertenece al grupo (GB) si es asi se modifica la alarma.
            --  if the running task (o la tarea que ejecutaba antes de la
            --  interrupcion o time event) belongs to the group the difference
            --  between the init of the execution and the replenishing of the
            --  timer must be stored

            System.BB.Threads.Queues.Running_Thread.Time_Diff_GB :=
              Now - System.BB.Threads.Queues.
                Running_Thread.Time_Init_Execution;

            --  insert alarm in the queue if needed

            System.BB.Threads.Queues.Insert_GB_Alarm
              (Now + (To_Time_Span (To)), Is_First_Alarm);

            --  We have to change the Time_Span value to a Time value

            Insert_Alarm_If_Needed (Is_First_Alarm, Now + (To_Time_Span (To)));
            System.BB.Threads.Queues.Update_Alarms;

         end if;

      end if;

   end Replenish;

   -----------
   --  Add  --
   -----------

   procedure Add (GB : in out Group_Budget;
                  Interval : Ada.Real_Time.Time_Span) is

      Aux_Time        : System.BB.Time.Time;
      Now             : System.BB.Time.Time;
      Running         : System.BB.Threads.Thread_Id;
      Elapsed         : System.BB.Time.Time;
      Is_First_Alarm  : Boolean;

      use type System.BB.Peripherals.Timer_Interval;

   begin
      Now := System.BB.Time.Clock;
      Running := System.BB.Threads.Queues.Running_Thread;

      if GB.GB_Id = 0 then
         --  If it is the first time that the timer has set, we have to give it
         --  an identifier

         GB.GB_Id := System.BB.Threads.Get_GB_Id;
      end if;

      --  The value of the time is stored in an array to its later use

      if Is_Member (GB, Ada.Task_Identification.Current_Task) then

         if System.BB.Threads.Budget_Array (GB.GB_Id) >
           To_Time_Span (Ada.Real_Time.Time_Span_Zero) then

            System.BB.Threads.Budget_Array (GB.GB_Id) :=
              System.BB.Threads.Budget_Array (GB.GB_Id)
              + To_Time_Span (Interval);
            --  If the running task is part of the group, we have to take into
            --  account the time elapsed since the init of its execution
            Elapsed  := Now - Running.Time_Init_Execution;
            Aux_Time := To_Time (System.BB.Threads.Budget_Array (GB.GB_Id))
              - Elapsed;
         else
            --  In case of the budget was initially at zero, we don't count
            --  the time elapsed
            System.BB.Threads.Budget_Array (GB.GB_Id) :=
              System.BB.Threads.Budget_Array (GB.GB_Id)
              + To_Time_Span (Interval);
            Aux_Time := To_Time (System.BB.Threads.Budget_Array (GB.GB_Id));
         end if;

         if System.BB.Threads.Budget_Array (GB.GB_Id) >
           To_Time_Span (Ada.Real_Time.Time_Span_Zero) then
            --  if the alarm was in the future, we insert it in the queue
            System.BB.Threads.Queues.Insert_GB_Alarm
              (Now + Aux_Time, Is_First_Alarm);
            Insert_Alarm_If_Needed (Is_First_Alarm, Now + Aux_Time);
         else
            --  if the alarm was in the past, the handler is executed
            Execute_Handler (GB.GB_Id);
            System.BB.Threads.Queues.GB_Alarm := System.BB.Time.Time'Last;
         end if;
         System.BB.Threads.Queues.Update_Alarms;
      else

         --  if the running task doesn't belong to the group, group data are
         --  updated
         System.BB.Threads.Budget_Array (GB.GB_Id) :=
           System.BB.Threads.Budget_Array (GB.GB_Id) + To_Time_Span (Interval);
         if System.BB.Threads.Budget_Array (GB.GB_Id) <=
           To_Time_Span (Ada.Real_Time.Time_Span_Zero) then
            Execute_Handler (GB.GB_Id);
         end if;
      end if;
   end Add;

   --------------------------
   --  Budget_Has_Expired  --
   --------------------------

   function Budget_Has_Expired (GB : Group_Budget) return Boolean is

   begin

      if System.BB.Threads.Budget_Array (GB.GB_Id) =
        To_Time_Span (Ada.Real_Time.Time_Span_Zero) then

         return True;

      else

         return False;

      end if;

   end Budget_Has_Expired;

   ------------------------
   --  Budget_Remaining  --
   ------------------------

   function Budget_Remaining (GB : Group_Budget)
      return Ada.Real_Time.Time_Span is

   begin
      if System.BB.Threads.Budget_Array (GB.GB_Id) <=
        To_Time_Span (Ada.Real_Time.Time_Span_Zero) then
         return  Ada.Real_Time.Time_Span_Zero;
      else

         if Is_Member (GB, Ada.Task_Identification.Current_Task) then

            --  if the running task belongs to the group, we have to
            --  substract
            --  the elapsed time since the init of its execution
            return To_Time_Span (System.BB.Threads.Budget_Array (GB.GB_Id) -
               System.BB.Time.Time_Span (System.BB.Time.Clock
               - System.BB.Threads.Queues.Running_Thread.Time_Init_Execution));
         else
            return To_Time_Span (System.BB.Threads.Budget_Array (GB.GB_Id));
         end if;
      end if;

   end Budget_Remaining;

   -------------------
   --  Set_Handler  --
   -------------------

   procedure Set_Handler (GB      : in out Group_Budget;
                          Handler :        Group_Budget_Handler) is

   begin

      if True then

      --  HandlerPriority >= Min_Handler_Ceiling then
      --  This condition must be applied.

         if GB.GB_Id = 0 then

            --  if it is the first time that the group is modified, it needs
            --  an identifier
            GB.GB_Id := System.BB.Threads.Get_GB_Id;

         end if;

         Handlers_Array (GB.GB_Id) := Handler;
         if Handler = null then

            GB.Is_Set_Handler := False;
         else

            GB.Is_Set_Handler := True;
         end if;

         Copy_GB (GB, GB.GB_Id);
      else

         raise Program_Error;
      end if;

   end Set_Handler;

   -----------------------
   --  Current_Handler  --
   -----------------------

   function Current_Handler (GB : Group_Budget) return Group_Budget_Handler is

   begin

      if not GB.Is_Set_Handler or
        GB.GB_Id = 0 then

         return null;
      else

         return Handlers_Array (GB.GB_Id);
      end if;

   end Current_Handler;

   ----------------------
   --  Cancel_Handler  --
   ----------------------

   procedure Cancel_Handler (GB        : in out Group_Budget;
                             Cancelled :    out Boolean) is

   begin

      if not GB.Is_Set_Handler then

         Cancelled := False;
      else

         GB.Is_Set_Handler := False;
         GB_Array (GB.GB_Id).Is_Set_Handler := False;
         Cancelled := True;
         Handlers_Array (GB.GB_Id) := null;
      end if;

   end Cancel_Handler;

   -----------------------
   --  Execute_Handler  --
   -----------------------

   procedure Execute_Handler (Id : Integer) is

   begin

      if GB_Array (Id).Is_Set_Handler then

         Handlers_Array (Id).all (GB_Array (Id));

      end if;

   exception
      when others => null;
   end Execute_Handler;

   ---------------
   --  Copy_GB  --
   ---------------

   procedure Copy_GB (GB : Group_Budget;
                         Id : Integer) is

   begin

      GB_Array (Id).GB_Id := GB.GB_Id;
      GB_Array (Id).Num_Members := GB.Num_Members;
      GB_Array (Id).Is_Set_Handler := GB.Is_Set_Handler;
      GB_Array (Id).Array_Threads := GB.Array_Threads;
      GB_Array (Id).Array_Task_Id := GB.Array_Task_Id;

   end Copy_GB;

   ------------------------------
   --  Insert_Alarm_If_Needed  --
   ------------------------------

   procedure Insert_Alarm_If_Needed (Is_First_Alarm : Boolean;
                                     Time : System.BB.Time.Time) is
      Time_Difference : System.BB.Peripherals.Timer_Interval;
      use type System.BB.Peripherals.Timer_Interval;

   begin
   --   Time_Difference : Split_Time;
   --     use type System.BB.Peripherals.Timer_Interval;

   --  begin
      if Is_First_Alarm then
         Time_Difference := To_Timer_Interval (Time);

   --        --  Check whether the alarm time is within a clock period

   --           --  needed. The timer that we are setting is always in the
   --           --  future because we have previously checked that the the
   --           --  value of the alarm is strictly greater than the
   --           --  selected clock value.

         pragma Assert (Time_Difference > 0);

         if System.BB.Time.Get_Pending_Alarm then
            System.BB.Peripherals.Cancel_And_Set_Alarm
              (Time_Difference);
         else
            System.BB.Peripherals.Set_Alarm (Time_Difference);
            System.BB.Time.Turn_True_Pending_Alarm;
         end if;
      end if;

   end Insert_Alarm_If_Needed;

end Ada.Execution_Time.Group_Budgets;
