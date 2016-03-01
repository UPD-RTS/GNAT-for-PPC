------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--            A D A . R E A L _ T I M E . T I M I N G _ E V E N T S         --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--           Copyright (C) 2009 Universidad Politecnica de Madrid           --
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
--  with System.BB.Threads;
--  Used for TE_Alarm_Queue_Id
--           Exec_Handler
--           Get_TE_Id
--  with My_IO2;

with System.BB.Threads.Queues;
--  Used for TE_Handler
--           Insert_TE_Alarm
--           Extract_TE_Alarm

--  with System.BB.Time;
--  Used for Time

--  with System.BB.Time.Execution_Time_Support;
--  use System.BB.Time.Execution_Time_Support;
--  with System.IO; use System.IO;
with System.BB.Serial_Output; use System.BB.Serial_Output;

with Ada.Unchecked_Conversion;

package body Ada.Real_Time.Timing_Events is

   function To_Time is new Ada.Unchecked_Conversion
     (Ada.Real_Time.Time, System.BB.Time.Time);
   --  Function to change the view between Time_Span types

   --------------------
   --  Get_TE_Alarm  --
   --------------------

   function Get_TE_Alarm (Id : Integer; Is_First : Boolean)
                       return System.BB.Threads.TE_Alarm_Queue_Id is
   begin
      --  Initializes the array of TE_Alarm_Queue_Id if needed
      if Is_First then
         Array_Queue (Id) :=
           (System.BB.Threads.Null_TE_Alarm_Queue_Id,
            System.BB.Time.Time'Last,
            0,
            System.BB.Threads.Null_TE_Alarm_Queue_Id);
      end if;
      return Array_Queue (Id)'Access;
   end Get_TE_Alarm;

   -------------------
   --  Set_Handler  --
   -------------------

   procedure Set_Handler (Event : in out Timing_Event; At_Time : Time;
                          Handler : Timing_Event_Handler) is
      Q_Id_Aux : System.BB.Threads.TE_Alarm_Queue_Id;
      First_Alarm : System.BB.Time.Time;
      Is_First   : Boolean := False;
      Id : Integer;
      use type System.BB.Threads.Exec_Handler;
      use type System.BB.Threads.TE_Alarm_Queue_Id;

   begin
      --  My_IO2.Put_Line ("timing_set...");
      if Event.TE_Id = 0 then
         --  An identifier is associated to the Timing_Event
         Event.TE_Id := System.BB.Threads.Get_TE_Id;
         Is_First := True;
      end if;

      if Event.Is_Set then
         System.BB.Threads.Queues.Get_TE_Id (Id);
         if System.BB.Threads.Queues.Is_TE_Alarm and
           Id = Event.TE_Id then
            --  If an alarm set by this own timing event is already set by
            --  the hardware we have to cancel it.

            --  JAPP
            --  Comento la linea de codigo Cancel_Alarm...
            --  System.BB.Time.Cancel_Alarm;
            null;
            --  JAPP

         end if;
         System.BB.Threads.Queues.Extract_TE_Alarm (Event.Queue_Id);
         --  If it is already set, the alarm is extracted from the queue
      end if;

      if Handler /= null then
         if At_Time > Clock then
            --  If the alarm time is in the future
            Q_Id_Aux := Get_TE_Alarm (Event.TE_Id, Is_First);
            Q_Id_Aux.TE_Id := Event.TE_Id;
            Q_Id_Aux.Alarm_Time := To_Time (At_Time);
            Q_Id_Aux.Next_Alarm := System.BB.Threads.Null_TE_Alarm_Queue_Id;
            Event.Queue_Id := Q_Id_Aux;
            --  Insert alarm in the Timing_Event alarms queue
            System.BB.Threads.Queues.Insert_TE_Alarm
              (To_Time (At_Time), Event.Queue_Id);

            --  JAPP
            Put_Line ("About to set TE");
            First_Alarm := System.BB.Threads.Queues.Get_Next_Alarm_Time;
            System.BB.Threads.Insert_Alarm_If_Needed (True, First_Alarm);
            --  Insert_Alarm_If_Needed (Is_First, To_Time (At_Time));
            System.BB.Threads.Queues.Update_Alarms;
            Put_Line (Time'Image (At_Time));
            --  JAPP

            Event.Is_Set := True;

            --  Store the data in the array to its later use
            Array_TE (Event.TE_Id).TE_Id    := Event.TE_Id;
            Array_TE (Event.TE_Id).Time_Of  := Event.Time_Of;
            Array_TE (Event.TE_Id).Is_Set   := Event.Is_Set;
            Array_TE (Event.TE_Id).Queue_Id := Event.Queue_Id;
            Array_TEH (Event.TE_Id) := Handler;

         else
            --  If the alarm time is in the past
            Handler.all (Event);
         end if;
      else
         Event.Is_Set := False;
      end if;

      if System.BB.Threads.Queues.TE_Handler = null then
         System.BB.Threads.Queues.TE_Handler := Execute_Handler'Access;
      end if;

   end Set_Handler;

   -------------------
   --  Set_Handler  --
   -------------------

   procedure Set_Handler (Event : in out Timing_Event; In_Time : Time_Span;
                          Handler : Timing_Event_Handler) is
      Q_Id_Aux : System.BB.Threads.TE_Alarm_Queue_Id;
      Is_First   : Boolean := False;
      First_Alarm : System.BB.Time.Time;
      Now        : Time;
      Id         : Integer;
      use type System.BB.Threads.Exec_Handler;
      use type System.BB.Threads.TE_Alarm_Queue_Id;
   begin
      Now := Clock;
      if Event.TE_Id = 0 then
         --  An identifier is associated to the Timing_Event
         Event.TE_Id := System.BB.Threads.Get_TE_Id;
         Is_First := True;
      end if;

      if Event.Is_Set then
         System.BB.Threads.Queues.Get_TE_Id (Id);
         if System.BB.Threads.Queues.Is_TE_Alarm and
           Id = Event.TE_Id then
            --  If an alarm set by this own timing event is already set by
            --  the hardware we have to cancel it.

            --  JAPP :  -- Comento la linea Cancel Alarm
            --  System.BB.Time.Cancel_Alarm;
            null;
         end if;
         System.BB.Threads.Queues.Extract_TE_Alarm (Event.Queue_Id);
         --  If it is already set, the alarm is extracted from the queue
      end if;

      if Handler /= null then
         if In_Time > 0 then
            Put ("a " & Duration'Image (To_Duration (In_Time)));
            --  If the alarm time is in the future
            Q_Id_Aux := Get_TE_Alarm (Event.TE_Id, Is_First);
            Q_Id_Aux.TE_Id := Event.TE_Id;
            Q_Id_Aux.Alarm_Time := To_Time (Now + In_Time);
            Q_Id_Aux.Next_Alarm := System.BB.Threads.Null_TE_Alarm_Queue_Id;
            Event.Queue_Id := Q_Id_Aux;
            --  Insert alarm in the Timing_Event alarms queue
            System.BB.Threads.Queues.Insert_TE_Alarm
              (To_Time (Now + In_Time), Event.Queue_Id);
            Put ("b " & Time'Image (Now + In_Time));
            --  JAPP
            First_Alarm := System.BB.Threads.Queues.Get_Next_Alarm_Time;
            Put_Line (System.BB.Time.Time'Image (First_Alarm));
            System.BB.Threads.Insert_Alarm_If_Needed (True, First_Alarm);
            System.BB.Threads.Queues.Update_Alarms;
            --  JAPP

            Event.Is_Set := True;

            --  Store the data in the array to its later use
            Array_TE (Event.TE_Id).TE_Id    := Event.TE_Id;
            Array_TE (Event.TE_Id).Time_Of  := Event.Time_Of;
            Array_TE (Event.TE_Id).Is_Set   := Event.Is_Set;
            Array_TE (Event.TE_Id).Queue_Id := Event.Queue_Id;
            Array_TEH (Event.TE_Id) := Handler;
         else
            --  If the alarm time is in the past
            --  My_IO2.Put_Line ("bueansss");
            Handler.all (Event);
         end if;
      else
         Event.Is_Set := False;
      end if;

      if System.BB.Threads.Queues.TE_Handler = null then
         System.BB.Threads.Queues.TE_Handler := Execute_Handler'Access;
      end if;

   end Set_Handler;

   -----------------------
   --  Current_Handler  --
   -----------------------

   function Current_Handler (Event : Timing_Event)
                             return Timing_Event_Handler is
   begin
      if Array_TE (Event.TE_Id).Is_Set then
         return Array_TEH (Event.TE_Id);
      else
         return null;
      end if;
   end Current_Handler;

   ----------------------
   --  Cancel_Handler  --
   ----------------------

   procedure Cancel_Handler (Event : in out Timing_Event;
                             Cancelled : out Boolean) is
   begin
      if Array_TE (Event.TE_Id).Is_Set then
         Cancelled := True;
         Event.Is_Set := False;
         Event.Time_Of := Time_First;
         Array_TE (Event.TE_Id).Is_Set := False;
         Array_TE (Event.TE_Id).Time_Of := Time_First;
         --  We have to extract the alarm from Timing_Event alarms queue
         System.BB.Threads.Queues.Extract_TE_Alarm (Event.Queue_Id);
         System.BB.Threads.Queues.Update_Alarms;
      else
         Cancelled := False;
      end if;
   end Cancel_Handler;

   ---------------------
   --  Time_Of_Event  --
   ---------------------

   function Time_Of_Event (Event : Timing_Event) return Time is
   begin
      if Array_TE (Event.TE_Id).Is_Set then
         return Event.Time_Of;
      else
         return Time_First;
      end if;
   end Time_Of_Event;

   -----------------------
   --  Execute_Handler  --
   -----------------------

   procedure Execute_Handler (Id : Integer) is
   begin
      Array_TE (Id).Is_Set := False;
      Array_TEH (Id).all (Array_TE (Id));
   exception
      when others => null;
   end Execute_Handler;

   procedure Nothing is
   begin
      null;
   end Nothing;

end Ada.Real_Time.Timing_Events;
