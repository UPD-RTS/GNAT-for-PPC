------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             A D A . E X E C U T I O N _ T I M E . T I M E R S            --
--                                                                          --
--                                 S p e c                                  --
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
private with System.BB.Threads;
private with System.BB.Threads.Queues;
private with System.Task_Primitives.Operations.Execution_Time_Support;

with System.OS_Interface;

with System;

package Ada.Execution_Time.Timers is

   type Timer (T : not null access constant Ada.Task_Identification.Task_Id) is
      tagged limited private;

   type Timer_Handler is access protected procedure (TM : in out Timer);

   --  It should be the interrupt priority of LEON 2 Timer 1
   Min_Handler_Ceiling : constant System.Any_Priority :=
     System.OS_Interface.Timer_1_Priority;

   procedure Set_Handler
     (TM      : in out Timer;
      In_Time : Ada.Real_Time.Time_Span;
      Handler : Timer_Handler);

   procedure Set_Handler
     (TM      : in out Timer;
      At_Time : CPU_Time;
      Handler : Timer_Handler);

   function Current_Handler (TM : Timer) return Timer_Handler;

   procedure Cancel_Handler
     (TM        : in out Timer;
      Cancelled : out Boolean);

   function Time_Remaining (TM : Timer) return Ada.Real_Time.Time_Span;

   Timer_Resource_Error : exception;

private
   type Timer (T : not null access constant Ada.Task_Identification.Task_Id)
     is tagged limited record
      Thread    : System.BB.Threads.Thread_Id
        := System.Task_Primitives.Operations
          .Execution_Time_Support.Convert_Id (T.all);
      TM_Id     : Integer := 0;
      Is_Set    : Boolean := False;
     end record;

   Acc         : aliased constant Ada.Task_Identification.Task_Id
     := Ada.Task_Identification.Null_Task_Id;

   Array_Timers : array (1 .. (256)) of Timer (Acc'Access);
   Array_Handlers : array (1 .. (256)) of Timer_Handler;

   procedure Execute_Handler (Id : Integer);

   procedure Insert_Alarm_If_Needed (Is_First_Alarm : Boolean;
                                     Time : System.BB.Time.Time);
end Ada.Execution_Time.Timers;
