------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--      A D A . E X E C U T I O N _ T I M E . G R O U P _ B U D G E T S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2009 Universidad Politecnica de Madrid            --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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
with System;

package Ada.Execution_Time.Group_Budgets is

   type Group_Budget is tagged limited private;

   type Group_Budget_Handler is access
      protected procedure (GB : in out Group_Budget);

   type Task_Array is
      array (Positive range <>) of Ada.Task_Identification.Task_Id;

   --  It should be the interrupt priority of LEON 2 Timer 1
   Min_Handler_Ceiling : constant System.Any_Priority :=
     System.Interrupt_Priority'First + 8;

   procedure Add_Task
     (GB : in out Group_Budget;
      T  : Ada.Task_Identification.Task_Id);

   procedure Remove_Task
     (GB : in out Group_Budget;
      T  : Ada.Task_Identification.Task_Id);

   function Is_Member
     (GB : Group_Budget;
      T  : Ada.Task_Identification.Task_Id) return Boolean;

   function Is_A_Group_Member
     (T : Ada.Task_Identification.Task_Id) return Boolean;

   function Members (GB : Group_Budget) return Task_Array;

   procedure Replenish
     (GB : in out Group_Budget;
      To : Ada.Real_Time.Time_Span);

   procedure Add
     (GB       : in out Group_Budget;
      Interval : Ada.Real_Time.Time_Span);

   function Budget_Has_Expired (GB : Group_Budget) return Boolean;

   function Budget_Remaining
     (GB : Group_Budget) return Ada.Real_Time.Time_Span;

   procedure Set_Handler
     (GB      : in out Group_Budget;
      Handler : Group_Budget_Handler);

   function Current_Handler (GB : Group_Budget) return Group_Budget_Handler;

   procedure Cancel_Handler
     (GB        : in out Group_Budget;
      Cancelled : out Boolean);

   Group_Budget_Error : exception;

private

   Num_Max_Tasks : constant Integer := 255;
   procedure Execute_Handler (Id : Integer);
   procedure Copy_GB (GB : Group_Budget;
                      Id : Integer);
   procedure Insert_Alarm_If_Needed (Is_First_Alarm : Boolean;
                                     Time : System.BB.Time.Time);

   type A_Th is array (1 .. Num_Max_Tasks + 1) of System.BB.Threads.Thread_Id;
   type A_Ta is array (1 .. Num_Max_Tasks + 1) of
     Ada.Task_Identification.Task_Id;

   type Group_Budget is tagged limited record
      GB_Id              : Integer := 0;
      --  This integer identifies the GB
      Num_Members        : Integer     := 0;
      Is_Set_Handler     : Boolean := False;
      Array_Threads      : A_Th;
      Array_Task_Id      : A_Ta;
   end record;

   GB_Array       : array (1 .. 256) of Group_Budget;
   --  Array with all group budget

   Handlers_Array : array (1 .. 256) of Group_Budget_Handler;
   --  Array with all group budget handlers

end Ada.Execution_Time.Group_Budgets;
