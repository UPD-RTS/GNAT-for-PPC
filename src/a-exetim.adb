------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                     A D A . E X E C U T I O N _ T I M E                  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--           Copyright (C) 2007 Universidad Politecnica de Madrid           --
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

with System.BB.Threads;
--  Used for Thread_Id

with System.BB.Threads.Queues;
--  Used for Running_Thread
--           First_Thread

with System.BB.Time;
--  Used for Time
--           Time_Span
--           Clock
use type System.BB.Time.Time;

with System.Task_Primitives.Operations.Execution_Time_Support;
--  Used for Convert_Id

with System.Task_Primitives.Operations;
--  Used for RT_Resolution

with Ada.Unchecked_Conversion;

package body Ada.Execution_Time is

   function To_CPU_Time is new Ada.Unchecked_Conversion
     (System.BB.Time.Time, CPU_Time);
   --  Function to change the view from Time (unsigned 64-bit)
   --  to CPU_Time (unsigned 64-bit).

   function To_Time is new Ada.Unchecked_Conversion
     (CPU_Time, System.BB.Time.Time);
   --  Function to change the view from CPU_Time (unsigned 64-bit)
   --  to Time (unsigned 64-bit).

   function To_CPU_Time is new Ada.Unchecked_Conversion
     (Ada.Real_Time.Time_Span, CPU_Time);
   --  Function to change the view from Time_Span (signed 63-bit)
   --  to CPU_Time (unsigned 64-bit).

   function To_Time_Span is new Ada.Unchecked_Conversion
     (CPU_Time, Ada.Real_Time.Time_Span);
   --  Function to change the view from CPU_Time (unsigned 64-bit)
   --  to Time_Span (signed 63-bit).

   -------------
   --  Clock  --
   -------------

   function Clock (T : Ada.Task_Identification.Task_Id
                   := Ada.Task_Identification.Current_Task) return CPU_Time is
      use type Ada.Task_Identification.Task_Id;
      use type System.BB.Threads.Thread_Id;
      Th  : System.BB.Threads.Thread_Id;
      Now : System.BB.Time.Time;

   begin
      --  Convert the Task_Id type in the thread information type of the
      --  kernel
      Th := System.Task_Primitives.Operations.
        Execution_Time_Support.Convert_Id (T);

      if Th = System.BB.Threads.Null_Thread_Id then

         raise Program_Error;
      end if;

      if Th.Time_Init_Execution /= System.BB.Time.Time'Last then

         --  If the thread is running right now, we have to add the execution
         --  time elapsed, since it was waken up
         Now := System.BB.Time.Clock;
         return To_CPU_Time
           (Th.Execution_Time + Now - Th.Time_Init_Execution);
      else

         return To_CPU_Time (Th.Execution_Time);
      end if;
   end Clock;

   --  Functions to handle between Execution_time types and Time types

   ---------
   -- "+" --
   ---------

   function "+"  (Left : CPU_Time;
                  Right : Ada.Real_Time.Time_Span) return CPU_Time is
   begin
      return Left + To_CPU_Time (Right);
   end "+";

   function "+"  (Left : Ada.Real_Time.Time_Span;
                  Right : CPU_Time) return CPU_Time is
   begin
      return To_CPU_Time (Left) + Right;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-"  (Left : CPU_Time;
                  Right : Ada.Real_Time.Time_Span) return CPU_Time is
   begin
      return Left - To_CPU_Time (Right);
   end "-";

   function "-"  (Left : CPU_Time;
                  Right : CPU_Time)  return Ada.Real_Time.Time_Span is
      Aux : CPU_Time;
   begin
      Aux := Left - Right;
      return To_Time_Span (Aux);
   end "-";

   ---------
   -- "<" --
   ---------
   function "<"  (Left, Right : CPU_Time) return Boolean is

   begin
      return To_Time (Left) < To_Time (Right);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : CPU_Time) return Boolean is

   begin
      return To_Time (Left) <= To_Time (Right);
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">"  (Left, Right : CPU_Time) return Boolean is

   begin
      return To_Time (Left) > To_Time (Right);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : CPU_Time) return Boolean is

   begin
      return To_Time (Left) >= To_Time (Right);
   end ">=";

   -----------
   -- Split --
   -----------

   procedure Split
     (T : CPU_Time;
      SC : out Ada.Real_Time.Seconds_Count;
      TS : out Ada.Real_Time.Time_Span) is

      Res : constant CPU_Time := CPU_Time
        (System.Task_Primitives.Operations.RT_Resolution);
      use type Ada.Real_Time.Time_Span;
   begin
      SC := Ada.Real_Time.Seconds_Count (T / Res);
      TS := To_Time_Span (T)
        - To_Time_Span (CPU_Time (SC) * Res);
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of (SC : Ada.Real_Time.Seconds_Count;
                     TS : Ada.Real_Time.Time_Span
                     := Ada.Real_Time.Time_Span_Zero) return CPU_Time is
   begin
      return CPU_Time (SC) * CPU_Time
        (System.Task_Primitives.Operations.RT_Resolution) + To_CPU_Time (TS);
   end Time_Of;

end Ada.Execution_Time;
