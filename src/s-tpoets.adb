------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--     S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S    --
--            . E X E C U T I O N _ T I M E _ S U P P O R T                 --
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

with System.Tasking;
--  Used for Task_Id

with Unchecked_Conversion;

package body System.Task_Primitives.Operations.Execution_Time_Support is

   function To_Tasking_Id is new Unchecked_Conversion
     (Ada.Task_Identification.Task_Id, System.Tasking.Task_Id);
   --  The Ada.Task_Identification.Task_Id type is the same type that
   --  System.Tasking.Task_Id

   ------------------
   --  Convert_Id  --
   ------------------

   function Convert_Id (T : Ada.Task_Identification.Task_Id)
                        return System.BB.Threads.Thread_Id is

      --  Function that returns the Thread_Id object with the information of
      --  the task that is kept into an Ada.Task_Identification.Task_Id object

      ST_Id : System.Tasking.Task_Id;

   begin

      ST_Id := To_Tasking_Id (T);

      --  The component Thread of the type System.Task_Primitives.Private_Data
      --  is a System.BB.Threads.Thread_Id type

      return ST_Id.Common.LL.Thread_Desc'Access;

   end Convert_Id;

end System.Task_Primitives.Operations.Execution_Time_Support;
