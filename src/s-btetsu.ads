------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                        S Y S T E M . B B . T I M E .                     --
--                 E X E C U T I O N _ T I M E _ S U P P O R T              --
--                                                                          --
--                                  S p e c                                 --
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

package System.BB.Time.Execution_Time_Support is

   pragma Preelaborate;

   procedure Execution_Time_Adjust (Now : System.BB.Time.Time);
   procedure Timer_Execution_Time_Adjust (Now : System.BB.Time.Time);
   procedure GB_Execution_Time_Adjust (Now : System.BB.Time.Time);

   procedure Execute_Handler (T : System.BB.Threads.Thread_Id);
   procedure Execute_GB_Handler (T : System.BB.Threads.Thread_Id);

   procedure Insert_Alarm_If_Needed (Is_First_Alarm : Boolean;
                                     Time : System.BB.Time.Time);

end System.BB.Time.Execution_Time_Support;
