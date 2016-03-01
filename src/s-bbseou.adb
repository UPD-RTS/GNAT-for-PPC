------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                S Y S T E M . B B . S E R I A L _ O U T P U T             --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2006 The European Space Agency            --
--                     Copyright (C) 2003-2010, AdaCore                     --
--           Copyright (C) 2011 Universidad Politecnica de Madrid           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
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

--  with System.BB.Peripherals;

pragma Restrictions (No_Elaboration_Code);

with Ada.Unchecked_Conversion, Interfaces.C;

package body System.BB.Serial_Output is

   use type System.BB.Peripherals.UART_Channel;
   use type System.BB.Peripherals.UART_Parity;
   use type System.BB.Peripherals.UART_Flow_Control;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Put (ASCII.LF);
      Put (ASCII.CR);
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (Item : Character) is
   begin
      System.BB.Peripherals.UART_Send
        (Item, System.BB.Peripherals.C1);
   end Put;

   procedure Put (Item : String) is
   begin
      for C in Item'Range loop
         Put (Item (C));
      end loop;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Item : Character) is
   begin
      Put (Item);
      New_Line;
   end Put_Line;

   procedure Put_Line (Item : String) is
   begin
      Put (Item);
      New_Line;
   end Put_Line;

begin
   --  Initialization of the UART to be used for the console output

   System.BB.Peripherals.Initialize_UART
     (Baudrate     => 19200,
      Parity       => System.BB.Peripherals.None,
      FlowControl  => System.BB.Peripherals.On);

end System.BB.Serial_Output;
