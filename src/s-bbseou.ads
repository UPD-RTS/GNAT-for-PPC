------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                S Y S T E M . B B . S E R I A L _ O U T P U T             --
--                                                                          --
--                                  S p e c                                 --
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

--  This package provides the primitives for sending characters to the remote
--  host machine through an UART channel.

package System.BB.Serial_Output is
   pragma Preelaborate;

   procedure Put (Item : Character);
   pragma Export (C, Put, "system_bb_put_char");
   pragma Inline (Put);
   --  Character output

   procedure Put (Item : String);
   pragma Inline (Put);
   --  String output

   procedure New_Line;
   pragma Export (C, New_Line, "system_bb_new_line");
   pragma Inline (New_Line);
   --  Line control

   procedure Put_Line (Item : Character);
   pragma Export (C, Put_Line, "system_bb_put_line_char");
   pragma Inline (Put_Line);
   --  Character output (plus line return)

   procedure Put_Line (Item : String);
   pragma Inline (Put_Line);
   --  String output (plus line return)

end System.BB.Serial_Output;
