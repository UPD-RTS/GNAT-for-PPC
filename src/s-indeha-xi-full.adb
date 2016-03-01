------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--    S Y S T E M . I N T E R R U P T S . D E F A U L T _ H A N D L E R     --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                     Copyright (C) 2001-2010, AdaCore                     --
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
------------------------------------------------------------------------------

--  This is a generic bare board version of this procedure with support for
--  exception propagation.

--  Note that the difference with respect to the high integrity version of
--  this package is that exception handlers are allowed. As exception
--  propagated from a handler that is invoked by an interrupt must have no
--  effect (ARM C.3 par. 7), interrupt handlers are wrapped by a null
--  exception handler to avoid exceptions to be propagated further.

separate (System.Interrupts)
procedure Default_Handler (Interrupt : System.OS_Interface.Interrupt_ID) is
begin
   User_Handlers (Interrupt_ID (Interrupt)).all;
exception
   when others =>
      null;  --  Simply avoid further exception propagation
end Default_Handler;
