------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             G N A T . T R A C E B A C K . S Y M B O L I C                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1999-2010, AdaCore                     --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Run-time symbolic traceback support for targets using DWARF debug data

with Ada.Exceptions.Traceback; use Ada.Exceptions.Traceback;

with Interfaces.C.Strings; use Interfaces.C.Strings;

with System.Dwarf_Lines; use System.Dwarf_Lines;

package body GNAT.Traceback.Symbolic is

   ------------------------
   -- Symbolic_Traceback --
   ------------------------

   function Symbolic_Traceback (Traceback : Tracebacks_Array) return String is

      function Executable_File_Name return String;
      --  Return the file name for the currently executing process

      --------------------------
      -- Executable_File_Name --
      --------------------------

      function Executable_File_Name return String is
         type Argv_Array is array (0 .. 0) of chars_ptr;
         gnat_argv : access Argv_Array;
         pragma Import (C, gnat_argv, "gnat_argv");

         function locate_exec_on_path
           (Name : chars_ptr) return chars_ptr;
         pragma Import (C, locate_exec_on_path, "__gnat_locate_exec_on_path");

      begin
         return Value (locate_exec_on_path (gnat_argv (0)));
      end Executable_File_Name;

      C : Dwarf_Context;

   --  Start of processing for Symbolic_Traceback

   begin
      Open (Executable_File_Name, C);

      declare
         Result : constant String := Symbolic_Traceback (C, Traceback);
      begin
         Close (C);
         return Result;
      end;

      --  We must not allow an unhandled exception here, since this function
      --  may be installed as a decorator for all automatic exceptions.

   exception
      when others =>
         return "";
   end Symbolic_Traceback;

   function Symbolic_Traceback (E : Exception_Occurrence) return String is
   begin
      return Symbolic_Traceback (Tracebacks (E));
   end Symbolic_Traceback;

end GNAT.Traceback.Symbolic;
