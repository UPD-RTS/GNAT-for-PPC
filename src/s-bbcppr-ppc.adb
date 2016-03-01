------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . C P U _ P R I M I T I V E S            --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2008, AdaCore                     --
--           Copyright (C) 2012-2013 Università degli Studi di Padova       --
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

--  This is the PPC version of this package

pragma Restrictions (No_Elaboration_Code);

with System.Storage_Elements;
with System.BB.Serial_Output;
use System.BB.Serial_Output;

package body System.BB.CPU_Primitives is

   package SSE renames System.Storage_Elements;
   use type SSE.Integer_Address;
   use type SSE.Storage_Offset;

   ----------------
   -- Local data --
   ----------------
   SP : constant Range_Of_Context :=  0; -- Stack Pointer
   LR : constant Range_Of_Context :=  1; -- Link register
--  CR : constant Range_Of_Context :=  2; -- condition register
   GPR_2 : constant Range_Of_Context :=  3;  -- r2
--   GPR13 : constant Range_Of_Context :=  4; -- r13
--   GPR14 : constant Range_Of_Context :=  5; --
--   GPR15 : constant Range_Of_Context :=  6; --
--   GPR16 : constant Range_Of_Context :=  7; -- General purpose register
--   GPR17 : constant Range_Of_Context :=  8; -- General purpose register
--   GPR18 : constant Range_Of_Context :=  9; -- General purpose register
--   GPR19 : constant Range_Of_Context := 10; -- General purpose register
--   GPR20 : constant Range_Of_Context := 11; -- General purpose register
--   GPR21 : constant Range_Of_Context := 12; -- General purpose register
--   GPR22 : constant Range_Of_Context := 13; -- General purpose register
--   GPR23 : constant Range_Of_Context := 14; -- General purpose register
--   GPR24 : constant Range_Of_Context := 15; -- General purpose register
--   GPR25 : constant Range_Of_Context := 16; -- General purpose register
--   GPR26 : constant Range_Of_Context := 17; -- General purpose register
--   GPR27 : constant Range_Of_Context := 18; -- General purpose register
--   GPR28 : constant Range_Of_Context := 19; -- General purpose register
--   GPR29 : constant Range_Of_Context := 20; -- General purpose register
--   GPR30 : constant Range_Of_Context := 21; -- General purpose register
--   GPR31 : constant Range_Of_Context := 22; -- General purpose register
--   pad0 : constant Range_Of_Context := 23; -- Alignment padding

--   FPSCR : constant Range_Of_Context := 24; -- FP status/control
--   FPSCRv : constant Range_Of_Context := 25; -- FPSCR 32bit (value)
--   FPR14 : constant Range_Of_Context := 26; -- Floating point register
--   FPR15 : constant Range_Of_Context := 28; -- Floating point register
--   FPR16 : constant Range_Of_Context := 30; -- Floating point register
--   FPR17 : constant Range_Of_Context := 32; -- Floating point register
--   FPR18 : constant Range_Of_Context := 34; -- Floating point register
--   FPR19 : constant Range_Of_Context := 36; -- Floating point register
--   FPR20 : constant Range_Of_Context := 38; -- Floating point register
--   FPR21 : constant Range_Of_Context := 40; -- Floating point register
--   FPR22 : constant Range_Of_Context := 42; -- Floating point register
--   FPR23 : constant Range_Of_Context := 44; -- Floating point register
--   FPR24 : constant Range_Of_Context := 46; -- Floating point register
--   FPR25 : constant Range_Of_Context := 48; -- Floating point register
--   FPR26 : constant Range_Of_Context := 50; -- Floating point register
--   FPR27 : constant Range_Of_Context := 52; -- Floating point register
--   FPR28 : constant Range_Of_Context := 54; -- Floating point register
--   FPR29 : constant Range_Of_Context := 56; -- Floating point register
--   FPR30 : constant Range_Of_Context := 58; -- Floating point register
--   FPR31 : constant Range_Of_Context := 60; -- Floating point register

--   pad1 : constant Range_Of_Context := 61; -- Alignment padding
--   pad2 : constant Range_Of_Context := 62; -- Alignment padding
--   BACKCHAIN : constant Range_Of_Context := 63; -- previous context
--  LR : constant Range_Of_Context := 64; -- previous LR

--   type Trap_Entry is
--      record
--         First_Instr  : SSE.Integer_Address;
--         Second_Instr : SSE.Integer_Address;
--         Third_Instr  : SSE.Integer_Address;
--         Fourth_Instr : SSE.Integer_Address;
--      end record;
   --  Each entry in the trap table contains the four first instructions that
   --  will be executed as part of the handler. A trap is a vectored transfer
   --  of control to the supervisor through a special trap table that contains
   --  the first four instructions of each trap handler. The base address of
   --  the table is established by supervisor and the displacement, within the
   --  table, is determined by the trap type.

--   Trap_Table : array (Parameters.Range_Of_Vector) of Trap_Entry;
--   pragma Import (Asm, Trap_Table, "_trap_table");
   --  This is the trap table, that is defined in the crt0 file. This table
   --  contains the trap entry for all the traps (synchronous and asynchronous)
   --  defined by the SPARC architecture.

   User_Vector_Table : array (Parameters.Range_Of_Vector) of System.Address;
   pragma Export (Asm, User_Vector_Table, "user_vector_table");
   --  In addition to the trap table there is another table that contains the
   --  addresses for the trap handlers defined by the user.

   ------------------------
   -- Initialize_Context --
   ------------------------

   procedure Initialize_Context
     (Buffer          : not null access Context_Buffer;
      Program_Counter : System.Address;
      Argument        : System.Address;
      Stack_Pointer   : System.Address)
   is
   begin
      --  PPC
      --  Put_Line ("Initialize context");
      --  Put_Line ("Arg: 0x" & Argument'Img);
      --  With user-level execution we must also initialize the MSR state upon
      --  thread activation as:
      --          (MSR_EE | MSR_IP | MSR_DR | MSR_IR | MSR_PR | MSR_FP);
      --          00000000000000001110000001110000
      --          16#E070#
      --  Or, if we want to execute as supervisor, as:
      --          00000000000000001010000001000000
      --          16#A040#
      --  Buffer (SRR1) := SSE.To_Address (16#A040#);

      --  Initialize the stack pointer
      --  PEM32 states the SP must be quadword aligned (i.e. multiple of 16)
      --  The GPR 1 is holds the SP
      Buffer (SP) :=
          SSE.To_Address ((SSE.To_Integer (Stack_Pointer) / 16) * 16 - 256);
      Buffer (LR) := Program_Counter;
      Buffer (GPR_2) := Argument;
      --  TODO To be changed: actually GPR_3 (parameter to the Wrapper)
      --  Put_Line ("Counter: 0x" & Program_Counter'Img);

      --  The rest of registers do not need to be initialized

   end Initialize_Context;

   ---------------------
   -- Install_Handler --
   ---------------------

   procedure Install_Handler
     (Service_Routine : System.Address;
      Vector          : System.BB.Parameters.Range_Of_Vector)
   is
   begin
      User_Vector_Table (Vector) := Service_Routine;
   end Install_Handler;

end System.BB.CPU_Primitives;
