------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . C P U _ P R I M I T I V E S            --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2004 The European Space Agency            --
--                     Copyright (C) 2003-2007, AdaCore                     --
--             Copyright (C) 2009 Universidad Politecnica de Madrid         --
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

--  This package contains the primitives which are dependent on the
--  underlying processor.

--  This is the PPC version of this package

pragma Restrictions (No_Elaboration_Code);

with System;
with System.Parameters;
with System.BB.Parameters;

package System.BB.CPU_Primitives is
   pragma Preelaborate;

   ------------------------
   -- Context management --
   ------------------------

   Context_Buffer_Capacity : constant := 66;
   --  The PowerPC processor needs to save volatile and dedicated registers:
   --     LR Link register
   --     CR Condition register
   --     CTR Count register
   --     XER Carries and overﬂows for integer operations
   --     SRR0 Machine status save/restore register 0
   --     SRR1 Machine status save/restore register 1
   --     R0 - R1(SP) - R2 - R3 - R4 - R5 - R6 - R7
   --          R8 - R9 - R10 - R11 - R12 - R13
   --     FPSCR_pad Floating-point status and control register (64bit pad)
   --     FPSCR_val (ditto) actual value
   --     FPR0 - FPR1 - FPR2 - FPR3 - FPR4 - FPR5 - FPR6 - FPR7
   --          FPR8 - FPR9 - FPR10 - FPR11 - FPR12 - FPR13

   Context_Buffer_Size : constant :=
                           Context_Buffer_Capacity * System.Word_Size;
   --  Size calculated taken into account that the components are 32-bit,
   --  and that we want then aligned on 64-bit boundaries.

   type Context_Buffer is private;
   --  This type contains the saved register set for each thread

   procedure Context_Switch;
   pragma Import (Asm, Context_Switch, "context_switch");
   --  Perform the context switch between the running_thread and the
   --  first_thread.

   procedure Initialize_Context
     (Buffer          : not null access Context_Buffer;
      Program_Counter : System.Address;
      Argument        : System.Address;
      Stack_Pointer   : System.Address);
   pragma Inline (Initialize_Context);
   --  Initialize_Context inserts inside the context buffer the
   --  default values for each register. The values for the stack
   --  pointer, the program counter, and argument to be passed
   --  are provided as arguments.

   ---------------------------------
   -- Interrupt and trap handling --
   ---------------------------------

   procedure Install_Handler
     (Service_Routine : System.Address;
      Vector          : System.BB.Parameters.Range_Of_Vector);
   --  Install a new handler in the trap table, both for synchronous and
   --  asynchronous traps.

   procedure Disable_Interrupts;
   pragma Import (Asm, Disable_Interrupts, "disable_interrupts");
   --  All external interrupts (asynchronous traps) are disabled

   procedure Enable_Interrupts
     (Level : System.BB.Parameters.Interrupt_Level);
   pragma Import (Asm, Enable_Interrupts, "enable_interrupts");
   --  Interrupts are enabled if they are above the value given by Level

   procedure Initialize_Floating_Point;
   pragma Import (Asm, Initialize_Floating_Point, "initialize_floating_point");
   --  Install the floating point trap handler in charge of performing
   --  floating point context switches.

private

   subtype Range_Of_Context is Natural range 0 .. Context_Buffer_Capacity - 1;
   --  Type used for accessing to the different elements in the context buffer

   type Context_Buffer is array (Range_Of_Context) of System.Address;
   for Context_Buffer'Size use Context_Buffer_Size;
   for Context_Buffer'Alignment use 8;
   --  This array contains all the registers that the thread needs to save
   --  within its thread descriptor. Using double word boundaries allows us
   --  to use double word loads and stores safely in the context switch.

end System.BB.CPU_Primitives;
