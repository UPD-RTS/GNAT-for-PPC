------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   S Y S T E M . B B . I N T E R R U P T S                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2011, AdaCore                     --
--           Copyright (C) 2012-2013 Università degli Studi di Padova       --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
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

pragma Restrictions (No_Elaboration_Code);

with System.Storage_Elements;
with System.BB.CPU_Primitives;
with System.BB.Threads;
with System.BB.Threads.Queues;

with System.BB.Serial_Output; use System.BB.Serial_Output;

package body System.BB.Interrupts is

   use type System.Storage_Elements.Storage_Offset;

   ----------------
   -- Local data --
   ----------------

   type Stack_Space is new Storage_Elements.Storage_Array
     (1 .. Storage_Elements.Storage_Offset (Parameters.Interrupt_Stack_Size));
   for Stack_Space'Alignment use 8;
   --  Type used to represent the stack area for each interrupt. The stack must
   --  be aligned to 8 bytes to allow double word data movements.

   --   subtype Real_Interrupt_ID is
   --     Interrupt_ID range Interrupt_ID'First + 1 .. Interrupt_ID'Last;
   --  This subtype is the same as Interrupt_ID but excluding No_Interrupt,
   --  which is not a real interrupt.

   Interrupt_Stack : Stack_Space;
   --  Array that contains the stack used for each interrupt

   Interrupt_Stack_Address : System.Address;
   pragma Export (Asm, Interrupt_Stack_Address, "interrupt_stack_address");
   --  XtratuM not allow nest interrupts, only one interrupt stack.
   --  Table that contains a pointer to the top of the stack for each interrupt

   type Handlers_Table is array (Interrupt_ID) of Interrupt_Handler;
   pragma Suppress_Initialization (Handlers_Table);
   --  Type used to represent the procedures used as interrupt handlers

   Interrupt_Handlers_Table : Handlers_Table;
   --  Table containing handlers attached to the different external interrupts

   Interrupt_Being_Handled : Interrupt_ID := No_Interrupt;
   pragma Atomic (Interrupt_Being_Handled);
   --  Interrupt_Being_Handled contains the interrupt currently being handled
   --  in the system, if any. It is equal to No_Interrupt when no interrupt is
   --  handled. Its value is updated by the trap handler.

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Interrupt_Wrapper (Vector : System.BB.Parameters.Range_Of_Vector);
   --  This wrapper procedure is in charge of setting the appropriate
   --  software priorities before calling the user-defined handler.

   --------------------
   -- Attach_Handler --
   --------------------

   procedure Attach_Handler (Handler : Interrupt_Handler; Id : Interrupt_ID) is
   begin
      --  Check that we are attaching to a real interrupt

      pragma Assert (Id /= No_Interrupt);

      --  Copy the user's handler to the appropriate place within the table

      Interrupt_Handlers_Table (Id) := Handler;

      --  Transform the interrupt ID to the place in the interrupt vector
      --  table. Then insert the wrapper for the interrupt handlers in the
      --  underlying vector table.

      CPU_Primitives.Install_Handler
        (Interrupt_Wrapper'Address, Peripherals.To_Vector (Id));
   end Attach_Handler;

   -----------------------
   -- Current_Interrupt --
   -----------------------

   function Current_Interrupt return Interrupt_ID is
   begin
      return Interrupt_Being_Handled;
   end Current_Interrupt;

   -----------------------
   -- Interrupt_Wrapper --
   -----------------------

   procedure Interrupt_Wrapper
     (Vector : System.BB.Parameters.Range_Of_Vector)
   is
      Self_Id                  : constant Threads.Thread_Id :=
                                   Threads.Thread_Self;
      Caller_Priority          : constant Any_Priority :=
                                   Threads.Get_Priority (Self_Id);
      Interrupt                : constant Interrupts.Interrupt_ID :=
                                   Peripherals.To_Interrupt (Vector);
      Previous_Interrupt_ID : constant Interrupt_ID :=
                                   Interrupt_Being_Handled;

   begin
      --  This must be an external interrupt

      pragma Assert (Interrupt /= Interrupts.No_Interrupt);

      --  Store the interrupt being handled

      Interrupt_Being_Handled := Interrupt;

      --  Then, we must set the appropriate software priority corresponding
      --  to the interrupt being handled. It comprises also the appropriate
      --  interrupt masking.

--        Put_Line ("[SYSTEM.BB.INTERRUPTS.INTERRUPT_WRAPPER] " &
--                    "****** ENTERING INTERRUPT ******");
      Threads.Queues.Change_Priority
        (Self_Id, Priority_Of_Interrupt);

      --  Call the user handler

      Interrupt_Handlers_Table (Interrupt).all (Interrupt);

      --  Restore the software priority to the state before the interrupt
      --  happened. Interrupt unmasking is not done here (it will be done
      --  later by the interrupt epilogue).

      CPU_Primitives.Disable_Interrupts;
      Threads.Queues.Change_Priority (Self_Id, Caller_Priority);

--        if Self_Id.Extraction_Pending then
--           Self_Id.State := Threads.Suspended;
--           Threads.Queues.Extract (Self_Id);
--           Self_Id.Extraction_Pending := False;
--        end if;

      --  Restore the interrupt that was being handled previously (if any)

--        Put_Line ("[SYSTEM.BB.INTERRUPTS.INTERRUPT_WRAPPER] " &
--                    "****** EXITING INTERRUPT ******");
      Interrupt_Being_Handled := Previous_Interrupt_ID;

   end Interrupt_Wrapper;

   ----------------------------
   -- Within_Interrupt_Stack --
   ----------------------------

   function Within_Interrupt_Stack
     (Stack_Address : System.Address) return Boolean
   is
      Interrupt_Handled : constant Interrupt_ID := Current_Interrupt;
      Stack_Start       : System.Address;
      Stack_End         : System.Address;

   begin
      if Interrupt_Handled = No_Interrupt then

         --  Return False if no interrupt is being handled

         return False;
      else
         --  Calculate stack boundaries for the interrupt being handled

         Stack_Start :=
           Interrupt_Stack (Stack_Space'First)'Address;
         Stack_End   :=
           Interrupt_Stack (Stack_Space'Last)'Address;

         --  Compare the Address passed as argument against the
         --  previosly calculated stack boundaries.

         return Stack_Address >= Stack_Start
           and then Stack_Address <= Stack_End;
      end if;

   end Within_Interrupt_Stack;

   ---------------------------
   -- Initialize_Interrupts --
   ---------------------------

   procedure Initialize_Interrupts is
   begin

      --  Store the pointer to the last double word

      Interrupt_Stack_Address :=
        Interrupt_Stack (Stack_Space'Last - 7)'Address;
   end Initialize_Interrupts;

end System.BB.Interrupts;
