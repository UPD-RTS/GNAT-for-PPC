------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                    S Y S T E M . O S _ I N T E R F A C E                 --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--             Copyright (C) 1995-2009, Free Software Foundation, Inc.      --
--           Copyright (C) 2006-2009, Universidad Politecnica de Madrid     --
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
------------------------------------------------------------------------------

--  This is the Ravenscar version of this package for a bare board PPC target

--  This package encapsulates all direct interfaces to OS services
--  that are needed by the tasking run-time (libgnarl).

pragma Restrictions (No_Elaboration_Code);

with System.Parameters;
with System.BB.Threads;
with System.BB.Time;
with System.BB.Interrupts;
with System.BB.Peripherals;

package System.OS_Interface is
   pragma Preelaborate;

   package SBP renames System.BB.Peripherals;

   ----------------
   -- Interrupts --
   ----------------

   Max_Interrupt : constant := System.BB.Interrupts.Max_Interrupt;
   --  Number of asynchronous interrupts

   subtype Interrupt_ID is System.BB.Interrupts.Interrupt_ID;
   --  Interrupt identifiers

   No_Interrupt : constant := System.BB.Interrupts.No_Interrupt;
   --  Special value indicating no interrupt

   subtype Interrupt_Handler is System.BB.Interrupts.Interrupt_Handler;
   --  Interrupt handlers

   --------------------------------------
   -- PPC Platform Specific Interrupts --
   --------------------------------------

   --  Only one priority level is supported

   System_Reset                 : constant Interrupt_ID := SBP.System_Reset;
   System_Reset_Priority        : constant System.Interrupt_Priority :=
     Interrupt_Priority'First;

   Machine_Check                : constant Interrupt_ID := SBP.Machine_Check;
   Machine_Check_Priority       : constant System.Interrupt_Priority :=
     Interrupt_Priority'First;

   Data_Storage                 : constant Interrupt_ID := SBP.Data_Storage;
   Data_Storage_Priority        : constant System.Interrupt_Priority :=
     Interrupt_Priority'First;

   Instruction_Storage          : constant Interrupt_ID :=
     SBP.Instruction_Storage;
   Instruction_Storage_Priority : constant System.Interrupt_Priority :=
     Interrupt_Priority'First;

   External_Interrupt           : constant Interrupt_ID :=
     SBP.External_Interrupt;
   External_Priority            : constant System.Interrupt_Priority :=
     Interrupt_Priority'First;

   Alignment                    : constant Interrupt_ID := SBP.Alignment;
   Alignment_Priority           : constant System.Interrupt_Priority :=
     Interrupt_Priority'First;

   Program                      : constant Interrupt_ID := SBP.Program;
   Program_Priority             : constant System.Interrupt_Priority :=
     Interrupt_Priority'First;

   FP_Unavailable               : constant Interrupt_ID := SBP.FP_Unavailable;
   FP_Unavailable_Priority      : constant System.Interrupt_Priority :=
     Interrupt_Priority'First;

   System_Call                 : constant Interrupt_ID := SBP.System_Call;
   System_Call_Priority       : constant System.Interrupt_Priority :=
     Interrupt_Priority'First;

   -----------------------
   -- Timers Interrupts --
   -----------------------

   Timer_2 : constant Interrupt_ID :=
     System.BB.Peripherals.Timer_2;
   Timer_2_Priority : constant System.Interrupt_Priority :=
     System.Interrupt_Priority'First;

   Timer_1 : constant Interrupt_ID :=
     System.BB.Peripherals.Timer_1;
   Timer_1_Priority : constant System.Interrupt_Priority :=
     System.Interrupt_Priority'First;

   ---------------------
   -- UART Interrupts --
   ---------------------

   --  UART_1_RX_TX          : constant := SBP.UART_1_RX_TX;
   --  UART_1_RX_TX_Priority : constant := Interrupt_Priority'First;
   --  One priority level is supported

   --  UART_2_RX_TX          : constant := SBP.UART_2_RX_TX;
   --  UART_2_RX_TX_Priority : constant := Interrupt_Priority'First;
   --  One priority level is supported

   --------------------------
   -- Interrupt processing --
   --------------------------

   function Current_Interrupt return Interrupt_ID
     renames System.BB.Interrupts.Current_Interrupt;
   --  Function that returns the hardware interrupt currently being
   --  handled (if any). In case no hardware interrupt is being handled
   --  the returned value is No_Interrupt.

   --  function Priority_Of_Interrupt (Id : Interrupt_ID) return Any_Priority
   --  renames System.BB.Interrupts.Priority_Of_Interrupt;
   --  Obtain the software priority of any hardware interrupt. This makes
   --  easier the selection of the priority of the protected handler
   --  attached to interrupts.

   procedure Attach_Handler
     (Handler : Interrupt_Handler;
      Id      : Interrupt_ID) renames System.BB.Interrupts.Attach_Handler;
   --  Attach a handler to a hardware interrupt

   ----------
   -- Time --
   ----------

   subtype Time is System.BB.Time.Time;
   --  Representation of the time in the underlying tasking system

   subtype Time_Span is System.BB.Time.Time_Span;
   --  Represents the length of time intervals in the underlying tasking
   --  system.

   Ticks_Per_Second : constant Time := System.BB.Time.Ticks_Per_Second;
   --  Number of ticks (or clock interrupts) per second

   function Clock return Time renames System.BB.Time.Clock;
   --  Get the number of ticks elapsed since startup

   procedure Delay_Until (T : Time) renames System.BB.Time.Delay_Until;
   --  Suspend the calling task until the absolute time specified by T

   -------------
   -- Threads --
   -------------

   subtype Thread_Descriptor is System.BB.Threads.Thread_Descriptor;
   --  Type that contains the information about a thread (registers,
   --  priority, etc.).

   subtype Thread_Id is System.BB.Threads.Thread_Id;
   --  Identifiers for the underlying threads

   Null_Thread_Id : constant Thread_Id := null;
   --  Identifier for a non valid thread

   procedure Initialize
     (Environment_Thread : Thread_Id;
      Main_Priority      : System.Any_Priority)
     renames System.BB.Threads.Initialize;
   --  Procedure for initializing the underlying tasking system

   procedure Thread_Create
     (Id            : Thread_Id;
      Code          : System.Address;
      Arg           : System.Address;
      Priority      : System.Any_Priority;
      Stack_Address : System.Address;
      Stack_Size    : System.Parameters.Size_Type;
      Task_Args     : System.Address)
     renames System.BB.Threads.Thread_Create;
   --  Create a new thread

   function Thread_Self return Thread_Id renames System.BB.Threads.Thread_Self;
   --  Return the thread identifier for the calling task

   ----------
   -- ATCB --
   ----------

   procedure Set_ATCB (ATCB : System.Address)
     renames System.BB.Threads.Set_ATCB;
   --  Associate the specified ATCB to the currently running thread

   function Get_ATCB return System.Address renames System.BB.Threads.Get_ATCB;
   --  Get the ATCB associated to the currently running thread

   ----------------
   -- Scheduling --
   ----------------

   procedure Set_Priority (Priority  : System.Any_Priority)
     renames System.BB.Threads.Set_Priority;
   --  Set the active priority of the executing thread to the given value

   function Get_Priority  (Id : Thread_Id) return System.Any_Priority
     renames System.BB.Threads.Get_Priority;
   --  Get the current base priority of a thread

   procedure Sleep renames System.BB.Threads.Sleep;
   --  The calling thread is unconditionally suspended

   procedure Wakeup (Id : Thread_Id) renames System.BB.Threads.Wakeup;
   --  The referred thread becomes ready (the thread must be suspended)

end System.OS_Interface;
