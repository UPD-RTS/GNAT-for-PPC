------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                  S Y S T E M . B B . P E R I P H E R A L S               --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2006 The European Space Agency            --
--                     Copyright (C) 2003-2007, AdaCore                     --
--           Copyright (C) 2006 Universidad Politecnica de Madrid           --
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

--  This package defines constants and primitives used for handling the
--  peripherals available in the target board.
--  This is the PPC version of this package.

pragma Restrictions (No_Elaboration_Code);

with System;
--  Used for Any_Priority

with System.BB.Parameters;
--  Used for Clock_Frequency
--  Interrupt_Level

with Ada.Unchecked_Conversion, Interfaces;

package System.BB.Peripherals is
   pragma Preelaborate;

   package SBP renames System.BB.Parameters;

   -----------------------------
   -- Hardware initialization --
   -----------------------------

   procedure Initialize_Board;
   --  Procedure that performs the hardware initialization of the board.
   --  Should be called before any other operations in this package.

   ------------------------------------------------
   -- Clock and timer definitions and primitives --
   ------------------------------------------------

   --  MHz
   --  Clock_Freq_Hz : constant Positive := SBP.Clock_Frequency * 10**6;
   --  GHz
   Clock_Freq_Hz : constant Positive := SBP.Clock_Frequency * 10**3;
   --  Clock frequency expressed in Hz

   --  Preescaler should be the same as for SPARC (i.e., 4)
   --  and Clock_Freq_Hz should be divided by the Preescaler
   --  However this does not apply to PROARTIS Sim model
   Preescaler :  constant Positive := 1;
   Timer_Freq_Hz : constant Positive := Clock_Freq_Hz;

   type Timer_Interval is mod 2**64;
   --  This type represents any interval that we can measure within a
   --  Clock_Interrupt_Period. (64-bit unsigned)

   procedure Set_Alarm (Ticks : Timer_Interval);
   --  Set an alarm that will expire after the specified number of clock ticks

   procedure Cancel_And_Set_Alarm (Ticks : Timer_Interval);
   --  Set a new alarm that will expire after the specified number of clock
   --  ticks, and cancel any previous alarm set.

   procedure Set_Partition_Alarm (Ticks : Timer_Interval);
   --  Set an alarm that will expire after the specified number of partition
   --  clock ticks

   procedure Cancel_And_Set_Partition_Alarm (Ticks : Timer_Interval);
   --  Set a new alarm that will expire after the specified number of
   --  partition clock ticks, and cancel any previous alarm set.

   function Read_Clock return Timer_Interval;
   pragma Inline (Read_Clock);
   --  Read the Xtratum global Clock.
   --  Read the 24-bit value contained in the clock hardware counter, and
   --  return the number of ticks elapsed since the last clock interrupt, that
   --  is, since the clock counter was last reloaded.

   procedure Clear_Alarm_Interrupt;
   pragma Inline (Clear_Alarm_Interrupt);
   --  Acknowledge the alarm interrupt

   procedure Clear_Clock_Interrupt;
   pragma Inline (Clear_Clock_Interrupt);
   --  Acknowledge the clock interrupt

   ----------------
   -- Interrupts --
   ----------------

   function To_Vector
     (Level : SBP.Interrupt_ID) return SBP.Range_Of_Vector;
   pragma Inline (To_Vector);
   --  Function to translate interrupt levels into interrupt vector entries

   function To_Interrupt
     (Vector : SBP.Range_Of_Vector) return SBP.Interrupt_ID;
   pragma Inline (To_Interrupt);
   --  Function to translate interrupt vector entries into their
   --  corresponding interrupt ID. If the trap does not correspond
   --  to an external interrupt (that is, if it is a synchronous trap)
   --  then interrupt ID 0 is returned.

   Priority_Of_Interrupt : constant System.Any_Priority := Any_Priority'Last;
   --  Function to obtain the priority associated to an interrupt. It returns
   --  System.Any_Priority'First if Level is equal to zero (no interrupt).

   --  Constants defining the external interrupts

   --------------------------------------
   -- PPC Platform Specific Interrupts --
   --------------------------------------

   System_Reset                : constant SBP.Interrupt_ID :=  1; -- 16#0100#;
   Machine_Check               : constant SBP.Interrupt_ID :=  2; -- 16#0200#;
   Data_Storage                : constant SBP.Interrupt_ID :=  3; -- 16#0300#;
   Instruction_Storage         : constant SBP.Interrupt_ID :=  4; -- 16#0400#;
   External_Interrupt          : constant SBP.Interrupt_ID :=  5; -- 16#0500#;
   Alignment                   : constant SBP.Interrupt_ID :=  6; -- 16#0600#;
   Program                     : constant SBP.Interrupt_ID :=  7; -- 16#0700#;
   FP_Unavailable              : constant SBP.Interrupt_ID :=  8; -- 16#0800#;
   Timer_1                     : constant SBP.Interrupt_ID :=  9; -- 16#0900#;
   System_Call                 : constant SBP.Interrupt_ID := 10; -- 16#0C00#;
   --  Use the next free slot in the vector table
   Timer_2                     : constant SBP.Interrupt_ID :=  11; -- 16#0000#;
   --  APU_unavailable             : constant SBP.Interrupt_ID := xxx;
   --  Watchdog_timer              : constant SBP.Interrupt_ID := xxx;
   --  Data_TLB_miss               : constant SBP.Interrupt_ID := xxx;
   --  Instruction_TLB_Miss        : constant SBP.Interrupt_ID := xxx;
   --  Debug                       : constant SBP.Interrupt_ID := xxx;
   --  External_Interrupt          : constant SBP.Interrupt_ID := xxx;
   --  UART_1_RX_TX                : constant SBP.Interrupt_ID := xxx;
   --  UART_2_RX_TX                : constant SBP.Interrupt_ID := xxx;

   --  Reserved_0                 : constant SBP.Interrupt_ID := 16#0000#;
   --  Reserved_A000              : constant SBP.Interrupt_ID := 16#0a00#;
   --  Reserved_B000              : constant SBP.Interrupt_ID := 16#0B00#;
   --  Trace_B000                 : constant SBP.Interrupt_ID := 16#0D00#;

end System.BB.Peripherals;
