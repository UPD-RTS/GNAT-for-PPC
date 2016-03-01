------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                  S Y S T E M . B B . P E R I P H E R A L S               --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2006 The European Space Agency            --
--                     Copyright (C) 2003-2007, AdaCore                     --
--           Copyright (C) 2006 Universidad Politecnica de Madrid           --
--         Copyright (C) 2012-2013 Università degli Studi di Padova         --
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

--  This is the PPC version of this package.

pragma Restrictions (No_Elaboration_Code);

with System.BB.Peripherals.Registers;
with System.Machine_Code;
with System.BB.Serial_Output;

package body System.BB.Peripherals is

   use type Registers.Scaler_12;
   use type Registers.Scaler_10;
   use type Registers.Timer_24;

   --  The following types are used to read from the TimeBase register and
   --  naturally switch from 32bit to 64bit representation and vice-versa

   --  Simple 32bit modular type
   type Time_Base_32 is mod 2**32;

   --  Structured type to hold the 64bit time base
   type Split_Time is
      record
         TBU : Time_Base_32;
         TBL : Time_Base_32;
      end record;

   for Split_Time use
      record
         TBU at 0 range 0 .. 31;
         TBL at 4 range 0 .. 31;
      end record;

   --  function To_Time is new Ada.Unchecked_Conversion (Split_Time, Time);
   --  Function to change the view from Split_Time (a record with two
   --  unsigned 32 bits fields) to Time (unsigned 64-bit).

   --  function To_Split_Time is new
   --    Ada.Unchecked_Conversion (Time, Split_Time);
   --  Function to change the view from Time (unsigned 64-bit) to
   --  Split_Time (a record with two unsigned 32 bits fields).

   function To_Timer_Interval is
     new Ada.Unchecked_Conversion (Split_Time, Timer_Interval);
   --  Function to change the view from Split_Time (a record with two
   --  unsigned 32 bits fields) to Time (unsigned 64-bit).

   function To_Split_Time is
     new Ada.Unchecked_Conversion (Timer_Interval, Split_Time);
   --  Function to change the view from Time_Interval (unsigned 64-bit) to
   --  Split_Time (a record with two unsigned 32 bits fields).

   ----------------------
   -- Local Procedures --
   ----------------------

   procedure Initialize_Memory;
   pragma Inline (Initialize_Memory);
   --  Initialize the memory on the board

   procedure Initialize_Clock;
   --  Perform all the initialization related to the clock

   --------------------------
   -- Cancel_And_Set_Alarm --
   --------------------------

   procedure Cancel_And_Set_Alarm (Ticks : Timer_Interval) renames
     Set_Alarm;
   --  This procedure cancel a previous alarm and set a new one.
   --  Setting a new alarm cancel the previous one in this target
   --  So Cancel_And_Set_Alarm and Set_Alarm are identical.

   --------------------------------------
   --  Cancel_And_Set_Partition_Alarm  --
   --------------------------------------

   procedure Cancel_And_Set_Partition_Alarm (Ticks : Timer_Interval) renames
     Set_Partition_Alarm;
   --  This procedure cancel a previous alarm and set a new one.
   --  Setting a new alarm cancel the previous one in this target
   --  so Cancel_And_Set_Partition_Alarm and Set_Partition_Alarm are identical

   --------------------------
   -- Clear_Alarm_Interupt --
   --------------------------

   procedure Clear_Alarm_Interrupt is
   begin
      --  From MEC Specification Document (MCD/SPC/0009/SE) page 35.

      --  The MEC includes a specific register called Interrupt Pending
      --  Register, which reflects the pending interrupts.

      --  The interrupts in the IPR are cleared automatically when the
      --  interrupt is acknowledged. The MEC will sample the trap address in
      --  order to know which bit to clear. Therefore, this procedure has a
      --  null body for this target.

      null;
   end Clear_Alarm_Interrupt;

   --------------------------
   -- Clear_Clock_Interupt --
   --------------------------

   procedure Clear_Clock_Interrupt is
   begin
      --  From MEC Specification Document (MCD/SPC/0009/SE) page 35.

      --  The MEC includes a specific register called Interrupt Pending
      --  Register, which reflects the pending interrupts.

      --  The interrupts in the IPR are cleared automatically when the
      --  interrupt is acknowledged. The MEC will sample the trap address in
      --  order to know which bit to clear. Therefore, this procedure has a
      --  null body for this target.

      null;
   end Clear_Clock_Interrupt;

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
   begin
      --  The initialization of the LEON board consists
      --  on initializing the memory and initializing the clock
      --  in order to have the desired granularity and range.

      Initialize_Memory;
      Initialize_Clock;
   end Initialize_Board;

   ----------------------
   -- Initialize_Clock --
   ----------------------

   procedure Initialize_Clock is

   begin
      null;
   end Initialize_Clock;

   -----------------------
   -- Initialize_Memory --
   -----------------------

   procedure Initialize_Memory is

   begin
      --  Nothing to be done for now
      --  TODO Clear memory, enable caches here instead of in the boot seq..
      null;
   end Initialize_Memory;

   ----------------
   -- Read_Clock --
   ----------------

--     function Read_Clock return Timer_Interval is
--        pragma Style_Checks (Off);
--        Result  : Split_Time;
--        tempTBU : Time_Base_32;
--        use System.Machine_Code;
--        use Peripherals, Interfaces;
--     begin
--        --  A clock interrupt has occurred after reading Result.TBU. Hence,
--        --  Result might need to be be adjusted.
--        Asm ("1:"          & ASCII.LF & ASCII.HT &   --  Branch label
--            "mftbu %0"    & ASCII.LF & ASCII.HT &   --  Get TBU
--            "mftb  %0"    & ASCII.LF & ASCII.HT &   --  Get TBL
--            "mftbu %2"    & ASCII.LF & ASCII.HT &   --  Get TBU
--            "cmpw  %2,%0" & ASCII.LF & ASCII.HT &   --  Repeat if TBU changed
--            "bne   1b",
--          Outputs => (Time_Base_32'Asm_Output ("=r", Result.TBU),
--                      Time_Base_32'Asm_Output ("=r", Result.TBL),
--                      Time_Base_32'Asm_Output ("=r", tempTBU)),
--          Volatile => True
--         );
--        return To_Timer_Interval (Result);
--     end Read_Clock;

   function Read_Clock return Timer_Interval is
      pragma Style_Checks (Off);
      Result  : Split_Time;
      use System.Machine_Code;
      use Peripherals, Interfaces;
   begin
      --  A clock interrupt has occurred after reading Result.TBU. Hence,
      --  Result might need to be be adjusted.
      Asm (
           "mftb  %0"    & ASCII.LF & ASCII.HT,
           Outputs => (Time_Base_32'Asm_Output ("=r", Result.TBL)),
           Volatile => True
      );
      return To_Timer_Interval (Result);
   end Read_Clock;

   ---------------
   -- Set_Alarm --
   ---------------

   procedure Set_Alarm (Ticks : Timer_Interval) is
      use System.Machine_Code, System.BB.Serial_Output;
      split : Split_Time;
   begin
      --  TODO Check Ticks <= 2^32 -1 --> Raise except
      split := To_Split_Time (Ticks);
      --  Put_Line ("Setting decrementer " & Time_Base_32'Image (split.TBL));
      Asm ("mtdec %0",
        Inputs => (Time_Base_32'Asm_Input ("r", split.TBL)), --  %0=tmpTBU
        Volatile => True
      );
   end Set_Alarm;

   -------------------------
   -- Set_Partition_Alarm --
   -------------------------

   procedure Set_Partition_Alarm (Ticks : Timer_Interval) is
   begin
      null;
   end Set_Partition_Alarm;

   ------------------
   -- To_Interrupt --
   ------------------

   function To_Interrupt
     (Vector : SBP.Range_Of_Vector) return SBP.Interrupt_ID
   is
   begin
      --  The range corresponding to asynchronous traps is between
      --  16#11# and 16#1F#.
      --  The range corresponding to asynchronous virtual traps is
      --  between 16#E0# and 16#FF#
      --  pragma Assert ((Vector >= 16#11# and then Vector <= 16#1F#)
      --  or (Vector >= 16#E0# and then Vector <= 16#FF#));

      --  if Vector >= 16#E0# then
      --     return Vector - 16#C0#;
      --  else
      --     return Vector - 16#10#;
      --  end if;
      return Vector;
   end To_Interrupt;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (Level : SBP.Interrupt_ID) return SBP.Range_Of_Vector
   is
   begin
      return Level;
      --  if Level >= 32 then
      --     return Level + 16#C0#;
      --  else
      --   return Level + 16#10#;
      --  end if;
   end To_Vector;

end System.BB.Peripherals;
