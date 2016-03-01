------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                         S Y S T E M . B B . T I M E                      --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2004 The European Space Agency            --
--                     Copyright (C) 2003-2011, AdaCore                     --
--           Copyright (C) 2011 Universidad Politecnica de Madrid           --
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

--  Package in charge of implementing clock and timer functionalities

pragma Restrictions (No_Elaboration_Code);

with System.BB.Peripherals;
--  Used for Clock_Freq_Hz

package System.BB.Time is
   pragma Preelaborate;

   type Time is mod 2 ** 64;
   for Time'Size use 64;
   --  Time is represented at this level as a 64-bit natural number. The
   --  upper 40 bits representing the number of clock periods (MSP), and the
   --  lower 24 bits containing the number of hardware clock ticks (LSP).

   type Time_Span is range -2 ** 63 .. 2 ** 63 - 1;
   for Time_Span'Size use 64;
   --  Time_Span represents the length of time intervals, and it is
   --  defined as a 64-bit signed integer.

   ---------------
   -- Constants --
   ---------------
   Time_Span_Zero : constant Time_Span := 0;

   Tick : constant := 1;
   --  A clock tick is a real time interval during which the clock value (as
   --  observed by calling the Clock function) remains constant. Tick is the
   --  average length of such intervals.

   Ticks_Per_Second : constant := System.BB.Peripherals.Timer_Freq_Hz;
   --  Number of ticks (or clock interrupts) per second

   --------------------
   -- Initialization --
   --------------------

   procedure Initialize_Timers;
   --  Initialize this package (clock and alarm handlers). Must be called
   --  before any other functions.

   ----------------
   -- Operations --
   ----------------

   function Number_Of_Ticks_Per_Second return Time;
   pragma Export (C, Number_Of_Ticks_Per_Second, "system_bb_ticks_per_second");
   --  Get the number of ticks (or clock interrupts) per second

   function Clock return Time;
   pragma Export (C, Clock, "system_bb_clock");
   --  Get the number of ticks elapsed since startup

   procedure Delay_Until (T : Time);
   pragma Export (C, Delay_Until, "system_bb_delay_until");
   --  Suspend the calling thread until the absolute time specified by T

   function "+" (Left  : Time; Right : Time_Span) return Time;

   function Get_Pending_Alarm return Boolean;
   --  Returns Pending_Alarm variable

   procedure Turn_True_Pending_Alarm;
   --  Turns Pending_Alarm variable to true

   procedure Inmediate_Alarm (Now : in out System.BB.Time.Time);

end System.BB.Time;
