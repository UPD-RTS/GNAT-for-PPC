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
--                     Copyright (C) 2003-2011, AdaCore                     --
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

--  This is the LEON3 version of this package

pragma Restrictions (No_Elaboration_Code);

with System.BB.Peripherals.LEON_3;
with System.Multiprocessors;

package body System.BB.Peripherals is

   use CPU_Primitives;
   use System.BB.Peripherals.LEON_3;
   use System.Multiprocessors;

   -----------------------
   -- Local Definitions --
   -----------------------

   Prescaler_Min : constant := 4;
   --  In order to obtain the highest granularity of the clock we set the
   --  minimum allowed prescaler division factor, which is 5, corresponding
   --  to a prescaler reload register value of 4.

   Periodic_Count : constant := Timer_Interval'Last - 1;
   --  Value to be loaded in the clock counter to accomplish the
   --  Clock_Interrupt_Period.
   --
   --  One is subtracted from Timer_Interval'Last because the timeout period
   --  will count an extra cycle for reloading the counter.

   Timer_1_Interrupt : constant SBP.Interrupt_Level := 6; --  alarm
   Timer_2_Interrupt : constant SBP.Interrupt_Level := 7; --  real time clock

   Poke_Interrupt    : constant SBP.Interrupt_Level := 14;
   --  Use interrupt 14 because 15 is unmaskable with PIL field of PSR register
   --  (see SPARCv8 manual 7.3. Trap Control).
   --
   --  The value is copied in trap_handler-bb-sparc.S

   Alarm_CPU : constant System.Multiprocessors.CPU :=
     System.Multiprocessors.CPU'First;
   --  CPU which will handle the alarm interrupt

   Clock_CPU : constant System.Multiprocessors.CPU :=
     System.Multiprocessors.CPU'First;
   --  CPU which will handle the clock interrupt

   subtype UART_Baudrate is Natural;
   --  The range of baudrates supported by the UART depends on the range of
   --  the scaler, which is a 12-bit value ranging from 0 to 2 ** 12 - 1, and
   --  on the system clock frequency.

   type UART_Parity is (None, Even, Odd);
   --  Parity values

   ----------------------
   -- Local Procedures --
   ----------------------

   procedure Initialize_Memory;
   pragma Inline (Initialize_Memory);
   --  Initialize the memory on the board

   procedure Initialize_Clock;
   --  Perform all the initialization related to the clock

   procedure Initialize_UART
     (Baudrate     : UART_Baudrate;
      Parity       : UART_Parity);
   --  Procedure to initialize the UART port

   ------------------------
   -- Alarm_Interrupt_ID --
   ------------------------

   function Alarm_Interrupt_ID return Interrupts.Interrupt_ID is
   begin
      return Timer_1_Interrupt;
   end Alarm_Interrupt_ID;

   --------------------------
   -- Cancel_And_Set_Alarm --
   --------------------------

   procedure Cancel_And_Set_Alarm (Ticks : Timer_Interval) renames Set_Alarm;
   --  This procedure cancel a previous alarm and set a new one. Setting a new
   --  alarm cancel the previous one in this target So Cancel_And_Set_Alarm and
   --  Set_Alarm are identical.

   ---------------------------
   -- Clear_Alarm_Interrupt --
   ---------------------------

   procedure Clear_Alarm_Interrupt is
   begin
      --  Interrupts are cleared automatically when they are acknowledged

      null;
   end Clear_Alarm_Interrupt;

   ---------------------------
   -- Clear_Clock_Interrupt --
   ---------------------------

   procedure Clear_Clock_Interrupt is
   begin
      --  Interrupts are cleared automatically when they are acknowledged

      null;
   end Clear_Clock_Interrupt;

   --------------------------
   -- Clear_Poke_Interrupt --
   --------------------------

   procedure Clear_Poke_Interrupt is
   begin
      --  Interrupts are cleared automatically when they are acknowledged

      null;
   end Clear_Poke_Interrupt;

   ------------------------
   -- Clock_Interrupt_ID --
   ------------------------

   function Clock_Interrupt_ID return Interrupts.Interrupt_ID is
   begin
      return Timer_2_Interrupt;
   end Clock_Interrupt_ID;

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
   begin
      --  The initialization of the LEON board consists on initializing the
      --  memory, and initializing the clock in order to have the desired
      --  granularity and range.

      Initialize_Memory;
      Initialize_Clock;

      if System.BB.Parameters.Multiprocessor then
         --  Enable Poke interrupts for all CPUs

         for CPU_Id in CPU loop
            Interrupt_Mask (CPU_Id) :=
              Interrupt_Mask (CPU_Id) or 2**Poke_Interrupt;
         end loop;
      end if;

   end Initialize_Board;

   ----------------------
   -- Initialize_Clock --
   ----------------------

   procedure Initialize_Clock is
      Prescaler               : constant Prescaler_Register :=
        (Value => Prescaler_Min, Reserved => (others => False));
      --  Minimum prescaler to be used to achieve best granularity

      Periodic_Mode : constant Timer_Control_Register :=
        (Enable            => True,
         Reload_Counter    => True,
         Load_Counter      => True,
         Interrupt_Enable  => True,
         Interrupt_Pending => False,
         Chain             => False,
         Debug_Halt        => False,
         Reserved => (others => False));

   begin
      --  Set the prescaler value to achieve the required granularity

      Prescaler_Reload := Prescaler;

      --  Load the counter for the real-time clock

      Timer_2_Reload := Periodic_Count;

      --  Program the timer in periodic mode to serve as a clock

      Timer_2_Control := Periodic_Mode;

      --  Enable clock interrupts

      Interrupt_Mask (Clock_CPU) :=
        Interrupt_Mask (Clock_CPU) or 2**Timer_2_Interrupt;
   end Initialize_Clock;

   ------------------------
   -- Initialize_Console --
   ------------------------

   procedure Initialize_Console is
   begin
      --  Initialize the UART as output console

      Initialize_UART (115200, None);
   end Initialize_Console;

   -----------------------
   -- Initialize_Memory --
   -----------------------

   procedure Initialize_Memory is
   begin
      --  Nothing to be done for LEON3

      null;
   end Initialize_Memory;

   ---------------------
   -- Initialize_UART --
   ---------------------

   procedure Initialize_UART
     (Baudrate     : UART_Baudrate;
      Parity       : UART_Parity)
   is
      Control : UART_Control_Register;
      Scaler  : UART_Scaler_Register;

   begin
      --  Read the Control Register

      Control := UART_Control;

      --  Skip initialization if if the UART has been already initialized
      --  (for example, by the remote monitor).

      if Control.Transmitter_Enable or Control.Receiver_Enable then
         return;
      end if;

      --  Set the UART scaler according to the baudrate given

      Scaler :=
        (UART_Scaler =>
              Scaler_12
                ((SBP.Clock_Frequency * 10 / (Baudrate * 8) - 5) / 10),
            Reserved    => (others => False));

      --  Enable TX and RX and disable interrupts

      Control :=
        (Receiver_Enable    => True,
         Transmitter_Enable => True,
         Parity_Select      => Parity_Kind'Val (0),
         Reserved_3         => (others => False),
         others             => False);

      --  Set the requested parity checking

      case Parity is
         when None =>
            null;

         when Even =>
            Control.Parity_Enable := True;
            Control.Parity_Select := Even;

         when Odd  =>
            Control.Parity_Enable := True;
            Control.Parity_Select := Odd;
      end case;

      --  Write to the Control and Scaler Registers

      UART_Control := Control;
      UART_Scaler  := Scaler;
   end Initialize_UART;

   ------------------------
   -- Max_Timer_Interval --
   ------------------------

   function Max_Timer_Interval return Timer_Interval is
   begin
      return Timer_Interval'Last;
   end Max_Timer_Interval;

   -----------------------
   -- Poke_Interrupt_ID --
   -----------------------

   function Poke_Interrupt_ID return Interrupts.Interrupt_ID is
   begin
      return Poke_Interrupt;
   end Poke_Interrupt_ID;

   ----------------
   -- Read_Clock --
   ----------------

   function Read_Clock return Timer_Interval is
   begin
      return Periodic_Count - Timer_2_Counter;
   end Read_Clock;

   ---------------
   -- Set_Alarm --
   ---------------

   procedure Set_Alarm (Ticks : Timer_Interval) is
      One_Shot_Mode : constant Timer_Control_Register :=
        (Enable            => True,
         Reload_Counter    => False,
         Load_Counter      => True,
         Interrupt_Enable  => True,
         Interrupt_Pending => False,
         Chain             => False,
         Debug_Halt        => False,
         Reserved => (others => False));

   begin
      --  Alarm Clock downcount will reach 0 in Ticks. The granularity of
      --  time intervals is equal to Clock Period.

      --  Set the prescaler: already done in Initialize_Clock

      --  Load the counter

      Timer_1_Reload := Ticks;

      --  Program the timer in one-shot mode

      Timer_1_Control := One_Shot_Mode;

      --   Enable Timer 1 Interrupts

      Interrupt_Mask (Alarm_CPU) :=
        Interrupt_Mask (Alarm_CPU) or 2**Timer_1_Interrupt;
   end Set_Alarm;

   ----------------------
   -- Ticks_Per_Second --
   ----------------------

   function Ticks_Per_Second return Natural is
   begin
      --  The prescaler is clocked by the system clock. When it underflows, it
      --  is reloaded from the prescaler reload register and a timer tick is
      --  generated. The effective division rate is therefore equal to the
      --  prescaler reload register value plus 1.

      return System.BB.Parameters.Clock_Frequency / (Prescaler_Min + 1);
   end Ticks_Per_Second;

   ---------------------------
   -- Get_Interrupt_Request --
   ---------------------------

   function Get_Interrupt_Request
     (Vector : CPU_Primitives.Vector_Id) return SBP.Interrupt_Level
   is
   begin
      --  The range corresponding to asynchronous traps is between
      --  16#11# and 16#1F#.

      pragma Assert (Vector >= 16#11# and then Vector <= 16#1F#);

      return SBP.Interrupt_Level (Vector - 16#10#);
   end Get_Interrupt_Request;

   ----------------
   -- Get_Vector --
   ----------------

   function Get_Vector
     (Interrupt : SBP.Interrupt_Level) return CPU_Primitives.Vector_Id
   is
   begin
      return CPU_Primitives.Vector_Id (Interrupt + 16#10#);
   end Get_Vector;

   ------------------
   -- Console_Send --
   ------------------

   procedure Console_Send (Char    : Character)
   is
      UART_Tx         : constant UART_Data_Register :=
        (FIFO => Char, Reserved => (others => False));

   begin
      --  Send the character through the selected channel by polling. Wait
      --  until the port is ready to load data.

      loop
         exit when not UART_Status.Transmitter_FIFO_Full;
      end loop;

      --  Send the character once the UART is ready

      UART_Data := UART_Tx;

   end Console_Send;

end System.BB.Peripherals;
