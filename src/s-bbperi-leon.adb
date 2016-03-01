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

pragma Restrictions (No_Elaboration_Code);

with System.BB.Peripherals.Registers;
--  Used for LEON Registers

package body System.BB.Peripherals is

   use type Registers.Scaler_12;
   use type Registers.Scaler_10;
   use type Registers.Timer_24;

   -----------------------
   -- Local Definitions --
   -----------------------

   Periodic_Scaler : constant Registers.Scaler_10 :=
                      Registers.Scaler_10 (Preescaler - 1);

   Periodic_Count : constant Registers.Timer_24 :=
                      Registers.Timer_24'Last - 1;
   --  Value to be loaded in the clock counter to accomplish the
   --  Clock_Interrupt_Period.
   --
   --  One is subtracted from Timer_24'Last because when the Scaler is
   --  set to 0, the timeout period will be the counter reload value plus 1.

   --  Timer_Control registers cannot be read. So the following objects
   --  hold a copy of the Timer_Control registers value.
   Timer_1_Control_Mirror : Registers.Timer_Control_Register;
   pragma Volatile (Timer_1_Control_Mirror);

   Timer_2_Control_Mirror : Registers.Timer_Control_Register;
   pragma Volatile (Timer_2_Control_Mirror);

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

   procedure Cancel_And_Set_Alarm (Ticks : Timer_Interval) renames Set_Alarm;
   --  This procedure cancel a previous alarm and set a new one.
   --  Setting a new alarm cancel the previous one in this target
   --  So Cancel_And_Set_Alarm and Set_Alarm are identical.

   ---------------------------
   -- Clear_Alarm_Interrupt --
   ---------------------------

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

   ---------------------------
   -- Clear_Clock_Interrupt --
   ---------------------------

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

   ---------------------
   -- Power Down mode --
   ---------------------
   --  Procedure that sets the processor in idle mode.
   --  Should be used for idle loop be writing any value in Power_Down

   procedure Power_Down_Mode is

      Power_Down_Aux : Registers.Power_Down_Register;

   begin

      Power_Down_Aux.Idle := 33;
      Registers.Power_Down := Power_Down_Aux;

   end Power_Down_Mode;

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
   end Initialize_Board;

   ----------------------
   -- Initialize_Clock --
   ----------------------

   procedure Initialize_Clock is
      Interrupt_Mask_Aux  : Registers.Interrupt_Mask_Register;
      Prescaler_Reload_Aux : Registers.Prescaler_Reload_Register;
      Timer_Reload_Aux : Registers.Timer_Reload_Register;

   begin
      --  Set the prescaler_reload
      Prescaler_Reload_Aux := Registers.Prescaler_Reload;
      Prescaler_Reload_Aux.Reload := Periodic_Scaler;
      Registers.Prescaler_Reload := Prescaler_Reload_Aux;

      --  Load the counter for the clock
      Timer_Reload_Aux.Reload := Periodic_Count;
      Timer_Reload_Aux.Reserved  := (others => False);
      Registers.Timer_2_Reload := Timer_Reload_Aux;

      --  Enable Timer 2
      Timer_2_Control_Mirror.LD := True;
      Timer_2_Control_Mirror.RL := True;
      Timer_2_Control_Mirror.EN := True;
      Timer_2_Control_Mirror.Reserved  := (others => False);
      --  Write Timer Control Register
      Registers.Timer_2_Control := Timer_2_Control_Mirror;

      --  Enable clock interrupts
      Interrupt_Mask_Aux := Registers.Interrupt_Mask;
      Interrupt_Mask_Aux.Timer_2 := True;
      Registers.Interrupt_Mask := Interrupt_Mask_Aux;
   end Initialize_Clock;

   -----------------------
   -- Initialize_Memory --
   -----------------------

   procedure Initialize_Memory is
   begin
      --  Nothing to be done for LEON

      null;
   end Initialize_Memory;

   ---------------------
   -- Initialize_UART --
   ---------------------

   procedure Initialize_UART
     (Baudrate    : UART_Baudrate;
      Parity      : UART_Parity;
      FlowControl : UART_Flow_Control)
   is
      Control_1_Aux : Registers.UART_Control_Register;
      Control_2_Aux : Registers.UART_Control_Register;
      Scaler_1_Aux : Registers.UART_Scaler_Register;
      Scaler_2_Aux : Registers.UART_Scaler_Register;

      Scaler : Registers.Scaler_12;

   begin
      --  Read the Control Register
      Control_1_Aux := Registers.UART_1_Control;
      Control_2_Aux := Registers.UART_2_Control;
      Scaler_1_Aux := Registers.UART_1_Scaler;
      Scaler_2_Aux := Registers.UART_2_Scaler;

      --  TODO: Check if the UART was already initialized by the remote monitor

      --  Set the UART scaler according to the baudrate given
      Scaler :=
      Registers.Scaler_12
         ((((Integer (Clock_Freq_Hz) * 10) /
         (Integer (Baudrate) * 8)) - 5) / 10);

      Scaler_1_Aux.UART_Scaler := Scaler - 1;
      Scaler_2_Aux.UART_Scaler := Scaler - 1;

      --  Enable TX and RX
      Control_1_Aux.RE := True;
      Control_1_Aux.TE := True;
      Control_2_Aux.RE := True;
      Control_2_Aux.TE := True;

      --  Disable interrupts
      Control_1_Aux.RI := False;
      Control_1_Aux.TI := False;
      Control_2_Aux.RI := False;
      Control_2_Aux.TI := False;

      --  Set the correct bits for parity checking
      case Parity is
         when None =>
            Control_1_Aux.PE := False;
            Control_2_Aux.PE := False;
         when Even =>
            Control_1_Aux.PE := True;
            Control_1_Aux.PS  := False;
         when Odd  =>
            Control_1_Aux.PE := True;
            Control_1_Aux.PS  := True;
      end case;

      --  Set the correct bits for flow control.
      case FlowControl is
         when Off =>
            Control_1_Aux.FL := False;
            Control_2_Aux.FL := False;
         when On  =>
            Control_1_Aux.FL := True;
            Control_2_Aux.FL := True;
      end case;

      --  Use the system clock for synchronization

      Control_1_Aux.EC := False;
      Control_2_Aux.EC := False;

      --  Write to the LEON regs

      Control_1_Aux.Reserved  := (others => False);
      Control_2_Aux.Reserved  := (others => False);
      Scaler_1_Aux.Reserved  := (others => False);
      Scaler_2_Aux.Reserved  := (others => False);

      Registers.UART_1_Control := Control_1_Aux;
      Registers.UART_2_Control := Control_2_Aux;

      Registers.UART_1_Scaler := Scaler_1_Aux;
      Registers.UART_2_Scaler := Scaler_2_Aux;
   end Initialize_UART;

   ---------------------------
   -- Priority_Of_Interrupt --
   ---------------------------

   function Priority_Of_Interrupt
     (Level : SBP.Interrupt_Level) return System.Any_Priority
   is
   begin
      --  Assert that it is a real interrupt

      pragma Assert (Level /= 0);

      return (Any_Priority (Level) + Interrupt_Priority'First - 1);
   end Priority_Of_Interrupt;

   ----------------
   -- Read_Clock --
   ----------------

   function Read_Clock return Timer_Interval is
   begin
      return Timer_Interval
        (Periodic_Count - Registers.Timer_2_Counter.Counter);
   end Read_Clock;

   ---------------
   -- Set_Alarm --
   ---------------

   procedure Set_Alarm (Ticks : Timer_Interval) is
      Interrupt_Mask_Aux : Registers.Interrupt_Mask_Register;
      Timer_Reload_Aux : Registers.Timer_Reload_Register;

   begin
      --  Alarm Clock downcount will reach 0 in Ticks. The granularity of
      --  time intervals is equal to Clock Period.

      --  Set the prescaler: already done in initialize_clock

      --  Load the counter
      Timer_Reload_Aux.Reload := Registers.Timer_24 (Ticks);
      Registers.Timer_1_Reload := Timer_Reload_Aux;

      --  Set the proper bits in mirrored Timer Control Register.
      --  Timer 1 is used in one-shot mode.
      Timer_1_Control_Mirror.RL := False;
      Timer_1_Control_Mirror.LD := True;
      Timer_1_Control_Mirror.EN := True;
      Timer_1_Control_Mirror.Reserved  := (others => False);

      --  Write Timer Control Register
      Registers.Timer_1_Control := Timer_1_Control_Mirror;

      --  Enable Timer 1 Interrupts
      Interrupt_Mask_Aux := Registers.Interrupt_Mask;
      Interrupt_Mask_Aux.Timer_1 := True;
      Registers.Interrupt_Mask := Interrupt_Mask_Aux;
   end Set_Alarm;

   ------------------
   -- To_Interrupt --
   ------------------

   function To_Interrupt
     (Vector : SBP.Range_Of_Vector) return SBP.Interrupt_Level
   is
   begin
      --  The range corresponding to asynchronous traps is between
      --  16#11# and 16#1F#.

      pragma Assert (Vector >= 16#11# and then Vector <= 16#1F#);

      return Vector - 16#10#;
   end To_Interrupt;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (Level : SBP.Interrupt_Level) return SBP.Range_Of_Vector
   is
   begin
      return Level + 16#10#;
   end To_Vector;

   ---------------
   -- UART_Send --
   ---------------

   procedure UART_Send
     (Char    : Character;
      Channel : UART_Channel)
   is
      UART_Tx         : Registers.UART_Channel_Rx_Tx_Register;
      UART_Status_Aux : Registers.UART_Status_Register;
   begin
      --  Set the Character that we want to send through the port

      UART_Tx.RTD := Char;

      --  NOTE !! All reserved bits have to be written with zeros in order
      --  to avoid parity error resulting in a internal error.

      UART_Tx.Reserved := (others => False);

      --  Send the character through the selected channel by polling

      case Channel is
         when C1 =>
            loop
               UART_Status_Aux := Registers.UART_1_Status;
               exit when UART_Status_Aux.TH;
            end loop;

            Registers.UART_1_Rx_Tx := UART_Tx;

         when C2 =>
            loop
               UART_Status_Aux := Registers.UART_2_Status;
               exit when UART_Status_Aux.TH;
            end loop;

            Registers.UART_2_Rx_Tx := UART_Tx;
      end case;

   end UART_Send;

end System.BB.Peripherals;
