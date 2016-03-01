------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--      S Y S T E M . B B . P E R I P H E R A L S . R E G I S T E R S       --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2006 The European Space Agency            --
--                     Copyright (C) 2003-2007, AdaCore                     --
--           Copyright (C) 2009 Universidad Politecnica de Madrid           --
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

--  This package provides the appropriate mapping for the system registers.
--  This is the LEON version of this package.

pragma Restrictions (No_Elaboration_Code);

package System.BB.Peripherals.Registers is
   pragma Preelaborate;

   --  Warning : The bit numbering within a register is opposed to the
   --  bit numbering of LEON Registers Specification Document.

   --  It seems to be a big-endian/little-endian notation problem.
   --  System.Default_Bit_Order is HIGH_ORDER_FIRST for this target.

   --  Pragma Suppress_Initialization (register_type) must be used in order
   --  to keep eficiency. Otherwise, initialization procedures are always
   --  generated for objects of packed boolean array types and of records types
   --  that have components of these types.

   ----------------------------
   -- Local type definitions --
   ----------------------------

   type Scaler_10 is mod 2 **  10;
   for Scaler_10'Size use  10;
   --  10-bit scaler

   type Scaler_12 is mod 2 **  12;
   for Scaler_12'Size use  12;
   --  12-bit scaler

   type Timer_24 is mod 2 ** 24;
   for Timer_24'Size use  24;
   --  Timer counters are 24-bit registers

   type Idle_32 is mod 2 ** 32;
   for Idle_32'Size use  32;
   --  Power down are 32-bit register

   type Protection_Mask_Type is mod 2 ** 15;
   for Protection_Mask_Type'Size use 15;

   type Protection_Tag_Type is mod 2 ** 15;
   for Protection_Tag_Type'Size use 15;

   type Check_Bits_Type is array (0 .. 6) of Boolean;
   for Check_Bits_Type'Size use 7;
   pragma Pack (Check_Bits_Type);

   type Reserved_4 is array (0 .. 3) of Boolean;
   for Reserved_4'Size use 4;
   pragma Pack (Reserved_4);

   type Reserved_7 is array (0 .. 6) of Boolean;
   for Reserved_7'Size use 7;
   pragma Pack (Reserved_7);

   type Reserved_8 is array (0 .. 7) of Boolean;
   for Reserved_8'Size use 8;
   pragma Pack (Reserved_8);

   type Reserved_9 is array (0 .. 8) of Boolean;
   for Reserved_9'Size use 9;
   pragma Pack (Reserved_9);

   type Reserved_10 is array (0 .. 9) of Boolean;
   for Reserved_10'Size use 10;
   pragma Pack (Reserved_10);

   type Reserved_11 is array (0 .. 10) of Boolean;
   for Reserved_11'Size use 11;
   pragma Pack (Reserved_11);

   type Reserved_16 is array (0 .. 15) of Boolean;
   for Reserved_16'Size use 16;
   pragma Pack (Reserved_16);

   type Reserved_17 is array (0 .. 16) of Boolean;
   for Reserved_17'Size use 17;
   pragma Pack (Reserved_17);

   type Reserved_20 is array (0 .. 19) of Boolean;
   for Reserved_20'Size use 20;
   pragma Pack (Reserved_20);

   type Reserved_22 is array (0 .. 21) of Boolean;
   for Reserved_22'Size use 22;
   pragma Pack (Reserved_22);

   type Reserved_23 is array (0 .. 22) of Boolean;
   for Reserved_23'Size use 23;
   pragma Pack (Reserved_23);

   type Reserved_24 is array (0 .. 23) of Boolean;
   for Reserved_24'Size use 24;
   pragma Pack (Reserved_24);

   type Reserved_25 is array (0 .. 24) of Boolean;
   for Reserved_25'Size use 25;
   pragma Pack (Reserved_25);

   type Reserved_29 is array (0 .. 28) of Boolean;
   for Reserved_29'Size use 29;
   pragma Pack (Reserved_29);

   ------------------------------------------
   -- Addresses of memory mapped registers --
   ------------------------------------------

   Power_Down_Register_Address :
   constant System.Address := System'To_Address (16#80000018#);

   Write_Protection_1_Register_Address :
   constant System.Address := System'To_Address (16#8000001C#);

   Write_Protection_2_Register_Address :
   constant System.Address := System'To_Address (16#80000020#);

   Timer_1_Counter_Register_Address :
   constant System.Address := System'To_Address (16#80000040#);

   Timer_1_Reload_Register_Address :
   constant System.Address := System'To_Address (16#80000044#);

   Timer_1_Control_Register_Address :
   constant System.Address := System'To_Address (16#80000048#);

   Watchdog_Counter_Register_Address :
   constant System.Address := System'To_Address (16#8000004C#);

   Timer_2_Counter_Register_Address :
   constant System.Address := System'To_Address (16#80000050#);

   Timer_2_Reload_Register_Address :
   constant System.Address := System'To_Address (16#80000054#);

   Timer_2_Control_Register_Address :
   constant System.Address := System'To_Address (16#80000058#);

   Prescaler_Reload_Register_Address :
   constant System.Address := System'To_Address (16#80000064#);

   Interrupt_Mask_Register_Address :
   constant System.Address := System'To_Address (16#80000090#);

   Interrupt_Force_Register_Address :
   constant System.Address := System'To_Address (16#80000098#);

   Interrupt_Clear_Register_Address :
   constant System.Address := System'To_Address (16#8000009C#);

   UART_1_Rx_Tx_Register_Address :
   constant System.Address := System'To_Address (16#80000070#);

   UART_1_Status_Register_Address :
   constant System.Address := System'To_Address (16#80000074#);

   UART_1_Control_Register_Address :
   constant System.Address := System'To_Address (16#80000078#);

   UART_1_Scaler_Register_Address :
   constant System.Address := System'To_Address (16#8000007C#);

   UART_2_Rx_Tx_Register_Address :
   constant System.Address := System'To_Address (16#80000080#);

   UART_2_Status_Register_Address :
   constant System.Address := System'To_Address (16#80000084#);

   UART_2_Control_Register_Address :
   constant System.Address := System'To_Address (16#80000088#);

   UART_2_Scaler_Register_Address :
   constant System.Address := System'To_Address (16#8000008C#);

   ----------------
   -- Power Down --
   ----------------

   type Power_Down_Register is
      record
         Idle : Idle_32;
      end record;

   for Power_Down_Register use
      record
         Idle at 0 range 0 .. 31;
      end record;

   for Power_Down_Register'Size use 32;

   pragma Suppress_Initialization (Power_Down_Register);

   Power_Down : Power_Down_Register;
   pragma Atomic (Power_Down);
   for Power_Down'Address use Power_Down_Register_Address;

   --------------------
   -- Timers Counter --
   --------------------

   type Timer_Counter_Register is
      record
         Counter : Timer_24;
         Reserved : Reserved_8;
      end record;

   for Timer_Counter_Register use
      record
         Counter at 0 range 8 .. 31;
         Reserved at 0 range 0 .. 7;
      end record;

   for Timer_Counter_Register'Size use 32;

   pragma Suppress_Initialization (Timer_Counter_Register);

   Timer_1_Counter : Timer_Counter_Register;
   pragma Atomic (Timer_1_Counter);
   for Timer_1_Counter'Address use Timer_1_Counter_Register_Address;

   Timer_2_Counter : Timer_Counter_Register;
   pragma Atomic (Timer_2_Counter);
   for Timer_2_Counter'Address use Timer_2_Counter_Register_Address;

   -------------------
   -- Timers Reload --
   -------------------

   type Timer_Reload_Register is
      record
         Reload : Timer_24;
         Reserved : Reserved_8;
      end record;

   for Timer_Reload_Register use
      record
         Reload at 0 range 8 .. 31;
         Reserved at 0 range 0 .. 7;
      end record;

   for Timer_Reload_Register'Size use 32;

   pragma Suppress_Initialization (Timer_Reload_Register);

   Timer_1_Reload : Timer_Reload_Register;
   pragma Atomic (Timer_1_Reload);
   for Timer_1_Reload'Address use Timer_1_Reload_Register_Address;

   Timer_2_Reload : Timer_Reload_Register;
   pragma Atomic (Timer_2_Reload);
   for Timer_2_Reload'Address use Timer_2_Reload_Register_Address;

   --------------------
   -- Timers Control --
   --------------------

   type Timer_Control_Register is
      record
         EN : Boolean; -- enable
         --  1  : enable counting
         --  0  : hold scaler (and counter) w

         RL : Boolean; -- counter reload
         --  1  : reload counter at zero and restart
         --  0  : stop counter at zero w

         LD : Boolean; -- counter load
         --  1  : load counter with preset value and start if enabled
         --  0  : no function w

         Reserved : Reserved_29; -- 0 Not used
      end record;

   for Timer_Control_Register use
      record
         EN at 0 range 31 .. 31;
         RL at 0 range 30 .. 30;
         LD at 0 range 29 .. 29;
         Reserved at 0 range 0 .. 28;
      end record;

   for Timer_Control_Register'Size use 32;

   pragma Suppress_Initialization (Timer_Control_Register);

   Timer_1_Control : Timer_Control_Register;
   pragma Atomic (Timer_1_Control);
   for Timer_1_Control'Address use Timer_1_Control_Register_Address;

   Timer_2_Control : Timer_Control_Register;
   pragma Atomic (Timer_2_Control);
   for Timer_2_Control'Address use Timer_2_Control_Register_Address;

   ----------------------
   -- Watchdog Counter --
   ----------------------

   type Watchdog_Counter_Register is
      record
         Counter : Timer_24;
         Reserved : Reserved_8;
      end record;

   for Watchdog_Counter_Register use
      record
         Counter at 0 range 8 .. 31;
         Reserved at 0 range 0 .. 7;
      end record;

   for Watchdog_Counter_Register'Size use 32;

   pragma Suppress_Initialization (Watchdog_Counter_Register);

   Watchdog_Counter : Watchdog_Counter_Register;
   pragma Atomic (Watchdog_Counter);
   for Watchdog_Counter'Address use Watchdog_Counter_Register_Address;

   ----------------------
   -- Prescaler Reload --
   ----------------------

   type Prescaler_Reload_Register is
      record
         Reload : Scaler_10;
         Reserved : Reserved_22;
      end record;

   for Prescaler_Reload_Register use
      record
         Reload at 0 range 22 .. 31;
         Reserved at 0 range 0 .. 21;
      end record;

   for Prescaler_Reload_Register'Size use 32;

   pragma Suppress_Initialization (Prescaler_Reload_Register);

   Prescaler_Reload : Prescaler_Reload_Register;
   pragma Atomic (Prescaler_Reload);
   for Prescaler_Reload'Address use Prescaler_Reload_Register_Address;

   ----------------------
   -- Write Protection --
   ----------------------

   type Write_Protection_Register is
      record
         MASK : Protection_Mask_Type; -- Address mask
         --  This field contains the address mask

         TAG  : Protection_Tag_Type; -- Address tag
         --  This field is compared against address

         BP : Boolean; -- Block protect
         --  If set, selects block protect mode

         EN : Boolean; -- Enable
         --  If set, enables the write protect unit
      end record;

   for Write_Protection_Register use
      record
         MASK at 0 range 17 .. 31;
         TAG  at 0 range 2 .. 16;
         BP at 0 range 1 .. 1;
         EN at 0 range 0 .. 0;
      end record;

   for Write_Protection_Register'Size use 32;

   pragma Suppress_Initialization (Write_Protection_Register);

   Protected_1_Register : Write_Protection_Register;
   pragma Atomic (Protected_1_Register);
   for Protected_1_Register'Address use
     Write_Protection_1_Register_Address;

   Protected_2_Register : Write_Protection_Register;
   pragma Atomic (Protected_2_Register);
   for Protected_2_Register'Address use
     Write_Protection_2_Register_Address;

   --------------------
   -- Interrupt Mask --
   --------------------

   type Interrupt_Mask_Register is
      record
         Reserved1 : Boolean;

         --  1  : interrupt X masked
         --  0  : interrupt X not masked r/w
         Correctable_Error_In_Memory : Boolean;
         UART_2_RX_TX : Boolean;
         UART_1_RX_TX : Boolean;
         External_Interrupt_0 : Boolean;
         External_Interrupt_1 : Boolean;
         External_Interrupt_2 : Boolean;
         External_Interrupt_3 : Boolean;
         Timer_1 : Boolean;
         Timer_2 : Boolean;
         Unused_1 : Boolean;
         DSU : Boolean;
         Unused_2 : Boolean;
         Unused_3 : Boolean;
         PCI : Boolean;
         Unused_4 : Boolean;
         Reserved16 : Reserved_16;
      end record;

   for Interrupt_Mask_Register use
      record
         Reserved1 at 0 range 31 .. 31;
         Correctable_Error_In_Memory at 0 range 30 .. 30;
         UART_2_RX_TX at 0 range 29 .. 29;
         UART_1_RX_TX at 0 range 28 .. 28;
         External_Interrupt_0 at 0 range 27 .. 27;
         External_Interrupt_1 at 0 range 26 .. 26;
         External_Interrupt_2 at 0 range 25 .. 25;
         External_Interrupt_3 at 0 range 24 .. 24;
         Timer_1 at 0 range 23 .. 23;
         Timer_2 at 0 range 22 .. 22;
         Unused_1 at 0 range 21 .. 21;
         DSU at 0 range 20 .. 20;
         Unused_2 at 0 range 19 .. 19;
         Unused_3 at 0 range 18 .. 18;
         PCI at 0 range 17 .. 17;
         Unused_4 at 0 range 16 .. 16;
         Reserved16 at 0 range 0 .. 15;
      end record;

   for Interrupt_Mask_Register'Size use 32;

   pragma Suppress_Initialization (Interrupt_Mask_Register);

   Interrupt_Mask : Interrupt_Mask_Register;
   pragma Atomic (Interrupt_Mask);
   for Interrupt_Mask'Address use Interrupt_Mask_Register_Address;

   ------------------------
   -- Interrupt Register --
   ------------------------

   type Interrupt_Register is
      record
         Reserved1 : Boolean;

         --  1  : interrupt X forced
         --  0  : interrupt X not forced r/w
         Correctable_Error_In_Memory : Boolean;
         UART_2_RX_TX : Boolean;
         UART_1_RX_TX : Boolean;
         External_Interrupt_0 : Boolean;
         External_Interrupt_1 : Boolean;
         External_Interrupt_2 : Boolean;
         External_Interrupt_3 : Boolean;
         Timer_1 : Boolean;
         Timer_2 : Boolean;
         Unused_1 : Boolean;
         DSU : Boolean;
         Unused_2 : Boolean;
         Unused_3 : Boolean;
         PCI : Boolean;
         Unused_4 : Boolean;
         Reserved16 : Reserved_16;
      end record;

   for Interrupt_Register use
      record
         Reserved1 at 0 range 31 .. 31;
         Correctable_Error_In_Memory at 0 range 30 .. 30;
         UART_2_RX_TX at 0 range 29 .. 29;
         UART_1_RX_TX at 0 range 28 .. 28;
         External_Interrupt_0 at 0 range 27 .. 27;
         External_Interrupt_1 at 0 range 26 .. 26;
         External_Interrupt_2 at 0 range 25 .. 25;
         External_Interrupt_3 at 0 range 24 .. 24;
         Timer_1 at 0 range 23 .. 23;
         Timer_2 at 0 range 22 .. 22;
         Unused_1 at 0 range 21 .. 21;
         DSU at 0 range 20 .. 20;
         Unused_2 at 0 range 19 .. 19;
         Unused_3 at 0 range 18 .. 18;
         PCI at 0 range 17 .. 17;
         Unused_4 at 0 range 16 .. 16;
         Reserved16 at 0 range 0 .. 15;
      end record;

   for Interrupt_Register'Size use 32;

   pragma Suppress_Initialization (Interrupt_Register);

   ------------------------------
   -- Interrupt Force Register --
   ------------------------------

   Interrupt_Force : Interrupt_Register;
   pragma Atomic (Interrupt_Force);
   for Interrupt_Force'Address use Interrupt_Force_Register_Address;

   ------------------------------
   -- Interrupt Clear Register --
   ------------------------------

   Interrupt_Clear : Interrupt_Register;
   pragma Atomic (Interrupt_Clear);
   for Interrupt_Clear'Address use Interrupt_Clear_Register_Address;

   ----------------------------
   -- UART Scaler Registers --
   ----------------------------

   type UART_Scaler_Register is
      record
         UART_Scaler : Scaler_12;
         --  1 - 4095  : Divide factor
         --  0  : stops the UART clock
         Reserved : Reserved_20;
      end record;

   for UART_Scaler_Register use
      record
         UART_Scaler at 0 range 20 .. 31;
         Reserved at 0 range 0 .. 19;
      end record;

   for UART_Scaler_Register'Size use 32;

   pragma Suppress_Initialization (UART_Scaler_Register);

   UART_1_Scaler : UART_Scaler_Register;
   pragma Atomic (UART_1_Scaler);
   for UART_1_Scaler'Address use UART_1_Scaler_Register_Address;

   UART_2_Scaler : UART_Scaler_Register;
   pragma Atomic (UART_2_Scaler);
   for UART_2_Scaler'Address use UART_2_Scaler_Register_Address;

   -----------------------------
   -- UART Control Registers --
   -----------------------------

   type UART_Control_Register is
      record
         RE : Boolean; --  Receiver enable
         --  1  : enables the receiver
         --  0  : disables the receiver

         TE : Boolean; --  Transmitter enable
         --  1  : enables the transmitter
         --  0  : disables the transmitter

         RI : Boolean; --  Receiver interrupt enable
         --  1  : enables generation of receiver interrupt
         --  0  : disables generation of receiver interrupt

         TI : Boolean; --  Transmitter interrupt enable
         --  1  : enables generation of transmitter interrupt
         --  0  : disables generation of transmitter interrupt

         PS : Boolean; --  Parity
         --  1  : odd parity
         --  0  : even parity

         PE : Boolean; --  Parity enable
         --  1  : parity enabled
         --  0  : no parity

         FL : Boolean; --  Flow Control
         --  1  : flow control using CTS/RTS
         --  0  : disables

         LB : Boolean; --  Loop back
         --  1  : enables loop back mode
         --  0  : disables loop back mode

         EC : Boolean; --  External clock
         --  1  : external clock
         --  0  : system clock

         Reserved : Reserved_23;
      end record;

   for UART_Control_Register use
      record
         RE at 0 range 31 .. 31;
         TE at 0 range 30 .. 30;
         RI at 0 range 29 .. 29;
         TI at 0 range 28 .. 28;
         PS at 0 range 27 .. 27;
         PE at 0 range 26 .. 26;
         FL at 0 range 25 .. 25;
         LB at 0 range 24 .. 24;
         EC at 0 range 23 .. 23;
         Reserved at 0 range 0 .. 22;
      end record;

   for UART_Control_Register'Size use 32;

   pragma Suppress_Initialization (UART_Control_Register);

   UART_1_Control : UART_Control_Register;
   pragma Atomic (UART_1_Control);
   for UART_1_Control'Address use UART_1_Control_Register_Address;

   UART_2_Control : UART_Control_Register;
   pragma Atomic (UART_2_Control);
   for UART_2_Control'Address use UART_2_Control_Register_Address;

   -----------------
   -- UART Status --
   -----------------

   type UART_Status_Register is
      record
         DR : Boolean; --  Data Ready

         TS : Boolean; --  Transmitter shift register empty (no data to send)

         TH : Boolean; --  Transmitter hold register empty (ready to load data)

         BR : Boolean; --  Break received

         OV : Boolean; --  Overrun error

         PE : Boolean; --  Parity error

         FE : Boolean; --  Framing error

         Reserved : Reserved_25; --  Not used r
      end record;

   for UART_Status_Register use
      record
         DR at 0 range 31 .. 31;
         TS at 0 range 30 .. 30;
         TH at 0 range 29 .. 29;
         BR at 0 range 28 .. 28;
         OV at 0 range 27 .. 27;
         PE at 0 range 26 .. 26;
         FE at 0 range 25 .. 25;
         Reserved at 0 range 0 .. 24;
      end record;

   for UART_Status_Register'Size use 32;

   pragma Suppress_Initialization (UART_Status_Register);

   UART_1_Status : UART_Status_Register;
   pragma Atomic (UART_1_Status);
   for UART_1_Status'Address use UART_1_Status_Register_Address;

   UART_2_Status : UART_Status_Register;
   pragma Atomic (UART_2_Status);
   for UART_2_Status'Address use UART_2_Status_Register_Address;

   ------------------------
   -- UART Channel Rx Tx --
   ------------------------

   type UART_Channel_Rx_Tx_Register is
      record
         RTD : Character; --  Rx/Tx Data r/w

         Reserved : Reserved_24; --  Not used r
      end record;

   for UART_Channel_Rx_Tx_Register use
      record
         RTD at 0 range 24 .. 31;
         Reserved at 0 range 0 .. 23;
      end record;

   for UART_Channel_Rx_Tx_Register'Size use 32;

   pragma Suppress_Initialization (UART_Channel_Rx_Tx_Register);

   UART_1_Rx_Tx : UART_Channel_Rx_Tx_Register;
   pragma Atomic (UART_1_Rx_Tx);
   for UART_1_Rx_Tx'Address use UART_1_Rx_Tx_Register_Address;

   UART_2_Rx_Tx : UART_Channel_Rx_Tx_Register;
   pragma Atomic (UART_2_Rx_Tx);
   for UART_2_Rx_Tx'Address use UART_2_Rx_Tx_Register_Address;

end System.BB.Peripherals.Registers;
