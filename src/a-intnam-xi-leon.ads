------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   A D A . I N T E R R U P T S . N A M E S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--                     Copyright (C) 1995-2005, AdaCore                     --
--             Copyright (C) 2009, Universidad Politecnica de Madrid        --
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
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is the version for LEON targets of this package

with Ada.Interrupts;

with System.OS_Interface;

package Ada.Interrupts.Names is

   -------------------------
   -- External Interrupts --
   -------------------------

   External_Interrupt_3 : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.External_Interrupt_3);
   External_Interrupt_3_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.External_Interrupt_3_Priority;

   External_Interrupt_2 : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.External_Interrupt_2);
   External_Interrupt_2_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.External_Interrupt_2_Priority;

   External_Interrupt_1 : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.External_Interrupt_1);
   External_Interrupt_1_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.External_Interrupt_1_Priority;

   External_Interrupt_0 : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.External_Interrupt_0);
   External_Interrupt_0_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.External_Interrupt_0_Priority;

   -----------------------
   -- Timers Interrupts --
   -----------------------

   Timer_2 : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.Timer_2);
   Timer_2_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.Timer_2_Priority;

   Timer_1 : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.Timer_1);
   Timer_1_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.Timer_1_Priority;

   ---------------------
   -- UART Interrupts --
   ---------------------

   UART_1_RX_TX : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.UART_1_RX_TX);
   UART_1_RX_TX_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.UART_1_RX_TX_Priority;

   UART_2_RX_TX : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.UART_2_RX_TX);
   UART_2_RX_TX_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.UART_2_RX_TX_Priority;

   -----------------------------
   -- Miscelaneous Interrupts --
   -----------------------------

   Correctable_Error_In_Memory : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.Correctable_Error_In_Memory);
   Correctable_Error_In_Memory_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.Correctable_Error_In_Memory_Priority;

   DSU : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.DSU);
   DSU_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.DSU_Priority;

   PCI : constant Interrupt_ID :=
     Interrupt_ID (System.OS_Interface.PCI);
   PCI_Priority : constant System.Interrupt_Priority :=
     System.OS_Interface.PCI_Priority;

end Ada.Interrupts.Names;
