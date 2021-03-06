------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--       A D A . C O N T A I N E R S . B O U N D E D _ V E C T O R S        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2011, Free Software Foundation, Inc.           --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
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
------------------------------------------------------------------------------

private with Ada.Finalization;
private with Ada.Streams;

generic
   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;

package Ada.Containers.Indefinite_Holders is
   pragma Preelaborate (Indefinite_Holders);
   pragma Remote_Types (Indefinite_Holders);

   type Holder is tagged private;
   pragma Preelaborable_Initialization (Holder);

   Empty_Holder : constant Holder;

   function "=" (Left, Right : Holder) return Boolean;

   function To_Holder (New_Item : Element_Type) return Holder;

   function Is_Empty (Container : Holder) return Boolean;

   procedure Clear (Container : in out Holder);

   function Element (Container : Holder) return Element_Type;

   procedure Replace_Element
     (Container : in out Holder;
      New_Item  : Element_Type);

   procedure Query_Element
     (Container : Holder;
      Process   : not null access procedure (Element : Element_Type));
   procedure Update_Element
     (Container : Holder;
      Process   : not null access procedure (Element : in out Element_Type));

   procedure Assign (Target : in out Holder; Source : Holder);

   function Copy (Source : Holder) return Holder;

   procedure Move (Target : in out Holder; Source : in out Holder);

private

   package AF renames Ada.Finalization;

   type Element_Access is access all Element_Type;

   procedure Read
     (Stream    : not null access Ada.Streams.Root_Stream_Type'Class;
      Container : out Holder);

   procedure Write
     (Stream    : not null access Ada.Streams.Root_Stream_Type'Class;
      Container : Holder);

   type Holder is new Ada.Finalization.Controlled with record
      Element : Element_Access;
      Busy    : Natural := 0;
   end record;
   for Holder'Read use Read;
   for Holder'Write use Write;

   overriding procedure Adjust (Container : in out Holder);
   overriding procedure Finalize (Container : in out Holder);

   Empty_Holder : constant Holder := (AF.Controlled with null, 0);

end Ada.Containers.Indefinite_Holders;
