------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . O B J E C T _ R E A D E R                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2009-2011, Free Software Foundation, Inc.          --
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
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

with Interfaces;           use Interfaces;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C_Streams; use Interfaces.C_Streams;

package body System.Object_Reader is

   SSU : constant := System.Storage_Unit;

   MAX_SECTIONS : constant := 512;
   --  There is no common maximum number of sections we can assume for all
   --  object formats, however 512 is extremely generous and far in excess of
   --  anything we ever expect to encounter in practice.

   function To_int32 is new Ada.Unchecked_Conversion (uint32, int32);

   procedure Detect_Object_Format (Obj : in out Object_File);
   --  Determine the object format and target architecture for this object file
   --  and set the fields Obj.Format and Obj.Arch accordingly. This is done by
   --  reading magic numbers and other metadata from the file. If this process
   --  fails and either Obj.Format or Obj.Arch can not be determined then
   --  Format_Error is raised.

   -------------------------------------
   -- ELF object file format handling --
   -------------------------------------

   generic
      type uword is mod <>;

   package ELF_Ops is

      --  ELF version codes

      ELFCLASS32 : constant :=  1;  --  32 bit ELF
      ELFCLASS64 : constant :=  2;  --  64 bit ELF

      --  ELF machine codes

      EM_NONE        : constant :=  0; --  No machine
      EM_SPARC       : constant :=  2; --  SUN SPARC
      EM_386         : constant :=  3; --  Intel 80386
      EM_MIPS        : constant :=  8; --  MIPS RS3000 Big-Endian
      EM_MIPS_RS3_LE : constant := 10; --  MIPS RS3000 Little-Endian
      EM_SPARC32PLUS : constant := 18; --  Sun SPARC 32+
      EM_PPC         : constant := 20; --  PowerPC
      EM_PPC64       : constant := 21; --  PowerPC 64-bit
      EM_ARM         : constant := 40; --  ARM
      EM_SPARCV9     : constant := 43; --  SPARC v9 64-bit
      EM_IA_64       : constant := 50; --  Intel Merced
      EM_X86_64      : constant := 62; --  AMD x86-64 architecture

      EN_NIDENT  : constant := 16;

      type E_Ident_Type is array (0 .. EN_NIDENT - 1) of uint8;

      type Header is record
         E_Ident     : E_Ident_Type; -- Magic number and other info
         E_Type      : uint16;       -- Object file type
         E_Machine   : uint16;       -- Architecture
         E_Version   : uint32;       -- Object file version
         E_Entry     : uword;        -- Entry point virtual address
         E_Phoff     : uword;        -- Program header table file offset
         E_Shoff     : uword;        -- Section header table file offset
         E_Flags     : uint32;       -- Processor-specific flags
         E_Ehsize    : uint16;       -- ELF header size in bytes
         E_Phentsize : uint16;       -- Program header table entry size
         E_Phnum     : uint16;       -- Program header table entry count
         E_Shentsize : uint16;       -- Section header table entry size
         E_Shnum     : uint16;       -- Section header table entry count
         E_Shstrndx  : uint16;       -- Section header string table index
      end record;

      type Section_Header is record
         Sh_Name      : uint32; -- Section name string table index
         Sh_Type      : uint32; -- Section type
         Sh_Flags     : uword;  -- Section flags
         Sh_Addr      : uword;  -- Section virtual addr at execution
         Sh_Offset    : uword;  -- Section file offset
         Sh_Size      : uword;  -- Section size in bytes
         Sh_Link      : uint32; -- Link to another section
         Sh_Info      : uint32; -- Additional section information
         Sh_Addralign : uword;  -- Section alignment
         Sh_Entsize   : uword;  -- Entry size if section holds table
      end record;

      type Section_Header_Table is
        array (uint16 range 0 .. MAX_SECTIONS - 1) of Section_Header;

      type Symtab_Entry32 is record
         St_Name  : uint32;  --  Name (string table index)
         St_Value : uint32;  --  Value
         St_Size  : uint32;  --  Size in bytes
         St_Info  : uint8;   --  Type and binding attributes
         St_Other : uint8;   --  Undefined
         St_Shndx : uint16;  --  Defining section
      end record;

      type Symtab_Entry64 is record
         St_Name  : uint32;  --  Name (string table index)
         St_Info  : uint8;   --  Type and binding attributes
         St_Other : uint8;   --  Undefined
         St_Shndx : uint16;  --  Defining section
         St_Value : uint64;  --  Value
         St_Size  : uint64;  --  Size in bytes
      end record;

      function Read_Header (Obj : Object_File) return Header;
      --  Read a header from an ELF format object

      function Read_Section_Header
        (Obj   : Object_File;
         Shnum : uint32) return Section_Header;
      --  Read the header for an ELF format object section indexed from zero

      function First_Symbol (Obj : Object_File) return Object_Symbol;
      --  Return the first element in the symbol table, or Null_Symbol if the
      --  symbol table is empty.

      function Next_Symbol
        (Obj  : Object_File;
         Prev : Object_Symbol) return Object_Symbol;
      --  Return the element following Prev in the symbol table, or Null_Symbol
      --  if Prev is the last symbol in the table.

      function Symbol_Name
        (Obj : Object_File;
         Sym : Object_Symbol) return String;
      --  Return the name of the symbol

      function Section_Name
        (Obj : Object_File;
         Sec : Object_Section) return String;
      --  Return the name of a section

      function Get_Section
        (Obj   : Object_File;
         Shnum : uint32) return Object_Section;
      --  Fetch a section by index from zero

      function Get_Section
        (Obj  : Object_File;
         Name : String) return Object_Section;
      --  Fetch a section by name

      procedure Seek_Section (Obj : Object_File; Sec : Object_Section);
      --  Seek to the first byte of the specified section in the object file

      function Get_String_Table (Obj : Object_File) return Object_Section;
      --  Fetch the the section containing the string table

      function Get_Symbol_Table (Obj : Object_File) return Object_Section;
      --  Fetch the the section containing the symbol table

   end ELF_Ops;

   -----------------------------------
   -- PECOFF object format handling --
   -----------------------------------

   package PECOFF_Ops is

      --  Constants and data layout are taken from the document "Microsoft
      --  Portable Executable and Common Object File Format Specification"
      --  Revision 8.1.

      Signature_Loc_Offset : constant := 16#3C#;
      --  Offset of pointer to the file signature

      Size_Of_Standard_Header_Fields : constant := 16#18#;
      --  Length in bytes of the standard header record

      Function_Symbol_Type : constant := 16#20#;
      --  Value of the type field indicating a symbol refers to a function

      Image_Load_Address : constant := 16#400000#;
      --  Base image load address for Windows 95 through Vista

      type Magic_Array is array (0 .. 3) of uint8;
      --  Array of magic numbers from the header

      --  Magic numbers for PECOFF variants

      VARIANT_PE32      : constant := 16#010B#;
      VARIANT_PE32_PLUS : constant := 16#020B#;

      --  PECOFF machine codes

      IMAGE_FILE_MACHINE_I386  : constant := 16#014C#;
      IMAGE_FILE_MACHINE_IA64  : constant := 16#0200#;
      IMAGE_FILE_MACHINE_AMD64 : constant := 16#8664#;

      --  PECOFF Data layout

      type Header is record
         Magics               : Magic_Array;
         Machine              : uint16;
         NumberOfSections     : uint16;
         TimeDateStamp        : uint32;
         PointerToSymbolTable : uint32;
         NumberOfSymbols      : uint32;
         SizeOfOptionalHeader : uint16;
         Characteristics      : uint16;
         Variant              : uint16;
      end record;

      pragma Pack (Header);

      subtype Name_Str is String (1 .. 8);

      type Section_Header is record
         Name                 : Name_Str;
         VirtualSize          : uint32;
         VirtualAddress       : uint32;
         SizeOfRawData        : uint32;
         PointerToRawData     : uint32;
         PointerToRelocations : uint32;
         PointerToLinenumbers : uint32;
         NumberOfRelocations  : uint16;
         NumberOfLinenumbers  : uint16;
         Characteristics      : uint32;
      end record;

      pragma Pack (Section_Header);

      type Section_Header_Table is
        array (uint32 range 0 .. MAX_SECTIONS - 1) of Section_Header;

      type Symtab_Entry is record
         Name                  : Name_Str;
         Value                 : uint32;
         SectionNumber         : int16;
         TypeField             : uint16;
         StorageClass          : uint8;
         NumberOfAuxSymbols    : uint8;
      end record;

      pragma Pack (Symtab_Entry);

      function Decode_Name
        (Obj      : Object_File;
         Raw_Name : String) return String;
      --  A section name is an 8 byte field padded on the right with null
      --  characters, or a '\' followed by an ASCII decimal string indicating
      --  an offset in to the string table. This routine decodes this

      function Read_Header (Obj : Object_File) return Header;
      --  Read the object file header

      function Read_Section_Header
        (Obj   : Object_File;
         Index : uint32) return Section_Header;
      --  Read a header from section table

      function First_Symbol (Obj : Object_File) return Object_Symbol;
      --  Return the first element in the symbol table, or Null_Symbol if the
      --  symbol table is empty.

      function Next_Symbol
        (Obj  : Object_File;
         Prev : Object_Symbol) return Object_Symbol;
      --  Return the element following Prev in the symbol table or Null_Symbol
      --  if Prev is the last symbol in the table.

      function Read_ST_Entry (Obj : Object_File) return Symtab_Entry;
      --  Read a Symtab_Entry at the current offset

      procedure Set_Function_Size_Heuristically
        (Obj : Object_File;
         Sym : in out Object_Symbol);
      --  Do our best to determine and set the size of a symbol pointing to a
      --  function.

      function Symbol_Name
        (Obj : Object_File;
         Sym : Object_Symbol) return String;
      --  Return the name of the symbol

      function Read_Section_Headers (Obj : Object_File) return
        Section_Header_Table;
      --  Read the complete section table

      function Section_Name
        (Obj : Object_File;
         Sec : Object_Section) return String;
      --  Return the name of a section

      function String_Table (Obj : Object_File; Index : Offset) return String;
      --  Return an entry from the string table

      function Get_Section
        (Obj   : Object_File;
         Index : uint32) return Object_Section;
      --  Fetch a section by index from zero

      function Get_Section
        (Obj  : Object_File;
         Name : String) return Object_Section;
      --  Fetch a section by name

      function Get_Section_Virtual_Address
        (Obj   : Object_File;
         Index : uint32) return uint64;
      --  Fetch the address at which a section is loaded

      procedure Seek_Section (Obj : Object_File; Sec : Object_Section);
      --  Seek to the first byte of the specified section in the object file

      function Trim_Trailing_Nulls (Str : String) return String;
      --  Return a copy of a string with any trailing null characters truncated

   end PECOFF_Ops;

   package body ELF_Ops is

      function Read_Symbol
        (Obj : Object_File;
         Off : Offset; Num : uint64) return Object_Symbol;
      --  Read a symbol at offset Off

      ------------------
      -- First_Symbol --
      ------------------

      function First_Symbol (Obj : Object_File) return Object_Symbol is
      begin
         if Obj.Num_Symbols = 0 then
            return Null_Symbol;
         end if;

         return Read_Symbol (Obj, Obj.Symtab, 0);
      end First_Symbol;

      -----------------
      -- Get_Section --
      -----------------

      function Get_Section
        (Obj   : Object_File;
         Shnum : uint32) return Object_Section
      is
         SHdr : constant Section_Header :=
                  Read_Section_Header (Obj, Shnum);
      begin
         return (Shnum, Offset (SHdr.Sh_Offset), uint64 (SHdr.Sh_Size));
      end Get_Section;

      function Get_Section
        (Obj  : Object_File;
         Name : String) return Object_Section
      is
         Sec : Object_Section;

      begin
         for J in 0 .. Obj.Num_Sections - 1 loop
            Sec := Get_Section (Obj, J);
            if Section_Name (Obj, Sec) = Name then
               return Sec;
            end if;
         end loop;

         raise Format_Error with
           "could not find section in ELF object file";
      end Get_Section;

      ------------------------
      --  Get_String_Table  --
      ------------------------

      function Get_String_Table (Obj : Object_File) return Object_Section is
      begin
         --  All cases except MIPS IRIX, string table located in .strtab

         if Obj.Arch /= MIPS then
            return Get_Section (Obj, ".strtab");

         --  On IRIX only .dynstr is available

         else
            return Get_Section (Obj, ".dynstr");
         end if;
      end Get_String_Table;

      ------------------------
      --  Get_Symbol_Table  --
      ------------------------

      function Get_Symbol_Table (Obj : Object_File) return Object_Section is
      begin
         --  All cases except MIPS IRIX, symbol table located in .symtab

         if Obj.Arch /= MIPS then
            return Get_Section (Obj, ".symtab");

         --  On IRIX, symbol table located somewhere other than .symtab

         else
            return Get_Section (Obj, ".dynsym");
         end if;
      end Get_Symbol_Table;

      ------------------
      -- Next_Symbol --
      ------------------

      function Next_Symbol
        (Obj  : Object_File;
         Prev : Object_Symbol) return Object_Symbol
      is
      begin
         if Prev.Num = Obj.Num_Symbols - 1 then

            --  Return Null_Symbol if Prev is the last entry in the table

            return Null_Symbol;

         else
            --  Otherwise read the next symbol in the table and return it

            return Read_Symbol (Obj, Prev.Next, Prev.Num + 1);
         end if;
      end Next_Symbol;

      -----------------
      -- Read_Header --
      -----------------

      function Read_Header (Obj : Object_File) return Header is
         Old_Off : Offset;
         Hdr     : Header;
      begin
         Tell (Obj, Old_Off);
         Seek (Obj, 0);
         Read (Obj, Hdr'Address, uint32 (Hdr'Size / SSU), 1);
         Seek (Obj, Old_Off);
         return Hdr;
      end Read_Header;

      -------------------------
      -- Read_Section_Header --
      -------------------------

      function Read_Section_Header
        (Obj   : Object_File;
         Shnum : uint32) return Section_Header
      is
         Old_Off : Offset;
         Hdr     : Header;
         Shdrs   : Section_Header_Table;

      begin
         Tell (Obj, Old_Off);
         Hdr := Read_Header (Obj);
         Seek (Obj, Offset (Hdr.E_Shoff));
         Read (Obj, Shdrs'Address,
               uint32 (Hdr.E_Shentsize),
               uint32 (Hdr.E_Shnum));

         if Hdr.E_Shnum > MAX_SECTIONS then
            raise Format_Error with "too many sections in object file";
         end if;

         Seek (Obj, Old_Off);
         return Shdrs (uint16 (Shnum));
      end Read_Section_Header;

      -----------------
      -- Read_Symbol --
      -----------------

      function Read_Symbol
        (Obj : Object_File;
         Off : Offset;
         Num : uint64) return Object_Symbol
      is
         Old_Off    : Offset;
         ST_Entry32 : Symtab_Entry32;
         ST_Entry64 : Symtab_Entry64;
         Res        : Object_Symbol;

      begin
         Tell (Obj, Old_Off);
         Seek (Obj, Off);

         case uword'Size is
            when 32 =>
               Read (Obj, ST_Entry32'Address,
                     uint32 (ST_Entry32'Size / SSU), 1);
               Res := (Num,
                       Off,
                       Off + ST_Entry32'Size / SSU,
                       uint64 (ST_Entry32.St_Value),
                       uint64 (ST_Entry32.St_Size));
            when 64 =>
               Read (Obj, ST_Entry64'Address,
                     uint32 (ST_Entry64'Size / SSU), 1);
               Res := (Num,
                       Off,
                       Off + ST_Entry64'Size / SSU,
                       ST_Entry64.St_Value,
                       ST_Entry64.St_Size);
            when others =>
               raise Program_Error;
         end case;

         Seek (Obj, Old_Off);
         return Res;
      end Read_Symbol;

      ------------------
      -- Section_Name --
      ------------------

      function Section_Name
        (Obj : Object_File;
         Sec : Object_Section) return String
      is
         Old_Off        : Offset;
         Name_Offset    : Offset;
         Hdr            : Header;
         SHdr           : Section_Header;
         String_Tbl_Hdr : Section_Header;

      begin
         Tell (Obj, Old_Off);
         Hdr := Read_Header (Obj);
         SHdr := Read_Section_Header (Obj, Sec.Num);
         String_Tbl_Hdr :=
           Read_Section_Header (Obj, uint32 (Hdr.E_Shstrndx));
         Name_Offset :=
           Offset (String_Tbl_Hdr.Sh_Offset + uword (SHdr.Sh_Name));
         Seek (Obj, Old_Off);
         return Offset_To_String (Obj, Name_Offset);
      end Section_Name;

      ------------------
      -- Seek_Section --
      ------------------

      procedure Seek_Section (Obj : Object_File; Sec : Object_Section) is
      begin
         Seek (Obj, Sec.Off);
      end Seek_Section;

      -----------------
      -- Symbol_Name --
      -----------------

      function Symbol_Name
        (Obj : Object_File;
         Sym : Object_Symbol) return String
      is
         Old_Off    : Offset;
         ST_Entry32 : Symtab_Entry32;
         ST_Entry64 : Symtab_Entry64;
         Name_Off   : Offset;

      begin
         --  Test that this symbol is not null

         if Sym = Null_Symbol then
            return "";
         end if;

         --  Read the symbol table entry

         Tell (Obj, Old_Off);
         Seek (Obj, Sym.Loc);

         case uword'Size is
            when 32 =>
               Read (Obj, ST_Entry32'Address,
                     uint32 (ST_Entry32'Size / SSU), 1);
               Name_Off := Offset (ST_Entry32.St_Name);

            when 64 =>
               Read (Obj, ST_Entry64'Address,
                     uint32 (ST_Entry64'Size / SSU), 1);
               Name_Off := Offset (ST_Entry64.St_Name);

            when others =>
               raise Program_Error;
         end case;

         Seek (Obj, Old_Off);

         --  Fetch the name from the string table

         return Offset_To_String (Obj, Obj.Strtab + Name_Off);

      end Symbol_Name;

   end ELF_Ops;

   package ELF32_Ops is new ELF_Ops (uint32);
   package ELF64_Ops is new ELF_Ops (uint64);

   package body PECOFF_Ops is

      -----------------
      -- Decode_Name --
      -----------------

      function Decode_Name
        (Obj      : Object_File;
         Raw_Name : String) return String
      is
         Name_Or_Ref : constant String := Trim_Trailing_Nulls (Raw_Name);
         Off         : Offset;

      begin
         --  We should never find a symbol with a zero length name. If we do it
         --  probably means we are not parsing the symbol table correctly. If
         --  this happens we raise a fatal error.

         if Name_Or_Ref'Length = 0 then
            raise Format_Error with
              "found zero length symbol in symbol table";
         end if;

         if Name_Or_Ref (1) /= '/' then
            return Name_Or_Ref;
         else
            Off := Offset'Value (Name_Or_Ref (2 .. Name_Or_Ref'Last));
            return String_Table (Obj, Off);
         end if;
      end Decode_Name;

      ------------------
      -- First_Symbol --
      ------------------

      function First_Symbol (Obj : Object_File) return Object_Symbol is
         Old_Off  : Offset;
         ST_Entry : Symtab_Entry;
         Sz       : constant Offset := ST_Entry'Size / SSU;
         Result   : Object_Symbol;

      begin
         --  Return Null_Symbol in the case that the symbol table is empty

         if Obj.Num_Symbols = 0 then
            return Null_Symbol;
         end if;

         Tell (Obj, Old_Off);
         Seek (Obj, Obj.Symtab);
         Read (Obj, ST_Entry'Address, uint32 (Sz), 1);

         --  Construct the symbol

         Result := (Num   => 0,
                    Loc   => Obj.Symtab,
                    Next  => Obj.Symtab +
                               Offset (1 + ST_Entry.NumberOfAuxSymbols) * Sz,
                    Value => uint64 (ST_Entry.Value),
                    Size  => 0);

         --  Set the size as accurately as possible if this is a function

         if ST_Entry.TypeField = Function_Symbol_Type then
            Set_Function_Size_Heuristically (Obj, Result);
         end if;

         --  Relocate the address if this symbol is associated with a section

         if ST_Entry.SectionNumber > 0 then
            Result.Value :=
              Result.Value + Get_Section_Virtual_Address
                (Obj, uint32 (ST_Entry.SectionNumber - 1));
         end if;

         Seek (Obj, Old_Off);

         return Result;
      end First_Symbol;

      -----------------
      -- Get_Section --
      -----------------

      function Get_Section
        (Obj   : Object_File;
         Index : uint32) return Object_Section
      is
         Sec : constant Section_Header := Read_Section_Header (Obj, Index);
      begin
         return (Index,
                 Offset (Sec.PointerToRawData),
                 uint64 (Sec.SizeOfRawData));
      end Get_Section;

      -----------------
      -- Get_Section --
      -----------------

      function Get_Section
        (Obj  : Object_File;
         Name : String) return Object_Section
      is
         Sec : Object_Section;

      begin
         for J in 0 .. Obj.Num_Sections - 1 loop
            Sec := Get_Section (Obj, J);

            if Section_Name (Obj, Sec) = Name then
               return Sec;
            end if;
         end loop;

         raise Format_Error with
           "could not find section in PECOFF object file";
      end Get_Section;

      ---------------------------------
      -- Get_Section_Virtual_Address --
      ---------------------------------

      function Get_Section_Virtual_Address
        (Obj   : Object_File;
         Index : uint32) return uint64
      is
         Sec : constant Section_Header := Read_Section_Header (Obj, Index);
      begin
         return Image_Load_Address + uint64 (Sec.VirtualAddress);
      end Get_Section_Virtual_Address;

      ------------------
      -- Next_Symbol --
      ------------------

      function Next_Symbol
        (Obj  : Object_File;
         Prev : Object_Symbol) return Object_Symbol
      is
         Old_Off  : Offset;
         ST_Entry : Symtab_Entry;
         Sz       : constant Offset := ST_Entry'Size / SSU;
         Result   : Object_Symbol;
         Last     : constant Offset := Obj.Symtab +
                                         Offset (Obj.Num_Symbols - 1) * Sz;

      begin
         --  Test whether we've reached the end of the symbol table

         if Prev.Next > Last then
            return Null_Symbol;
         end if;

         --  Seek to the successor of Prev

         Tell (Obj, Old_Off);
         Seek (Obj, Prev.Next);
         Read (Obj, ST_Entry'Address, uint32 (Sz), 1);

         --  Construct the symbol

         Result :=
           (Num   => Prev.Num + 1,
            Loc   => Prev.Next,
            Next  => Prev.Next + Offset (1 + ST_Entry.NumberOfAuxSymbols) * Sz,
            Value => uint64 (ST_Entry.Value),
            Size  => 0);

         --  Set the size as accurately as possible if this is a function

         if ST_Entry.TypeField = Function_Symbol_Type then
            Set_Function_Size_Heuristically (Obj, Result);
         end if;

         --  Relocate the address if this symbol is associated with a section

         if ST_Entry.SectionNumber > 0 then
            Result.Value :=
              Result.Value + Get_Section_Virtual_Address
                (Obj, uint32 (ST_Entry.SectionNumber - 1));
         end if;

         Seek (Obj, Old_Off);

         return Result;
      end Next_Symbol;

      ------------------
      -- Read_Header  --
      ------------------

      function Read_Header (Obj : Object_File) return Header is
         Hdr     : Header;
         Off     : int32;
         Old_Off : Offset;
      begin
         Tell (Obj, Old_Off);
         Seek (Obj, Signature_Loc_Offset);
         Off := Read (Obj);
         Seek (Obj, Offset (Off));
         Read (Obj, Hdr'Address, uint32 (Hdr'Size / SSU), 1);
         Seek (Obj, Old_Off);
         return Hdr;
      end Read_Header;

      -------------------------
      -- Read_Section_Header --
      -------------------------

      function Read_Section_Header
        (Obj   : Object_File;
         Index : uint32) return Section_Header
      is
      begin
         return Read_Section_Headers (Obj) (Index);
      end Read_Section_Header;

      ---------------------------
      -- Read_Section_Headers  --
      ---------------------------

      function Read_Section_Headers
        (Obj : Object_File) return Section_Header_Table
      is
         Old_Off : Offset;
         Off     : uint32;
         Hdr     : Header;
         Secs    : Section_Header_Table;

      begin
         Tell (Obj, Old_Off);

         --  Seek to the end of the object header

         Seek (Obj, Signature_Loc_Offset);
         Off := Read (Obj);
         Seek (Obj, Offset (Off));
         Read (Obj, Hdr'Address, Size_Of_Standard_Header_Fields, 1);

         --  Skip past the optional header

         Seek_Relative (Obj, Offset (Hdr.SizeOfOptionalHeader));

         --  Read the section table

         Read (Obj,
               Secs'Address,
               Section_Header'Size / SSU,
               uint32 (Hdr.NumberOfSections));

         --  Restore offset and return

         Seek (Obj, Old_Off);
         return Secs;
      end Read_Section_Headers;

      ---------------------
      --  Read_ST_Entry  --
      ---------------------

      function Read_ST_Entry (Obj : Object_File) return Symtab_Entry is
         Sz     : constant Offset  := Symtab_Entry'Size / SSU;
         Result : Symtab_Entry;
      begin
         Read (Obj, Result'Address, uint32 (Sz), 1);
         Seek_Relative (Obj, Offset (Result.NumberOfAuxSymbols) * Sz);
         return Result;
      end Read_ST_Entry;

      ------------------
      -- Section_Name --
      ------------------

      function Section_Name
        (Obj : Object_File;
         Sec : Object_Section) return String
      is
         Shdr : constant Section_Header :=
                  Read_Section_Header (Obj, Sec.Num);
      begin
         return Decode_Name (Obj, Shdr.Name);
      end Section_Name;

      -------------------
      -- Seek_Section  --
      -------------------

      procedure Seek_Section (Obj : Object_File; Sec : Object_Section) is
      begin
         Seek (Obj, Sec.Off);
      end Seek_Section;

      ---------------------------------------
      --  Set_Function_Size_Heuristically  --
      ---------------------------------------

      procedure Set_Function_Size_Heuristically
        (Obj : Object_File;
         Sym : in out Object_Symbol)
      is
         N        : uint64;
         ST_First : Symtab_Entry;
         ST_Last  : Symtab_Entry;

      begin
         --  The size of a symbol is not directly available so we try scanning
         --  to the next function and assuming the code ends there.

         Sym.Size := 0;
         Seek (Obj, Sym.Loc);

         N := Sym.Num;
         ST_First := Read_ST_Entry (Obj);

         if ST_First.TypeField /= Function_Symbol_Type then
            return;
         end if;

         --  If this is the last entry in the table then there's no next entry
         --  to use for the size of the function. Since symbol size information
         --  is used to test whether addresses fall within a range a very loose
         --  upper bound will do, we use uint32'Last as an arbitrary huge size
         --  which won't overflow.

         Sym.Size := uint64 (uint32'Last);
         while N < Obj.Num_Symbols - 1 loop
            ST_Last := Read_ST_Entry (Obj);
            N := N + 1;
            exit when ST_Last.Value < ST_First.Value;

            if ST_Last.TypeField = Function_Symbol_Type then
               Sym.Size := uint64 (ST_Last.Value - ST_First.Value);
               exit;
            end if;
         end loop;
      end Set_Function_Size_Heuristically;

      -------------------
      -- String_Table  --
      -------------------

      function String_Table
        (Obj   : Object_File;
         Index : Offset) return String
      is
         Hdr : constant Header := Read_Header (Obj);
         Off : Offset;

      begin
         --  An index of zero is used to represent an empty string, as the
         --  first word of the string table is specified to contain the length
         --  of the table rather than its contents.

         if Index = 0 then
            return "";

         else
            Off :=
              Offset (Hdr.PointerToSymbolTable) +
              Offset (Hdr.NumberOfSymbols * 18) +
              Index;
            return Offset_To_String (Obj, Off);
         end if;
      end String_Table;

      -----------------
      -- Symbol_Name --
      -----------------

      function Symbol_Name
        (Obj : Object_File;
         Sym : Object_Symbol) return String
      is
         ST_Entry : Symtab_Entry;
         Old_Off  : Offset;

      begin
         Tell (Obj, Old_Off);

         Seek (Obj, Sym.Loc);
         Read (Obj, ST_Entry'Address, ST_Entry'Size / SSU, 1);
         Seek (Obj, Old_Off);

         declare
            --  Symbol table entries are packed and Table_Entry.Name may not be
            --  sufficiently aligned to interpret as a 32 bit word, so it is
            --  copied to a temporary

            Aligned_Name : Name_Str := ST_Entry.Name;
            for Aligned_Name'Alignment use 4;

            First_Word : uint32;
            for First_Word'Address use Aligned_Name (1)'Address;

            Second_Word : uint32;
            for Second_Word'Address use Aligned_Name (5)'Address;

         begin
            if First_Word = 0 then
               return String_Table (Obj, int64 (Second_Word));
            else
               return Trim_Trailing_Nulls (ST_Entry.Name);
            end if;
         end;
      end Symbol_Name;

      -------------------------
      -- Trim_Trailing_Nulls --
      -------------------------

      function Trim_Trailing_Nulls (Str : String) return String is
         Left_Most : Integer := Str'First;

      begin
         for J in Str'Range loop
            exit when Str (J) = ASCII.NUL;
            Left_Most := J;
         end loop;

         return Str (Str'First .. Left_Most);
      end Trim_Trailing_Nulls;

   end PECOFF_Ops;

   ----------
   -- Arch --
   ----------

   function Arch (Obj : Object_File) return Object_Arch is
   begin
      return Obj.Arch;
   end Arch;

   -----------
   -- Close --
   -----------

   procedure Close (Obj : in out Object_File) is
   begin
      if fclose (Obj.fp) /= 0 then
         raise IO_Error with "could not close object file";
      end if;

      Obj.fp := NULL_Stream;
   end Close;

   ----------------------
   -- Decoded_Ada_Name --
   ----------------------

   function Decoded_Ada_Name
     (Obj : Object_File;
      Sym : Object_Symbol) return String
   is
      procedure gnat_decode
        (Coded_Name_Addr : Address;
         Ada_Name_Addr   : Address;
         Verbose         : int);
      pragma Import (C, gnat_decode, "__gnat_decode");

      subtype size_t is Interfaces.C.size_t;
      function strlen (Str_Addr : Address) return size_t;
      pragma Import (C, strlen, "strlen");

      Raw     : char_array := To_C (Name (Obj, Sym));
      Raw_Len : constant size_t := strlen (Raw'Address);
      Decoded : char_array (0 .. Raw_Len * 2 + 60);

   begin

      --  In the PECOFF case most but not all symbol table entries have an
      --  extra leading underscore. In this case we trim it.

      if Obj.Format = PECOFF and then Raw (0) = '_' then
         gnat_decode (Raw (1)'Address, Decoded'Address, 0);
      else
         gnat_decode (Raw'Address, Decoded'Address, 0);
      end if;

      return To_Ada (Decoded);
   end Decoded_Ada_Name;

   --------------------------
   -- Detect_Object_Format --
   --------------------------

   procedure Detect_Object_Format (Obj : in out Object_File) is

      function Detect_ELF return Boolean;
      --  If this is an ELF format file set Obj.Format and Obj.Arch and return
      --  True, otherwise return False.

      function Detect_PECOFF return Boolean;
      --  If this is an PECOFF format file set Obj.Format and Obj.Arch and
      --  return True, otherwise return False.

      ----------------
      -- Detect_ELF --
      ----------------

      function Detect_ELF return Boolean is
         Hdr : constant ELF64_Ops.Header := ELF64_Ops.Read_Header (Obj);

      begin
         --  Look for the magic numbers for the ELF case

         if Hdr.E_Ident (0) = 16#7F#              and then
            Hdr.E_Ident (1) = Character'Pos ('E') and then
            Hdr.E_Ident (2) = Character'Pos ('L') and then
            Hdr.E_Ident (3) = Character'Pos ('F')
         then
            if Hdr.E_Ident (4) = ELF64_Ops.ELFCLASS32 then
               Obj.Format := ELF32;
            elsif Hdr.E_Ident (4) = ELF64_Ops.ELFCLASS64 then
               Obj.Format := ELF64;
            end if;

         else
            return False;
         end if;

         --  Attempt to detect architecture for the ELF case

         if Obj.Format = ELF32 or else Obj.Format = ELF64 then
            case Hdr.E_Machine is
               when ELF64_Ops.EM_SPARC        |
                    ELF64_Ops.EM_SPARC32PLUS  =>
                  Obj.Arch := SPARC;
               when ELF64_Ops.EM_386          =>
                  Obj.Arch := i386;
               when ELF64_Ops.EM_MIPS         |
                    ELF64_Ops.EM_MIPS_RS3_LE  =>
                  Obj.Arch := MIPS;
               when ELF64_Ops.EM_PPC          =>
                  Obj.Arch := PPC;
               when ELF64_Ops.EM_PPC64        =>
                  Obj.Arch := PPC64;
               when ELF64_Ops.EM_SPARCV9      =>
                  Obj.Arch := SPARC64;
               when ELF64_Ops.EM_IA_64        =>
                  Obj.Arch := IA64;
               when ELF64_Ops.EM_X86_64       =>
                  Obj.Arch := x86_64;
               when others                    =>
                  raise Format_Error with "unrecognized architecture";
            end case;
         end if;

         return True;
      end Detect_ELF;

      -------------------
      -- Detect_PECOFF --
      -------------------

      function Detect_PECOFF return Boolean is
         Hdr : PECOFF_Ops.Header;

      begin
         --  Read the file header

         Hdr := PECOFF_Ops.Read_Header (Obj);

         --  Test the magic numbers

         if Hdr.Magics (0) = Character'Pos ('P') and then
            Hdr.Magics (1) = Character'Pos ('E') and then
            Hdr.Magics (2) = 0                   and then
            Hdr.Magics (3) = 0
         then
            case Hdr.Variant is
               when PECOFF_Ops.VARIANT_PE32 =>
                  Obj.Format := PECOFF;
               when PECOFF_Ops.VARIANT_PE32_PLUS =>
                  Obj.Format := PECOFF_PLUS;
               when others =>
                  raise Format_Error with "unrecognized PECOFF variant";
            end case;

            case (Hdr.Machine) is
               when PECOFF_Ops.IMAGE_FILE_MACHINE_I386  =>
                  Obj.Arch := i386;
               when PECOFF_Ops.IMAGE_FILE_MACHINE_IA64  =>
                  Obj.Arch := IA64;
               when PECOFF_Ops.IMAGE_FILE_MACHINE_AMD64 =>
                  Obj.Arch := x86_64;
               when others =>
                  raise Format_Error with "unrecognized architecture";
            end case;

         else
            return False;
         end if;

         return True;

      exception
         --  If this is not a PECOFF file then we've done a seek and read to a
         --  random address, possibly raising IO_Error

         when IO_Error =>
            return False;
      end Detect_PECOFF;

   --  Start of processing for Detect_Object_Format

   begin
      if Detect_ELF then
         return;
      elsif Detect_PECOFF then
         return;
      else
         raise Format_Error with "unrecognized object format";
      end if;
   end Detect_Object_Format;

   ------------------
   -- First_Symbol --
   ------------------

   function First_Symbol
     (Obj : Object_File) return Object_Symbol
   is
   begin
      case Obj.Format is
         when ELF32       =>
            return ELF32_Ops.First_Symbol (Obj);
         when ELF64       =>
            return ELF64_Ops.First_Symbol (Obj);
         when PECOFF      |
              PECOFF_PLUS =>
            return PECOFF_Ops.First_Symbol (Obj);
         when others      =>
            raise Format_Error with "unrecognized object format";
      end case;
   end First_Symbol;

   ------------
   -- Format --
   ------------

   function Format (Obj : Object_File) return Object_Format is
   begin
      return Obj.Format;
   end Format;

   -----------------
   -- Get_Section --
   -----------------

   function Get_Section
     (Obj   : Object_File;
      Shnum : uint32) return Object_Section
   is
   begin
      case Obj.Format is
         when ELF32       =>
            return ELF32_Ops.Get_Section (Obj, Shnum);
         when ELF64       =>
            return ELF64_Ops.Get_Section (Obj, Shnum);
         when PECOFF      |
              PECOFF_PLUS =>
            return PECOFF_Ops.Get_Section (Obj, Shnum);
         when others      =>
            raise Format_Error with "unrecognized object format";
      end case;
   end Get_Section;

   function Get_Section
     (Obj  : Object_File;
      Name : String) return Object_Section
   is
   begin
      case Obj.Format is
         when ELF64        =>
            return ELF64_Ops.Get_Section (Obj, Name);
         when ELF32        =>
            return ELF32_Ops.Get_Section (Obj, Name);
         when PECOFF       |
              PECOFF_PLUS  =>
            return PECOFF_Ops.Get_Section (Obj, Name);
         when others       =>
            raise Format_Error with "unrecognized object format";
      end case;
   end Get_Section;

   ----------
   -- Name --
   ----------

   function Name (Obj : Object_File; Sec : Object_Section) return String is
   begin
      case Obj.Format is
         when ELF32        =>
            return ELF32_Ops.Section_Name (Obj, Sec);
         when ELF64        =>
            return ELF64_Ops.Section_Name (Obj, Sec);
         when PECOFF       |
              PECOFF_PLUS  =>
            return PECOFF_Ops.Section_Name (Obj, Sec);
         when others       =>
            raise Format_Error with "unrecognized object format";
      end case;
   end Name;

   function Name (Obj : Object_File; Sym : Object_Symbol) return String is
   begin
      case Obj.Format is
         when ELF32       =>
            return ELF32_Ops.Symbol_Name (Obj, Sym);
         when ELF64       =>
            return ELF64_Ops.Symbol_Name (Obj, Sym);
         when PECOFF      |
              PECOFF_PLUS  =>
            return PECOFF_Ops.Symbol_Name (Obj, Sym);
         when others      =>
            raise Format_Error with "unrecognized object format";
      end case;
   end Name;

   -----------------
   -- Next_Symbol --
   -----------------

   function Next_Symbol
     (Obj  : Object_File;
      Prev : Object_Symbol) return Object_Symbol
   is
   begin
      case Obj.Format is
         when ELF32        =>
            return ELF32_Ops.Next_Symbol (Obj, Prev);
         when ELF64        =>
            return ELF64_Ops.Next_Symbol (Obj, Prev);
         when PECOFF       |
              PECOFF_PLUS  =>
            return PECOFF_Ops.Next_Symbol (Obj, Prev);
         when others       =>
            raise Format_Error with "unrecognized object format";
      end case;
   end Next_Symbol;

   ---------
   -- Num --
   ---------

   function Num (Sec : Object_Section) return uint32 is
   begin
      return Sec.Num;
   end Num;

   ------------------
   -- Num_Sections --
   ------------------

   function Num_Sections (Obj : Object_File) return uint32 is
   begin
      return Obj.Num_Sections;
   end Num_Sections;

   -----------------
   -- Num_Symbols --
   -----------------

   function Num_Symbols (Obj : Object_File) return uint64 is
   begin
      return Obj.Num_Symbols;
   end Num_Symbols;

   ---------
   -- Off --
   ---------

   function Off (Sec : Object_Section) return Offset is
   begin
      return Sec.Off;
   end Off;

   ----------------------
   -- Offset_To_String --
   ----------------------

   function Offset_To_String
     (Obj : Object_File;
      Off : Offset) return String
   is
      Old_Off : Offset;
      Buf     : Buffer;
   begin
      Tell (Obj, Old_Off);
      Seek (Obj, Off);
      Read_C_String (Obj, Buf);
      Seek (Obj, Old_Off);
      return To_String (Buf);
   end Offset_To_String;

   ----------
   -- Open --
   ----------

   procedure Open (Obj : out Object_File; File_Name : String) is
      C_Name : char_array := To_C (File_Name);
      C_Mode : char_array := To_C ("rb");

   begin
      --  Open the file

      Obj.fp := fopen (C_Name'Address, C_Mode'Address);

      if Obj.fp = NULL_Stream then
         raise IO_Error with "could not open object file";
      end if;

      --  Detect the object type

      Detect_Object_Format (Obj);

      --  Initialize section and symbol counts and locations

      case Obj.Format is

         when ELF32 =>
            declare
               Hdr : constant ELF32_Ops.Header :=
                       ELF32_Ops.Read_Header (Obj);
               Sec : Object_Section;
            begin
               Obj.Num_Sections := uint32 (Hdr.E_Shnum);
               Obj.Strtab := ELF32_Ops.Get_String_Table (Obj).Off;
               Sec := ELF32_Ops.Get_Symbol_Table (Obj);
               Obj.Symtab := Sec.Off;
               Obj.Num_Symbols :=
                 Sec.Size / (ELF32_Ops.Symtab_Entry32'Size / SSU);
            end;

         when ELF64 =>
            declare
               Hdr : constant ELF64_Ops.Header :=
                       ELF64_Ops.Read_Header (Obj);
               Sec : Object_Section;
            begin
               Obj.Num_Sections := uint32 (Hdr.E_Shnum);
               Obj.Strtab := ELF64_Ops.Get_String_Table (Obj).Off;
               Sec := ELF64_Ops.Get_Symbol_Table (Obj);
               Obj.Symtab := Sec.Off;
               Obj.Num_Symbols :=
                 Sec.Size / (ELF64_Ops.Symtab_Entry64'Size / SSU);
            end;

         when PECOFF | PECOFF_PLUS =>
            declare
               Hdr : constant PECOFF_Ops.Header :=
                       PECOFF_Ops.Read_Header (Obj);
            begin
               Obj.Num_Symbols  := uint64 (Hdr.NumberOfSymbols);
               Obj.Num_Sections := uint32 (Hdr.NumberOfSections);
               Obj.Symtab       := Offset (Hdr.PointerToSymbolTable);
            end;

         when Unknown =>
            raise Format_Error with "unrecognized object format";

      end case;
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (Obj  : Object_File;
      Addr : Address;
      Size : uint32; Count : uint32)
   is
      subtype size_t is Interfaces.C_Streams.size_t;
      Num_Read : uint32;

   begin
      Num_Read := uint32 (fread (Addr, size_t (Size), size_t (Count), Obj.fp));

      if Num_Read /= Count then
         raise IO_Error with "could not read from object file";
      end if;
   end Read;

   function Read (Obj : Object_File) return uint8 is
      Data : uint8;
   begin
      Read (Obj, Data'Address, Data'Size / SSU, 1);
      return Data;
   end Read;

   function Read (Obj : Object_File) return uint16 is
      Data : uint16;
   begin
      Read (Obj, Data'Address, Data'Size / SSU, 1);
      return Data;
   end Read;

   function Read (Obj : Object_File) return uint32 is
      Data : uint32;
   begin
      Read (Obj, Data'Address, Data'Size / SSU, 1);
      return Data;
   end Read;

   function Read (Obj : Object_File) return uint64 is
      Data : uint64;
   begin
      Read (Obj, Data'Address, Data'Size / SSU, 1);
      return Data;
   end Read;

   function Read (Obj : Object_File) return int8 is
      Data : int8;
   begin
      Read (Obj, Data'Address, Data'Size / SSU, 1);
      return Data;
   end Read;

   function Read (Obj : Object_File) return int16 is
      Data : int16;
   begin
      Read (Obj, Data'Address, Data'Size / SSU, 1);
      return Data;
   end Read;

   function Read (Obj : Object_File) return int32 is
      Data : int32;
   begin
      Read (Obj, Data'Address, Data'Size / SSU, 1);
      return Data;
   end Read;

   function Read (Obj : Object_File) return int64 is
      Data : int64;
   begin
      Read (Obj, Data'Address, Data'Size / SSU, 1);
      return Data;
   end Read;

   ------------------
   -- Read_Address --
   ------------------

   function Read_Address (Obj : Object_File) return uint64 is
      Address_32 : uint32;
      Address_64 : uint64;

   begin
      case Obj.Arch is
         when SPARC | i386 | PPC | MIPS =>
            Address_32 := Read (Obj);
            return uint64 (Address_32);

         when SPARC64 | x86_64 | IA64 | PPC64 =>
            Address_64 := Read (Obj);
            return Address_64;

         when others =>
            raise Format_Error with "unrecognized machine architecture";
      end case;
   end Read_Address;

   -------------------
   -- Read_C_String --
   -------------------

   procedure Read_C_String (Obj : Object_File; B : out Buffer) is
      J : Integer := 0;

   begin
      loop
         --  Handle overflow case

         if J = B'Last then
            B (J) := 0;
            exit;
         end if;

         B (J) := Read (Obj);
         exit when B (J) = 0;
         J := J + 1;
      end loop;
   end Read_C_String;

   -----------------
   -- Read_LEB128 --
   -----------------

   function Read_LEB128 (Obj : Object_File) return uint32 is
      B     : uint8;
      Shift : Integer := 0;
      Res   : uint32 := 0;

   begin
      loop
         B := Read (Obj);
         Res := Res or Shift_Left (uint32 (B and 16#7f#), Shift);
         exit when (B and 16#80#) = 0;
         Shift := Shift + 7;
      end loop;

      return Res;
   end Read_LEB128;

   function Read_LEB128 (Obj : Object_File) return int32 is
      B     : uint8;
      Shift : Integer := 0;
      Res   : uint32 := 0;

   begin
      loop
         B := Read (Obj);
         Res := Res or Shift_Left (uint32 (B and 16#7f#), Shift);
         Shift := Shift + 7;
         exit when (B and 16#80#) = 0;
      end loop;

      if Shift < 32 and then (Res and Shift_Left (1, Shift - 1)) /= 0 then
         Res := Res or Shift_Left (-1, Shift);
      end if;

      return To_int32 (Res);
   end Read_LEB128;

   ----------
   -- Seek --
   ----------

   procedure Seek (Obj : Object_File; Off : Offset) is
      rv : Interfaces.C_Streams.int;
      subtype long is Interfaces.C_Streams.long;

   begin
      rv := fseek (Obj.fp, long (Off), SEEK_SET);

      if rv /= 0 then
         raise IO_Error with "could not seek to offset in object file";
      end if;
   end Seek;

   procedure Seek (Obj : Object_File; Sec : Object_Section) is
   begin
      case Obj.Format is
         when ELF32       =>
            ELF32_Ops.Seek_Section (Obj, Sec);
         when ELF64       =>
            ELF64_Ops.Seek_Section (Obj, Sec);
         when PECOFF      |
              PECOFF_PLUS =>
            PECOFF_Ops.Seek_Section (Obj, Sec);
         when others      =>
            raise Format_Error with "unrecognized object format";
      end case;
   end Seek;

   -------------------
   -- Seek_Relative --
   -------------------

   procedure Seek_Relative (Obj : Object_File; Count : Offset) is
      rv : Interfaces.C_Streams.int;
      subtype long is Interfaces.C_Streams.long;

   begin
      rv := fseek (Obj.fp, long (Count), SEEK_CUR);

      if rv /= 0 then
         raise IO_Error with "could not seek in object file";
      end if;
   end Seek_Relative;

   ----------
   -- Size --
   ----------

   function Size (Sec : Object_Section) return uint64 is
   begin
      return Sec.Size;
   end Size;

   function Size (Sym : Object_Symbol) return uint64 is
   begin
      return Sym.Size;
   end Size;

   ------------
   -- Spans  --
   ------------

   function Spans (Sym : Object_Symbol; Addr : uint64) return Boolean is
   begin
      return Addr >= Sym.Value and then Addr < Sym.Value + Sym.Size;
   end Spans;

   ------------
   -- Strlen --
   ------------

   function Strlen (Buf : Buffer) return int32 is
   begin
      for J in Buf'Range loop
         if Buf (J) = 0 then
            return int32 (J);
         end if;
      end loop;

      return Buf'Length;
   end Strlen;

   ----------
   -- Tell --
   ----------

   procedure Tell (Obj : Object_File; Off : out Offset) is
   begin
      Off :=  Offset (ftell (Obj.fp));
   end Tell;

   ---------------
   -- To_String --
   ---------------

   function To_String (Buf : Buffer) return String is
      Count  : constant Integer := Integer (Strlen (Buf));
      Result : String (1 .. Count);

   begin
      if Count = 0 then
         return "";
      end if;

      for J in 0 .. Count - 1 loop
         Result (J + 1) := Character'Val (Buf (J));
      end loop;

      return Result;
   end To_String;

   -----------
   -- Value --
   -----------

   function Value (Sym : Object_Symbol) return uint64 is
   begin
      return Sym.Value;
   end Value;

end System.Object_Reader;
