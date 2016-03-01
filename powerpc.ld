/****************************************************************************
 * PowerPC linker script to be fed to ld                                    *
 *                                                                          *
 *        Copyright (C) 2012-2013 Università degli Studi di Padova          *
 ****************************************************************************/

OUTPUT_FORMAT("elf32-powerpc", "elf32-powerpc",
	      "elf32-powerpc")
OUTPUT_ARCH(powerpc)
ENTRY(_start)
/* Do we need any of these for elf?
   __DYNAMIC = 0;    */

_STACK_SIZE = (100 * 1024);

/* 
 Space 0x0000 0000 - 0x0001 0000 is reserved for the 64KB page table
 Space 0xFFF0 0000 - 0xFFFF FFFC is reserved for the exception vector   
*/

_RAM_START = 0x10000 ;
_RAM_END = 0xFFF00000 ;
/* _RAM_START + _RAM_SIZE; */

_PAGE_TABLE_START = 0x00000000;
_PAGE_TABLE_SIZE = 0x10000;
_PAGE_TABLE_END = _PAGE_TABLE_START + _PAGE_TABLE_SIZE;
PROVIDE (_text_start = _PAGE_TABLE_END);

PROVIDE (__stack = _RAM_END);
PROVIDE (_stack_start =  __stack - _STACK_SIZE);
/* size_of_C_stacks =  512K; */


SECTIONS
{
  /* Read-only sections, merged into text segment: */

  /* We reserve the space for the static interrupt vector  */
  . = 0xfff00000;
  .interrupt_vector           :
  {
    *(.start)
  } = 0
  
  . = 0xfffffffc;
  .reset : AT (0xfffffffc)
  {
   *(.reset)
  }

  . = _PAGE_TABLE_END ; 
  .text      : 
  {
    *(.text)
    /* .gnu.warning sections are handled specially by elf32.em.  */
    *(.gnu.warning)
    *(.gnu.linkonce.t*)
  } =0
  .init		  : { *(.init)		} =0
  .fini		  : { *(.fini)		} =0
  .rodata	  : { *(.rodata) *(.gnu.linkonce.r*) }
  .rodata1	  : { *(.rodata1) }
  _etext = .;
  PROVIDE (etext = .);
  .sdata2   : { *(.sdata2) }
  .sbss2   :
  { 
     __sbss2_start = .;
     *(.sbss2) 
    . = ALIGN(4);
    __sbss2_end = .;
  }
  . =  ALIGN(8);
  .data    :
  {
    *(.data)
    *(.gnu.linkonce.d*)
    CONSTRUCTORS
  }
  .data1   : { *(.data1) }
  .got1		  : { *(.got1) }
  .dynamic	  : { *(.dynamic) }
  /* Put .ctors and .dtors next to the .got2 section, so that the pointers
     get relocated with -mrelocatable. Also put in the .fixup pointers.
     The current compiler no longer needs this, but keep it around for 2.7.2  */
		PROVIDE (_GOT2_START_ = .);
  .got2		  :  { *(.got2) }
		PROVIDE (__CTOR_LIST__ = .);
  .ctors	  : { *(.ctors) }
		PROVIDE (__CTOR_END__ = .);
		PROVIDE (__DTOR_LIST__ = .);
  .dtors	  : { *(.dtors) }
		PROVIDE (__DTOR_END__ = .);
		PROVIDE (_FIXUP_START_ = .);
  .fixup	  : { *(.fixup) }
		PROVIDE (_FIXUP_END_ = .);
		PROVIDE (_GOT2_END_ = .);
		PROVIDE (_GOT_START_ = .);
  .got		  : { *(.got) }
  .got.plt	  : { *(.got.plt) }
		PROVIDE (_GOT_END_ = .);
  /* We want the small data sections together, so single-instruction offsets
     can access them all, and initialized data all before uninitialized, so
     we can shorten the on-disk segment size.  */
  .sdata	  : { *(.sdata) }
  _edata  =  .;
  PROVIDE (edata = .);
  .sbss      :
  {
    PROVIDE (__sbss_start = .);
    *(.sbss)
    *(.scommon)
    . = ALIGN(4);
    PROVIDE (__sbss_end = .);
  }
  .bss       :
  {
   PROVIDE (__bss_start = .);
   *(.dynbss)
   *(.bss)
   *(COMMON)
   . = ALIGN(4);
   PROVIDE (__bss_end = .);
  }
  
  /* Allocate the main stack area.  Make sure it starts 16bytes aligned
     to accommodate any possible program expectation.  */
  . = ALIGN(8) + 0x1000;
  PROVIDE (end = .);
  PROVIDE (_end = .);
  PROVIDE (__end = .);
  PROVIDE (__heap1 = .);

  /* These are needed for ELF backends which have not yet been
     converted to the new style linker.  */
  .stab 0 : { *(.stab) }
  .stabstr 0 : { *(.stabstr) }
  /* DWARF debug sections.
     Symbols in the DWARF debugging sections are relative to the beginning
     of the section so we begin them at 0.  */
  /* DWARF 1 */
  .debug          0 : { *(.debug) }
  .line           0 : { *(.line) }
  /* GNU DWARF 1 extensions */
  .debug_srcinfo  0 : { *(.debug_srcinfo) }
  .debug_sfnames  0 : { *(.debug_sfnames) }
  /* DWARF 1.1 and DWARF 2 */
  .debug_aranges  0 : { *(.debug_aranges) }
  .debug_pubnames 0 : { *(.debug_pubnames) }
  /* DWARF 2 */
  .debug_info     0 : { *(.debug_info) }
  .debug_abbrev   0 : { *(.debug_abbrev) }
  .debug_line     0 : { *(.debug_line) }
  .debug_frame    0 : { *(.debug_frame) }
  .debug_str      0 : { *(.debug_str) }
  .debug_loc      0 : { *(.debug_loc) }
  .debug_macinfo  0 : { *(.debug_macinfo) }
  .debug_ranges   0 : { *(.debug_ranges) }
  /* SGI/MIPS DWARF 2 extensions */
  .debug_weaknames 0 : { *(.debug_weaknames) }
  .debug_funcnames 0 : { *(.debug_funcnames) }
  .debug_typenames 0 : { *(.debug_typenames) }
  .debug_varnames  0 : { *(.debug_varnames) }
  /* These must appear regardless of  .  */
}
