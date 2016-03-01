/****************************************************************************
 *                                                                          *
 *                GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  *
 *                                                                          *
 *                                SOCLIB                                    *
 *                                                                          *
 *                             Assembly File                                *
 *                                                                          *
 *          Copyright (C) 2012-2013 Università degli Studi di Padova        *
 *                                                                          *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 *                                                                          *
 *                                                                          * 
 *                                                                          * 
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 ****************************************************************************/

/* This file offers the SocLib specific serial putput facility required to
 * perform IO operations on the PROARTIS Sim simulator.
 */

	.text
	.align 4

	.global soclibputchar			
	.type  soclibputchar,@function
soclibputchar: 
	stwu    1,-32(1);
	mflr    0;	
	stw     0,36(1);
	stw     31,28(1);
	mr      31,1;
	stw     3,8(31);
	bl      procnum  
	mr      0,3;
	rlwinm  0,0,2,0,29;
	mr      9,0;
	lwz     0,8(31);
	clrlwi  0,0,24;
	lis     3,-16256;
	mr      4,9;
	mr      5,0; 
	bl      soclib_io_write8;
	lwz     0,8(31);
	mr      3,0;
	addi    11,31,32;
	lwz     0,4(11);
	mtlr    0;
	lwz     31,-4(11);
 	mr      1,11;
	blr  

	.size soclibputchar, . - soclibputchar

	.global soclib_io_write8				 
	.type  soclib_io_write8,@function    
soclib_io_write8:  
	stwu    1,-48(1);
	stw     31,44(1); 
	mr      31,1;
	stw     3,24(31);
	stw     4,28(31);
	mr      0,5;
	stb     0,32(31);
	lwz     0,24(31); 
	stw     0,8(31);
	lwz     0,28(31);  
	rlwinm  0,0,2,0,29;
	lwz     9,8(31);
	add     0,9,0;
	stw     0,8(31); 
	lwz     0,8(31);
	lbz     9,32(31);
	mr      11,0;
	stb     9,0(11);
	addi    11,31,48;
	lwz     31,-4(11);
	mr      1,11;
	blr 

	.size soclib_io_write8, . - soclib_io_write8

	.global procnum				 
	.type  procnum,@function    
procnum:
	stwu    1,-32(1);
	stw     31,28(1); 
	mr      31,1;
	.long 0x7c000286;
	stw     0,8(31);
	lwz     0,8(31);
	mr      3,0;
	addi    11,31,32;
	lwz     31,-4(11);
	mr      1,11;
	blr

	.size procnum, . - procnum

/*
int putchar(const int x)
{

	soclib_io_write8(
		base(TTY),
		procnum()*TTY_SPAN+
		TTY_WRITE,
		(uint8_t)x);
	return x;
}
*/
