/*
    io.h: Header file for various file and i/o handling in miriad library.
    Copyright (C) 1999
    Associated Universities, Inc. Washington DC, USA.

    This library is free software; you can redistribute it and/or modify it
    under the terms of the GNU Library General Public License as published by
    the Free Software Foundation; either version 2 of the License, or (at your
    option) any later version.

    This library is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
    License for more details.

    You should have received a copy of the GNU Library General Public License
    along with this library; if not, write to the Free Software Foundation,
    Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.

    Correspondence concerning AIPS++ should be addressed as follows:
           Internet email: aips2-request@nrao.edu.
           Postal address: AIPS++ Project Office
                           National Radio Astronomy Observatory
                           520 Edgemont Road
                           Charlottesville, VA 22903-2475 USA

    $Id$
*/

/************************************************************************/
/*									*/
/*	A general header file for the various file and i/o handling	*/
/*	routines.							*/
/*									*/
/*  History:								*/
/*   rjs  Dark-ages Original version.					*/
/*   rjs  20aug92   Correct "roundup" macro when rounding 0.		*/
/*   rjs  15may96   Moved roundup macro elsewhere.			*/
/************************************************************************/

/* Binary items start with a sequence to allow routines to blindly determine
   how to read them. The "binary_item" is a catch all with only indicates
   that the data is binary valued, but does not hint at the format. */

#if !defined(BIMA_IO_H)
#define BIMA_IO_H

#include "hio.h"
#include <unistd.h>

#define ITEM_HDR_SIZE		4
static char 	binary_item[ITEM_HDR_SIZE]	= {0,0,0,0},
		real_item[ITEM_HDR_SIZE]	= {0,0,0,H_REAL},
		int_item[ITEM_HDR_SIZE]		= {0,0,0,H_INT},
		int2_item[ITEM_HDR_SIZE]	= {0,0,0,H_INT2},
		char_item[ITEM_HDR_SIZE]	= {0,0,0,H_BYTE},
		dble_item[ITEM_HDR_SIZE]	= {0,0,0,H_DBLE},
		cmplx_item[ITEM_HDR_SIZE]	= {0,0,0,H_CMPLX};

#endif /* BIMA_IO_H */
