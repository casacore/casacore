/*
    maxdimc.h: Header file with miriad parameters for miriad library.
    Copyright (C) 1999,2001
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

/*
	-------------------------------------------------------------
	maxdimc.h - include file for C code containing MIRIAD-wide
		    parameters

	MAXDIM .... maximum number of elements in any one plane
		    (ie, maximum dimensionality of a map)
	MAXANT .... maximum number of antennae
	MAXBASE ... maximum number of baselines
	MAXCHAN ... maximum number of channels in spectral data

	History:

	04aug91 mjs   Original version.
	05aug91 mjs   Put parentheses around MAXBASE defined value.
        bpw  20jul91  Created as xyzio.h
        mjs  08apr92  Minor mod to compile on VAX
        rjs  23feb93  Merged maxdimc.h and xyzio.h. Include MAXNAX.
	pjt  25oct94  now defined MAXWIN globally
	-------------------------------------------------------------
*/
#if !defined(BIMA_MAXDIMC_H)
#define BIMA_MAXDIMC_H

#define		MAXDIM		2048
#define		MAXANT		28
#define		MAXBASE		((MAXANT * (MAXANT - 1)) / 2)
#define		MAXCHAN		2048
#define		MAXNAX		7
#define         MAXWIN          16
#define         MAXWIDE         18

#ifdef unicos
#  define	MAXBUF		2097152
#endif
#ifdef convex
#  define	MAXBUF		4194304
#endif
#ifdef alliant
#  define	MAXBUF		4194304
#endif
#ifndef MAXBUF
#  define	MAXBUF		1048576
#endif

#endif /* BIMA_MAXDIMC_H */
