/*
    hio.h: Header file for file heirarch manipulation for miriad library.
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

#if !defined(BIMA_HIO_H)
#define BIMA_HIO_H

#include "sysdep.h"

#define H_BYTE		1
#define H_INT		2
#define H_INT2		3
#define H_REAL		4
#define H_DBLE		5
#define H_TXT		6
#define H_CMPLX		7

#define H_BYTE_SIZE	1
#define H_INT_SIZE	4
#define H_INT2_SIZE	2
#define H_REAL_SIZE	4
#define H_DBLE_SIZE	8
#define H_TXT_SIZE	1
#define H_CMPLX_SIZE	8

#define MAXPATH		256
#define MAXOPEN		20

/*
  The following macros define routines which read or write data of a
  particular sort (byte, integer, integer*2, real, etc).
  Offsets and lengths are always measured in bytes!
								   */
#define hreadb_c(item,buf,offset,length,iostat) \
	hio_c(item,FALSE,H_BYTE,buf,offset,length,iostat)
#define hwriteb_c(item,buf,offset,length,iostat) \
	hio_c(item,TRUE,H_BYTE,buf,offset,length,iostat)
#define hreadi_c(item,buf,offset,length,iostat) \
	hio_c(item,FALSE,H_INT,(char *)(buf),offset,length,iostat)
#define hwritei_c(item,buf,offset,length,iostat) \
	hio_c(item,TRUE,H_INT,(char *)(buf),offset,length,iostat)
#define hreadj_c(item,buf,offset,length,iostat) \
	hio_c(item,FALSE,H_INT2,(char *)(buf),offset,length,iostat)
#define hwritej_c(item,buf,offset,length,iostat) \
	hio_c(item,TRUE,H_INT2,(char *)(buf),offset,length,iostat)
#define hreadr_c(item,buf,offset,length,iostat) \
	hio_c(item,FALSE,H_REAL,(char *)(buf),offset,length,iostat)
#define hwriter_c(item,buf,offset,length,iostat) \
	hio_c(item,TRUE,H_REAL,(char *)(buf),offset,length,iostat)
#define hreadd_c(item,buf,offset,length,iostat) \
	hio_c(item,FALSE,H_DBLE,(char *)(buf),offset,length,iostat)
#define hwrited_c(item,buf,offset,length,iostat) \
	hio_c(item,TRUE,H_DBLE,(char *)(buf),offset,length,iostat)
#define hreadc_c(item,buf,offset,length,iostat) \
	hio_c(item,FALSE,H_CMPLX,(char *)(buf),offset,length,iostat)
#define hwritec_c(item,buf,offset,length,iostat) \
	hio_c(item,TRUE,H_CMPLX,(char *)(buf),offset,length,iostat)
#define hwrite_c(item,type,buf,offset,length,iostat) \
	hio_c(item,TRUE,type,(char *)(buf),offset,length,iostat)
#define hread_c(item,type,buf,offset,length,iostat) \
	hio_c(item,FALSE,type,(char *)(buf),offset,length,iostat)

void hopen_c(),hclose_c(),haccess_c(),hdaccess_c(),hio_c(),
	hreada_c(),hwritea_c(),hdelete_c(),habort_c(),hrm_c(),hseek_c(),
	hmode_c(),hflush_c();
int hsize_c(),hexists_c(),htell_c();

/* Other handy definitions. */

#define TRUE 			1
#define FALSE			0
#define max(a,b) ((a)>(b)?(a):(b))
#define min(a,b) ((a)<(b)?(a):(b))
#define mroundup(a,b) ((b)*(((a)+(b)-1)/(b)))

#endif /* BIMA_HIO_H */
