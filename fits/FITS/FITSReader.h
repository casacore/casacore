//# FITSReader.h: Parse a FITS disk file
//# Copyright (C) 1993,1994,1995,1999
//# Associated Universities, Inc. Washington DC, USA.
//# 
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//# 
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//# 
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//# 
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id: FITSReader.h 2012-01-31$

#ifndef FITS_Reader_H
#define FITS_Reader_H

# include <fits/FITS/fits.h>
# include <fits/FITS/hdu.h>
# include <fits/FITS/fitsio.h>
# include <casa/BasicSL/String.h>
# include <casa/iostream.h>
# include <casa/stdlib.h>
# include <casa/namespace.h>
# include <casa/Arrays/Vector.h>

namespace casa {

class FITSReader {

public:


   // Read and display values from a FITS file by using 
   // the FitsInput::read(FITS::HDUType t, char *addr, int nb) method. 

   void listFits(const char* fitsfile);

};


}

#endif
