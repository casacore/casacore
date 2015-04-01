//# FITS2.h: Transform a Casacore Array to or from a FITS disk file (helper functions)
//# Copyright (C) 1994,1995
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
//# $Id$

#ifndef FITS_FITS2_H
#define FITS_FITS2_H

#include <casacore/casa/aips.h>
//# Would like to forward declare
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/Map.h>
#include <casacore/fits/FITS/hdu.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class String;        // Forward declaration


//<summary> helper function for ReadFITS and WriteFITS</summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
// Helper functions to reduce the tedium/code replication of writing the
// ReadFITS and WriteFITS functions. If a baseclass is introduced that
// aboved the PrimaryArray class that contains functions like operator()
// and copy() then these functions won't be necessary.
//<group name=ReadFITSin>
template<class StorageType>
void ReadFITSin(PrimaryArray<StorageType> &fitsdata,
	      Array<Float> &data,
	      Bool &ok, 
	      String &ErrorMessage,
	      String *unitName,
	      Vector<String> *axisNames,
	      Vector<Float> *refPixel,
	      Vector<Float> *refLocation,
	      Vector<Float> *delta,
	      Map<String, Double> *keywords,
              String *objectName);
//</group>

} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/fits/FITS/FITS2.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
