//# HDF5Image2.cc: non-templated function in HDF5Image
//# Copyright (C) 2008
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

#include <casacore/images/Images/HDF5Image.h>
#include <casacore/casa/HDF5/HDF5File.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  DataType hdf5imagePixelType (const String& fileName)
  {
    DataType retval = TpOther;
    if (HDF5File::isHDF5(fileName)) {
      try {
	HDF5File file(fileName);
	HDF5Group gid(file, "/", true);
	retval = HDF5DataSet::getDataType (gid.getHid(), "map");
      } catch (AipsError& x) {
	// Nothing
      } 
    }
    return retval;
  }

  Bool isHDF5Image (const String& fileName)
  {
    // It is an image if it is an HDF5 file with group /coordinfo.
    Bool retval = False;
    if (HDF5File::isHDF5(fileName)) {
      try {
	HDF5File file(fileName);
	HDF5Group gid1(file, "/", true);
	HDF5Group gid2(gid1, "coordinfo", true);
	retval = True;
      } catch (AipsError& x) {
	// Nothing
      } 
    }
    return retval;
  }

} //# NAMESPACE CASACORE - END
