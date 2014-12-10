//# HDF5HidMeta.cc: Classes representing an HDF5 hid of meta objects
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

#include <casacore/casa/HDF5/HDF5HidMeta.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

#ifdef HAVE_HDF5

  void HDF5HidProperty::close()
  {
    if (itsHid>=0) H5Pclose(itsHid);
    itsHid=-1;
  }

  void HDF5HidDataType::close()
  {
    if (itsHid>=0) H5Tclose(itsHid);
    itsHid=-1;
  }

  void HDF5HidDataSpace::close()
  {
    if (itsHid>=0) H5Sclose(itsHid);
    itsHid=-1;
  }

  void HDF5HidAttribute::close()
  {
    if (itsHid>=0) H5Aclose(itsHid);
    itsHid=-1;
  }

#else

  void HDF5HidProperty::close()
  {}

  void HDF5HidDataType::close()
  {}

  void HDF5HidDataSpace::close()
  {}

  void HDF5HidAttribute::close()
  {}

#endif

}
