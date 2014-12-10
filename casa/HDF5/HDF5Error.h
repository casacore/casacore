//# HDF5Error.h: HDF5 error classes
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

#ifndef CASA_HDF5ERROR_H
#define CASA_HDF5ERROR_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# This header file defines the error classes belonging to the table
//# descriptor class and its associated classes.


// <summary>
// Base error class for HDF5 wrapper classes
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <synopsis> 
// This is the generic HDF5 exception; catching this one means catching
// all HDF5* exceptions.
// Note that you have to catch AipsError to catch all possible exceptions.
// </synopsis> 

class HDF5Error : public AipsError
{
public:
  // The default constructor generates the message "HDF5 error".
  HDF5Error (Category c=GENERAL);
  // Construct with given message.
  HDF5Error (const String& message, Category c=GENERAL);
  ~HDF5Error() throw();
};


} //# NAMESPACE CASACORE - END

#endif
