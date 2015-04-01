//# MSFits.h: FITS related functionality for MeasurementSets
//# Copyright (C) 2005
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

#ifndef MSFITS_MSFITS_H
#define MSFITS_MSFITS_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>
//
// <summary>
// FITS related functionality for MeasurementSets
// </summary>

// <prerequisite>
//   <li> <linkto module="MeasurementSets:description">MeasurementSets</linkto> module
// </prerequisite>
//
//
// <reviewed reviewer="Bob Garwood" date="1997/02/01" demos="">
// </reviewed>

// <synopsis>
// The MeasurementSets module handles storage of telescope data in Casacore
// tables and defines functions to access the data.
// The MSFits module is an extra layer on top of MeasurementSets to convert
// a MeasurementSet to and from FITS.
// There are classes for three FITS formats:
// <ul>
//  <li> Conventional UVFits
//  <li> IDIFits
//  <li> Single Dish FITS
// </ul>
// </synopsis> 

// <motivation>
// To avoid that package <src>ms</src> depends on FITS (and cfitsio),
// all FITS-related MS code is put in a separate package.
// </motivation>

//# <todo asof="1997/02/01">
//# </todo>

// </module>


} //# NAMESPACE CASACORE - END

#endif




