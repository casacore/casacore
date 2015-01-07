//# DerivedMC.h: Derived MS and CalTable columns
//# Copyright (C) 2010
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

#ifndef DERIVEDMSCAL_DERIVEDMC_H
#define DERIVEDMSCAL_DERIVEDMC_H

#include <casacore/casa/aips.h>
#include <casacore/derivedmscal/DerivedMC/DerivedMSCal.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>
//
// <summary>
// Derived MS and CalTable columns
// </summary>

// <prerequisite>
//   <li> <linkto module="MeasurementSets:description">MeasurementSets</linkto> module
// </prerequisite>
//
// <reviewed reviewer="" date="" demos="">
// </reviewed>

// <etymology>
// Class to handle derived columns in an MS or CalTable.
// </etymology>
//
// <synopsis>
// A MeasurementSet or CalTable can be extended with virtual columns to
// be able to use hourangle, azimuth/elevation, parallactic angle, and UVW
// as if they were stored in the table using the DerivedMSCal virtual column
// engine. Such columns have a fixed name, otherwise the engine cannot handle
// them. The class description of the engine shows which columns can be handled
// and gives an example how to add them.
//
// Class UDFMSCal contains TaQL user defined functions for these virtual
// columns. In this way the columns do not need to be added to the table,
// but can be used directly in a TaQL command. For example:
// <srcblock>
//     select from my.ms where derivedmscal.ha1() > 10deg
// </srcblock>
// to select the rows where the hourangle of ANTENNA1 fulfills the condition.
// If HA1 was added as a virtual column (which is more intrusive), the
// command could look like:
// <srcblock>
//     select from my.ms where HA1 > 10deg
// </srcblock>
//
// UVW coordinates are already stored in an MS, but the virtual UVW_J2000
// column makes it possible to calculate them on the fly.
//
// An MS or CalTable can be created with one or more of these columns or they
// can be added later using 'DerivedMSCal' as their data manager.
// A column can, of course, also be removed. If all these columns are removed,
// the Table System will also remove the data manager from the table.
//
// The derivedmscal library can be used in two ways:
// <ol>
//  <li> It needs to be linked in if the DerivedMSCal class is used. This
//       mode will probably be used rarely.
//  <li> It will be loaded dynamically when a table is opened with columns
//       using this data manager, when a column is added to a table using
//       this data manager by name (thus without an object), or when
//       such a TaQL user defined function is used.
// </ol>
// <note> For the second reason above it is important that the library
// and the other casacore libraries are built shared.
// </note>
// </synopsis> 
//
// <motivation>
// It is very handy to be able to use a column like PA in software that can
// deal with various table columns (e.g. TaQL, TablePlot, pyrap).
// </motivation>

//# <todo asof="1997/02/01">
//# </todo>

// </module>


} //# NAMESPACE CASACORE - END

#endif
