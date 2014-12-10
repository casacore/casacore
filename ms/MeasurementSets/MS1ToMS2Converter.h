//# MS1ToMS2Converter.h: Definition for ms1 to ms2 converter
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000
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

#ifndef MS_MS1TOMS2CONVERTER_H
#define MS_MS1TOMS2CONVERTER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/Logging/LogIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
class String;


// <summary> 
// Class to convert a MeasurementSet v1 to v2.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <synopsis>
// This class converts a version 1 MeasurementSet to version 2.
// The keyword MS_VERSION tells the version. If not present it is 1.
// If it is found that the MS is already a version 2 one, nothing is done.
// <p>
// It is possible to do the conversion in place. If not, first a copy
// is made which is thereafter changed in place.
// <br>
// The conversion process does the following steps:
// <ul>
//  <li> Create the newly required columns and keywords
//       and fill them with new or existing data.
//  <li> Convert the old way of defining measure info to the new
//       TableMeasures way.
//  <li> Rename obsolete columns by prefixing their names with _OBSOLETE_.
// </ul>
//
// The constructor only keeps the names of the input and output MS.
// The actual conversion is done by the <src>convert</src> function.
// </synopsis>


class MS1ToMS2Converter
{
public:
  // Create the converter for the given output (ms2) and input (ms1) name.
  // The input name has to be an MS version 1. If not, nothing will be done.
  // <br>If <src>inPlace==True</src>, the ms2 name is ignored. In that
  // case the ms is changed in place.
  MS1ToMS2Converter (const String& ms2,
		     const String& ms1,
		     Bool inPlace);

  ~MS1ToMS2Converter();

  // Do the actual conversion.
  Bool convert();

private:
  // Forbid copy constrcutor and assignment.
  // <group>
  MS1ToMS2Converter (const MS1ToMS2Converter&);
  MS1ToMS2Converter& operator= (const MS1ToMS2Converter&);
  // </group>

  // If possible remove a column from the table.
  // Otherwise rename it by prefixing it with _OBSOLETE_.
  void removeColumn(Table& t, const String& col);


  String ms1_p;
  String ms2_p;
  Bool inPlace_p;

  // Logger
  LogIO os_p;
};



} //# NAMESPACE CASACORE - END

#endif
