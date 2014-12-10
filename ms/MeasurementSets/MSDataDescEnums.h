//# MSDataDescEnums.h: Defs for the MS DATA_DESCRIPTION table
//# Copyright (C) 1999,2000
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
//#
//# $Id$

#ifndef MS_MSDATADESCENUMS_H
#define MS_MSDATADESCENUMS_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <use visibility=export>

// <summary> Enums for the MeasurementSet DATA_DESCRIPTION table </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
// <etymology>
// This class contains the enums for the MeasurementSet DATA_DESCRIPTION table
// </etymology>
// <synopsis>
// This class does nothing.  It is merely a container for the enumerations
// used by the MSDataDescription class.  These enumerations define the
// standard columns, keywords.
// </synopsis>

// <example>
// See the documentation for MeasurementSet for examples on the use of these
// enumerations.
// </example>
// <todo asof="1999/01/12">
// All the todo items which may be related to this class are
// grouped with the todo items for MeasurementSet
// </todo>
//

class MSDataDescriptionEnums {
public:
    // The DATA_DESCRIPTION table colums with predefined meaning.
    enum PredefinedColumns {
    // "True" columns are defined.<BR>
    // TYPE - UNIT - MEASURE
    UNDEFINED_COLUMN=0,
    // Flag for this row <BR>
    // Bool
    FLAG_ROW,
    // polarization id, pointer to polarization table <BR>
    // Int.
    POLARIZATION_ID,
    // Spectral window id, pointer to spectral window table <BR>
    // Int.
    SPECTRAL_WINDOW_ID,
    // Number of required columns
    NUMBER_REQUIRED_COLUMNS=SPECTRAL_WINDOW_ID,
    // Lag Id - points to LAG subtable which describes lag correlation functions <BR>
    // Int
    LAG_ID,
    // Not a column, but just a final enum specifying the number of enums.
    NUMBER_PREDEFINED_COLUMNS=LAG_ID
    };
  
    // Keywords with a predefined meaning
    enum PredefinedKeywords {
    //
    // "True" keywords are defined. 
    UNDEFINED_KEYWORD=0,
    //
    // Not a keyword, but just a final enum specifying the number of enums.
    NUMBER_PREDEFINED_KEYWORDS=0
    };
};

} //# NAMESPACE CASACORE - END

#endif
