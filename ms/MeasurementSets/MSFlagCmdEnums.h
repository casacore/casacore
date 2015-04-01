//# MSFlagCmdEnums.h: Defs for the MS FLAG_CMD table
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

#ifndef MS_MSFLAGCMDENUMS_H
#define MS_MSFLAGCMDENUMS_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <use visibility=export>

// <summary> Enums for the MeasurementSet FLAG_CMD table </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
// <etymology>
// This class contains the enums for the MeasurementSet FLAG_CMD table
// </etymology>
// <synopsis>
// This class does nothing.  It is merely a container for the enumerations
// used by the MSFlagCmd class.  These enumerations define the
// standard columns, keywords.
// </synopsis>

// <example>
// See the documentation for MeasurementSet for examples on the use of these
// enumerations.
// </example>
// <todo asof="1999/01/13">
// All the todo items which may be related to this class are
// grouped with the todo items for MeasurementSet
// </todo>
//

class MSFlagCmdEnums {
public:
    // The FLAG_CMD table colums with predefined meaning.
    enum PredefinedColumns {
    // "True" columns are defined.<BR>
    // TYPE - UNIT - MEASURE
    UNDEFINED_COLUMN=0,
    // applied flag - true if flag has been applied to main table <BR>
    // Bool
    APPLIED,
    // flag command <BR>
    // String
    COMMAND,
    // Time interval <BR>
    // Double
    INTERVAL,
    // Flag level - revision <BR>
    // Int
    LEVEL,
    // Flag reason, user specified <BR>
    // String
    REASON,
    // severity code (0-10) <BR>
    // Int
    SEVERITY,
    // Midpoint of time interval for which this flag command applies <BR>
    // Double
    TIME,
    // Flag type: FLAG or UNFLAG <BR>
    // String
    TYPE,
    // Number of required columns
    NUMBER_REQUIRED_COLUMNS=TYPE,
    // Not a column, but just a final enum specifying the number of enums.
    NUMBER_PREDEFINED_COLUMNS=NUMBER_REQUIRED_COLUMNS
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
