//# NewMSDopplerEnums.h: Defs for the NewMS DOPPLER table
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

#if !defined(AIPS_NewMSDOPPLERENUNewMS_H)
#define AIPS_NewMSDOPPLERENUNewMS_H

#include <aips/aips.h>

// <use visibility=export>

// <summary> Enums for the NewMeasurementSet DOPPLER table </summary>
// <etymology>
// This class contains the enums for the NewMeasurementSet DOPPLER table
// </etymology>
// <synopsis>
// This class does nothing.  It is merely a container for the enumerations
// used by the NewMSDoppler class.  These enumerations define the
// standard columns, keywords.
// </synopsis>

// <example>
// See the documentation for NewMeasurementSet for examples on the use of these
// enumerations.
// </example>
// <todo asof="1999/01/13">
// All the todo items which may be related to this class are
// grouped with the todo items for NewMeasurementSet
// </todo>
//

class NewMSDopplerEnums {
public:
    // The DOPPLER table colums with predefined meaning.
    enum PredefinedColumns {
    // "True" columns are defined.<BR>
    // TYPE - UNIT - MEASURE
    UNDEFINED_COLUMN=0,
    // doppler tracking id, used in SPECTRAL_WINDOW table <BR>
    // Int.
    DOPPLER_ID,
    // Source id, pointer to SOURCE table <BR>
    // Int.
    SOURCE_ID,
    // Transition id, index into list of transitions in SOURCE table<BR>
    // Int
    TRANSITION_ID,
    // Velocity definition for Doppler shift
    // Double - m/s - DOPPLER
    VELDEF,
    // Number of required columns
    NUMBER_REQUIRED_COLUMNS=VELDEF,
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
#endif
