//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002,2003
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
//

#ifndef LATTICES_LATTSTATSSPECIALIZE2_TCC
#define LATTICES_LATTSTATSSPECIALIZE2_TCC

#include <casacore/lattices/LatticeMath/LattStatsSpecialize.h>

#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/DataType.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T> Bool LattStatsSpecialize::setIncludeExclude(
    String& errorMessage, Vector<T>& range, Bool& noInclude, Bool& noExclude,
    const Vector<T>& include, const Vector<T>& exclude
) {
//  
// Take the user's data inclusion and exclusion data ranges and
// generate the range and Booleans to say what sort it is
//
// Inputs:
//   include   Include range given by user. Zero length indicates
//             no include range   
//   exclude   Exclude range given by user. As above.
// Outputs:
//   noInclude If True user did not give an include range
//   noExclude If True user did not give an exclude range
//   range     A pixel value selection range.  Will be resized to
//             zero length if both noInclude and noExclude are True
//   Bool      True if successfull, will fail if user tries to give too
//             many values for includeB or excludeB, or tries to give
//             values for both
    static const T dummy(0);
    ThrowIf (
        ! isReal(whatType(&dummy)),
        "Logic error, this method is for real data types only"
    );
    noInclude = True;
    range.resize(0);
    if (include.empty()) {
        // do nothing
    }
    else if (include.size() == 1) {
        range.resize(2);
        range(0) = -abs(include(0));
        range(1) =  abs(include(0));
        noInclude = False;
    }
    else if (include.size() == 2) {
        range.resize(2);
        range(0) = min(include(0),include(1));
        range(1) = max(include(0),include(1));
        noInclude = False;
    }
    else {
        errorMessage = String("Too many elements for argument include");
        return False;
    }
    noExclude = True;
    if (exclude.empty()) {
        // do nothing
    }
    else if (exclude.size() == 1) {
        range.resize(2);
        range(0) = -abs(exclude(0));
        range(1) =  abs(exclude(0));
        noExclude = False;
    }
    else if (exclude.size() == 2) {
        range.resize(2);
        range(0) = min(exclude(0),exclude(1));
        range(1) = max(exclude(0),exclude(1));
        noExclude = False;
    }
    else {
        errorMessage = String("Too many elements for argument exclude");
        return False;
    }
    if (! noInclude && ! noExclude) {
        errorMessage = String("You can only give one of arguments include or exclude");
        return False;
    }
    return True;
}
 
} //# NAMESPACE CASACORE - END

#endif

