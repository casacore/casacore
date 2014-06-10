//# FluxStdSrcs.h: flux standard source enum
//# Copyright (C) 2013
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
//# Correspondence concerning AIPS++ should be adressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#
#ifndef COMPONENTS_FLUXSTDSRCS_H
#define COMPONENTS_FLUXSTDSRCS_H

#include <casa/BasicSL/String.h>
#include <casa/Arrays/Vector.h>
#include <measures/Measures/MDirection.h>
#include <scimath/Mathematics/RigidVector.h>
#include <map>
namespace casa { //# NAMESPACE CASA - BEGIN

// <summary> 
// Basic calibrator data used in the flux standards. 
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="" demos="">

// <prerequisite>
// <li><linkto class="FluxStandard">FluxStandard</linkto> module
// </prerequisite>
//
// <etymology>
// Flux standard sources 
// </etymology>
//
// <synopsis>
// FluxStdSrcs encapsulates an enum of the source names and lists of the directions and 
// alternate names for the flux standards. The utility methods to translate between the enum
// and String name are also defined.
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// Encapsulate data on flux standards
// </motivation>

class FluxStdSrcs
{
public:

  typedef FluxStdSrcs FSS;
  typedef RigidVector<String, 6> RVS6;

  // Source identifiers.
  enum Source {
    THREEC286 = 0,      // 3C286
    THREEC48,
    THREEC147,
    THREEC138,
    NINETEEN34M638,   // 1934-638
    THREEC295,
    THREEC196,
    THREEC123,
    THREEC380,
    // The number of standards in this enumerator.
    NUMBER_SOURCES,
    UNKNOWN_SOURCE = NUMBER_SOURCES
  };

  // Returns an enum of srcName
  FSS::Source srcNameToEnum(const String& srcName, const MDirection& dir) const;

  // Returns srcName string of the srcEnum
  String EnumToSrcName(const FSS::Source srcEnum) const;

  // Get source direction of srcEnum
  MDirection getDirection(const FSS::Source srcEnum) const;

  ~FluxStdSrcs();

protected:
  FluxStdSrcs();

private:
  // A map form an FSS::Source enum to known names
  std::map<FSS::Source, Vector<String> > names_p;
  // A map from an FSS::Source enum to its J2000 direction.
  std::map<FSS::Source, MDirection> directions_p;
};

} //# NAMESPACE CASA - END

#endif /* COMPONENTS_FLUXSTDSRCS_H */
