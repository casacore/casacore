//# ComponentEnums.h: Enumerators common to the Components Module
//# Copyright (C) 1997
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

#if !defined(AIPS_COMPONENTENUMS_H)
#define AIPS_COMPONENTENUMS_H

#include <aips/aips.h>
class String;

// <use visibility=export>

// <etymology>
// This class contains the enum defininitions for the componentModels module
// </etymology>

// <synopsis>
// This class does nothing.  It is merely a container for the enumerations
// used by the ComponentModels module.  These enumerations define the
// standard component types
// </synopsis>

// <example>
// See the documentation for the ComponentModels classes for examples on the
// use of these enumerations.
// </example>
// <todo asof="1997/06/17">
// Nothing I can think of.
// </todo>
//

class ComponentType {
public:
  // The types of all the components
  enum Type {
    // A simple Point Component
    POINT = 0,
    // A elliptical Gaussian Component
    GAUSSIAN,
    // An unknown Component
    UNKNOWN,
    // The number of types in this enumerator
    NUMBER_TYPES
  };
  // Convert the Type enumerator to a string
  static String name(Type componentEnum);
  // Convert a given String to a Type enumerator
  //  static Type getType(const String & componentName);
  static ComponentType::Type type(const String & componentName);
};
#endif
