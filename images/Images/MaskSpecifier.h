//# MaskSpecifier.h: Class to specify which mask to use in an image
//# Copyright (C) 1999
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

#ifndef IMAGES_MASKSPECIFIER_H
#define IMAGES_MASKSPECIFIER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Class to specify which mask to use in an image.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tPagedImage">
// </reviewed>

// <synopsis> 
// The only purpose of MaskSpecifier is to reduce the number of constructors
// in PagedImage. It makes it possible to specify if no mask, the default
// mask, or another mask should be used when opening an existing PagedImage
// object.
// <p>
// Because the constructors automatically converts from a Bool or
// a String, the user does not need to be aware of MaskSpecifier.
// </synopsis> 

// <motivation>
// The number of constructors in PagedImage would be many more
// without this class. It would need one taking a Bool and a String.
// Because C++ converts a const char* to Bool instead of String,
// a const char* would also be needed multiple times.
// </motivation>

//# <todo asof="1997/11/11">
//# <li>
//# </todo>


class MaskSpecifier
{
public:
    // Default constructor.
    // It tells if the default mask should or no mask be used.
    MaskSpecifier (Bool useDefaultMask = True)
      : itsFlag(useDefaultMask) {}

    // Construct from a string.
    // It tells to use an alternative mask. An empty name means no mask.
    //# Note the const Char* constructor is needed, otherwise "name"
    //# is converted to a Bool by the compiler.
    // <group>
    MaskSpecifier (const Char* maskName)
      : itsFlag(False), itsName(maskName) {}
    MaskSpecifier (const String& maskName)
      : itsFlag(False), itsName(maskName) {}
    // </group>

    // Give the flag or name.
    // <group>
    Bool useDefault() const
      { return itsFlag; }
    const String& name() const
      { return itsName; }
    // </group>
  

private:
    Bool   itsFlag;
    String itsName;
};



} //# NAMESPACE CASACORE - END

#endif
