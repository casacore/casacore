//# LELLattCoordBase.h: The base letter class for lattice coordinates in LEL
//# Copyright (C) 1998,1999,2000,2001
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

#ifndef LATTICES_LELLATTCOORDBASE_H
#define LATTICES_LELLATTCOORDBASE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class LELImageCoord;
class IPosition;
template<class T> class Vector;


// <summary>
// The base letter class for lattice coordinates in LEL.
// </summary>

// <use visibility=local>

// <reviewed reviewer="Bob Garwood" date="2000/01/25" tests="tLatticeExpr">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
//   <li> <linkto class="LELCoordinates"> LELCoordinates</linkto>
// </prerequisite>

// <synopsis>
// This abstract base class is the basic letter for the envelope class
// <linkto class=LELCoordinates>LELCoordinates</linkto>.
// It does not do anything, but makes it possible that derived classes
// (like <linkto class=LELLattCoord>LELLattCoord</linkto> and
// <linkto class=LELImageCoord>LELImageCoord</linkto>)
// implement their own behaviour.
// </synopsis> 

// <motivation>
// It must be possible to handle image coordinates in a lattice
// expression.   
// </motivation>

//# <todo asof="1998/01/31">
//#  <li>
//# </todo>


class LELLattCoordBase
{
public:
    LELLattCoordBase()
      {};

    // A virtual destructor is needed so that it will use the actual
    // destructor in the derived class.
    virtual ~LELLattCoordBase();

    // Does the class have true coordinates?
    virtual Bool hasCoordinates() const = 0;

    // The name of the class.
    virtual String classname() const = 0;

    // Get the coordinates of the spectral axis for the given shape.
    // It returns the pixel axis number of the spectral coordinates.
    // -1 indicates that there is no pixel spectral axis.
    // An exception is thrown if there are no world spectral coordinates.
    virtual uInt getSpectralInfo (Vector<Double>& worldCoordinates,
				  const IPosition& shape) const = 0;

    // Check how the coordinates of this and that compare.
    virtual Int compare (const LELLattCoordBase& other) const = 0;

    // Check how the coordinates of this and that image compare.
    // This function is used by <src>conform</src> to make a
    // double virtual dispatch possible.
    virtual Int doCompare (const LELImageCoord& other) const = 0;
};



} //# NAMESPACE CASACORE - END

#endif

