//# LELCoordinates.h: Envelope class for Lattice coordinates in LEL
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

#ifndef LATTICES_LELCOORDINATES_H
#define LATTICES_LELCOORDINATES_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/CountedPtr.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class LELLattCoordBase;


// <summary>
// Envelope class to handle Lattice Coordinates in LEL.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="2000/01/25" tests="tLatticeExpr">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice">Lattice</linkto>
//   <li> <linkto class="LELLattCoordBase">LELLattCoordBase</linkto>
// </prerequisite>

// <synopsis> 
//  The LatticeExpression classes (LatticeExpr, LatticeExprNode, LEL*)
//  exist so that the C++ programmer can manipulate mathematical 
//  expressions involving Lattices.  A further usage of these classes 
//  is to manipulate ImageInterface objects (which inherit from Lattice) such 
//  as PagedImages.  These objects have Coordinates as well as the Lattice 
//  pixels.  In order that Coordinate conformance be enforcable, we must 
//  give the LatticeExpression classes access to the Coordinates of the 
//  ImageInterface objects.
//
//  This is done through the interface of the LELCoordinates class.
//  It is actually an envelope class which holds letter classes which
//  are the actual implementation of the objects which hold the Lattice
//  CoordinateSystems.
//  Lattice objects have a member function called <src>lelCoordinates</src>.
//  This returns a LELCoordinates object.  This object contains a
//  pointer (actually a CountedPtr) of type
//  <linkto class=LELLattCoordBase>LELLattCoordBase</linkto>.  This is the
//  base class of the letter classes.  For Lattices such as ImageInterface,
//  this pointer actually points at the derived letter class LELImageCoord.
//  This class in turn contains a pointer (a CountedPtr) to the actual
//  CoordinateSystem object.   
// 
//  Note that every time the <src>lelCoordinates</src> function is called,
//  the <linkto class=LELLattCoord>LELLattCoord</linkto>
//  and <linkto class=LELImageCoord>LELImageCoord</linkto>
//  (or whatever the letter class actually being invoked is)
//  objects are constructed.  For example
//  the internals of <src>ImageInterface::lelCoordinates</src> are
//  <br><src>return LELCoordinates (new LELImageCoord (coords_p));</src>
//  <br>so that the LELCoordinates constructor invokes the LELImageCoord
//  constructor with the CoordinateSystem as its argument.  However,
//  the internal use of CountedPtrs makes subsequent constructions inexpensive.
//
//  Having a LELCoordinates object in hand, the programmer then has access
//  to the CoordinateSystem that it ultimately contains.  This is via the
//  LELCoordinates member function <src>coordinates</src> which returns
//  a reference to the letter base class LELLattCoordBase.
//  For example, if the actual letter class object was LELImageCoord,
//  one has to then cast the reference returned by
//  <src>LELCoordinates::coordinates()</src> to an LELImageCoord.
//  This is because the LELImageCoord class functions that actually deal
//  with the CoordinateSystem are not virtual (otherwise LELLattCoordBase
//  needs to know about Coordinates).
// </synopsis>

// <example>
// <srcblock>
//    PagedImage<Float> im("myimage");
//    const LELCoordinates* pLatCoord = &(im.lelCoordinates());
//    const LELImageCoord* pImCoord =
//                  dynamic_cast<const LELImageCoord*>(pLatCoord);
//    CoordinateSystem coords = pImCoord->coordinates();
// </srcblock>
// </example>

// <motivation>
//  We needed access to CoordinateSystems in the Lattice Expression classes
//  without making the Lattices module dependent on the Images or Coordinates
//  module.
// </motivation>

//# <todo asof="1995/09/12">
//#  <li>
//# </todo>


class LELCoordinates
{
public:
    // Define the possible comparison results.
    // The default constructor creates a null object.
    LELCoordinates();

    // Construct the object from the given letter class.
    // It takes over the pointer and takes care of destructing
    // the LELLattCoordBase object.
    LELCoordinates (LELLattCoordBase* coordinates);

    // Copy constructor (reference semantics).
    LELCoordinates (const LELCoordinates& that);

    ~LELCoordinates();

    // Assignment (reference semantics).
    LELCoordinates& operator= (const LELCoordinates& that);

    // Is the coordinates a null object?
    Bool isNull() const
      { return coords_p.null(); }

    // Does the class have true coordinates?
    // It returns False if this is a null object.
    Bool hasCoordinates() const;

    // Check how the coordinates of this and that compare.
    // The return value tells how they compare.
    // <br>-1: this is subset
    // <br>0: equal 
    // <br>1: this is superset
    // <br>9: invalid (mismatch)
    Int compare (const LELCoordinates& other) const;

    // Return the underlying letter object.
    // This should in general not be used, but for specific (Image) cases
    // it might be needed.
    const LELLattCoordBase& coordinates() const;

private:
    // The pointer to the underlying object.
    CountedPtr<LELLattCoordBase> coords_p;
};



} //# NAMESPACE CASACORE - END

#endif
