//# LELCoordinates.h: Envelope class for Lattice coordinates in LEL
//# Copyright (C) 1998,1999,2000
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

#if !defined(AIPS_LELCOORDINATES_H)
#define AIPS_LELCOORDINATES_H


//# Includes
#include <aips/aips.h>
#include <aips/Lattices/LELLattCoordBase.h>
#include <aips/Utilities/CountedPtr.h>


// <summary>
// Envelope class to handle Lattice Coordinates in LEL.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Lattice">Lattice</linkto>
//   <li> <linkto class="LELLattCoordBase">LELLattCoordBase</linkto>
// </prerequisite>
//
// <synopsis> 
//  The LatticeExpression classes (LatticeExpr, LatticeExprNode, LEL*)
//  exist so that the C++ programmer can manipulate  mathematical 
//  expressions involving Lattices.  A further usage of these classes 
//  is to manipulate ImageInterface objects (which inherit from Lattice) such 
//  as PagedImages.   These objects have Coordinates as well as the Lattice 
//  pixels.  In order that Coordinate conformance be enforcable, we must 
//  give the LatticeExpression classes access to the Coordinates of the 
//  ImageInterface objects.
//
//  This is done through the interface of the LELCoordinates class.
//  It is actually an envelope class which holds letter classes which
//  are the actual implementation of the objects which hold the Lattice
//  CoordinateSystems.
//  Lattice objects have a member function called lelCoordinates.
//  This returns a LELCoordinates object.   This object contains a
//  pointer (actually a CountedPtr) of type LELLattCoordBase.  This is the base
//  class of the letter classes.   For Lattices such as ImageInterface,
//  this pointer actually points at the derived letter class LELImageCoord.
//  This class in turn contains a pointer (a CountedPtr) to the actual
//  CoordinateSystem object.   
// 
//  Note that every time the lelCoordinates function is called, the
//  LELLattCoord and LELImageCoord (or whatever the letter class
//  actually being invoked is)  objects are constructed.  For example
//  the internals of ImageInterface::lelCoordinates are
//  <src>return LELCoordinates (new LELImageCoord (coords_p));</src>
//  so that the LELCoordinates constructor invokes the LELImageCoord
//  constructor with the CoordinateSystem as its argument.  However,
//  the internal use of CountedPtrs make subsequent constructions inexpensive.
//
//  Having a LELCoordinates object in hand, the programmer then has access
//  to the CoordinateSystem that it ultimately contains.  This is via the
//  LELCoordinates member function coordinates which returns a reference to
//  the letter base class LELLattCoordBase. For example, if the actual letter
//  class object was LELImageCoord, one has to then cast the reference to
//  the LELLattCoordBase returned
//  by LELCoordinates::coordinates() to an LELImageCoord.  This is because
//  the letter class functions that actually return the CoordinateSystem
//  are not virtual (i.e. they are not defined in LELLattCoordBase).
// </synopsis>
//
// <example>
// <srcblock>
//    PagedImage<Float> im("myimage");
//    LELCoordinates* pLatCoord = &(im.lelCoordinates());
//    LELImageCoord* pImCoord = (LELImageCoord*)pLatCoord;
//    CoordinateSystem coords = pImCoord->coordinates();
// </srcblock>
// </example>
//
// <motivation>
//   We needed access to CoordinateSystems in the Lattice Expression classes
// </motivation>
//
// <todo asof="1995/09/12">
//  <li>
// </todo>


class LELCoordinates
{
public:
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

    // Does the class have true coordinates?
    // It returns False if this is a null object.
    Bool hasCoordinates() const;

    // Check if the coordinates of this and that conform.
    // It returns False if this or that is a null object.
    Bool conform (const LELCoordinates& that) const;

    // Return the underlying letter object.
    // This should in general not be used, but for specific (Image) cases
    // it might be needed.
    const LELLattCoordBase& coordinates() const;

private:
    // The pointer to the underlying object.
    CountedPtr<LELLattCoordBase> coords_p;
};


#endif
