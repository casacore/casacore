//# LELAttribute.h: Ancillary information for the LEL letter classes
//# Copyright (C) 1997,1998,1999,2000,2001,2003
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

#ifndef LATTICES_LELATTRIBUTE_H
#define LATTICES_LELATTRIBUTE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/lattices/LEL/LELCoordinates.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Ancillary information for the LEL letter classes.
// </summary>
//
// <use visibility=local>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
//   <li> <linkto class="LatticeExpr"> LatticeExpr</linkto>
//   <li> <linkto class="LatticeExprNode"> LatticeExprNode</linkto>
//   <li> <linkto class="LELInterface"> LELInterface</linkto> and
//        derived classes
// </prerequisite>
//
// <etymology>
// Holds attribute information for the Lattice Expression 
// Language letter classes.
// </etymology>

// <synopsis>
// The Lattice Expression Language letter classes provide
// expression objects. There is ancilliary information or 
// attributes associated with these objects:
// <ul>
// <li> Scalar or lattice (i.e. array) or region.
// <li> In case of an array, is it a reduced array. I.e. is it an array
//      that has to be calculated beforehand (e.g. partialMax).
//      A scalar is always reduced.
// <li> Shape and tile shape of a lattice. This can be undefined.
// <li> Is the lattice masked?
// <li> Optionally coordinates of the lattice.
// </ul>
// Two attribute objects can be combined mirroring the combination of two
// expressions (like the addition of two lattices).
// Regions cannot be combined.
// </synopsis> 


class LELAttribute
{
public:
// Default constructor sets it as a scalar.
   LELAttribute();

// Constructor sets it as lattice with given attributes.
// An empty shape indicates that the shape is not known.
   LELAttribute(Bool isMasked,
		const IPosition& shape,
		const IPosition& tileShape,
		const LELCoordinates& coordinates,
		Bool isReduced = False);

// Constructor sets it as a region with given attributes.
   explicit LELAttribute(uInt regionNdim);

// Copy constructor (copy semantics)
   LELAttribute(const LELAttribute& attr);

// Constructor that combines the two attributes given.
// An array can be combined with a scalar.
// If matchAxes is True and if two arrays are given, the shapes and
// coordinates have to match exactly, otherwise one can be a subset of
// the other (and LEL will auto-extend).
   LELAttribute(const LELAttribute& attrLeft,
		const LELAttribute& attrRight,
		Bool matchAxes = True);

// Destructor
   ~LELAttribute();

// Assignment (copy semantics)
   LELAttribute& operator= (const LELAttribute& other);

// Is expression a scalar?
   Bool isScalar() const { return isScalar_p; }

// Is expression a reduced array? A scalar is always reduced.
   Bool isReduced() const { return isReduced_p; }

// Is expression a region?
   Bool isRegion() const { return isRegion_p; }

// Is the expression result masked?
   Bool isMasked() const { return isMasked_p; }

// What is the shape of the expression?
   const IPosition& shape() const { return shape_p; }

// What is the tile shape of the expression?
   const IPosition& tileShape() const { return tileShape_p; }

// What are the coordinates of the expression?
   const LELCoordinates& coordinates() const { return coords_p; }

// Compare the coordinates and shapes to see if this is a subset of other.
   Int compareCoord (const LELAttribute& other) const;

private:
   Bool      isScalar_p;
   Bool      isReduced_p;
   Bool      isRegion_p;
   Bool      isMasked_p;
   IPosition shape_p;
   IPosition tileShape_p;
   LELCoordinates coords_p;
};



} //# NAMESPACE CASACORE - END

#endif
