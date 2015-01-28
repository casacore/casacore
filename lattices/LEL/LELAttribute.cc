//# LELAttribute.cc: Ancillary information for the LEL letter classes
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


#include <casacore/lattices/LEL/LELAttribute.h>
#include <casacore/lattices/LEL/LELLattCoord.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h> 


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LELAttribute::LELAttribute()
: isScalar_p  (True),
  isReduced_p (True),
  isRegion_p  (False),
  isMasked_p  (False),
  coords_p    (new LELLattCoord())
{}

LELAttribute::LELAttribute (Bool isMasked,
			    const IPosition& shape,
			    const IPosition& tileShape,
			    const LELCoordinates& coordinates,
			    Bool isReduced)
: isScalar_p  (False),
  isReduced_p (isReduced),
  isRegion_p  (False),
  isMasked_p  (isMasked),
  shape_p     (shape),
  tileShape_p (tileShape),
  coords_p    (coordinates)
{
  if (coords_p.isNull()) {
    coords_p = LELCoordinates (new LELLattCoord());
  }
}

LELAttribute::LELAttribute (uInt regionNdim)
: isScalar_p  (False),
  isReduced_p (False),
  isRegion_p  (True),
  isMasked_p  (False),
  shape_p     (IPosition(regionNdim, 0)),
  coords_p    (new LELLattCoord())
{}

LELAttribute::LELAttribute (const LELAttribute& other)
: isScalar_p  (other.isScalar_p),
  isReduced_p (other.isReduced_p),
  isRegion_p  (other.isRegion_p),
  isMasked_p  (other.isMasked_p),
  shape_p     (other.shape_p),
  tileShape_p (other.tileShape_p),
  coords_p    (other.coords_p)
{}

LELAttribute::LELAttribute (const LELAttribute& leftAttr,
			    const LELAttribute& rightAttr,
			    Bool matchAxes)
{
  isScalar_p = False;
  isRegion_p = False;
  isMasked_p = (leftAttr.isMasked() || rightAttr.isMasked());
  if (leftAttr.isRegion()  ||  rightAttr.isRegion()) {
    throw (AipsError ("LELAttribute: regions cannot be combined here"));
  }
  if (leftAttr.isScalar()) {
    if (rightAttr.isScalar()) {
      isScalar_p  = True;
      isReduced_p = True;
      isMasked_p  = False;
    } else {
      isReduced_p = rightAttr.isReduced();
      shape_p     = rightAttr.shape();
      tileShape_p = rightAttr.tileShape();
      coords_p    = rightAttr.coordinates();
    }
  } else {
    isReduced_p = leftAttr.isReduced();
    shape_p     = leftAttr.shape();
    tileShape_p = leftAttr.tileShape();
    coords_p    = leftAttr.coordinates();
    if (!rightAttr.isScalar()) {
      // Two arrays are combined.
      // The result is reduced if one of them is reduced.
      if (rightAttr.isReduced()) {
	isReduced_p = True;
      }
      // Check shapes if both are defined.
      const IPosition& rShape = rightAttr.shape();
      Bool ok = False;
      if (shape_p.nelements() == 0) {
	shape_p = rShape;
	ok = True;
      } else if (rShape.nelements() == 0) {
	ok = True;
      }       
      if (!ok  &&  matchAxes) {
	ok = shape_p.isEqual (rShape);
      } else if (shape_p.nelements() > rShape.nelements()) {
	ok = shape_p.isSubSet (rShape);
      } else {
	ok = rShape.isSubSet (shape_p);
	shape_p.resize(0);
	shape_p = rShape;
	tileShape_p.resize(0);
	tileShape_p = rightAttr.tileShape();
      }
      if (!ok) {
	throw AipsError ("LELAttribute: "
			 "shapes of operands mismatch");
      }
      if (rightAttr.coordinates().hasCoordinates()) {
	if (coords_p.hasCoordinates()) {
	  Int result = leftAttr.coordinates().compare
                                                 (rightAttr.coordinates());
	  if (matchAxes) {
	    if (result != 0) {
	      throw AipsError ("LELAttribute: "
			       "coordinates of operands mismatch");
	    }
	  } else {
	    if (result == -1) {
	      // left is subset, so use coordinates of other operand
	      coords_p = rightAttr.coordinates();
	    } else if (result > 1) {
	      throw AipsError ("LELAttribute: "
			       "coordinates of operands incompatible");
	    }
	  }
	} else {
	  coords_p = rightAttr.coordinates();
	}
      } 
    }
  }
}
                
LELAttribute::~LELAttribute()
{}


LELAttribute& LELAttribute::operator= (const LELAttribute& other)
{
  if (this != &other) {
    isScalar_p  = other.isScalar_p;
    isReduced_p = other.isReduced_p;
    isRegion_p  = other.isRegion_p;
    isMasked_p  = other.isMasked_p;
    shape_p.resize (other.shape_p.nelements());
    tileShape_p.resize (other.tileShape_p.nelements());
    shape_p     = other.shape_p;
    tileShape_p = other.tileShape_p;
    coords_p    = other.coords_p;
  }
  return *this;
}

Int LELAttribute::compareCoord (const LELAttribute& other) const
{
  // Both scalars is always equal.
  if (isScalar()  ||  other.isScalar()) {
    return 0;
  }
  // Compare coordinates; exit if not equal.
  Int result = coordinates().compare (other.coordinates());
  if (result != 0) {
    return result;
  }
  // The coordinates are equal, check if shapes are also equal.
  // A length can be 1, thus be a subset of the other.
  const IPosition& thisShape = shape();
  const IPosition& thatShape = other.shape();
  if (thisShape.nelements() != thatShape.nelements()) {
    return 8;
  }
  for (uInt i=0; i<thisShape.nelements(); i++) {
    if (thisShape(i) != thatShape(i)) {
      if (thisShape(i) == 1  &&  thatShape(i) > 1) {
	// This is subset of that; check if not already the other way.
	if (result == 1) {
	  return 8;
	}
	result = -1;
      } else if (thisShape(i) > 1  &&  thatShape(i) == 1) {
	// That is subset of this; check if not already the other way.
	if (result == -1) {
	  return 8;
	}
	result = 1;
      } else {
	// Mismatching shapes
	return 8;
      }
    }
  }
  return result;
}

} //# NAMESPACE CASACORE - END

