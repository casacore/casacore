//# LELAttribute.cc: Ancillary information for the LEL letter classes
//# Copyright (C) 1997,1998,1999,2000
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


#include <trial/Lattices/LELAttribute.h>
#include <trial/Lattices/LELLattCoord.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h> 


LELAttribute::LELAttribute()
: isScalar_p (True),
  isRegion_p (False),
  isMasked_p (False),
  coords_p   (new LELLattCoord())
{}

LELAttribute::LELAttribute (Bool isMasked,
			    const IPosition& shape,
			    const IPosition& tileShape,
			    const LELCoordinates& coordinates)
: isScalar_p  (False),
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
  isRegion_p  (True),
  isMasked_p  (False),
  shape_p     (IPosition(regionNdim, 0)),
  coords_p    (new LELLattCoord())
{}

LELAttribute::LELAttribute (const LELAttribute& other)
: isScalar_p  (other.isScalar_p),
  isRegion_p  (other.isRegion_p),
  isMasked_p  (other.isMasked_p),
  shape_p     (other.shape_p),
  tileShape_p (other.tileShape_p),
  coords_p    (other.coords_p)
{}

LELAttribute::LELAttribute (const LELAttribute& leftAttr,
			    const LELAttribute& rightAttr)
{
   isScalar_p = False;
   isRegion_p = False;
   isMasked_p = ToBool (leftAttr.isMasked() || rightAttr.isMasked());
   if (leftAttr.isRegion()  ||  rightAttr.isRegion()) {
      throw (AipsError ("LELAttribute: regions cannot be combined here"));
   }
   if (leftAttr.isScalar()) {
      if (rightAttr.isScalar()) {
         isScalar_p = True;
	 isMasked_p = False;
      } else {
         shape_p     = rightAttr.shape();
	 tileShape_p = rightAttr.tileShape();
	 coords_p    = rightAttr.coordinates();
      }
   } else {
      shape_p     = leftAttr.shape();
      tileShape_p = leftAttr.tileShape();
      coords_p    = leftAttr.coordinates();
      if (!rightAttr.isScalar()) {
         AlwaysAssert (leftAttr.shape().isEqual(rightAttr.shape()), AipsError);
	 if (rightAttr.coordinates().hasCoordinates()) {
	    if (coords_p.hasCoordinates()) {
		AlwaysAssert (leftAttr.coordinates().conform
			      (rightAttr.coordinates()), AipsError);
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
