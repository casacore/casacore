//# LCRegionFixed.cc: Abstract base class to define a fixed region
//# Copyright (C) 1998
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


#include <casacore/lattices/LRegions/LCRegionFixed.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LCRegionFixed::LCRegionFixed()
{}

LCRegionFixed::LCRegionFixed (const IPosition& latticeShape)
: LCRegionSingle (latticeShape)
{}

LCRegionFixed::LCRegionFixed (const LCRegionFixed& other)
: LCRegionSingle (other),
  itsMask        (other.itsMask)
{
    setMaskPtr (itsMask);
}

LCRegionFixed::~LCRegionFixed()
{}

LCRegionFixed& LCRegionFixed::operator= (const LCRegionFixed& other)
{
    if (this != &other) {
        LCRegionSingle::operator= (other);
	itsMask = other.itsMask;
	setMaskPtr (itsMask);
    }
    return *this;
}

Bool LCRegionFixed::operator== (const LCRegion& other) const
{
   return LCRegion::operator== (other);
}

void LCRegionFixed::setMask (const Array<Bool>& mask)
{
    itsMask = mask;
    setMaskPtr (itsMask);
}

} //# NAMESPACE CASACORE - END

