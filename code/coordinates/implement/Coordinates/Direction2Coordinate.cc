//# <ClassFileName.h>: this defines <ClassName>, which ...
//# Copyright (C) 1997,1999,2000
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

#include <trial/Coordinates/DirectionCoordinate.h>
#include <aips/Arrays/Vector.h>
#include <aips/Measures/MDirection.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/Unit.h>

// A different file so that apps which don't need measures don't link them all
// in (measures bring in tables and lots of other stuff)

Bool DirectionCoordinate::toWorld(MDirection &world, 
				  const Vector<Double> &pixel) const
{
    Bool ok = toWorld(world_tmp_p, pixel);

// We could cache much of this for efficiency

    Quantum<Double> longi(world_tmp_p(0), units_p(0));
    Quantum<Double> lati(world_tmp_p(1), units_p(1));
    world = MDirection(longi, lati, type_p);
//
    return ok;
}

Bool DirectionCoordinate::toPixel(Vector<Double> &pixel,
                                  const MDirection &world) const
{
// Convert to current units

    Quantum<Vector<Double> > lonlat = world.getAngle();
    Vector<Double> lonlatVal(2);

// Don't think this can be done more easily

    lonlat.convert(units_p(0));
    lonlatVal(0) = lonlat.getValue()(0);
//
    lonlat.convert(units_p(1));
    lonlatVal(1) = lonlat.getValue()(1);

// Do it

    if (pixel.nelements()!=2) pixel.resize(2,False);
    return toPixel(pixel, lonlatVal);
}


