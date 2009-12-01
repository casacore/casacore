//# Direction2Coordinate.cc: this defines measures related DirectionCoordinate code
//# Copyright (C) 1997,1999,2000,2001,2002,2003
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


#include <coordinates/Coordinates/DirectionCoordinate.h>
#include <casa/Arrays/Vector.h>
#include <casa/BasicSL/Constants.h>
#include <measures/Measures/MDirection.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Quanta/MVDirection.h>
#include <casa/Quanta/Unit.h>

// A different file so that apps which don't need measures don't link them all
// in (measures bring in tables and lots of other stuff)

namespace casa { //# NAMESPACE CASA - BEGIN


Bool DirectionCoordinate::toWorld(MDirection &world, 
				  const Vector<Double> &pixel) const
{
    static MVDirection world_tmp;
    if (toWorld(world_tmp, pixel)) {
       world.set(world_tmp, MDirection::Ref(type_p));
       return True;
    }
//
    return False;
}

Bool DirectionCoordinate::toWorld(MVDirection &world, 
				  const Vector<Double> &pixel) const
{
    static Vector<Double> world_tmp(2);
    if (toWorld(world_tmp, pixel)) {
       world.setAngle(world_tmp(0)*to_radians_p[0],
                      world_tmp(1)*to_radians_p[1]);
       return True;
    }
    return False;
}

Bool DirectionCoordinate::toPixel(Vector<Double> &pixel,
                                  const MDirection &world) const
{
    return toPixel(pixel, world.getValue());
}


Bool DirectionCoordinate::toPixel(Vector<Double> &pixel,
                                  const MVDirection &world) const
{
   static Vector<Double> world_tmp(2);

// Convert to current units

   world_tmp(0) = world.getLong() / to_radians_p[0]; 
   world_tmp(1) = world.getLat()  / to_radians_p[1];
//
   return toPixel(pixel, world_tmp);
}

} //# NAMESPACE CASA - END

