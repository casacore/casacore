//# Spectral2Coordinate.cc: this defines Measures related SpectralCoordinate functions
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
//#
//# $Id$

#include <trial/Coordinates/SpectralCoordinate.h>
#include <aips/Arrays/Vector.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Quanta/MVFrequency.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Utilities/String.h>


Bool SpectralCoordinate::toWorld(MFrequency& world, 
				 Double pixel) const
{
    static MVFrequency world_tmp;
    Bool ok = toWorld(world_tmp, pixel);
    if (ok) {
       world.set(world_tmp, type_p);
    }
    return ok;
}

Bool SpectralCoordinate::toWorld(MVFrequency& world, 
				 Double pixel) const
{
    static Double world_tmp;
    static Quantum<Double> q_tmp;
//
    Bool ok = toWorld(world_tmp, pixel);
    if (ok) {
       const Unit& units = Unit(worldAxisUnits()(0));
       q_tmp.setValue(world_tmp);
       q_tmp.setUnit(units);
       world = MVFrequency(q_tmp);
    }
    return ok;
}

Bool SpectralCoordinate::toPixel(Double& pixel,
                                 const MFrequency& world) const
{
    return toPixel(pixel, world.getValue());
}
 
Bool SpectralCoordinate::toPixel(Double& pixel,
                                 const MVFrequency& world) const
{
   static Double world_tmp;
   const Unit& units = Unit(worldAxisUnits()(0));
   world_tmp = world.get(units).getValue();
   return toPixel(pixel, world_tmp);
}
 
 

