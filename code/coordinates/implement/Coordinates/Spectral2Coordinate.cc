//# Spectral2Coordinate.cc: this defines Measures related SpectralCoordinate functions
//# Copyright (C) 1997,1998,1999
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
#include <aips/Quanta/Quantum.h>
#include <aips/Utilities/String.h>


Bool SpectralCoordinate::toWorld(MFrequency& world, 
				 Double pixel) const
{
    if (pixel_tmp_p.nelements()!=1) pixel_tmp_p.resize(1);
    pixel_tmp_p(0) = pixel;
    Bool ok = toWorld(world_tmp_p, pixel_tmp_p);
    if (ok) {
       Unit units = worldAxisUnits()(0);
       world = MFrequency(Quantity(world_tmp_p(0), units), type_p);
    }
    return ok;
}

Bool SpectralCoordinate::toPixel(Double& pixel,
                                 const MFrequency& world) const
{
// Convert to current units

    const Vector<String>& units = worldAxisUnits();
    Quantum<Double> value = world.get(Unit(units(0)));
    if (world_tmp_p.nelements()!=1) world_tmp_p.resize(1);
    world_tmp_p(0) = value.getValue();

// Convert to pixel

    Bool ok = toPixel(pixel_tmp_p, world_tmp_p);
    if (ok) {
       pixel = pixel_tmp_p(0);
    }
    return ok;
}
 
 

