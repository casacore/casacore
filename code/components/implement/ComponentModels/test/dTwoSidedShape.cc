//# dGaussianCompRep.cc:  this defines dGaussianCompRep
//# Copyright (C) 1997,1998
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

#include <aips/aips.h>
#include <trial/ComponentModels/Flux.h>
#include <trial/ComponentModels/GaussianCompRep.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Coordinates/Projection.h>
#include <trial/Images/PagedImage.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Measures/MDirection.h>
#include <aips/Quanta/MVAngle.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Utilities/String.h>
#include <iostream.h>

int main() {
  try {
    Flux<Double> J0533_flux(2.23, 0.0, 0.0, 0.0);
    const Quantity J0533_ra = Quantity(5.0, "h").get("'") + Quantity(33, "'");
    const Quantity J0533_dec = Quantity(-32, "deg") + Quantity(-18, "'");
    const MDirection J0533_pos(J0533_ra, J0533_dec, MDirection::J2000);
    const MVAngle J0533_majAxis = Quantity(1, "deg");
    const MVAngle J0533_minAxis = Quantity(.2, "deg");
    const MVAngle J0533_pa(Quantity(63, "deg"));
    const GaussianCompRep J0533(J0533_flux, J0533_pos, 
				J0533_majAxis, J0533_minAxis, J0533_pa); 
    // This component can now be projected onto an image
    CoordinateSystem coords;
    {
      const Double pixInc = Quantity(1, "'").getValue("rad");
      Matrix<Double> xform(2,2);
      xform = 0.0; xform.diagonal() = 1.0;
      const Double refPixel = 32.0;
      const DirectionCoordinate dirCoord(MDirection::J2000,
					 Projection(Projection::SIN),
					 J0533_ra.getValue("rad"),
					 J0533_dec.getValue("rad"),
					 pixInc , pixInc, xform,
					 refPixel, refPixel);
      coords.addCoordinate(dirCoord);
    }
    CoordinateUtil::addFreqAxis(coords);
    PagedImage<Float> skyModel(IPosition(3,64,64,16), coords,
			       "dGaussianCompRep_tmp.image");
    skyModel.set(0.0f);
    J0533.project(skyModel);
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    return 1;
  } end_try;
  return 0;
}
// Local Variables:
// compile-command: "gmake OPTLIB=1 dGaussianCompRep"
// End:
