//# dTwoSidedShape.cc:
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

#include <aips/aips.h>
#include <trial/ComponentModels/Flux.h>
#include <trial/ComponentModels/GaussianShape.h>
#include <trial/ComponentModels/DiskShape.h>
#include <trial/ComponentModels/SkyComponent.h>
#include <trial/ComponentModels/ConstantSpectrum.h>
#include <trial/ComponentModels/ComponentType.h>
#include <trial/ComponentModels/TwoSidedShape.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MeasFrame.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/MVAngle.h>
#include <aips/Quanta/MVTime.h>
#include <aips/Utilities/String.h>
#include <iostream.h>

void printShape(const TwoSidedShape& theShape) {
  cout << "This is a " << ComponentType::name(theShape.type())
       << " shape " << endl 
       << "with a reference direction of "
       << theShape.refDirection().getAngle("deg") << " ("
       << theShape.refDirection().getRefString() << ")" << endl
       << "and a major axis of " << theShape.majorAxis() << endl
       << "      minor axis of " << theShape.minorAxis() << endl
       << "and   position angle of " << theShape.positionAngle() << endl;
}

int main() {
  try {
    { // Construct a Gaussian shape
      MDirection blob_dir;
      { // get the right direction into blob_dir
	Quantity blob_ra; MVAngle::read(blob_ra, "19:39:");
	Quantity blob_dec; MVAngle::read(blob_dec, "-63.43.");
	blob_dir = MDirection(blob_ra, blob_dec, MDirection::J2000);
      }
      {
	const Flux<Double> flux(6.28, 0.1, 0.15, 0.01);
	const GaussianShape shape(blob_dir,
				  Quantity(30, "arcmin"), 
				  Quantity(2000, "mas"), 
				  Quantity(C::pi_2, "rad"));
	const ConstantSpectrum spectrum;
	SkyComponent component(flux, shape, spectrum);
	printShape(shape);
      }
    }
    { // construct a model for Jupiter.
      Quantity clk_time; MVTime::read(clk_time, "01-10-2000/12:59");
      MEpoch obs_epoch(clk_time, MEpoch::UTC);
      MeasFrame obs_frame(obs_epoch);
      MDirection jupiter_dir(MVDirection(0), 
			     MDirection::Ref(MDirection::JUPITER, obs_frame));
      DiskShape jupiter_shape;
      jupiter_shape.setRefDirection(jupiter_dir);
      jupiter_shape.setWidth(Quantity(4,"arcmin"), Quantity(3.9,"arcmin"),
			     Quantity(3, "deg"));
      printShape(jupiter_shape);
      MDirection sample_dir(MVDirection(1.218, 0.37), MDirection::J2000);
      if (jupiter_shape.sample(sample_dir, MVAngle(0.1), MVAngle(0.1)) > 0.0) {
	cout << "The position in J2000 coordinates is near: " 
	     << sample_dir.getAngle("deg") << endl;
      }
    }
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  }
  catch (...) {
    cerr << "Exception not derived from AipsError" << endl;
    cout << "FAIL" << endl;
    return 2;
  }
  cout << "OK" << endl;
  return 0;
}
// Local Variables:
// compile-command: "gmake OPTLIB=1 dTwoSidedShape"
// End:
