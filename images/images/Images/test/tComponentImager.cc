//# tComponentImager.cc: Test program for the ComponentImager class
//# Copyright (C) 1999,2000,2001
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

#include <casa/aips.h>
#include <images/Images/ComponentImager.h>
#include <components/ComponentModels/ComponentList.h>
#include <components/ComponentModels/ComponentType.h>
#include <components/ComponentModels/ConstantSpectrum.h>
#include <components/ComponentModels/Flux.h>
#include <components/ComponentModels/PointShape.h>
#include <components/ComponentModels/SkyComponent.h>
#include <components/ComponentModels/SpectralIndex.h>
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <coordinates/Coordinates/CoordinateUtil.h>
#include <coordinates/Coordinates/DirectionCoordinate.h>
#include <coordinates/Coordinates/SpectralCoordinate.h>
#include <images/Images/PagedImage.h>
//#include <lattices/Lattices/TiledShape.h>
#include <casa/Arrays/Array.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/Vector.h>
#include <casa/Exceptions/Error.h>
#include <casa/Arrays/IPosition.h>
#include <casa/BasicSL/Constants.h>
#include <casa/BasicMath/Math.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MFrequency.h>
#include <casa/OS/File.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Quanta/Unit.h>
#include <tables/Tables/Table.h>
#include <casa/Utilities/Assert.h>
#include <casa/BasicSL/String.h>
#include <casa/iostream.h>
// #include <casa/BasicSL/Complex.h>
// #include <casa/BasicMath/Math.h>

#include <casa/namespace.h>
int main() {
  try {
    const uInt nx = 4;
    const uInt ny = nx;
    const uInt nFreq = 3;

    ComponentList clist;
    {
      const MDirection ra0dec0(Quantity(0, "deg"), Quantity(0, "deg"),
			       MDirection::J2000);
      SkyComponent c1(Flux<Double>(1.0), 
		      PointShape(ra0dec0), ConstantSpectrum());
      clist.add(c1);
    }
    
    CoordinateSystem coords2D;
    {
      CoordinateUtil::addDirAxes(coords2D);
      DirectionCoordinate dirCoord = coords2D.directionCoordinate(0);
      {
	Vector<Double> refPix(2);
	refPix(0) = nx/2;
	refPix(1) = ny/2;
	dirCoord.setReferencePixel(refPix);
      }
      dirCoord.setWorldAxisUnits(Vector<String>(2, "deg"));
      dirCoord.setIncrement(Vector<Double>(2, 1.0));
      coords2D.replaceCoordinate(dirCoord, 0);
    }
    {
      PagedImage<Float> 
	image2D((TiledShape(IPosition(2,nx,ny))), coords2D,
		File::newUniqueName("./", "tComponentImager_tmp_")
		.absoluteName());
      image2D.setUnits(Unit("Jy/pixel"));
      image2D.set(0.0f);
      ComponentImager::project(image2D, clist);
      AlwaysAssert(near(image2D.getAt(IPosition(2,nx/2,ny/2)), 1.0f),
		   AipsError);
      image2D.putAt(0.0f, IPosition(2,nx/2,ny/2));
      AlwaysAssert(allNear(image2D.get(), 0.0f, C::flt_min), AipsError);
      image2D.table().markForDelete();
    }
    cerr << "Passed the 2-D Image test" << endl;
    CoordinateSystem coords3D;
    {
      CoordinateUtil::addFreqAxis(coords3D);
      coords3D.addCoordinate(coords2D.coordinate(0));
      {
	SpectralCoordinate specCoord = coords3D.spectralCoordinate(0);
	specCoord.setWorldAxisUnits(Vector<String>(1, "GHz"));
	specCoord.setIncrement(Vector<Double>(1, 1.0));
	specCoord.setReferencePixel(Vector<Double>(1, nFreq/2));
	specCoord.setReferenceValue(Vector<Double>(1, 2.0));
	coords3D.replaceCoordinate(specCoord, 0);
      }
      PagedImage<Float> 
	image3D((TiledShape(IPosition(3, nFreq, nx, ny))),
		coords3D,
		File::newUniqueName("./", "tComponentImager_tmp_")
		.absoluteName());
      image3D.setUnits(Unit("WU/pixel"));
      const MDirection ra1dec2(Quantity(1, "deg"), Quantity(-2, "deg"),
			       MDirection::J2000);
      const MFrequency oneGhz(Quantity(1, "GHz"), MFrequency::LSRK);
      SkyComponent c2(Flux<Double>(0.5), 
		      PointShape(ra1dec2), 
		      SpectralIndex(oneGhz, 1.0));
      clist.add(c2);
      ComponentImager::project(image3D, clist);
      AlwaysAssert(near(image3D.getAt(IPosition(3, 0, nx/2,ny/2)), 200.0f),
		   AipsError);
      AlwaysAssert(near(image3D.getAt(IPosition(3, 1, nx/2,ny/2)), 200.0f),
		   AipsError);
      AlwaysAssert(near(image3D.getAt(IPosition(3, 2, nx/2,ny/2)), 200.0f),
		   AipsError);
      AlwaysAssert(near(image3D.getAt(IPosition(3, 0, nx/2+1,ny/2-2)), 100.f),
		   AipsError);
      AlwaysAssert(near(image3D.getAt(IPosition(3, 1, nx/2+1,ny/2-2)), 200.0f),
		   AipsError);
      AlwaysAssert(near(image3D.getAt(IPosition(3, 2, nx/2+1,ny/2-2)), 300.0f),
		   AipsError);
      image3D.table().markForDelete();
    }
    cerr << "Passed the 3-D Image test" << endl;
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
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 tComponentImager"
// End: 
