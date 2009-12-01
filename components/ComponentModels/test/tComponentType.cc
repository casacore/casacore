//# ClassFileName.cc:  this defines ClassName, which ...
//# Copyright (C) 1997,1998,1999,2000,2002
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
#include <casa/Exceptions/Error.h>
#include <casa/Utilities/Assert.h>
#include <components/ComponentModels/ComponentType.h>
#include <components/ComponentModels/SkyCompRep.h>
#include <components/ComponentModels/ComponentShape.h>
#include <components/ComponentModels/SpectralModel.h>
#include <casa/iostream.h>

#include <casa/namespace.h>
int main() {
  try {
    {
      ComponentType::Shape e(ComponentType::POINT);
      AlwaysAssert(ComponentType::name(e) == "Point", AipsError);
      e = ComponentType::GAUSSIAN;
      AlwaysAssert(ComponentType::name(e) == "Gaussian", AipsError);
      e = ComponentType::UNKNOWN_SHAPE;
      AlwaysAssert(ComponentType::name(e) == "Unknown", AipsError);
      e = ComponentType::NUMBER_SHAPES;
      AlwaysAssert(ComponentType::name(e) == "Unknown", AipsError);
      String s("point");
      AlwaysAssert(ComponentType::shape(s) == ComponentType::POINT, 
		   AipsError);
      s = "GAUSSIAN";
      AlwaysAssert(ComponentType::shape(s) == ComponentType::GAUSSIAN,
		   AipsError);
      s = "Pointer";
      AlwaysAssert(ComponentType::shape(s) == ComponentType::UNKNOWN_SHAPE,
		   AipsError);
    }
    {
      ComponentType::Polarisation e(ComponentType::STOKES);
      AlwaysAssert(ComponentType::name(e) == "Stokes", AipsError);
      e = ComponentType::LINEAR;
      AlwaysAssert(ComponentType::name(e) == "Linear", AipsError);
      e = ComponentType::CIRCULAR;
      AlwaysAssert(ComponentType::name(e) == "Circular", AipsError);
      e = ComponentType::UNKNOWN_POLARISATION;
      AlwaysAssert(ComponentType::name(e) == "Unknown", AipsError);
      e = ComponentType::NUMBER_POLARISATIONS;
      AlwaysAssert(ComponentType::name(e) == "Unknown", AipsError);
      String s("stokes");
      AlwaysAssert(ComponentType::polarisation(s) == ComponentType::STOKES,
		   AipsError);
      s = "LINEAR";
      AlwaysAssert(ComponentType::polarisation(s) == ComponentType::LINEAR,
		   AipsError);
      s = "Circular";
      AlwaysAssert(ComponentType::polarisation(s) == ComponentType::CIRCULAR,
		   AipsError);
      s = "Pointer";
      AlwaysAssert(ComponentType::polarisation(s) ==
		   ComponentType::UNKNOWN_POLARISATION, AipsError);
    }
    {
      ComponentType::SpectralShape e(ComponentType::CONSTANT_SPECTRUM);
      AlwaysAssert(ComponentType::name(e) == "Constant", AipsError);
      e = ComponentType::SPECTRAL_INDEX;
      AlwaysAssert(ComponentType::name(e) == "Spectral Index", AipsError);
      e = ComponentType::UNKNOWN_SPECTRAL_SHAPE;
      AlwaysAssert(ComponentType::name(e) == "Unknown", AipsError);
      e = ComponentType::NUMBER_SPECTRAL_SHAPES;
      AlwaysAssert(ComponentType::name(e) == "Unknown", AipsError);
      String s("constant");
      AlwaysAssert(ComponentType::spectralShape(s) ==
		   ComponentType::CONSTANT_SPECTRUM, AipsError);
      s = "SPECTRAL INDEX";
      AlwaysAssert(ComponentType::spectralShape(s) == 
		   ComponentType::SPECTRAL_INDEX, AipsError);
      s = "Pointer";
      AlwaysAssert(ComponentType::spectralShape(s) ==
		   ComponentType::UNKNOWN_SPECTRAL_SHAPE, AipsError);
    }
    {
      SkyCompRep cp(ComponentType::POINT, ComponentType::CONSTANT_SPECTRUM);
      AlwaysAssert(cp.shape().type() == ComponentType::POINT, AipsError);
      AlwaysAssert(cp.spectrum().type() == ComponentType::CONSTANT_SPECTRUM,
		   AipsError);
    }
    {
      SkyCompRep g(ComponentType::GAUSSIAN);
      AlwaysAssert(g.shape().type() == ComponentType::GAUSSIAN, AipsError);
      AlwaysAssert(g.spectrum().type() == ComponentType::CONSTANT_SPECTRUM,
		   AipsError);
    }
    {
      SkyCompRep dsi(ComponentType::DISK, ComponentType::SPECTRAL_INDEX);
      AlwaysAssert(dsi.shape().type() == ComponentType::DISK, AipsError);
      AlwaysAssert(dsi.spectrum().type() == ComponentType::SPECTRAL_INDEX,
		   AipsError);
    }
    try {
      SkyCompRep u(ComponentType::UNKNOWN_SHAPE);
      throw(AipsError("Should not be able to make an unknown shape"));
    }
    catch (AipsError x) {
      if (!x.getMesg().contains("Failed AlwaysAssert ok()")) {
	throw;
      }
    }
    try {
      SkyCompRep u(ComponentType::POINT,
		   ComponentType::UNKNOWN_SPECTRAL_SHAPE);
      throw(AipsError("Should not be able to make an unknown spectrum"));
    }
    catch (AipsError x) {
      if (!x.getMesg().contains("Failed AlwaysAssert ok()")) {
	throw;
      }
    }
    cout << "Two SEVERE logger error messages are expected" << endl;
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
// compile-command: "gmake OPTLIB=1 tComponentType"
// End: 
