//# tSkyCompRep.cc:
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

#include <aips/aips.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MVDirection.h>
#include <aips/Measures/Quantum.h>
#include <aips/Measures/Stokes.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/String.h>
#include <trial/ComponentModels/ComponentShape.h>
#include <trial/ComponentModels/ConstantSpectrum.h>
#include <trial/ComponentModels/Flux.h>
#include <trial/ComponentModels/GaussianShape.h>
#include <trial/ComponentModels/PointShape.h>
#include <trial/ComponentModels/SkyCompRep.h>
#include <trial/ComponentModels/SpectralIndex.h>
#include <trial/ComponentModels/SpectralModel.h>
// #include <aips/Arrays/Vector.h>
// #include <aips/Lattices/IPosition.h>
// #include <aips/Measures/MVAngle.h>
// #include <trial/Coordinates/CoordinateSystem.h>
// #include <trial/Coordinates/CoordinateUtil.h>
// #include <trial/Coordinates/StokesCoordinate.h>
// #include <trial/Images/PagedImage.h>
#include <iostream.h>

int main() {
  try {
    //  SkyCompRep();
    const SkyCompRep constComp;
//   virtual const Flux<Double> & flux() const;
    AlwaysAssert(near(constComp.flux().value(0), 1.0, C::dbl_epsilon), 
		 AipsError);
//   virtual const ComponentShape & shape() const;
    AlwaysAssert(constComp.shape().type() == ComponentType::POINT, AipsError);
//   virtual const SpectralModel & spectrum() const;
    AlwaysAssert(constComp.spectrum().type() == 
		 ComponentType::CONSTANT_SPECTRUM, AipsError);
//   virtual const String & label() const;
    AlwaysAssert(constComp.label() == String(""), AipsError);
//   SkyCompRep(const ComponentType::Shape & shape);
    SkyCompRep gComp(ComponentType::GAUSSIAN);
    AlwaysAssert(gComp.shape().type() == ComponentType::GAUSSIAN,
		 AipsError);
    AlwaysAssert(gComp.spectrum().type() == 
		 ComponentType::CONSTANT_SPECTRUM, AipsError);
//   SkyCompRep(const ComponentType::Shape & shape,
//  	     const ComponentType::SpectralShape & spectrum);
    SkyCompRep siComp(ComponentType::POINT, ComponentType::SPECTRAL_INDEX);
    AlwaysAssert(siComp.shape().type() == ComponentType::POINT, AipsError);
    AlwaysAssert(siComp.spectrum().type() == 
		 ComponentType::SPECTRAL_INDEX, AipsError);
//   SkyCompRep & operator=(const SkyCompRep & other);
    {
      Flux<Double> flux(10, 5, 2, 1);
      PointShape shape(MDirection(Quantity(1, "deg"), 
				  Quantity(2, "deg"), 
				  MDirection::B1950));
      SpectralIndex spectrum(MFrequency(Quantity(100, "MHz"),MFrequency::TOPO),
			     -0.2);
      gComp = SkyCompRep(flux, shape, spectrum);
      gComp.label() = String("Original component");
      flux.setValue(20.0);
      shape.setRefDirection(MDirection(Quantity(10, "deg"), 
				       Quantity(20, "deg"), 
				       MDirection::J2000));
      spectrum.setRefFrequency(MFrequency(Quantity(1, "GHz"),MFrequency::LSR));
      spectrum.setIndex(1.0, Stokes::I);
    }
    AlwaysAssert(near(gComp.flux().value(0), 10, C::dbl_epsilon), AipsError);
    AlwaysAssert(gComp.shape().type() == ComponentType::POINT,
		 AipsError);
    AlwaysAssert(gComp.shape().refDirection().getValue().near
		 (MVDirection(Quantity(1, "deg"), Quantity(2, "deg")), 
		  C::dbl_epsilon), AipsError);
    AlwaysAssert(gComp.shape().refDirection().getRef().getType() == 
		 MDirection::B1950, AipsError);
    AlwaysAssert(gComp.spectrum().type() == 
		 ComponentType::SPECTRAL_INDEX, AipsError);
    AlwaysAssert(gComp.spectrum().refFrequency().getValue().near
		 (MVFrequency(Quantity(100, "MHz")), C::dbl_epsilon),
		 AipsError);
    AlwaysAssert(gComp.spectrum().refFrequency().getRef().getType() == 
		 MFrequency::TOPO, AipsError);
    {
      const SpectralIndex & si((const SpectralIndex &) gComp.spectrum());
      AlwaysAssert(near(si.index(Stokes::I), -0.2, C::dbl_epsilon), AipsError);
    }
    AlwaysAssert(gComp.label() == String("Original component"), AipsError);
//   SkyCompRep(const SkyCompRep & other);
    SkyCompRep saveComp(gComp);
//   virtual Flux<Double> & flux();
    gComp.flux() = Flux<Double>(100,99,98,97);
//   virtual ComponentShape & shape();
    gComp.setShape(GaussianShape());
//   virtual SpectralModel & spectrum();
    gComp.setSpectrum(ConstantSpectrum());
//   virtual String & label();
    gComp.label() = String("New one"); 
    AlwaysAssert(near(saveComp.flux().value(0), 10, C::dbl_epsilon), AipsError);
    AlwaysAssert(saveComp.shape().type() == ComponentType::POINT,
		 AipsError);
    AlwaysAssert(saveComp.shape().refDirection().getValue().near
		 (MVDirection(Quantity(1, "deg"), Quantity(2, "deg")), 
		  C::dbl_epsilon), AipsError);
    AlwaysAssert(saveComp.shape().refDirection().getRef().getType() == 
		 MDirection::B1950, AipsError);
    AlwaysAssert(saveComp.spectrum().type() == 
		 ComponentType::SPECTRAL_INDEX, AipsError);
    AlwaysAssert(saveComp.spectrum().refFrequency().getValue().near
		 (MVFrequency(Quantity(100, "MHz")), C::dbl_epsilon),
		 AipsError);
    AlwaysAssert(saveComp.spectrum().refFrequency().getRef().getType() == 
		 MFrequency::TOPO, AipsError);
    {
      const SpectralIndex & si((const SpectralIndex &) saveComp.spectrum());
      AlwaysAssert(near(si.index(Stokes::I), -0.2, C::dbl_epsilon), AipsError);
    }
    AlwaysAssert(saveComp.label() == String("Original component"), AipsError);
    AlwaysAssert(near(gComp.flux().value(0), 100, C::dbl_epsilon), AipsError);
    AlwaysAssert(gComp.shape().type() == ComponentType::GAUSSIAN,
		 AipsError);
    AlwaysAssert(gComp.shape().refDirection().getValue().nearAbs
		 (MVDirection(Quantity(0, "deg"), Quantity(90, "deg")), 
		  C::dbl_epsilon), AipsError);
    AlwaysAssert(gComp.shape().refDirection().getRef().getType() == 
		 MDirection::J2000, AipsError);
    AlwaysAssert(gComp.spectrum().type() == 
		 ComponentType::CONSTANT_SPECTRUM, AipsError);
    AlwaysAssert(gComp.spectrum().nParameters() == 0, AipsError);
    AlwaysAssert(gComp.label() == String("New one"), AipsError);
//   virtual Flux<Double> sample(const MDirection & direction, 
// 			      const MVAngle & pixelSize, 
// 			      const MFrequency & centerFrequency) const;
//   virtual void project(ImageInterface<Float> & plane) const;
//   virtual Flux<Double> visibility(const Vector<Double> & uvw,
// 				  const Double & frequency) const;
//   virtual Bool fromRecord(String & errorMessage, 
// 			  const RecordInterface & record);
//   virtual Bool toRecord(String & errorMessage, 
// 			RecordInterface & record) const;

//   virtual Bool ok() const;
    {
      AlwaysAssert(constComp.ok() == True, AipsError);
      AlwaysAssert(gComp.ok() == True, AipsError);
      AlwaysAssert(siComp.ok() == True, AipsError);
    }
//   virtual ~SkyCompRep();
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } end_try;
  cout << "OK" << endl;
  return 0;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 tSkyCompRep"
// End: 
