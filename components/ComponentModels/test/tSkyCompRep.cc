//# tSkyCompRep.cc:
//# Copyright (C) 1998,1999,2000,2001,2002
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
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Exceptions/Error.h>
#include <casa/BasicSL/Constants.h>
#include <casa/BasicMath/Math.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MFrequency.h>
#include <casa/Quanta/MVDirection.h>
#include <casa/Quanta/Quantum.h>
#include <measures/Measures/Stokes.h>
#include <casa/Utilities/Assert.h>
#include <casa/BasicSL/String.h>
#include <casa/BasicSL/String.h>
#include <components/ComponentModels/ComponentShape.h>
#include <components/ComponentModels/ConstantSpectrum.h>
#include <components/ComponentModels/Flux.h>
#include <components/ComponentModels/GaussianShape.h>
#include <components/ComponentModels/PointShape.h>
#include <components/ComponentModels/SkyCompRep.h>
#include <components/ComponentModels/SpectralIndex.h>
#include <components/ComponentModels/SpectralModel.h>
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <coordinates/Coordinates/DirectionCoordinate.h>
#include <coordinates/Coordinates/SpectralCoordinate.h>

#include <casa/iostream.h>

#include <casa/namespace.h>
Bool pixelReflection (const SkyCompRep& sky, const CoordinateSystem& cSys,
                      const Vector<Quantum<Double> >& beam, const Unit& unit,
                      Double tol=1.0e-5);

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
      PointShape shape((MDirection(Quantity(1, "deg"), 
				   Quantity(2, "deg"), 
				   MDirection::B1950)));
      SpectralIndex spectrum((MFrequency(Quantity(100, "MHz"),
					 MFrequency::TOPO)),
			     -0.2);
      gComp = SkyCompRep(flux, shape, spectrum);
      gComp.label() = String("Original component");
      flux.setValue(20.0);
      shape.setRefDirection(MDirection(Quantity(10, "deg"), 
				       Quantity(20, "deg"), 
				       MDirection::J2000));
      spectrum.setRefFrequency(MFrequency(Quantity(1, "GHz"),
					  MFrequency::LSRK));
      spectrum.setIndex(1.0);
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
      AlwaysAssert(near(si.index(), -0.2, C::dbl_epsilon), AipsError);
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
      AlwaysAssert(near(si.index(), -0.2, C::dbl_epsilon), AipsError);
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

   {

// Make CoordinateSystem

      Matrix<Double> xform(2,2);
      xform = 0.0; xform.diagonal() = 1.0;
      Vector<Double> incr(2), rp(2), rv(2);
      incr(0) = -3.8785096e-05;
      incr(1) = 5.817764e-05;
      rp(0) = 55.0;
      rp(1) = 37.0;
      rv(0) = 0.0;
      rv(1) = 0.0;
      DirectionCoordinate dC(MDirection::J2000, Projection::SIN, 
                             rv(0), rv(1),
                             incr(0), incr(1), 
                             xform, rp(0), rp(1));
      CoordinateSystem cSys;
      cSys.addCoordinate(dC);

// Make beam

      Vector<Quantum<Double> > beam(3);
      beam(0) = Quantum<Double>(10.0, "arcsec");
      beam(1) = Quantum<Double>(5.0, "arcsec");
      beam(2) = Quantum<Double>(-20.0, "deg");
      Unit unit("Jy/beam");

// Now Flux and shape

      Flux<Double> flux(100.0, 0.1, 0.2, 0.01);
      Quantum<Double> majorAxis(100.0, String("arcsec"));
      Quantum<Double> minorAxis(50.0, String("arcsec"));
      Quantum<Double> pa(120.0, String("deg"));
      ConstantSpectrum cs;

// Try reflection test for just one case.  We are only testing
// the flux conversion part of it here.  The shape stuff is tested
// much harder in tGaussianShape.cc

      {
         MDirection dir(MVDirection(0.01, -0.01), MDirection::J2000);
         GaussianShape gc(dir, majorAxis, minorAxis, pa);
         SkyCompRep sky(flux, gc, cs);
         Bool ok = pixelReflection(sky, cSys, beam, unit, 1.0e-3);
         AlwaysAssert(ok, AipsError);
      }
//
      {

// Peak<->Integral conversions

// First check the scale factor function

         Unit brightnessUnit(String("mJy/beam"));
         Double facToJy = SkyCompRep::convertToJy (brightnessUnit);
         AlwaysAssert(near(facToJy, 1.0e-3), AipsError);      

// Now convert a point source 

         {
            Quantum<Double> peakFlux(1.0, brightnessUnit);
            Quantum<Double> integralFlux = 
               SkyCompRep::peakToIntegralFlux (dC, ComponentType::GAUSSIAN,
                                               peakFlux, beam(0), beam(1),
                                               beam);
            Quantum<Double> peakFlux2 = 
                SkyCompRep::integralToPeakFlux (dC, ComponentType::GAUSSIAN,
                                                integralFlux, brightnessUnit,
                                                beam(0), beam(1), beam);
            AlwaysAssert(near(peakFlux.getValue(),peakFlux2.getValue()), AipsError);
         }

// Now an extended source

         {
            Quantum<Double> peakFlux(1.0, brightnessUnit);
            Quantum<Double> integralFlux = 
               SkyCompRep::peakToIntegralFlux (dC, ComponentType::GAUSSIAN,
                                               peakFlux, majorAxis, minorAxis,
                                               beam);
            Quantum<Double> peakFlux2 = 
                SkyCompRep::integralToPeakFlux (dC, ComponentType::GAUSSIAN,
                                                integralFlux, brightnessUnit,
                                                majorAxis, minorAxis, beam);
            AlwaysAssert(near(peakFlux.getValue(),peakFlux2.getValue()), AipsError);
         }
      }
   }


  } catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 
  cout << "OK" << endl;
  return 0;
}

Bool pixelReflection (const SkyCompRep& sky, const CoordinateSystem& cSys,
                      const Vector<Quantum<Double> >& beam, const Unit& unit,
                      Double tol)
{
   Vector<Double> pars1 = sky.toPixel(unit, beam, cSys, Stokes::I);
   AlwaysAssert(pars1.nelements()==6, AipsError);
//
   SkyCompRep sky2;
   Double ratio;
   sky2.fromPixel (ratio, pars1, unit, beam, cSys, ComponentType::GAUSSIAN, Stokes::I);
   Vector<Double> pars2 = sky2.toPixel(unit, beam, cSys, Stokes::I);
/*
   cerr << pars1 << endl;
   cerr << pars2 << endl;
*/
   AlwaysAssert(allNear(pars1, pars2, tol), AipsError);
   AlwaysAssert(near(ratio,1.0), AipsError);
   return True;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 tSkyCompRep"
// End: 
