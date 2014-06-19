//# tImageUtilities.cc: Test program for the static ImageUtilities functions
//# Copyright (C) 2001,2002,2003
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

#include <casa/Arrays/IPosition.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/Arrays/MaskedArray.h>
#include <casa/Quanta/QLogical.h>

#include <coordinates/Coordinates/CoordinateSystem.h>
#include <coordinates/Coordinates/LinearCoordinate.h>
#include <coordinates/Coordinates/CoordinateUtil.h>
#include <coordinates/Coordinates/SpectralCoordinate.h>
#include <images/Images/ImageUtilities.h>
#include <images/Images/PagedImage.h>
#include <images/Images/ImageFITSConverter.h>
#include <images/Images/TempImage.h>
#include <images/Images/ImageOpener.h>
#include <lattices/Lattices/PagedArray.h>
#include <lattices/Lattices/ArrayLattice.h>
#include <lattices/Lattices/LatticeUtilities.h>
#include <casa/Logging/LogIO.h>
#include <casa/Logging/LogOrigin.h>
#include <casa/OS/RegularFile.h>
#include <casa/OS/Directory.h>
#include <casa/IO/RegularFileIO.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Utilities/Assert.h>
#include <casa/iostream.h>

#include <casa/namespace.h>
void doOpens()
{
   Directory dir("tImageUtilities_tmp");
   dir.create();
   LogIO os(LogOrigin("tImageUtilities", "doOpens()", WHERE));
   os << "Open Image tests" << LogIO::POST;
//
   {
      String name1("tImageUtilities_tmp/app.img");
      PagedImage<Float> img (IPosition(2,10,10),
   			      CoordinateUtil::defaultCoords2D(), 
                              name1);
      String error;
      String name2("tImageUtilities_tmp/fits.img");
      ImageFITSConverter::ImageToFITS(error, img, name2,
				      64, True, True, -32, 1, -1,
				      True);

      {
         PtrHolder<ImageInterface<Float> > im;
         ImageUtilities::openImage(im, name1);
      }
      {
         PtrHolder<ImageInterface<Float> > im;
         //ImageUtilities::openImage(im, name2, os);
	 os << "Skipping openImage() for fits.img because ImageOpener::openImage('fits.img') reports type is unknown.  Needs investigation." << LogIO::POST;
      }
   }
//
  dir.removeRecursive();
}


void doTypes()
{
   LogIO os(LogOrigin("tImageUtilities", __FUNCTION__, WHERE));
   os << "Image Type test" << LogIO::POST;
//
   Directory dir("tImageUtilities_tmp");
   dir.create();
  {
    PagedImage<Float> img (IPosition(2,10,10),
			   CoordinateUtil::defaultCoords2D(),
			   "tImageUtilities_tmp/app.img");
  }
  AlwaysAssertExit (ImageOpener::imageType ("tImageUtilities_tmp/app.img")
		    == ImageOpener::AIPSPP);
  {
    PagedArray<Float> arr (IPosition(2,10,10),
			   "tImageUtilities_tmp/app.img");
  }
  AlwaysAssertExit (ImageOpener::imageType ("tImageUtilities_tmp/app.img")
		    == ImageOpener::UNKNOWN);
  {
    Directory dir("tImageUtilities_tmp/mir.img");
    dir.create();
    RegularFile rfile("tImageUtilities_tmp/mir.img/image");
    rfile.create();
  }
  AlwaysAssertExit (ImageOpener::imageType ("tImageUtilities_tmp/mir.img")
		    == ImageOpener::UNKNOWN);
  {
    RegularFile rfile("tImageUtilities_tmp/mir.img/header");
    rfile.create();
  }
  AlwaysAssertExit (ImageOpener::imageType("tImageUtilities_tmp/mir.img")
		    == ImageOpener::MIRIAD);
  {
    RegularFile rfile("tImageUtilities_tmp/a.image");
    rfile.create();
  }
  AlwaysAssertExit (ImageOpener::imageType("tImageUtilities_tmp/a.image")
		    == ImageOpener::UNKNOWN);
  char buf[2880];
  memset (buf, ' ', 2880);
  memcpy (buf, "SIMPLE  =   T  ", 17);
  {
    RegularFileIO file (RegularFile("tImageUtilities_tmp/a.image"),
			ByteIO::Update);
    file.write (2879, buf);
  }
  AlwaysAssertExit (ImageOpener::imageType ("tImageUtilities_tmp/a.image")
		    == ImageOpener::UNKNOWN);
  {
    RegularFileIO file (RegularFile("tImageUtilities_tmp/a.image"),
			ByteIO::Update);
    file.write (2880, buf);
  }
  AlwaysAssertExit (ImageOpener::imageType("tImageUtilities_tmp/a.image")
		    == ImageOpener::FITS);
  {
    RegularFile rfile("tImageUtilities_tmp/a.descr");
    rfile.create();
  }
  AlwaysAssertExit (ImageOpener::imageType ("tImageUtilities_tmp/a.image")
		    == ImageOpener::GIPSY);
//
  dir.removeRecursive();
}

void listWorld (const Vector<Quantum<Double> >& wPars)
{
   cerr << "World" << endl;
   if (wPars.nelements()==3){ 
      cerr << "  Major, minor, pa = " << wPars(0) << ", " << wPars(1)
           << ", " << wPars(2) << endl;
   } if (wPars.nelements()==5) {
      cerr << "  X, Y = " << wPars(0) << ", " << wPars(1) << endl;
      cerr << "  Major, minor, pa = " << wPars(2) << ", " << wPars(3)
           << ", " << wPars(4) << endl;
   }
}

void listWorld (const GaussianBeam& wPars)
{
   cerr << "World" << endl;
   if (! wPars.isNull()){
      cerr << "  Major, minor, pa = " << wPars.getMajor()
    		<< ", " << wPars.getMinor() << ", " << wPars.getPA() << endl;
   }
}
   
void listPixel(const Vector<Double>& pPars)
{
   cerr << "Pixel" << endl;
   if (pPars.nelements()==3) {
      cerr << "  Major, minor, pa = " << pPars(0) << ", " << pPars(1) << ", " 
           << pPars(2)/C::degree  << endl;   
   } else if (pPars.nelements()==5) {
      cerr << "  X, Y = " << pPars(0) << ", " << pPars(1) << endl;
      cerr << "  Major, minor, pa = " << pPars(2) << ", " << pPars(3) << ", " 
           << pPars(4)/C::degree  << endl;   
   }
}

void doConversions()
{
   LogOrigin lor("tImageUtilities", "doConversions()", WHERE);
   LogIO os(lor);
   os << "Conversion Tests" << LogIO::POST;
//
   {
      CoordinateSystem cSys = CoordinateUtil::defaultCoords2D();
      cerr << "inc = " << cSys.increment() << endl;
      const Vector<String>& units = cSys.worldAxisUnits();
      IPosition pixelAxes(2, 0, 1);
      IPosition worldAxes(2, 0, 1);
//
      Vector<Double> world;
      Vector<Double> pixel = cSys.referencePixel().copy();
      pixel += 10.0;
      cSys.toWorld(world, pixel);
//
      Vector<Quantum<Double> > wPars(5);

// Position

      wPars(0).setValue(world(worldAxes(0)));
      wPars(0).setUnit(units(worldAxes(0)));
      wPars(1).setValue(world(worldAxes(1)));
      wPars(1).setUnit(units(worldAxes(1)));

// Shape

      wPars(2).setValue(20.0);
      wPars(2).setUnit(Unit(String("arcmin")));
      wPars(3).setValue(10.0);
      wPars(3).setUnit(Unit(String("arcmin")));
      wPars(4).setValue(70.0);
      wPars(4).setUnit(Unit(String("deg")));

// Convert to pixel

      Vector<Double> pPars;
      ImageUtilities::worldWidthsToPixel (pPars, wPars, cSys, pixelAxes);

// Back to world

      Vector<Double> pPars2(5);
      pPars2(0) = pixel(pixelAxes(0));
      pPars2(1) = pixel(pixelAxes(1));
      for (uInt i=0; i<3; i++) {
         pPars2(i+2) = pPars(i);
      }
      GaussianBeam wPars2;
      ImageUtilities::pixelWidthsToWorld (wPars2, pPars2, cSys, pixelAxes);     
//
      listWorld(wPars);
      listPixel(pPars);
      listWorld(wPars2);
//
      for (uInt i=0; i<wPars.nelements(); i++) {
//         AlwaysAssert(wPars(i)==wPars2(i), AipsError);
      }
   }
}

void doBin()
{
   LogOrigin lor("tImageUtilities", __FUNCTION__, WHERE);
   LogIO os(lor);
   os << "Binning Tests" << LogIO::POST;
//
   uInt n = 32;
   IPosition shape(1,n);
   SpectralCoordinate cIn, cOut;
   Array<Float> data(shape);
   Array<Bool> mask(shape);
   indgen(data);
   mask = True;
   MaskedArray<Float> maIn(data,mask);
   MaskedArray<Float> maOut;
   uInt bin = 2;
   uInt axis = 0;
//
   ImageUtilities::bin(maOut, cOut, maIn, cIn, axis, bin);
   AlwaysAssert(maOut.nelements()==n/bin, AipsError);
   Array<Float> pOut(maOut.shape());
   indgen(pOut);
   pOut *= Float(bin);
   pOut += Float(0.5);
   AlwaysAssert(allNear(pOut,maOut.getArray(),1e-6), AipsError);
   AlwaysAssert(allEQ(maOut.getMask(),True), AipsError);
}

/*
void doDeconvolveFromBeam() {
	LogOrigin lor("tImageUtilities", __FUNCTION__, WHERE);
	LogIO os(lor);
	Angular2DGaussian convolved(
		Quantity(5, "arcsec"),
		Quantity(5, "arcsec"),
		Quantity(0, "deg")
	);
	Angular2DGaussian deconvolved;
	Bool fitSuccess = False;
	GaussianBeam beam(
		Quantity(4, "arcsec"), Quantity(4, "arcsec"),
		Quantity(0, "deg")
	);

	Bool isPointSource = ImageUtilities::deconvolveFromBeam(
		deconvolved, convolved, fitSuccess, os, beam
	);
	Angular2DGaussian exp(
		Quantity(3, "arcsec"),
		Quantity(3, "arcsec"),
		Quantity(0, "deg")
	);
	AlwaysAssert(! isPointSource, AipsError);
	AlwaysAssert(fitSuccess, AipsError);
	AlwaysAssert(deconvolved == exp, AipsError);

	convolved = Angular2DGaussian(
		Quantity(10, "arcsec"),
		Quantity(5, "arcsec"),
		Quantity(20, "deg")
	);
	fitSuccess = False;
	beam = GaussianBeam(
		Quantity(8, "arcsec"), Quantity(4, "arcsec"),
		Quantity(20, "deg")
	);

	isPointSource = ImageUtilities::deconvolveFromBeam(
		deconvolved, convolved, fitSuccess, os, beam
	);
	exp = Angular2DGaussian(
		Quantity(6, "arcsec"),
		Quantity(3, "arcsec"),
		Quantity(20, "deg")
	);
	AlwaysAssert(! isPointSource, AipsError);
	AlwaysAssert(fitSuccess, AipsError);
	AlwaysAssert(near(deconvolved, exp, 1e-7, Quantity(1, "uas")), AipsError);


	// make beam larger than the source so the fit fails
	beam = GaussianBeam(
		Quantity(6, "arcsec"), Quantity(6, "arcsec"),
		Quantity(0, "deg")
	);
	convolved = Angular2DGaussian(
		Quantity(5, "arcsec"),
		Quantity(5, "arcsec"),
		Quantity(20, "deg")
	);
	isPointSource = ImageUtilities::deconvolveFromBeam(
		deconvolved, convolved, fitSuccess, os, beam
	);
	AlwaysAssert(! fitSuccess, AipsError);

	// TODO test for point source, I can't figure out how to actually set parameters so the method
	// returns true
}
  */
int main()
{
  try {
    doBin();
    doTypes();
    doOpens();
  } catch (const AipsError& x) {
    cout << "Unexpected exception: " << x.getMesg() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
