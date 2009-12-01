//# tImageFit1D.cc: test the ImageFit1D class
//# Copyright (C) 1995,1996,1998,1999,2000,2001,2002
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
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
#include<images/Images/ImageFit1D.h>

#include<casa/Arrays/Vector.h>
#include<casa/Arrays/ArrayMath.h>
#include<casa/Arrays/ArrayLogical.h>
#include<casa/Arrays/ArrayIO.h>
#include<casa/Arrays/IPosition.h>
#include <casa/Inputs/Input.h>
#include<lattices/Lattices/TiledShape.h>
#include<lattices/Lattices/ArrayLattice.h>
#include<lattices/Lattices/LatticeUtilities.h>
#include<lattices/Lattices/LCSlicer.h>
#include<coordinates/Coordinates/CoordinateSystem.h>
#include<coordinates/Coordinates/SpectralCoordinate.h>
#include<components/SpectralComponents/SpectralList.h>
#include<components/SpectralComponents/SpectralElement.h>
#include<images/Images/TempImage.h>
#include<images/Images/ImageInterface.h>
#include<images/Regions/ImageRegion.h>
#include<casa/Utilities/Assert.h>

#include <casa/iostream.h>

#include <casa/namespace.h>



void testSpectral(uInt what, Bool doVector, Double fac);
//
SpectralCoordinate makeSpectralCoordinate(Double fac);
void makeSpectralData (Vector<Double>& pPars,
                       Vector<Double>& gPars,
                       Vector<Float>& y, 
                       Bool doGauss, Bool doPoly, const Vector<Double>& x);
void makeImage (TempImage<Float>*& pIm, Int n, const Coordinate& c,
                const Vector<Float>& y, Bool doVector);
void check  (const Vector<Double>& pPars, const Vector<Double>& gPars,
             TempImage<Float>*& pIm, Int what, Bool doGauss, Bool doPoly);
void forcedFailure();


int main (int argc, const char* argv[])
{

try {

   Input inputs(1);
   inputs.version ("$Revision$");
   inputs.create("fac", "1.0", "Increment factor");
   inputs.readArguments(argc, argv);
   const Double fac = inputs.getDouble("fac");

// Vector image

      testSpectral(0, True, fac);
      testSpectral(1, True, fac);
      testSpectral(2, True, fac);

// Non-vector image

      testSpectral(0, False, fac);
      testSpectral(1, False, fac);
      testSpectral(2, False, fac);

// Forced failure

      forcedFailure();
//
      cout << "OK" << endl;
      return 0;
 } catch (AipsError err) {
    cerr << err.getMesg() << endl;
    cout << "Not OK" << endl;
    return 1;
 }
}

void testSpectral (uInt abcType, Bool doVector, Double fac)
{
   cerr << endl << endl;
//cerr << "********************************************************************************" << endl;
   if (abcType==0) {
      cerr << "Abcissa in Pixels" << endl;
   } else if (abcType==1) { 
      cerr << "Abcissa in Native" << endl;
   } else if (abcType==2) {
      cerr << "Abcissa in velocity" << endl;
   }
//
   const uInt n = 100;
   SpectralCoordinate sC = makeSpectralCoordinate(fac);

// Make abcissa

   Vector<Double> x(n);
   Double world;
   for (uInt i=0; i<n; i++) {
      if (abcType==0) {
         world = i;
      } else if (abcType==1) {
         sC.toWorld(world, Double(i));
      } else if (abcType==2) {
         sC.pixelToVelocity(world, Double(i));
      }
      x(i) = world;
   }
/*
cerr.setf(ios::fixed, ios::floatfield);
cerr.precision(6);

cerr << "x = " << x << endl;
*/

// Gaussian and Polynomial parameters

   Vector<Double> pPars;
   Vector<Double> gPars;

// Generate Gaussian

   Vector<Float> y;
   {
      cerr << "   Gaussian" << endl;
      Bool doGauss = True;
      Bool doPoly = False;
      makeSpectralData (pPars, gPars, y, doGauss, doPoly, x);
      TempImage<Float>* pIm = 0;
      makeImage(pIm, n, sC, y, doVector);
//
      check  (pPars, gPars, pIm, abcType, doGauss, doPoly);
      delete pIm;
   }

// Polynomial

 {
      cerr << "   Polynomial" << endl;
      Bool doGauss = False;
      Bool doPoly = True;
      makeSpectralData (pPars, gPars, y, doGauss, doPoly, x);
      TempImage<Float>* pIm = 0;
      makeImage(pIm, n, sC, y, doVector);
//
      check  (pPars, gPars, pIm, abcType, doGauss, doPoly);
      delete pIm;
   }

// Gaussian + polynomial

 {
      cerr << "   Gaussian + polynomial" << endl;
      Bool doGauss = True;
      Bool doPoly = True;
      makeSpectralData (pPars, gPars, y, doGauss, doPoly, x);
      TempImage<Float>* pIm = 0;
      makeImage(pIm, n, sC, y, doVector);
//
      check  (pPars, gPars, pIm, abcType, doGauss, doPoly);
      delete pIm;
   }
}



SpectralCoordinate makeSpectralCoordinate(Double fac)
{
   SpectralCoordinate c(MFrequency::TOPO, 1.4e9, 1.0e3, 0.0, 1.41421e9);
   Vector<String> units(1);
   units = "GHz";
   Bool ok = c.setWorldAxisUnits(units);
   AlwaysAssert(ok, AipsError);
//
   Vector<Double> inc;
   inc = c.increment();
   inc *= fac;
   c.setIncrement(inc);
//
   cerr << "Abcissa increment = " << c.increment() << endl;
//
   return c;
}

void makeSpectralData (Vector<Double>& pPars, Vector<Double>& gPars,
                       Vector<Float>& y, Bool doGauss, 
                       Bool doPoly, const Vector<Double>& x)
//
// abcType
//  0     pixel
//  1     native
//  2     velocity
//
{

// Set Gaussian

   uInt n = x.nelements();
   y.resize(n);
   y = 0.0;
//
   gPars.resize(3);
   gPars = 0.0;
//
   if (doGauss) {
      gPars(0) = 2.5;
      gPars(1) = x(n/2);
      uInt idx = 15;                                  // Make width 10 pixels
      AlwaysAssert(n>idx,AipsError);
      gPars(2) = abs(x(idx) - x(0));
      SpectralElement g(SpectralElement::GAUSSIAN, gPars(0), 
                        gPars(1), gPars(2));

// Generate ordinate

      for (uInt i=0; i<n; i++) {
         y(i) = g(x(i));
      }
   }
//
   pPars.resize(2);
   pPars = 0.0;
   if (doPoly) { 
      Float yMax;
      if (doGauss) {
         yMax = max(y) / 10.0;
      } else {
         yMax = 1.0;
      }
//
      pPars(1) = yMax / (x[n-1] - x[0]);         // 0->yMax
      pPars(0) = -pPars(1) * x[0];
//
      SpectralElement p(pPars.nelements()-1);
      p.set(pPars);
/*
cerr << "x[0], x[1] = " << x[0] << ", " << x[n-1] << endl;
cerr << "Polynomial Parameters = " << pPars << endl;
*/

// Generate ordinate

      for (uInt i=0; i<n; i++) {
	y[i] += p(x(i));
      }
   }
// cerr << "y = " << y << endl << endl;
}

void makeImage (TempImage<Float>*& pIm, Int n, const Coordinate& c,
                const Vector<Float>& y, Bool doVector)
{
   CoordinateSystem cSys;
//
   if (doVector) {
      cSys.addCoordinate(c);
      IPosition shape(1,n);
      TiledShape tShape(shape);
//
      pIm = new TempImage<Float>(tShape, cSys);
      pIm->put(y);
//
      ArrayLattice<Bool> maskLat(shape);
      maskLat.set(True);
      pIm->attachMask(maskLat);
   } else {
      IPosition shape(2, n, 10);
      TiledShape tShape(shape);
//
      cSys.addCoordinate(c);
      cSys.addCoordinate(c);
//
      pIm = new TempImage<Float>(tShape, cSys);
//
      Slicer sl (IPosition(2,0,0), shape, Slicer::endIsLength);
      LatticeUtilities::replicate (*pIm, sl, y);
//
      ArrayLattice<Bool> maskLat(shape);
      maskLat.set(True);
      pIm->attachMask(maskLat);
   }
}


void check  (const Vector<Double>& pPars, const Vector<Double>& gPars, 
             TempImage<Float>*& pIm,  Int what, Bool doGauss, Bool doPoly)
{
   const uInt axis = 0;
   ImageFit1D<Float> fitter(*pIm, axis);

// Set abcissa state

   ImageFit1D<Float>::AbcissaType type = ImageFit1D<Float>::PIXEL;
   if (what==0) {
   } else if (what==1) {
      type = ImageFit1D<Float>::IM_NATIVE; 
   } else if (what==2) {
      type = ImageFit1D<Float>::VELOCITY; 
   }

// Set data

   const uInt nDim = pIm->ndim();
   if (nDim==1) {
      IPosition pos(1,0);
      AlwaysAssert(fitter.setData(pos, type), AipsError);
   } else {
      Slicer sl(IPosition(nDim,0), pIm->shape(), Slicer::endIsLength);
      LCSlicer sl2(sl);
      ImageRegion region(sl2);
      AlwaysAssert(fitter.setData(region, type), AipsError);
   }

// Make estimate

   if (doGauss) {
      uInt nGauss = 1;
      AlwaysAssert(fitter.setGaussianElements(nGauss),AipsError);
   }
   if (doPoly) {
      SpectralElement s(pPars.nelements()-1);            // 0 parameters
      fitter.addElement(s);
   }

/*
{
SpectralList list = fitter.getList(False);
cerr << "Estimate = " << list << endl;
}
*/

// Fit

//   AlwaysAssert(fitter.fit(), AipsError);

   Bool ok = fitter.fit();
   if (!ok) {
     cerr << "      Fitter did not converge in " << fitter.getNumberIterations() << " iterations" <<  endl;
   }
 
// Compare

   const SpectralList list = fitter.getList(True);
   Double tol(1e-2);
   Vector<Double> p;
//
   if (doGauss) {
      list[0].get(p);
      AlwaysAssert(p.nelements()==3,AipsError);
      cerr << "      Parameters of gaussian model = " << gPars << endl;
      cerr << "      Parameters of gaussian fit   = " << p << endl;
      if (!(doGauss&&doPoly)) {
	AlwaysAssert(near(gPars[0], p[0], tol), AipsError);
	AlwaysAssert(near(gPars[1], p[1], tol), AipsError); 
	AlwaysAssert(near(gPars[2], p[2], tol), AipsError);
      }
   }
   if (doPoly) {
      if (doGauss) {
         list[1].get(p);
         cerr << endl;
      } else {
         list[0].get(p);
      }
      cerr << "      Parameters of polynomial model = " << pPars << endl;
      cerr << "      Parameters of polynomial fit   = " << p << endl;
/*
      if (what==1 && doGauss&&doPoly) {
         cerr << "       Solution known not to work" << endl;
      }
*/
      AlwaysAssert(p.nelements()==2,AipsError);
      if (!(doGauss&&doPoly)) {
	if (nearAbs(pPars[0], 0.0, tol)) { 
	  AlwaysAssert(nearAbs(pPars[0], p[0], tol), AipsError);
	} else {
	  AlwaysAssert(near(pPars[0], p[0], tol), AipsError);
	};
        AlwaysAssert(near(pPars[1], p[1], tol), AipsError);
      }
   }      
}


void forcedFailure()
{
   ImageFit1D<Float> fitter;
   try {
      fitter.fit();
   } catch (AipsError x) {
      return;
   }
   throw (AipsError("Failed to catch forced error"));
}  
