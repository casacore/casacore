//# tFrequencyAligner.cc: Test program for FrequencyAligner
//# Copyright (C) 1998,1999,2000,2001,2002,2003,2004
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
//#

 
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/VectorIter.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/QC.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/coordinates/Coordinates/SpectralCoordinate.h>
#include <casacore/coordinates/Coordinates/FrequencyAligner.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Logging/LogIO.h> 
#include <casacore/casa/Logging/LogOrigin.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/casa/Quanta/MVEpoch.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/casa/Quanta/MVFrequency.h>
#include <casacore/measures/Measures/MFrequency.h>

#include <casacore/casa/namespace.h>
#include <casacore/casa/iostream.h>

SpectralCoordinate makeLinearCoordinate(MFrequency::Types type,
                                        Double& crval,
                                        Double& cdelt,
                                        Double& crpix,
                                        Double& restFreq, const String& unit);


int main()
{
   try {

      Double f0, finc, refchan, restFreq;
      Vector<Double> freqs;
      Matrix<Double> xform(1,1); xform(0,0) = 1.0;
      String unit("GHz");

// Make SC

      MFrequency::Types sysIn(MFrequency::TOPO);
      MFrequency::Types sysOut(MFrequency::BARY);

      SpectralCoordinate lc = 
         makeLinearCoordinate(sysIn, f0, finc, refchan, restFreq, unit);

// Make aligner

      const uInt nPix = 16;
      Quantum<Double> t(50237.29, Unit(String("d")));  
      MVEpoch t2(t);
      MEpoch refEpoch(t2);
//
      MPosition pos;
      MeasTable::Observatory(pos, String("ATCA"));
//
      Quantum<Double> lon(0.0,Unit(String("rad")));
      Quantum<Double> lat(-35.0,Unit(String("deg")));
      MDirection dir(lon, lat, MDirection::J2000);
      FrequencyAligner<Float> fa(lc, nPix, refEpoch, dir, pos, sysOut);
      InterpolateArray1D<Double,Float>::InterpolationMethod method=InterpolateArray1D<Double,Float>::linear;
      Bool extrapolate=False;
      Bool useCachedX = False;

// Generate some data

      Vector<Bool> maskIn(nPix,True), maskOut(nPix);
      Vector<Float> yOut(nPix), yIn(nPix), yOut2(nPix);
      Float val = 0.0;
      for (uInt i=0; i<nPix; i++) {
         if (i<nPix/2) {
           val += 1.0;
         } else {
           val -= 1.0;
         }
         yIn[i] = val;
      }

// CHeck output SC

      {
         SpectralCoordinate sCNew = fa.alignedSpectralCoordinate();
         AlwaysAssert(sCNew.frequencySystem()==sysOut,AipsError);
//
         MEpoch tEp;
         MPosition tPos;
         MDirection tDir;
         MFrequency::Types tType;
         sCNew.getReferenceConversion(tType, tEp, tPos, tDir);
         AlwaysAssert(tType==sysOut,AipsError);
      }

// Set tolerance so no interpolation

      {
         fa.setTolerance (1.0);
//
         Quantum<Double> tt(50237.50, Unit(String("d")));  
         MVEpoch t3(tt);
         MEpoch epoch(t3);
         cerr << "No Interpolation" << endl;
         AlwaysAssert(!fa.align (yOut, maskOut, yIn, maskIn, epoch, 
                                 useCachedX, method, extrapolate), AipsError);
/*
         cerr << "   yIn = " << yIn << endl;
         cerr << "   yOut = " << yOut << endl;
*/
         AlwaysAssert (allNear(yOut, yIn, 1e-6), AipsError);

// Remove tolerance

         fa.setTolerance (0.0);
      }

// Align with new abcissa computed

      {
         Quantum<Double> tt(50237.50, Unit(String("d")));  
         MVEpoch t3(tt);
         MEpoch epoch(t3);
         cerr << "Interpolation" << endl;
         AlwaysAssert(fa.align (yOut, maskOut, yIn, maskIn, epoch, 
                                useCachedX, method, extrapolate),AipsError);
/*
         cerr << "   yIn = " << yIn << endl;
         cerr << "   yOut = " << yOut << endl;
*/
         AlwaysAssert (!allNear(yOut, yIn, 1e-6), AipsError);    // Feeble test !

// Align with new abcissa taken from cached vector

         useCachedX = True;
         AlwaysAssert(fa.align (yOut2, maskOut, yIn, maskIn, epoch, 
                                useCachedX, method, extrapolate),AipsError);
         AlwaysAssert (allNear(yOut2, yOut, 1e-6), AipsError);
      }

// Get Abcissas for fun

      {
         Vector<Double> xRefOut, xOut;
         fa.getReferenceAbcissa (xRefOut);
         fa.getAbcissa (xOut);
/*
         cerr << "   xRefOut = " << xRefOut << endl;
         cerr << "   xOut = " << xOut << endl;
*/
      }


// Align many in one call

      {
         cerr << "Align many" << endl;
         const uInt nx = yIn.nelements();
         const uInt ny = 5;
         IPosition shp(2,nx,ny);
         Array<Float> yInMany(shp);
         Array<Bool> maskInMany(shp);
         Array<Float> yOutMany;
         Array<Bool> maskOutMany;
//
         IPosition pp(2,0);
         for (uInt j=0; j<ny; j++) {
            pp(1) = j;
            for (uInt i=0; i<nx; i++) {
              pp(0) = i;
              yInMany(pp) = yIn(i);
              maskInMany(pp) = maskIn(i);
            }
         }            
//
         Quantum<Double> tt(50237.50, Unit(String("d")));  
         MVEpoch t3(tt);
         MEpoch epoch(t3);
//
         uInt axis = 0;
//
         Bool ok = fa.alignMany (yOutMany, maskOutMany, yInMany, maskInMany,
                                 axis, epoch, method, extrapolate);
         AlwaysAssert(ok, AipsError);
         ReadOnlyVectorIterator<Float> it(yOutMany,axis);
         Vector<Float> data1;
         Vector<Bool> mask1;
         uInt cnt = 0;
         while (!it.pastEnd()) {
           if (cnt==0) {
              data1 = it.vector();
           } else {
              AlwaysAssert (allNear(it.vector(), data1, 1e-6), AipsError);
           }
           it.next(); cnt++;
         }
      }


// Copy constructor and test results the same

      FrequencyAligner<Float> va2(fa);
      {
         cerr << "Copy Constructor" << endl;
         Quantum<Double> tt(50237.50, Unit(String("d")));  
         MVEpoch t3(tt);
         MEpoch epoch(t3);
         Vector<Float> yOut3;
         useCachedX = True;
         AlwaysAssert(fa.align (yOut3, maskOut, yIn, maskIn, epoch, 
                                useCachedX, method, extrapolate),AipsError); // Use cached
         AlwaysAssert (allNear(yOut3, yOut, 1e-6), AipsError);
//
         useCachedX = False;
         AlwaysAssert(fa.align (yOut2, maskOut, yIn, maskIn, epoch, 
                                useCachedX, method, extrapolate),AipsError); // Recompute
         AlwaysAssert (allNear(yOut3, yOut, 1e-6), AipsError);
      }

// Assignment and test results the same

      FrequencyAligner<Float> va3;
      va3 = fa;
      {
         cerr << "Assignment operator" << endl;
         Quantum<Double> tt(50237.50, Unit(String("d")));  
         MVEpoch t3(tt);
         MEpoch epoch(t3);
         Vector<Float> yOut3;
         useCachedX = True;
         AlwaysAssert(fa.align (yOut3, maskOut, yIn, maskIn, epoch, 
                                useCachedX, method, extrapolate),AipsError);  // Use cached
         AlwaysAssert (allNear(yOut3, yOut, 1e-6), AipsError);
//
         useCachedX = False;
         AlwaysAssert(fa.align (yOut2, maskOut, yIn, maskIn, epoch, 
                                useCachedX, method, extrapolate),AipsError); // Recompute
         AlwaysAssert (allNear(yOut3, yOut, 1e-6), AipsError);
      }
   } catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      return (1);
   }

   cout << "ok" << endl; 
   return (0);
}

SpectralCoordinate makeLinearCoordinate (MFrequency::Types type,
                                         Double& f0,
                                         Double& finc,
                                         Double& refchan,
                                         Double& restFreq,
                                         const String& unit)
{
   refchan = 10.5;
   finc = 4e6;
   f0 = 1.4e9;
   restFreq = 1.420405752E9;
//
   SpectralCoordinate sc(type, f0, finc, refchan, restFreq);
   Vector<String> units(1);
   units(0) = unit;
   sc.setWorldAxisUnits(units);
   return sc;
}



