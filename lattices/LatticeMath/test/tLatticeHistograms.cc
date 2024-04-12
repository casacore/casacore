//# tLatticeHistograms.cc: test LatticeHistograms class
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
// 
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/Logging.h>
#include <casacore/scimath/Mathematics/NumericTraits.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/lattices/LatticeMath/LatticeHistograms.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/LatticeMath/LatticeStatsBase.h>
#include <casacore/lattices/Lattices/LatticeUtilities.h>
#include <casacore/lattices/LRegions/LCSlicer.h>
#include <casacore/casa/System/PGPlotter.h>

#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
void doitFloat(LogIO& os);
void do1DFloat (const Array<Float>& inArr,
                LogIO& os);
void do2DFloat (const Array<Float>& inArr, LogIO& os);
void test1DFloat (LatticeHistograms<Float>& histo, const IPosition& shape, uInt nBin);
void test2DFloat (LatticeHistograms<Float>& histo,  const IPosition& shape, uInt nBin);


int main() {
    try {
        LogOrigin lor("tLatticeHistograms", __func__, WHERE);
        LogIO os(lor);
        doitFloat(os);
        {
            Array<Float> arr(IPosition(2, 5, 10), 0.0f);
            for (uInt i=0; i<10; i++ ) {
                arr(IPosition(2, 4, i)) = 100*(i+1);
                arr(IPosition(2, 0, i)) = -arr(IPosition(2, 4, i));
            }
            ArrayLattice<Float> latt(arr);
            SubLattice<Float> subLatt(latt);
            LatticeHistograms<Float> lh(subLatt);
            lh.setNBins(25);
            Array<Float> values, counts;
            Vector<Float> range(2);
            range[0] = -5;
            range[1] = 5;
            lh.setIncludeRange(range);
            lh.getHistograms(values, counts);
            AlwaysAssert(counts(IPosition(1, 12)) == 30, AipsError);
        }
        {
            Array<Float> arr(IPosition(3, 2, 4, 6), 0.0f);
            Float sum = 0;
            for (uInt i=0; i<2; ++i) {
                for (uInt j=0; j<4; ++j) {
                    for (uInt k=0; k<6; ++k) {
                        arr(IPosition(3, i, j, k)) = i + j + k;
                        sum += arr(IPosition(3, i, j, k));
                    }
                }
            }
            ArrayLattice<Float> latt(arr);
            SubLattice<Float> subLatt(latt);
            LatticeHistograms<Float> lh(subLatt);
            lh.setNBins(25);
            Array<Float> values, counts;
            Array<Vector<Float> > stats;
            lh.getHistograms(values, counts, stats);
            AlwaysAssert(stats.shape() == IPosition(1, 1), AipsError);
            lh.getHistograms(values, counts, stats);
            AlwaysAssert(
                    stats(IPosition(1, 0))[LatticeStatsBase::SUM] == sum,
                    AipsError
            );
            AlwaysAssert(
                    stats(IPosition(1, 0))[LatticeStatsBase::NPTS] == 48,
                    AipsError
            );
            AlwaysAssert(
                    stats(IPosition(1, 0))[LatticeStatsBase::MEAN] == sum/48,
                    AipsError
            );
            Vector<Int> axes(2, 0);
            axes[1] = 1;
            lh.setAxes(axes);
            lh.getHistograms(values, counts, stats);
            AlwaysAssert(stats.shape() == IPosition(1, 6), AipsError);
            for (uInt i=0; i<6; ++i) {
                Float sum = 16 + 8*i;
                AlwaysAssert(
                        stats(IPosition(1, i))[LatticeStatsBase::SUM] == sum,
                        AipsError
                );
                AlwaysAssert(
                        stats(IPosition(1, i))[LatticeStatsBase::NPTS] == 8,
                        AipsError
                );
                AlwaysAssert(
                        stats(IPosition(1, i))[LatticeStatsBase::MEAN] = sum/8.0,
                        AipsError
                );
            }
            axes.resize(1);
            axes[0] = 1;
            lh.setAxes(axes);
            lh.getHistograms(values, counts, stats);
            AlwaysAssert(stats.shape() == IPosition(2, 2, 6), AipsError);
            for (uInt i=0; i<2; ++i) {
                for (uInt j=0; j<6; ++j) {
                    Float sum = 4*(i+j) + 6;
                    AlwaysAssert(
                            stats(IPosition(2, i, j))[LatticeStatsBase::SUM] == sum,
                            AipsError
                    );
                    AlwaysAssert(
                            stats(IPosition(2, i, j))[LatticeStatsBase::NPTS] == 4,
                            AipsError
                    );
                    AlwaysAssert(
                            stats(IPosition(2, i, j))[LatticeStatsBase::MEAN] = sum/4.0,
                            AipsError
                    );
                }
            }
        }
    }
    catch (const std::exception& x) {
        cerr << "aipserror: error " << x.what() << endl;
        return 1;
    }
    return 0;
}
   
 
void doitFloat (LogIO& os) 
{   
      
// Construct data array

   IPosition shape(1);
   shape = 40;
   Array<Float> inArr(shape);
   indgen(inArr);

// Make 1D Lattice and test

   do1DFloat(inArr, os);

// Make 2D lattice and test

   do2DFloat(inArr, os);
}


void do1DFloat (const Array<Float>& inArr, LogIO& os)
{
   const IPosition shape = inArr.shape();
   ArrayLattice<Float> inLat(inArr);
   SubLattice<Float> subLat(inLat);
   LatticeHistograms<Float> histo(subLat, os, False, False);

// Make a flat histogram so we can test it easily

   const uInt nBin = shape(0)/4;
   AlwaysAssert(histo.setNBins(nBin), AipsError);

// Test

   test1DFloat (histo, shape, nBin);

// Test copy constructor - feeble test
     
   {
      LatticeHistograms<Float> histo2(histo);
      test1DFloat (histo2, shape, nBin);
   }

// Test assignment operator - feeble test
   
   {
      LatticeHistograms<Float> histo2(histo);
      histo = histo2;
      test1DFloat (histo, shape, nBin);
   }

// Test setNewLattice - feeble test

   {
      AlwaysAssert(histo.setNewLattice(subLat), AipsError);
      test1DFloat (histo, shape, nBin);
   }
}

void do2DFloat (const Array<Float>& arr, LogIO& os)
{
   uInt nX = arr.shape()(0);
   uInt nY = 20;
   IPosition shape(2,nX,nY);

// Fill Lattice with replicated rows

   ArrayLattice<Float> lat(shape);
   Slicer slice(IPosition(2,0,0),shape,Slicer::endIsLength);
   LatticeUtilities::replicate (lat, slice, arr);
   SubLattice<Float> subLat(lat);

// Make LS object and set axes so that we work out histo
// over first axis as a function of nY replicated rows

   LatticeHistograms<Float> histo(subLat, os, False, False);
   Vector<Int> axes(1);
   axes = 0;
   AlwaysAssert(histo.setAxes(axes), AipsError);

// Make a flat histogram so we can test it easily

   const uInt nBin = shape(0)/4;
   AlwaysAssert(histo.setNBins(nBin), AipsError);
//
   const uInt n = nX/4;
   AlwaysAssert(histo.setNBins(n), AipsError);

// Test

   test2DFloat (histo, shape, nBin);

// Test copy constructor - feeble test
     
   {
      LatticeHistograms<Float> histo2(histo);
      test2DFloat (histo2, shape, nBin);
   }

// Test assignment operator - feeble test
   
   {
      LatticeHistograms<Float> histo2(histo);
      histo = histo2;
      test2DFloat (histo, shape, nBin);
   }

// Test setNewLattice - feeble test

   {
      AlwaysAssert(histo.setNewLattice(subLat), AipsError);
      test2DFloat (histo, shape, nBin);
   }
}


void test1DFloat (LatticeHistograms<Float>& histo, const IPosition& shape, uInt nBin)
{
   AlwaysAssert(histo.displayAxes().nelements()==0, AipsError);
//
   Float val = Float(shape(0) / nBin);
   Double tol = 1.0e-6;
   {
      IPosition pos(1,0);
      Vector<Float> values, counts;
      AlwaysAssert(histo.getHistogram(values, counts, pos, True), AipsError);
   }
//
   {
      Vector<Float> values, counts;
      AlwaysAssert(histo.getHistograms(values, counts), AipsError);
      AlwaysAssert(values.shape()==IPosition(1,nBin),AipsError);
      AlwaysAssert(counts.shape()==IPosition(1,nBin),AipsError);
      AlwaysAssert(allNear(counts, val, tol), AipsError);
   }
}


void test2DFloat (LatticeHistograms<Float>& histo, const IPosition& shape, uInt nBin)
{
   AlwaysAssert(shape.nelements()==2,AipsError);
   const Vector<Int> dA = histo.displayAxes();
   AlwaysAssert(dA.nelements()==1, AipsError);
   AlwaysAssert(dA(0)==1, AipsError);
   const uInt nY = shape(1);
//
   Float val = Float(shape(0) / nBin);
   Double tol = 1.0e-6;
//
   {
      IPosition pos(2,0,0);
      Vector<Float> values, counts;
      AlwaysAssert(histo.getHistogram(values, counts, pos, True), AipsError);
      AlwaysAssert(values.shape()==IPosition(1,nBin),AipsError);
   }

// Check histo correct for each row 

   {
      Array<Float> values, counts;
      AlwaysAssert(histo.getHistograms(values, counts), AipsError);
      AlwaysAssert(values.shape()==IPosition(2,nBin,nY),AipsError);
      AlwaysAssert(counts.shape()==IPosition(2,nBin,nY),AipsError);
//
      IPosition beg(2,0,0);
      IPosition end(2,nBin-1,0);
      for (uInt j=0; j<nY; j++) {
         beg(1) = j;
         end(1) = j;
         Array<Float> h = counts(beg, end);
         AlwaysAssert(allNear(h,val,tol), AipsError);
      }
   }
}
