//# tLatticeConvolver.cc:
//# Copyright (C) 1997,1998,1999,2000,2001
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

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>
//#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/scimath/Mathematics/NumericTraits.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/lattices/LatticeMath/LatticeConvolver.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/lattices/Lattices/PagedArray.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
void print(const Lattice<Float> & psf, const Lattice<Float> & model,
	   const Lattice<Float> & result) {
  cout << "Psf: " << psf.get() << endl;
  cout << "Model: " << model.get() << endl;
  cout << "Result: " << result.get() << endl;
}

int main() {
  try {
    { // test linear and circular convolution using the default psf.
      LatticeConvolver<Float> d;
      AlwaysAssert(d.shape() == IPosition(1,1), AipsError);
      AlwaysAssert(d.fftShape() == IPosition(1,1) , AipsError);
      AlwaysAssert(d.psfShape() == IPosition(1,1) , AipsError);
      AlwaysAssert(d.type() == ConvEnums::CIRCULAR , AipsError);
      TempLattice<Float> model(IPosition(1,9));
      model.set(1.0f);
      model.putAt(2.0f, model.shape()/2);
      TempLattice<Float> result(model.shape());
      d.circular(result, model);
      AlwaysAssert(allNearAbs(model.get(), result.get(), 
			      NumericTraits<Float>::epsilon), AipsError);
      AlwaysAssert(d.shape() == IPosition(1,9), AipsError);
      AlwaysAssert(d.fftShape() == IPosition(1,9) , AipsError);
      AlwaysAssert(d.psfShape() == IPosition(1,1) , AipsError);
      AlwaysAssert(d.type() == ConvEnums::CIRCULAR , AipsError);

      result.set(0.0f);
      d.linear(result, model);
      AlwaysAssert(allNear(model.get(), result.get(),
			   NumericTraits<Float>::epsilon), AipsError);
      AlwaysAssert(d.shape() == IPosition(1,9), AipsError);
      AlwaysAssert(d.fftShape() == IPosition(1,9) , AipsError);
      AlwaysAssert(d.psfShape() == IPosition(1,1) , AipsError);
      AlwaysAssert(d.type() == ConvEnums::LINEAR , AipsError);
    }
    { 
      TempLattice<Float> psf(IPosition(4,16,5,1,9));
      psf.set(0.0f);
      psf.putAt(1.0f, psf.shape()/2);
      const LatticeConvolver<Float> c(psf);
      AlwaysAssert(c.shape() == psf.shape(), AipsError);
      AlwaysAssert(c.fftShape() == psf.shape(), AipsError);
      AlwaysAssert(c.psfShape() == psf.shape(), AipsError);
      AlwaysAssert(c.type() == ConvEnums::CIRCULAR , AipsError);
      TempLattice<Float> extractedPsf(psf.shape());
      // test the getPsf function (tests the FFT's but not padding)
      c.getPsf(extractedPsf);
      AlwaysAssert(allNear(extractedPsf.get(), psf.get(),
			   NumericTraits<Float>::epsilon), AipsError);
    }
    {
      // test 1-D convolution with a large variety of model/psf shapes
      const IPosition evenPsfShape(1,10);
      TempLattice<Float> evenPsf1D(evenPsfShape);
      evenPsf1D.set(0.0f);
      evenPsf1D.putAt(0.2f, evenPsfShape*0);
      evenPsf1D.putAt(0.5f, evenPsfShape/2-1);
      evenPsf1D.putAt(1.0f, evenPsfShape/2);
      evenPsf1D.putAt(0.3f, evenPsfShape/2+1);
      evenPsf1D.putAt(0.1f, evenPsfShape-1);
      {
	// psfShape = 10, imageShape = 4, linear
	IPosition imageShape(1,4);
	LatticeConvolver<Float> c(evenPsf1D, imageShape);
	AlwaysAssert(c.shape() == imageShape, AipsError);
	AlwaysAssert(c.fftShape() == IPosition(1,7) , AipsError);
	AlwaysAssert(c.psfShape() == evenPsfShape , AipsError);
	AlwaysAssert(c.type() == ConvEnums::LINEAR , AipsError);
	{
	  TempLattice<Float> model(imageShape);
	  model.set(0.0);
	  model.putAt(2.0, IPosition(1,0));
	  model.putAt(5.0, model.shape()-1);
	  TempLattice<Float> result(model.shape());
	  c.linear(result, model);
	  AlwaysAssert(near(result(IPosition(1,0)), 2.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,1)), 0.6f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,2)), 2.5f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,3)), 5.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  TempLattice<Float> psf(evenPsfShape);
	  c.getPsf(psf);
	  Array<Float> psfArr = psf.get();
	  AlwaysAssert(allNear(psf.get(), evenPsf1D.get(),
			    NumericTraits<Float>::epsilon), AipsError);
	  // psfShape = 10, imageShape = 4, circular
	  model.set(0.0);
	  model.putAt(2.0, IPosition(1,0));
	  model.putAt(5.0, model.shape()-1);
	  c.circular(result, model);
	  AlwaysAssert(c.shape() == imageShape, AipsError);
	  AlwaysAssert(c.fftShape() == IPosition(1,10) , AipsError);
	  AlwaysAssert(c.psfShape() == evenPsfShape , AipsError);
	  AlwaysAssert(c.type() == ConvEnums::CIRCULAR , AipsError);
	  AlwaysAssert(near(result(IPosition(1,0)), 2.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,1)), 0.6f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,2)), 2.5f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,3)), 5.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	}
	// psfShape = 10, imageShape = 5, linear
	imageShape = IPosition(1,5);
	c.resize(imageShape, ConvEnums::LINEAR);
	AlwaysAssert(c.shape() == imageShape, AipsError);
	AlwaysAssert(c.fftShape() == IPosition(1,10) , AipsError);
	AlwaysAssert(c.psfShape() == evenPsfShape , AipsError);
	AlwaysAssert(c.type() == ConvEnums::LINEAR , AipsError);
	{
	  TempLattice<Float> model(imageShape);
	  model.set(0.0);
	  model.putAt(2.0, IPosition(1,0));
	  model.putAt(5.0, model.shape()-1);
	  TempLattice<Float> result(model.shape());
	  c.linear(result, model);
	  AlwaysAssert(near(result(IPosition(1,0)), 2.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,1)), 0.6f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,2)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,3)), 2.5f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,4)), 5.2f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  // psfShape = 10, imageShape = 5, circular
	  model.set(0.0);
	  model.putAt(2.0, IPosition(1,0));
	  model.putAt(5.0, model.shape()-1);
	  c.circular(result, model);
	  AlwaysAssert(c.shape() == imageShape, AipsError);
	  AlwaysAssert(c.fftShape() == IPosition(1,10) , AipsError);
	  AlwaysAssert(c.psfShape() == evenPsfShape , AipsError);
	  AlwaysAssert(c.type() == ConvEnums::CIRCULAR , AipsError);
	  AlwaysAssert(near(result(IPosition(1,0)), 2.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,1)), 0.6f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,2)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,3)), 2.5f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,4)), 5.2f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	}
	// psfShape = 10, imageShape = 6, linear
	imageShape = IPosition(1,6);
	c.resize(imageShape, ConvEnums::LINEAR);
	AlwaysAssert(c.shape() == imageShape, AipsError);
	AlwaysAssert(c.fftShape() == IPosition(1,11) , AipsError);
	AlwaysAssert(c.psfShape() == evenPsfShape , AipsError);
	AlwaysAssert(c.type() == ConvEnums::LINEAR , AipsError);
	{
	  TempLattice<Float> model(imageShape);
	  model.set(0.0);
	  model.putAt(2.0, IPosition(1,0));
	  model.putAt(5.0, model.shape()-1);
	  TempLattice<Float> result(model.shape());
	  c.linear(result, model);
	  AlwaysAssert(near(result(IPosition(1,0)), 3.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,1)), 0.6f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,2)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,3)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,4)), 2.7f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,5)), 5.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	}
	// psfShape = 10, imageShape = 10, linear
	imageShape = IPosition(1,10);
	c.resize(imageShape, ConvEnums::LINEAR);
	AlwaysAssert(c.shape() == imageShape, AipsError);
	AlwaysAssert(c.fftShape() == IPosition(1,15) , AipsError);
	AlwaysAssert(c.psfShape() == evenPsfShape , AipsError);
	AlwaysAssert(c.type() == ConvEnums::LINEAR , AipsError);
 	{
	  TempLattice<Float> result(imageShape);
	  result.set(0.0);
	  result.putAt(2.0, IPosition(1,0));
	  result.putAt(5.0, result.shape()-1);
	  c.linear(result);
	  AlwaysAssert(near(result(IPosition(1,0)), 2.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,1)), 0.6f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,2)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,3)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,4)), 1.2f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,5)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,6)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,7)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,8)), 2.5f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,9)), 5.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
 	}
	// psfShape = 10, imageShape = 11, linear
	imageShape = IPosition(1,11);
	c.resize(imageShape, ConvEnums::LINEAR);
	AlwaysAssert(c.shape() == imageShape, AipsError);
	AlwaysAssert(c.fftShape() == IPosition(1,16) , AipsError);
	AlwaysAssert(c.psfShape() == evenPsfShape, AipsError);
	AlwaysAssert(c.type() == ConvEnums::LINEAR, AipsError);
 	{
	  TempLattice<Float> result(imageShape);
	  result.set(0.0);
	  result.putAt(2.0, IPosition(1,0));
	  result.putAt(5.0, result.shape()-1);
	  c.linear(result);
	  AlwaysAssert(near(result(IPosition(1,0)), 2.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,1)), 0.6f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,2)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,3)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,4)), 0.2f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,5)), 1.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,6)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,7)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,8)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,9)), 2.5f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,10)), 5.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
 	}
	// psfShape = 10, imageShape = 9, linear
	imageShape = IPosition(1,9);
	c.resize(imageShape, ConvEnums::LINEAR);
	AlwaysAssert(c.shape() == imageShape, AipsError);
	AlwaysAssert(c.fftShape() == IPosition(1,14) , AipsError);
	AlwaysAssert(c.psfShape() == evenPsfShape , AipsError);
	AlwaysAssert(c.type() == ConvEnums::LINEAR , AipsError);
 	{
	  TempLattice<Float> result(imageShape);
	  result.set(0.0);
	  result.putAt(2.0, IPosition(1,0));
	  result.putAt(5.0, result.shape()-1);
	  c.linear(result);
	  AlwaysAssert(near(result(IPosition(1,0)), 2.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,1)), 0.6f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,2)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,3)), 1.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,4)), 0.2f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,5)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,6)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,7)), 2.5f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,8)), 5.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
 	}
      }
      const IPosition oddPsfShape(1,11);
      TempLattice<Float> oddPsf1D(oddPsfShape);
      oddPsf1D.set(0.0f);
      oddPsf1D.putAt(0.2f, oddPsfShape*0);
      oddPsf1D.putAt(0.5f, oddPsfShape/2-1);
      oddPsf1D.putAt(1.0f, oddPsfShape/2);
      oddPsf1D.putAt(0.3f, oddPsfShape/2+1);
      oddPsf1D.putAt(0.1f, oddPsfShape-1);
      {
	// psfShape = 11, imageShape = 4, linear
 	IPosition imageShape(1,4);
	LatticeConvolver<Float> c(oddPsf1D, imageShape);
 	AlwaysAssert(c.shape() == imageShape, AipsError);
 	AlwaysAssert(c.fftShape() == IPosition(1,7) , AipsError);
 	AlwaysAssert(c.psfShape() == oddPsfShape , AipsError);
 	AlwaysAssert(c.type() == ConvEnums::LINEAR , AipsError);
 	{
	  TempLattice<Float> result(imageShape);
	  result.set(0.0);
	  result.putAt(2.0, IPosition(1,0));
	  result.putAt(5.0, result.shape()-1);
	  c.linear(result);
	  AlwaysAssert(near(result(IPosition(1,0)), 2.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,1)), 0.6f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,2)), 2.5f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,3)), 5.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
 	}
	// psfShape = 11, imageShape = 5, linear
	imageShape = IPosition(1,5);
	c.resize(imageShape, ConvEnums::LINEAR);
	AlwaysAssert(c.shape() == imageShape, AipsError);
	AlwaysAssert(c.fftShape() == IPosition(1,9) , AipsError);
	AlwaysAssert(c.psfShape() == oddPsfShape , AipsError);
	AlwaysAssert(c.type() == ConvEnums::LINEAR , AipsError);
	{
	  TempLattice<Float> result(imageShape);
	  result.set(0.0);
	  result.putAt(2.0, IPosition(1,0));
	  result.putAt(5.0, result.shape()-1);
	  c.linear(result);
	  AlwaysAssert(near(result(IPosition(1,0)), 2.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,1)), 0.6f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,2)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,3)), 2.5f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,4)), 5.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	}
	// psfShape = 11, imageShape = 6, linear
	imageShape = IPosition(1,6);
	c.resize(imageShape, ConvEnums::LINEAR);
	AlwaysAssert(c.shape() == imageShape, AipsError);
	AlwaysAssert(c.fftShape() == IPosition(1,11) , AipsError);
	AlwaysAssert(c.psfShape() == oddPsfShape , AipsError);
	AlwaysAssert(c.type() == ConvEnums::LINEAR , AipsError);
	{
	  TempLattice<Float> result(imageShape);
	  result.set(0.0);
	  result.putAt(2.0, IPosition(1,0));
	  result.putAt(5.0, result.shape()-1);
	  c.linear(result);
	  AlwaysAssert(near(result(IPosition(1,0)), 3.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,1)), 0.6f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,2)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,3)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,4)), 2.5f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,5)), 5.2f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	}
	// psfShape = 11, imageShape = 11, linear
	imageShape = IPosition(1,11);
	c.resize(imageShape, ConvEnums::LINEAR);
	AlwaysAssert(c.shape() == imageShape, AipsError);
	AlwaysAssert(c.fftShape() == IPosition(1,16) , AipsError);
	AlwaysAssert(c.psfShape() == oddPsfShape , AipsError);
	AlwaysAssert(c.type() == ConvEnums::LINEAR , AipsError);
 	{
	  TempLattice<Float> result(imageShape);
	  result.set(0.0);
	  result.putAt(2.0, IPosition(1,0));
	  result.putAt(5.0, result.shape()-1);
	  c.linear(result);
	  AlwaysAssert(near(result(IPosition(1,0)), 2.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,1)), 0.6f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,2)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,3)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,4)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,5)), 1.2f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,6)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,7)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,8)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,9)), 2.5f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,10)), 5.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
 	}
	// psfShape = 11, imageShape = 12, linear
	imageShape = IPosition(1,12);
	c.resize(imageShape, ConvEnums::LINEAR);
	AlwaysAssert(c.shape() == imageShape, AipsError);
	AlwaysAssert(c.fftShape() == IPosition(1,17) , AipsError);
	AlwaysAssert(c.psfShape() == oddPsfShape, AipsError);
	AlwaysAssert(c.type() == ConvEnums::LINEAR, AipsError);
 	{
	  TempLattice<Float> result(imageShape);
	  result.set(0.0);
	  result.putAt(2.0, IPosition(1,0));
	  result.putAt(5.0, result.shape()-1);
	  c.linear(result);
	  AlwaysAssert(near(result(IPosition(1,0)), 2.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,1)), 0.6f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,2)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,3)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,4)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,5)), 0.2f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,6)), 1.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,7)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,8)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,9)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,10)), 2.5f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,11)), 5.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
 	}
	// psfShape = 11, imageShape = 10, linear
	imageShape = IPosition(1,10);
	c.resize(imageShape, ConvEnums::LINEAR);
	AlwaysAssert(c.shape() == imageShape, AipsError);
	AlwaysAssert(c.fftShape() == IPosition(1,15) , AipsError);
	AlwaysAssert(c.psfShape() == oddPsfShape , AipsError);
	AlwaysAssert(c.type() == ConvEnums::LINEAR , AipsError);
 	{
	  TempLattice<Float> result(imageShape);
	  result.set(0.0);
	  result.putAt(2.0, IPosition(1,0));
	  result.putAt(5.0, result.shape()-1);
	  c.linear(result);
	  //	  print(oddPsf1D, result, result);
	  AlwaysAssert(near(result(IPosition(1,0)), 2.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,1)), 0.6f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,2)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,3)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,4)), 1.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,5)), 0.2f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,6)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(nearAbs(result(IPosition(1,7)), 0.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,8)), 2.5f,
			    10*NumericTraits<Float>::epsilon), AipsError);
	  AlwaysAssert(near(result(IPosition(1,9)), 5.0f,
			    10*NumericTraits<Float>::epsilon), AipsError);
 	}
      }
//       {
// 	TempLattice<Float> psf1D(IPosition(1,4));
// 	psf1D.set(0.0f);
// 	psf1D.putAt(0.1f, psf1D.shape()/2-2);
// 	psf1D.putAt(0.5f, psf1D.shape()/2-1);
// 	psf1D.putAt(1.0f, psf1D.shape()/2);
// 	psf1D.putAt(0.3f, psf1D.shape()/2+1);
  
// 	TempLattice<Float> model(IPosition(1,7));
// 	model.set(0.0);
// 	model.putAt(2.0, IPosition(1,0));
// 	model.putAt(5.0, model.shape()-1);
  
// 	const IPosition imageShape = model.shape();
// 	LatticeConvolver<Float> c(psf1D, imageShape);
// 	{
// 	  TempLattice<Float> result(model.shape());
// 	  c.linear(result, model);
// 	  AlwaysAssert(result.shape() == model.shape(), AipsError);
// 	  AlwaysAssert(near(result(IPosition(1,0)), 2.0f, 10*NumericTraits<Float>::epsilon), AipsError);
// 	  AlwaysAssert(near(result(IPosition(1,1)), 0.6f, 10*NumericTraits<Float>::epsilon), AipsError);
// 	  AlwaysAssert(nearAbs(result(IPosition(1,2)), 0.0f, 10*NumericTraits<Float>::epsilon), AipsError);
// 	  AlwaysAssert(nearAbs(result(IPosition(1,3)), 0.0f, 10*NumericTraits<Float>::epsilon), AipsError);
// 	  AlwaysAssert(nearAbs(result(IPosition(1,4)), 0.0f, 10*NumericTraits<Float>::epsilon), AipsError);
// 	  AlwaysAssert(near(result(IPosition(1,5)), 2.5f, 10*NumericTraits<Float>::epsilon), AipsError);
// 	  AlwaysAssert(near(result(IPosition(1,6)), 5.0f, 10*NumericTraits<Float>::epsilon), AipsError);
// 	}
//  	{
//  	  const IPosition resultShape(model.shape()*2);
//  	  TempLattice<Float> result(resultShape);
//  	  c.linear(result, model);
// 	  AlwaysAssert(result.shape() == resultShape, AipsError);
// 	  AlwaysAssert(near(result(IPosition(1,4)), 2.0f, 10*NumericTraits<Float>::epsilon), AipsError);
// 	  AlwaysAssert(near(result(IPosition(1,5)), 0.6f, 10*NumericTraits<Float>::epsilon), AipsError);
// 	  AlwaysAssert(nearAbs(result(IPosition(1,6)), 0.0f, 10*NumericTraits<Float>::epsilon), AipsError);
// 	  AlwaysAssert(nearAbs(result(IPosition(1,7)), 0.0f, 10*NumericTraits<Float>::epsilon), AipsError);
// 	  AlwaysAssert(nearAbs(result(IPosition(1,8)), 0.0f, 10*NumericTraits<Float>::epsilon), AipsError);
// 	  AlwaysAssert(near(result(IPosition(1,9)), 2.5f, 10*NumericTraits<Float>::epsilon), AipsError);
// 	  AlwaysAssert(near(result(IPosition(1,10)), 5.0f, 10*NumericTraits<Float>::epsilon), AipsError);
// 	}
// 	{
// 	  const IPosition resultShape(model.shape()*2-1);
// 	  TempLattice<Float> result(resultShape);
// 	  c.linear(result, model);
// 	  AlwaysAssert(result.shape() == resultShape, AipsError);
// 	  AlwaysAssert(near(result(IPosition(1,4)), 2.0f, 10*NumericTraits<Float>::epsilon), AipsError);
// 	  AlwaysAssert(near(result(IPosition(1,5)), 0.6f, 10*NumericTraits<Float>::epsilon), AipsError);
// 	  AlwaysAssert(nearAbs(result(IPosition(1,6)), 0.0f, 10*NumericTraits<Float>::epsilon), AipsError);
// 	  AlwaysAssert(nearAbs(result(IPosition(1,7)), 0.0f, 10*NumericTraits<Float>::epsilon), AipsError);
// 	  AlwaysAssert(nearAbs(result(IPosition(1,8)), 0.0f, 10*NumericTraits<Float>::epsilon), AipsError);
// 	  AlwaysAssert(near(result(IPosition(1,9)), 2.5f, 10*NumericTraits<Float>::epsilon), AipsError);
// 	  AlwaysAssert(near(result(IPosition(1,10)), 5.0f, 10*NumericTraits<Float>::epsilon), AipsError);
// 	}
//       }
//       {
// 	TempLattice<Float> psf2D(IPosition(2,3,3));
// 	psf2D.set(0.0f);
// 	IPosition centre = psf2D.shape()/2;
// 	psf2D.putAt(1.0f, centre);
// 	centre(0) -= 1;
// 	psf2D.putAt(0.5f, centre);
// 	centre(0) += 2;
// 	psf2D.putAt(0.4f, centre);
// 	centre = psf2D.shape()/2; centre(1) -= 1;
// 	psf2D.putAt(0.2f, centre);
// 	centre(1) += 2;
// 	psf2D.putAt(0.1f, centre);
// 	Array<Float> psfArray;
// 	psf2D.getSlice(psfArray, IPosition(2,0), psf2D.shape());
// 	cout << "psf = " << psfArray << endl;

// 	TempLattice<Float> model(IPosition(2,7,6));
// 	model.set(0.0);
// 	model.putAt(2.0, IPosition(2,0));
// 	model.putAt(5.0, model.shape()-1);
// 	Array<Float> modelArray;
// 	model.getSlice(modelArray, IPosition(2,0), model.shape());
// 	cout << "model = " << modelArray << endl;

// 	const IPosition imageShape = model.shape();
// 	LatticeConvolver<Float> c(psf2D, imageShape);

// 	TempLattice<Float> result(model.shape());
// 	c.linear(result, model);

// 	Array<Float> resultArray;
// 	result.getSlice(resultArray, IPosition(2,0), result.shape());
// 	cout << "result = " << resultArray << endl;
//       }
      }
  } catch (AipsError x) {
    cout<< "FAIL"<< endl;
    cerr << x.getMesg() << endl;
    return 1;
  } 
  cout<< "OK"<< endl;
  return 0;
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 tLatticeConvolver"
// End: 
