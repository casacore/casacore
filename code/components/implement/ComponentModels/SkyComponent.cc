//# SkyComponent.cc:  this defines SkyComponent
//# Copyright (C) 1996,1997
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

#include <trial/ComponentModels/SkyComponent.h>
#include <aips/Measures/MVDirection.h>
#include <trial/Images/ImageInterface.h>
#include <aips/Arrays/Vector.h>
#include <aips/Containers/Block.h>
#include <trial/Lattices/LatticeIterator.h>
#include <aips/Mathematics/Convolver.h>
#include <aips/Utilities/COWPtr.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Utilities/Assert.h>

SkyComponent::~SkyComponent() {}

// This code will be rewritten when Brian's new
// co-ordinates class is incorporated into images.
void SkyComponent::operator()(ImageInterface<Float>& image) const {
  CoordinateSystem coords = image.coordinates();
  IPosition shape = image.shape();
  uInt naxis = shape.nelements();

  // For the moment I will assume that the first two dimensions of an image
  // are the RA and DEC. I should actually check which axes these are. This
  // task is relagated until the new coords are available. I should also
  // check that the polarization axis has length of 4 or less. And that an
  // RA and a DEC axis can be found (otherwise throw an error). Currently
  // the image must have at least three dimensions (ra, dec and pol). The
  // Frequency axis is optional. 

  uInt raAxis = 0;
  uInt decAxis = 1;
  uInt polAxis = 2;

  uInt raLen = shape(raAxis);
  uInt npol = 1;
  if (polAxis != 0)
    npol = shape(polAxis);
  IPosition iterShape(naxis);
  iterShape = 1; iterShape(raAxis) = raLen;
  if (npol != 1)
    iterShape(polAxis) = npol;
  LatticeIterator<Float> iter(image, iterShape);
  Matrix<Float> slab(raLen, npol); 
  Vector<Float> pixVal(npol);
  Vector<Double> pixelPos(naxis), worldPos(naxis);
  uInt i;
  MDirection skyPos;
  StokesVector flux;

  while (! iter.atEnd() ) {
    convertArray(pixelPos.ac(), iter.position().asVector().ac());
    pixelPos.ac() += 1.0; 
    slab.reference(iter.cursor());
    for (i = 0; i < raLen; i++){
      AlwaysAssert(coords.toWorld(worldPos, pixelPos), AipsError);
      // Try to avoid calling a constructor here
      skyPos.set(MVDirection(worldPos(raAxis), worldPos(decAxis)));
      flux = this->operator()(skyPos);
      pixVal.reference(slab.row(i).ac());
      switch (npol) {
      case 4:
	pixVal.ac() += flux.vector().ac();
	break;
      case 3:
	pixVal(2) += flux(2);
      case 2:
	pixVal(1) += flux(1);
      case 1:
	pixVal(0) += flux(0);
      }
      pixelPos(raAxis)++; 
    }
    iter++;
  }
}

void SkyComponent::operator()(ImageInterface<Float>& image, 
			      const ImageInterface<Float>& psf) const {
  this->operator()(image);
  // Start up a convolver and convolve the image with the psf.  Currently
  // the image and the psf should have the same co-ordinate spacings (This
  // means the deltas should be the same). However this is not checked

  // Also the dimension of the psf MUST be the same or smaller than the
  // dimension of the image. The sizes do not need to be the same. I should also not assume that the RA and Dec axis are the first axies. 
  IPosition psfShape = psf.shape();
  uInt psfDim = psfShape.nelements();
  uInt psfNdDim = psfShape.nonDegenerate().nelements();
  IPosition imageShape = image.shape();
  IPosition convShape = imageShape.getFirst(psfNdDim);
  Convolver<Float> convolver;
  {
    COWPtr< Array<Float> > ptrPsfArray(new Array<Float>);
    Bool isCopy;
    isCopy = psf.getSlice(ptrPsfArray, IPosition(psfDim, 0), psfShape, 
 			  IPosition(psfDim, 1));
    convolver.setPsf(*ptrPsfArray, convShape);
  }
  // To avoid using too much memory I will not use the internal iterator
  // of the convolver class and will iterate through the image using a
  // latticeIterator
  LatticeIterator<Float> iter(image, convShape); 
  for (iter.reset(); !iter.atEnd(); iter++) {
     Array<Float> section(iter.cursor());
     convolver.linearConv(section, section);
  }
}

Vector<StokesVector> SkyComponent::operator()(const Vector<MDirection> &
					     samplePos) const
{
  uInt nsamples = samplePos.nelements();
  Vector<StokesVector> results(nsamples);
  for (uInt i = 0; i < nsamples; i++)
    results(i) = this->operator()(samplePos(i));
  return results;
}
