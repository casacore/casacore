//# ComponentImager.cc:  this defines ComponentImager which modifies images by ComponentLists
//# Copyright (C) 1999,2000
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

#include <trial/Images/ComponentImager.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Vector.h>
#include <aips/Containers/Block.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Mathematics/Math.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MeasRef.h>
#include <aips/Measures/Stokes.h>
#include <aips/Quanta/MVAngle.h>
#include <aips/Quanta/MVDirection.h>
#include <aips/Quanta/MVFrequency.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <trial/ComponentModels/ComponentList.h>
#include <trial/ComponentModels/SpectralModel.h>
#include <trial/ComponentModels/Flux.h>
#include <trial/Coordinates/Coordinate.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Coordinates/SpectralCoordinate.h>
#include <trial/Images/ImageInterface.h>
#include <aips/Lattices/LatticeIterator.h>
#include <aips/Lattices/LatticeStepper.h>

void ComponentImager::project(ImageInterface<Float>& image, const ComponentList& list) 
{
  const CoordinateSystem& coords = image.coordinates();
  const IPosition imageShape = image.shape();
  LogIO os(LogOrigin("ComponentImager", "project(...)", WHERE));
  
  // I currently REQUIRE that:
  // * The list has at least one element.
  // * The image has at least one pixel.
  // * The image has one direction coordinate (only).
  // * The direction coordinate has two pixel and two world axes.
  // * Polarization and frequency coordinates are optional, however at most one
  //   each of these coordinates can exist.
  // * If there is a Stokes axis it can only contain Stokes::I,Q,U,V pols.
  // * No other coordinate types, like LinearCoordinate, are used.
  uInt latAxis, longAxis;
  {
    const Vector<Int> dirAxes = CoordinateUtil::findDirectionAxes(coords);
    DebugAssert(dirAxes.nelements() == 2, AipsError);
    latAxis = dirAxes(0);
    longAxis = dirAxes(1);
  }
  DirectionCoordinate dirCoord = 
    coords.directionCoordinate(coords.findCoordinate(Coordinate::DIRECTION));
  DebugAssert(dirCoord.nPixelAxes() == 2, AipsError);
  DebugAssert(dirCoord.nWorldAxes() == 2, AipsError);
  dirCoord.setWorldAxisUnits(Vector<String>(2, "rad"));
  const MeasRef<MDirection> dirRef(dirCoord.directionType());
  MVAngle pixelLatSize, pixelLongSize;
  {
    const Vector<Double> inc = dirCoord.increment();
    pixelLatSize = MVAngle(abs(inc(0)));
    pixelLongSize = MVAngle(abs(inc(1)));
  }
  
  // Check if there is a Stokes Axes and if so which polarizations. Otherwise
  // only grid the I polarisation.
  uInt nStokes;
  Vector<Stokes::StokesTypes> stokes; 
  // Vector stating which polarisations are on each plane
  // Find which axis is the stokes pixel axis
  const Int polAxis = CoordinateUtil::findStokesAxis(stokes, coords);  
  if (polAxis >= 0) {
    nStokes = stokes.nelements();
    DebugAssert(static_cast<uInt>(imageShape(polAxis)) == nStokes, AipsError);
    for (uInt p = 0; p < nStokes; p++) {
      DebugAssert(stokes(p) == Stokes::I || stokes(p) == Stokes::Q ||
		  stokes(p) == Stokes::U || stokes(p) == Stokes::V, 
		  AipsError);
    }
  } else {
    nStokes = stokes.nelements();
  }

  // Check if there is a frequency axis and if so get the all the frequencies
  // as a Vector<MVFrequency>. Otherwise assume the reference frequency is the
  // same as the reference frequency of the first component in the list.
  MeasRef<MFrequency> freqRef;
  uInt nFreqs = 1;
  Vector<MVFrequency> freqValues(nFreqs);
  const Int freqAxis = CoordinateUtil::findSpectralAxis(coords);
  if (freqAxis >= 0) {
    nFreqs = static_cast<uInt>(imageShape(freqAxis));
    freqValues.resize(nFreqs);
    SpectralCoordinate specCoord = 
      coords.spectralCoordinate(coords.findCoordinate(Coordinate::SPECTRAL));
    specCoord.setWorldAxisUnits(Vector<String>(1, "Hz"));
    freqRef = MeasRef<MFrequency>(specCoord.frequencySystem());
    Double thisFreq;
    for (uInt f = 0; f < nFreqs; f++) {
      if (!specCoord.toWorld(thisFreq, static_cast<Double>(f))) {
	throw(AipsError("ComponentImager::project(...) - "
			"cannot convert a frequency value"));
      }
      freqValues(f) = MVFrequency(thisFreq);
    }
  } else {
    const MFrequency& defaultFreq = 
      list.component(0).spectrum().refFrequency();
    freqRef = defaultFreq.getRef();
    freqValues(0) = defaultFreq.getValue();
  }

  // Setup an iterator to step through the image in chunks that can fit into
  // memory. Go to a bit of effort to make the chunck size as large as
  // possible but still minimize the number of tiles in the cache.
  IPosition chunkShape = imageShape;
  {
    const IPosition tileShape = image.niceCursorShape();
    chunkShape(latAxis) = tileShape(latAxis);
    chunkShape(longAxis) = tileShape(longAxis);
  }
  IPosition pixelShape = imageShape;
  pixelShape(latAxis) = pixelShape(longAxis) = 1;
  LatticeStepper pixelStepper(imageShape, pixelShape, LatticeStepper::RESIZE);
  LatticeIterator<Float> chunkIter(image, chunkShape);
  const uInt nDirs = chunkShape(latAxis) * chunkShape(longAxis);
  Matrix<Flux<Double> > pixelVals(nDirs, nFreqs);
  Vector<MVDirection> dirVals(nDirs);
  Vector<Bool> coordIsGood(nDirs);
  const uInt naxis = imageShape.nelements();
  IPosition pixelPosition(naxis);
  Vector<Double> pixelDir(2);
  Vector<Double> worldDir(2);
  uInt d;

// Does the image have a writable mask ?  Output pixel values are
// only modified if the mask==T  and the coordinate conversions
// succeeded.  The mask==F on output if the coordinate conversion 
// fails (usually means a pixel is outside of the valid CoordinateSystem)
// 
  Bool doMask = False;
  if (image.isMasked() && image.hasPixelMask()) {
    if (image.pixelMask().isWritable()) {
      doMask = True;
    } else {
       os << LogIO::WARN << "The image is masked, but it cannot be written to" << LogIO::POST;
    }
  }
  Lattice<Bool>* pixelMaskPtr = 0;
  if (doMask) pixelMaskPtr = &image.pixelMask();
  Array<Bool>* maskPtr = 0;
//
  for (chunkIter.reset(); !chunkIter.atEnd(); chunkIter++) {

// Iterate through sky plane of cursor and do coordinate conversions

    pixelStepper.subSection(chunkIter.position(), chunkIter.endPosition());
    for (pixelStepper.reset(), d=0; !pixelStepper.atEnd(); pixelStepper++, d++) {
      pixelPosition = pixelStepper.position();
      pixelDir(0) = pixelPosition(latAxis);
      pixelDir(1) = pixelPosition(longAxis);
      if (!dirCoord.toWorld(worldDir, pixelDir)) {

// These pixels will be masked

	coordIsGood(d) = False;
      } else {
	dirVals(d) = MVDirection(worldDir(0), worldDir(1));
	coordIsGood(d) = True;
      }
    }

// Sample model

    list.sample(pixelVals, dirVals, dirRef, pixelLatSize, 
		pixelLongSize, freqValues, freqRef);

// Modify data by model for this chunk of data

    Array<Float>& imageChunk = chunkIter.rwCursor();

// Get input mask values if available

    if (doMask) {
       maskPtr = new Array<Bool>(image.getMaskSlice(chunkIter.position(),
                                 chunkIter.cursorShape(), False));
    }
//
    for (pixelStepper.reset(),d=0; !pixelStepper.atEnd(); pixelStepper++,d++) {
      pixelPosition = pixelStepper.relativePosition();
      if (coordIsGood(d)) {
        for (uInt f = 0; f < nFreqs; f++) {
          if (freqAxis >= 0) pixelPosition(freqAxis) = f;
          const Flux<Double>& thisFlux = pixelVals(d, f);
          for (uInt s = 0; s < nStokes; s++) {
            if (polAxis >= 0) pixelPosition(polAxis) = s;
//
            if (doMask) {
               if ((*maskPtr)(pixelPosition)) {
                 imageChunk(pixelPosition) += 
                   static_cast<Float>(thisFlux.value(s).real());
               }
            } else {
              imageChunk(pixelPosition) += 
                 static_cast<Float>(thisFlux.value(s).real());
            }
          }
        }
      } else {
        if (doMask) (*maskPtr)(pixelPosition) = False;
      }
    }

// Update output mask in approprate fashion

    if (doMask) {
       pixelMaskPtr->putSlice(*maskPtr, chunkIter.position());
       if (maskPtr!=0) delete maskPtr;
    }
  }
}
// Local Variables: 
// compile-command: "gmake ComponentImager"
// End: 
