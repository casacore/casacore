//# ClassFileName.cc:  this defines ClassName, which ...
//# Copyright (C) 1999
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

#include <trial/ComponentModels/ComponentImager.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Vector.h>
#include <aips/Containers/Block.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/Stokes.h>
#include <aips/Quanta/MVAngle.h>
#include <aips/Quanta/MVDirection.h>
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
#include <trial/Images/ImageInterface.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/ArrayLattice.h>

void ComponentImager::
project(ImageInterface<Float>& image, const ComponentList& list) {
  const CoordinateSystem coords = image.coordinates();
  const IPosition imageShape = image.shape();
  const uInt naxis = imageShape.nelements();
  
  // I currently REQUIRE that the image has one direction coordinate (only).
  // All other coordinates (ie. polarization and frequency) are optional. 
  const Vector<Int> dirAxes = CoordinateUtil::findDirectionAxes(coords);
  if (dirAxes.nelements() == 0) {
    throw(AipsError("ComponentImager::project(...) - The supplied image" 
		    " does not have any direction coordinates"));
  }
  const uInt nPixAxes = dirAxes.nelements();
  Vector<Double> pixelCoord(nPixAxes); pixelCoord = 0.0;
  Vector<Double> worldCoord(2);

  const DirectionCoordinate& dirCoord = 
    coords.directionCoordinate(coords.findCoordinate(Coordinate::DIRECTION));
  MDirection pixelDir(MVDirection(0.0), dirCoord.directionType());
  Vector<Quantum<Double> > dirVal(2);
  MVAngle pixelSize;
  {
    Vector<String> units = dirCoord.worldAxisUnits();
    dirVal(0).setUnit(units(0));
    dirVal(1).setUnit(units(1));
    Vector<Double> inc = dirCoord.increment();
    Quantum<Double> inc0(abs(inc(0)), units(0));
    Quantum<Double> inc1(abs(inc(1)), units(1));
    AlwaysAssert(near(inc0.getValue("rad"), inc1.getValue("rad")), AipsError);
    pixelSize = MVAngle(inc0);
  }
  
  // Setup an iterator to step through the image in chunks that can fit into
  // memory. Go to a bit of effort to make the chunck size as large as
  // possible but still minimize the number of tiles in the cache.
  IPosition elementShape = imageShape;
  IPosition chunckShape = imageShape;
  uInt axis;
  {
    const IPosition tileShape(image.niceCursorShape());
    for (uInt k = 0; k < nPixAxes; k++) {
      axis = dirAxes(k);
      elementShape(axis) = 1;
      chunckShape(axis) = tileShape(axis);
    }
  }

  // Check if there is a Stokes Axes and if so which polarizations. Otherwise
  // only grid the I polarisation.
  Vector<Stokes::StokesTypes> stokes; 
  // Vector stating which polarisations are on each plane
  // Find which axis is the stokes pixel axis
  const Int polAxis = CoordinateUtil::findStokesAxis(stokes, coords);  
  const uInt nStokes = stokes.nelements(); 
  if (polAxis >= 0) {
    AlwaysAssert(imageShape(polAxis) == Int(nStokes), AipsError);
  }
  for (uInt p = 0; p < nStokes; p++)
    AlwaysAssert(stokes(p) == Stokes::I || stokes(p) == Stokes::Q ||
 		 stokes(p) == Stokes::U || stokes(p) == Stokes::V, 
 		 AipsError);

  Block<IPosition> blc;
  Block<IPosition> trc;
  if (nStokes > 1) {
    blc.resize(nStokes);
    blc = IPosition(naxis,0);
    trc.resize(nStokes);
    trc = elementShape - 1;
    for (uInt p = 0; p < nStokes; p++) {
      blc[p](polAxis) = p;
      trc[p](polAxis) = p;
    }
  }

  const MFrequency refFreq = list.component(0).spectrum().refFrequency();
  LatticeIterator<Float> chunkIter(image, chunckShape);
  Flux<Double> pixelVal;
  IPosition chunkOrigin(naxis), elementPosition(naxis);
  for (chunkIter.reset(); !chunkIter.atEnd(); chunkIter++) {
    ArrayLattice<Float> array(chunkIter.rwCursor());
    LatticeIterator<Float> elementIter(array, elementShape);
    chunkOrigin = chunkIter.position();
    for (elementIter.reset(); !elementIter.atEnd(); elementIter++) {
      elementPosition = elementIter.position();
      for (uInt k = 0; k < nPixAxes; k++) {
 	axis = dirAxes(k);
 	pixelCoord(k) = elementPosition(axis) + chunkOrigin(axis);
      }
      if (!dirCoord.toWorld(worldCoord, pixelCoord)) {
// I am not sure what to do here, probably this message should be logged.
  	cerr << "ComponentImager::Pixel at " << pixelCoord 
  	     << " cannot be projected" << endl;
      }
      else {
 	dirVal(0).setValue(worldCoord(0));
 	dirVal(1).setValue(worldCoord(1));
 	pixelDir.set(MVDirection(dirVal));
	pixelVal = list.sample(pixelDir, pixelSize, refFreq);
	pixelVal.convertPol(ComponentType::STOKES);
 	if (nStokes == 1) {
	  switch (stokes(0)) {
	  case Stokes::I:
	    elementIter.rwCursor() += Float(pixelVal.value(0).real()); break;
	  case Stokes::Q:
	    elementIter.rwCursor() += Float(pixelVal.value(1).real()); break;
	  case Stokes::U:
	    elementIter.rwCursor() += Float(pixelVal.value(2).real()); break;
	  case Stokes::V:
	    elementIter.rwCursor() += Float(pixelVal.value(3).real()); break;
	  }
 	}
	else if (elementShape.product() == Int(nStokes))
	  for (uInt p = 0; p < nStokes; p++) {
	    switch (stokes(p)) {
	    case Stokes::I:
	      elementIter.rwCursor()(blc[p]) += Float(pixelVal.value(0).real()); break;
	    case Stokes::Q:
	      elementIter.rwCursor()(blc[p]) += Float(pixelVal.value(1).real()); break;
	    case Stokes::U:
	      elementIter.rwCursor()(blc[p]) += Float(pixelVal.value(2).real()); break;
	    case Stokes::V:
	      elementIter.rwCursor()(blc[p]) += Float(pixelVal.value(3).real()); break;
	    }
	  }
	else
	for (uInt p = 0; p < nStokes; p++) {
	  switch (stokes(p)) {
	  case Stokes::I:
	    elementIter.rwCursor()(blc[p], trc[p]) += Float(pixelVal.value(0).real());
	    break;
	  case Stokes::Q:
	    elementIter.rwCursor()(blc[p], trc[p]) += Float(pixelVal.value(1).real());
	    break;
	  case Stokes::U:
	    elementIter.rwCursor()(blc[p], trc[p]) += Float(pixelVal.value(2).real());
	    break;
	  case Stokes::V:
	    elementIter.rwCursor()(blc[p], trc[p]) += Float(pixelVal.value(3).real());
	    break;
	  }
	}
      }
    }
  }
}

// Local Variables: 
// compile-command: "gmake ComponentImager"
// End: 
