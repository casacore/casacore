//# ImageRegrid.cc: Regrids images
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002
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

#ifndef IMAGES_IMAGEREGRID_TCC
#define IMAGES_IMAGEREGRID_TCC

#include <casacore/images/Images/ImageRegrid.h>

#include <casacore/casa/Arrays/ArrayAccessor.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/coordinates/Coordinates/DirectionCoordinate.h>
#include <casacore/coordinates/Coordinates/LinearCoordinate.h>
#include <casacore/coordinates/Coordinates/SpectralCoordinate.h>
#include <casacore/images/Images/SubImage.h>
#include <casacore/images/Images/TempImage.h>
#include <casacore/lattices/Lattices/LatticeUtilities.h>
#include <casacore/measures/Measures/MCDirection.h>
#include <casacore/measures/Measures/MCFrequency.h>
#include <casacore/scimath/Mathematics/InterpolateArray1D.h>
#include <casacore/casa/System/ProgressMeter.h>

#include <casacore/casa/sstream.h>
#include <casacore/casa/fstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
ImageRegrid<T>::ImageRegrid()
: itsShowLevel(0),
  itsDisableConversions(false),
  itsNotify(false)
{;}

template<class T>
ImageRegrid<T>::ImageRegrid(const ImageRegrid& other)  
: itsShowLevel(other.itsShowLevel),
  itsDisableConversions(other.itsDisableConversions),
  itsNotify(other.itsNotify)
{;}


template<class T>
ImageRegrid<T>::~ImageRegrid()
{;}

template<class T>
ImageRegrid<T>& ImageRegrid<T>::operator=(const ImageRegrid& other) 
{
  if (this != &other) {
    itsShowLevel = other.itsShowLevel;
    itsDisableConversions = other.itsDisableConversions;
    itsNotify = other.itsNotify;
  }
  return *this;
}

template<class T>
void ImageRegrid<T>::regrid(
	ImageInterface<T>& outImage,
	typename Interpolate2D::Method method,
	const IPosition& outPixelAxesU,
	const ImageInterface<T>& inImage,
	bool replicate, uint32_t decimate, bool showProgress,
	bool forceRegrid, bool verbose
) {
	LogIO os(LogOrigin("ImageRegrid", __func__, WHERE));
	Timer t0;
	IPosition outShape = outImage.shape();
	IPosition inShape = inImage.shape();
	const uint32_t nDim = inImage.ndim();
	ThrowIf(
		nDim != outImage.ndim(),
		"The input and output images must have the same "
		"number of axes"
	);
	if (inImage.imageInfo().hasMultipleBeams()) {
		ThrowIf(
			inImage.coordinates().hasSpectralAxis()
			&& anyTrue(
				outPixelAxesU.asVector()
				== inImage.coordinates().spectralAxisNumber()
			),
			"This image has multiple beams. The spectral axis cannot be regridded"
		);
		ThrowIf(
			inImage.coordinates().hasPolarizationCoordinate()
			&& anyTrue(
				outPixelAxesU.asVector()
				== inImage.coordinates().polarizationAxisNumber()
			),
			"This image has multiple beams. The polarization axis cannot be regridded"
		);
	}
	const bool outIsMasked = outImage.isMasked() && outImage.hasPixelMask()
			&& outImage.pixelMask().isWritable();
	const CoordinateSystem& inCoords = inImage.coordinates();
	CoordinateSystem outCoords = outImage.coordinates();
	IPosition outPixelAxes = outPixelAxesU;

	// Find world and pixel axis maps

	Vector<int32_t> pixelAxisMap1, pixelAxisMap2;
	findMaps (nDim, pixelAxisMap1, pixelAxisMap2, inCoords, outCoords);

	// Check user pixel axes specifications

	_checkAxes(outPixelAxes, inShape, outShape, pixelAxisMap1, outCoords, verbose);

	{
		// warn if necessary, CAS-5110
		Vector<int32_t> dirAxes = outCoords.directionAxesNumbers();
		bool regridDirAxis = false;
		for (uint32_t i=0; i<outPixelAxes.size(); i++) {
			for (uint32_t j=0; j<dirAxes.size(); j++) {
				if (outPixelAxes[i] == dirAxes[j]) {
					regridDirAxis = true;
					break;
				}
			}
			if (regridDirAxis) {
				break;
			}
		}
		const ImageInfo info = inImage.imageInfo();
		if (regridDirAxis && info.hasBeam()) {
			const DirectionCoordinate dc = inImage.coordinates().directionCoordinate();
			Vector<double> inc = dc.increment();
			Vector<String> units = dc.worldAxisUnits();
			Quantity inpix = min(Quantity(inc[0], units[0]), Quantity(inc[1], units[1]));
			Quantity inbeam = info.hasSingleBeam()
				? info.restoringBeam().getMinor()
				: info.getBeamSet().getSmallestMinorAxisBeam().getMinor();
			const DirectionCoordinate dcout = outImage.coordinates().directionCoordinate();
			inc = dcout.increment();
			units = dcout.worldAxisUnits();
			Quantity outpix = min(Quantity(inc[0], units[0]), Quantity(inc[1], units[1]));
			if (
				(
					method == Interpolate2D::NEAREST && inbeam/inpix < Quantity(5, "")
					&& outpix/inpix > Quantity(0.5, "")
				)
				|| (
					method == Interpolate2D::LINEAR && inbeam/inpix < Quantity(3, "")
					&& outpix/inpix > Quantity(0.75, "")
				)
				|| (
					method == Interpolate2D::CUBIC && inbeam/inpix < Quantity(3, "")
					&& outpix/inpix > Quantity(1, "")
				)
			) {
				LogIO log;
				log << LogOrigin("ImageRegrid", __func__) << LogIO::WARN
					<< "You are regridding an image whose beam is not well sampled by the "
					<< "pixel size.  Total flux can be lost when regridding such "
					<< "images, especially when the new pixel size is larger than "
					<< "the old pixel size. It is recommended to check the total "
					<< "flux of your input and output image, and if necessary "
					<< "rebin the input to have smaller pixels." << LogIO::POST;
			}

		}
	}
	const uint32_t nOutRegridPixelAxes = outPixelAxes.nelements();
	if (itsShowLevel>0) {
		cerr << "outPixelAxes = " << outPixelAxes << endl;
	}

	// Set output shape.  This shape is incremental, for each regridding
	// pass it incrementally changes from the input shape to the output shape
	// We account here for different pixel axis orders

	IPosition outShape2(nDim);
	for (uint32_t paOut=0; paOut<nDim; paOut++) {
		outShape2(paOut) = inShape(pixelAxisMap1[paOut]);
	}

	// Specify input and output lattices for each regridding pass

	MaskedLattice<T>* inPtr = 0;
	CoordinateSystem inCoords2(inCoords);
	MaskedLattice<T>* outPtr = 0;
	MaskedLattice<T>* finalOutPtr = &outImage;
	Vector<bool> doneOutPixelAxes(outCoords.nPixelAxes(), true);
	for (uint32_t i=0; i<nOutRegridPixelAxes; i++) {
		doneOutPixelAxes(outPixelAxes[i]) = false;
	}
	// Loop over specified pixel axes of output image

	bool first = true;
	for (uint32_t i=0; i<nOutRegridPixelAxes; i++) {
		if (!doneOutPixelAxes(outPixelAxes[i])) {

			// Set input and output images for this pass. The new  input must be the last
			// output image.  We end up with at least one temporary image. Could
			// probably improve this.
			if (first) {
				inPtr = inImage.cloneML();
				first = false;
			}
			else {

				// If inPtr == 0, it means that in the previous pass, the coordinate
				// and shape information for the regrid axis was identical (in==out)
				// so we didn't
				// actually need to regrid, but just swap in and out pointers.

				if (inPtr) delete inPtr;
				inPtr = outPtr;
				outPtr = 0;
			}
			// Regrid one Coordinate, pertaining to this axis. If the axis
			// belongs to a DirectionCoordinate or 2-axis LinearCoordinate,
			// it will also regrid the other axis.  After the first pass,
			// the output image is in the final order.
			_regridOneCoordinate (
				os, outShape2, doneOutPixelAxes,
				finalOutPtr, inPtr, outPtr, outCoords,
				inCoords2, outPixelAxes[i], inImage,
				outShape, replicate, decimate, outIsMasked,
				showProgress, forceRegrid, method, verbose
			);
			// The input CS for the next pass is now the present output
			// CS, except that we overwrite the coordinates not yet regridded
			inCoords2 = outCoords;
			for (uint32_t j=0; j<doneOutPixelAxes.nelements(); j++) {
				// For every pixel axis that has not yet been regridded, put back
				// the original coordinate (so that its other axes can be regridded)
				if (!doneOutPixelAxes(j)) {
					int32_t inCoord2, inAxisInCoord2;
					inCoords2.findPixelAxis(inCoord2, inAxisInCoord2, j);
					int32_t inCoord, inAxisInCoord;
					inCoords.findPixelAxis(inCoord, inAxisInCoord,
							pixelAxisMap1[j]);
					// We might end up replacing the same coordinate more than
					// once with this algorithm
					inCoords2.replaceCoordinate(
						inCoords.coordinate(inCoord),
						inCoord2
					);
				}
			}
		}
	}
	delete inPtr;
	inPtr = 0;
	if (itsShowLevel>0) {
		cerr << "Function regrid took " << t0.all() << endl;
	}
}

template<class T>
void ImageRegrid<T>::_regridOneCoordinate (LogIO& os, IPosition& outShape2,
		Vector<bool>& doneOutPixelAxes,
		MaskedLattice<T>* &finalOutPtr,
		MaskedLattice<T>* &inPtr,
		MaskedLattice<T>* &outPtr,
		CoordinateSystem& outCoords,
		const CoordinateSystem& inCoords,
		int32_t outPixelAxis,
		const ImageInterface<T>& inImage,
		const IPosition& outShape,
		bool replicate, uint32_t decimate,
		bool outIsMasked, bool showProgress,
		bool forceRegrid,
		typename Interpolate2D::Method method,
		bool verbose)
		{
	Timer t0;
	double s0 = 0.0;
	// Find world and pixel axis maps

	Vector<int32_t> pixelAxisMap1, pixelAxisMap2;
	findMaps (inImage.ndim(), pixelAxisMap1, pixelAxisMap2, inCoords, outCoords);

	// Find equivalent world axis
	int32_t outWorldAxis = outCoords.pixelAxisToWorldAxis(outPixelAxis);
	int32_t outCoordinate, outAxisInCoordinate;
	int32_t inCoordinate, inAxisInCoordinate;
	outCoords.findPixelAxis(outCoordinate, outAxisInCoordinate, outPixelAxis);
	Coordinate::Type type = outCoords.type(outCoordinate);

	// Find Coordinate in input image.

	int32_t inPixelAxis = pixelAxisMap1[outPixelAxis];
	int32_t inWorldAxis = inCoords.pixelAxisToWorldAxis(inPixelAxis);
	inCoords.findPixelAxis(inCoordinate, inAxisInCoordinate, inPixelAxis);
	if (inCoordinate==-1 || inAxisInCoordinate==-1) {
		ostringstream oss1;
		ostringstream oss2;
		oss1 << outCoords.showType(outCoordinate);
		oss2 << outPixelAxis+1;
		String msg = String("Output axis (") + String(oss2) +
				String(") of coordinate type ") + String(oss1) +
				String("does not have a coordinate in the input "
						"CoordinateSystem");
		ThrowCc(msg);
	}

	// Where are the input and output pixel axes for this  coordinate ?
	// DIrectionCoordinates and LinearCoordinates (holding two axes) are
	// done in one pass.  Others are done axis by axis which is flawed
	// when those axes are coupled (e.g. other axes of LC)

	Vector<int32_t> outPixelAxes = outCoords.pixelAxes(outCoordinate);
	Vector<int32_t>  inPixelAxes = inCoords.pixelAxes(inCoordinate);
	int32_t maxMemoryInMB = 0;

	// Work out if we can do a 2-D regrid. Either a DC or a 2-axis LC
	// (maybe extend to two axes of N-axis LC in the future)

	if ( (type==Coordinate::DIRECTION) ||
			(type==Coordinate::LINEAR &&
					outPixelAxes.nelements()==2 &&
					inPixelAxes.nelements()==2) ) {
		// We will do two pixel axes in this pass

		doneOutPixelAxes(outPixelAxes[0]) = true;
		doneOutPixelAxes(outPixelAxes[1]) = true;

		// Update the incremental output image shape.

		outShape2(outPixelAxes[0]) = outShape(outPixelAxes[0]);
		outShape2(outPixelAxes[1]) = outShape(outPixelAxes[1]);
		ThrowIf(
			outShape2(outPixelAxes[0])==1 && outShape2(outPixelAxes[1])==1,
			"You cannot regrid the Coordinate as it is "
			"of shape [1,1]"
		);
		const IPosition inShape = inPtr->shape();
		bool shapeDiff =
				(outShape2(outPixelAxes[0]) != inShape(inPixelAxes(0))) ||
				(outShape2(outPixelAxes[1]) != inShape(inPixelAxes(1)));
		// See if we really need to regrid this axis. If the coordinates and shape are the
		// same there is nothing to do apart from swap in and out pointers
		// or copy on last pass

		const Coordinate& cIn = inCoords.coordinate(inCoordinate);
		const Coordinate& cOut = outCoords.coordinate(outCoordinate);
		bool regridIt = shapeDiff || forceRegrid || !(cIn.near(cOut));
		bool lastPass = allEQ(doneOutPixelAxes, true);
		//
		if (!regridIt) {
			if (verbose) {
				os << "Input and output shape/coordinate information for "
					<< Coordinate::typeToString(cIn.type())
					<< " axes equal - no regridding needed" << LogIO::POST;
			}
			if (lastPass) {

				// Can't avoid this copy

				LatticeUtilities::copyDataAndMask (os, *finalOutPtr, *inPtr);
			} else {
				outPtr = inPtr;
				inPtr = 0;
			}
			return;
		}
		// Deal with pointers

		if (lastPass) {
			outPtr = finalOutPtr;
		} else {
			outPtr = new TempImage<T>(TiledShape(outShape2), outCoords,
					maxMemoryInMB);
			if (outIsMasked) {
				String maskName("mask0");
				TempImage<T>* tmpPtr = dynamic_cast<TempImage<T>*>(outPtr);
				tmpPtr->makeMask(maskName, true, true, false);
			}
		}
		//
		regridTwoAxisCoordinate  (os, *outPtr, *inPtr, inImage.units(),
				inCoords, outCoords, inCoordinate,
				outCoordinate, inPixelAxes, outPixelAxes,
				pixelAxisMap1, pixelAxisMap2, method,
				replicate, decimate, showProgress);
	} else {

		// Note that will do one pixel axis in this pass

		doneOutPixelAxes(outPixelAxes(outAxisInCoordinate)) = true;

		// Update the incremental output image shape.

		outShape2(outPixelAxes(outAxisInCoordinate)) =
				outShape(outPixelAxes(outAxisInCoordinate));
		const IPosition inShape = inPtr->shape();
		bool shapeDiff = (outShape2(outPixelAxes(0)) != inShape(inPixelAxes(0)));

		// Get Coordinates.  Set world axis units for input and output
		// coordinates for this pixel
		// axis to be the same.  We can only do this via the
		// CoordinateSystem (or casting)
		// or by breaking polymorphism like we did for DirectionCoordinate.
		// Ho hum.

		Vector<String> inUnits = inCoords.worldAxisUnits();
		Vector<String> outUnits = outCoords.worldAxisUnits();
		outUnits(outWorldAxis) = inUnits(inWorldAxis);
		ThrowIf(
			!outCoords.setWorldAxisUnits(outUnits),
			"Failed to set output CoordinateSystem units"
		);
		const Coordinate& inCoord = inCoords.coordinate(inCoordinate);
		const Coordinate& outCoord = outCoords.coordinate(outCoordinate);

		// See if we really need to regrid this axis.
		//  If the coordinates are the same
		// there is nothing to do apart from swap in and out pointers or
		// copy on last pass

		IPosition t(1, outAxisInCoordinate);
		IPosition excludeAxes = IPosition::otherAxes(outCoord.nPixelAxes(), t);
		bool regridIt = shapeDiff ||
				forceRegrid || !(inCoord.near(outCoord, excludeAxes.asVector()));
		bool lastPass = allEQ(doneOutPixelAxes, true);
		//
		if (!regridIt) {
			if (verbose) {
				os << "Input and output shape/coordinate information for "
					<< Coordinate::typeToString(inCoord.type())
					<< " axis equal - no regridding needed" << LogIO::POST;
			}
			if (lastPass) {

				// Can't avoid this copy

				LatticeUtilities::copyDataAndMask (os, *finalOutPtr, *inPtr);
			} else {
				outPtr = inPtr;
				inPtr = 0;
			}
			return;
		}

		// Deal with pointers

		if (lastPass) {
			outPtr = finalOutPtr;
		} else {
			outPtr = new TempImage<T>(TiledShape(outShape2), outCoords,
					maxMemoryInMB);
			if (outIsMasked) {
				String maskName("mask0");
				TempImage<T>* tmpPtr = dynamic_cast<TempImage<T>*>(outPtr);
				tmpPtr->makeMask(maskName, true, true, true, true);
			}
		}

		// Possibly make Frequency reference conversion machine.
		//  We could use the internal
		// machine layer inside the SpectralCoordinate, but making
		// the machine explicitly
		// this way is more general because it allows the ObsInfos
		// and SpectralCoordinates
		// to be different.

		bool madeIt = false;
		MFrequency::Convert machine;
		if (!itsDisableConversions && type==Coordinate::SPECTRAL) {
			madeIt = CoordinateUtil::makeFrequencyMachine(os, machine,
					inCoordinate, outCoordinate, inCoords, outCoords);
		}

		// Regrid

		if (itsShowLevel>0) {
			cerr << "usemachine=" << madeIt << endl;
		}
		regrid1D (*outPtr, *inPtr, inCoord, outCoord, inPixelAxes,
				outPixelAxes, inAxisInCoordinate, outAxisInCoordinate,
				pixelAxisMap2, method, machine, replicate,
				madeIt, showProgress);
	}
	//
	if (itsShowLevel > 0) {
		s0 += t0.all();
		cerr << "   Function regridOneCoordinate took " << s0 << endl;
	}
}


template<class T>
bool ImageRegrid<T>::insert (ImageInterface<T>& outImage,
                             const Vector<double>& outPixel,
                             const ImageInterface<T>& inImage) 
{
   LogIO os(LogOrigin("ImageRegrid", "insert(...)", WHERE));
//
   ThrowIf(
		   outImage.ndim()!=inImage.ndim(),
		   "The input and output images must have the same number of dimensions"
	);
//
   const CoordinateSystem& inCoords = inImage.coordinates();
   const CoordinateSystem& outCoords = outImage.coordinates();
   const uint32_t nPixelAxes = inCoords.nPixelAxes();
   AlwaysAssert(outImage.shape().nelements()==nPixelAxes,AipsError);
//
   bool locateByRefPix = outPixel.nelements()==0;
   if (!locateByRefPix) {
      AlwaysAssert(outPixel.nelements()==nPixelAxes,AipsError);
   }
//
   const IPosition& inShape = inImage.shape();
   const IPosition& outShape = outImage.shape();
//
   const Vector<double>& inRefPix = inCoords.referencePixel();
   const Vector<double>& outRefPix = outCoords.referencePixel();

// Where are the output blc/trc after placing the input image
// No trimming yet

   IPosition outBlc(nPixelAxes), outTrc(nPixelAxes);
   IPosition inBlc(nPixelAxes), inTrc(nPixelAxes);
   int32_t coordinate, axisInCoordinate;
   for (uint32_t i=0; i<nPixelAxes; i++) {
      outCoords.findPixelAxis(coordinate, axisInCoordinate, i);
      if (coordinate==-1 || axisInCoordinate==-1) {
         ostringstream oss;
         oss << "Pixel axis " << i <<
	   " has been removed from the output CoordinateSystem" << endl;
         ThrowCc(String(oss));
      }
      Coordinate::Type type = outCoords.type(coordinate);
      ThrowIf(
    		  type==Coordinate::STOKES && outShape(i)!=inShape(i),
    		  "It is not possible to change the shape of the Stokes axis"
      );
//
      if (locateByRefPix) {
         outBlc(i) = static_cast<int32_t>(outRefPix(i) - inRefPix(i) + 0.5);
      } else {
         outBlc(i) = static_cast<int32_t>(outPixel(i) + 0.5);
      }
      outTrc(i) = outBlc(i) + inShape(i) - 1;
//
      inBlc(i) = 0;
      inTrc(i) = inShape(i) - 1;
   }
   if (itsShowLevel>0) {
      cerr << "inBlc, inTrc = " << inBlc << inTrc << endl;
      cerr << "outBlc, outTrc = " << outBlc << outTrc << endl;
   }

// Does the input miss the output entirely ?

   bool missedIt = true;
   for (uint32_t i=0; i<nPixelAxes; i++) {
      if ( (outTrc(i)>=0 && outTrc(i)<outShape(i)) ||
           (outBlc(i)>=0 && outTrc(i)<outShape(i)) ||
           (outBlc(i)>=0 && outBlc(i)<outShape(i)) ) {
         missedIt = false;
         break;
      }
   }
   if (itsShowLevel>0) {
      cerr << "missedIt = " << missedIt << endl;
   }
   if (missedIt) return false;

// Now trim blc/trc

   for (uint32_t i=0; i<nPixelAxes; i++) {
      int32_t t = outBlc(i);
      outBlc(i) = max(0,outBlc(i));
      int32_t d = t - outBlc(i);
      inBlc(i) -= d;
//
      t = outTrc(i);
      outTrc(i) = std::min(outShape(i)-1, outTrc(i));
      d = t - outTrc(i);
      inTrc(i) -= d;
   }

   if (itsShowLevel>0) {
      cerr << "After trimming " << endl;
      cerr << "inBlc, inTrc = " << inBlc << inTrc << endl;
      cerr << "outBlc, outTrc = " << outBlc << outTrc << endl;
   }

// Copy the relevant portion

   Slicer inBox(inBlc, inTrc, Slicer::endIsLast);
   Slicer outBox(outBlc, outTrc, Slicer::endIsLast);
   SubImage<T> inSub(inImage, inBox);
   SubImage<T> outSub(outImage, outBox, true);
//
   const bool outIsMasked = outImage.isMasked() && outImage.hasPixelMask() &&
                            outImage.pixelMask().isWritable();
   if (outIsMasked) {
      LatticeUtilities::copyDataAndMask(os, outSub, inSub);
   } else {
      outSub.copyData(inSub);
   }
//
   return true;
}

template<class T> CoordinateSystem ImageRegrid<T>::makeCoordinateSystem(
	LogIO& os, std::set<Coordinate::Type>& coordsToBeRegridded,
	const CoordinateSystem& cSysTo, const CoordinateSystem& cSysFrom,
	const IPosition& outPixelAxes, const IPosition& inShape,
	bool giveStokesWarning
) {
	coordsToBeRegridded.clear();
	os << LogOrigin("ImageRegrid<T>", __func__, WHERE);
	const uint32_t nCoordsFrom = cSysFrom.nCoordinates();
	const uint32_t nPixelAxesFrom = cSysFrom.nPixelAxes();
	ThrowIf(
		inShape.nelements() > 0 && inShape.nelements() != nPixelAxesFrom,
		"Inconsistent size and csysFrom"
	);

	// Create output CS.  Copy the output ObsInfo over first.

	CoordinateSystem cSysOut(cSysFrom);

	// If specified axes are empty, set to all

	IPosition outPixelAxes2 = outPixelAxes.nelements() == 0
		? IPosition::makeAxisPath(nPixelAxesFrom)
		: outPixelAxes;

	// Loop over coordinates in the From CS

	for (uint32_t i=0; i<nCoordsFrom; i++) {
		Coordinate::Type typeFrom = cSysFrom.type(i);
		bool regridIt = false;

		// Stokes is never regridded
		if (typeFrom == Coordinate::STOKES) {
			if (outPixelAxes.size() > 0 && giveStokesWarning) {
				os << LogIO::WARN << "A stokes coordinate cannot be regridded, ignoring" << LogIO::POST;
			}
			continue;
		}
		Vector<int32_t> pixelAxes = cSysFrom.pixelAxes(i);
		for (uint32_t k=0; k<pixelAxes.nelements(); k++) {
			if (
				inShape.nelements() == 0
				|| (inShape.nelements() > 0 && inShape[pixelAxes[k]] > 1)
			) {
				for (uint32_t j=0; j<outPixelAxes2.nelements(); j++) {
					if (pixelAxes[k] == outPixelAxes2(j)) {
						regridIt = true;
					}
				}
			}
		}

		// We are regridding some axis from this coordinate.
		// Replace it from 'To' if we can find it.

		if (regridIt) {

			// Trouble with multiple Coordinates of this type here.

			int32_t iCoordTo = cSysTo.findCoordinate(typeFrom, -1);
			if (iCoordTo < 0) {
				os << LogIO::WARN << Coordinate::typeToString(typeFrom) << " coordinate is not present "
					<< " in the output coordinate system, so it cannot be regridded"
					<< LogIO::POST;
			}
			else {
				ThrowIf(
					cSysTo.pixelAxes(iCoordTo).nelements()
					!= cSysFrom.pixelAxes(i).nelements(),
					"Wrong number of pixel axes in 'To' CoordinateSystem for "
					"coordinate of type " + cSysTo.showType(iCoordTo)
				);
				cSysOut.replaceCoordinate (cSysTo.coordinate(iCoordTo), i);
				coordsToBeRegridded.insert(typeFrom);
			}
		}
	}
	return cSysOut;
}


template<class T>
void ImageRegrid<T>::regridTwoAxisCoordinate (
	LogIO& os, MaskedLattice<T>& outLattice,
	const MaskedLattice<T>& inLattice,
	const Unit& imageUnit, const CoordinateSystem& inCoords,
	const CoordinateSystem& outCoords,
	int32_t inCoordinate, int32_t outCoordinate,
	const Vector<int32_t> inPixelAxes,
	const Vector<int32_t> outPixelAxes,
	const Vector<int32_t> pixelAxisMap1,
	const Vector<int32_t> pixelAxisMap2,
	typename Interpolate2D::Method method,
	bool replicate, uint32_t decimate, bool showProgress) {
	// Compute output coordinate, find region around this coordinate
	// in input, interpolate. Any output mask is overwritten

	Timer t0, t1, t2, t3, t4;
	double s0 = 0.0;
	double s1 = 0.0;
	double s2 = 0.0;
	double s3 = 0.0;
	double s4 = 0.0;
	AlwaysAssert(inPixelAxes.nelements()==2, AipsError);
	AlwaysAssert(outPixelAxes.nelements()==2, AipsError);
	bool inIsMasked = inLattice.isMasked();
	bool outIsMasked = outLattice.isMasked() && outLattice.hasPixelMask() &&
			outLattice.pixelMask().isWritable();
	const IPosition inShape = inLattice.shape();
	const IPosition outShape = outLattice.shape();
	const uint32_t nDim = inLattice.ndim();
	if (itsShowLevel>0) {
		cerr << "Replicate = " << replicate << endl;
		cerr << "inPixelAxes = " << inPixelAxes << endl;
		cerr << "outPixelAxes = " << outPixelAxes << endl;
		cerr << "inIsMasked " << inIsMasked << endl;
		cerr << "outIsMasked " << outIsMasked << endl;
	}
	if (itsShowLevel>0) {
		if (method==Interpolate2D::NEAREST) {
			cerr << "Method is nearest" << endl;
		} else if (method==Interpolate2D::LINEAR) {
			cerr << "Method is linear" << endl;
		} else if (method==Interpolate2D::CUBIC) {
			cerr << "Method is cubic" << endl;
		}
	}
	// We iterate through the output image by tile.  We iterate through
	// each tile by matrix holding the Direction Coordinate axes (in
	// some order).  We have a matrix(i=1:nrows,j=1:ncols).
	//
	// pixelAxisMap1(i) says where pixel axis i in the output image is
	//			 in the input  image
	// pixelAxisMap2(i) says where pixel axis i in the
	//			  input image is in the output image
	// pixelAxes(0)    says where Lon  is in image (in or out)
	// pixelAxes(1)    says where Lat  is in image (in or out)
	// xOutAxis        is the first direction axis in the output image
	//				 (associated with i)
	// yOutAxis        is the second direction axis in the output image
	//				 (associated with j)
	// xInCorrAxis     is the corresponding axis to xOutAxis in the input image
	// yInCorrAxis     is the corresponding axis to yOutAxis in the input image
	// xInAxis         is the first direction axis in the input image
	// yInAxis         is the second direction axis in the input image
	// xOutCorrAxis    is the corresponding axis to xInAxis in the output image
	// yOutCorrAxis    is the corresponding axis to yInAxis in the output image

	// Example:
	//   Regrid ra/dec axes with input order ra/dec/freq and output order
	// 			freq/dec/ra
	//
	//   input  image shape = [20, 30, 40] (ra/dec/freq)
	//   output image shape = [40, 90, 60] (freq/dec/ra) - we are making ra/dec
	//				 shape 3x input
	//
	//   outPixelAxes = [2,1] = [lon,lat]
	//   The cursor matrix is of shape [nrow,ncol] = [90,60]
	//       xOutAxis = 1   (dec)
	//       yOutAxis = 2   (ra)
	//    xInCorrAxis = 1   (dec)
	//    yInCorrAxis = 0   (ra)
	//        xInAxis = 0   (ra)
	//        yInAxis = 1   (dec)
	//   xOutCorrAxis = 2   (ra)
	//   yOutCorrAxis = 1   (dec)

	const uint32_t xOutAxis = min(outPixelAxes(0), outPixelAxes(1));
	const uint32_t yOutAxis = max(outPixelAxes(0), outPixelAxes(1));
	uint32_t xInCorrAxis = pixelAxisMap1[xOutAxis];
	uint32_t yInCorrAxis = pixelAxisMap1[yOutAxis];
	const uint32_t xInAxis = min(inPixelAxes(0), inPixelAxes(1));
	const uint32_t yInAxis = max(inPixelAxes(0), inPixelAxes(1));
	uint32_t xOutCorrAxis = pixelAxisMap2[xInAxis];
	uint32_t yOutCorrAxis = pixelAxisMap2[yInAxis];
	// Make navigator and iterator for output data and mask.  It is vital that
	// the "niceShape" is the same for both iterators.  Because the mask and
	// lattice are both TempLattices, one might be on disk, one in core.
	// Hence we pick one nice shape and use it on both iterators, although
	// it may be suboptimal for one of the lattices.

	//IPosition niceShape = outLattice.niceCursorShape();
	// Temporary fix for AIT/SIN regriding for full sky images
	// Hold a plane in memory
	IPosition niceShape=outLattice.shape();
	niceShape=1;
	niceShape(xOutAxis)=outLattice.shape()(xOutAxis);
	niceShape(yOutAxis)=outLattice.shape()(yOutAxis);

	LatticeStepper outStepper(outShape, niceShape, LatticeStepper::RESIZE);

	LatticeIterator<T> outIter(outLattice, outStepper);

	if (itsShowLevel>0) {
		cerr << "xOutAxis, yOutAxis = " << xOutAxis << ", " << yOutAxis << endl;
		cerr << "xInCorrAxis, yInCoorrAxis = " << xInCorrAxis << ", " <<
				yInCorrAxis << endl;
		cerr << "xInAxis, yInAxis = " << xInAxis << ", " << yInAxis << endl;
		cerr << "xOutCorrAxis, yOutCoorrAxis = " << xOutCorrAxis << ", " <<
				yOutCorrAxis << endl;
		//
		cerr << "cursor shape = " << niceShape << endl;
		cerr << "shape in, shape out" << inShape << outShape << endl;
	}

	// Deal with mask.  Stepper will make a reference copy of the mask

	LatticeIterator<bool>* outMaskIterPtr = 0;
	if (outIsMasked) {
		Lattice<bool>& outMask = outLattice.pixelMask();
        outMaskIterPtr = new LatticeIterator<bool>(outMask, outStepper);
	}
	// These tell us which chunk of input data we need to service each
	// iteration through the output image

	IPosition inChunkBlc(nDim);
	IPosition inChunkTrc(nDim);

	// These tell us which 2D piece of inChunk to read.
	// This is  what we regrid from.

	IPosition inChunkBlc2D(nDim);
	IPosition inChunkTrc2D(nDim);

	// Coordinate conversion vectors

	Vector<double> world(2), inPixel(2), outPixel(2);
	Vector<double> pixelScale(nDim);
	pixelScale = 1.0;
	pixelScale(xInAxis) = float(outShape(xOutCorrAxis)) /
			float(inShape(xInAxis));
	pixelScale(yInAxis) = float(outShape(yOutCorrAxis)) /
			float(inShape(yInAxis));
	if (itsShowLevel > 0) {
		cerr << "pixelScale = " << pixelScale << endl;
	}

	// 2D interpolator

	Interpolate2D interp(method);
	// Various things needed along the way

	Vector<double> pix2DPos2(2);
	IPosition outPos4, outPos3, outPos2, inPos;
	double minInX, minInY, maxInX, maxInY;

	// Generate full plane of coordinates mapping each output
	// pixel to an input pixel

	IPosition shapeGrid(3, outShape(xOutAxis), outShape(yOutAxis), 2);
	its2DCoordinateGrid.resize(shapeGrid);
	its2DCoordinateGridMask.resize(outShape(xOutAxis), outShape(yOutAxis));
	bool allFailed = false;
	bool missedIt = false;

	// Either generate the coordinate grid or use what the user has supplied
	t1.mark();
	if (itsUser2DCoordinateGrid.nelements() > 0 &&
			itsUser2DCoordinateGridMask.nelements() > 0) {
		if (itsNotify) {
			os << "Using user set DirectionCoordinate grid" << LogIO::POST;
		}
		//
		{
			IPosition shp1 = its2DCoordinateGrid.shape();
			IPosition shp2 = itsUser2DCoordinateGrid.shape();
			if (shp1.isEqual(shp2)) {
				its2DCoordinateGrid = itsUser2DCoordinateGrid;
			} else {
				os << "User set 2D coordinate grid has the wrong shape" <<
						LogIO::POST;
			}
		}
		{
			IPosition shp1 = its2DCoordinateGridMask.shape();
			IPosition shp2 = itsUser2DCoordinateGridMask.shape();
			if (shp1.isEqual(shp2)) {
				its2DCoordinateGridMask = itsUser2DCoordinateGridMask;
			} else {
				os << "User set 2D coordinate grid mask has the wrong shape" <<
						LogIO::POST;
			}
		}
		//
		allFailed = false;
		missedIt = false;
	}
	else {
		allFailed = false;
		missedIt = true;
		IPosition outPosFull(outLattice.ndim(),0);
		if (replicate) {
			make2DCoordinateGrid (its2DCoordinateGrid, minInX, minInY, maxInX,
					maxInY,
					pixelScale, xInAxis, yInAxis, xOutAxis, yOutAxis,
					xInCorrAxis, yInCorrAxis, xOutCorrAxis,
					yOutCorrAxis,
					outPosFull, outShape);
			missedIt = false;
			allFailed = false;
			its2DCoordinateGridMask.set(true);
		}
		else {
			make2DCoordinateGrid (os, allFailed, missedIt, minInX, minInY, maxInX,
					maxInY,
					its2DCoordinateGrid, its2DCoordinateGridMask,
					inCoords, outCoords, inCoordinate, outCoordinate,
					xInAxis, yInAxis, xOutAxis,
					yOutAxis,
					inPixelAxes, outPixelAxes, inShape, outPosFull,
					outShape, decimate);
		}
	}
	s1 += t1.all();
	if (missedIt || allFailed) {
		outLattice.set(0.0);
		if (outIsMasked) {
			outLattice.pixelMask().set(false);
		}
        delete outMaskIterPtr;
        return;
	}
	// Progress meter

	ProgressMeter* pProgressMeter = 0;
	if (showProgress) {
		double nMin = 0.0;
		double nMax = double(outLattice.shape().product());
		ostringstream oss;
		oss << "Axes " << outPixelAxes + 1 << " : Pixels Regridded";
		pProgressMeter = new ProgressMeter(nMin, nMax, String(oss),
				String("Regridding"),
				String(""), String(""),
				true,
				max(1,int32_t(nMax/20)));
	}
	// Find scale factor for Jy/pixel images

	double scale = findScaleFactor(imageUnit, inCoords, outCoords,
			inCoordinate, outCoordinate, os);

	// Iterate through output image

	t2.mark();
	double iPix = 0.0;
	int32_t i2;
	for (outIter.reset(); !outIter.atEnd(); outIter++) {
		const IPosition& outCursorShape = outIter.cursorShape();
		const IPosition& outPos = outIter.position();
		if (itsShowLevel>0) {
			cerr << endl;
			cerr << "Output lattice iterator position = " <<  outPos << endl;
			cerr << "Shape of cursor = " << outIter.cursor().shape() << endl;
		}

		// Now get a chunk of input data which we will access over and over
		// as we interpolate it.

		missedIt = true;
		allFailed = true;
		t3.mark();
		findXYExtent (missedIt, allFailed, minInX, minInY, maxInX, maxInY,
				its2DCoordinateGrid,
				its2DCoordinateGridMask, xInAxis, yInAxis, xOutAxis,
				yOutAxis, outPos,
				outCursorShape, inShape);
		s3 += t3.all();
		if (itsShowLevel>0) {
			cerr << "missedIt, allFailed, minInX, maxInX, minInY, maxInY = " <<
					missedIt << ", " << allFailed << ", " <<
					minInX << ", " << maxInX << ", " <<  minInY << ", " <<
					maxInY << endl;
		}
		if (missedIt || allFailed) {
			outIter.rwCursor().set(0.0);
			if (outIsMasked) outMaskIterPtr->rwCursor().set(false);
			if (showProgress) {
				pProgressMeter->update(iPix);
				iPix += double(outCursorShape.product());
			}
		}
		else {
			// For the non-regrid axes, the input and output shapes,
			// and hence positions, are the same.
			// pixelAxisMap2(i) says where pixel axis i in the input
			// image is in the output image.

			for (uint32_t k=0; k<nDim; k++) {
				inChunkBlc(k) = outPos[pixelAxisMap2[k]];
				inChunkTrc(k) = outIter.endPosition()(pixelAxisMap2[k]);
			}

			// Now overwrite the blc/trc for the regrid axes.
			// The interpolation schemes (Interpolate2D) use
			// a small grid about the pixel of interest I happen
			// to know that allowing 3
			// pixels on either side is enough.
			// If this should change, the interpolation
			// would return false at the edges
			i2 = static_cast<int32_t>(floor(minInX)) - 3;
			inChunkBlc(xInAxis) = max(0,i2);
			i2 = static_cast<int32_t>(floor(minInY)) - 3;
			inChunkBlc(yInAxis) = max(0,i2);
			i2 = static_cast<int32_t>(ceil(maxInX)) + 3;
			inChunkTrc(xInAxis) = min(inShape(xInAxis)-1,i2);
			i2 = static_cast<int32_t>(ceil(maxInY)) + 3;
			inChunkTrc(yInAxis) = min(inShape(yInAxis)-1,i2);
			IPosition inChunkShape = inChunkTrc - inChunkBlc + 1;
			if (itsShowLevel>0) {
				cerr << "inChunkShape = " << inChunkShape << endl;
				cerr << "inChunkBlc, inChunkTrc " << inChunkBlc << inChunkTrc <<
						endl;
			}

			// Get the input data and mask

			Array<T> inDataChunk = inLattice.getSlice(inChunkBlc, inChunkShape);
			Array<bool>* inMaskChunkPtr = 0;
			if (inIsMasked) {
				inMaskChunkPtr =
						new Array<bool>(inLattice.getMaskSlice(inChunkBlc,
								inChunkShape));
			}
			// Iterate through the output cursor by Matrices,
			// each holding a Direction plane.
			// This gets us just a few percent speed up over
			// iterating through pixel by pixel.

			ArrayLattice<T> outCursor(outIter.rwCursor());    // reference copy
			t4.mark();
			ThrowIf(
				inChunkShape(xInAxis)==1 && inChunkShape(yInAxis)==1,
				"Cannot regrid degenerate DirectionCoordinate plane"
			);
			ThrowIf(
				inChunkShape(xInAxis)==1 || inChunkShape(yInAxis)==1,
				"Cannot yet handle DirectionCoordinate plane with one "
				"degenerate axis"
			);
			regrid2DMatrix(outCursor, outMaskIterPtr, interp, pProgressMeter,
					iPix, nDim,
					xInAxis, yInAxis, xOutAxis, yOutAxis, scale,
					inIsMasked, outIsMasked,
					outPos, outCursorShape, inChunkShape, inChunkBlc,
					pixelAxisMap2,
					inDataChunk, inMaskChunkPtr, its2DCoordinateGrid,
					its2DCoordinateGridMask);


			s4 += t4.all();
		}
		if (outIsMasked) {
			(*outMaskIterPtr)++;
		}
	}
	if (outIsMasked) delete outMaskIterPtr;
	if (showProgress) delete pProgressMeter;
	if (itsShowLevel > 0) {
		s2 += t2.all();
		s0 += t0.all();
		cerr << "         Function regrid2DMatrix took "  << s4 << endl;
		cerr << "         Function findXYExtent took "  << s3 << endl;
		cerr << "      Iterating and regridding took "  << s2 << endl;
		if (replicate) {
			cerr << "      Function make2DCoordinateGrid (replication) took " <<
					s1 << endl;
		} else {
			cerr << "      Function make2DCoordinateGrid (regridding)  took " <<
					s1 << endl;
		}
		cerr << "    Function regrid2D took " << s0  << endl;
	}
}



template<class T>
void ImageRegrid<T>::make2DCoordinateGrid(LogIO& os, bool& allFailed, bool&missedIt,
					  double& minInX, double& minInY, 
					  double& maxInX, double& maxInY,  
					  Cube<double>& in2DPos,
					  Matrix<bool>& succeed,
					  const CoordinateSystem& inCoords,
					  const CoordinateSystem& outCoords,
                                          int32_t inCoordinate, int32_t outCoordinate,
					  uint32_t xInAxis, uint32_t yInAxis,
					  uint32_t xOutAxis, uint32_t yOutAxis,
					  const IPosition& inPixelAxes,
					  const IPosition& outPixelAxes,
					  const IPosition& inShape,
					  const IPosition& outPos,
					  const IPosition& outCursorShape,
					  uint32_t decimate) {
//
// in2DPos says where the output pixel (i,j) is located in the input image
//
  Vector<double> world(2), inPixel(2), outPixel(2);
  minInX =  100000000.0;
  minInY =  100000000.0;
  maxInX = -100000000.0;
  maxInY = -100000000.0;
  allFailed = true;
  bool ok1=false, ok2=false;
  MVDirection inMVD, outMVD;
//
  uint32_t ni = outCursorShape(xOutAxis);
  uint32_t nj = outCursorShape(yOutAxis);
  
// Where in the Direction Coordinates are X and Y ?
// pixelAxes(0) says where Lon is in DirectionCoordinate
// pixelAxes(1) says where Lat is in DirectionCoordinate
// The X axis is always the direction axis that appears first in the image
//
  uint32_t inXIdx = 0;         // [x,y] = [lon,lat]
  uint32_t inYIdx = 1;
  if (inPixelAxes(0)==int32_t(yInAxis)) {
    inXIdx = 1;            // [x,y] = [lat,lon]
    inYIdx = 0;
  };
//
  uint32_t outXIdx = 0;         
  uint32_t outYIdx = 1;
  if (outPixelAxes(0)==int32_t(yOutAxis)) {
    outXIdx = 1;         
    outYIdx = 0;
  };
//
  Matrix<bool> doneIt(ni,nj);
  doneIt = false;
  //
  if (itsShowLevel > 0) {                 
    cerr << "inXIdx, inYIdx = " << inXIdx << ", " << inYIdx << endl;
    cerr << "outXIdx, outYIdx = " << outXIdx << ", " << outYIdx << endl;
  }
//
  uint32_t nOutI = 0;
  uint32_t nOutJ = 0;
  uint32_t iInc = 1;
  uint32_t jInc = 1;
  if (decimate > 1) {
    int32_t nOut = ni / decimate;
    if (nOut <= 1) {
      cerr << "Illegal decimation factor for X; setting to unity" << endl;
      nOut = ni;
    }   
    iInc = ni / (nOut- 1);
    //
    nOut = nj / decimate;
    if (nOut <= 1) {
      cerr << "Illegal decimation factor for Y; setting to unity" << endl;
      nOut = nj;
    }   
    jInc = nj / (nOut - 1);
    if (iInc < 1) {
      cerr << "Illegal decimation increment computed for X; "
	"setting to unity" << endl;
      iInc = 1;
    }
    if (jInc < 1) {
      cerr << "Illegal decimation increment computed for Y; "
	"setting to unity" << endl;
      jInc = 1;
    }
    //
    if (iInc==1 && jInc==1) decimate = 0;
    else {
      for (uint32_t j=0; j<nj; j+=jInc,nOutJ++) {;}
      for (uint32_t i=0; i<ni; i+=iInc,nOutI++) {;}
    };
    //
    if (itsShowLevel > 0) {                 
      cerr << "decimate, nOutI, nOutJ = " << decimate << ", " << nOutI <<
	", " << nOutJ << endl;
      cerr << "iInc, jInc = " << iInc << ", " << jInc << endl;
    }
  }
//
  Matrix<double> iInPos2D(nOutI,nOutJ);
  Matrix<double> jInPos2D(nOutI,nOutJ);
  Matrix<bool> ijInMask2D(nOutI,nOutJ);

// Are we dealing with a DirectionCoordinate or LinearCoordinate ?

  bool isDir = inCoords.type(inCoordinate)==Coordinate::DIRECTION && 
                outCoords.type(outCoordinate)==Coordinate::DIRECTION;
  DirectionCoordinate inDir, outDir;
  LinearCoordinate inLin, outLin;
  bool useMachine = false;
  MDirection::Convert machine;
  if (isDir) {
      inDir = inCoords.directionCoordinate(inCoordinate);
      outDir = outCoords.directionCoordinate(outCoordinate);

// Set units to degrees

      Vector<String> units(2);
      units.set("deg");
      ThrowIf(
    	!inDir.setWorldAxisUnits(units),
        "Failed to set input DirectionCoordinate units to degrees"
    	);
      ThrowIf(
    	!outDir.setWorldAxisUnits(units),
        "Failed to set output DirectionCoordinate units to degrees"
       );

// Possibly make Direction reference conversion machine.  We could use the internal
// machine layer inside the DirectionCoordinate, but making the machine explicitly
// this way is more general because it allows the ObsInfo to be different.

      if (!itsDisableConversions) {
         useMachine = CoordinateUtil::makeDirectionMachine(os, machine, inDir, outDir, 
                                                           inCoords.obsInfo(),
                                                           outCoords.obsInfo());
      }
  } else {
     inLin = inCoords.linearCoordinate(inCoordinate);
     outLin = outCoords.linearCoordinate(outCoordinate);

// Set units to same thing

      const Vector<String>& units = inLin.worldAxisUnits().copy();
      ThrowIf(
    	!outLin.setWorldAxisUnits(units),
         "Failed to set output and input LinearCoordinate axis units the same"
    	);
  }
//
  if (itsShowLevel>0) {
     cerr << "usemachine=" << useMachine << endl;
  }

// If decimating, compute a sparse grid of coordinates and then
// interpolate the others.
// Otherwise, compute all coordinates (very expensive)
// This approach is going to cause pixels along the right and top edges
// to be masked as the coarse grid is unlikely to finish exactly
// on the lattice edge

  const uint32_t nPixelAxes = 2;
  uint32_t nConversions;
  if ( decimate > 1 ) {
    nConversions = nOutI*nOutJ;
  } else {
    nConversions = ni*nj;
  }

  Timer t0;
  uint32_t ii = 0;
  uint32_t jj = 0;

  // if useMachine, then do each pixel separately. Otherwise do a bulk conversion
  if (useMachine) { // must be Direction
    //
    jj = 0;
    for (uint32_t j=0; j<nj; j+=jInc,jj++) {
	  ii = 0;
	  for (uint32_t i=0; i<ni; i+=iInc,ii++) {
		outPixel(outXIdx) = i + outPos[xOutAxis];
        outPixel(outYIdx) = j + outPos[yOutAxis];

        // Do coordinate conversions (outpixel to world to inpixel)
        // for the axes of interest

        ok1 = outDir.toWorld(outMVD, outPixel);
        ok2 = false;
        if (ok1) {
          inMVD = machine(outMVD).getValue();
          ok2 = inDir.toPixel(inPixel, inMVD);
        };
		  //
        if (!ok1 || !ok2) {
          succeed(i,j) = false;
          if (decimate>1) ijInMask2D(ii,jj) = false;
        } else {

          // This gives the 2D input pixel coordinate (relative to
          // the start of the full Lattice)
          // to find the interpolated result at.  (,,0) pertains to
          // inX and (,,1) to inY
          in2DPos(i,j,0) = inPixel(inXIdx);
          in2DPos(i,j,1) = inPixel(inYIdx);
          allFailed = false;
          succeed(i,j) = true;
          //
          if (decimate <= 1) {
            minInX = min(minInX,inPixel(inXIdx));
            minInY = min(minInY,inPixel(inYIdx));
            maxInX = max(maxInX,inPixel(inXIdx));
            maxInY = max(maxInY,inPixel(inYIdx));
          } else {
            iInPos2D(ii,jj) = inPixel(inXIdx);
            jInPos2D(ii,jj) = inPixel(inYIdx);
            ijInMask2D(ii,jj) = true;
          };
        };
      };
    };
  } else {
    // generate coordinate conversions in bulk
    // set storage matrices for the conversions
    Matrix<double> inPixelMatrix(nPixelAxes,nConversions);
    Matrix<double> outPixelMatrix(nPixelAxes,nConversions);
    Matrix<double> worldMatrix(nPixelAxes,nConversions);
    Vector<bool> failures1(nConversions);
    Vector<bool> failures2(nConversions);
    // set the output coordinates
    uint32_t kk = 0;
    jj = 0;
    for (uint32_t j=0; j<nj; j+=jInc,jj++) {
      ii = 0;
      for (uint32_t i=0; i<ni; i+=iInc,ii++) {
        outPixelMatrix(outXIdx,kk) = i + outPos[xOutAxis];
        outPixelMatrix(outYIdx,kk) = j + outPos[yOutAxis];
        kk++;
      };
    };
    // do the conversions
    if (isDir) {
      ok1 = outDir.toWorldMany( worldMatrix, outPixelMatrix, failures1 );
      ok2 = false;
      if (ok1) ok2 = inDir.toPixelMany( inPixelMatrix, worldMatrix, failures2 );
    } else {
      ok1 = outLin.toWorldMany( worldMatrix, outPixelMatrix, failures1 );
      ok2 = false;
      if (ok1) ok2 = inLin.toPixelMany( inPixelMatrix, worldMatrix, failures2 );
    }
    // only keep going if some of the conversions succeeded
    if (!ok2) {
      allFailed = true;
      succeed.set(false);
      ijInMask2D.set(false);
    } else {
      allFailed = false;
      kk = 0;
      jj = 0;
      for (uint32_t j=0; j<nj; j+=jInc,jj++) {
        ii = 0;
        for (uint32_t i=0; i<ni; i+=iInc,ii++) {
          if (failures1(kk) || failures2(kk)) {
            succeed(i,j) = false;
            if (decimate>1) ijInMask2D(ii,jj) = false;
          } else {
            // This gives the 2D input pixel coordinate (relative to
            // the start of the full Lattice)
            // to find the interpolated result at.  (,,0) pertains to
            // inX and (,,1) to inY
            in2DPos(i,j,0) = inPixelMatrix(inXIdx,kk);
            in2DPos(i,j,1) = inPixelMatrix(inYIdx,kk);
            succeed(i,j) = true;
            //
            if (decimate <= 1) {
              minInX = min(minInX,inPixelMatrix(inXIdx,kk));
              minInY = min(minInY,inPixelMatrix(inYIdx,kk));
              maxInX = max(maxInX,inPixelMatrix(inXIdx,kk));
              maxInY = max(maxInY,inPixelMatrix(inYIdx,kk));
            } else {
              iInPos2D(ii,jj) = inPixelMatrix(inXIdx,kk);
              jInPos2D(ii,jj) = inPixelMatrix(inYIdx,kk);
              ijInMask2D(ii,jj) = true;
            };
          };
          kk++;
        };
      };
    };
  };
  if (itsShowLevel > 0) {
    cerr << "nII, nJJ= " << ii << ", " << jj << endl;
    cerr << "Sparse grid took " << t0.all() << endl;
  };
  
  // Bi-linear Interpolation of x and y coordinates in the sparse grid 
  // The coordinates that were already computed get interpolated as 
  // well (should be identical).
  if (decimate > 1) {
    Timer t1;
    //
    Interpolate2D interp(Interpolate2D::LINEAR);
    Vector<double> pos(2);
    double resultI=0.0, resultJ=0.0;

    ArrayAccessor<bool, Axis<0> > sucp0;
    ArrayAccessor<bool, Axis<1> > sucp1(succeed);
    ArrayAccessor<double, Axis<0> > in2Dp0;
    ArrayAccessor<double, Axis<1> > in2Dp1(in2DPos);
    for (uint32_t j=0; j<nj; j++) {
      pos[1] = double(j) / double(jInc); 
      sucp0 = sucp1;
      in2Dp0 = in2Dp1;
      for (uint32_t i=0; i<ni; i++) {
	pos[0] = double(i) / double(iInc); 
	ok1 = interp.interp(resultI, resultJ, pos,
			    iInPos2D, jInPos2D, ijInMask2D);
	if (ok1) {
	  *in2Dp0 = resultI;
	  in2Dp0.next(AxisN(2)) = resultJ;
	  *sucp0 = true;
	  allFailed = false;
	  //
	  minInX = minInX < resultI ? minInX : resultI;
	  minInY = minInY < resultJ ? minInY : resultJ;
	  maxInX = maxInX > resultI ? maxInX : resultI;
	  maxInY = maxInY > resultJ ? maxInY : resultJ;
	} else *sucp0 = false;
	sucp0++;
	in2Dp0++;
      };
      sucp1++;
      in2Dp1++;
    };
    if (itsShowLevel > 0) {
      cerr << "Interpolated grid took " << t1.all() << endl;
    };
  };
  
  
  // Does the output map to anywhere on the input ?
  
  missedIt = false;
  if (!allFailed) {
    double ijMin = -0.5;
    double iMax = inShape(xInAxis) - 0.5;
    double jMax = inShape(yInAxis) - 0.5;
    //
    missedIt  = (minInX<ijMin && maxInX<ijMin)  ||
      (minInX>iMax && maxInX>iMax) ||
      (minInY<ijMin && maxInY<ijMin)  ||
      (minInY>jMax && maxInY>jMax);
  }
  if (itsShowLevel>0) {
    cerr << "allFailed, missedIt  = " << allFailed << ", " << missedIt <<
      endl;
  }
}


template<class T>
void ImageRegrid<T>::make2DCoordinateGrid (Cube<double>& in2DPos,
                                           double& minInX, double& minInY, 
                                           double& maxInX, double& maxInY,  
                                           const Vector<double>& pixelScale,
                                           uint32_t xInAxis, uint32_t yInAxis,
                                           uint32_t xOutAxis, uint32_t yOutAxis,
                                           uint32_t, uint32_t,
                                           uint32_t xOutCorrAxis, uint32_t yOutCorrAxis,
                                           const IPosition& outPos, 
                                           const IPosition& outCursorShape)

{
   double oX = -0.5 + (1.0/2/pixelScale(xInAxis));
   double oY = -0.5 + (1.0/2/pixelScale(yInAxis));
//
   uint32_t ni = outCursorShape(xOutAxis);
   uint32_t nj = outCursorShape(yOutAxis);
//
   if (xOutAxis == xOutCorrAxis) {                      

// First output Direction axis corresponds to the first input Direction axis

      double t0 = (outPos[xOutCorrAxis] / pixelScale(xInAxis)) + oX;
      double t1 = (outPos[yOutCorrAxis] / pixelScale(yInAxis)) + oY;
//
      for (uint32_t j=0; j<nj; j++) {
         for (uint32_t i=0; i<ni; i++) {
           in2DPos(i,j,0) = double(i) / pixelScale(xInAxis) + t0;
           in2DPos(i,j,1) = double(j) / pixelScale(yInAxis) + t1;
         }
      }  
   } else if (xOutAxis == yOutCorrAxis) {

// First output Direction axis corresponds to the second input Direction axis

      double t0 = (outPos[yOutCorrAxis] / pixelScale(xInAxis)) + oX;
      double t1 = (outPos[xOutCorrAxis] / pixelScale(yInAxis)) + oY;
//
      for (uint32_t j=0; j<nj; j++) {
         for (uint32_t i=0; i<ni; i++) {
           in2DPos(i,j,0) = double(j) / pixelScale(xInAxis) + t0;
           in2DPos(i,j,1) = double(i) / pixelScale(yInAxis) + t1;
         }
      }  
   } else {
      throw(AipsError("Big trouble in make2CoordinateGrid"));
   }
//
   minInX = in2DPos(0,0,0);
   minInY = in2DPos(0,0,1);
   maxInX = in2DPos(ni-1,nj-1,0);
   maxInY = in2DPos(ni-1,nj-1,1);
}


template<class T>
void ImageRegrid<T>::findXYExtent (bool& missedIt, bool& allFailed,
                                   double& minInX, double& minInY, 
                                   double& maxInX, double& maxInY,  
                                   Cube<double>& in2DPos,
                                   const Matrix<bool>& succeed,
                                   uint32_t xInAxis, uint32_t yInAxis,
                                   uint32_t xOutAxis, uint32_t yOutAxis,
                                   const IPosition& outPos,
                                   const IPosition& outCursorShape,
                                   const IPosition& inShape) 
//
// Finds the blc and trc (absolute pixel coordinates) of the INPUT image 
// for the OUTPUT chunk being regridded.
//
{
   uint32_t ni = outCursorShape(xOutAxis);
   uint32_t nj = outCursorShape(yOutAxis);
               
// outPos is the BLC of this chunk in the output lattice
 
   uint32_t iOff = outPos[xOutAxis];
   uint32_t jOff = outPos[yOutAxis];
//
   IPosition blc(2);
   blc(0) = iOff;
   blc(1) = jOff;
   IPosition trc(2);
   trc(0) = iOff + ni - 1;
   trc(1) = jOff + nj - 1;
   IPosition minPos, maxPos;
//
   IPosition s = succeed.shape();
   if (blc(0)==0 && blc(1)==0 && trc(0)==(s(0)-1) && trc(1)==(s(1)-1)) {
// int16_t cut if we are going to use the full matrix

       allFailed = minmax (minInX, maxInX, minInY, maxInY, 
                           in2DPos.xyPlane(0), in2DPos.xyPlane(1), succeed);

   } else {
// Pull out the relevant piece

      allFailed = minmax (minInX, maxInX, minInY, maxInY, 
                          in2DPos.xyPlane(0)(blc,trc), 
                          in2DPos.xyPlane(1)(blc,trc), succeed(blc,trc));
   }
//
   if (!allFailed) {
      double ijMin = -0.5;
      double iMax = inShape(xInAxis) - 0.5;
      double jMax = inShape(yInAxis) - 0.5;
      missedIt  = (minInX<ijMin && maxInX<ijMin)  ||
                  (minInX>iMax && maxInX>iMax) ||
                  (minInY<ijMin && maxInY<ijMin)  ||
                  (minInY>jMax && maxInY>jMax);
   } else {
      missedIt = true;
   }
}

template<class T>
void ImageRegrid<T>::regrid2DMatrix(Lattice<T>& outCursor, 
                                    LatticeIterator<bool>*& outMaskIterPtr,
                                    const Interpolate2D& interp,
                                    ProgressMeter*& pProgressMeter,
                                    double& iPix,
                                    uint32_t nDim, 
                                    uint32_t xInAxis, uint32_t yInAxis, 
                                    uint32_t xOutAxis, uint32_t yOutAxis, 
                                    double scale,
                                    bool inIsMasked, bool outIsMasked,
                                    const IPosition& outPos,
                                    const IPosition& outCursorShape,
                                    const IPosition& inChunkShape,
                                    const IPosition& inChunkBlc,
                                    const IPosition& pixelAxisMap2,
                                    Array<T>& inDataChunk,
                                    Array<bool>*& inMaskChunkPtr,
                                    const Cube<double>& pix2DPos,
                                    const Matrix<bool>& succeed) {
  // 
  // Iterate through a stack of DirectionCoordinate planes and interpolate them
  //
  // Setup Navigator and tell it which axes are the Direction ones in case
  // of other degenerate axes
  
  IPosition axisPath;
  IPosition outCursorAxes(2, xOutAxis, yOutAxis);
  IPosition outCursorIterShape(2, outCursorShape(xOutAxis),
			       outCursorShape(yOutAxis));
  LatticeStepper outCursorIterStepper(outCursor.shape(), outCursorIterShape,
				      outCursorAxes, axisPath);
  LatticeIterator<T> outCursorIter(outCursor, outCursorIterStepper);
  //
  LatticeIterator<bool>* outMaskCursorIterPtr = 0;
  Lattice<bool>* outMaskCursorPtr = 0;
  if (outIsMasked) {
    outMaskCursorPtr = new ArrayLattice<bool>(outMaskIterPtr->rwCursor());
    outMaskCursorIterPtr = new LatticeIterator<bool>(*outMaskCursorPtr,
						     outCursorIterStepper);
  }
  //
  IPosition inChunkBlc2D(nDim, 0);
  IPosition inChunkTrc2D(nDim);
  inChunkTrc2D = inChunkShape - 1;
  //
  IPosition inChunk2DShape(2);
  inChunk2DShape[0] = inChunkTrc2D[xInAxis] - inChunkBlc2D[xInAxis] + 1;
  inChunk2DShape[1] = inChunkTrc2D[yInAxis] - inChunkBlc2D[yInAxis] + 1;
  //
  Vector<double> pix2DPos2(2);
  IPosition outPos3;
  bool interpOK;
  T result(0);
  //
  for (outCursorIter.reset(); !outCursorIter.atEnd(); outCursorIter++) {
    
    // outCursorIter.position is the location of the BLC of the current matrix
    // within the current
    // cursor (tile) of data. outPos3 is the location of the BLC of the
    // current matrix within
    // the full lattice
    outPos3 = outPos + outCursorIter.position();
    
    // Fish out the 2D piece of the inChunk relevant to this plane of the cursor
    for (uint32_t k=0; k<nDim; k++) {
      if (k!=xInAxis&& k!=yInAxis) {
	inChunkBlc2D[k] = outPos3[pixelAxisMap2[k]] - inChunkBlc[k];
	inChunkTrc2D[k] = inChunkBlc2D[k];
      }; 
    };
    //
    const Matrix<T>& inDataChunk2D =
      inDataChunk(inChunkBlc2D, inChunkTrc2D).reform(inChunk2DShape);
    Matrix<bool>* inMaskChunk2DPtr = 0;
    if (inIsMasked) {
      inMaskChunk2DPtr = 
	new Matrix<bool>((*inMaskChunkPtr)
			 (inChunkBlc2D, inChunkTrc2D).
			 reform(inChunk2DShape));
    };

    // Now work through each output pixel in the data Matrix and do the
    // interpolation
    uint32_t nCol = outCursorIter.matrixCursor().ncolumn();
    uint32_t nRow = outCursorIter.matrixCursor().nrow();
    Matrix<T> &outMCursor = outCursorIter.rwMatrixCursor();
    Matrix<bool> *outMaskMCursor = 0;
    if (outIsMasked) {
      outMaskMCursor = &(outMaskCursorIterPtr->rwMatrixCursor());
    };
    
    ArrayAccessor<bool, Axis<0> > sucp0;
    ArrayAccessor<bool, Axis<1> > sucp1(succeed);
    ArrayAccessor<T, Axis<0> > outMp0;
    ArrayAccessor<T, Axis<1> > outMp1(outMCursor);
    ArrayAccessor<bool, Axis<0> > outMaskMp0;
    ArrayAccessor<bool, Axis<1> > outMaskMp1;
    if (outIsMasked) outMaskMp1.init(*outMaskMCursor);
    uint32_t dpix2DPos = &pix2DPos(0,0,1) - &pix2DPos(0,0,0);

    for (uint32_t j=0; j<nCol; j++) {
      if (outIsMasked) outMaskMp0 = outMaskMp1;
      sucp0 = sucp1;
      outMp0 = outMp1;
      for (uint32_t i=0; i<nRow; i++) {
	if (! *sucp0) {
	  *outMp0 = 0.0;
	  if (outIsMasked) *outMaskMp0 = false;
	} else {
	  
	  // Now do the interpolation. pix2DPos(i,j,) is the absolute input
	  // pixel coordinate in the input lattice for the
	  // current output pixel.
	  uint32_t ii = outPos3[xOutAxis] + i;
	  uint32_t jj = outPos3[yOutAxis] + j;
	  const double *pix2Dp = &pix2DPos(ii,jj,0);
	  pix2DPos2[0] = *pix2Dp - inChunkBlc[xInAxis];
	  pix2DPos2[1] = *(pix2Dp + dpix2DPos) - inChunkBlc[yInAxis];
	  if (inIsMasked) {                     
	    interpOK = interp.interp(result, pix2DPos2, inDataChunk2D,
				     *inMaskChunk2DPtr);
	  } else {
	    interpOK = interp.interp(result, pix2DPos2, inDataChunk2D);
	  };
	  if (interpOK) {
	    *outMp0 = scale * result;
	    if (outIsMasked) *outMaskMp0 = true; 
	  } else {
	    *outMp0 = 0.0;
	    if (outIsMasked) *outMaskMp0 = false; 
	  };
	};
	sucp0++;
	outMp0++;
	outMaskMp0++;
      };
      sucp1++;
      outMp1++;
      outMaskMp1++;
    };
    //
    if (pProgressMeter) {
      pProgressMeter->update(iPix); 
      iPix += nCol*nRow;
    };
    //
    if (outIsMasked) (*outMaskCursorIterPtr)++;
    if (inIsMasked) delete inMaskChunk2DPtr;
  };
  //
  if (inIsMasked) delete inMaskChunkPtr;
  if (outIsMasked) {
    delete outMaskCursorIterPtr;
    delete outMaskCursorPtr;
  };
}

template<class T>
void ImageRegrid<T>::regrid1D (MaskedLattice<T>& outLattice,
                               const MaskedLattice<T>& inLattice,
                               const Coordinate& inCoord,
                               const Coordinate& outCoord,
                               const Vector<int32_t>& inPixelAxes,
                               const Vector<int32_t>& outPixelAxes,
                               int32_t inAxisInCoordinate,
                               int32_t outAxisInCoordinate,
                               const Vector<int32_t> pixelAxisMap,
                               typename Interpolate2D::Method method,
                               MFrequency::Convert& machine,
                               bool replicate,
                               bool useMachine, bool showProgress)

//
// Any output mask is overwritten
//
{
   const bool inIsMasked = inLattice.isMasked();
   const bool outIsMasked = outLattice.isMasked() &&
     outLattice.hasPixelMask() &&
     outLattice.pixelMask().isWritable();
//
   if (itsShowLevel>0) {
      cerr << "inIsMasked = " << inIsMasked << endl;
      cerr << "outIsMasked = " << outIsMasked << endl;
   }
//
   const IPosition& inShape = inLattice.shape();
   const IPosition& outShape = outLattice.shape();
   const uint32_t nDim = inLattice.ndim();
   const int32_t inPixelAxis = inPixelAxes(inAxisInCoordinate);
   const int32_t outPixelAxis = outPixelAxes(outAxisInCoordinate);

// Generate vector of pixel coordinates

   const uint32_t nLine = outShape(outPixelAxis);
   Vector<bool> failed(nLine);
   Block<typename NumericTraits<T>::BaseType> outX(nLine);
   bool allFailed = false;
   bool allGood = true;
//
   if (replicate) {
      float pixelScale = float(outShape(outPixelAxis)) /
	float(inShape(inPixelAxis));
      make1DCoordinateGrid (outX, pixelScale);
   } else {
      make1DCoordinateGrid (outX, failed, allFailed, allGood,
                            inCoord, outCoord, inAxisInCoordinate,
                            outAxisInCoordinate, machine, useMachine);
   }

// int16_t cut if all conversions cactus

   if (allFailed) {
      outLattice.set(0.0);
      if (outIsMasked) {
         Lattice<bool>& outMask = outLattice.pixelMask();
         outMask.set(false);
      }
      return;
   }

// Generate vector of input X values for interpolator

   const uint32_t nIn = inShape(inPixelAxis);
   Block<typename NumericTraits<T>::BaseType> inX(nIn);
   if (itsShowLevel>0) cerr << "inX = ";
   for (uint32_t i=0; i<nIn; i++) {
      inX[i] = i;
      if (itsShowLevel>0) cerr << inX[i] << ",";
   }
   if (itsShowLevel>0) cerr << endl;
//
   if (itsShowLevel>0) cerr << "outX = ";
   for (uint32_t i=0; i<outX.nelements(); i++) {
      if (itsShowLevel>0) cerr << outX[i] << ",";
   }
   if (itsShowLevel>0) cerr << endl;
//

// Make navigator and iterator for output data and mask. It is vital that
// the "niceShape" is the same for both iterators.  Because the mask and 
// lattice are both TempLattices, one might be on disk, one in core.
// Hence we pick one nice shape and use it on both iterators

   IPosition niceShape = outLattice.niceCursorShape();
   TiledLineStepper outStepper(outShape, niceShape, outPixelAxis);
   LatticeIterator<T> outIter(outLattice, outStepper);
//
   LatticeIterator<bool>* outMaskIterPtr = 0;
   if (outIsMasked) {
      Lattice<bool>& outMask = outLattice.pixelMask();    
      TiledLineStepper outMaskStepper(outShape, niceShape, outPixelAxis);
      outMaskIterPtr = new LatticeIterator<bool>(outMask, outMaskStepper);
   }
//
   IPosition inSubShape(nDim,1);
   IPosition inPos(nDim);
   inSubShape(inPixelAxis) = inShape(inPixelAxis);
//
   if (itsShowLevel>0) {
      cerr << "in, out pixel axis = " << inPixelAxis << ", " << outPixelAxis <<
	endl;  
      cerr << "shape in, shape out" << inShape << outShape << endl;  
      cerr << "inSubShape=" << inSubShape << endl;
   }

// Set interpolator method

   auto method1D =
        InterpolateArray1D<typename NumericTraits<T>::BaseType, T>::linear;
   if (method==Interpolate2D::NEAREST) {
      method1D = InterpolateArray1D<typename NumericTraits<T>::BaseType, T>::nearestNeighbour;
      if (itsShowLevel>0) {
         cerr << "Method = nearest" << endl;
      }
   } else if (method==Interpolate2D::LINEAR) {
      method1D = InterpolateArray1D<typename NumericTraits<T>::BaseType, T>::linear;
      if (itsShowLevel>0) {
         cerr << "Method = linear" << endl;
      }
   } else if (method==Interpolate2D::CUBIC) {
      method1D = InterpolateArray1D<typename NumericTraits<T>::BaseType, T>::spline;
      if (itsShowLevel>0) {
         cerr << "Method = cubic spline" << endl;
      }
   }

// Progress meter

   ProgressMeter* pProgressMeter = 0;
   if (showProgress) {
     double nMin = 0.0;
     double nMax = double(outLattice.shape().product()) /
       double(outIter.cursorShape().product());
     ostringstream oss;
     oss << "Axis " << outPixelAxis + 1 << " : Lines Regridded";
     pProgressMeter = new ProgressMeter(nMin, nMax, String(oss),
                                        String("Regridding"),
                                        String(""), String(""),
                                        true, max(1,int32_t(nMax/20)));
   }

// Iterate through output image by line

   bool goodIsTrue = true;
   bool extrapolate = false;
   Vector<bool> dummyOutMask(nLine);
   for (outIter.reset(); !outIter.atEnd(); outIter++) {
      const IPosition& outPos = outIter.position();
      if (itsShowLevel>1) {
         cerr << endl;
         cerr << "Output lattice iterator position = " <<  outPos << endl;
         cerr << "Output lattice iterator cursor shape = " << 
	   outIter.cursorShape()<< endl;
      }

// Get input vector of data and mask

      for (uint32_t i=0; i<nDim; i++) {
         inPos[i] = outPos[pixelAxisMap[i]];
      }
      const Vector<T>& inY = inLattice.getSlice(inPos, inSubShape, true);
      const Vector<bool>& inMask = inLattice.getMaskSlice(inPos, inSubShape,
							  true);

      if (itsShowLevel>1) {
         cerr << "inPos=" << inPos << endl;
         cerr << "inY=" << inY << endl;
         cerr << "inY=" << inMask << endl;
      }
//
      if (allGood) {
         if (outIsMasked) {
            InterpolateArray1D<typename NumericTraits<T>::BaseType, T>::interpolate(outIter.rwVectorCursor(),
                                                     outMaskIterPtr->
						     rwVectorCursor(),
                                                     outX, inX, inY, inMask,
                                                     method1D, goodIsTrue,
						     extrapolate);
         } else {
            InterpolateArray1D<typename NumericTraits<T>::BaseType, T>::interpolate(outIter.rwVectorCursor(),
                                                     dummyOutMask,
                                                     outX, inX, inY, inMask,
                                                     method1D, goodIsTrue,
						     extrapolate);
         }
      } else {

// AND the coordinate conversion success vector and the input mask

         if (outIsMasked) {
            InterpolateArray1D<typename NumericTraits<T>::BaseType, T>::interpolate(outIter.rwVectorCursor(),
                                                     outMaskIterPtr->
						     rwVectorCursor(),
                                                     outX, inX, inY, 
                                                    (failed && inMask),
                                                     method1D, goodIsTrue,
						     extrapolate);
         } else {
            InterpolateArray1D<typename NumericTraits<T>::BaseType, T>::interpolate(outIter.rwVectorCursor(),
                                                     dummyOutMask,
                                                     outX, inX, inY, 
                                                    (failed && inMask),
                                                     method1D, goodIsTrue,
						     extrapolate);
         }
      }
//
      if (itsShowLevel>1) {
         cerr << "outY = " << outIter.rwVectorCursor() << endl;
         if (outIsMasked) cerr << "outMask = " <<
			    outMaskIterPtr->rwVectorCursor() << endl;
      }
//
      if (outIsMasked) (*outMaskIterPtr)++;
      if (showProgress) pProgressMeter->update(double(outIter.nsteps()));
   } 
//
   if (outIsMasked) delete outMaskIterPtr;
   if (showProgress) delete pProgressMeter;
}



template<class T>
void ImageRegrid<T>::make1DCoordinateGrid (Block<typename NumericTraits<T>::BaseType>& outX,
                                           Vector<bool>& failed,
                                           bool& allFailed,
                                           bool& allGood,
                                           const Coordinate& inCoord,
                                           const Coordinate& outCoord,
                                           int32_t inAxisInCoordinate,
                                           int32_t outAxisInCoordinate,
                                           MFrequency::Convert& machine,
                                           bool useMachine)
{
// Precompute vector of output coordinates to interpolate data at

   double outPixel2, inPixel2;
   Vector<double> world, inPixel;
   Vector<double> outPixel = outCoord.referencePixel().copy();
//
   const uint32_t nLine = outX.nelements();
   failed.resize(nLine);
   allFailed = true;
   allGood = true;
   bool ok1 = false;
   bool ok2 = false;
   MFrequency inMVF, outMVF;
//
   if (useMachine) {

// If we are going to Stoke up the MFrequency machine it means
// we have a SpectralCoordinate; cast to it

      const SpectralCoordinate& inSpecCoord =
	dynamic_cast<const SpectralCoordinate&>(inCoord);
      const SpectralCoordinate& outSpecCoord =
	dynamic_cast<const SpectralCoordinate&>(outCoord);
//
      for (uint32_t i=0; i<nLine; i++) {

// Fill Coordinate pixel locations
//
         outPixel2 = i;
         ok1 = outSpecCoord.toWorld(outMVF, outPixel2);
         if (ok1) {
            inMVF = machine(outMVF).getValue();
            ok2 = inSpecCoord.toPixel(inPixel2, inMVF);
         } 
//
         if (!ok1 || !ok2) {
            failed(i) = true;
            allGood = false;
         } else {

// This one ok

            outX[i] = inPixel2;
            failed(i) = false;
            allFailed = false;
         }
      }
   } else {
      for (uint32_t i=0; i<nLine; i++) {

// Fill Coordinate pixel locations
//
         outPixel(outAxisInCoordinate) = i;
         ok1 = outCoord.toWorld(world, outPixel);
         if (ok1) ok2 = inCoord.toPixel(inPixel, world);
//
         if (!ok1 || !ok2) {
            failed(i) = true;
            allGood = false;
         } else {

// This one ok

            outX[i] = inPixel(inAxisInCoordinate);
            failed(i) = false;
            allFailed = false;
         }
      }
   }
//
   if (itsShowLevel>0) {
      cerr << "allFailed=" << allFailed << endl;
      cerr << "allGood =" << allGood << endl;
   }
   if (itsShowLevel>1) {
       cerr << "failed = " << failed << endl;
       cerr << "outX=";
       for (uint32_t i=0;i<nLine;i++) {
          cerr << outX[i] << ", ";
       }
       cerr << endl;
   }
}



template<class T>
void ImageRegrid<T>::make1DCoordinateGrid (Block<typename NumericTraits<T>::BaseType>& outX,
                                           typename NumericTraits<T>::BaseType pixelScale) const
{
   float oX = -0.5 + (1.0/2/pixelScale);
   const uint32_t nLine = outX.nelements();
   for (uint32_t i=0; i<nLine; i++) {
      outX[i]  = (typename NumericTraits<T>::BaseType(i) / pixelScale) + oX;
   }
}


template<class T>
void ImageRegrid<T>::_checkAxes(IPosition& outPixelAxes,
                               const IPosition& inShape,
                               const IPosition& outShape,
                               const Vector<int32_t>& pixelAxisMap1,
                               const CoordinateSystem& outCoords,
                               bool verbose)
{
   LogIO os(LogOrigin("ImageRegrid", __func__, WHERE));
   ThrowIf(inShape.nelements()==0, "The input shape is illegal");
   ThrowIf(
		   outShape.nelements()==0,
		   "The output shape is illegal"
   );
   int32_t n1 = outPixelAxes.nelements();
   const int32_t nOut = outShape.nelements();
   ThrowIf(
		   n1 > nOut,
      "You have specified more pixel axes than there are dimensions"
   );

// Fill in all axes if null pixelAxes given

   if (n1==0) {
      outPixelAxes = IPosition::makeAxisPath(nOut);
      n1 = outPixelAxes.nelements();
   }

// Check for Stokes and discard

   int32_t outCoordinate, outAxisInCoordinate;
   int32_t j = 0;
   for (int32_t i=0; i<n1; i++) {

// Find pixel axis in output coordinates if not yet done

      outCoords.findPixelAxis(outCoordinate, outAxisInCoordinate, 
                              outPixelAxes(i));
      if (outCoordinate==-1 || outAxisInCoordinate==-1) {
         ostringstream oss;
         oss << "Pixel axis " << outPixelAxes(i)+1 << 
                " has been removed from the output CoordinateSystem" << endl;
         ThrowCc(String(oss));
      }

// Find out the coordinate type and don't allow Stokes
 
      Coordinate::Type type = outCoords.type(outCoordinate);

      if (type==Coordinate::STOKES) {
         os << LogIO::POST << "The Stokes axis cannot be regridded "
	   "- removing from list" << endl;
      } else {
         bool ok = true;
         if (outShape(outPixelAxes(i))==1) {

// We can (will be able to) handle DirectionCoordinates which
// are degenerate in one axis.  The test for this is in Regrid2d
// Otherwise, regridding a one-pixel axis is useless.

            if (type!=Coordinate::DIRECTION) {
            	if (verbose) {
            		os << "Cannot regrid zero-based axis " << outPixelAxes(i)
            			<< " because it is of unit length - removing from list"
            			<< LogIO::POST;
            	}
              ok = false;
            }
         } 
//
         if (ok) {
            outPixelAxes(j) = outPixelAxes(i);
            j++;
         }
      }
   }
   outPixelAxes.resize(j,true);
   n1 = outPixelAxes.nelements();

// Check for range

   Vector<bool> found(nOut, false);
   for (int32_t i=0; i<n1; i++) {
      ThrowIf(
    	outPixelAxes(i)<0 || outPixelAxes(i)>=nOut,
         "Pixel axes are out of range"
      );
//
      ThrowIf(
    		  found(outPixelAxes(i)),
    		  "Specified pixel axes " + String::toString( outPixelAxes+1)
            	+ " are not unique"
      );
         found(outPixelAxes(i)) = true;
   }
// CHeck non-regriddded axis shapes are ok

   for (int32_t i=0; i<nOut; i++) {
      bool foundIt = false;
      for (int32_t j=0;j<n1; j++) {
         if (outPixelAxes(j)==i) {
            foundIt = true;
            break;
         }
      }

// pixelAxisMap1(i) says where pixel axis i in the output image
// is in the input image

      if (!foundIt && outShape(i) != inShape(pixelAxisMap1[i])) {
    	  ostringstream oss;
    	  oss << "Any axis not being regridded must have the same "
    	     << "input and output shapes. Output axis " << i
    	     << ", which corresponds to input axis "
    	     << pixelAxisMap1[i] << ", has a length of " << outShape(i)
    	     << ", whereas the corresponding input axis has length "
    	     << inShape(pixelAxisMap1[i]);
    	  ThrowCc(oss.str());
      }  
   }
}


template<class T>
void ImageRegrid<T>::findMaps (uint32_t nDim, 
                               Vector<int32_t>& pixelAxisMap1,
                               Vector<int32_t>& pixelAxisMap2,
                               const CoordinateSystem& inCoords,
                               const CoordinateSystem& outCoords) const
{

// Find mapping between CoordinateSystems
//
// worldAxisMap(i) is the location of world axis i (from the supplied
// coordinate system, cSys, in the current coordinate system. 
// worldAxisTranspose(i) is the location of world axis i (from the current
// coordinate system) in the supplied coordinate system, cSys.  

   Vector<int32_t> worldAxisTranspose, worldAxisMap;
   Vector<bool> worldRefChange;
   if (!outCoords.worldMap(worldAxisMap, worldAxisTranspose,
                           worldRefChange, inCoords)) {
      throw(AipsError(inCoords.errorMessage()));
   }

// pixelAxisMap1(i) says where pixel axis i in the output coordinate system
// is in the input coordinate system
// pixelAxisMap2(i) says where pixel axis i in the input coordinate system
// is in the output coordinate system

   pixelAxisMap1.resize(nDim);
   pixelAxisMap2.resize(nDim);
   for (uint32_t paOut=0; paOut<nDim; paOut++) {
      int32_t waOut = outCoords.pixelAxisToWorldAxis(paOut);
      int32_t waIn = worldAxisTranspose(waOut);
      int32_t paIn = inCoords.worldAxisToPixelAxis(waIn);      
//
      pixelAxisMap1[paOut] = paIn;
      pixelAxisMap2[paIn] = paOut;
   }
//
   if (itsShowLevel>0) {
      cerr << "worldmap, worldtranspose, refChange = " <<
             worldAxisMap << worldAxisTranspose << worldRefChange << endl;
      cerr << "pixelaxismap{1,2} = " << pixelAxisMap1 << pixelAxisMap2 << endl;
   }
}   



template<class T>
double ImageRegrid<T>::findScaleFactor(const Unit& units, 
                                       const CoordinateSystem& inCoords, 
                                       const CoordinateSystem& outCoords,
                                       int32_t inCoordinate, int32_t outCoordinate,
                                       LogIO& os) const
{
   double fac = 1.0;
   String t = units.getName();
   t.upcase();
   if (t==String("JY/PIXEL")) {

// Set units to the same thing

      if (inCoords.type(inCoordinate)==Coordinate::DIRECTION) {
         DirectionCoordinate inDir = inCoords.directionCoordinate(inCoordinate);
         DirectionCoordinate outDir = outCoords.directionCoordinate(outCoordinate);
//
         Vector<String> units(2);
         units.set("deg");
//
         inDir.setWorldAxisUnits(units);
         outDir.setWorldAxisUnits(units);
//
         const Vector<double>& incIn = inDir.increment();
         const Vector<double>& incOut = outDir.increment();
//
         fac = abs(incOut(0)*incOut(1) / incIn(0) / incIn(1));
         os << "Applying Jy/pixel scale factor of " << fac << endl;
      } else if (inCoords.type(inCoordinate)==Coordinate::LINEAR) {
         LinearCoordinate inLin = inCoords.linearCoordinate(inCoordinate);
         LinearCoordinate outLin = outCoords.linearCoordinate(outCoordinate);
//
         const Vector<String>& units = inLin.worldAxisUnits().copy();
         ThrowIf(
        	!outLin.setWorldAxisUnits(units),
            "Failed to set output and input LinearCoordinate axis units the same"
         );
//
         const Vector<double>& incIn = inLin.increment();
         const Vector<double>& incOut = outLin.increment();
//
         fac = abs(incOut(0)*incOut(1) / incIn(0) / incIn(1));
         os << "Applying Jy/pixel scale factor of " << fac << endl;
     }
   }
//
   return fac;
}

template<class T>
bool ImageRegrid<T>::minmax(double &minX, double &maxX, double &minY,
			    double &maxY,
                            const Array<double> &xData, 
                            const Array<double> &yData, 
                            const Array<bool> &mask) {                                 
  minX = 1.0e30;
  maxX = -1.0e30;
  minY = 1.0e30;
  maxY = -1.0e30;
  Array<bool>::const_iterator pMask = mask.begin();
  Array<double>::const_iterator pXend = xData.end();
  for (Array<double>::const_iterator pX = xData.begin(), pY = yData.begin();
       pX != pXend; ++pX, ++pY, ++pMask) {
    if (*pMask) {
      minX = minX < *pX ? minX : *pX;
      maxX = maxX > *pX ? maxX : *pX;
      minY = minY < *pY ? minY : *pY;
      maxY = maxY > *pY ? maxY : *pY;
    };
  };
  return (maxX < minX);
}

template<class T>
void ImageRegrid<T>::get2DCoordinateGrid (Cube<double> &grid,
					  Matrix<bool> &gridMask) const
{
   grid = its2DCoordinateGrid;
   gridMask = its2DCoordinateGridMask;
}


template<class T>
void ImageRegrid<T>::set2DCoordinateGrid (const Cube<double> &grid, 
                                          const Matrix<bool> &gridMask,
                                          bool)
{
   itsUser2DCoordinateGrid.resize();
   itsUser2DCoordinateGrid = grid;
   itsUser2DCoordinateGridMask.resize();
   itsUser2DCoordinateGridMask = gridMask;
}



} //# NAMESPACE CASACORE - END


#endif
