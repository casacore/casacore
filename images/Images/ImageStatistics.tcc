//# ImageStatistics.cc: generate statistics from an image
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003,2004
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

#include <casacore/images/Images/ImageStatistics.h>

#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/DirectionCoordinate.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>  
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/images/Images/ImageUtilities.h>
#include <casacore/images/Images/ImageInterface.h>
#include <casacore/images/Images/ImageExprParse.h>
#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/lattices/LatticeMath/LatticeStatistics.h>
#include <casacore/lattices/LatticeMath/LattStatsSpecialize.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/LinearSearch.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/DataType.h>

#include <casacore/casa/iostream.h>
#include <casacore/casa/iomanip.h>
#include <casacore/casa/stdlib.h>
#include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Public functions

template <class T>
ImageStatistics<T>::ImageStatistics (const ImageInterface<T>& image,
                                     LogIO& os, 
                                     Bool showProgress,
                                     Bool forceDisk)
// 
// Constructor
//
: LatticeStatistics<T>(image, os, showProgress, forceDisk),
  pInImage_p(0), blc_(IPosition(image.coordinates().nPixelAxes(), 0)),
  precision_(-1), _showRobust(False), _recordMessages(False), _messages()
{
   if (!setNewImage(image)) {
      os_p << error_p << LogIO::EXCEPTION;
   }
}

template <class T>
ImageStatistics<T>::ImageStatistics (const ImageInterface<T>& image,
                                     Bool showProgress,
                                     Bool forceDisk)
// 
// Constructor
//
: LatticeStatistics<T>(image, showProgress, forceDisk),
  pInImage_p(0), blc_(IPosition(image.coordinates().nPixelAxes(), 0)),
  precision_(-1), _showRobust(False), _recordMessages(False), _messages()
{
   if (!setNewImage(image)) {
      os_p << error_p << LogIO::EXCEPTION;
   }
}


template <class T>
ImageStatistics<T>::ImageStatistics(const ImageStatistics<T> &other) 
//
// Copy constructor.  Storage image is not copied.
//
: LatticeStatistics<T>(other),
  pInImage_p(0), blc_(other.getBlc()), precision_(other.getPrecision()),
  _showRobust(other._showRobust)
{
   pInImage_p = other.pInImage_p->cloneII();
}

// Assignment operator.  Storage image is not copied

template <class T>
ImageStatistics<T> &ImageStatistics<T>::operator=(const ImageStatistics<T> &other) {
   if (this != &other) {
      LatticeStatistics<T>::operator=(other);
      if (pInImage_p!=0) {
    	  delete pInImage_p;
      }
      pInImage_p = other.pInImage_p->cloneII();
      precision_ = other.precision_;
      blc_ = other.blc_;
      _showRobust = other._showRobust;
   }
   return *this;
}

template <class T>
ImageStatistics<T>::~ImageStatistics()
//
// Destructor.  
//
{
   delete pInImage_p;
   pInImage_p = 0;
}



template <class T>
Bool ImageStatistics<T>::setNewImage(const ImageInterface<T>& image)
{ 
   if (!goodParameterStatus_p) {
      return False;
   }

// Make a clone of the image

   if (pInImage_p!=0) {
	   delete pInImage_p;
   }
   pInImage_p = image.cloneII();


// Pass it on to LatticeStatistics

   goodParameterStatus_p = this->setNewLattice(image);
//
   return goodParameterStatus_p;
}

template <class T> Bool ImageStatistics<T>::_getBeamArea(
    Array<Double>& beamArea
) const {
	ImageInfo ii = pInImage_p->imageInfo();
	Bool hasMultiBeams = ii.hasMultipleBeams();
	Bool hasSingleBeam = !hasMultiBeams && ii.hasBeam();
	const CoordinateSystem& cSys = pInImage_p->coordinates();

	// use contains() not == so moment maps are dealt with nicely
	if (! hasMultiBeams && ! hasSingleBeam ) {
		_messages.push_back(String("Image has no beam, cannot compute flux"));
		return False;
	}
	else if (! cSys.hasDirectionCoordinate()) {
		_messages.push_back(String("Image does not have a direction coordinate, cannot computer flux"));
		return False;
	}
	else {
		String imageUnits = pInImage_p->units().getName();
		imageUnits.upcase();
		if (! imageUnits.contains("JY/BEAM")) {
		_messages.push_back(String("Image brightness units not conformant with Jy/beam, cannot compute flux"));
		return False;
		}
	}
	DirectionCoordinate dCoord = cSys.directionCoordinate();
	IPosition beamAreaShape;
	if (this->_storageLatticeShape().size() == 1) {
		beamAreaShape.resize(1);
		beamAreaShape[0] = 1;
	}
	else {
		beamAreaShape.resize(this->_storageLatticeShape().size() - 1);
		for (uInt i = 0; i < beamAreaShape.size(); i++) {
			beamAreaShape[i] = this->_storageLatticeShape()[i];
		}
	}
	beamArea.resize(beamAreaShape);
	beamArea.set(-1.0);
	if (hasSingleBeam) {
		beamArea.set(
			ii.getBeamAreaInPixels(-1, -1, dCoord)
		);
		return True;
	}
	// per plane beams
	// ensure both the spectral and polarization axes are display axes
	Bool foundSpec = !cSys.hasSpectralAxis() || False;
	Bool foundPol = !cSys.hasPolarizationCoordinate() || False;
	Int specAxis = foundSpec ? -1 : cSys.spectralAxisNumber();
	Int polAxis = foundPol ? -1 : cSys.polarizationAxisNumber();
	Bool found = False;
	const ImageBeamSet& beams = ii.getBeamSet();
	Int storageSpecAxis = -1;
	Int storagePolAxis = -1;

	for (uInt i = 0; i < displayAxes_p.size(); i++) {
		if (displayAxes_p[i] == specAxis) {
			foundSpec = True;
			storageSpecAxis = i;
		}
		else if (displayAxes_p[i] == polAxis) {
			foundPol = True;
			storagePolAxis = i;
		}
		found = foundSpec && foundPol;
		if (found) {
			break;
		}
	}
	if (! found) {
		// if per-plane beams, either the spectral axis and/or the
		// polarization axis is not a display axis
		// or else the image has no beam
		return False;
	}

	IPosition beamsShape = beams.shape();
	if (cSys.hasSpectralAxis()) {
		AlwaysAssert(
			beamsShape[0] == beamAreaShape[storageSpecAxis],
			AipsError
		);
	}
	Int beamPolAxis = -1;
	if (cSys.hasPolarizationCoordinate()) {
		beamPolAxis = specAxis < 0 ? 0 : 1;
		AlwaysAssert(
			beamsShape[beamPolAxis] == beamAreaShape[storagePolAxis],
			AipsError
		);
	}
	IPosition curPos(beamAreaShape.nelements(), 0);
	GaussianBeam curBeam;
	IPosition curBeamPos(beams.shape().nelements(), 0);
	IPosition axisPath = IPosition::makeAxisPath(beamAreaShape.size());
	ArrayPositionIterator iter(beamAreaShape, axisPath, False);
	Double pixAreaRad2 = dCoord.getPixelArea().getValue("rad2");
	while (!iter.pastEnd()) {
		const IPosition curPos = iter.pos();
		if (storageSpecAxis >= 0) {
			curBeamPos[0] = curPos[storageSpecAxis];
		}
		if (storagePolAxis >= 0) {
			curBeamPos[beamPolAxis] = curPos[storagePolAxis];
		}
		curBeam = beams(curBeamPos[0], curBeamPos[1]);
		beamArea(curPos) = curBeam.getArea("rad2")/pixAreaRad2;
		iter.next();
	}
	return True;
}

template <class T>
Bool ImageStatistics<T>::listStats (Bool hasBeam, const IPosition& dPos,
                                    const Matrix<AccumType>& stats)
//
// List the statistics for this row to the logger
//
// Inputs:
//   dPos    The location of the start of the cursor in the
//           storage image for this row
//   stats   Statistics matrix
// Outputs:
//   Bool    Indicates coordinate transformations failed
//
{
   if (!haveLogger_p) {

// We will consider this situation as successful

      return True;
   }

   os_p << endl;

// Set up the manipulators. We list the number of points as an integer so find
// out how big the field width needs to be.  Min of 6 so label fits.

   T* dummy(0);
   DataType type = whatType(dummy);
   Int oDWidth = 14;
   if (type==TpComplex) {
      oDWidth = 2*oDWidth + 3;    // (x,y)
   }


// Have to convert LogIO object to ostream before can apply
// the manipulators

   Int oPrec = 6;
   setStream(os_p.output(), oPrec);

// Write the pixel and world coordinate of the higher order display axes to the logger

   if (displayAxes_p.nelements()>1) {
      String hLabel, xLabel;
      getLabels(hLabel, xLabel, dPos);
      os_p << hLabel << endl;
   }

// Find the width of the field into which we are going to write the coordinate value
// of the first display axis.  Do this by formatting a dummy value.

   Vector<String> sWorld(1);
   Vector<Double> pixels(1);
   pixels(0) = 1.0;
   IPosition blc(pInImage_p->ndim(),0);
   IPosition trc(pInImage_p->shape()-1);

   CoordinateSystem cSys = pInImage_p->coordinates();
   ImageUtilities::pixToWorld(sWorld, cSys,
                              displayAxes_p(0), cursorAxes_p, 
                              blc, trc, pixels, -1);
   String cName = 
     ImageUtilities::shortAxisName(cSys.worldAxisNames()(displayAxes_p(0)));
   Int oCWidth = max(uInt(cName.length()), uInt(sWorld(0).length())) + 1;
   
// Write headers

   const uInt nStatsAxes = cursorAxes_p.nelements();
   os_p << endl;
   Int len0;
   if (nStatsAxes == 1) {
      os_p << "Profile ";
      len0 = 8;
   }
   else if (nStatsAxes == 2) {
      os_p << "Plane ";
      len0 = 6;
   }
   else if (nStatsAxes == 3) {
      os_p << "Cube ";
      len0 = 5;
   }
   else {
      os_p << "Hyper-cube ";
      len0 = 11;
   }

   os_p.output() << setw(oCWidth) << cName;
   os_p.output() << setw(oDWidth) << "Npts";
   os_p.output() << setw(oDWidth) << "Sum";
   if (hasBeam) os_p.output() << setw(oDWidth) << "FluxDensity";
   os_p.output() << setw(oDWidth) << "Mean"; 
   if (doRobust_p) os_p.output() << setw(oDWidth) << "Median"; 
   os_p.output() << setw(oDWidth) << "Rms";
   os_p.output() << setw(oDWidth) << "Std dev";
   os_p.output() << setw(oDWidth) << "Minimum";
   os_p.output() << setw(oDWidth) << "Maximum" << endl;


// Convert pixel coordinates Vector of the first display axis to world coordinates

   const uInt n1 = stats.shape()(0);
   sWorld.resize(n1);
   pixels.resize(n1);
//
   for (uInt j=0; j<n1; j++) pixels(j) = Double(j);
   if (!ImageUtilities::pixToWorld(sWorld, cSys,
                                   displayAxes_p(0), cursorAxes_p, 
                                   blc, trc, pixels, -1)) return False;


// Write statistics to logger.  We write the pixel location
// relative to the parent image (zero based)

   for (uInt j=0; j<n1; j++) {
      os_p.output() << setw(len0)     << j+blcParent_p(displayAxes_p(0));
      os_p.output() << setw(oCWidth)   << sWorld(j);
      ostringstream os00; setStream(os00, oPrec);
      os00 << stats.column(NPTS)(j);   
      os_p.output() << setw(oDWidth)   << String(os00);   
      if (LattStatsSpecialize::hasSomePoints(stats.column(NPTS)(j))) {

// Convert to strings.
   
         ostringstream os0, os1, os2, os3, os4, os5, os6, os7, os8, os9;
         setStream(os0, oPrec); setStream(os1, oPrec); setStream(os2, oPrec);
         setStream(os3, oPrec); setStream(os4, oPrec); setStream(os5, oPrec);
         setStream(os6, oPrec); setStream(os7, oPrec); setStream(os8, oPrec);
         setStream(os9, oPrec); 
//
         os0 << stats.column(SUM)(j);
         if (hasBeam) os1 << stats.column(FLUX)(j);
         os2 << stats.column(MEAN)(j);
         if (doRobust_p) os8 << stats.column(MEDIAN)(j);
         os3 << stats.column(RMS)(j);
         os4 << stats.column(SIGMA)(j);
         os5 << stats.column(MIN)(j);
         os6 << stats.column(MAX)(j);
//
         os_p.output() << setw(oDWidth)   << String(os0);
         if (hasBeam) os_p.output() << setw(oDWidth)   << String(os1);
         os_p.output() << setw(oDWidth)   << String(os2);
         if (doRobust_p) os_p.output() << setw(oDWidth)   << String(os8);
         os_p.output() << setw(oDWidth)   << String(os3);
         os_p.output() << setw(oDWidth)   << String(os4);
         os_p.output() << setw(oDWidth)   << String(os5);
         os_p.output() << setw(oDWidth)   << String(os6);
      }
      os_p.output() << endl;
   }
   os_p.post();

   return True;
}

template <class T>
void ImageStatistics<T>::showRobust(const Bool show) {
	_showRobust = show;
}

template <class T>
void ImageStatistics<T>::displayStats(
		AccumType nPts, AccumType sum, AccumType median,
		AccumType medAbsDevMed, AccumType quartile, AccumType sumSq,
		AccumType mean, AccumType var, AccumType rms, AccumType sigma,
		AccumType dMin, AccumType dMax, AccumType, AccumType
) {
	if ( ! doList_p ) {
		// Nothing to display, listing data is turned off.
		return;
	}


	// Find world coordinates of min and max. We list pixel coordinates
	// of min/max relative to the start of the parent lattice
	//if (!fixedMinMax_p) {

	CoordinateSystem cSys(pInImage_p->coordinates());
	String minPosString = CoordinateUtil::formatCoordinate (minPos_p, cSys, precision_);
	String maxPosString = CoordinateUtil::formatCoordinate (maxPos_p, cSys, precision_);
	//}


	// Have to convert LogIO object to ostream before can apply
	// the manipulators.  Also formatting Complex numbers with
	// the setw manipulator fails, so I go to a lot of trouble
	// with ostringstreams (which are useable only once).
	const Int oPrec = 6;
	Int oWidth = 14;
	T* dummy = 0;
	DataType type = whatType(dummy);
	if (type==TpComplex) {
		oWidth = 32;
	}
	setStream(os_p.output(), oPrec);

	Unit bunit = pInImage_p->units();
	String sbunit = bunit.getName();
	Quantity uSquared(1, bunit);
	uSquared *= uSquared;
	String bunitSquared = uSquared.getUnit();

	///////////////////////////////////////////////////////////////////////
	//                 Do Values Section
	///////////////////////////////////////////////////////////////////////
	vector<String> messages;
	messages.push_back("Values --- ");
	ostringstream oss;
	if (_canDoFlux()) {
		Array<Double> beamArea;
		Bool hasBeam = _getBeamArea(beamArea);
		Quantum<AccumType> qFlux = _flux(
			sum, hasBeam ? *(beamArea.begin()) : 0
		);
		AccumType val = qFlux.getValue();
		String unit = qFlux.getFullUnit().getName();
		oss << "         -- flux density [flux]:                    "
			<< val << " " << unit;
		messages.push_back(oss.str());
		oss.str("");
	}

	IPosition myMaxPos = maxPos_p;
	IPosition myMinPos = minPos_p;
	myMaxPos += blc_;
	myMinPos += blc_;

	if (LattStatsSpecialize::hasSomePoints(nPts)) {
		oss << "         -- number of points [npts]:                " << nPts;
		messages.push_back(oss.str());
		oss.str("");
		oss << "         -- maximum value [max]:                    " << dMax << " " << sbunit;
		messages.push_back(oss.str());
		oss.str("");
		oss << "         -- minimum value [min]:                    " << dMin << " " << sbunit;
		messages.push_back(oss.str());
		oss.str("");
		oss << "         -- position of max value (pixel) [maxpos]: " << myMaxPos;
		messages.push_back(oss.str());
		oss.str("");
		oss << "         -- position of min value (pixel) [minpos]: " << myMinPos;
		messages.push_back(oss.str());
		oss.str("");
		oss << "         -- position of max value (world) [maxposf]: " << maxPosString;
		messages.push_back(oss.str());
		oss.str("");
		oss << "         -- position of min value (world) [minposf]: " << minPosString;
		messages.push_back(oss.str());
		oss.str("");
		oss << "         -- Sum of pixel values [sum]:               " << sum << " " << sbunit;
		messages.push_back(oss.str());
		oss.str("");
		oss << "         -- Sum of squared pixel values [sumsq]:     " << sumSq
				<< " " << bunitSquared;
		messages.push_back(oss.str());
		oss.str("");
	}

	///////////////////////////////////////////////////////////////////////
	//                 Do Statistical Section
	///////////////////////////////////////////////////////////////////////
	messages.push_back("Statistics --- ");
	Vector<LogIO::Command> priorities(0);
	if (LattStatsSpecialize::hasSomePoints(nPts)) {
		oss << "        -- Mean of the pixel values [mean]:         " << mean << " "
				<< sbunit;
		messages.push_back(oss.str());
		oss.str("");
		oss << "        -- Variance of the pixel values :           " << var << " "
				<< sbunit << LogIO::POST;
		messages.push_back(oss.str());
		oss.str("");
		oss << "        -- Standard deviation of the Mean [sigma]:  " << sigma << " "
				<< sbunit;
		messages.push_back(oss.str());
		oss.str("");
		oss << "        -- Root mean square [rms]:                  " << rms << " "
				<< sbunit;
		messages.push_back(oss.str());
		oss.str("");
		if (_showRobust) {
			oss << "        -- Median of the pixel values [median]:     " << median <<
				" " << sbunit;
			messages.push_back(oss.str());
			oss.str("");
			oss << "        -- Median of the deviations [medabsdevmed]: " << medAbsDevMed
				<< " " << sbunit;
			messages.push_back(oss.str());
			oss.str("");
			oss << "        -- Quartile [quartile]:                     " << quartile << " " <<
				sbunit;
			messages.push_back(oss.str());
			oss.str("");
		}
		priorities.resize(messages.size());
		priorities = LogIO::NORMAL;
	}
	else {
		messages.push_back("No valid points found ");
		priorities.resize(messages.size());
		priorities = LogIO::NORMAL;
		priorities[priorities.size()-1] = LogIO::WARN;
	}
	Vector<LogIO::Command>::const_iterator jiter = priorities.begin();
	for (
		vector<String>::const_iterator iter=messages.begin();
		iter!=messages.end(); iter++, jiter++
	) {
		os_p << *jiter << *iter << LogIO::POST;
		if (_recordMessages) {
			_messages.push_back(*iter);
		}
	}
}

template <class T> Quantum<typename casacore::NumericTraits<T>::PrecisionType> ImageStatistics<T>::_flux(
	AccumType sum, Double beamAreaInPixels
) const {
	ThrowIf(
		! _canDoFlux(),
		"This object cannot be used to determine flux densities"
	);
	AccumType flux = 0;
	String fUnit;
	String sbunit = pInImage_p->units().getName();
	if (sbunit.contains("K")) {
		String areaUnit = "arcsec2";
		fUnit = sbunit + "." + areaUnit;
		flux = sum * pInImage_p->coordinates().directionCoordinate().getPixelArea().getValue(areaUnit);
	}
	else {
		fUnit = "Jy";
		if (sbunit.contains("/beam")) {
			uInt iBeam = sbunit.find("/beam");
			flux = sum/beamAreaInPixels;
			fUnit = sbunit.substr(0, iBeam) + sbunit.substr(iBeam+5);
		}
	}
	return Quantum<AccumType>(flux, fUnit);
}

template <class T> Bool ImageStatistics<T>::_canDoFlux() const {
	String unit = pInImage_p->units().getName();
	return unit.contains("K")
		|| (
			pInImage_p->imageInfo().hasBeam()
			&& unit.contains("/beam")
		);
}

template <class T>
void ImageStatistics<T>::setPrecision(Int precision) {
	precision_ = precision;
}

template <class T>
void ImageStatistics<T>::setBlc(const IPosition& blc) {
	blc_ = blc;
}

template <class T>
IPosition ImageStatistics<T>::getBlc() const {
	return blc_;
}

template <class T>
Int ImageStatistics<T>::getPrecision() const {
	return precision_;
}

template <class T>
void ImageStatistics<T>::getLabels(String& hLabel, String& xLabel, const IPosition& dPos) const
//
// Get labels for top of plot and listing for the higher order axes
// and get the label for the X-axis when plotting
//
{
   CoordinateSystem cSys = pInImage_p->coordinates();
   xLabel = cSys.worldAxisNames()(displayAxes_p(0)) + " (pixels)";

   hLabel =String("");
   const uInt nDisplayAxes = displayAxes_p.nelements();
   ostringstream oss;
   if (nDisplayAxes > 1) {
      Vector<String> sWorld(1);
      Vector<Double> pixels(1);
      IPosition blc(pInImage_p->ndim(),0);
      IPosition trc(pInImage_p->shape()-1);

      for (uInt j=1; j<nDisplayAxes; j++) {
         Int worldAxis = cSys.pixelAxisToWorldAxis(displayAxes_p(j));
         String name = cSys.worldAxisNames()(worldAxis);
         pixels(0) = Double(locInLattice(dPos,False)(j));

         if (!ImageUtilities::pixToWorld (sWorld, cSys,
                                     displayAxes_p(j), cursorAxes_p,
                                     blc, trc, pixels, -1)) return;

         oss <<  ImageUtilities::shortAxisName(name)
             << " = " << locInLattice(dPos,True)(j) << " (" << sWorld(0) << ")";
         if (j < nDisplayAxes-1) oss << ", ";
      }
      hLabel = String(oss);
   }
}


template <class T>
void ImageStatistics<T>::listMinMax(ostringstream& osMin,
                                    ostringstream& osMax,
                                    Int oWidth, DataType type)
{
   if (!fixedMinMax_p) {

// Find world coordinates of min and max. We list pixel coordinates
// of min/max relative to the start of the parent lattice

      CoordinateSystem cSys(pInImage_p->coordinates());
      String minPosString = CoordinateUtil::formatCoordinate (minPos_p, cSys);
      String maxPosString = CoordinateUtil::formatCoordinate (maxPos_p, cSys);
//
      os_p << "Minimum value "; 
      os_p.output() << setw(oWidth) << osMin.str();
      if (type==TpFloat && minPos_p.size() > 0) {
          os_p <<  " at " << blcParent_p + minPos_p+1 << " (" << minPosString << ")" << endl;
      }
      os_p.post();
//
      os_p << "Maximum value ";
      os_p.output() << setw(oWidth) << osMax.str();
      if (type==TpFloat && maxPos_p.size() > 0) {
         os_p <<  " at " << blcParent_p + maxPos_p+1 << " (" << maxPosString << ")" << endl;
      }
      os_p << endl;
      os_p.post();
   }
}


} //# NAMESPACE CASACORE - END

