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

#ifndef IMAGES_IMAGESTATISTICS_TCC
#define IMAGES_IMAGESTATISTICS_TCC

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
ImageStatistics<T>::ImageStatistics (
    const ImageInterface<T>& image, LogIO& os,
    bool showProgress, bool forceDisk, bool clone
) : LatticeStatistics<T>(image, os, showProgress, forceDisk, clone),
    pInImage_p(0), blc_(IPosition(image.coordinates().nPixelAxes(), 0)),
    precision_(-1), _showRobust(false), _recordMessages(false),
    _listStats(true),  _messages() {
    ThrowIf(! setNewImage(image, clone), error_p);
}

template <class T>
ImageStatistics<T>::ImageStatistics (
    const ImageInterface<T>& image, bool showProgress,
    bool forceDisk, bool clone
) : LatticeStatistics<T>(image, showProgress, forceDisk, clone),
  pInImage_p(0), blc_(IPosition(image.coordinates().nPixelAxes(), 0)),
  precision_(-1), _showRobust(false), _recordMessages(false),
  _listStats(true), _messages() {
   ThrowIf(!setNewImage(image, clone), error_p);
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
    _inImPtrMgr.reset(other.pInImage_p->cloneII());
    pInImage_p = _inImPtrMgr.get();
}

// Assignment operator.  Storage image is not copied

template <class T>
ImageStatistics<T> &ImageStatistics<T>::operator=(const ImageStatistics<T> &other) {
   if (this != &other) {
      LatticeStatistics<T>::operator=(other);
      _inImPtrMgr.reset(other.pInImage_p->cloneII());
      pInImage_p = _inImPtrMgr.get();
      precision_ = other.precision_;
      blc_ = other.blc_;
      _showRobust = other._showRobust;
   }
   return *this;
}

template <class T>
ImageStatistics<T>::~ImageStatistics() {}

template <class T>
bool ImageStatistics<T>::setNewImage(
    const ImageInterface<T>& image, bool clone
) {
   if (!goodParameterStatus_p) {
      return false;
   }
   if (clone) {
       _inImPtrMgr.reset(image.cloneII());
       pInImage_p = _inImPtrMgr.get();
   }
   else {
       _inImPtrMgr.reset();
       pInImage_p = &image;
   }

// Pass it on to LatticeStatistics

   goodParameterStatus_p = this->setNewLattice(image, clone);
   return goodParameterStatus_p;
}

template <class T> bool ImageStatistics<T>::_getBeamArea(
    Array<double>& beamArea, String& msg
) const {
    ImageInfo ii = pInImage_p->imageInfo();
    bool hasMultiBeams = ii.hasMultipleBeams();
    bool hasSingleBeam = !hasMultiBeams && ii.hasBeam();
    const CoordinateSystem& cSys = pInImage_p->coordinates();

    // use contains() not == so moment maps are dealt with nicely
    if (! hasMultiBeams && ! hasSingleBeam ) {
        msg = "Image has no beam";
        return false;
    }
    else if (! cSys.hasDirectionCoordinate()) {
        msg = "Image does not have a direction coordinate";
        return false;
    }
    else {
        String imageUnits = pInImage_p->units().getName();
        imageUnits.upcase();
        if (! imageUnits.contains("JY/BEAM")) {
            msg = "Image brightness units not conformant with Jy/beam";
            return false;
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
        for (uint32_t i = 0; i < beamAreaShape.size(); i++) {
            beamAreaShape[i] = this->_storageLatticeShape()[i];
        }
    }
    beamArea.resize(beamAreaShape);
    beamArea.set(-1.0);
    if (hasSingleBeam) {
        beamArea.set(
            ii.getBeamAreaInPixels(-1, -1, dCoord)
        );
        return true;
    }

    // per plane beams
    // ensure both the spectral and polarization axes are display axes,
    // a degenerate axis is considered not to be a cursor axis for
    // this purpose since no aggregation along that axis actually occurs
    IPosition shape = pInImage_p->shape();

    bool foundSpec = ! cSys.hasSpectralAxis()
        || shape[cSys.spectralAxisNumber(false)] == 1;
    bool foundPol = ! cSys.hasPolarizationCoordinate()
        || shape[cSys.polarizationAxisNumber(false)] == 1;
    int32_t specAxis = foundSpec ? -1 : cSys.spectralAxisNumber();
    int32_t polAxis = foundPol ? -1 : cSys.polarizationAxisNumber();
    bool found = false;
    int32_t storageSpecAxis = -1;
    int32_t storagePolAxis = -1;

    for (uint32_t i = 0; i < displayAxes_p.size(); i++) {
        if (displayAxes_p[i] == specAxis) {
            foundSpec = true;
            storageSpecAxis = i;
        }
        else if (displayAxes_p[i] == polAxis) {
            foundPol = true;
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
        msg = "One or both of the spectral or polarization axes is "
            "not a display axis, not degenerate, and the image has multiple beams";
        return false;
    }
    const ImageBeamSet& beams = ii.getBeamSet();
    IPosition beamsShape = beams.shape();
    int32_t beamPolAxis = -1;
    if (cSys.hasPolarizationCoordinate()) {
        beamPolAxis = specAxis < 0 ? 0 : 1;
    }
    IPosition curPos(beamAreaShape.nelements(), 0);
    GaussianBeam curBeam;
    IPosition curBeamPos(beams.shape().nelements(), 0);
    IPosition axisPath = IPosition::makeAxisPath(beamAreaShape.size());
    ArrayPositionIterator iter(beamAreaShape, axisPath, false);
    double pixAreaRad2 = dCoord.getPixelArea().getValue("rad2");
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
    return true;
}

template <class T>
bool ImageStatistics<T>::listStats (bool hasBeam, const IPosition& dPos,
                                    const Matrix<AccumType>& stats)
//
// List the statistics for this row to the logger
//
// Inputs:
//   dPos    The location of the start of the cursor in the
//           storage image for this row
//   stats   Statistics matrix
// Outputs:
//   bool    Indicates coordinate transformations failed
//
{
   if (!haveLogger_p || ! _listStats) {

// We will consider this situation as successful

      return true;
   }

   os_p << endl;

// Set up the manipulators. We list the number of points as an integer so find
// out how big the field width needs to be.  Min of 6 so label fits.

   DataType type = whatType<T>();
   int32_t oDWidth = 14;
   if (type==TpComplex) {
      oDWidth = 2*oDWidth + 3;    // (x,y)
   }


// Have to convert LogIO object to ostream before can apply
// the manipulators

   int32_t oPrec = 6;
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
   Vector<double> pixels(1);
   pixels(0) = 1.0;
   IPosition blc(pInImage_p->ndim(),0);
   IPosition trc(pInImage_p->shape()-1);

   CoordinateSystem cSys = pInImage_p->coordinates();
   ImageUtilities::pixToWorld(sWorld, cSys,
                              displayAxes_p(0), cursorAxes_p, 
                              blc, trc, pixels, -1);
   String cName = 
   ImageUtilities::shortAxisName(cSys.worldAxisNames()(displayAxes_p(0)));
   int32_t oCWidth = max(uint32_t(cName.length()), uint32_t(sWorld(0).length())) + 1;
   
// Write headers

   const uint32_t nStatsAxes = cursorAxes_p.nelements();
   os_p << endl;
   int32_t len0;
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

   const uint32_t n1 = stats.shape()(0);
   sWorld.resize(n1);
   pixels.resize(n1);
//
   for (uint32_t j=0; j<n1; j++) pixels(j) = double(j);
   if (!ImageUtilities::pixToWorld(sWorld, cSys,
                                   displayAxes_p(0), cursorAxes_p, 
                                   blc, trc, pixels, -1)) return false;


// Write statistics to logger.  We write the pixel location
// relative to the parent image (zero based)

   for (uint32_t j=0; j<n1; j++) {
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

   return true;
}

template <class T>
void ImageStatistics<T>::showRobust(const bool show) {
    _showRobust = show;
}

template <class T>
void ImageStatistics<T>::displayStats(
    AccumType nPts, AccumType sum, AccumType median,
    AccumType medAbsDevMed, AccumType quartile, AccumType sumSq,
    AccumType mean, AccumType var, AccumType rms, AccumType sigma,
    AccumType dMin, AccumType dMax, AccumType q1, AccumType q3
) {
    if ( ! doList_p ) {
        // Nothing to display, listing data is turned off.
        return;
    }
    const CoordinateSystem& cSys(pInImage_p->coordinates());
    // Have to convert LogIO object to ostream before can apply
    // the manipulators.  Also formatting Complex numbers with
    // the setw manipulator fails, so I go to a lot of trouble
    // with ostringstreams (which are useable only once).
    const int32_t oPrec = 6;
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
        Array<double> beamArea;
        String msg;
        bool hasBeam = _getBeamArea(beamArea, msg);
        bool isFluxDensity;
        Quantum<AccumType> qFlux = _flux(
            isFluxDensity, sum, hasBeam ? *(beamArea.begin()) : 0
        );
        AccumType val = qFlux.getValue();
        String unit = qFlux.getFullUnit().getName();
        oss << "         -- flux" << (isFluxDensity ? " density" : "")
            << " [flux]:" << (isFluxDensity ? "" : "        ")
            << "                    " << val << " " << unit;
        messages.push_back(oss.str());
        oss.str("");
    }
    bool doBiweight = this->_getAlgorithm() == StatisticsData::BIWEIGHT;
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
        if (maxPos_p.size() > 0) {
            IPosition myMaxPos = maxPos_p + blc_;
            oss << "         -- position of max value (pixel) [maxpos]: " << myMaxPos;
            messages.push_back(oss.str());
            oss.str("");
        }
        if (minPos_p.size() > 0) {
            IPosition myMinPos = minPos_p + blc_;
            oss << "         -- position of min value (pixel) [minpos]: " << myMinPos;
            messages.push_back(oss.str());
            oss.str("");
        }
        if (maxPos_p.size() > 0) {
            oss << "         -- position of max value (world) [maxposf]: " 
                << CoordinateUtil::formatCoordinate (maxPos_p, cSys, precision_);
            messages.push_back(oss.str());
            oss.str("");
        }
        if (minPos_p.size() > 0) {
            oss << "         -- position of min value (world) [minposf]: "
                << CoordinateUtil::formatCoordinate (minPos_p, cSys, precision_);
            messages.push_back(oss.str());
            oss.str("");
        }
        if (! doBiweight) {
            oss << "         -- Sum of pixel values [sum]:               " << sum << " " << sbunit;
            messages.push_back(oss.str());
            oss.str("");
            oss << "         -- Sum of squared pixel values [sumsq]:     " << sumSq
                << " " << bunitSquared;
            messages.push_back(oss.str());
            oss.str("");
        }
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
        if (! doBiweight) {
            oss << "        -- Variance of the pixel values :           " << var << " "
                << sbunit;
            messages.push_back(oss.str());
            oss.str("");
        }
        oss << "        -- Standard deviation of the Mean [sigma]:  " << sigma << " "
            << sbunit;
        messages.push_back(oss.str());
        oss.str("");
        if (! doBiweight) {
            oss << "        -- Root mean square [rms]:                  " << rms << " "
                << sbunit;
            messages.push_back(oss.str());
            oss.str("");
        }
        if (_showRobust) {
            oss << "        -- Median of the pixel values [median]:     " << median <<
                " " << sbunit;
            messages.push_back(oss.str());
            oss.str("");
            oss << "        -- Median of the deviations [medabsdevmed]: " << medAbsDevMed
                << " " << sbunit;
            messages.push_back(oss.str());
            oss.str("");
            oss << "        -- IQR [quartile]:                          " << quartile << " " <<
                sbunit;
            messages.push_back(oss.str());
            oss.str("");
            oss << "        -- First quartile [q1]:                     " << q1 << " " << sbunit;
            messages.push_back(oss.str());
            oss.str("");
            oss << "        -- Third quartile [q3]:                     " << q3 << " " << sbunit;
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

template <class T> Quantum<typename ImageStatistics<T>::AccumType> ImageStatistics<T>::_flux(
    bool& isFluxDensity, AccumType sum, double beamAreaInPixels
) const {
    ThrowIf(
        ! _canDoFlux(),
        "This object cannot be used to determine flux densities"
    );
    isFluxDensity = true;
    Quantum<AccumType> flux(0, "");
    String sbunit = pInImage_p->units().getName();
    bool intensityBeamBased = false;
    if (sbunit.contains("K")) {
        String areaUnit = "arcsec2";
        flux.setUnit(sbunit + "." + areaUnit);
        flux.setValue(
            sum * pInImage_p->coordinates().directionCoordinate().getPixelArea().getValue(areaUnit)
        );
    }
    else {
        flux.setUnit("Jy");
        if (sbunit.contains("/beam")) {
            intensityBeamBased = true;
            uint32_t iBeam = sbunit.find("/beam");
            if (beamAreaInPixels > 0) {
                flux.setValue(sum/beamAreaInPixels);
            }
            flux.setUnit(sbunit.substr(0, iBeam) + sbunit.substr(iBeam+5));
        }
    }
    if (pInImage_p->coordinates().hasSpectralAxis()) {

        int32_t specAxis = pInImage_p->coordinates().spectralAxisNumber(false);
        Vector<int32_t>::const_iterator myend = cursorAxes_p.end();
        if (
            pInImage_p->shape()[specAxis] > 1
            && std::find(cursorAxes_p.begin(), myend, specAxis) != myend
        ) {
            // integrate over nondegenerate spectral axis
            if (intensityBeamBased && pInImage_p->imageInfo().hasMultipleBeams()) {
                // the resolution varies by channel, so the previously computed
                // value based on the passed in sum is bogus because the beam area
                // varies
                vector<int32_t> newCursorAxes = cursorAxes_p.tovector();
                newCursorAxes.erase(
                    std::find(newCursorAxes.begin(), newCursorAxes.end(), specAxis)
                );
                ImageStatistics<T> newStats(*this);
                newStats.setAxes(Vector<int32_t>(newCursorAxes));
                Array<AccumType> fluxDensities;
                newStats.getStatistic(fluxDensities, LatticeStatsBase::FLUX);
                flux.setValue(casacore::sum(fluxDensities));
            }
            const SpectralCoordinate& spCoord = pInImage_p->coordinates().spectralCoordinate();
            Quantity inc(0, "");
            if (spCoord.restFrequency() > 0) {
                double v0, v1;
                if (
                    spCoord.pixelToVelocity(v0, 0)
                    && spCoord.pixelToVelocity(v1, 1)
                ) {
                    inc = Quantity(abs(v1 - v0), spCoord.velocityUnit());
                }
            }
            else {
                inc = Quantity(spCoord.increment()[0], spCoord.worldAxisUnits()[0]);
            }
            flux.setValue(flux.getValue()*inc.getValue());
            Quantity q1(1, flux.getUnit());
            Quantity q2(1, inc.getUnit());
            flux.setUnit((q1*q2).getUnit());
            isFluxDensity = false;
        }
    }
    if (isFluxDensity) {
        // the brightness unit may already imply this
        // image has been integrated over a spectral
        // range, such as the case for moment images
        UnitVal u = flux.getFullUnit().getValue();
        std::vector<UnitVal> fluxDensityUnits(2);
        fluxDensityUnits[0] = UnitVal(1, "Jy");
        fluxDensityUnits[1] = UnitVal(1, "K*arcsec2");
        std::vector<UnitVal> spectralUnits(2);
        spectralUnits[0] = UnitVal(1, "km/s");
        spectralUnits[1] = UnitVal(1, "Hz");
        std::vector<UnitVal>::const_iterator fiter = fluxDensityUnits.begin();
        std::vector<UnitVal>::const_iterator fend = fluxDensityUnits.end();
        std::vector<UnitVal>::const_iterator send = spectralUnits.end();
        while (isFluxDensity && fiter != fend) {
            std::vector<UnitVal>::const_iterator siter = spectralUnits.begin();
            while (isFluxDensity && siter != send) {
                if (u == (*fiter) * (*siter)) {
                    isFluxDensity = false;
                }
                ++siter;
            }
            ++fiter;
        }
    }
    return flux;
}

template <class T> bool ImageStatistics<T>::_computeFlux(
    Array<AccumType>& flux,const Array<AccumType>& npts, const Array<AccumType>& sum
) {
    Array<double> beamArea;
    String msg;
    bool gotBeamArea = _getBeamArea(beamArea, msg);
    if (! gotBeamArea) {
        String unit = pInImage_p->units().getName();
        unit.downcase();
        if (unit.contains("/beam") && ! pInImage_p->imageInfo().hasMultipleBeams()) {
            os_p << LogIO::WARN << "Unable to compute flux density: "
                << msg << LogIO::POST;
            return false;
        }
    }
    ReadOnlyVectorIterator<AccumType> sumIt(sum);
    ReadOnlyVectorIterator<AccumType> nPtsIt(npts);
    VectorIterator<AccumType> fluxIt(flux);
    PtrHolder<ReadOnlyVectorIterator<double> > beamAreaIter(
        gotBeamArea ? new ReadOnlyVectorIterator<double>(beamArea) : 0
    );
    uint32_t n1 = nPtsIt.vector().nelements();
    while (!nPtsIt.pastEnd()) {
        for (uint32_t i=0; i<n1; ++i) {
            if (nPtsIt.vector()(i) > 0.5) {
                bool isFluxDensity;
                fluxIt.vector()(i) = _flux(
                    isFluxDensity, sumIt.vector()(i), gotBeamArea ? beamAreaIter->vector()(i) : 0
                ).getValue();
            }
        }
        nPtsIt.next();
        sumIt.next();
        fluxIt.next();
        if (gotBeamArea) {
            beamAreaIter->next();
        }
    }
    return true;
}

template <class T>  bool ImageStatistics<T>::_computeFlux(
    Quantum<AccumType>& flux, AccumType sum, const IPosition& pos,
    bool posInLattice
) {
    Array<double> beamArea;
    String msg;
    bool unused;
    if (_getBeamArea(beamArea, msg)) {
        IPosition beamPos = pos;
        if (posInLattice) {
            this->_latticePosToStoragePos(beamPos, pos);
        }
        flux = _flux(unused, sum, beamArea(beamPos)).getValue();
    }
    else {
        String unit = pInImage_p->units().getName();
        unit.downcase();
        if (unit.contains("/beam")) {
            return false;
        }
        flux = _flux(unused, sum, 0).getValue();
    }
    return true;
}

template <class T> bool ImageStatistics<T>::_canDoFlux() const {
    const CoordinateSystem& csys = pInImage_p->coordinates();
    if (! csys.hasDirectionCoordinate()) {
        return false;
    }
    String unit = pInImage_p->units().getName();
    bool unitOK = unit.contains("K")
        || (
            pInImage_p->imageInfo().hasBeam()
            && unit.contains("/beam")
        );
    if (! unitOK) {
        return false;
    }
    bool cursorHasDirection = false;
    Vector<int32_t> dirAxesNumbers = csys.directionAxesNumbers();
    Vector<int32_t>::const_iterator dIter = dirAxesNumbers.begin();
    Vector<int32_t>::const_iterator dEnd = dirAxesNumbers.end();
    Vector<int32_t>::const_iterator curBegin = cursorAxes_p.begin();
    Vector<int32_t>::const_iterator curEnd = cursorAxes_p.end();
    while (dIter != dEnd) {
        if(
            std::find(curBegin, curEnd, *dIter) != curEnd
        ) {
            cursorHasDirection = true;
            break;
        }
        ++dIter;
    }
    if (! cursorHasDirection) {
        return false;
    }
    std::set<int32_t> okCursorAxes;
    okCursorAxes.insert(dirAxesNumbers.begin(), dirAxesNumbers.end());
    IPosition shape = pInImage_p->shape();
    if (csys.hasSpectralAxis()) {
        int32_t specAxis = csys.spectralAxisNumber(false);
        if (
            shape[specAxis] > 1
            && std::find(curBegin, curEnd, specAxis) != curEnd
            && csys.spectralCoordinate().isTabular()
        ) {
            // spectral axis is tabular,
            // spectral axis is nondegenerate and a cursor axis
            // FIXME the tabular constraints can
            // be removed, but that will take a bit of work
            return false;
        }
        okCursorAxes.insert(specAxis);
    }
    Vector<int32_t>::const_iterator curIter = curBegin;
    while (curIter != curEnd) {
        if (
            shape[*curIter] > 1
            && std::find(okCursorAxes.begin(), okCursorAxes.end(), *curIter)
            == okCursorAxes.end()
        ) {
            // There is a cursor axis that is nondegenerate and is neither
            // a spectral nor a direction axis
            return false;
        }
        ++curIter;
    }
    return true;
}

template <class T>
void ImageStatistics<T>::setPrecision(int32_t precision) {
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
int32_t ImageStatistics<T>::getPrecision() const {
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
   const uint32_t nDisplayAxes = displayAxes_p.nelements();
   ostringstream oss;
   if (nDisplayAxes > 1) {
      Vector<String> sWorld(1);
      Vector<double> pixels(1);
      IPosition blc(pInImage_p->ndim(),0);
      IPosition trc(pInImage_p->shape()-1);

      for (uint32_t j=1; j<nDisplayAxes; j++) {
         int32_t worldAxis = cSys.pixelAxisToWorldAxis(displayAxes_p(j));
         String name = cSys.worldAxisNames()(worldAxis);
         pixels(0) = double(locInLattice(dPos,false)(j));

         if (!ImageUtilities::pixToWorld (sWorld, cSys,
                                     displayAxes_p(j), cursorAxes_p,
                                     blc, trc, pixels, -1)) return;

         oss <<  ImageUtilities::shortAxisName(name)
             << " = " << locInLattice(dPos,true)(j) << " (" << sWorld(0) << ")";
         if (j < nDisplayAxes-1) oss << ", ";
      }
      hLabel = String(oss);
   }
}

template <class T>
void ImageStatistics<T>::listMinMax(ostringstream& osMin,
                                    ostringstream& osMax,
                                    int32_t oWidth, DataType type)
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

}

#endif
