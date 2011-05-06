//# tSubImage.cc: Test program for class SubImage
//# Copyright (C) 1998,1999,2000,2001,2003
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

#ifndef IMAGES_IMAGEFITTER_H
#define IMAGES_IMAGEFITTER_H

#include <measures/Measures/Stokes.h>
#include <lattices/LatticeMath/Fit2D.h>
#include <casa/Logging/LogIO.h>
#include <components/ComponentModels/ComponentList.h>
#include <images/Images/ImageInterface.h>
#include <images/Images/ImageInputProcessor.h>

#include <components/ComponentModels/ComponentType.h>
#include <casa/namespace.h>

namespace casa {

    class ImageFitter {
        // <summary>
        // Top level interface to ImageAnalysis::fitsky to handle inputs, bookkeeping etc and
        // ultimately call fitsky to do fitting
        // </summary>

        // <reviewed reviewer="" date="" tests="" demos="">
        // </reviewed>

        // <prerequisite>
        // </prerequisite>

        // <etymology>
        // Fits components to sources in images (ImageSourceComponentFitter was deemed to be to long
        // of a name)
        // </etymology>

        // <synopsis>
        // ImageFitter is the top level interface for fitting image source components. It handles most
        // of the inputs, bookkeeping etc. It can be instantiated and its one public method, fit,
        // run from either a C++ app or python.
        // </synopsis>

        // <example>
        // <srcblock>
        // ImageFitter fitter(...)
        // fitter.fit()
        // </srcblock>
        // </example>

        public:
			enum CompListWriteControl {
				NO_WRITE,
				WRITE_NO_REPLACE,
				OVERWRITE
			};

            // constructor approprate for API calls.
            // Parameters:
            // <ul>
            // <li>imagename - the name of the input image in which to fit the models</li>
            // <li>box - A 2-D rectangular box in which to use pixels for the fitting, eg box=100,120,200,230
            // In cases where both box and region are specified, box, not region, is used.</li>
            // <li>region - Named region to use for fitting</li>
			// <li>regionPtr - A pointer to a region. Note there are unfortunately several different types of
			// region records throughout CASA. In this case, it must be a Record produced by creating a
			// region via a RegionManager method.
            // <li>chanInp - Zero-based channel number on which to do the fit. Only a single channel can be
            // specified.</li>
            // <li>stokes - Stokes plane on which to do the fit. Only a single Stokes parameter can be
            // specified.</li>
            // <li> maskInp - Mask (as LEL) to use as a way to specify which pixels to use </li>
            // <li> includepix - Pixel value range to include in the fit. includepix and excludepix
            // cannot be specified simultaneously. </li>
            // <li> excludepix - Pixel value range to exclude from fit</li>
            // <li> residualInp - Name of residual image to save. Blank means do not save residual image</li>
            // <li> modelInp - Name of the model image to save. Blank means do not save model image</li>

			// DEPRECATED, DO NOT USE FOR NEW CODE AND CHANGE OLD CODE TO USE ONE OF THE CONSTRUCTORS BELOW
            ImageFitter(
                const String& imagename, const String& region, const String& box="",
                const uInt chanInp=0, const String& stokes="I",
                const String& maskInp="",
                const Vector<Float>& includepix = Vector<Float>(0),
                const Vector<Float>& excludepix = Vector<Float>(0),
                const String& residualInp="", const String& modelInp="",
                const String& estiamtesFilename="", const String& logfile="",
                const Bool& append=True, const String& newEstimatesInp="",
                const String& compListName="",
                const CompListWriteControl writeControl=NO_WRITE
            ); 

            // DEPRECATED, DO NOT USE FOR NEW CODE AND CHANGE OLD CODE TO USE ONE OF THE CONSTRUCTORS BELOW
            ImageFitter(
                const String& imagename, const Record* regionPtr, const String& box="",
                const uInt chanInp=0, const String& stokes="I",
                const String& maskInp="",
                const Vector<Float>& includepix = Vector<Float>(0),
                const Vector<Float>& excludepix = Vector<Float>(0),
                const String& residualInp="", const String& modelInp="",
                const String& estiamtesFilename="", const String& logfile="",
                const Bool& append=True, const String& newEstimatesInp="",
                const String& compListName="",
                const CompListWriteControl writeControl=NO_WRITE
            );

            // use these constructors when you already have a pointer to a valid ImageInterface object

            ImageFitter(
                const ImageInterface<Float>*& image, const String& region, const String& box="",
                const uInt chanInp=0, const String& stokes="I",
                const String& maskInp="",
                const Vector<Float>& includepix = Vector<Float>(0),
                const Vector<Float>& excludepix = Vector<Float>(0),
                const String& residualInp="", const String& modelInp="",
                const String& estiamtesFilename="", const String& logfile="",
                const Bool& append=True, const String& newEstimatesInp="",
                const String& compListName="",
                const CompListWriteControl writeControl=NO_WRITE
            );

            ImageFitter(
                const ImageInterface<Float>*& image, const Record* regionPtr, const String& box="",
                const uInt chanInp=0, const String& stokes="I",
                const String& maskInp="",
                const Vector<Float>& includepix = Vector<Float>(0),
                const Vector<Float>& excludepix = Vector<Float>(0),
                const String& residualInp="", const String& modelInp="",
                const String& estiamtesFilename="", const String& logfile="",
                const Bool& append=True, const String& newEstimatesInp="",
                const String& compListName="",
                const CompListWriteControl writeControl=NO_WRITE
            );

            // destructor
            ~ImageFitter();

            // Do the fit. If componentList is specified, store the fitted components in
            // that object.
            ComponentList fit();

            // Did the fit converge? Throw AipsError if the fit has not yet been done.
			Bool converged() const;

        private:
            LogIO *_log;
            ImageInterface<Float> *_image;
            Record _regionRecord;
            uInt _chan;
            String _stokesString, _mask, _residual, _model, _logfileName,
				regionString, estimatesString, _newEstimatesFileName, _compListName;
            Vector<Float> includePixelRange, excludePixelRange;
            ComponentList estimates, _results;
            Vector<String> fixed;
            Bool logfileAppend, _fitConverged, fitDone, _noBeam, _deleteImageOnDestruct;
            Vector<Quantity> _peakIntensities, _peakIntensityErrors, _fluxDensityErrors,
				_fluxDensities, _majorAxes, _majorAxisErrors, _minorAxes, _minorAxisErrors,
				_positionAngles, _positionAngleErrors;
            Record _residStats, inputStats;
            Double chiSquared;
            String _kludgedStokes;
            CompListWriteControl _writeControl;

            // does the lion's share of constructing the object, ie checks validity of
            // inputs, etc.
            void _construct(
                const String& imagename, const String& box, const String& regionName,
                const Record* regionPtr, const String& estimatesFilename
            );

            void _construct(
                const ImageInterface<Float> *image, const String& box, const String& regionName,
                const Record* regionPtr, const String& estimatesFilename
            );

            Vector<ImageInputProcessor::OutputStruct> _getOutputs();

            void _finishConstruction(const String& estimatesFilename);

            // summarize the results in a nicely formatted string
            String _resultsToString();

            //summarize the size details in a nicely formatted string
            String _sizeToString(const uInt compNumber) const;

            String _fluxToString(uInt compNumber) const;

           // String _fluxToString2(uInt compNumber) const;


            String _spectrumToString(uInt compNumber) const;

            // write output to log file
            void _writeLogfile(const String& output) const;

            // Write the estimates file using this fit.
            void _writeNewEstimatesFile() const;

            // Set the flux densities and peak intensities of the fitted components.
            void _setFluxes();

            // Set the convolved sizes of the fitted components.
            void _setSizes();

			void _getStandardDeviations(Double& inputStdDev, Double& residStdDev) const;

			void _getRMSs(Double& inputRMS, Double& residRMS) const;

			Double _getStatistic(const String& type, const Record& stats) const;

			String _statisticsToString() const;

			void setErrors(const Record& residStats);

			void _writeCompList();
    };
}

#endif
