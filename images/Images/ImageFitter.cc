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

#include <casa/Containers/HashMap.h>
#include <casa/Containers/HashMapIter.h>

#include <casa/IO/FilebufIO.h>
#include <casa/IO/FiledesIO.h>

#include <casa/Quanta/Quantum.h>
#include <casa/Quanta/Unit.h>
#include <casa/Quanta/UnitMap.h>
#include <casa/Quanta/MVAngle.h>
#include <casa/Quanta/MVTime.h>
#include <casa/OS/Time.h>
#include <casa/Utilities/Precision.h>

#include <components/ComponentModels/Flux.h>
#include <components/ComponentModels/SpectralModel.h>

#include <images/IO/FitterEstimatesFileParser.h>
#include <images/Images/ImageAnalysis.h>
#include <images/Images/ImageFitter.h>
#include <images/Images/ImageMetaData.h>
#include <images/Images/ImageStatistics.h>
#include <images/Images/FITSImage.h>
#include <images/Images/MIRIADImage.h>
#include <images/Regions/RegionManager.h>

#include <images/Regions/WCUnion.h>
#include <images/Regions/WCBox.h>
#include <lattices/Lattices/LCRegion.h>

#include <components/ComponentModels/SkyComponent.h>
#include <components/ComponentModels/ComponentShape.h>

#include <components/ComponentModels/GaussianShape.h>

namespace casa {

    ImageFitter::ImageFitter(
        const String& imagename, const String& region, const String& box,
        const uInt chanInp, const String& stokes,
        const String& maskInp, const Vector<Float>& includepix,
        const Vector<Float>& excludepix, const String& residualInp,
        const String& modelInp, const String& estimatesFilename,
        const String& logfile, const Bool& append,
        const String& newEstimatesInp, const String& compListName,
        const CompListWriteControl writeControl
    ) : _log(new LogIO()), _image(0), _chan(chanInp), _stokesString(stokes),
		_mask(maskInp), _residual(residualInp),_model(modelInp), _logfileName(logfile),
		regionString(""), estimatesString(""), _newEstimatesFileName(newEstimatesInp),
		_compListName(compListName),
		includePixelRange(includepix), excludePixelRange(excludepix),
		estimates(), fixed(0), logfileAppend(append), _fitConverged(False),
		fitDone(False), _noBeam(False), _deleteImageOnDestruct(True), _peakIntensities(),
		_writeControl(writeControl) {
        *_log << LogOrigin("ImageFitter", __FUNCTION__);
        *_log << LogIO::WARN << "This constructor has been deprecated and will be removed "
        	<< "in an upcoming version. Please modify existing code not to use another constructor"
        	<< LogIO::POST;
        _construct(imagename, box, region, 0, estimatesFilename);
    }

    ImageFitter::ImageFitter(
        const String& imagename, const Record* regionPtr, const String& box,
        const uInt chanInp, const String& stokes,
        const String& maskInp, const Vector<Float>& includepix,
        const Vector<Float>& excludepix, const String& residualInp,
        const String& modelInp, const String& estimatesFilename,
        const String& logfile, const Bool& append,
        const String& newEstimatesInp, const String& compListName,
        const CompListWriteControl writeControl
    ) : _log(new LogIO()), _image(0), _chan(chanInp), _stokesString(stokes),
		_mask(maskInp), _residual(residualInp),_model(modelInp), _logfileName(logfile),
		regionString(""), estimatesString(""), _newEstimatesFileName(newEstimatesInp),
		_compListName(compListName),
		includePixelRange(includepix), excludePixelRange(excludepix),
		estimates(), fixed(0), logfileAppend(append), _fitConverged(False),
		fitDone(False), _noBeam(False), _deleteImageOnDestruct(True), _peakIntensities(),
		_writeControl(writeControl) {
        *_log << LogOrigin("ImageFitter", __FUNCTION__);
        *_log << LogIO::WARN << "This constructor has been deprecated and will be removed "
        	<< "in an upcoming version. Please modify existing code not to use another constructor"
        	<< LogIO::POST;
        _construct(imagename, box, "", regionPtr, estimatesFilename);
    }

    ImageFitter::ImageFitter(
        const ImageInterface<Float>*& image, const String& region, const String& box,
        const uInt chanInp, const String& stokes,
        const String& maskInp, const Vector<Float>& includepix,
        const Vector<Float>& excludepix, const String& residualInp,
        const String& modelInp, const String& estimatesFilename,
        const String& logfile, const Bool& append,
        const String& newEstimatesInp, const String& compListName,
        const CompListWriteControl writeControl
    ) : _log(new LogIO()), _image(image->cloneII()), _chan(chanInp), _stokesString(stokes),
		_mask(maskInp), _residual(residualInp),_model(modelInp), _logfileName(logfile),
		regionString(""), estimatesString(""), _newEstimatesFileName(newEstimatesInp),
		_compListName(compListName),
		includePixelRange(includepix), excludePixelRange(excludepix),
		estimates(), fixed(0), logfileAppend(append), _fitConverged(False),
		fitDone(False), _noBeam(False), _deleteImageOnDestruct(False), _peakIntensities(),
		_writeControl(writeControl) {
        _construct(_image, box, region, 0, estimatesFilename);
    }

    ImageFitter::ImageFitter(
        const ImageInterface<Float>*& image, const Record* regionPtr, const String& box,
        const uInt chanInp, const String& stokes,
        const String& maskInp, const Vector<Float>& includepix,
        const Vector<Float>& excludepix, const String& residualInp,
        const String& modelInp, const String& estimatesFilename,
        const String& logfile, const Bool& append,
        const String& newEstimatesInp, const String& compListName,
        const CompListWriteControl writeControl
    ) : _log(new LogIO()), _image(image->cloneII()), _chan(chanInp), _stokesString(stokes),
		_mask(maskInp), _residual(residualInp),_model(modelInp), _logfileName(logfile),
		regionString(""), estimatesString(""), _newEstimatesFileName(newEstimatesInp),
		_compListName(compListName),
		includePixelRange(includepix), excludePixelRange(excludepix),
		estimates(), fixed(0), logfileAppend(append), _fitConverged(False),
		fitDone(False), _noBeam(False), _deleteImageOnDestruct(False), _peakIntensities(),
		_writeControl(writeControl) {
        _construct(_image, box, "", regionPtr, estimatesFilename);
    }


    ImageFitter::~ImageFitter() {
        delete _log;
        if (_deleteImageOnDestruct) {
        	delete _image;
        }
    }

    ComponentList ImageFitter::fit() {
        *_log << LogOrigin("ImageFitter", __FUNCTION__);
        Array<Float> residPixels;
        Array<Bool> residMask;
        Bool converged;
        uInt ngauss = estimates.nelements() > 0 ? estimates.nelements() : 1;
        Vector<String> models(ngauss);
        models.set("gaussian");
        Bool fit = True;
        Bool deconvolve = False;
        Bool list = True;
        String errmsg;
        Record estimatesRecord;

        estimates.toRecord(errmsg, estimatesRecord);
		ImageAnalysis myImage(_image);
		Vector<String> allowFluxUnits(1);
		allowFluxUnits[0] = "Jy.km/s";
		FluxRep<Double>::setAllowedUnits(allowFluxUnits);

		try {
        	_results = myImage.fitsky(
            	residPixels, residMask, converged,
            	inputStats, _residStats, chiSquared,
            	_regionRecord, _chan, _stokesString,
            	_mask, models, estimatesRecord, fixed,
            	includePixelRange, excludePixelRange,
            	fit, deconvolve, list, _residual, _model
        	);
		}
		catch (AipsError err) {
    		*_log << LogIO::WARN << "Fit failed to converge because of exception: "
			<< err.getMesg() << LogIO::POST;
			converged = false;
		}
		FluxRep<Double>::clearAllowedUnits();

        fitDone = True;
        _fitConverged = converged;
        if(converged) {
        	_setFluxes();
        	_setSizes();
        	_writeCompList();
        }
        String resultsString = _resultsToString();
        if (converged && ! _newEstimatesFileName.empty()) {
        	_writeNewEstimatesFile();
        }
        *_log << LogIO::NORMAL << resultsString << LogIO::POST;
        if (! _logfileName.empty()) {
        	_writeLogfile(resultsString);
        }
        return _results;
    }

    Bool ImageFitter::converged() const {
    	if (!fitDone) {
    		throw AipsError("fit has not yet been performed");
    	}
    	return _fitConverged;
    }

	void ImageFitter::_getStandardDeviations(Double& inputStdDev, Double& residStdDev) const {
		inputStdDev = _getStatistic("sigma", inputStats);
		residStdDev = _getStatistic("sigma", _residStats);
	}

	void ImageFitter::_getRMSs(Double& inputRMS, Double& residRMS) const {
		inputRMS = _getStatistic("rms", inputStats);
		residRMS = _getStatistic("rms", _residStats);
	}

	Double ImageFitter::_getStatistic(const String& type, const Record& stats) const {
		Vector<Double> statVec;
		stats.get(stats.fieldNumber(type), statVec);
		return statVec[0];
	}

	Vector<ImageInputProcessor::OutputStruct> ImageFitter::_getOutputs() {
    	LogOrigin logOrigin("ImageFitter", __FUNCTION__);
        *_log << logOrigin;

        ImageInputProcessor::OutputStruct residualIm;
        residualIm.label = "residual image";
        residualIm.outputFile = &_residual;
        residualIm.required = False;
        residualIm.replaceable = True;
        ImageInputProcessor::OutputStruct modelIm;
        modelIm.label = "model image";
        modelIm.outputFile = &_model;
        modelIm.required = False;
        modelIm.replaceable = True;
        ImageInputProcessor::OutputStruct newEstFile;
        newEstFile.label = "new estiamtes file";
        newEstFile.outputFile = &_newEstimatesFileName;
        newEstFile.required = False;
        newEstFile.replaceable = True;
        ImageInputProcessor::OutputStruct logFile;
        logFile.label = "log file";
        logFile.outputFile = &_logfileName;
        logFile.required = False;
        logFile.replaceable = True;

        Vector<ImageInputProcessor::OutputStruct> outputs(4);
        outputs[0] = residualIm;
        outputs[1] = modelIm;
        outputs[2] = newEstFile;
        outputs[3] = logFile;

        return outputs;
        // Vector<Coordinate::Type> reqCoordTypes(1, Coordinate::DIRECTION);
        //reqCoordTypes[0] = Coordinate::DIRECTION;
        // return inputProcessor;
	}

    void ImageFitter::_construct(
        const String& imagename, const String& box, const String& regionName,
        const Record* regionPtr, const String& estimatesFilename
    ) {
    	LogOrigin logOrigin("ImageFitter", __FUNCTION__);
        *_log << logOrigin;
        *_log << LogIO::WARN
        	<< "THIS CONSTRUCTOR IS DEPRECATED. PLEASE USE A CONSTRUCTOR WHICH "
        	<< "TAKES A VALID IMAGE INTERFACE POINTER" << LogIO::POST;
        Vector<ImageInputProcessor::OutputStruct> outputs = _getOutputs();
        //ImageInputProcessor inputProcessor = _beginConstruction();

        String diagnostics;
        Vector<Coordinate::Type> reqCoordTypes(1, Coordinate::DIRECTION);
		ImageInputProcessor inputProcessor;

        inputProcessor.process(
        	_image, _regionRecord, diagnostics, &outputs,
        	_stokesString, imagename, regionPtr,
        	regionName, box, String::toString(_chan),
        	RegionManager::USE_FIRST_STOKES, False,
        	&reqCoordTypes
        );
        _finishConstruction(estimatesFilename);
    }

    void ImageFitter::_construct(
        const ImageInterface<Float> *image, const String& box, const String& regionName,
        const Record* regionPtr, const String& estimatesFilename
    ) {
    	LogOrigin logOrigin("ImageFitter", __FUNCTION__);
        *_log << logOrigin;
        Vector<ImageInputProcessor::OutputStruct> outputs = _getOutputs();
        //ImageInputProcessor inputProcessor = _beginConstruction();

        String diagnostics;
        Vector<Coordinate::Type> reqCoordTypes(1, Coordinate::DIRECTION);
		ImageInputProcessor inputProcessor;

        inputProcessor.process(
        	_regionRecord, diagnostics, &outputs,
        	_stokesString, image, regionPtr,
        	regionName, box, String::toString(_chan),
        	RegionManager::USE_FIRST_STOKES, False,
        	&reqCoordTypes
        );
        _finishConstruction(estimatesFilename);
    }

    void ImageFitter::_finishConstruction(const String& estimatesFilename) {
    	LogOrigin logOrigin("ImageFitter", __FUNCTION__);
        *_log << logOrigin;
        // <todo> kludge because Flux class is really only made for I, Q, U, and V stokes

        String iquv = "IQUV";
        _kludgedStokes = (iquv.index(_stokesString) == String::npos) || _stokesString.empty()
        	? "I" : _stokesString;
        // </todo>
        //_doRegion(box, regionName, regionPtr);
        if(estimatesFilename.empty()) {
        	*_log << LogIO::NORMAL << "No estimates file specified, so will attempt to find and fit one gaussian."
        		<< LogIO::POST;
        }
        else {
        	FitterEstimatesFileParser parser(estimatesFilename, *_image);
        	estimates = parser.getEstimates();
        	estimatesString = parser.getContents();
        	fixed = parser.getFixed();
        	Record rec;
        	String errmsg;
        	estimates.toRecord(errmsg, rec);
        	*_log << LogIO::NORMAL << "File " << estimatesFilename << " has " << estimates.nelements()
        		<< " specified, so will attempt to fit that many gaussians " << LogIO::POST;
        }
    }

    String ImageFitter::_resultsToString() {
    	ostringstream summary;
    	summary << "****** Fit performed at " << Time().toString() << "******" << endl << endl;
    	summary << "Input parameters ---" << endl;
    	summary << "       --- imagename:           " << _image->name() << endl;
    	summary << "       --- region:              " << regionString << endl;
    	summary << "       --- channel:             " << _chan << endl;
    	summary << "       --- stokes:              " << _stokesString << endl;
    	summary << "       --- mask:                " << _mask << endl;
    	summary << "       --- include pixel ragne: " << includePixelRange << endl;
    	summary << "       --- exclude pixel ragne: " << excludePixelRange << endl;
    	summary << "       --- initial estimates:   " << estimatesString << endl;

    	if (converged()) {
        	if (_noBeam) {
        		*_log << LogIO::WARN << "Flux density not reported because "
        				<< "there is no clean beam in image header so these quantities cannot "
        				<< "be calculated" << LogIO::POST;
        	}
    		summary << _statisticsToString() << endl;
    		for (uInt i = 0; i < _results.nelements(); i++) {
    			summary << "Fit on " << _image->name(True) << " component " << i << endl;
    			summary << _results.component(i).positionToString(&(_image->coordinates())) << endl;
    			summary << _sizeToString(i) << endl;
    			summary << _fluxToString(i) << endl;
    			summary << _spectrumToString(i) << endl;
    		}
    	}
    	else {
    		summary << "*** FIT FAILED ***" << endl;
    	}
    	return summary.str();
    }

    String ImageFitter::_statisticsToString() const {
    	ostringstream stats;
    	// TODO It is not clear how this chi squared value is calculated and atm it does not
    	// appear to be useful, so don't report it. In the future, investigate more deeply
    	// how it is calculated and see if a useful value for reporting can be derived from
    	// it.
    	// stats << "       --- Chi-squared of fit " << chiSquared << endl;
    	stats << "Input and residual image statistics (to be used as a rough guide only as to goodness of fit)" << endl;
    	Double inputStdDev, residStdDev, inputRMS, residRMS;
    	_getStandardDeviations(inputStdDev, residStdDev);
    	_getRMSs(inputRMS, residRMS);
    	String unit = _fluxDensities[0].getUnit();
    	stats << "       --- Standard deviation of input image " << inputStdDev << " " << unit << endl;
    	stats << "       --- Standard deviation of residual image " << residStdDev << " " << unit << endl;
    	stats << "       --- RMS of input image " << inputRMS << " " << unit << endl;
    	stats << "       --- RMS of residual image " << residRMS << " " << unit << endl;
    	return stats.str();
    }

    void ImageFitter::_setFluxes() {
    	uInt ncomps = _results.nelements();

    	_fluxDensities.resize(ncomps);
    	_fluxDensityErrors.resize(ncomps);

    	_peakIntensities.resize(ncomps);
    	_peakIntensityErrors.resize(ncomps);

    	_majorAxes.resize(ncomps);
    	_minorAxes.resize(ncomps);
    	Vector<Quantity> fluxQuant;

		ImageAnalysis ia;
		ia.open(_image->name());

    	Double rmsPeak = Vector<Double>(_residStats.asArrayDouble("rms"))[0];
        Quantity rmsPeakError(
        	rmsPeak,
        	_image->units()
        );

        ImageMetaData md(*_image);
        Quantity resArea;
        Bool found = False;
    	Quantity intensityToFluxConversion = _image->units().getName().contains("/beam")
    		? Quantity(1.0, "beam")
    		: Quantity(1.0, "pixel");

        if (intensityToFluxConversion.getUnit() == "beam") {
        	if(md.getBeamArea(resArea)) {
        		found = True;
        	}
        	else {
        		*_log << LogIO::WARN
        			<< "Image units are per beam but beam area could not "
        			<< "be determined. Assume beam area is pixel area."
        			<< LogIO::POST;
        	}
        }
        if (! found) {
        	if (! md.getDirectionPixelArea(resArea)) {
        		*_log << LogIO::EXCEPTION
        			<< "Pixel area could not be determined";
        	}
        }

        uInt polNum = 0;
    	for(uInt i=0; i<ncomps; i++) {
    		_results.getFlux(fluxQuant, i);
    		// TODO there is probably a better way to get the flux component we want...
    		Vector<String> polarization = _results.getStokes(i);
    		for (uInt j=0; j<polarization.size(); j++) {
    			if (polarization[j] == _kludgedStokes) {
    				_fluxDensities[i] = fluxQuant[j];
				std::complex<double> error = _results.component(i).flux().errors()[j];
    				_fluxDensityErrors[i].setValue(sqrt(error.real()*error.real() + error.imag()*error.imag()));
    				_fluxDensityErrors[i].setUnit(_fluxDensities[i].getUnit());
    	    		polNum = j;
    				break;
    			}
    		}
    		const ComponentShape* compShape = _results.getShape(i);

    		AlwaysAssert(compShape->type() == ComponentType::GAUSSIAN, AipsError);
    		_majorAxes[i] = (static_cast<const GaussianShape *>(compShape))->majorAxis();
    		_minorAxes[i] = (static_cast<const GaussianShape *>(compShape))->minorAxis();
            _peakIntensities[i] = ia.convertflux(
    	        _noBeam, _fluxDensities[i], _majorAxes[i], _minorAxes[i], "Gaussian", True, True
            );

            rmsPeakError.convert(_peakIntensities[i].getUnit());
            Double rmsPeakErrorValue = rmsPeakError.getValue();
            Double peakErrorFromFluxErrorValue = (
            		_peakIntensities[i]*_fluxDensityErrors[i]/_fluxDensities[i]
				).getValue();

            _peakIntensityErrors[i].setValue(
            	max(
            		rmsPeakErrorValue,
            		peakErrorFromFluxErrorValue
            	)
            );
            _peakIntensityErrors[i].setUnit(_image->units());
            if (rmsPeakErrorValue > peakErrorFromFluxErrorValue) {
            	const GaussianShape *gaussShape = static_cast<const GaussianShape *>(compShape);
            	Quantity compArea = gaussShape->getArea();
            	Quantity rmsFluxError = rmsPeakError*compArea/resArea;
            	rmsFluxError.convert(_fluxDensityErrors[i].getUnit());
            	_fluxDensityErrors[i].setValue(
            			max(
            				_fluxDensityErrors[i].getValue(),
            				rmsFluxError.getValue()
            			)
					);
            	Vector<std::complex<double> > errors(4, std::complex<double>(0, 0));
            	errors[polNum] = std::complex<double>(_fluxDensityErrors[i].getValue(), 0);
            	_results.component(i).flux().setErrors(errors);

            }
    	}
    }

    void ImageFitter::_setSizes() {
    	uInt ncomps = _results.nelements();

    	_positionAngles.resize(ncomps);
    	_majorAxisErrors.resize(ncomps);
    	_minorAxisErrors.resize(ncomps);
    	_positionAngleErrors.resize(ncomps);
    	Double rmsPeak = Vector<Double>(_residStats.asArrayDouble("rms"))[0];

        Quantity rmsPeakError(
        	rmsPeak,
        	_image->units()
        );

	   	Vector<Quantity> beam = _image->imageInfo().restoringBeam();
	   	Bool hasBeam = beam.nelements() == 3;

    	Quantity xBeam;
    	Quantity yBeam;
    	Quantity paBeam;

    	if (hasBeam) {
    		xBeam = beam[0];
    		yBeam = beam[1];
    		paBeam = beam[2];
    	}
    	else {
    		ImageMetaData md(*_image);
    		Int dirCoordNumber = md.directionCoordinateNumber();
    		Vector<Double> pixInc = _image->coordinates().directionCoordinate(dirCoordNumber).increment();
    		xBeam = Quantity(pixInc[0], "rad");
    		yBeam = Quantity(pixInc[1], "rad");
    		paBeam = Quantity(0, "rad");
    	}

    	for(uInt i=0; i<_results.nelements(); i++) {
    		const ComponentShape* compShape = _results.getShape(i);
    		AlwaysAssert(compShape->type() == ComponentType::GAUSSIAN, AipsError);
    		_positionAngles[i]  = (static_cast<const GaussianShape *>(compShape))->positionAngle();
    		_majorAxisErrors[i] = (static_cast<const GaussianShape *>(compShape))->majorAxisError();
    		_minorAxisErrors[i] = (static_cast<const GaussianShape *>(compShape))->minorAxisError();
    		_positionAngleErrors[i] = (static_cast<const GaussianShape *>(compShape))->positionAngleError();
    		Double signalToNoise = fabs((_peakIntensities[i]/rmsPeakError).getValue());

    	   	Quantity paRelToBeam = _positionAngles[i] - paBeam;
    	   	paRelToBeam.convert("rad");

    	   	xBeam.convert(_majorAxisErrors[i].getUnit());
    	   	yBeam.convert(_majorAxisErrors[i].getUnit());
    	   	Double xBeamVal = xBeam.getValue();
    	   	Double yBeamVal = yBeam.getValue();

       	   	Double cosPA = cos(paRelToBeam.getValue());
        	Double sinPA = sin(paRelToBeam.getValue());

        	// angles are measured from north (y direction).
    	   	_majorAxisErrors[i].setValue(
    	   			max(
    	   				_majorAxisErrors[i].getValue(),
    	   				sqrt(
    	   					(
    	   						xBeamVal*sinPA * xBeamVal*sinPA
    	   						+ yBeamVal*cosPA * yBeamVal*cosPA
    	   					)/signalToNoise
    	   				)
    	   			)
    	   		);
     	   	_minorAxisErrors[i].setValue(
        	   		max(
        	   			_minorAxisErrors[i].getValue(),
        	   			sqrt(
        	 				(
        	   					xBeamVal*cosPA * xBeamVal*cosPA
        	   					+ yBeamVal*sinPA * yBeamVal*sinPA
        	   				)/signalToNoise
        	   			)
        	   		)
        	   	);

    		Double posAngleRad = _positionAngles[i].getValue(Unit("rad"));
    		Quantity posAngErrorFromSN = _positionAngles[i] * sqrt(
    		    		_majorAxisErrors[i]/_majorAxes[i] * _majorAxisErrors[i]/_majorAxes[i]
    		    		+ _minorAxisErrors[i]/_minorAxes[i] * _minorAxisErrors[i]/_minorAxes[i]
    		    	);
    		posAngErrorFromSN *= 1/(1 + posAngleRad*posAngleRad);
    		posAngErrorFromSN.convert(_positionAngleErrors[i].getUnit());
    		_positionAngleErrors[i].setValue(
    				max(_positionAngleErrors[i].getValue(), posAngErrorFromSN.getValue())
    			);

    		_majorAxisErrors[i].convert(_majorAxes[i].getUnit());
    		_minorAxisErrors[i].convert(_minorAxes[i].getUnit());
    		_positionAngleErrors[i].convert(_positionAngles[i].getUnit());

     	   	GaussianShape* newShape = dynamic_cast<GaussianShape *>(compShape->clone());

     	   	newShape->setErrors(
     	   		_majorAxisErrors[i], _minorAxisErrors[i],
     	   		_positionAngleErrors[i]
     	    );

     	   	// newShape->setErrors(newErrors);

     	   	// set the position uncertainties

     	   	Quantity latError = compShape->refDirectionErrorLat();
     	   	Quantity longError = compShape->refDirectionErrorLong();

     	   	paBeam.convert("rad");
     	   	Double cosPaBeam = cos(paBeam.getValue());
     	   	Double sinPaBeam = sin(paBeam.getValue());

     	   	Quantity longErrorFromSN = sqrt(
     	   			xBeam*sinPaBeam*xBeam*sinPaBeam + yBeam*cosPaBeam*yBeam*cosPaBeam
     	   		)/(2*signalToNoise);
     	   	Quantity latErrorFromSN = sqrt(
     	   			xBeam*cosPaBeam*xBeam*cosPaBeam + yBeam*sinPaBeam*yBeam*sinPaBeam
     	   		)/(2*signalToNoise);

     	   longErrorFromSN.convert(longError.getUnit());
     	   latErrorFromSN.convert(latError.getUnit());
     	   longError.setValue(
     			   max(longError.getValue(), longErrorFromSN.getValue())
			   );
     	   latError.setValue(
     			   max(latError.getValue(), latErrorFromSN.getValue())
			   );
     	   	newShape->setRefDirectionError(latError, longError);

     	   	Vector<Int> index(1, i);
     	   	_results.setShape(index, *newShape);
    	}
    }

    String ImageFitter::_sizeToString(const uInt compNumber) const  {
    	ostringstream size;
    	const ComponentShape* compShape = _results.getShape(compNumber);
    	AlwaysAssert(compShape->type() == ComponentType::GAUSSIAN, AipsError);

    	Vector<Quantum<Double> > beam = _image->imageInfo().restoringBeam();
    	Bool hasBeam = beam.nelements() == 3;
    	size << "Image component size";
    	if (hasBeam) {
    		size << " (convolved with beam)";
    	}
    	size << " ---" << endl;
    	size << compShape->sizeToString() << endl;
    	if (hasBeam) {
    		Quantity maj = _majorAxes[compNumber];
    		Quantity min = _minorAxes[compNumber];
    		Quantity pa = _positionAngles[compNumber];
    		const GaussianShape *gaussShape = static_cast<const GaussianShape *>(compShape);
    		Quantity emaj = gaussShape->majorAxisError();
    		Quantity emin = gaussShape->minorAxisError();
    		Quantity epa  = gaussShape->positionAngleError();

    		size << "Clean beam size ---" << endl;
    		size << TwoSidedShape::sizeToString(beam[0], beam[1], beam[2], False) << endl;
    		// Quantity femaj = emaj/maj;
    		// Quantity femin = emin/min;
    		Bool fitSuccess = False;
    		Vector<Quantity> best(3);
    		best[0] = maj;
    		best[1] = min;
    		best[2] = pa;
    		Quantity majFit;
    		Quantity minFit;
    		Quantity paFit;

    		Vector<Quantity> bestFit(3);
    		Bool isPointSource = ImageUtilities::deconvolveFromBeam(
    		    bestFit[0], bestFit[1], bestFit[2], fitSuccess, *_log, best, beam
    		);
    		size << "Image component size (deconvolved from beam) ---" << endl;

    		if(fitSuccess) {
    			if (isPointSource) {
    				size << "    Component is a point source" << endl;
    			}
    			else {
    				Vector<Quantity> majRange(2, maj - emaj);
    				majRange[1] = maj + emaj;
    				Vector<Quantity> minRange(2, min - emin);
    				minRange[1] = min + emin;
    				Vector<Quantity> paRange(2, pa - epa);
    				paRange[1] = pa + epa;
    				Vector<Quantity> sourceIn(3);
    				for (uInt i=0; i<2; i++) {
    					sourceIn[0] = majRange[i];
    					for (uInt j=0; j<2; j++) {
    						sourceIn[1] = minRange[j];
    						for (uInt k=0; k<2; k++) {
    							sourceIn[2] = paRange[k];
    							minFit = Quantity();
    							majFit = Quantity();
    							paFit = Quantity();
    							isPointSource = ImageUtilities::deconvolveFromBeam(
    								majFit, minFit, paFit, fitSuccess,
    								*_log, sourceIn, beam, False
    							);
    							if (fitSuccess) {
    								Quantity errMaj = bestFit[0] - majFit;
    								errMaj.convert(emaj.getUnit());
    								Quantity errMin = bestFit[1] - minFit;
    								errMin.convert(emin.getUnit());
    								Quantity errPA = bestFit[2] - paFit;
    								errPA.convert("deg");
    								errPA.setValue(fmod(errPA.getValue(), 180.0));
    								errPA.convert(epa.getUnit());
    								emaj.setValue(
    										max(emaj.getValue(), abs(errMaj.getValue()))
										);
    								emin.setValue(
    										max(emin.getValue(), abs(errMin.getValue()))
										);
    								epa.setValue(
    										max(epa.getValue(), abs(errPA.getValue()))
										);
    							}
    						}
    					}
					}
					size << TwoSidedShape::sizeToString(
						bestFit[0], bestFit[1], bestFit[2],
						True, emaj, emin, epa
					);
    			}
    		}
    		else {
    			*_log << LogIO::SEVERE << "Could not deconvolve source from beam" << LogIO::POST;
    		}
    	}
    	return size.str();
    }

    String ImageFitter::_fluxToString(uInt compNumber) const {
    	Vector<String> unitPrefix(8);
		unitPrefix[0] = "T";
		unitPrefix[1] = "G";
		unitPrefix[2] = "M";
		unitPrefix[3] = "k";
		unitPrefix[4] = "";
		unitPrefix[5] = "m";
		unitPrefix[6] = "u";
		unitPrefix[7] = "n";

    	ostringstream fluxes;
    	Quantity fluxDensity = _fluxDensities[compNumber];
       	Quantity fluxDensityError = _fluxDensityErrors[compNumber];
		Vector<String> polarization = _results.getStokes(compNumber);
		/*
		for (uInt i=0; i<polarization.nelements(); i++) {
            if (polarization[i] == _kludgedStokes) {
            	std::complex<double> error = _results.component(compNumber).flux().errors()[i];
            	fluxDensityError.setValue(sqrt(error.real()*error.real() + error.imag()*error.imag()));
            	fluxDensityError.setUnit(fluxDensity.getUnit());
            	break;
            }
		}
		*/
        String unit;
        for (uInt i=0; i<unitPrefix.size(); i++) {
        	unit = unitPrefix[i] + "Jy";
        	if (fluxDensity.getValue(unit) > 1) {
        		fluxDensity.convert(unit);
        		fluxDensityError.convert(unit);
        		break;
        	}
        }
        Vector<Double> fd(2);
        fd[0] = fluxDensity.getValue();
        fd[1] = fluxDensityError.getValue();
    	Quantity peakIntensity = _peakIntensities[compNumber];
    	Quantity intensityToFluxConversion = _image->units().getName().contains("/beam")
    		? Quantity(1.0, "beam")
    		: Quantity(1.0, "pixel");

    	Quantity tmpFlux = peakIntensity * intensityToFluxConversion;
    	tmpFlux.convert("Jy");

        Quantity peakIntensityError = _peakIntensityErrors[compNumber];
        Quantity tmpFluxError = peakIntensityError * intensityToFluxConversion;

        uInt precision = 0;
        fluxes << "Flux ---" << endl;

        if (! _noBeam) {
        	precision = precisionForValueErrorPairs(fd, Vector<Double>());
        	fluxes << std::fixed << setprecision(precision);
        	fluxes << "       --- Integrated:   " << fluxDensity.getValue()
					<< " +/- " << fluxDensityError.getValue() << " "
					<< fluxDensity.getUnit() << endl;
        }

        for (uInt i=0; i<unitPrefix.size(); i++) {
        	String unit = unitPrefix[i] + tmpFlux.getUnit();
         	if (tmpFlux.getValue(unit) > 1) {
         		tmpFlux.convert(unit);
         		tmpFluxError.convert(unit);
         		break;
         	}
        }
        //String newUnit = tmpFlux.getUnit() + "/" + intensityToFluxConversion.getUnit();
        peakIntensity = Quantity(tmpFlux.getValue(), tmpFlux.getUnit() + "/" + intensityToFluxConversion.getUnit());
        peakIntensityError = Quantity(tmpFluxError.getValue(), peakIntensity.getUnit());


        Vector<Double> pi(2);
        pi[0] = peakIntensity.getValue();
        pi[1] = peakIntensityError.getValue();
        precision = precisionForValueErrorPairs(pi, Vector<Double>());
        fluxes << std::fixed << setprecision(precision);
        fluxes << "       --- Peak:         " << peakIntensity.getValue()
 			<< " +/- " << peakIntensityError.getValue() << " "
 			<< peakIntensity.getUnit() << endl;
        fluxes << "       --- Polarization: " << _stokesString << endl;
        return fluxes.str();
    }

    String ImageFitter::_spectrumToString(uInt compNumber) const {
    	Vector<String> unitPrefix(9);
		unitPrefix[0] = "T";
		unitPrefix[1] = "G";
		unitPrefix[2] = "M";
		unitPrefix[3] = "k";
		unitPrefix[4] = "";
		unitPrefix[5] = "c";
		unitPrefix[6] = "m";
		unitPrefix[7] = "u";
		unitPrefix[8] = "n";
    	ostringstream spec;
    	const SpectralModel& spectrum = _results.component(compNumber).spectrum();
    	Quantity frequency = spectrum.refFrequency().get("MHz");
    	Quantity c(C::c, "m/s");
    	Quantity wavelength = c/frequency;
    	String prefUnit;
    	for (uInt i=0; i<unitPrefix.size(); i++) {
    		prefUnit = unitPrefix[i] + "Hz";
    		if (frequency.getValue(prefUnit) > 1) {
    			frequency.convert(prefUnit);
    			break;
    		}
    	}
    	for (uInt i=0; i<unitPrefix.size(); i++) {
    		prefUnit = unitPrefix[i] + "m";
    		if (wavelength.getValue(prefUnit) > 1) {
    			wavelength.convert(prefUnit);
    			break;
    		}
    	}

    	spec << "Spectrum ---" << endl;
    	spec << "      --- frequency:        " << frequency << " (" << wavelength << ")" << endl;
    	return spec.str();
    }

    void ImageFitter::_writeLogfile(const String& output) const {
    	File log(_logfileName);
    	switch (File::FileWriteStatus status = log.getWriteStatus()) {
    	case File::OVERWRITABLE:
    		if (logfileAppend) {
    			Int fd = open(_logfileName.c_str(), O_RDWR | O_APPEND);
    			FiledesIO fio(fd, _logfileName.c_str());
    			fio.write(output.length(), output.c_str());
    			FiledesIO::close(fd);
    			*_log << LogIO::NORMAL << "Appended results to file "
    					<< _logfileName << LogIO::POST;
    		}
    		// no break here to fall through to the File::CREATABLE block if logFileAppend is false
    	case File::CREATABLE:
    		if (status == File::CREATABLE || ! logfileAppend) {
    			// can fall throw from previous case block so status can be File::OVERWRITABLE
    			String action = (status == File::OVERWRITABLE) ? "Overwrote" : "Created";
    			Int fd = FiledesIO::create(_logfileName.c_str());
    			FiledesIO fio (fd, _logfileName.c_str());
    			fio.write(output.length(), output.c_str());
    			FiledesIO::close(fd);
    			*_log << LogIO::NORMAL << action << " file "
    					<< _logfileName << " with new log file"
    					<< LogIO::POST;
    		}
    		break;
    	default:
    		// checks to see if the log file is not creatable or not writeable should have already been
    		// done and if so _logFileName set to the empty string so this method wouldn't be called in
    		// those cases.
    		*_log << "Programming logic error. This block should never be reached" << LogIO::EXCEPTION;
    	}
    }

    void ImageFitter::_writeNewEstimatesFile() const {
    	ostringstream out;
    	for (uInt i=0; i<_results.nelements(); i++) {

        	MDirection mdir = _results.getRefDirection(i);
           	Quantity lat = mdir.getValue().getLat("rad");
            Quantity longitude = mdir.getValue().getLong("rad");
    		Vector<Double> world(4,0), pixel(4,0);
    		_image->coordinates().toWorld(world, pixel);
    		world[0] = longitude.getValue();
    		world[1] = lat.getValue();
    		if (_image->coordinates().toPixel(pixel, world)) {
    			out << _peakIntensities[i].getValue() << ", "
    				<< pixel[0] << ", " << pixel[1] << ", "
    				<< _majorAxes[i] << ", " << _minorAxes[i] << ", "
    				<< _positionAngles[i] << endl;
    		}
    		else {
    			*_log << LogIO::WARN << "Unable to calculate pixel location of "
    				<< "component number " << i << " so cannot write new estimates"
    				<< "file" << LogIO::POST;
    			return;
    		}
    	}
    	String output = out.str();
    	File estimates(_newEstimatesFileName);
    	String action = (estimates.getWriteStatus() == File::OVERWRITABLE) ? "Overwrote" : "Created";
    	Int fd = FiledesIO::create(_newEstimatesFileName.c_str());
    	FiledesIO fio(fd, _logfileName.c_str());
    	fio.write(output.length(), output.c_str());
    	FiledesIO::close(fd);
    	*_log << LogIO::NORMAL << action << " file "
    		<< _newEstimatesFileName << " with new estimates file"
    		<< LogIO::POST;
    }

    void ImageFitter::_writeCompList() {
    	if (! _compListName.empty()) {
    		switch (_writeControl) {
    		case NO_WRITE:
    			return;
    		case WRITE_NO_REPLACE:
    		{
    			File file(_compListName);
    			if (file.exists()) {
    		    	LogOrigin logOrigin("ImageFitter", __FUNCTION__);
    		        *_log << logOrigin;
    		        *_log << LogIO::WARN << "Requested persistent component list " << _compListName
    		        	<< " already exists and user does not wish to overwrite it so "
    		        	<< "the component list will not be written" << LogIO::POST;
    		       return;
    			}
    		}
    		// allow fall through
    		case OVERWRITE: {
    			Path path(_compListName);
    			_results.rename(path, Table::New);
    			*_log << LogIO::NORMAL << "Wrote component list table " << _compListName << endl;
    		}
    		return;
    		default:
    			// should never get here
    			return;
    		}
    	}
    }

}

