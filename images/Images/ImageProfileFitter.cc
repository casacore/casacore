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

#include <images/Images/ImageProfileFitter.h>

#include <casa/Quanta/MVAngle.h>
#include <casa/Quanta/MVTime.h>
#include <casa/Utilities/Precision.h>
#include <images/Images/ImageAnalysis.h>
#include <images/Images/ImageCollapser.h>
#include <images/Images/PagedImage.h>
#include <images/Images/TempImage.h>
#include <scimath/Mathematics/Combinatorics.h>

namespace casa {

ImageProfileFitter::ImageProfileFitter(
    const String& imagename, const String& region, const String& box,
    const String& chans, const String& stokes,
    const String& mask, const Int axis, const Bool multiFit,
    const String& residual, const String& model, const uInt ngauss,
    const Int polyOrder, const String& ampName,
	const String& ampErrName, const String& centerName,
	const String& centerErrName, const String& fwhmName,
	const String& fwhmErrName
) : _log(new LogIO()), _image(0),
	_regionName(region), _box(box), _chans(chans), _stokes(stokes),
	_mask(mask), _residual(residual),
	_model(model), _regionString(""), _xUnit(""),
	_centerName(centerName), _centerErrName(centerErrName),
	_fwhmName(fwhmName), _fwhmErrName(fwhmErrName),
	_ampName(ampName), _ampErrName(ampErrName),
	_multiFit(multiFit), _deleteImageOnDestruct(True),
	_polyOrder(polyOrder), _fitAxis(axis), _ngauss(ngauss),
	_results(Record()) {
    _construct(imagename);
}

ImageProfileFitter::ImageProfileFitter(
	const ImageInterface<Float> * const image, const String& region,
	const String& box, const String& chans, const String& stokes,
	const String& mask, const Int axis, const Bool multiFit,
	const String& residual, const String& model, const uInt ngauss,
	const Int polyOrder, const String& ampName,
	const String& ampErrName, const String& centerName,
	const String& centerErrName, const String& fwhmName,
	const String& fwhmErrName
) : _log(new LogIO()), _image(image->cloneII()),
	_regionName(region), _box(box), _chans(chans), _stokes(stokes),
	_mask(mask), _residual(residual),
	_model(model), _regionString(""), _xUnit(""),
	_centerName(centerName), _centerErrName(centerErrName),
	_fwhmName(fwhmName), _fwhmErrName(fwhmErrName),
	_ampName(ampName), _ampErrName(ampErrName),
	_multiFit(multiFit), _deleteImageOnDestruct(False),
	_polyOrder(polyOrder), _fitAxis(axis), _ngauss(ngauss),
	_results(Record()) {
    _construct(_image);
}

ImageProfileFitter::~ImageProfileFitter() {
    delete _log;
    delete _image;
}

Record ImageProfileFitter::fit() {
    LogOrigin logOrigin("ImageProfileFitter", __FUNCTION__);
    *_log << logOrigin;

	ImageAnalysis ia(_image);
	Record estimate;

	String weightsImageName = "";
	try {
		if (_multiFit) {
			// FIXME need to be able to specify the weights image
			_fitters = ia.fitallprofiles(
				_regionRecord, _subImage, _xUnit, _fitAxis, _mask,
				_ngauss, _polyOrder, weightsImageName,
				_model, _residual
			);
		}
		else {
			ImageFit1D<Float> fitter = ia.fitprofile(
				_regionRecord, _subImage, _xUnit, _fitAxis, _mask,
				estimate, _ngauss, _polyOrder, _model,
				_residual
			);
			Vector<uInt> axes(1, _fitAxis);
			ImageCollapser collapser(
				&_subImage, axes, True,
				ImageCollapser::MEAN, "", True
			);
			ImageInterface<Float> *x = static_cast<SubImage<Float>*>(collapser.collapse(True));
            _subImage = (SubImage<Float>)(*x);
			delete x;
			_fitters.resize(1);
			_fitters[0] = fitter;
		}
	}
	catch (AipsError exc) {
		*_log << "Exception during fit: " << exc.getMesg()
			<< LogIO::EXCEPTION;
	}
	_setResults();
	*_log << LogIO::NORMAL << _resultsToString() << LogIO::POST;
	return _results;
}

Record ImageProfileFitter::getResults() const {
	return _results;
}

void ImageProfileFitter::_construct(const String& imagename) {
	LogOrigin logOrigin("ImageProfileFitter", __FUNCTION__);
    *_log << logOrigin;

    _checkNGaussAndPolyOrder();
    ImageInputProcessor inputProcessor;
    Vector<ImageInputProcessor::OutputStruct>  outputStruct;
    _getOutputStruct(outputStruct);
    Vector<ImageInputProcessor::OutputStruct>* outputPtr = outputStruct.size() == 0
        ? 0
        : &outputStruct;
    String diagnostics;
    inputProcessor.process(
    	_image, _regionRecord, diagnostics,
    	outputPtr, _stokes, imagename, 0,
    	_regionName, _box, _chans,
    	RegionManager::USE_FIRST_STOKES,
    	False, 0
    );
    _finishConstruction();
}

void ImageProfileFitter::_construct(const ImageInterface<Float>* image) {
	LogOrigin logOrigin("ImageProfileFitter", __FUNCTION__);
    *_log << logOrigin;
    _checkNGaussAndPolyOrder();
    ImageInputProcessor inputProcessor;
    Vector<ImageInputProcessor::OutputStruct>  outputStruct;
    _getOutputStruct(outputStruct);
    Vector<ImageInputProcessor::OutputStruct>* outputPtr = outputStruct.size() == 0
        ? 0
        : &outputStruct;
    String diagnostics;
    inputProcessor.process(
    	_regionRecord, diagnostics,
    	outputPtr, _stokes, image, 0,
    	_regionName, _box, _chans,
    	RegionManager::USE_FIRST_STOKES,
    	False, 0
    );
    _finishConstruction();
}

void ImageProfileFitter::_getOutputStruct(
    Vector<ImageInputProcessor::OutputStruct>& outputs
) {
	outputs.resize(0);
    if (! _model.empty()) {
    	ImageInputProcessor::OutputStruct modelImage;
    	modelImage.label = "model image";
    	modelImage.outputFile = &_model;
    	modelImage.required = True;
    	modelImage.replaceable = False;
    	outputs.resize(1);
    	outputs[0] = modelImage;
    }
    if (! _residual.empty()) {
    	ImageInputProcessor::OutputStruct residImage;
    	residImage.label = "residual image";
    	residImage.outputFile = &_residual;
    	residImage.required = True;
    	residImage.replaceable = False;
    	outputs.resize(outputs.size() + 1, True);
    	outputs[outputs.size() - 1] = residImage;
    }
}

void ImageProfileFitter::_checkNGaussAndPolyOrder() const {
	if (_ngauss == 0 && _polyOrder < 0) {
		*_log << "Number of gaussians is 0 and polynomial order is less than zero. "
			<< "According to these inputs there is nothing to fit."
			<< LogIO::EXCEPTION;
	}
}

void ImageProfileFitter::_finishConstruction() {
    if (! _box.empty() && ! _regionName.empty()) {
    	// for output later
    	_regionName = "";
    }
    if (_fitAxis >= (Int)_image->ndim()) {
    	*_log << "Specified fit axis " << _fitAxis
    		<< " must be less than the number of image axes ("
    		<< _image->ndim() << ")" << LogIO::EXCEPTION;
    }
    if (_fitAxis < 0) {
      Int specCoord = _image->coordinates().findCoordinate(Coordinate::SPECTRAL);
		if (specCoord < 0) {
			_fitAxis = 0;
			*_log << LogIO::WARN << "No spectral coordinate found in image, "
                << "using axis 0 as fit axis" << LogIO::POST;
		}
		else {
            _fitAxis = _image->coordinates().pixelAxes(specCoord)[0];
			*_log << LogIO::NORMAL << "Using spectral axis (axis " << _fitAxis
				<< ") as fit axis" << LogIO::POST;
		}
	}
}

void ImageProfileFitter::_setResults() {
	uInt nComps = _polyOrder < 0 ? _ngauss : _ngauss + 1;
	Vector<Bool> convergedArr(_fitters.size());
	Vector<Int> niterArr(_fitters.size(), 0);
    Matrix<Double> centerMat(_fitters.size(), nComps, -1);
	Matrix<Double> fwhmMat(_fitters.size(), nComps, -1);
	Matrix<Double> ampMat(_fitters.size(), nComps, -1);
	Matrix<Double> centerErrMat(_fitters.size(), nComps, -1);
	Matrix<Double> fwhmErrMat(_fitters.size(), nComps, -1);
	Matrix<Double> ampErrMat(_fitters.size(), nComps, -1);
	Matrix<String> typeMat(_fitters.size(), nComps, "");

	Vector<Int> nCompArr(_fitters.size(), 0);

	IPosition inTileShape = _subImage.niceCursorShape();
	TiledLineStepper stepper (_subImage.shape(), inTileShape, _fitAxis);
	RO_MaskedLatticeIterator<Float> inIter(_subImage, stepper);
	Vector<ImageFit1D<Float> >::const_iterator fitter = _fitters.begin();
	SpectralList solutions;

	uInt count = 0;
	for (
		inIter.reset();
		! inIter.atEnd() && fitter != _fitters.end();
		inIter++, fitter++, count++
	) {
		convergedArr[count] = fitter->converged();
		if (fitter->converged()) {
			niterArr[count] = (Int)fitter->getNumberIterations();
			nCompArr[count] = (Int)fitter->getList().nelements();
			solutions = fitter->getList();
			for (uInt i=0; i<solutions.nelements(); i++) {
				typeMat(count, i) = SpectralElement::fromType(solutions[i].getType());
				if (solutions[i].getType() == SpectralElement::GAUSSIAN) {
					centerMat(count, i) = solutions[i].getCenter();
					fwhmMat(count, i) = solutions[i].getFWHM();
					ampMat(count, i) = solutions[i].getFWHM();
					ampMat(count, i) = solutions[i].getAmpl();
					centerErrMat(count, i) = solutions[i].getCenterErr();
					fwhmErrMat(count, i) = solutions[i].getFWHMErr();
					ampErrMat(count, i) = solutions[i].getAmplErr();
				}
			}
		}
	}
	_results.define("converged", convergedArr);
	_results.define("niter", niterArr);
	_results.define("ncomps", nCompArr);
	_results.define("xUnit", _xUnit);
	_results.define("yUnit", _image->units().getName());

	String key;
	TempImage<Float> *tmp = static_cast<TempImage<Float>* >(_image->cloneII());
	Vector<uInt> axes(1, _fitAxis);
	ImageCollapser collapser(
		tmp, axes, False, ImageCollapser::ZERO, String(""), False
	);
	TempImage<Float> *myTemplate = static_cast<TempImage<Float>* >(collapser.collapse(True));
	delete tmp;
	IPosition shape = myTemplate->shape();
	CoordinateSystem csys = myTemplate->coordinates();
	delete myTemplate;
	uInt gaussCount = 0;
	if (
		! _multiFit && (
			! _centerName.empty() || ! _centerErrName.empty()
			|| ! _fwhmName.empty() || ! _fwhmErrName.empty()
			|| ! _ampName.empty() || ! _ampErrName.empty()
		)
	) {
		*_log << LogIO::WARN << "This was not a multi-pixel fit request so solution "
			<< "images will not be written" << LogIO::POST;
	}

	for (uInt i=0; i<nComps; i++) {
		String num = String::toString(i);
		if (_multiFit && solutions[i].getType() == SpectralElement::GAUSSIAN) {
			String gnum = String::toString(gaussCount);
			String mUnit = _xUnit;
			if (!_centerName.empty()) {
				_makeSolutionImage(
					_centerName + "_" + gnum, shape,
					csys, centerMat.column(i), mUnit
				);
			}
			if (!_centerErrName.empty()) {
				_makeSolutionImage(
					_centerErrName + "_" + gnum, shape,
					csys, centerErrMat.column(i), mUnit
				);
			}
			if (!_fwhmName.empty()) {
				_makeSolutionImage(
					_fwhmName + "_" + gnum, shape,
					csys, fwhmMat.column(i), mUnit
				);
			}
			if (!_fwhmErrName.empty()) {
				_makeSolutionImage(
					_fwhmErrName + "_" + gnum, shape,
					csys, fwhmErrMat.column(i), mUnit
				);
			}
			mUnit = _image->units().getName();
			if (!_ampName.empty()) {
				_makeSolutionImage(
					_ampName + "_" + gnum, shape,
					csys, ampMat.column(i), mUnit
				);
			}
			if (!_ampErrName.empty()) {
				_makeSolutionImage(
					_ampErrName + "_" + gnum, shape,
					csys, ampErrMat.column(i), mUnit
				);
			}
			gaussCount++;
		}
		key = "center" + num;
		_results.define(key, centerMat.column(i));
		key = "fwhm" + num;
		_results.define(key, fwhmMat.column(i));
		key = "amp" + num;
		_results.define(key, ampMat.column(i));
		key = "centerErr" + num;
		_results.define(key, centerErrMat.column(i));
		key = "fwhmErr" + num;
		_results.define(key, fwhmErrMat.column(i));
		key = "ampErr" + num;
		_results.define(key, ampErrMat.column(i));
		key = "type" + num;
		_results.define(key, typeMat.column(i));
	}
}

String ImageProfileFitter::_radToRa(Float ras) const{

   Int h, m;
   Float rah = ras * 12 / C::pi;
   h = (int)floor(rah);
   Float ram = (rah - h) * 60;
   m = (int)floor(ram);
   ras = (ram - m) * 60;
   ras = (int)(1000 * ras) / 1000.;

   String raStr = (h < 10) ? "0" : "";
        raStr.append(String::toString(h)).append(String(":"))
        .append(String((m < 10) ? "0" : ""))
        .append(String::toString(m)).append(String(":")) 
        .append(String((ras < 10) ? "0" : ""))
        .append(String::toString(ras));

   return raStr;

}


String ImageProfileFitter::_resultsToString() const {
	ostringstream summary;
	summary << "****** Fit performed at " << Time().toString() << "******" << endl << endl;
	summary << "Input parameters ---" << endl;
	summary << "       --- imagename:           " << _image->name() << endl;
	summary << "       --- region:              " << _regionName << endl;
	summary << "       --- box:                 " << _box << endl;
	summary << "       --- channels:            " << _chans << endl;
	summary << "       --- stokes:              " << _stokes << endl;
	summary << "       --- mask:                " << _mask << endl;
	summary << "       --- polynomial order:    " << _polyOrder << endl;
	summary << "       --- number of Gaussians: " << _ngauss << endl;

	if (_multiFit) {
		summary << "       --- Multiple spectra fit, one per pixel over selected region" << endl;
	}
	else {
		summary << "       --- One spectrum fit, averaged over several pixels if necessary" << endl;
	}

	IPosition inTileShape = _subImage.niceCursorShape();
	TiledLineStepper stepper (_subImage.shape(), inTileShape, _fitAxis);
	RO_MaskedLatticeIterator<Float> inIter(_subImage, stepper);
	CoordinateSystem csysSub = _subImage.coordinates();
	CoordinateSystem csys = _image->coordinates();
	Vector<Double> worldStart;
	if (! csysSub.toWorld(worldStart, inIter.position())) {
		*_log << csysSub.errorMessage() << LogIO::EXCEPTION;
	}
	CoordinateSystem csysIm = _image->coordinates();
	Vector<Double> pixStart;
	if (! csysIm.toPixel(pixStart, worldStart)) {
		*_log << csysIm.errorMessage() << LogIO::EXCEPTION;
	}
	if (_multiFit) {
		for (uInt i=0; i<pixStart.size(); i++) {
			pixStart[i] = (Int)std::floor( pixStart[i] + 0.5);
		}
	}
	Vector<ImageFit1D<Float> >::const_iterator fitter = _fitters.begin();
	Vector<String> axesNames = csysSub.worldAxisNames();
	Vector<Double> imPix(pixStart.size());
	Vector<Double> world;
	IPosition subimPos;
	SpectralList solutions;
	String axisUnit = csysIm.worldAxisUnits()[_fitAxis];
	Int pixPrecision = _multiFit ? 0 : 3;
	Int basePrecision = summary.precision(1);
	summary.precision(basePrecision);
	for (
		Vector<String>::iterator iter = axesNames.begin();
		iter != axesNames.end(); iter++
	) {
		iter->upcase();
	}
	for (
		inIter.reset();
		! inIter.atEnd() && fitter != _fitters.end();
		inIter++, fitter++
	) {
		subimPos = inIter.position();
		if (csysSub.toWorld(world, subimPos)) {
			summary << "Fit centered at:" << endl;
			for (uInt i=0; i<world.size(); i++) {
				if ((Int)i != _fitAxis) {
                                        //summary << "ax=" << axesNames[i] 
                                        //        << " world=" << world[i] 
                                        //        << endl;
					if (axesNames[i].startsWith("RIG")) {
						// right ascension
						summary << "    RA         :   "
						//	<< MVTime(world[i]).string(MVTime::TIME, 9) << endl;
							<< _radToRa(world[i]) << endl;
					}
					else if (axesNames[i].startsWith("DEC")) {
						// declination
						summary << "    Dec        : "
							<< MVAngle(world[i]).string(MVAngle::ANGLE_CLEAN, 8) << endl;
					}
					else if (axesNames[i].startsWith("FREQ")) {
						// frequency
						summary << "    Freq       : "
							<< world[i]
							<< csysSub.spectralCoordinate(i).formatUnit() << endl;
					}
					else if (axesNames[i].startsWith("STO")) {
						// stokes
						summary << "    Stokes     : "
							<< Stokes::name(Stokes::type((Int)world[i])) << endl;
					}
				}
			}
		}
		else {
			*_log << csysSub.errorMessage() << LogIO::EXCEPTION;
		}
		for (uInt i=0; i<pixStart.size(); i++) {
			imPix[i] = pixStart[i] + subimPos[i];
		}

		summary.setf(ios::fixed);
		summary << setprecision(pixPrecision) << "    Pixel      : [";
		for (uInt i=0; i<imPix.size(); i++) {
			if (i == (uInt)_fitAxis) {
				summary << " *";
			}
			else {
				summary << imPix[i];
			}
			if (i != imPix.size()-1) {
				summary << ", ";
			}
		}
		summary << "]" << setprecision(basePrecision) << endl;
		summary.unsetf(ios::fixed);
		String converged = fitter->converged() ? "YES" : "NO";
		summary << "    Converged  : " << converged << endl;
		if (fitter->converged()) {
			solutions = fitter->getList();
			summary << "    Iterations : " << fitter->getNumberIterations() << endl;
			for (uInt i=0; i<solutions.nelements(); i++) {
				summary << "    Results for component " << i << ":" << endl;
				if (solutions[i].getType() == SpectralElement::GAUSSIAN) {
					summary << _gaussianToString(
						solutions[i], csys, world.copy()
					);
				}
				else if (solutions[i].getType() == SpectralElement::POLYNOMIAL) {
					summary << _polynomialToString(solutions[i], csys, imPix, world);
				}
			}
		}
		summary << endl;
	}
	return summary.str();
}

String ImageProfileFitter::_elementToString(
	const Double value, const Double error,
	const String& unit
) const {
	ostringstream out;

	Unit myUnit(unit);
	Vector<String> unitPrefix;
	String outUnit;
	Quantity qVal(value, unit);
	Quantity qErr(error, unit);

	if (myUnit.getValue() == UnitVal::ANGLE) {
		Vector<String> angUnits(5);
		angUnits[0] = "deg";
		angUnits[1] = "arcmin";
		angUnits[2] = "arcsec";
		angUnits[3] = "marcsec";
		angUnits[4] = "uarcsec";
	    for (uInt i=0; i<angUnits.size(); i++) {
	    	outUnit = angUnits[i];
	    	if (fabs(qVal.getValue(outUnit)) > 1) {
	    		qVal.convert(outUnit);
	    		qErr.convert(outUnit);
	    		break;
	    	}
	    }
	}
	else if (unit.empty() || Quantity(1, myUnit).isConform(Quantity(1, "m/s"))) {
		// do nothing
	}
    else {
		Vector<String> unitPrefix(10);
		unitPrefix[0] = "T";
		unitPrefix[1] = "G";
		unitPrefix[2] = "M";
		unitPrefix[3] = "k";
		unitPrefix[4] = "";
		unitPrefix[5] = "m";
		unitPrefix[6] = "u";
		unitPrefix[7] = "n";
		unitPrefix[8] = "p";
		unitPrefix[9] = "f";

		for (uInt i=0; i<unitPrefix.size(); i++) {
			outUnit = unitPrefix[i] + unit;
			if (fabs(qVal.getValue(outUnit)) > 1) {
				qVal.convert(outUnit);
				qErr.convert(outUnit);
				break;
			}
		}
	}
    Vector<Double> valErr(2);
    valErr[0] = qVal.getValue();
    valErr[1] = qErr.getValue();

    uInt precision = precisionForValueErrorPairs(valErr, Vector<Double>());
    out << std::fixed << setprecision(precision);
    out << qVal.getValue() << " +/- " << qErr.getValue()
    	<< " " << qVal.getUnit();
    return out.str();
}

String ImageProfileFitter::_gaussianToString(
	const SpectralElement& gauss, const CoordinateSystem& csys,
	const Vector<Double> world
) const {
	Vector<Double> myWorld = world;
    String yUnit = _image->units().getName();
	ostringstream summary;
	summary << "        Type   : GAUSSIAN" << endl;
	summary << "        Peak   : "
		<< _elementToString(
			gauss.getAmpl(), gauss.getAmplErr(), yUnit
		)
		<< endl;
	Double center = gauss.getCenter();
	Double centerErr = gauss.getCenterErr();
	Double fwhm = gauss.getFWHM();
	Double fwhmErr = gauss.getFWHMErr();

	Double pCenter, pCenterErr, pFWHM, pFWHMErr;
	Int specCoordIndex = csys.findCoordinate(Coordinate::SPECTRAL);
	Bool convertedCenterToPix = True;
	Bool convertedFWHMToPix = True;

    if (
    	specCoordIndex >= 0
    	&& _fitAxis == csys.pixelAxes(specCoordIndex)[0]
    	&& ! csys.spectralCoordinate(specCoordIndex).velocityUnit().empty()
    ) {;
    	if (csys.spectralCoordinate(specCoordIndex).velocityToPixel(pCenter, center)) {
    		Double nextVel;
    		csys.spectralCoordinate(specCoordIndex).pixelToVelocity(nextVel, pCenter+1);
    		Double velInc = fabs(center - nextVel);
    		pCenterErr = centerErr/velInc;
    		pFWHM = fwhm/velInc;
    		pFWHMErr = fwhmErr/velInc;
    	}
    	else {
    		convertedCenterToPix = False;
    		convertedFWHMToPix = False;
    	}
    }
    else {
    	Vector<Double> pixel(myWorld.size());
    	myWorld[_fitAxis] = center;
    	Double delta = csys.increment()[_fitAxis];
    	if (csys.toPixel(pixel, myWorld)) {
    		pCenter = pixel[_fitAxis];
    		pCenterErr = centerErr/delta;
    	}
    	else {
    		convertedCenterToPix = False;
    	}
    	pFWHM = fwhm/delta;
    	pFWHMErr = fwhmErr/delta;
    }
	summary << "        Center : "
		<< _elementToString(
			center, centerErr, _xUnit
		)
		<< endl;
	if (convertedCenterToPix) {
		summary << "                 "
			<< _elementToString(
				pCenter, pCenterErr, "pixel"
			)
			<< endl;
	}
	else {
		summary << "                Could not convert world to pixel for center" << endl;
	}
	summary << "        FWHM   : "
		<< _elementToString(
			fwhm, fwhmErr, _xUnit
		)
		<< endl;
	if (convertedFWHMToPix) {
		summary << "                 " << _elementToString(
			pFWHM, pFWHMErr, "pixel"
		)
		<< endl;
	}
	else {
		summary << "                Could not convert FWHM to pixel" << endl;
	}
	return summary.str();
}

String ImageProfileFitter::_polynomialToString(
	const SpectralElement& poly, const CoordinateSystem& csys,
	const Vector<Double> imPix, const Vector<Double> world
) const {
	ostringstream summary;
	summary << "        Type: POLYNOMIAL" << endl;
	Vector<Double> parms, errs;
	poly.get(parms);
	poly.getError(errs);
	for (uInt j=0; j<parms.size(); j++) {
		String unit = _image->units().getName();
        if (j > 0) {
          String denom = _xUnit.find("/") != String::npos
                ? "(" + _xUnit + ")"
                : _xUnit;
			unit = unit + "/" + denom + "^" + String::toString(j);
		}
		summary << "         c" << j << " : "
            << _elementToString(parms[j], errs[j], unit) << endl;
	}
    // coefficents in pixels
    Double x0, deltaX;
    if (Quantity(1,_xUnit).isConform(Quantity(1, "m/s"))) {
        Double x1;
        csys.spectralCoordinate(csys.findCoordinate(Coordinate::SPECTRAL)).pixelToVelocity(x0, 0);
        csys.spectralCoordinate(csys.findCoordinate(Coordinate::SPECTRAL)).pixelToVelocity(x1, 1);
        deltaX = x1 - x0;
    }
    else {
        Vector<Double> p0 = imPix;
        p0[_fitAxis] = 0;
        Vector<Double> world0 = world;
        csys.toWorld(world0, p0);
        x0 = world0[_fitAxis];
        deltaX = csys.increment()[_fitAxis];
    }
    Vector<Double> pCoeff(_polyOrder + 1, 0);
    Vector<Double> pCoeffErr(_polyOrder + 1, 0);
    for (Int j=0; j<=_polyOrder; j++) {
        Double sumsq = 0;
        for (Int k=j; k<=_polyOrder; k++) {
            Double multiplier = Combinatorics::choose(k, j)
                * casa::pow(x0, Float(k-j))
                * casa::pow(deltaX, Float(j));

            pCoeff[j] += multiplier * parms[k];
            Double errCoeff = multiplier * errs[k];
            sumsq += errCoeff * errCoeff;
        }
        pCoeffErr[j] = casa::sqrt(sumsq);
        summary << "         c" << j << " : ";
		String unit = _image->units().getName() + "/(pixel)^" + String::toString(j);
        summary << _elementToString(pCoeff[j], pCoeffErr[j], unit) << endl;
    }
	return summary.str();
}

void ImageProfileFitter::_makeSolutionImage(
	const String& name, const IPosition& shape, const CoordinateSystem& csys,
	const Vector<Double>& values, const String& unit
) {
	Vector<Float> tmpVec(values.size());
	for (uInt i=0; i<tmpVec.size(); i++) {
		tmpVec[i] = values[i];
	}
	PagedImage<Float> image( shape, csys, name);
	image.put(tmpVec.reform(shape));
	image.setUnits(Unit(unit));
}
}

