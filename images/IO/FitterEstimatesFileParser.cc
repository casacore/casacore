//# Copyright (C) 1996,1997,1998,1999,2001,2002
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

#include <casa/aips.h>
#include <casa/IO/RegularFileIO.h>
#include <casa/Utilities/Regex.h>
#include <casa/Containers/Record.h>
#include <components/ComponentModels/ConstantSpectrum.h>
#include <components/ComponentModels/Flux.h>
#include <components/ComponentModels/GaussianShape.h>
#include <coordinates/Coordinates/CoordinateUtil.h>
#include <coordinates/Coordinates/DirectionCoordinate.h>
#include <images/IO/FitterEstimatesFileParser.h>
#include <images/Images/ImageMetaData.h>
#include <images/Images/ImageStatistics.h>


namespace casa { //# NAMESPACE CASA - BEGIN

FitterEstimatesFileParser::FitterEstimatesFileParser (
		const String& filename,
		const ImageInterface<Float>& image
	) : componentList(), peakValues(0), xposValues(0), yposValues(0),
		majValues(0), minValues(0), paValues(0), contents("") {
	itsLog = new LogIO();

	RegularFile myFile(filename);
	itsLog->origin(LogOrigin("FitterEstimatesFileParser","constructor"));

	if (! myFile.exists()) {
		*itsLog << LogIO::NORMAL << "Estimates file " << filename << " does not exist"
			<< LogIO::EXCEPTION;
	}
	if (! myFile.isReadable()) {
		*itsLog << LogIO::NORMAL << "Estimates file " << filename << " is not readable"
			<< LogIO::EXCEPTION;
	}
	_parseFile(myFile, image);
	_createComponentList(image);

}

FitterEstimatesFileParser::~FitterEstimatesFileParser() {
	delete itsLog;
}

ComponentList FitterEstimatesFileParser::getEstimates() const {
	return componentList;
}

Vector<String> FitterEstimatesFileParser::getFixed() const {
	return fixedValues;
}

String FitterEstimatesFileParser::getContents() const {
	return contents;
}

void FitterEstimatesFileParser::_parseFile(
	const RegularFile& myFile, const ImageInterface<Float>& image
) {
	itsLog->origin(LogOrigin("FitterEstimatesFileParser","_parseFile"));

	RegularFileIO fileIO(myFile);
	// I doubt a genuine estimates file will ever have this many characters
	Int bufSize = 4096;
	char *buffer = new char[bufSize];
	int nRead;

	while ((nRead = fileIO.read(bufSize, buffer, False)) == bufSize) {
		*itsLog << LogIO::NORMAL << "read: " << nRead << LogIO::POST;
		String chunk(buffer, bufSize);

		contents += chunk;
	}
	// get the last chunk
	String chunk(buffer, nRead);
	contents += chunk;

	Vector<String> lines = stringToVector(contents, '\n');
	Regex blankLine("^[ \n\t\r\v\f]+$",1000);
	uInt componentIndex = 0;
	for(Vector<String>::iterator iter=lines.begin(); iter!=lines.end(); iter++) {
		if (iter->empty() || iter->firstchar() == '#' ||  iter->matches(blankLine)) {
			// ignore comments and blank lines
			continue;
		}
		uInt commaCount = iter->freq(',');
		if (commaCount < 5 || commaCount > 6) {
			*itsLog << "bad format for line " << *iter << LogIO::EXCEPTION;
		}
		Vector<String> parts = stringToVector(*iter);
		for (Vector<String>::iterator viter = parts.begin(); viter != parts.end(); viter++) {
			viter->trim();
		}
		String filename = myFile.path().dirName() + "/" + myFile.path().baseName();
		String peak = parts[0];
//		String flux = parts[0];
		String xpos = parts[1];
		String ypos = parts[2];
		String maj = parts[3];
		String min = parts[4];
		String pa = parts[5];

		String fixedMask;
		peakValues.resize(componentIndex + 1, True);
//		fluxValues.resize(componentIndex + 1, True);
		xposValues.resize(componentIndex + 1, True);
		yposValues.resize(componentIndex + 1, True);
		majValues.resize(componentIndex + 1, True);
		minValues.resize(componentIndex + 1, True);
		paValues.resize(componentIndex + 1, True);
		fixedValues.resize(componentIndex + 1, True);

		if (! peak.matches(RXdouble) ) {
			*itsLog << "File " << filename << ", line " << *iter
				<< ": peak value " << peak << " is not numeric"
				<< LogIO::EXCEPTION;
		}
		peakValues(componentIndex) = String::toDouble(peak);

		if (! xpos.matches(RXdouble) ) {
			*itsLog << "File " << filename << ", line " << *iter
				<< ": x position value " << xpos << " is not numeric"
				<< LogIO::EXCEPTION;
		}
		xposValues(componentIndex) = String::toDouble(xpos);

		if (! ypos.matches(RXdouble) ) {
			*itsLog << "File " << filename << ", line " << *iter
				<< ": y position value " << ypos << " is not numeric"
				<< LogIO::EXCEPTION;
		}
		yposValues(componentIndex) = String::toDouble(ypos);

		Quantity majQuantity;
		if (! readQuantity(majQuantity, maj)) {
			*itsLog << "File " << filename << ", line " << *iter
				<< ": Major axis value " << maj << " is not a quantity"
				<< LogIO::EXCEPTION;
		}
		majValues(componentIndex) = majQuantity;

		Quantity minQuantity;
		if (! readQuantity(minQuantity, min)) {
			*itsLog << "File " << filename << ", line " << *iter
				<< ": Major axis value " << min << " is not a quantity"
				<< LogIO::EXCEPTION;
		}
		minValues(componentIndex) = minQuantity;

		Quantity paQuantity;
		if (! readQuantity(paQuantity, pa)) {
			*itsLog << "File " << filename << ", line " << *iter
				<< ": Position angle value " << pa << " is not a quantity"
				<< LogIO::EXCEPTION;
		}
		paValues(componentIndex) = paQuantity;

		if (parts.size() == 7) {
			fixedMask = parts[6];
			for (
				String::iterator siter = fixedMask.begin();
				siter != fixedMask.end(); siter++
			) {
				if (
					*siter != 'a' && *siter != 'b' && *siter != 'f'
					&& *siter != 'p' && *siter != 'x' && *siter != 'y'
				) {
					*itsLog << "fixed parameter ID " << String(*siter) << " is not recognized"
						<< LogIO::EXCEPTION;
				}
			}
			fixedValues(componentIndex) = fixedMask;
		}
		fixedValues(componentIndex) = fixedMask;
		componentIndex++;
	}
}

void FitterEstimatesFileParser::_createComponentList(
	const ImageInterface<Float>& image
) {
	ConstantSpectrum spectrum;
    CoordinateSystem csys = image.coordinates();
    Vector<Double> pos(image.ndim(),0);
    ImageMetaData metadata(image);
    Vector<Int> dirAxesNums = metadata.directionAxesNumbers();
    Vector<Double> world;
    Int dirCoordNumber = metadata.directionCoordinateNumber();
    const DirectionCoordinate& dirCoord = csys.directionCoordinate(
     	dirCoordNumber
    );
    MDirection::Types mtype = dirCoord.directionType();
    // SkyComponents require the flux density but users and the fitting
	// code really want to specify peak intensities. So we must convert
	// here. To do that, we need to know the brightness units of the image.

	Quantity resolutionElementArea;
	ImageMetaData md(image);
	Quantity intensityToFluxConversion(1.0, "beam");

	// does the image have a restoring beam?
	if(! md.getBeamArea(resolutionElementArea)) {
		// if no restoring beam, let's hope the the brightness units are
		// in [prefix]Jy/pixel and let's find the pixel size.
		if(md.getDirectionPixelArea(resolutionElementArea)) {
			intensityToFluxConversion.setUnit("pixel");
		}
		else {
			// can't find  pixel size, which is extremely bad!
			*itsLog << "Unable to determine the resolution element area of image "
					<< image.name()<< LogIO::EXCEPTION;
		}
	}
	for(uInt i=0; i<peakValues.size(); i++) {
		pos[dirAxesNums[0]] = xposValues[i];
		pos[dirAxesNums[1]] = yposValues[i];
        csys.toWorld(world, pos);
        Quantity ra(world[0], "rad");
        Quantity dec(world[1], "rad");
        MDirection mdir(ra, dec, mtype);

        GaussianShape gaussShape(
        	mdir, majValues[i], minValues[i], paValues[i]
        );
		Unit brightnessUnit = image.units();
		// Estimate the flux density

		Quantity fluxQuantity = Quantity(peakValues[i], brightnessUnit) * intensityToFluxConversion;
		fluxQuantity.convert("Jy");
		fluxQuantity = fluxQuantity*gaussShape.getArea()/resolutionElementArea;
		// convert to Jy again to get rid of the superfluous sr/(sr)
		fluxQuantity.convert("Jy");

		// Just fill the Stokes which aren't being fit with the same value as
		// the Stokes that is. Doesn't matter that the other three are bogus
		// for the purposes of this, since we only fit one stokes at a time
		Vector<Double> fluxStokes(4);

		for(uInt j=0; j<4; j++) {
			fluxStokes[j] = fluxQuantity.getValue();
		}
		Quantum<Vector<Double> > fluxVector(fluxStokes, fluxQuantity.getUnit());
		Flux<Double> flux(fluxVector);
        SkyComponent skyComp(flux, gaussShape, spectrum);
        componentList.add(skyComp);
	}

}
} //# NAMESPACE CASA - END

