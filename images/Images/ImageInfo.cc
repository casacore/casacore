//# ImageInfo.cc: Miscellaneous information related to an image
//# Copyright (C) 1998,1999,2001,2002,2003
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
//#
//# $Id$

#include <images/Images/ImageInfo.h>
#include <casa/Exceptions/Error.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Quanta/QuantumHolder.h>
#include <casa/Arrays/Vector.h>
#include <casa/BasicMath/Math.h>
#include <casa/Containers/Record.h>
#include <casa/BasicSL/String.h>
#include <casa/Utilities/Regex.h>
#include <tables/LogTables/LoggerHolder.h>
#include <casa/sstream.h>
#include <coordinates/Coordinates/SpectralCoordinate.h>


#include <casa/iostream.h>


namespace casa { //# NAMESPACE CASA - BEGIN

ImageInfo::ImageInfo()
: _beams(ImageBeamSet()),
  itsImageType(defaultImageType()),
  itsObjectName(defaultObjectName()) {}

ImageInfo::~ImageInfo()
{}

void ImageInfo::copy_other(const ImageInfo &other)
{
    if (this != &other) {
       _beams = other._beams;
       itsImageType = other.itsImageType;
       itsObjectName = other.itsObjectName;
    }
}

ImageInfo::ImageInfo(const ImageInfo &other)
: RecordTransformable() {
    copy_other(other);
}

ImageInfo &ImageInfo::operator=(const ImageInfo &other) {
    copy_other(other);
    return *this;
}

ImageInfo::ImageTypes ImageInfo::defaultImageType() {
    return ImageInfo::Intensity;
}

String ImageInfo::defaultObjectName() {
   static const String tmp;
   return tmp;
}

GaussianBeam ImageInfo::defaultRestoringBeam() {
	static const GaussianBeam x;
	return x;
}


GaussianBeam ImageInfo::restoringBeam(
    const Int channel, const Int polarization
) const {
	if (_beams.empty()) {
		// return a null beam
		return defaultRestoringBeam();
	}
	else if (_beams.nelements() == 1) {
		return _beams.getBeam();
	}
	else {
		return _beams.getBeam(_beamPosition(channel, polarization));
	}
}

IPosition ImageInfo::_beamPosition(
	const Int channel, const Int polarization
) const {
	Int c = channel;
	Int p = polarization;
	_validateChannelStokes(c, p);
	uInt nChannels, nStokes;
	_getChansAndStokes(nChannels, nStokes);

	if (nChannels > 0 && nStokes > 0) {
		return IPosition(2, c, p);
	}
	else {
		return IPosition(
			1, nChannels > 0 ? c : p
		);
	}
}

void ImageInfo::_getChansAndStokes(
	uInt& nChannels, uInt& nStokes
) const {
	if (_beams.empty() || _beams.hasSingleBeam()) {
		nChannels = 0;
		nStokes = 0;
	}
	else {
		// multi beam
		IPosition shape = _beams.shape();
		if (shape.nelements() == 2) {
			nChannels = shape[0];
			nStokes = shape[1];
		}
		else if (_beams.getAxes()[0] == ImageBeamSet::SPECTRAL) {
			nChannels = shape[0];
			nStokes = 0;
		}
		else {
			nChannels = 0;
			nStokes = shape[0];
		}
	}

}

void ImageInfo::setRestoringBeam(const GaussianBeam& beam) {
	if (_beams.hasMultiBeam()) {
		throw AipsError(
			"This object has multiple beams. They must be removed before you can define a single global restoring beam"
		);
	}
    if (beam.isNull()) {
    	throw AipsError("Beam is null and therefore invalid.");
    }
    ImageBeamSet bs(beam);
    _beams = bs;
}


void ImageInfo::_setRestoringBeam(const Record& inRecord) {
	if (_beams.hasMultiBeam()) {
		throw AipsError(
			"This object has multiple beams. They must be removed before you can define a single, global beam"
		);
	}
	if (! inRecord.isDefined("restoringbeam")) {
		throw (AipsError("Input record must have a 'restoringbeam' field"));
	}
	QuantumHolder qh;
	GaussianBeam restoringBeam = GaussianBeam::fromRecord(inRecord.asRecord("restoringbeam"));

	setRestoringBeam(restoringBeam);
}

/*
void ImageInfo::setRestoringBeam(
	const Quantity& major, const Quantity& minor,
	const Quantity& pa
) {
	if (_beams.hasMultiBeam()) {
		throw AipsError(
				"This object has multiple beams. They must be removed before you can define a single, global restoring beam"
		);
	}
	GaussianBeam beam(major, minor, pa);
	_beams = ImageBeamSet(beam);
}
*/
void ImageInfo::removeRestoringBeam() {
   _beams = ImageBeamSet();
}


Bool ImageInfo::getRestoringBeam (LoggerHolder& logger) {
	for (
		LoggerHolder::const_iterator iter = logger.begin();
		iter != logger.end(); iter++
	) {
		String line = iter->message();
		if (
			line.contains(String("BMAJ"))
			&& line.contains(String("BMIN"))
			&& line.contains(String("BPA"))
		) {
			Quantity major, minor, pa;
			String s[20];
			int n = split(line, s, 20, RXwhite);
			for (Int i=0; i<n; i++) {
				if (s[i].contains("BMAJ")) {
					istringstream oss(s[i+1].chars());
					Double x;
					oss >> x;
					if (x <= 0) {
						return False;
					}
					major = Quantity(x, Unit(String("deg")));
				}
				else if (s[i].contains("BMIN")) {
					istringstream oss(s[i+1].chars());
					Double x;
					oss >> x;
					if (x <= 0) {
						return False;
					}
					minor = Quantity(x, Unit(String("deg")));
				}
				else if (s[i].contains("BPA")) {
					istringstream oss(s[i+1].chars());
					Double x;
					oss >> x;
					pa = Quantity(x, Unit(String("deg")));
				}
			}
			if (minor.getValue() > major.getValue()) {
				return False;
			}
			_beams = ImageBeamSet(GaussianBeam(major, minor, pa));
			return True;
		}
	}
	return False;
}

ImageInfo::ImageTypes ImageInfo::imageType() const {
    return itsImageType;
}

ImageInfo& ImageInfo::setImageType(ImageInfo::ImageTypes type) {
    itsImageType = type;
    return *this;
}

String ImageInfo::imageType(ImageInfo::ImageTypes type) {
   String typeOut;
   switch(type) {
      case ImageInfo::Undefined:
        typeOut = String("Undefined"); 
        break;
      case ImageInfo::Intensity:
        typeOut = String("Intensity"); 
        break;
      case ImageInfo::Beam:
        typeOut = String("Beam"); 
        break;
      case ImageInfo::ColumnDensity:
        typeOut = String("Column Density"); 
        break;
      case ImageInfo::DepolarizationRatio:
        typeOut = String("Depolarization Ratio"); 
        break;
      case ImageInfo::KineticTemperature:
        typeOut = String("Kinetic Temperature"); 
        break;
      case ImageInfo::MagneticField:
        typeOut = String("Magnetic Field"); 
        break;
      case ImageInfo::OpticalDepth:
        typeOut = String("Optical Depth"); 
        break;
      case ImageInfo::RotationMeasure:
        typeOut = String("Rotation Measure"); 
        break;
      case ImageInfo::RotationalTemperature:
        typeOut = String("Rotational Temperature"); 
        break;
      case ImageInfo::SpectralIndex:
        typeOut = String("Spectral Index"); 
        break;
      case ImageInfo::Velocity:
        typeOut = String("Velocity"); 
        break;
      case ImageInfo::VelocityDispersion:
        typeOut = String("Velocity Dispersion"); 
        break;
      default:
        typeOut = String("Undefined"); 
        break;
   }
   return typeOut;
}

ImageInfo::ImageTypes ImageInfo::imageType(String type) {
   String typeUp = upcase(type);
   for (uInt i=0; i<ImageInfo::nTypes; i++) {
      ImageInfo::ImageTypes t0 = static_cast<ImageInfo::ImageTypes>(i);
      String t1Up = upcase(ImageInfo::imageType(t0));
      if (t1Up==typeUp) {
    	  return t0;
      }
   }
   return defaultImageType();
}

ImageInfo::ImageTypes ImageInfo::imageTypeFromFITS (Int value) {
    if (value==0) {
       return ImageInfo::Beam;
    }
    else if (value==8) {
       return ImageInfo::SpectralIndex;
    }
    else if (value==9) {
       return ImageInfo::OpticalDepth;
    }
    else {
       return ImageInfo::Undefined;
    }
}

String ImageInfo::objectName () const {
    return itsObjectName;
}

ImageInfo& ImageInfo::setObjectName (const String& objectName) {
    itsObjectName = objectName;
    return *this;
}

Bool ImageInfo::toRecord(
	String & error, RecordInterface & outRecord
) const {
    error = "";
    Bool ok = True;
    // If the beam is null, dont do anything as it will get
    // restored as null as well if ist not in the record
    if (_beams.hasSingleBeam()) {
       Record restoringBeamRecord = _beams.getBeam().toRecord();
       outRecord.defineRecord("restoringbeam", restoringBeamRecord);
    }
    outRecord.define("imagetype", ImageInfo::imageType(itsImageType));
    outRecord.define("objectname", itsObjectName);
    if (_beams.hasMultiBeam()) {
    	Record perPlaneBeams;
    	uInt nChannels, nStokes;
    	_getChansAndStokes(nChannels, nStokes);
    	perPlaneBeams.define(
    		"nChannels", nChannels
    	);
    	perPlaneBeams.define(
    		"nStokes", nStokes
    	);
    	Record rec;
    	uInt count = 0;
    	const Array<GaussianBeam>& beams = _beams.getBeams();
    	for (
    		Array<GaussianBeam>::const_iterator iter=beams.begin();
    		iter!=beams.end(); iter++, count++
    	) {
    		if (iter->isNull()) {
    			error = "Invalid per plane beam found";
    			return False;
    		}
    		Record rec = iter->toRecord();
    		perPlaneBeams.defineRecord("*" + String::toString(count), rec);
    	}
    	outRecord.defineRecord("perplanebeams", perPlaneBeams);
    }
    return ok;
}

Bool ImageInfo::fromRecord(String& error, const RecordInterface& inRecord) {
	// Returns default object if none in record

	// Make sure we are "empty" first

	ImageInfo tmp;
	(*this) = tmp;

	error = "";
	QuantumHolder qh;

	if (inRecord.isDefined("restoringbeam")) {
		_setRestoringBeam(inRecord);
	}
	if (inRecord.isDefined("imagetype")) {
		String type = inRecord.asString("imagetype");
		setImageType (ImageInfo::imageType(type));
	}
	if (inRecord.isDefined("objectname")) {
		String objectName = inRecord.asString("objectname");
		setObjectName (objectName);
	}
	if (inRecord.isDefined("perplanebeams")) {
		Record hpBeams = inRecord.asRecord("perplanebeams");
		if (! hpBeams.isDefined("nChannels")) {
			error = "perplanebeams subrecord has no nChannels field";
			return False;
		}
		if (! hpBeams.isDefined("nStokes")) {
			error = "perplanebeams subrecord has no nStokes field";
			return False;
		}
		_setUpBeamArray(hpBeams.asuInt("nChannels"), hpBeams.asuInt("nStokes"));
		Record rec;
		uInt count = 0;
		Array<GaussianBeam> beams = _beams.getBeams();
		for (
			Array<GaussianBeam>::iterator iter=beams.begin();
			iter!=beams.end(); iter++, count++
		) {
			String field = "*" + String::toString(count);
			if (! hpBeams.isDefined(field)) {
				error = "Field " + field + " is not defined in the per plane beams subrecord";
				return False;
			}
			*iter = GaussianBeam::fromRecord(hpBeams.asRecord(field));
		}
		_beams.setBeams(beams);
	}
	return True;
}

void ImageInfo::_setUpBeamArray(const uInt nChan, const uInt nStokes) {
	IPosition shape;
	Vector<ImageBeamSet::AxisType> axisTypes;
	if (nChan > 0 && nStokes > 0) {
		shape.resize(2);
		shape = IPosition(2, nChan, nStokes);
		axisTypes.resize(2);
		axisTypes[0] = ImageBeamSet::SPECTRAL;
		axisTypes[1] = ImageBeamSet::POLARIZATION;
	}
	else {
		shape.resize(1);
		axisTypes.resize(1);
		if (nChan > 0) {
			shape = IPosition(1, nChan);
			axisTypes[0] = ImageBeamSet::SPECTRAL;
		}
		else {
			shape = IPosition(1, nStokes);
			axisTypes[0] = ImageBeamSet::POLARIZATION;
		}
	}
	ImageBeamSet beamSet(shape, axisTypes);
	_beams = beamSet;
}



Bool ImageInfo::toFITS(String & error, RecordInterface & outRecord) const {
	error = "";
	if (hasBeam()) {
		if (hasSingleBeam()) {
			GaussianBeam beam = restoringBeam();
			outRecord.define("bmaj", beam.getMajor("deg"));
			outRecord.define("bmin", beam.getMinor("deg"));
			outRecord.define("bpa", beam.getPA(Unit("deg")));
		}
		else {
			// caller now responsible for writing beams in multi-beam iamge
		}
    }
    else {
       if (!outRecord.isFixed()) {
          Int field = outRecord.fieldNumber("bmaj");
          if (field >= 0) outRecord.removeField(field);
          field = outRecord.fieldNumber("bmin");
          if (field >= 0) outRecord.removeField(field);
          field = outRecord.fieldNumber("bpa");
          if (field >= 0) outRecord.removeField(field);
       } 
    }
//
    ImageInfo::ImageTypes type = imageType();
    if (type!=ImageInfo::Undefined) {
       String type = ImageInfo::imageType(itsImageType);
       outRecord.define("btype", type);
    }
    else {
       if (!outRecord.isFixed()) {
          Int field = outRecord.fieldNumber("btype");
          if (field >= 0) outRecord.removeField(field);
       }
    }
    {
       outRecord.define("object", itsObjectName);
    }

    return True;
}

Bool ImageInfo::fromFITS(
		Vector<String>& error, const RecordInterface& header
) {
	// keyname
	//   value          - required
	//   unit           - optional
	//   comment        - optional
	error.resize(3);
	Bool ok = True;
	ImageInfo tmp;
	(*this) = tmp; // Make sure we are "empty" first;
	if (
		header.isDefined("bmaj") && header.isDefined("bmin")
		&& header.isDefined("bpa")
	) {
		const RecordInterface& subRec0 = header.asRecord("bmaj");
		const RecordInterface& subRec1 = header.asRecord("bmin");
		const RecordInterface& subRec2 = header.asRecord("bpa");
		Double bmaj, bmin, bpa;
		try {
			subRec0.get(0, bmaj);
			subRec1.get(0, bmin);
			subRec2.get(0, bpa);
			if(bmaj*bmin>0.){
				// Assume FITS standard unit "degrees"
				Unit unit(String("deg"));
				Quantity bmajq(max(bmaj,bmin), unit);
				Quantity bminq(min(bmaj,bmin), unit);
				Quantity bpaq(bpa, unit);

				bmajq.convert(Unit("arcsec"));
				bminq.convert(Unit("arcsec"));
				bpaq.convert(Unit("deg"));

				setRestoringBeam(GaussianBeam(bmajq, bminq, bpaq));
			}
			else {
				ostringstream oss;
				oss << "BMAJ, BMIN ("<< bmaj << ", " << bmin <<") are not positive";
				error(0) = oss.str();
				ok = False;
			}
		}
		catch(const AipsError& x) {
			error(0) = "ERROR reading BMAJ, BMIN, BPA: " + x.getMesg();
			ok = False;
		}
	}
	if (header.isDefined("btype")) {
		const RecordInterface& subRec = header.asRecord("btype");
		if (subRec.dataType(0)==TpString) {
			String type;
			subRec.get(0, type);

			// We are going to cope with aips++ values and Miriad values
			// For Miriad there are a few extra ones (which we put on the Stokes
			// axis in aips++ - e.g. position angle).  For the ones that are common
			// the Miriad ones have underscores and the aips++ ones have spaces

			ImageInfo::ImageTypes imageType = ImageInfo::imageType(type);
			if (imageType != ImageInfo::Undefined) {
				setImageType(imageType);
			} else {
				imageType = MiriadImageType (type);
				if (imageType != ImageInfo::Undefined) {
					setImageType(imageType);
				}
			}
		}
		else {
			error(1) = "BTYPE field is not of type String";
			ok = False;
		}
	}
	if (header.isDefined("object")) {
		const RecordInterface& subRec = header.asRecord("object");
		if (subRec.dataType(0)==TpString || subRec.dataType(0)==TpArrayChar) {
			String objectName;
			subRec.get(0, objectName);
			setObjectName(objectName);
		}
		else {
			error(2) = "OBJECT field is not of type String";
			ok = False;
		}
	}
	if (ok) {
		error.resize(0);
	}
	return ok;
}

const ImageBeamSet& ImageInfo::getBeamSet() const {
	return _beams;
}

ostream &operator<<(ostream &os, const ImageInfo &info) {

	if (info.hasMultipleBeams()) {
		os << "Per plane beams: " << info.getBeamSet().getBeams() << endl;
	}
	else if (info.hasSingleBeam()) {
		GaussianBeam beam = info.getBeamSet().getBeam();
		os << "Restoring beam : " << beam.getMajor() << ", "
			<< beam.getMinor() << ", "
			<< beam.getPA(True) << endl;
	}
	os << "Image Type  = " << info.imageType(info.imageType()) << endl;
	os << "Object Name = " << info.objectName() << endl;
    return os;
}

Vector<String> ImageInfo::keywordNamesFITS() {
    Vector<String> vs(5);
    vs(0) = "bmaj";
    vs(1) = "bmin";
    vs(2) = "bpa";
    vs(3) = "btype";              // Miriad convention
    vs(4) = "object";
    return vs;
}  

ImageInfo::ImageTypes ImageInfo::MiriadImageType (
	const String& type
) {
	// We don't fully handle all the Miriad values because
	// some of them  (see below) are dealt with in aips++ by
	// the Stokes axis.
	String typeUp = upcase(type);
	if (typeUp==String("INTENSITY")) {
		return ImageInfo::Intensity;
	}
	if (typeUp==String("BEAM")) {
		return ImageInfo::Beam;
	}
	if (typeUp==String("COLUMN_DENSITY")) {
		return ImageInfo::ColumnDensity;
	}
	if (typeUp==String("DEPOLARIZATION_RATIO")) {
		return ImageInfo::DepolarizationRatio;
	}
	if (typeUp==String("KINETIC_TEMPERATURE")) {
		return ImageInfo::KineticTemperature;
	}
	if (typeUp==String("MAGNETIC_FIELD")) {
		return ImageInfo::MagneticField;
	}
	if (typeUp==String("OPTICAL_DEPTH")) {
		return ImageInfo::OpticalDepth;
	}
	if (typeUp==String("ROTATION_MEASURE")) {
		return ImageInfo::RotationMeasure;
	}
	if (typeUp==String("ROTATIONAL_TEMPERATURE")) {
		return ImageInfo::RotationalTemperature;
	}
	if (typeUp==String("SPECTRAL_INDEX")) {
		return ImageInfo::SpectralIndex;
	}
	if (typeUp==String("VELOCITY")) {
		return ImageInfo::Velocity;
	}
	if (typeUp==String("VELOCITY_DISPERSION")) {
		return ImageInfo::VelocityDispersion;
	}
	return ImageInfo::Undefined;
}

void ImageInfo::setBeam(
    const Int channel, const Int stokes, const Quantity& majAx,
	const Quantity& minAx, const Quantity& pa
) {
	GaussianBeam beam(majAx, minAx, pa);
	setBeam(channel, stokes, beam);
}

void ImageInfo::setBeam(
	const Int channel, const Int stokes, const GaussianBeam& beam
) {
	Int c = channel;
	Int s = stokes;
	_validateChannelStokes(c, s);
	IPosition plane = _beamPosition(c, s);
	_beams.setBeam(beam, plane);
}

Bool ImageInfo::hasMultipleBeams() const {
	return _beams.hasMultiBeam();
}

GaussianBeam ImageInfo::_getBeam(
	const Int channel, const Int stokes
) const {
	Int c = channel;
	Int s = stokes;
	_validateChannelStokes(c, s);
	return _beams.getBeam(_beamPosition(c, s));
}

void ImageInfo::_validateChannelStokes(
	Int& channel, Int& stokes
) const {
	if (! _beams.hasMultiBeam()) {
		throw AipsError("The per plane beam array shape has not been set");
	}
	uInt nChannels, nStokes;
	_getChansAndStokes(nChannels, nStokes);
	if (channel >= 0) {
		if (nChannels == 0) {
			throw AipsError(
				"Spectral channel specified but per plane beams has no channel axis"
			);
		}
		else if (channel >= (Int)nChannels) {
			ostringstream oss;
			oss << "Specified channel (" << channel
				<< ") exceeds number of channels (" << nChannels
				<< ") in per plane beams array.";
			throw AipsError(oss.str());
		}
	}
	else if (nChannels > 0) {
		if (nChannels == 1) {
			// silently support degenerate spectral axis
			channel = 0;
		}
		else {
			throw AipsError(
				"Channel not specified but must be"
			);
		}
	}
	if (stokes >= 0) {
		if (nStokes == 0) {
			throw AipsError(
				"Polarization specified but per plane beams has no stokes axis"
			);
		}
		if (stokes >= (Int)nStokes) {
			throw AipsError(
				_className() + "::" + __FUNCTION__
				+ ": Specified stokes (" + String::toString(stokes)
			    + ") exceeds number of stokes (" + String::toString(nStokes)
			    + ") in per plane beams array"
			);
		}
	}
	else if (nStokes > 0) {
		if (nStokes == 1) {
			// silently support degenerate polarization axis
			stokes = 0;
		}
		else {
			throw AipsError(
				"Stokes not specified but must be."
			);
		}
	}
}

void ImageInfo::setBeams(const ImageBeamSet& beams) {
	Vector<ImageBeamSet::AxisType> axisTypes = beams.getAxes();
	uInt ndim = beams.ndim();

	if (ndim == 0 || ndim > 2) {
		throw AipsError("Incorrect dimensionality for beams array");
	}
	for (
		Vector<ImageBeamSet::AxisType>::const_iterator iter = axisTypes.begin();
		iter != axisTypes.end(); iter++
	) {
		if (*iter != ImageBeamSet::SPECTRAL && *iter != ImageBeamSet::POLARIZATION) {
			throw AipsError("Unsupported axis type in ImageBeamSet");
		}
	}
	if (ndim == 2) {
		if (
			axisTypes[0] != ImageBeamSet::SPECTRAL
			|| axisTypes[1] != ImageBeamSet::POLARIZATION
		) {
			throw AipsError("Unsupported axis ordering in ImageBeamSet");
		}
	}
	_beams = beams;
}

void ImageInfo::setAllBeams(
	const uInt nChannels, const uInt nPolarizations,
	const GaussianBeam& beam
) {
	_setUpBeamArray(nChannels, nPolarizations);
	_beams.set(beam);
}

Bool ImageInfo::hasSingleBeam() const {
	return _beams.hasSingleBeam();
}

Bool ImageInfo::hasBeam() const {
	return _beams.hasSingleBeam()
		|| _beams.hasMultiBeam();
}

Record ImageInfo::beamToRecord(const Int channel, const Int stokes) const {
	if (_beams.nelements() == 0) {
		return Record();
	}
	if (_beams.nelements() == 1 || channel >= 0 || stokes >= 0) {
		return this->restoringBeam(channel, stokes).toRecord();
	}
	Record rstat;

	// return all multi beams in a record
	Record myRec;
	uInt nChannels, nStokes;
	_getChansAndStokes(nChannels, nStokes);
	rstat.define("nChannels", nChannels);
	rstat.define("nStokes", nStokes);
	Record beamRec;
	const Array<GaussianBeam>& beams = _beams.getBeams();
	if (_beams.ndim() == 1) {
		uInt i = 0;
		for (
			Array<GaussianBeam>::const_iterator iter=beams.begin();
			iter!=beams.end(); iter++, i++
		) {
			beamRec.defineRecord(
				"*" + String::toString(i), iter->toRecord()
			);
		}
	}
	else {
		for (uInt i=0; i<nChannels; i++) {
			Record chanRec;
			for (uint j=0; j<nStokes; j++) {
				chanRec.defineRecord(
					"*" + String::toString(j),
					_beams(IPosition(2, i, j)).toRecord()
				);
			}
			beamRec.defineRecord("*" + String::toString(i), chanRec);
		}
	}
	rstat.defineRecord("beams", beamRec);
	return rstat;
}

uInt ImageInfo::nChannels() const {
	uInt nChannels, nStokes;
    _getChansAndStokes(nChannels, nStokes);
    return nChannels;
}

uInt ImageInfo::nStokes() const {
	uInt nChannels, nStokes;
	_getChansAndStokes(nChannels, nStokes);
	return nStokes;
}

ImageInfo ImageInfo::adaptMultiBeam (const CoordinateSystem& coords,
                                     const IPosition& shape,
                                     const String& imageName,
                                     LogIO& logSink) const
{
  // If the image has multiple beams, check them.
  // Adapt the info as needed.
  AlwaysAssert(hasMultipleBeams(), AipsError);
  logSink << LogOrigin("ImageInfo", __FUNCTION__);
  ImageInfo cinfo(*this);
  if (! coords.hasDirectionCoordinate()) {
    logSink << "This image has no direction coordinate so cannot "
            << "have per plane beams." << LogIO::EXCEPTION;
  }
  Int beamChannels = cinfo.nChannels();
  const Array<GaussianBeam>& beams = cinfo.getBeamSet().getBeams();
  if (beamChannels == 0) {
    if (coords.hasSpectralAxis()) {
      Int specAxisNum = coords.spectralAxisNumber();
      if (shape[specAxisNum] == 1) {
        // allow silent reforming of the array in the case of a degenerate
        // spectral axis
        Vector<ImageBeamSet::AxisType> axisTypes(2);
        axisTypes[0] = ImageBeamSet::SPECTRAL;
        axisTypes[1] = ImageBeamSet::POLARIZATION;
        ImageBeamSet newBeamSet(IPosition(2, 1, beams.size()), axisTypes);
        Array<GaussianBeam> newBeams = newBeamSet.getBeams();
        Array<GaussianBeam>::const_iterator citer = beams.begin();
        for (
             Array<GaussianBeam>::iterator iter=newBeams.begin();
             iter!=newBeams.end(); ++iter, ++citer
             ) {
          *iter = *citer;
        }
        newBeamSet.setBeams(newBeams);
        cinfo.setBeams(newBeamSet);
      } else {
        logSink << "The per plane beams array does not "
                << "define a spectral axis but this image has "
                << "a spectral axis" << LogIO::EXCEPTION;
      }
    }
  } else if (! coords.hasSpectralAxis()) {
    if (beamChannels == 1) {
      // silently allow reforming of the array in case it has a
      // degenerate spectral/temporal axis
      Vector<ImageBeamSet::AxisType> axisTypes(1);
      axisTypes[0] = ImageBeamSet::POLARIZATION;
      ImageBeamSet newBeamSet(IPosition(1, beams.size()), axisTypes);
      Array<GaussianBeam> newBeams = newBeamSet.getBeams();
      Array<GaussianBeam>::const_iterator citer = beams.begin();
      for (
           Array<GaussianBeam>::iterator iter=newBeams.begin();
           iter!=newBeams.end(); ++iter, ++citer
           ) {
        *iter = *citer;
      }
      newBeamSet.setBeams(newBeams);
      cinfo.setBeams(newBeamSet);
    } else {
      logSink << "The per plane beams array defines a spectral axis but "
              << imageName << " does not have a spectral axis"
              << LogIO::EXCEPTION;
    }
  } else if (coords.hasSpectralAxis() &&
             beamChannels != shape[coords.spectralAxisNumber()]) {
    logSink << "The number of channels defined in the per plane beams array ("
            << beamChannels << ") is "
            << "different from the number of image spectral channels("
            << shape[coords.spectralAxisNumber()] << ")"
            << LogIO::EXCEPTION;
  }
  Int beamStokes = nStokes();
  if (beamStokes == 0) {
    if (coords.hasPolarizationCoordinate()) {
      Int polAxisNum = coords.polarizationAxisNumber();
      if (shape[polAxisNum] == 1) {
        // allow silent reforming of the array in the case of a degenerate
        // polarization axis
        Vector<ImageBeamSet::AxisType> axisTypes(2);
        axisTypes[0] = ImageBeamSet::SPECTRAL;
        axisTypes[1] = ImageBeamSet::POLARIZATION;
        ImageBeamSet newBeamSet(IPosition(2, beams.size(), 1), axisTypes);
        Array<GaussianBeam> newBeams = newBeamSet.getBeams();
        Array<GaussianBeam>::const_iterator citer = beams.begin();
        for (
             Array<GaussianBeam>::iterator iter=newBeams.begin();
             iter!=newBeams.end(); iter++, citer++
             ) {
          *iter = *citer;
        }
        newBeamSet.setBeams(newBeams);
        cinfo.setBeams(newBeamSet);
      } else {
        logSink << "The per hyper-beams array does not define a polarization "
                << "axis but this image has a polarization axis"
                << LogIO::EXCEPTION;
      }
    }
  } else if (! coords.hasPolarizationCoordinate()) {
    if (beamStokes == 1) {
      // silently allow reforming of an array with a degenerate
      // polarization axis
      Vector<ImageBeamSet::AxisType> axisTypes(1);
      axisTypes[0] = ImageBeamSet::SPECTRAL;
      ImageBeamSet newBeamSet(IPosition(1, beams.size()), axisTypes);
      Array<GaussianBeam> newBeams = newBeamSet.getBeams();
      Array<GaussianBeam>::const_iterator citer = beams.begin();
      for (
           Array<GaussianBeam>::iterator iter=newBeams.begin();
           iter!=newBeams.end(); ++iter, ++citer
           ) {
        *iter = *citer;
      }
      newBeamSet.setBeams(newBeams);
      cinfo.setBeams(newBeamSet);
    } else {
      logSink << "ImageInterface::setImageInfo(): The per plane "
              << "beams array defines a polarization axis but this image "
              << "does not have a polarization axis" << LogIO::EXCEPTION;
    }
  } else if (beamStokes != shape[coords.polarizationAxisNumber()]) {
    logSink << "The number of polarizations defined in the per "
      "plane beam array is different from the number of image polarizations"
            << LogIO::EXCEPTION;
  }
  for (
       Array<GaussianBeam>::const_iterator iter=beams.begin();
       iter!=beams.end(); ++iter
       ) {
    if (iter->isNull()) {
      logSink << "At least one of the beams in the beam set of "
              << imageName << " is null and thus invalid" << LogIO::EXCEPTION;
    }
  }
  return cinfo;
}    

} //# NAMESPACE CASA - END

