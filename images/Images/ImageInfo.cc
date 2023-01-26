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

#include <casacore/images/Images/ImageInfo.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/QuantumHolder.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/tables/LogTables/LoggerHolder.h>
#include <casacore/casa/sstream.h>
#include <casacore/coordinates/Coordinates/DirectionCoordinate.h>
#include <casacore/coordinates/Coordinates/SpectralCoordinate.h>


#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ImageInfo::ImageInfo()
: _beams(ImageBeamSet()),
  _warnBeam(true),
  itsImageType(defaultImageType()),
  itsObjectName(defaultObjectName()) {}

ImageInfo::~ImageInfo()
{}

void ImageInfo::copy_other(const ImageInfo &other)
{
    if (this != &other) {
       _beams = other._beams;
       _warnBeam = other._warnBeam;
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
   return String();
}

GaussianBeam ImageInfo::defaultRestoringBeam() {
   static const GaussianBeam x;
   return x;
}

GaussianBeam ImageInfo::restoringBeam(
    const int32_t channel, const int32_t polarization
) const {
	if (_beams.empty()) {
		// return a null beam
		return defaultRestoringBeam();
    }
	else if (_beams.nelements() == 1) {
		return _beams.getBeam();
    }
    else {
    	return _beams.getBeam(channel, polarization);
    }
}


void ImageInfo::setRestoringBeam(const GaussianBeam& beam) {
	ThrowIf (
		_beams.hasMultiBeam(),
		"This object has multiple beams. They must be removed before you can define a single global restoring beam"
	);
    ThrowIf(
    	beam.isNull(),
    	"Beam is null and therefore invalid."
    );
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
	GaussianBeam restoringBeam = GaussianBeam::fromRecord(inRecord.asRecord("restoringbeam"));
	setRestoringBeam(restoringBeam);
}

void ImageInfo::removeRestoringBeam() {
   _beams = ImageBeamSet();
}

bool ImageInfo::getRestoringBeam (LoggerHolder& logger) {
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
			for (int32_t i=0; i<n; i++) {
				if (s[i].contains("BMAJ")) {
					istringstream oss(s[i+1].chars());
					double x;
					oss >> x;
					if (x <= 0) {
						return false;
					}
					major = Quantity(x, Unit(String("deg")));
				}
				else if (s[i].contains("BMIN")) {
					istringstream oss(s[i+1].chars());
					double x;
					oss >> x;
					if (x <= 0) {
						return false;
					}
					minor = Quantity(x, Unit(String("deg")));
				}
				else if (s[i].contains("BPA")) {
					istringstream oss(s[i+1].chars());
					double x;
					oss >> x;
					pa = Quantity(x, Unit(String("deg")));
				}
			}
			if (!(minor.isConform("rad") && major.isConform("rad") && pa.isConform("rad"))
			    || (minor.getValue() > major.getValue())
			    ) {
			        return false;
			}
			_beams = ImageBeamSet(GaussianBeam(major, minor, pa));
			return true;
		}
	}
	return false;
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
   for (uint32_t i=0; i<ImageInfo::nTypes; i++) {
      ImageInfo::ImageTypes t0 = static_cast<ImageInfo::ImageTypes>(i);
      String t1Up = upcase(ImageInfo::imageType(t0));
      if (t1Up==typeUp) {
    	  return t0;
      }
   }
   return defaultImageType();
}

ImageInfo::ImageTypes ImageInfo::imageTypeFromFITS (int32_t value) {
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

bool ImageInfo::toRecord(
    String & error, RecordInterface & outRecord
) const {
    error = "";
    bool ok = true;
    // If the beam is null, don't do anything as it will get
    // restored as null as well if it is not in the record
    if (_beams.hasSingleBeam()) {
       Record restoringBeamRecord = _beams.getBeam().toRecord();
       outRecord.defineRecord("restoringbeam", restoringBeamRecord);
    }
    outRecord.define("imagetype", ImageInfo::imageType(itsImageType));
    outRecord.define("objectname", itsObjectName);
    if (_beams.hasMultiBeam()) {
    	try {
    		outRecord.defineRecord("perplanebeams", _beams.toRecord());
    	}
    	catch (const AipsError& x) {
    		error = x.getLastMessage();
    		return false;
    	}
    	catch (const std::exception& x) {
        error = x.what();
        return false;
      }
    }
    return ok;
}

bool ImageInfo::fromRecord(String& error, const RecordInterface& inRecord) {
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
		setImageType(ImageInfo::imageType(type));
	}
	if (inRecord.isDefined("objectname")) {
		String objectName = inRecord.asString("objectname");
		setObjectName(objectName);
	}
	if (inRecord.isDefined("perplanebeams")) {
		Record hpBeams = inRecord.asRecord("perplanebeams");
		_beams = ImageBeamSet::fromRecord(hpBeams);
	}
	return true;
}

bool ImageInfo::toFITS(String & error, RecordInterface & outRecord) const {
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
          int32_t field = outRecord.fieldNumber("bmaj");
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
          int32_t field = outRecord.fieldNumber("btype");
          if (field >= 0) outRecord.removeField(field);
       }
    }
    {
       outRecord.define("object", itsObjectName);
    }

    return true;
}

bool ImageInfo::fromFITS(
		Vector<String>& error, const RecordInterface& header
) {
	// keyname
	//   value          - required
	//   unit           - optional
	//   comment        - optional
	error.resize(3);
	bool ok = true;
	ImageInfo tmp;
	(*this) = tmp; // Make sure we are "empty" first;
	if (
		header.isDefined("bmaj") && header.isDefined("bmin")
		&& header.isDefined("bpa")
	) {
		const RecordInterface& subRec0 = header.asRecord("bmaj");
		const RecordInterface& subRec1 = header.asRecord("bmin");
		const RecordInterface& subRec2 = header.asRecord("bpa");
		double bmaj, bmin, bpa;
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
				ok = false;
			}
		}
		catch(const std::exception& x) {
			error(0) = std::string("ERROR reading BMAJ, BMIN, BPA: ") + x.what();
			ok = false;
		}
	}
	if (header.isDefined("btype")) {
		const RecordInterface& subRec = header.asRecord("btype");
		if (subRec.dataType(0)==TpString) {
			String type;
			subRec.get(0, type);

			// We are going to cope with Casacore values and Miriad values
			// For Miriad there are a few extra ones (which we put on the Stokes
			// axis in Casacore - e.g. position angle).  For the ones that are common
			// the Miriad ones have underscores and the Casacore ones have spaces

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
			ok = false;
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
			ok = false;
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
			<< beam.getPA(true) << endl;
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
	// some of them  (see below) are dealt with in Casacore by
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
    const int32_t channel, const int32_t stokes, const Quantity& majAx,
	const Quantity& minAx, const Quantity& pa
) {
	GaussianBeam beam(majAx, minAx, pa);
	setBeam(channel, stokes, beam);
}

void ImageInfo::setBeam(
    const int32_t channel, const int32_t stokes, const GaussianBeam& beam
) {
	ThrowIf(
		_beams.empty(),
		"Logic error: setAllBeams() or setBeams() must be called prior to setBeam()"
	);
    _beams.setBeam(channel, stokes, beam);
}

void ImageInfo::setBeams(const ImageBeamSet& beams) {
    _beams = beams;
}

void ImageInfo::setAllBeams(
    const uint32_t nChannels, const uint32_t nPolarizations,
    const GaussianBeam& beam
) {
	_beams.resize (nChannels, nPolarizations);
    _beams.set(beam);
}

Record ImageInfo::beamToRecord(const int32_t channel, const int32_t stokes) const {
    if (_beams.nelements() == 0) {
        return Record();
    }
    if (_beams.nelements() == 1 || channel >= 0 || stokes >= 0) {
        return this->restoringBeam(channel, stokes).toRecord();
    }
    Record rstat;

	// return all multi beams in a record
	Record myRec;
	uint32_t nchan =  _beams.nchan();
	uint32_t nstokes = _beams.nstokes();
	rstat.define("nChannels", nchan);
	rstat.define("nStokes", nstokes);
	Record beamRec;
	for (uint32_t i = 0; i < nchan; i++) {
		Record chanRec;
		for (uint32_t j = 0; j < nstokes; j++) {
			chanRec.defineRecord("*" + String::toString(j),
					_beams(i, j).toRecord());
		}
		beamRec.defineRecord("*" + String::toString(i), chanRec);
	}
	rstat.defineRecord("beams", beamRec);
	return rstat;
}

void ImageInfo::checkBeamSet(
	const CoordinateSystem& coords,
	const IPosition& shape,
	const String& imageName
) const {
	if (!hasBeam()) {
		return;
	}
	// Adapt the info as needed.
	/*
   // removing this constraint because PV images do not have a direction coordinate
   // but users still want to carry beam information along.
  	  if (! coords.hasDirectionCoordinate()) {
    logSink << "Image " << imageName << " has no direction coordinate so "
            << "cannot have per plane beams." << LogIO::EXCEPTION;
  	  }
	 */
	uint32_t beamChannels = _beams.nchan();
	uint32_t crdChannels = 1;
	if (coords.hasSpectralAxis()) {
		int32_t specAxisNum = coords.spectralAxisNumber();
		crdChannels = shape[specAxisNum];
	}
	uint32_t beamStokes = _beams.nstokes();
	uint32_t crdStokes = 1;
	if (coords.hasPolarizationCoordinate()) {
		int32_t polAxisNum = coords.polarizationAxisNumber();
		crdStokes = shape[polAxisNum];
	}
	// Either the imageinfo has 1 channel or crdChannels channels.
	// Same for Stokes.
	ThrowIf(
		beamChannels != 1  &&  beamChannels != crdChannels,
		"Number of channels is not consistent"
	);
	ThrowIf(
		beamStokes != 1  &&  beamStokes != crdStokes,
		"Number of polarizations is not consistent"
	);
	// Check if no null beams.
	Array<GaussianBeam>::const_iterator iterEnd=_beams.getBeams().end();
	for (
		Array<GaussianBeam>::const_iterator iter=_beams.getBeams().begin();
		iter!=iterEnd; ++iter
	) {
		ThrowIf(
			iter->isNull(),
			"At least one of the beams in the beam set of "
			+ imageName + " is null and thus invalid"
		);
	}
}

void ImageInfo::_checkBeamShape (uint32_t& nchan, uint32_t& npol,
                                 const IPosition& shape,
                                 const CoordinateSystem& csys) const
{
  nchan = 0;
  if (csys.hasSpectralAxis()) {
    nchan = shape[csys.spectralAxisNumber()];
  }
  AlwaysAssert (getBeamSet().nchan() == nchan  ||
                getBeamSet().nchan() == 1, AipsError);
  npol = 0;
  if (csys.hasPolarizationCoordinate()) {
    npol = shape[csys.polarizationAxisNumber()];
  }
  AlwaysAssert (getBeamSet().nstokes() == npol  ||
                getBeamSet().nstokes() == 1, AipsError);
}

void ImageInfo::combineBeams (const ImageInfo& infoThat,
                              const IPosition& shapeThis,
                              const IPosition& shapeThat,
                              const CoordinateSystem& csysThis,
                              const CoordinateSystem& csysThat,
                              int32_t axis,
                              bool relax,
                              LogIO& os)
{
  ImageBeamSet beamSet;
  // Check if coord shape and beam shape match.
  uint32_t nchan1, npol1, nchan2, npol2;
  if (hasBeam()) {
    this->_checkBeamShape (nchan1, npol1, shapeThis, csysThis);
  }
  if (infoThat.hasBeam()) {
    infoThat._checkBeamShape (nchan2, npol2, shapeThat, csysThat);
  }
  // No beams if one info has no beams.
  if (hasBeam() != infoThat.hasBeam()) {
    logMessage (_warnBeam, os, relax,
                "One image does not have a beam while another does",
                "The concat image will have no beam");
  } else if (hasBeam()) {
    // Both have a beam.
    // Concatenate if a beam axis is the concatenation axis.
    // Otherwise combine the beam sets.
    if (axis == csysThis.spectralAxisNumber()) {
      concatFreqBeams (beamSet, infoThat, nchan1, nchan2, relax, os);
    } else if (axis == csysThis.polarizationAxisNumber()) {
      concatPolBeams (beamSet, infoThat, npol1, npol2, relax, os);
    } else {
      mergeBeams (beamSet, infoThat, relax, os);
    }
  }
  _beams = beamSet;
}

uint32_t ImageInfo::setInfoSplitBeamSet (uint32_t ndone, const ImageInfo& concatInfo,
                                     const IPosition& shape,
                                     const CoordinateSystem& csys, int32_t concatAxis)
{
  // Copy the non-beam info.
  _warnBeam     = concatInfo._warnBeam;
  itsImageType  = concatInfo.itsImageType;
  itsObjectName = concatInfo.itsObjectName;
  // Copy the beam info, if needed part of it.
  // If the concat is not freq nor stokes, the entire beam info can be copied.
  // This is also the case if the beam axes have length 1.
  // Otherwise part of the beamset has to be taken.
  IPosition st(shape.size(), 0);
  IPosition ss(shape);
  st[concatAxis] = ndone;
  if (csys.hasSpectralAxis()  &&
      concatAxis == csys.spectralAxisNumber()  &&
      concatInfo.getBeamSet().nchan() > 1) {
    setBeams (concatInfo.getBeamSet().subset (Slicer(st, ss), csys));
    return shape[concatAxis];
  } else if (csys.hasPolarizationAxis()  &&
             concatAxis == csys.polarizationAxisNumber()  &&
             concatInfo.getBeamSet().nstokes() > 1) {
    setBeams (concatInfo.getBeamSet().subset (Slicer(st, ss), csys));
    return shape[concatAxis];
  }
  // Set to the entire beam set.
  setBeams (concatInfo.getBeamSet());
  return 1;
}

void ImageInfo::concatFreqBeams (ImageBeamSet& beamsOut,
                                 const ImageInfo& infoThat,
                                 int32_t nchanThis,
                                 int32_t nchanThat,
                                 bool,
                                 LogIO&) const
{
  // Determine the number of beams for the axes in both sets.
  int32_t nc1 = _beams.nchan();
  int32_t np1 = _beams.nstokes();
  int32_t nc2 = infoThat.getBeamSet().nchan();
  int32_t np2 = infoThat.getBeamSet().nstokes();
  AlwaysAssert (nc1 == nchanThis  ||  nc1 == 1, AipsError);
  AlwaysAssert (nc2 == nchanThat  ||  nc2 == 1, AipsError);
  AlwaysAssert (np1 == np2  ||  np1 == 1  ||  np2 == 1, AipsError);
  // If the first beam axis has size 1 and the beamsets are equivalent,
  // a first beamset can be used.
  // Note: in principle the same test could be done if nc2==1, but chances
  // are very low such a test is true, thus it is a waste of time.
  if (nc1 == 1  &&  _beams.equivalent(infoThat.getBeamSet())) {
    beamsOut = _beams;
    return;
  }
  // Determine nr of output beams in both axes.
  // The concat axis is the sum of the image axes.
  int32_t nc = nchanThis+nchanThat;
  int32_t np = max(np1,np2);
  // Now concatenate the beams.
  beamsOut.resize (nc,np);
  for (int32_t ip=0; ip<np; ++ip) {
    for (int32_t ic=0; ic<nchanThis; ++ic) {
      beamsOut.setBeam (ic, ip, _beams.getBeam(ic,ip));
    }
  }
  for (int32_t ip=0; ip<np; ++ip) {

    for (int32_t ic=0; ic<nchanThat; ++ic) {
      beamsOut.setBeam (ic+nchanThis, ip,
                        infoThat.getBeamSet().getBeam(ic,ip));
    }
  }
}

void ImageInfo::concatPolBeams (ImageBeamSet& beamsOut,
                                const ImageInfo& infoThat,
                                int32_t npolThis,
                                int32_t npolThat,
                                bool,
                                LogIO&) const
{
  // Determine the number of beams for the axes in both sets.
  int32_t nc1 = _beams.nchan();
  int32_t np1 = _beams.nstokes();
  int32_t nc2 = infoThat.getBeamSet().nchan();
  int32_t np2 = infoThat.getBeamSet().nstokes();
  AlwaysAssert (np1 == npolThis  ||  np1 == 1, AipsError);
  AlwaysAssert (np2 == npolThat  ||  np2 == 1, AipsError);
  AlwaysAssert (nc1 == nc2  ||  nc1 == 1  ||  nc2 == 1, AipsError);
  // If the first beam axis has size 1 and the beamsets are equivalent,
  // a first beamset can be used.
  // Note: in principle the same test could be done for np2==1, but chances
  // are very low such a test is true, thus it is a waste of time.
  if (np1 == 1  &&  _beams.equivalent(infoThat.getBeamSet())) {
    beamsOut = _beams;
    return;
  }
  // Determine nr of output beams in both axes.
  // The concat axis is the sum of the image axes.
  int32_t np = npolThis+npolThat;
  int32_t nc = max(nc1,nc2);
  // Now concatenate the beams.
  beamsOut.resize (nc,np);
  for (int32_t ip=0; ip<npolThis; ++ip) {
    for (int32_t ic=0; ic<nc; ++ic) {
      beamsOut.setBeam (ic, ip, _beams.getBeam(ic,ip));
    }
  }
  for (int32_t ip=0; ip<npolThat; ++ip) {
    for (int32_t ic=0; ic<nc; ++ic) {
      beamsOut.setBeam (ic, ip+npolThis,
                        infoThat.getBeamSet().getBeam(ic,ip));
    }
  }
}

void ImageInfo::mergeBeams (ImageBeamSet& beamsOut,
                            const ImageInfo& infoThat,
                            bool relax,
                            LogIO& os) const
{
  // Determine the number of beams for the axes in both sets.
  int32_t nc1 = _beams.nchan();
  int32_t np1 = _beams.nstokes();
  int32_t nc2 = infoThat.getBeamSet().nchan();
  int32_t np2 = infoThat.getBeamSet().nstokes();
  AlwaysAssert (nc1 == nc2  ||  nc1 == 1  ||  nc2 == 1, AipsError);
  AlwaysAssert (np1 == np2  ||  np1 == 1  ||  np2 == 1, AipsError);
  // Determine nr of output beams in both axes.
  int32_t nc = max(nc1,nc2);
  int32_t np = max(np1,np2);
  if (nc1 == nc  &&  np1 == np) {
    if (! _beams.equivalent (infoThat.getBeamSet())) {
      logMessage (_warnBeam, os, relax,
                  "Beams of images are not equivalent",
                  "The resulting image will have the first image's beams.");
    }
    beamsOut = _beams;
  } else if (nc2 == nc  &&  np2 == np){
    if (! _beams.equivalent (infoThat.getBeamSet())) {
      logMessage (_warnBeam, os, relax,
                  "Beams of images are not equivalent",
                  "The resulting image will have the second image's beams.");
    }
    beamsOut = infoThat.getBeamSet();
  } else {
    logMessage (_warnBeam, os, relax,
                "One beam set varies in frequency, the other in polarization",
                "using the frequency beam set");
    if (nc1 == nc) {
      beamsOut = _beams;
    } else {
      beamsOut = infoThat.getBeamSet();
    }
  }
}

void ImageInfo::logMessage (bool& warn, LogIO& os, bool relax,
                            const String& msg1, const String msg2)
{
  if (relax) {
    if (warn) {
      os << LogIO::WARN << msg1 << " " << msg2 << LogIO::POST;
      warn = false;
    }
  } else {
    os << msg1 << LogIO::EXCEPTION;
  }
}

double ImageInfo::getBeamAreaInPixels(
    int32_t channel, int32_t stokes, const DirectionCoordinate& dc
) const {
	ThrowIf(
		! hasBeam(),
		"There is no beam set associated with this object"
	);
	return getBeamAreaInPixels(restoringBeam(channel, stokes), dc);
}

double ImageInfo::getBeamAreaInPixels(
	const GaussianBeam& beam, const DirectionCoordinate& dc
) {
	Quantity pixelArea = dc.getPixelArea();
	return beam.getArea(pixelArea.getUnit())/pixelArea.getValue();
}

} //# NAMESPACE CASACORE - END

