//# <ClassFileName.h>: this defines <ClassName>, which ...
//# Copyright (C) 1997
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

#include <trial/Coordinates/SpectralCoordinate.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Utilities/Assert.h>
#include <aips/Containers/Record.h>
#include <aips/Functionals/Interpolate1D.h>
#include <aips/Functionals/ScalarSampledFunctional.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Containers/RecordInterface.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>
#include <trial/FITS/FITSUtil.h>

#if defined(__GNUG__)
typedef Interpolate1D<Double,Double> gpp_bug;
#endif

SpectralCoordinate::SpectralCoordinate(MFrequency::Types type,
				       Double f0, Double inc, Double refChan,
				       Double restFrequency)
 : type_p(type), crval_p(f0), cdelt_p(inc), restfreq_p(restFrequency),
   crpix_p(refChan), unit_p("Hz"), name_p("Frequency"), matrix_p(1.0),
   channel_corrector_p(0), channel_corrector_rev_p(0)
{
    // Nothing
}

SpectralCoordinate::SpectralCoordinate()
 : type_p(MFrequency::TOPO), crval_p(0.0), cdelt_p(0.0), restfreq_p(0.0),
   crpix_p(0), unit_p("Hz"), name_p("Frequency"), matrix_p(1.0),
   channel_corrector_p(0), channel_corrector_rev_p(0)
{
    // Nothing
}

SpectralCoordinate::SpectralCoordinate(
		       MFrequency::Types type, const Vector<Double> &freqs,
		       Double restFrequency)
    : type_p(type), crval_p(0), cdelt_p(0), restfreq_p(restFrequency),
      crpix_p(0.0), unit_p("Hz"), name_p("Frequency"), matrix_p(1.0),
      channel_corrector_p(0), channel_corrector_rev_p(0)
{
    const uInt nchan = freqs.nelements();

    if (nchan < 2) {
	throw(AipsError("SpectralCoordinate - frequency table must have >1"
			" member"));
    } 

    // Work out "global" crval etc.
    crval_p = freqs(0);
    cdelt_p = (freqs(nchan-1) - freqs(0)) / Double(nchan - 1);
    crpix_p = 0.0;

    if (cdelt_p == 0.0) {
	throw(AipsError("SpectralCoordinate - start and "
			"end frequency must differ"));
    }
    
    Double sign = (cdelt_p > 0 ? 1.0 : -1.0);

    // Check that freqs is ok - monotonically increasing or decreasing
    for (uInt i=1; i<nchan; i++) {
	Double diff = sign*(freqs(i) - freqs(i-1));
	if (diff <= 0) {
	    throw(AipsError("SpectralCoordinate - frequency table must increas "
			    "or decrease monotonically"));
	}
    }
    
    Vector<Double> chann_in(nchan), chann_diff(nchan);
    Vector<Double> world(1), pixel(1);

    for (i=0; i<nchan; i++) {
	world(0) = freqs(i);
	AlwaysAssert(toPixel(pixel, world), AipsError);
	chann_in(i) = Double(i);
	chann_diff(i) = pixel(0) - chann_in(i);;
    }
    ScalarSampledFunctional<Double> c_in(chann_in), c_diff(chann_diff);

    channel_corrector_p = 
      new Interpolate1D<Double,Double>(c_in, c_diff, True, True);
    channel_corrector_rev_p = 
      new Interpolate1D<Double,Double>(c_diff, c_in, True, True);
    AlwaysAssert(channel_corrector_p != 0 &&
		 channel_corrector_rev_p != 0, AipsError);
    channel_corrector_p->setMethod(Interpolate1D<Double,Double>::linear);
    channel_corrector_rev_p->setMethod(Interpolate1D<Double,Double>::linear);
				      
							   
}


SpectralCoordinate::SpectralCoordinate(const SpectralCoordinate &other)
{
    type_p = other.type_p;
    crval_p = other.crval_p;
    cdelt_p = other.cdelt_p;
    crpix_p = other.crpix_p;
    unit_p = other.unit_p;
    name_p = other.name_p;
    matrix_p = other.matrix_p;
    restfreq_p = other.restfreq_p;
    channel_corrector_p = 0;
    if (other.channel_corrector_p) {
      channel_corrector_p = 
	  new Interpolate1D<Double,Double>(*other.channel_corrector_p);
      channel_corrector_rev_p = 
	  new Interpolate1D<Double,Double>(*other.channel_corrector_rev_p);
      AlwaysAssert(channel_corrector_p != 0 &&
		   channel_corrector_rev_p != 0, AipsError);
    }
}

SpectralCoordinate &SpectralCoordinate::operator=(
					  const SpectralCoordinate &other)
{
    if (this != &other) {
	type_p = other.type_p;
	crval_p = other.crval_p;
	cdelt_p = other.cdelt_p;
	crpix_p = other.crpix_p;
	unit_p = other.unit_p;
	name_p = other.name_p;
	matrix_p = other.matrix_p;
	restfreq_p = other.restfreq_p;
	channel_corrector_p = 0;
	if (other.channel_corrector_p) {
	    channel_corrector_p = 
		new Interpolate1D<Double,Double>(*other.channel_corrector_p);
	    channel_corrector_rev_p = 
		new Interpolate1D<Double,Double>(*other.channel_corrector_rev_p);
	    AlwaysAssert(channel_corrector_p != 0 &&
			 channel_corrector_rev_p != 0, AipsError);
	}
    }
    return *this;
}

SpectralCoordinate::~SpectralCoordinate()
{
    if (channel_corrector_p) {
	delete channel_corrector_p;
	delete channel_corrector_rev_p;
	channel_corrector_p = 0;
	channel_corrector_rev_p = 0;
    }
}

Coordinate::Type SpectralCoordinate::type() const
{
    return Coordinate::SPECTRAL;
}

uInt SpectralCoordinate::nPixelAxes() const
{
    return 1;
}

uInt SpectralCoordinate::nWorldAxes() const
{
    return 1;
}

Bool SpectralCoordinate::toWorld(Vector<Double> &world, 
				 const Vector<Double> &pixel) const
{
    AlwaysAssert(world.nelements() == 1 && pixel.nelements() == 1, AipsError);
    Double channel = pixel(0);
    if (channel_corrector_p) {
	channel += (*channel_corrector_p)(channel);
    }

    world(0) = crval_p + cdelt_p * matrix_p * (channel - crpix_p);
    return True;
}

Bool SpectralCoordinate::toPixel(Vector<Double> &pixel, 
				 const Vector<Double> &world) const
{
    AlwaysAssert(world.nelements() == 1 && pixel.nelements() == 1, AipsError);
    Double channel = (world(0) - crval_p) / (cdelt_p * matrix_p) + crpix_p;
    if (channel_corrector_rev_p) {
	channel -= (*channel_corrector_rev_p)(channel);
    }
    pixel(0) = channel;
    return True;
}


Vector<String> SpectralCoordinate::worldAxisNames() const
{
    Vector<String> names(1);
    names = name_p;
    return names;
}

Vector<String> SpectralCoordinate::worldAxisUnits() const
{
    Vector<String> units(1);
    units = unit_p;
    return units;
}

Vector<Double> SpectralCoordinate::referencePixel() const
{
    Vector<Double> crpix(1);
    crpix = crpix_p;
    return crpix;
}

Matrix<Double> SpectralCoordinate::linearTransform() const
{
    Matrix<Double> matrix(1,1);
    matrix(0,0) = matrix_p;
    return matrix;
}

Vector<Double> SpectralCoordinate::increment() const
{
    Vector<Double> cdelt(1);
    cdelt = cdelt_p;
    return cdelt;
}

Vector<Double> SpectralCoordinate::referenceValue() const
{
    Vector<Double> crval(1);
    crval = crval_p;
    return crval;
}



Bool SpectralCoordinate::setWorldAxisNames(const Vector<String> &names)
{
    AlwaysAssert(names.nelements() == 1, AipsError);
    name_p = names(0);
    return True;
}

Bool SpectralCoordinate::setWorldAxisUnits(const Vector<String> &units,
					   Bool adjust)
{
    Double before = cdelt_p;
    AlwaysAssert(units.nelements() == 1, AipsError);
    Bool ok = Coordinate::setWorldAxisUnits(units, adjust);
    if (ok) {
	unit_p = units(0);
	Double after = cdelt_p;
	restfreq_p *= after / before;
    }
    return ok;
}


Bool SpectralCoordinate::setReferencePixel(const Vector<Double> &refPix)
{
    AlwaysAssert(refPix.nelements() == 1, AipsError);
    crpix_p = refPix(0);
    return True;
}

Bool SpectralCoordinate::setLinearTransform(const Matrix<Double> &xform)
{
    AlwaysAssert(xform.nelements() == 1, AipsError);
    matrix_p = xform(0,0);
    return True;
}

Bool SpectralCoordinate::setIncrement(const Vector<Double> &inc) 
{
    AlwaysAssert(inc.nelements() == 1, AipsError);
    cdelt_p = inc(0);
    return True;
}

Bool SpectralCoordinate::setReferenceValue(const Vector<Double> &refval)
{
    AlwaysAssert(refval.nelements() == 1, AipsError);
    crval_p = refval(0);
    return True;
}

Double SpectralCoordinate::restFrequency() const
{
    return restfreq_p;
}

Vector<Double> SpectralCoordinate::pixelCorrections() const
{
    Vector<Double> retval;
    if (channel_corrector_p) {
	retval = channel_corrector_p->getY();
    }
    return retval;
}

MFrequency::Types SpectralCoordinate::frequencySystem() const
{
    return type_p;
}

void  SpectralCoordinate::setFrequencySystem(MFrequency::Types type)
{
    type_p = type;
}


Bool SpectralCoordinate::setRestFrequency(Double newFrequency)
{
    restfreq_p = newFrequency;
    return True;
}

Bool SpectralCoordinate::save(RecordInterface &container,
			    const String &fieldName) const
{
    Bool ok = ToBool(!container.isDefined(fieldName));
    if (ok) {
	Record subrec;
	String system = "unknown";
	switch (type_p) {
	case MFrequency::REST: system = "REST"; break;
	case MFrequency::LSR: system = "LSR"; break;
	case MFrequency::LSRK: system = "LSRK"; break;
	case MFrequency::BARY: system = "BARY"; break;
	case MFrequency::GEO: system = "GEO"; break;
	case MFrequency::TOPO: system = "TOPO"; break;
	case MFrequency::GALACTO: system = "GALACTO"; break;
	}
	subrec.define("system", system);
	subrec.define("crval", referenceValue());
	subrec.define("crpix", referencePixel());
	subrec.define("cdelt", increment());
	subrec.define("pc", linearTransform());
	subrec.define("axes", worldAxisNames());
	subrec.define("units", worldAxisUnits());
	subrec.define("restfreq", restFrequency());
	if (channel_corrector_p) {
	    subrec.define("pixelcorrections", pixelCorrections());
	}

	container.defineRecord(fieldName, subrec);
    }
    return ok;
}

SpectralCoordinate *SpectralCoordinate::restore(const RecordInterface &container,
					   const String &fieldName)
{
    if (! container.isDefined(fieldName)) {
	return 0;
    }

    Record subrec(container.asRecord(fieldName));
    
    // We should probably do more type-checking as well as checking
    // for existence of the fields.
    if (! subrec.isDefined("system")) {
	return 0;
    }
    String system;
    subrec.get("system", system);
    MFrequency::Types sys;
    if (system == "REST") {
	sys = MFrequency::REST;
    } else if (system == "LSR") {
	sys = MFrequency::LSR;
    } else if (system == "LSRK") {
	sys = MFrequency::LSRK;
    } else if (system == "BARY") {
	sys = MFrequency::BARY;
    } else if (system == "GEO") {
	sys = MFrequency::GEO;
    } else if (system == "TOPO") {
	sys = MFrequency::TOPO;
    } else if (system == "GALACTO") {
	sys = MFrequency::GALACTO;
    } else {
	return 0;
    }
    
    if (!subrec.isDefined("crval")) {
	return 0;
    }
    Vector<Double> crval;
    subrec.get("crval", crval);

    if (!subrec.isDefined("crpix")) {
	return 0;
    }
    Vector<Double> crpix;
    subrec.get("crpix", crpix);

    if (!subrec.isDefined("cdelt")) {
	return 0;
    }
    Vector<Double> cdelt;
    subrec.get("cdelt", cdelt);

    if (!subrec.isDefined("pc")) {
	return 0;
    }
    Matrix<Double> pc;
    subrec.get("pc", pc);

    
    if (!subrec.isDefined("axes")) {
	return 0;
    }
    Vector<String> axes;
    subrec.get("axes", axes);
    
    if (!subrec.isDefined("units")) {
	return 0;
    }
    Vector<String> units;
    subrec.get("units", units);

    if (!subrec.isDefined("restfreq")) {
	return 0;
    }
    Double restfreq;
    subrec.get("restfreq", restfreq);

    SpectralCoordinate *retval = 
	new SpectralCoordinate(sys, 0, 1, 0, 0);
    AlwaysAssert(retval, AipsError);

    // We have to do the units first since they will change the
    // reference value and increment if we do them too late.
    retval->setWorldAxisUnits(units);
    retval->setWorldAxisNames(axes);
    retval-> setIncrement(cdelt);
    retval->setReferenceValue(crval);
    retval->setReferencePixel(crpix);
    retval->setRestFrequency(restfreq);

    if (subrec.isDefined("pixelcorrections")) {
	Vector<Double> pixrec;
	subrec.get("pixelcorrections", pixrec);
	Vector<Double> channs(pixrec.nelements());
	indgen(channs.ac());
	ScalarSampledFunctional<Double> c_in(channs), c_diff(pixrec);
	retval->channel_corrector_p = 
	    new Interpolate1D<Double,Double>(c_in, c_diff, True, True);
	retval->channel_corrector_p = 
	    new Interpolate1D<Double,Double>(c_diff, c_in, True, True);
	AlwaysAssert(retval->channel_corrector_p != 0 &&
		     retval->channel_corrector_rev_p != 0, AipsError);
	retval->channel_corrector_p->setMethod(
			       Interpolate1D<Double,Double>::linear);
	retval->channel_corrector_rev_p->setMethod(
			       Interpolate1D<Double,Double>::linear);
    }
							  
    return retval;
}

Coordinate *SpectralCoordinate::clone() const
{
    return new SpectralCoordinate(*this);
}


void SpectralCoordinate::toFITS(RecordInterface &header, uInt whichAxis, 
		LogIO &logger, Bool oneRelative, 
                Bool preferVelocity,  Bool opticalVelDef) const
{
    const Double offset(1.0*Int(oneRelative == True));

    logger << LogOrigin("SpectralCoordinate", "toFITS", WHERE);

    // Verify that the required headers exist and are the right type
    AlwaysAssert(header.isDefined("ctype") && 
		 header.dataType("ctype") == TpArrayString &&
		 header.shape("ctype").nelements() == 1 &&
		 header.shape("ctype")(0) > whichAxis, AipsError);
    AlwaysAssert(header.isDefined("crval") && 
		 header.dataType("crval") == TpArrayDouble &&
		 header.shape("crval").nelements() == 1 &&
		 header.shape("crval")(0) > whichAxis, AipsError);
    AlwaysAssert(header.isDefined("crpix") && 
		 header.dataType("crpix") == TpArrayDouble &&
		 header.shape("crpix").nelements() == 1 &&
		 header.shape("crpix")(0) > whichAxis, AipsError);
    AlwaysAssert(header.isDefined("cdelt") && 
		 header.dataType("cdelt") == TpArrayDouble &&
		 header.shape("cdelt").nelements() == 1 &&
		 header.shape("cdelt")(0) > whichAxis, AipsError);

    Vector<String> ctype, cunit;
    Vector<Double> crval, cdelt, crpix;
    header.get("ctype", ctype);
    header.get("crval", crval);
    header.get("crpix", crpix);
    header.get("cdelt", cdelt);

    if (header.isDefined("cunit")) {
	AlwaysAssert(header.dataType("cunit") == TpArrayString &&
		     header.shape("cunit").nelements() == 1 &&
		     header.shape("cunit")(0) > whichAxis, AipsError);
	header.get("cunit", cunit);
    }

    // Wacky capitalizatoin to avoid running into other variables
    String Ctype;
    Double Crval, Cdelt, Crpix, Altrval, Altrpix;
    Int Velref;
    Bool HaveAlt;
    Double Restfreq = Quantity(restfreq_p, unit_p).getBaseValue(); // canonicalized
    Double RefFreq = Quantity(crval_p, unit_p).getBaseValue();
    Double FreqInc = Quantity(cdelt_p, unit_p).getBaseValue();
    MDoppler::Types VelPreference = opticalVelDef ? MDoppler::OPTICAL :
	MDoppler::RADIO;
    AlwaysAssert(FITSSpectralUtil::toFITSHeader(Ctype,
						Crval,
						Cdelt,
						Crpix,
						HaveAlt,
						Altrval,
						Altrpix,
						Velref,
						Restfreq,
						logger,
						RefFreq,
						crpix_p + offset,
						FreqInc,
						type_p,
						preferVelocity,
						VelPreference), AipsError);


    ctype(whichAxis) = Ctype;
    crval(whichAxis) = Crval;
    crpix(whichAxis) = Crpix;
    cdelt(whichAxis) = Cdelt;
    if (cunit.nelements() > 0) {
	if (Ctype.contains("FELO")) {
	    cunit(whichAxis) = "M/S";
	} else if (Ctype.contains("FREQ")) {
	    cunit(whichAxis) = "HZ";
	} else {
	    AlwaysAssert(0, AipsError); // NOTREACHED
	}
    }

    if (Restfreq > 0) {
	header.define("restfreq", Restfreq);
	header.setComment("restfreq", "Rest Frequency (Hz)");
    }
    if (HaveAlt) {
	header.define("altrval", Altrval);
	header.setComment("altrval", "Alternate frequency reference value");
	header.define("altrpix", Altrpix);
	header.setComment("altrpix", "Alternate frequency reference pixel");
	header.define("velref", Velref);
	header.setComment("velref", "1 LSR, 2 HEL, 3 OBS, +256 Radio");
    }

    // OK, put the primary header information back
    header.define("ctype", ctype);
    header.define("crval", crval);
    header.define("crpix", crpix);
    header.define("cdelt", cdelt);
    if (cunit.nelements() > 0) {
	header.define("cunit", cunit);
    }
}

Bool SpectralCoordinate::fromFITS(SpectralCoordinate &out, String &error,
				  const RecordInterface &header, 
				  uInt whichAxis, LogIO &logger,
				  Bool oneRelative)
{
    Int spectralAxis;
    Double referenceChannel, referenceFrequency, deltaFrequency;
    Vector<Double> frequencies;
    MFrequency::Types refFrame;
    MDoppler::Types velocityPreference;
    Double restFrequency;
    
    Bool ok = FITSSpectralUtil::fromFITSHeader(spectralAxis,
					       referenceChannel,
					       referenceFrequency,
					       deltaFrequency,
					       frequencies,
					       refFrame,
					       velocityPreference,
					       restFrequency,
					       logger,
					       header,
					       'c',
					       oneRelative);
    if (ok && spectralAxis == whichAxis) {
	SpectralCoordinate tmp(refFrame, referenceFrequency, deltaFrequency, 
			       referenceChannel, restFrequency);
	out = tmp;
    } else if (ok && spectralAxis != whichAxis) {
	logger << LogOrigin("SpectralCoordinate", "fromFITS") << LogIO::SEVERE <<
	    "Disgreement about where the spectral axis is. " << spectralAxis << " vs. " 
	       << whichAxis << LogIO::POST;
	ok = False;
    }
					       
    return ok;
}


void SpectralCoordinate::checkFormat(Coordinate::formatType& format,
                                     const Bool absolute) const
//
//
{     
// Scientific or fixed formats only are allowed.
// Absolute or offset is irrelevant

   if (format == Coordinate::DEFAULT) {
      format = Coordinate::SCIENTIFIC;
   } else {
      if (format != Coordinate::SCIENTIFIC && 
          format != Coordinate::FIXED) format = Coordinate::SCIENTIFIC;
   }
}


void SpectralCoordinate::getPrecision(Int& precision,
                                      Coordinate::formatType& format, 
                                      const Bool absolute,
                                      const Int defPrecScientific,
                                      const Int defPrecFixed,
                                      const Int defPrecTime) const
 
{
// Scientific or fixed formats only are allowed.
// Absolute or offset is irrelevant

   checkFormat (format, absolute);

   if (format == Coordinate::SCIENTIFIC) {
      if (defPrecScientific >= 0) {
         precision = defPrecScientific;
      } else {
         precision = 6;
      }
   } else if (format == Coordinate::FIXED) {
      if (defPrecFixed >= 0) {
         precision = defPrecFixed;
      } else {
         precision = 6;
      }
   }
}
 
String SpectralCoordinate::format(String& units,
                                const Coordinate::formatType format,
                                const Double worldValue,
                                const uInt worldAxis,
                                const Bool absolute,
                                const Int precision) const
//
// Scientific or fixed formats only are allowed.
// Absolute or offset is irrelevant
//
{
   AlwaysAssert(worldAxis < nWorldAxes(), AipsError);
 
// Check format 
 
   Coordinate::formatType form = format;
   checkFormat (form, absolute);


// Set default precision

   Int prec = precision;
   if (prec < 0) getPrecision(prec, form, absolute, -1, -1, -1);

                                    
// Format and get units
 
   ostrstream oss;
   if (form == Coordinate::SCIENTIFIC) {
      oss.setf(ios::scientific, ios::floatfield);
      oss.precision(prec);
      oss << worldValue;
   } else if (form == Coordinate::FIXED) {
      oss.setf(ios::fixed, ios::floatfield);
      oss.precision(prec);
      oss << worldValue;
   }
   units = worldAxisUnits()(worldAxis);
 
   return String(oss);
}

