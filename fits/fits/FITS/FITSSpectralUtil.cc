//# FitsKeywordUtil: this defines FitsKeywordUtil
//# Copyright (C) 2002
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

#include <fits/FITS/FITSSpectralUtil.h>

#include <casa/Arrays/Vector.h>
#include <casa/Containers/RecordInterface.h>
#include <casa/Logging/LogIO.h>
#include <casa/Logging/LogOrigin.h>
#include <casa/BasicSL/Constants.h>
#include <casa/Utilities/Assert.h>
#include <casa/BasicSL/String.h>

namespace casa { //# NAMESPACE CASA - BEGIN

Bool FITSSpectralUtil::fromFITSHeader(Int &spectralAxis,
				      Double &referenceChannel,
				      Double &referenceFrequency,
				      Double &deltaFrequency,
				      Vector<Double> &frequencies,
				      MFrequency::Types &refFrame,
				      MDoppler::Types &velocityPreference,
				      Double &restFrequency,
				      LogIO &logger,
				      const RecordInterface &header,
				      char prefix,
				      Bool oneRelative)
{
    Bool retval = True;

    // Start out invalid
    spectralAxis = -1;
    frequencies.resize(0);
    referenceChannel = 0;
    referenceFrequency = -1.0;
    deltaFrequency = 0.0;
    restFrequency = -1.0;

    const Double offset(oneRelative == True ? 1.0 : 0.0);
    logger << LogOrigin("FITSUtil", "fromFITSHeader", WHERE);

    String n_ctype = String(prefix) + "type";
    String n_crval = String(prefix) + "rval";
    String n_crpix = String(prefix) + "rpix";
    String n_cdelt = String(prefix) + "delt";
    Int ndim;
    
    // Verify that the required headers exist and are the right type
    if (! (header.isDefined(n_ctype) && 
	   header.dataType(n_ctype) == TpArrayString &&
	   header.shape(n_ctype).nelements() == 1 &&
	   header.isDefined(n_crval) && 
	   header.dataType(n_crval) == TpArrayDouble &&
	   header.shape(n_crval).nelements() == 1 &&
	   header.isDefined(n_crpix) && 
	   header.dataType(n_crpix) == TpArrayDouble &&
	   header.shape(n_crpix).nelements() == 1 &&
	   header.isDefined(n_cdelt) && 
	   header.dataType(n_cdelt) == TpArrayDouble &&
	   header.shape(n_cdelt).nelements() == 1 &&
	   header.shape(n_cdelt)(0)) ) {
	logger << LogIO::SEVERE << "One of " << n_ctype << "," << n_crval << 
	  "," << n_crpix << "or " << n_cdelt <<
	  " does not exist or is the wrong type." << LogIO::POST;
	return False;
    }
    Bool has_altrval=header.isDefined(String("altrval")) 
      && (header.dataType("altrval") == TpDouble || header.dataType("altrval") == TpFloat);
    Bool has_altrpix=header.isDefined(String("altrpix")) 
      && (header.dataType("altrpix") == TpDouble ||header.dataType("altrpix") == TpFloat) ;



    Vector<String> ctype;
    Vector<Double> crval, crpix, cdelt;
    header.get(n_ctype, ctype);
    header.get(n_crval, crval);
    header.get(n_crpix, crpix);
    header.get(n_cdelt, cdelt);

// naxis might not be consistent with the length of the CTYPEs
// we need both. use naxis preferentially

    Vector<Int> naxis;
    if (header.isDefined("naxis")) {
       header.get("naxis", naxis);
       ndim = naxis.nelements();
    } else {
       ndim = ctype.nelements();
    }

    // Find the spectral axis, if any.

    for (Int i=0; i<ndim; i++) {
	if (ctype(i).contains("FELO") || ctype(i).contains("FREQ") ||
	    ctype(i).contains("VELO")) {
	    spectralAxis = i;
	    break;
	}
    }
    if (spectralAxis < 0) {
	return False;
    }

    Int velref = 3; // Default is optical + topocentric ("OBS")
    if (header.isDefined("velref")) {
	if (header.dataType("velref") != TpInt) {
	    logger << LogIO::SEVERE << "Illegal type for VELREF"
		", assuming optical, topocentric" << LogIO::POST;
	} else {
	    header.get("velref", velref);
	}
    } else {
	if (ctype(spectralAxis).contains("VELO")) {
	    velref = 259; // radio + OBS
	}
    }
    
    // Try to work out OPTICAL/RADIO/. Default to Optical
    String type(ctype(spectralAxis).before(4));
    velocityPreference = MDoppler::OPTICAL;
    if (velref > 256) {
	velocityPreference = MDoppler::RADIO;
    }

    if (header.isDefined("restfreq")) {
	if (header.dataType("restfreq") != TpDouble && 
	    header.dataType("restfreq") != TpFloat) {
	    logger << LogIO::SEVERE << "Illegal type for RESTFREQ" <<
		", assuming 0.0 - velocity conversions will be impossible" 
		   << LogIO::POST;
	} else {
	    header.get("restfreq", restFrequency);
	}
    }

    // convert the velocity frame tag in ctype  to a reference frame
    String spectralAxisQualifier;
    if (ctype(spectralAxis).length() <= 5) {
      spectralAxisQualifier = "";
    } else {
      spectralAxisQualifier = ctype(spectralAxis).after(4);
    };

    if (!FITSSpectralUtil::frameFromTag(refFrame, 
					spectralAxisQualifier, 
					velref)) {
	if (spectralAxisQualifier == "") {
	    if ((velref%256) >= 0) {
		// no tag and velref is unrecognized
		logger << LogIO::SEVERE << "Illegal value for VELREF("
		       << velref << 
		    ") assuming topocentric" << LogIO::POST;
	    }
	} else {
	    // unrecognized tag
	    logger << LogIO::SEVERE << "Unknown spectral reference frame " 
		   << spectralAxisQualifier << 
		". Assuming topocentric." << LogIO::POST;
	}
    }

    referenceChannel = crpix(spectralAxis) - offset;

    // ALTRVAL and ALTRPIX are being used in "FREQ" axis mode

    // Get NAXIS if we have it

// This function returns a vector of frequencies as well as the reference 
// value/pixel, increment etc.  However, this vector is linear in 
// frequency, so offers no more information than the reference value/increment

// For random group, NAXIS1=0 and then CTYPE1,CRVAL1,CDELT1,CROTA1 are
// omitted. spectralAxis is determined from ctype string array, so in order
// to get corresponding naxis, it needs to be shift by 1. 
    Int nChan = 1;
    if (naxis.nelements()>0) {
      Int naxisoffset=0;
      if (naxis(0)==0) naxisoffset=1;
	nChan = naxis(spectralAxis+naxisoffset);
	AlwaysAssert(nChan >= 1, AipsError);
    }

    const Double delt = cdelt(spectralAxis);
    const Double rval = crval(spectralAxis);
    const Double rpix = crpix(spectralAxis) - offset;

    if (ctype(spectralAxis).contains("FREQ")) {
     
      referenceFrequency = rval;
      //HAS ALTRVAL
      if(has_altrval && (restFrequency >= 0.0)){
	Double velo;
	header.get("altrval",velo);
	MDoppler ledop(Quantity(velo, "m/s"), 
		       velocityPreference);
	referenceFrequency=MFrequency::fromDoppler(ledop, restFrequency).getValue().getValue(); 
	
      }

      //HAS ALTRPIX
      if(has_altrpix){
	header.get("altrpix", referenceChannel);
      }
      // include one-based offset
      // NB: UVFITS refChan is generally one-based
      referenceChannel-=offset;

      deltaFrequency = delt;
      frequencies.resize(nChan);
      for (Int i=0; i<nChan; i++) {
	frequencies(i) = 
	  referenceFrequency + (Double(i)-referenceChannel)*delt;
      }
      
    } else if (ctype(spectralAxis).contains("FELO")) {
      if (restFrequency < 0) {
	logger << LogIO::SEVERE << "FELO axis does not have rest frequency "
	  "information (RESTFREQ)" << LogIO::POST;
	return False;
      } else {
	// Have RESTFREQ
	referenceChannel = rpix;
	switch(velocityPreference) {
	case MDoppler::OPTICAL:
	  {
	    referenceFrequency = restFrequency / (1.0 + rval/C::c);
	    deltaFrequency =   -delt*referenceFrequency / (
							   ( (C::c + rval) ) );
	  }
	  break;
	case MDoppler::RADIO:
	  {
	    logger << LogIO::SEVERE << "FELO/RADIO is illegal" <<
	      LogIO::POST;
	    return False;
	  }
	  break;
	default:
	  {
	    AlwaysAssert(0, AipsError); // NOTREACHED
	  }
	}
	frequencies.resize(nChan);
	for (Int i=0; i<nChan; i++) {
	  frequencies(i) = referenceFrequency + 
	    (Double(i)-referenceChannel) * deltaFrequency;
	}
      }
    } else if (ctype(spectralAxis).contains("VELO")) {
      if (restFrequency < 0) {
	logger << LogIO::SEVERE << "VELO axis does not have rest frequency "
	  "information (RESTFREQ)" << LogIO::POST;
	return False;
      } else {
	    // Have RESTFREQ
	referenceChannel = rpix;
	switch(velocityPreference) {
	case MDoppler::RADIO:
	  {
	    referenceFrequency = -rval/C::c*restFrequency + 
	      restFrequency;
	    deltaFrequency =  
	      -delt*referenceFrequency / (C::c - rval);
	  }
	  break;
	case MDoppler::OPTICAL:
		{
		  logger << LogIO::SEVERE << 
		    "VELO/OPTICAL is not implemented" <<LogIO::POST;
		  return False;
		}
		break;
	default:
	  {
	    AlwaysAssert(0, AipsError); // NOTREACHED
	  }
	}
	frequencies.resize(nChan);
	for (Int i=0; i<nChan; i++) {
	  frequencies(i) = referenceFrequency + 
	    (Double(i)-referenceChannel) * deltaFrequency;
	}
      }
    } else {
      AlwaysAssert(0, AipsError); // NOTREACHED
    }

    return retval;
}

Bool FITSSpectralUtil::toFITSHeader(String &ctype, 
				    Double &crval, 
				    Double &cdelt,
				    Double &crpix, 
				    Bool &haveAlt, 
				    Double &altrval,
				    Double &altrpix,
				    Int &velref,
				    Double &restfreq,
				    LogIO &logger,
				    Double refFrequency,
				    Double refChannel,
				    Double freqIncrement,
				    MFrequency::Types referenceFrame,
				    Bool preferVelocity,
				    MDoppler::Types velocityPreference)
{
    // Dummy defaults
    ctype = "";
    crval = cdelt = crpix = 0.0;
    haveAlt = False;
    altrval = altrpix = 0.0;
    velref = 0;

    logger << LogOrigin("FITSUtil", "toFITSHeader", WHERE);

    // Calculate the velocity related things first
    
    String ctypetag = "";
    if (restfreq > 0.0) {
	haveAlt = True;
	if (!FITSSpectralUtil::tagFromFrame(ctypetag, velref, referenceFrame)) {
	    logger << LogIO::SEVERE << "Cannot turn spectral type# " << 
		Int(referenceFrame) <<	
		" into a FITS spectral frame. Using " << ctypetag <<
		LogIO::POST;
	}
	switch (velocityPreference) {
	case MDoppler::OPTICAL: break; // NOTHING
	case MDoppler::RADIO: velref += 256; break;
	default:
	    {
		logger << LogIO::SEVERE << "Can only handle OPTICAL and RADIO "
		    "velocities. Using OPTICAL" << LogIO::POST;
	    }
	}
	
    }

    // Calculate velocity quantities
    Double refVelocity(0.0), velocityIncrement(0.0);
    if (haveAlt) {
	if (velref < 256) {
	    // OPTICAL
	    refVelocity = -C::c * (1.0 - restfreq / refFrequency);
	    velocityIncrement = -freqIncrement*(C::c + refVelocity)/refFrequency;
	} else {
	    // RADIO
	    refVelocity = -C::c * (refFrequency/restfreq - 1.0);
	    velocityIncrement = -freqIncrement*(C::c - refVelocity)/refFrequency;
	}
    }

    if (!haveAlt || !preferVelocity) {
	// FREQ is primary
	ctype = String("FREQ") + ctypetag;
	crval = refFrequency;
	cdelt = freqIncrement;
	crpix = refChannel;
	if (haveAlt) {
	    altrval = refVelocity;
	    altrpix = crpix;
	}
    } else {
	// Velocity of some type is primary
	if (velref < 256) {
	    // Optical
	    ctype = String("FELO") + ctypetag;
	} else {
	    // Radio
	    ctype = String("VELO") + ctypetag;
	}
	crval = refVelocity;
	cdelt = velocityIncrement;
	crpix = refChannel;
	// Always have ALT* because we fundamentally work in terms of
	// frequencies.
	altrval = refFrequency;
	altrpix = crpix;
    }

    return True;
}

Bool FITSSpectralUtil::frameFromTag(MFrequency::Types &refFrame,
				    const String& tag,
				    Int velref)
{
    String theTag;
    for (uInt i=0; i<tag.length(); i++) {
	if (tag[i] != '-' && tag[i] != ' ') {
	    theTag += tag[i];
	}
    }
    // Try to work out LSR/OBS/HEL/...
    Bool result = True;
    refFrame = MFrequency::TOPO; // The default
    if (theTag == "LSR" || theTag == "LSRK") {
	// the tag "LSRK" was generate by tagFromFrame until June of 2000
	// it is supported here in case any FITS files with that tag survive.
	// Greisen, Paper III indicates that "LSR" should be associated with
	// LSRK.  Prior to June of 2000, this code associated it with LSR
	// (now known as LSRD).
	// No attempt is made to distinquish that difference by this code
	// since there doesn't seem to be any way to do so.
	refFrame = MFrequency::LSRK;
    } else if (theTag == "HEL") {
	refFrame = MFrequency::BARY;
    } else if (theTag == "OBS") {
	refFrame = MFrequency::TOPO;
    } else if (theTag == "LSD") {
	// "LSD" is the tag described in Greisen, Paper III and it is written
	// by tagFromFrame for lack of anything better.  As described above,
	// prior to June of 2000, LSRD was associated with FITS "LSR", apparently
	// in error.
	refFrame = MFrequency::LSRD;
    } else if (theTag == "GEO") {
	refFrame = MFrequency::GEO;
    } else if (theTag == "SOU" || theTag == "REST") {
	// "SOU" is the tag described in Greisen, Paper III and it is written
	// by tagFromFrame for lack of anything better.  "REST" was used
	// prior to June of 2000.
	refFrame = MFrequency::REST;
    } else if (theTag == "GAL") {
	refFrame = MFrequency::GALACTO;
    } else if (theTag == "") {
	// See if we can get it from the velref value
	// Prior to June of 2000, case 1 was associated with LSR, apparently
	// in error. Case 4 is now LSRD (formerly LSR).
	if (velref >= 0) {
	    switch(velref % 256) {
	    case 1:
		refFrame = MFrequency::LSRK;
		break;
	    case 2:
		refFrame = MFrequency::BARY;
		break;
	    case 3:
		refFrame = MFrequency::TOPO;
		break;
	    case 4:
		refFrame = MFrequency::LSRD;
		break;
	    case 5:
		refFrame = MFrequency::GEO;
		break;
	    case 6:
		refFrame = MFrequency::REST;
		break;
	    case 7:
		refFrame = MFrequency::GALACTO;
		break;
	    default:
		result = False;
		// empty tag, illegal velref
	    }
	} else {
	    result = False;
	    // empty tag, no velref
	}
    } else {
	result = False;
    }
    return result;
}

Bool FITSSpectralUtil::tagFromFrame(String &tag,
				    Int &velref,
				    MFrequency::Types refFrame)
{
    Bool result = True;
    switch (refFrame) {
    case MFrequency::LSRK:
	tag = "-LSR";
	velref = 1;
	break;
    case MFrequency::BARY:
	tag = "-HEL";
	velref = 2;
	break;
    case MFrequency::TOPO:
	tag = "-OBS";
	velref = 3;
	break;
    case MFrequency::LSRD:
	tag = "-LSD";
	velref = 4;
	break;
    case MFrequency::GEO:
	tag = "-GEO";
	velref = 5;
	break;
    case MFrequency::REST:
	tag = "-SOU";
	velref = 6;
	break;
    case MFrequency::GALACTO:
	tag = "-GAL";
	velref = 7;
	break;
    default:
	tag = "-OBS";
	velref = 3;
	result = False;
    }
    return result;
}

} //# NAMESPACE CASA - END

