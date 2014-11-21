//# tFITSSpectralUtil.cc: Test program for FITSSpectralUtil
//# Copyright (C) 2002,2003
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

//# Includes
#include <casacore/fits/FITS/FITSSpectralUtil.h>

#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/fits/FITS/FITSKeywordUtil.h>
#include <casacore/casa/Logging.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main()
{
    try {
	// to from FITSHeader
	String ctype, cunit;
	Double crval, cdelt, crpix, altrval, altrpix;
	Int velref;
	Bool haveAlt;
	String specsys;
	Double restFreq = 1420.4058e6;
	Double refFreq = 1400.0e6;
	Double freqInc = 2.5e6;
	Double refPix = 512.0;
	MFrequency::Types refFrame = MFrequency::GALACTO;
	MDoppler::Types velPref = MDoppler::RADIO;
	LogIO logger;
	AlwaysAssertExit(FITSSpectralUtil::
			 toFITSHeader(ctype, crval, cdelt,
				      crpix, cunit, haveAlt, altrval,
				      altrpix, velref, restFreq, 
				      specsys, logger,
				      refFreq, refPix, freqInc,
				      refFrame, True, velPref));
	// Actually construct the header record
	Record header;
	if (restFreq > 0) {
	    header.define("restfrq",restFreq);
	    header.setComment("restfrq","Rest Frequency (Hz)");
	}
	if (haveAlt) {
	    header.define("altrval",altrval);
	    header.setComment("altrval","Alternate frequency reference value");
	    header.define("altrpix", altrpix);
	    header.setComment("altrpix","Alternate frequency reference pixel");
	    header.define("velref", velref);
	    header.setComment("velref", "1 LSR, 2 HEL, 3 OBS, +256 Radio");
	    // the following agree with the current usage in FITSSpectralUtil
	    // which in turn follows from Greisen, Paper III.  On the other
	    // hand, that usage as applied here, to VELREF, is unlikely to
	    // be understood by other FITS readers.  Still, its better than
	    // doing nothing for these rest frames until the convention in
	    // Paper III or its successor is formally adopted.
	    FITSKeywordUtil::
		addComment(header,
		   "casacore non-standard usage: 4 LSD, 5 GEO, 6 SOU, 7 GAL");
	}
	header.define("specsys",specsys);
	
	// dummy primary header axes
	Vector<String> ctypeVec(2), cunitVec(2);
	Vector<Double> crvalVec(2), crpixVec(2), cdeltVec(2);
	
	ctypeVec(0) = ctype;
	crvalVec(0) = crval;
	crpixVec(0) = crpix;
	cdeltVec(0) = cdelt;
	
	ctypeVec(1) = "STOKES";
	crvalVec(1) = 1;
	crpixVec(1) = 1;
	cdeltVec(1) = 1;
	
	cunitVec(0) = "Hz";
	cunitVec(1) = "";
	
	// OK, put the primary header information back
	header.define("ctype", ctypeVec);
	header.define("crval", crvalVec);
	header.define("crpix", crpixVec);
	header.define("cdelt", cdeltVec);
	header.define("cunit", cunitVec);
	
	// and the other direction
	Int whichAxis;
	Double refPixOut, refFreqOut, freqIncOut, restFreqOut;
	Vector<Double> freqs;
	MFrequency::Types refFrameOut = MFrequency::GALACTO;
	MDoppler::Types velPrefOut = MDoppler::RADIO;
	AlwaysAssertExit(FITSSpectralUtil::fromFITSHeader(whichAxis,
							  refPixOut,
							  refFreqOut,
							  freqIncOut,
							  freqs,
							  refFrameOut,
							  velPrefOut,
							  restFreqOut,
							  logger,
							  header,
							  'c',
							  False));
	AlwaysAssertExit(whichAxis==0);
	// note: the following is only true when onRelative==False in 
	// fromFITSHeader
	AlwaysAssertExit(near(refPix,refPixOut));
	AlwaysAssertExit(near(refFreq,refFreqOut));
	AlwaysAssertExit(near(freqInc,freqIncOut));
	AlwaysAssertExit(refFrame==refFrameOut);
	AlwaysAssertExit(velPref==velPrefOut);
	AlwaysAssertExit(near(restFreq,restFreqOut));
	
	// tags from/to frames
	for (uInt i=0;i<MFrequency::N_Types; i++) {
	    String tag;
	    Int velref;
	    MFrequency::Types inFrame, outFrame;
	    inFrame = MFrequency::Types(i);
	    if (!FITSSpectralUtil::tagFromFrame(tag, velref, inFrame)) {
		// expect this to fail for LGROUP and larger
		if (i < MFrequency::LGROUP) {
		    throw(AipsError("testFITSSpectralUtil: unexpected failure in tagFromFrame"));
		}
	    }
	    if (!FITSSpectralUtil::frameFromTag(outFrame, tag, velref)) {
		// expect this to fail for LGROUP and larger
		if (i < MFrequency::LGROUP) {
		    throw(AipsError("testFITSSpectralUtil: unexpected failure in frameFromTag"));
		}
	    }
	    if (inFrame != outFrame) {
		// expect this to fail for LGROUP and larger
		if (i < MFrequency::LGROUP) {
		    throw(AipsError("testFITSSpectralUtil: frameFromTag did not return original frame"));
		}
	    }
	    if (!FITSSpectralUtil::specsysFromFrame(tag, inFrame)) {
	      throw(AipsError("testFITSSpectralUtil: unexpected failure in specsysFromFrame"));
	    }

	}
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    }
    try { // same as above, however with wavelength spectral axis
	// to from FITSHeader
      String ctype, cunit, specsys;
	Double crval, cdelt, crpix, altrval, altrpix;
	Int velref;
	Bool haveAlt;
	Double restFreq = 1420.4058e6;
	Double refFreq = 1400.0e6;
	Double freqInc = 2.5e6;
	Double refPix = 512.0;
	MFrequency::Types refFrame = MFrequency::GALACTO;
	MDoppler::Types velPref = MDoppler::RADIO;
	LogIO logger;
	AlwaysAssertExit(FITSSpectralUtil::
			 toFITSHeader(ctype, crval, cdelt,
				      crpix, cunit, haveAlt, altrval,
				      altrpix, velref, restFreq, 
				      specsys, logger,
				      refFreq, refPix, freqInc,
				      refFrame, False, velPref,
				      True)); // wavelength preferred
	// Actually construct the header record
	Record header;
	if (restFreq > 0) {
	    header.define("restfreq",restFreq);
	    header.setComment("restfreq","Rest Frequency (Hz)");
	}
	if (haveAlt) {
	    header.define("altrval",altrval);
	    header.setComment("altrval","Alternate frequency reference value");
	    header.define("altrpix", altrpix);
	    header.setComment("altrpix","Alternate frequency reference pixel");
	    header.define("velref", velref);
	    header.setComment("velref", "1 LSR, 2 HEL, 3 OBS, +256 Radio");
	    // the following agree with the current usage in FITSSpectralUtil
	    // which in turn follows from Greisen, Paper III.  On the other
	    // hand, that usage as applied here, to VELREF, is unlikely to
	    // be understood by other FITS readers.  Still, its better than
	    // doing nothing for these rest frames until the convention in
	    // Paper III or its successor is formally adopted.
	    FITSKeywordUtil::
		addComment(header,
		   "casacore non-standard usage: 4 LSD, 5 GEO, 6 SOU, 7 GAL");
	}
	header.define("specsys",specsys);

	// dummy primary header axes
	Vector<String> ctypeVec(2), cunitVec(2);
	Vector<Double> crvalVec(2), crpixVec(2), cdeltVec(2);
	
	ctypeVec(0) = ctype;
	crvalVec(0) = crval;
	crpixVec(0) = crpix;
	cdeltVec(0) = cdelt;
	cunitVec(0) = cunit;
	
	ctypeVec(1) = "STOKES";
	crvalVec(1) = 1;
	crpixVec(1) = 1;
	cdeltVec(1) = 1;
	
	cunitVec(1) = "";
	
	// OK, put the primary header information back
	header.define("ctype", ctypeVec);
	header.define("crval", crvalVec);
	header.define("crpix", crpixVec);
	header.define("cdelt", cdeltVec);
	header.define("cunit", cunitVec);
	
	// and the other direction
	Int whichAxis;
	Double refPixOut, refFreqOut, freqIncOut, restFreqOut;
	Vector<Double> freqs;
	MFrequency::Types refFrameOut = MFrequency::GALACTO;
	MDoppler::Types velPrefOut = MDoppler::RADIO;
	AlwaysAssertExit(FITSSpectralUtil::fromFITSHeader(whichAxis,
							  refPixOut,
							  refFreqOut,
							  freqIncOut,
							  freqs,
							  refFrameOut,
							  velPrefOut,
							  restFreqOut,
							  logger,
							  header,
							  'c',
							  False));
	AlwaysAssertExit(whichAxis==0);
	// note: the following is only true when onRelative==False in 
	// fromFITSHeader
	AlwaysAssertExit(near(refPix,refPixOut));
	AlwaysAssertExit(near(refFreq,refFreqOut));
	AlwaysAssertExit(near(freqInc,freqIncOut, 3E-6));
	AlwaysAssertExit(refFrame==refFrameOut);
	AlwaysAssertExit(near(restFreq,restFreqOut));
	
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    }
    try { // same as above, however with air wavelength spectral axis
    	// to from FITSHeader
    	String ctype, cunit, specsys;
    	Double crval, cdelt, crpix, altrval, altrpix;
    	Int velref;
    	Bool haveAlt;
    	Double restFreq = 1420.4058e6;
    	Double refFreq = 1400.0e6;
    	Double freqInc = 2.5e6;
    	Double refPix = 512.0;
    	MFrequency::Types refFrame = MFrequency::GALACTO;
    	MDoppler::Types velPref = MDoppler::RADIO;
    	LogIO logger;
    	AlwaysAssertExit(FITSSpectralUtil::
    			toFITSHeader(ctype, crval, cdelt,
    					crpix, cunit, haveAlt, altrval,
    					altrpix, velref, restFreq,
    					specsys, logger,
    					refFreq, refPix, freqInc,
    					refFrame, False, velPref,
    					True, True)); // air wavelength preferred
    	// Actually construct the header record
    	Record header;
    	if (restFreq > 0) {
    		header.define("restfreq",restFreq);
    		header.setComment("restfreq","Rest Frequency (Hz)");
    	}
    	if (haveAlt) {
    		header.define("altrval",altrval);
    		header.setComment("altrval","Alternate frequency reference value");
    		header.define("altrpix", altrpix);
    		header.setComment("altrpix","Alternate frequency reference pixel");
    		header.define("velref", velref);
    		header.setComment("velref", "1 LSR, 2 HEL, 3 OBS, +256 Radio");
    		// the following agree with the current usage in FITSSpectralUtil
    		// which in turn follows from Greisen, Paper III.  On the other
    		// hand, that usage as applied here, to VELREF, is unlikely to
    		// be understood by other FITS readers.  Still, its better than
    		// doing nothing for these rest frames until the convention in
    		// Paper III or its successor is formally adopted.
    		FITSKeywordUtil::
    		addComment(header,
    				"casacore non-standard usage: 4 LSD, 5 GEO, 6 SOU, 7 GAL");
    	}
    	header.define("specsys",specsys);

    	// dummy primary header axes
    	Vector<String> ctypeVec(2), cunitVec(2);
    	Vector<Double> crvalVec(2), crpixVec(2), cdeltVec(2);

    	ctypeVec(0) = ctype;
    	crvalVec(0) = crval;
    	crpixVec(0) = crpix;
    	cdeltVec(0) = cdelt;
    	cunitVec(0) = cunit;

    	ctypeVec(1) = "STOKES";
    	crvalVec(1) = 1;
    	crpixVec(1) = 1;
    	cdeltVec(1) = 1;

    	cunitVec(1) = "";

    	// OK, put the primary header information back
    	header.define("ctype", ctypeVec);
    	header.define("crval", crvalVec);
    	header.define("crpix", crpixVec);
    	header.define("cdelt", cdeltVec);
    	header.define("cunit", cunitVec);

    	// and the other direction
    	Int whichAxis;
    	Double refPixOut, refFreqOut, freqIncOut, restFreqOut;
    	Vector<Double> freqs;
    	MFrequency::Types refFrameOut = MFrequency::GALACTO;
    	MDoppler::Types velPrefOut = MDoppler::RADIO;
    	AlwaysAssertExit(FITSSpectralUtil::fromFITSHeader(whichAxis,
    			refPixOut,
    			refFreqOut,
    			freqIncOut,
    			freqs,
    			refFrameOut,
    			velPrefOut,
    			restFreqOut,
    			logger,
    			header,
    			'c',
    			False));
    	AlwaysAssertExit(whichAxis==0);
    	// note: the following is only true when onRelative==False in
    	// fromFITSHeader
    	AlwaysAssertExit(near(refPix,refPixOut));
    	AlwaysAssertExit(near(refFreq,refFreqOut, 1E-11));
    	AlwaysAssertExit(near(freqInc,freqIncOut, 3E-6));
    	AlwaysAssertExit(refFrame==refFrameOut);
    	AlwaysAssertExit(near(restFreq,restFreqOut));

    } catch (AipsError x) {
    	cout << "Caught an exception: " << x.getMesg() << endl;
    	return 1;
    }
    return 0;
}

