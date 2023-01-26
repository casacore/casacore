//# FITSCoordinateUtil.cc: inter-convert CoordinateSystem and FITS headers
//# Copyright (C) 1997,1998,1999,2000,2001,2002,2003,2004
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


#include <casacore/coordinates/Coordinates/FITSCoordinateUtil.h>

#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/LinearCoordinate.h>
#include <casacore/coordinates/Coordinates/DirectionCoordinate.h>
#include <casacore/coordinates/Coordinates/SpectralCoordinate.h>
#include <casacore/coordinates/Coordinates/TabularCoordinate.h>
#include <casacore/coordinates/Coordinates/StokesCoordinate.h>
#include <casacore/coordinates/Coordinates/ObsInfo.h>

#include <casacore/coordinates/Coordinates/CoordinateUtil.h>

#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/Quanta/MVDirection.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/Quanta/UnitMap.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/fits/FITS/FITSDateUtil.h>
#include <casacore/measures/Measures/MDoppler.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MFrequency.h>

#include <casacore/casa/iostream.h>

#include <wcslib/wcs.h>
#include <wcslib/wcshdr.h>
#include <wcslib/wcsfix.h>
#include <wcslib/wcsmath.h>
#include <wcslib/fitshdr.h>
#include <wcslib/wcsconfig.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

    bool FITSCoordinateUtil::toFITSHeader(RecordInterface &header, 
					  IPosition &shape,
					  const CoordinateSystem& cSys,
					  bool oneRelative,
					  char prefix, bool writeWCS,
					  bool preferVelocity, 
					  bool opticalVelocity,
					  bool preferWavelength,
					  bool airWavelength) const
    {
	LogIO os(LogOrigin("FITSCoordinateUtil", "toFITSHeader", WHERE));


// Validation

	const int32_t n = cSys.nWorldAxes();
	String sprefix(prefix);
	if (header.isDefined(sprefix + "rval") ||
	    header.isDefined(sprefix + "rpix") ||
	    header.isDefined(sprefix + "delt") ||
	    header.isDefined(sprefix + "type") ||
	    header.isDefined(sprefix + "unit")) {
	    os << LogIO::SEVERE << "Already contains one or more of *rval, *rpix, "
		"*delt, *type, *unit";
	    return false;
	}

	double offset = 0.0;
	if (oneRelative) {
	    offset = 1.0;
	}

// Canonicalize units and find sky axes

	CoordinateSystem coordsys = cSys;

// Find the sky coordinate, if any

	int32_t skyCoord = coordsys.findCoordinate(Coordinate::DIRECTION);
	int32_t longAxis = -1, latAxis = -1;

// Find the spectral axis, if any

	int32_t specCoord = coordsys.findCoordinate(Coordinate::SPECTRAL);
	int32_t specAxis = -1;
    
// Find the Stokes axis if any. 

	int32_t stokesCoord = coordsys.findCoordinate(Coordinate::STOKES);
	int32_t stokesAxis = -1;

// If any axes have been removed from a coordinate, you find it out here.  

	int32_t i;
	for (i=0; i<n ; i++) {
	    int32_t c, a;
	    coordsys.findWorldAxis(c, a, i);
	    if (c == skyCoord) {
		if (a == 0) {
		    longAxis = i;
		} else if (a == 1) {
		    latAxis = i;
		}
	    } else if (c == specCoord) {
		specAxis = i;
	    } else if (c == stokesCoord) {
		stokesAxis = i;
	    }
	}

// If both axes for the DC are removed, set coordinate to missing.
// For other coordinates, they are handled via axes, not
// Coordinate number, so we don't need to do this. I think.

	if (longAxis==-1 && latAxis==-1) skyCoord = -1;

// If we have any tabular axes that aren't pure linear and are not optical velo or wavelength,
// report that the table will be lost.

	int32_t tabCoord = -1;
	while ((tabCoord = cSys.findCoordinate(Coordinate::TABULAR, tabCoord)) > 0) {
	    if (cSys.tabularCoordinate(tabCoord).pixelValues().nelements() > 0 
		&& !( ((preferVelocity && opticalVelocity) || preferWavelength) && tabCoord==specCoord)
		) {
		os << LogIO::NORMAL <<
		    "Note: Your coordinate system has one or more TABULAR axes for which\n"
		    "the lookup table will be lost in the conversion to FITS, and\n"
		    "will be replaced by averaged (i.e. linearized) axes." <<
		    LogIO::POST;
		break;
	    }
	}


// change the units to degrees for the sky axes

	Vector<String> units(coordsys.worldAxisUnits().copy());
	if (longAxis >= 0) units(longAxis) = "deg";
	if (latAxis >= 0) units(latAxis) = "deg";

// and to the canonical units for the spectral and stokes axis
	if (specAxis >= 0) units(specAxis) = "Hz";
	if (stokesAxis >= 0) units(stokesAxis) = "";
	coordsys.setWorldAxisUnits(units);

// If there is a spectral conversion layer, make it permanent here
	if(specCoord>=0){
	  SpectralCoordinate sCoord(coordsys.spectralCoordinate(specCoord));
	  MFrequency::Types nativeCtype = sCoord.frequencySystem(false); // native type
	  MFrequency::Types convCtype = sCoord.frequencySystem(true); // converted type

	  if (convCtype != nativeCtype) {
	    MEpoch convEpoch;
	    MPosition convPosition;
	    MDirection convDirection;
	    sCoord.getReferenceConversion(convCtype, convEpoch, convPosition, convDirection);
	    // modify the spec coordsys corresponding to the conversion layer
	    sCoord.transformFrequencySystem(convCtype, convEpoch, convPosition, convDirection);
	    // replace the spec-coordsys in coordsys by the new one
	    coordsys.replaceCoordinate(sCoord, specCoord);
	  }
	}

// Generate keywords.  If we find we have a DC with one of the
// axes removed, it will be linearized here.

	double longPole, latPole;
	Vector<double> crval, crpix, cdelt, pvi_ma;
	// crota is deprecated for FITS output
	//	Vector<double> crota;
	Vector<String> ctype, cunit;
	Matrix<double> pc;
	bool isNCP = false;
	if (!generateFITSKeywords (os, isNCP, longPole, latPole, crval, crpix, 
				   cdelt, 
				   // crota,  
				   pvi_ma, ctype, cunit, pc, 
				   coordsys, skyCoord, longAxis, latAxis, 
				   specAxis, stokesAxis, writeWCS,
				   offset, sprefix)) {
	    return false;
	}

// Special stokes handling

	if (stokesAxis >= 0) {
	    if (!toFITSHeaderStokes (crval, crpix, cdelt, os, coordsys,
				     stokesAxis, stokesCoord)) return false;
	}

// If there are more world than pixel axes, we will need to add
// degenerate pixel axes and modify the shape.

	if (int32_t(coordsys.nPixelAxes()) < n) {
	    IPosition shapetmp = shape; 
	    shape.resize(n);
	    Vector<double> crpixtmp = crpix.copy();
	    crpix.resize(n);
	    int32_t count = 0;
	    for (int32_t worldAxis=0; worldAxis<n; worldAxis++) {
		int32_t coordinate, axisInCoordinate;
		coordsys.findWorldAxis(coordinate, axisInCoordinate, worldAxis);
		int32_t pixelAxis = coordsys.pixelAxes(coordinate)(axisInCoordinate);
		if (pixelAxis >= 0) {
		    // We have a pixel axis
		    shape(worldAxis) = shapetmp(count);
		    crpix(worldAxis) = crpixtmp(count);
		    count++;
		} else {
		    // No corresponding pixel axis.
		    shape(worldAxis) = 1;
		    crpix(worldAxis) = 1.0;
		}
	    }
	}

// Try to work out the epoch/equinox
// Also LONGPOLE and LATPOLE here.

	if (skyCoord >= 0) {
	    const DirectionCoordinate& dCoord = coordsys.directionCoordinate(skyCoord);
	    MDirection::Types radecsys = dCoord.directionType();
	    double equinox = -1.0;
	    String radesys = "";
	    switch(radecsys) {
	    case MDirection::J2000:
		equinox = 2000.0;
		radesys = "FK5";
		break;
	    case MDirection::B1950:
		equinox = 1950.0;
		radesys = "FK4";
		break;
	    case MDirection::B1950_VLA:
		equinox = 1979.9;
		radesys = "FK4";
		break;
	    case MDirection::ICRS:
	      radesys = "ICRS";
	      break;
	    default:
		; // Nothing
	    }
	    if (equinox > 0) {
		if (writeWCS) {
		    header.define("equinox", equinox);
		} else {
		    header.define("epoch", equinox);
		}
	    }
	    if (radesys!=""){
	      header.define("radesys", radesys);
	    }
//
	    header.define("lonpole", longPole);
	    //const Projection& proj = dCoord.projection();
	    //if (!Projection::isZenithal(proj.type())) {    
	    //	header.define("latpole", latPole);       // Not relevant for zenithals
	    //}
	    header.define("latpole", latPole);     
	}

// Actually write the header

	if (writeWCS && int32_t(coordsys.nPixelAxes()) == n) {
	    header.define("pc", pc);
	} else if (writeWCS) {
	    os << LogIO::SEVERE << "writeWCS && nPixelAxes() != n. Requires "
		"development!!!"  << LogIO::POST;
	}

	header.define(sprefix + "type", ctype);
	header.define(sprefix + "rval", crval);
	header.define(sprefix + "delt", cdelt);
	//	header.define(sprefix + "rota", crota);
	header.define(sprefix + "rpix", crpix);
	header.define(sprefix + "unit", cunit);

	if (skyCoord >=0 && pvi_ma.nelements() > 0) {
	    if (!writeWCS) {
		for (uint32_t k=0; k<pvi_ma.nelements(); k++) {
		    if (!casacore::nearAbs(pvi_ma(k), 0.0)) {
			os << LogIO::WARN << 
			    "Projection parameters not all zero.Information lost in FITS"
			    " conversion. Try WCS?." <<
			    LogIO::POST;
			break;
		    }
		}
	    }
	    else {
		// determine which axis is the "latitude" axis, i.e. DEC or xLAT
		int theLatAxisNum = -1;
		for (uint32_t k=0; k<ctype.nelements(); k++){
		    string theType(ctype[k]);
		    if (theType.substr(0,3) == "DEC" || theType.substr(1,3) == "LAT"){
			theLatAxisNum = k;
			break;
		    }
		}
		if (theLatAxisNum == -1){
		    header.define("pv2_", pvi_ma);
		    os << LogIO::NORMAL << 
			"There is no axis with type DEC or LAT. Cannot identify latitude axis for WCS."
			" Will assume axis 2 as default." <<
			LogIO::POST;
		}
		else {
		    ostringstream oss;
		    oss << "pv" << theLatAxisNum+1 << "_"; // numbers are start at 1 in WCS
		    String s(oss);

		    header.define(s, pvi_ma);

		    os << LogIO::DEBUG1 << 
			"Identified axis number " << theLatAxisNum+1 << " as latitude axis for WCS. " << 
			s << " is the keyword name." <<
			LogIO::POST;
		}
	    }
	}
	if (specAxis >= 0) {
	    const SpectralCoordinate &spec = coordsys.spectralCoordinate(specCoord);
	    spec.toFITS(header, specAxis, os, oneRelative, preferVelocity, 
			opticalVelocity, preferWavelength, airWavelength);
	}

// Write out the obsinfo

	String error;
	bool ok = coordsys.obsInfo().toFITS(error, header);
	if (!ok) {
	    os << LogIO::SEVERE << "Error converting ObsInfo: " << error << 
		LogIO::POST;
	}

	return ok;
    }


    bool FITSCoordinateUtil::toFITSHeaderStokes(Vector<double>& crval,
						Vector<double>& crpix,
						Vector<double>& cdelt,
						LogIO& os,
						const CoordinateSystem& coordsys,
						int32_t stokesAxis, int32_t stokesCoord)  const
    {
	Vector<int32_t> stokes(coordsys.stokesCoordinate(stokesCoord).stokes());
	int32_t inc = 1;
	bool inorder = true;
	if (stokes.nelements() > 1) {
	    inc = Stokes::FITSValue(Stokes::StokesTypes(stokes(1))) - 
		Stokes::FITSValue(Stokes::StokesTypes(stokes(0)));
	    for (uint32_t k=2; k<stokes.nelements(); k++) {
		if ((Stokes::FITSValue(Stokes::StokesTypes(stokes(k))) - 
		     Stokes::FITSValue(Stokes::StokesTypes(stokes(k-1)))) !=
		    inc) {
		    inorder = false;
		}
	    }
	}
	if (inorder) {
	    crval(stokesAxis) = Stokes::FITSValue(Stokes::StokesTypes(stokes(0)));
	    crpix(stokesAxis) = 1;
	    cdelt(stokesAxis) = inc;
	} else {

	    os << LogIO::SEVERE 
	       <<  "The Stokes coordinate in this CoordinateSystem is too" << endl;
	    os << LogIO::SEVERE 
	       << "complex to convert to the FITS convention" << LogIO::POST;
	    return false;
	}
//
	return true;
    }


    bool FITSCoordinateUtil::generateFITSKeywords (LogIO&, bool& isNCP, 
						   double& longPole,  double& latPole,
						   Vector<double>& crval,
						   Vector<double>& crpix,
						   Vector<double>& cdelt,
						   // Vector<double>& crota,
						   Vector<double>& pvi_ma,
						   Vector<String>& ctype,
						   Vector<String>& cunit,
						   Matrix<double>& pc,
						   const CoordinateSystem& cSys,
						   int32_t skyCoord, int32_t longAxis, 
						   int32_t latAxis, int32_t specAxis, 
						   int32_t stokesAxis, bool, 
						   double offset, const String&) const
    {
	const int32_t n = cSys.nWorldAxes();
	crval = cSys.referenceValue();
	crpix = cSys.referencePixel() + offset;
	cdelt = cSys.increment();

// Generate FITS ctypes from DirectionCoordinate

	Vector<String> cctype(2);
	if (skyCoord >= 0) {
	    const DirectionCoordinate dCoord = cSys.directionCoordinate(skyCoord);


	    pvi_ma = dCoord.projection().parameters();

	    longPole = dCoord.longLatPoles()(2);
	    latPole =  dCoord.longLatPoles()(3);
//
	    const DirectionCoordinate &dc = cSys.directionCoordinate(skyCoord);
	    double reflat = 0.;
	    if(latAxis>=0){
	      reflat = C::pi/180.0*crval(latAxis);
	    }
	    cctype = cTypeFromDirection (isNCP, dc.projection(), 
					 DirectionCoordinate::axisNames(dc.directionType(),
									true), reflat, true);
	}
//
	ctype = cSys.worldAxisNames();
	for (int32_t i=0; i < n; i++) {
	    if (i == longAxis || i == latAxis) { 
		if (i==longAxis) {
		    ctype[i] = cctype[0];
		} else {
		    ctype[i] = cctype[1];
		}
	    } else if (i == specAxis) {

// Nothing - will be handled by SpectralCoordinate

	    } else if (i == stokesAxis) {
		ctype[i] = "STOKES";
	    } else {

// Linear and Tabular

		ctype[i].upcase();
		if (ctype[i].length() > 8) {
		    ctype[i] = ctype[i].at(0,8);
		}
		while (ctype[i].length() < 8) ctype[i] += " ";
	    }
	}

// CUNIT is case sensitive. 

	cunit = cSys.worldAxisUnits();
	for (int32_t i=0; i<n; i++) {
	    if (cunit(i).length() > 8) {
		cunit(i) = cunit(i).at(0,8);
	    }
	    while (cunit(i).length() < 8) cunit(i) += " ";
	}
//
	Matrix<double> imageLT = cSys.linearTransform();
	pc = imageLT;
	// need to transpose to conform with FITSKeywordUtil
	for(uint32_t i=0; i<imageLT.nrow(); i++){
	  for(uint32_t j=0; j<imageLT.ncolumn(); j++){
	    pc(i,j) = imageLT(j,i);
	  }
	}

	return true;
    }




    bool FITSCoordinateUtil::fromFITSHeader (int32_t& stokesFITSValue, 
					     CoordinateSystem& cSys,
					     RecordInterface& recHeader,
					     const Vector<String>& header,
					     const IPosition& shape, 
					     uint32_t which) const

    {
// this method takes header converts it into cSys and puts the remainer into recHeader


	LogIO os(LogOrigin("FITSCoordinateUtil", "fromFITSHeader"));
	CoordinateSystem cSysTmp;

	if (header.nelements()==0) {
	    os << "Header is empty - cannot create CoordinateSystem" << LogIO::WARN;
	    return false;
	}

// Convert header to char* for wcs parser

        // Keep obsgeo-x,y,z because wcspih removes them, but they are needed
        // by ObsInfo.
        vector<String> saveCards;
	int nkeys = header.nelements();
	String all;
	for (int i=0; i<nkeys; i++) {
            if (header[i].substr(0,7) == "OBSGEO-") {
                saveCards.push_back (header[i]);
            }
	    int hsize = header[i].size();
	    char *tmp = new char[hsize+1];
	    if (hsize >= 19 &&       // kludge changes 'RA--SIN ' to 'RA---SIN', etc.
		header[i][0]=='C' && header[i][1]=='T' && header[i][2]=='Y' &&
		header[i][3]=='P' && header[i][4]=='E' &&
		(header[i][5]=='1'|| header[i][5]=='2') &&
		header[i][14]=='-' && header[i][18]==' ') {
		strncpy(tmp,header[i].c_str(),hsize+1);
		tmp[18]=tmp[17];tmp[17]=tmp[16];tmp[16]=tmp[15];tmp[15]=tmp[14];
		all = all.append(tmp);
		os << LogIO::NORMAL
		   << "Header\n"<< header[i] << "\nwas interpreted as\n" << tmp << LogIO::POST;
	    } else if (hsize >= 19 &&	  // change GLON-FLT to GLON-CAR, etc.
		       header[i][0]=='C' && header[i][1]=='T' && header[i][2]=='Y' &&
		       header[i][3]=='P' && header[i][4]=='E' &&
		       (header[i][5]=='1'|| header[i][5]=='2') &&
		       header[i][15]=='-' && header[i][16]=='F' &&
		       header[i][17]=='L' && header[i][18]=='T') {
		strncpy(tmp,header[i].c_str(),hsize+1);
		tmp[16]='C'; tmp[17]='A'; tmp[18]='R';
		all = all.append(tmp);
		os << LogIO::NORMAL
		   << "Header\n"<< header[i] << "\nwas interpreted as\n" << tmp << LogIO::POST;
	    }
	    else if (
            // adding the first condition (FEEQ, VRAD, VOPT) is necessary to avoid incorrect munging
            // of position-velocity image axes.
	    	! (header[i].contains("FREQ") || header[i].contains("VRAD") || header[i].contains("VOPT"))
            && hsize >= 19 && (
	    		header[i].startsWith("CTYPE1")
	    		|| header[i].startsWith("CTYPE2")
	    	)
		    && header[i][15]==' ' && header[i][16]==' '
		    && header[i][17]==' ' && header[i][18]==' '
		) {
	    	// change 'GLON    ' to 'GLON-CAR', etc.
	    	strncpy(tmp,header[i].c_str(),hsize+1);
	    	tmp[15]='-'; tmp[16]='C'; tmp[17]='A'; tmp[18]='R';
	    	all = all.append(tmp);
	    	os << LogIO::NORMAL
	    		<< "Header\n"<< header[i] << "\nwas interpreted as\n" << tmp << LogIO::POST;
	    }
	    else if (hsize >= 19 &&	  // change 'OBSFREQ' to 'RESTFRQ'
		       header[i][0]=='O' && header[i][1]=='B' && header[i][2]=='S' &&
		       header[i][3]=='F' && header[i][4]=='R' &&
		       header[i][5]=='E' && header[i][6]=='Q' &&
		       header[i][7]==' ') {
		strncpy(tmp,header[i].c_str(),hsize+1);
		tmp[0]='R'; tmp[1]='E'; tmp[2]='S'; tmp[3]='T';
		tmp[4]='F'; tmp[5]='R'; tmp[6]='Q'; tmp[7]=' ';
		all = all.append(tmp);
		os << LogIO::NORMAL
		   << "Header\n"<< header[i] << "\nwas interpreted as\n" << tmp << LogIO::POST;
	    } else if (hsize >= 24 &&       // ignore "-SIP"
		       header[i][0]=='C' && header[i][1]=='T' && header[i][2]=='Y' &&
		       header[i][3]=='P' && header[i][4]=='E' &&
		       (header[i][5]=='1'|| header[i][5]=='2') &&
		       header[i][19]=='-' && header[i][20]=='S' && 
		       header[i][21]=='I' && header[i][22]=='P' && 
		       header[i][23]=='\'') {
		strncpy(tmp,header[i].c_str(),hsize+1);
		tmp[19]='\'';tmp[20]=tmp[21]=tmp[22]=tmp[23]=' ';
		all = all.append(tmp);
		os << LogIO::NORMAL
		   << "The SIP convention for representing distortion in FITS headers\n  is not part of FITS standard v3.0"
		   << " and not yet supported by CASA.\n  Header\n  "<< header[i] << "\n  was interpreted as\n  " << tmp << LogIO::POST;
	    } else {
	        if(header[i].contains("-GLS")){
	  	    os << LogIO::WARN << "Note: The GLS projection is deprecated. Use SFL instead." << LogIO::POST;
	        }
		all = all.append(header(i));
	    }
	    delete [] tmp;
	}
	char* pChar2 = const_cast<char *>(all.chars());
    
// Print cards for debugging
  
	bool print(false);
        if (print) {
	    cerr << "Header Cards " << endl;
	    for (int32_t i=0; i<nkeys; i++) {
		uint32_t pt = i*80;
		char* pChar3 = &pChar2[pt];
		String s(pChar3,80);
		cerr << s << endl;
            }
	    cerr << endl;
        }
  
// Parse FITS header cards with wcs and remove wcs cards from char header

	::wcsprm* wcsPtr = 0;
	int relax = WCSHDR_all;
	int nrej = 0;
	int nwcs = 0;
	int ctrl = -2;
	int status = wcspih(pChar2, nkeys, relax, ctrl, &nrej, &nwcs, &wcsPtr);
	if (status!=0) {
	    os << LogIO::SEVERE << "wcs FITS parse error with error code " << status << LogIO::POST;
	    return false;
	}
	if (uint32_t(nwcs) == 0) {
	    os << LogIO::NORMAL << "No WCS compliant coordinate representation found. Will try to continue ..." << LogIO::POST;
	    cardsToRecord (os, recHeader, pChar2);
	    return false;
	}
	else if (which >= uint32_t(nwcs)) {
	    os << LogIO::WARN << "Requested WCS # " << which << " (zero-based) exceeds the number available, i.e. must be smaller than " << nwcs << LogIO::POST;
	    os << LogIO::WARN << "Will use the last available one." << LogIO::POST;
	    which = nwcs-1;
	}

// Add the saved OBSGEO keywords.
// This is a bit tricky because pChar2 is in fact the char* of String 'all'.
// So make a new string and add them to it.
        String newHdr;
        if (saveCards.size() > 0) {
            newHdr = String(pChar2);
            for (uint32_t i=0; i<saveCards.size(); ++i) {
                newHdr.append (saveCards[i]);
            }
            pChar2 = const_cast<char*>(newHdr.chars());
        }

// Put the rest of the header into a Record for subsequent use
	cardsToRecord (os, recHeader, pChar2);


// Add FITS units to system

	UnitMap::addFITS();

// Set the ObsInfo.  Some of what we need is in the WCS struct (date) and some in
// the FITS Records now.  Remove cards from recHeader as used.

	ObsInfo obsInfo = getObsInfo (os, recHeader, wcsPtr[which]);
	cSysTmp.setObsInfo(obsInfo);
//
// Now fix up wcs internal values for various inconsistencies,errors
// and non-standard FITS formats.  This may invoke  :
//  celfix:   translate AIPS-convention celestial projection types, -NCP and -GLS, set in CTYPEia.
//  spcfix:   translate AIPS-convention spectral types, FREQ-LSR, FELO-HEL, etc., set in CTYPEia.
//  datfix:   recast the older DATE-OBS date format to year-2000 standard
//            form, and derive MJD-OBS from it if not already set.
//  cylfix:   fixes WCS FITS header cards for malformed cylindrical projections 
//            that suffer from the problem described in  Sect. 7.3.4 of Paper I.
//  unitifx:  fixes non-standard units

//
	Vector<String> wcsNames(NWCSFIX);
	wcsNames(DATFIX) = String("datfix");
	wcsNames(UNITFIX) = String("unitfix");
	wcsNames(CELFIX) = String("celfix");
	wcsNames(SPCFIX) = String("spcfix");
	wcsNames(CYLFIX) = String("cylfix");
//
	int stat[NWCSFIX];
	ctrl = 7;                         // Do all unsafe unit corrections
        // wcsfix needs int32_t shape, so copy it.
        std::vector<int32_t> tmpshp(shape.begin(), shape.end());

	bool doAbort=false;
	uint32_t eCount=0;
        if (wcsfix(ctrl, &(tmpshp[0]), &wcsPtr[which], stat) > 0) {
	    for (int i=0; i<NWCSFIX; i++) {
		int err = stat[i];
		if (err>0) {
		    os << LogIO::NORMAL << wcsNames(i) << " incurred the error " << wcsfix_errmsg[err] <<  LogIO::POST;
		    eCount++;
		    if(i==CELFIX){
			doAbort=true;
		    }
		}
	    }
	    if(eCount>1 || doAbort) {
		os << LogIO::WARN << "The wcs function failures are too severe to continue ..." <<  LogIO::POST;

		status = wcsvfree(&nwcs, &wcsPtr);
		if (status!=0) {
		    String errmsg = "wcs memory deallocation error: ";
		    os << errmsg << LogIO::EXCEPTION;
		}
//
		return false;
	    }
	    os << LogIO::NORMAL << "Will try to continue ..." <<  LogIO::POST; 
	}	  

// Now fish out the various coordinates from the wcs structure and build the CoordinateSystem

	Vector<int32_t> dirAxes;
	Vector<int32_t> linAxes;
	int32_t longAxis = -1;
	int32_t latAxis = -1;
	int32_t specAxis = -1;
	int32_t stokesAxis = -1;
	const uint32_t nAxes = wcsPtr[which].naxis;

	if(nAxes>shape.size()){
	  os << LogIO::NORMAL << "The WCS for this image contains " << nAxes - shape.size()
	     << " degenerate axes." <<  LogIO::POST;
	}
	else if(nAxes<shape.size()){
	  os << LogIO::WARN << "WCS does only provide information for "  
	     << nAxes << " out of " << shape.size() << " axes of this image." 
	     <<  LogIO::POST;
	}	  
//
	bool ok=true;
	ok = addDirectionCoordinate (cSysTmp, dirAxes, wcsPtr[which], os);
	if (!ok) {
	    wcsvfree(&nwcs, &wcsPtr);
	    return false;
	}
	if (dirAxes.nelements()==2) {
	    longAxis = dirAxes[0];
	    latAxis = dirAxes[1];
	}
//
	ok = addStokesCoordinate (cSysTmp, stokesAxis, stokesFITSValue, wcsPtr[which], shape, os);
	if (!ok) {
	    wcsvfree(&nwcs, &wcsPtr);
	    return false;
	}
//
	ok = addSpectralCoordinate (cSysTmp, specAxis, wcsPtr[which], shape, os);
	if (!ok) {
	    wcsvfree(&nwcs, &wcsPtr);
	    return false;
	}
//
	ok = addLinearCoordinate (cSysTmp, linAxes, wcsPtr[which], os);
	if (!ok) {
	    wcsvfree(&nwcs, &wcsPtr);
	    return false;
	}

// Free up wcs memory

	status = wcsvfree(&nwcs, &wcsPtr);
	if (status!=0) {
	    String errmsg = "wcs memory deallocation error: ";
	    os << errmsg << LogIO::EXCEPTION;
	}

// Now we need to work out the transpose order of the CS

	Vector<int32_t> order(nAxes);
	int32_t nspecial = 0;                    // Anything other than linear
//
	if (longAxis >=0) nspecial++;
	if (latAxis >=0) nspecial++;
	if (stokesAxis >= 0) nspecial++;
	if (specAxis >= 0) nspecial++;
//
	int32_t linused = 0;
	for (int32_t i=0; i<int32_t(nAxes); i++) {
	    if (i == longAxis) {
		order(i) = 0; // long is always first if it exist
	    } else if (i == latAxis) {
		order(i) = 1; // lat is always second if it exists
	    } else if (i == stokesAxis) {
		if (longAxis >= 0) { // stokes is axis 0 if no dir, otherwise 2
		    order(i) = 2;
		} else {
		    order(i) = 0;
		}
	    } else if (i == specAxis) {
		if (longAxis >= 0 && stokesAxis >= 0) {
		    order(i) = 3; // stokes and dir
		} else if (longAxis >= 0) {
		    order(i) = 2; // dir only
		} else if (stokesAxis >= 0) {
		    order(i) = 1;  // stokes but no dir
		} else {
		    order(i) = 0; // neither stokes or dir
		}
	    } else {
		order(i) = nspecial + linused;
		linused++;
	    }
	}
//
	cSysTmp.transpose(order,order);
//
	cSys = cSysTmp;
	return true;
    }

    static bool do_sub_wcs(const ::wcsprm& wcs, int &nsub, Block<int> &axes, ::wcsprm &wcsDest, LogIO &os)
    {
    	try {
    		Coordinate::sub_wcs(wcs, nsub, axes.storage(), wcsDest);
    		return true;
    	} catch (const AipsError &e) {
    		os << LogIO::WARN << e.what() << LogIO::POST;
    		return false;
    	}
    }

    bool FITSCoordinateUtil::addDirectionCoordinate (CoordinateSystem& cSys, 
						     Vector<int32_t>& dirAxes, 
						     const ::wcsprm& wcs,
						     LogIO& os) const
    {

// Extract wcs structure pertaining to Direction Coordinate

	int nsub = 2;
	Block<int> axes(nsub);
	axes[0] = WCSSUB_LONGITUDE;
	axes[1] = WCSSUB_LATITUDE;
//
	::wcsprm wcsDest;
	wcsInit (wcsDest);
	bool ok = do_sub_wcs(wcs, nsub, axes, wcsDest, os);

// See if we found the Sky

	if (ok && nsub==2) {

// Call wcssset on new struct

	    setWCS (wcsDest);
//
	    dirAxes.resize(2);
	    dirAxes[0] = axes[0] - 1;          // 1 -> 0 rel
	    dirAxes[1] = axes[1] - 1;

// Extract Direction system

	    MDirection::Types dirSystem;
	    String errMsg;
	    if (!directionSystemFromWCS (os, dirSystem, errMsg, wcsDest)) {
		os << LogIO::WARN << errMsg << LogIO::POST;
		ok = false;
	    }

// Try to make DirectionCoordinate and fix up zero increments etc and add to CoordinateSystem

	    if (ok) {
		try {
		    bool oneRel = true;           // wcs structure from FITS has 1-rel pixel coordinates
		    DirectionCoordinate c(dirSystem, wcsDest, oneRel);
//
		    fixCoordinate (c, os);
		    cSys.addCoordinate(c);
		} catch (std::exception& x) {
		    os << LogIO::WARN << x.what() << LogIO::POST;
		    ok = false;
		}
	    }
	}

// Clean up

	wcsfree (&wcsDest);
	return ok;
    }


    bool FITSCoordinateUtil::addLinearCoordinate (CoordinateSystem& cSys, 
						  Vector<int32_t>& linAxes, 
						  const ::wcsprm& wcs,
						  LogIO& os) const
    {

// Extract wcs structure pertaining to Linear Coordinate

	int nsub = 1;
	Block<int> axes(wcs.naxis);
	axes[0] = -(WCSSUB_LONGITUDE | WCSSUB_LATITUDE | WCSSUB_SPECTRAL | WCSSUB_STOKES);
//
	::wcsprm wcsDest;
	wcsInit (wcsDest);
//
	bool ok = do_sub_wcs(wcs, nsub, axes, wcsDest, os);

// See if we found the coordinate

	if (ok && nsub>0) {

// Call wcssset on new struct

	    setWCS (wcsDest);

	    linAxes.resize(nsub);
	    for (int i=0; i<nsub; i++) {
		linAxes[i] = axes[i] - 1;           // 1 -> 0 rel
	    }

// Try to make LinearCoordinate from wcs structure and
// fix up zero increments etc and add to CoordinateSystem

	    if (ok) {
		try {
		    bool oneRel = true;    // wcs structure from FITS has 1-rel pixel coordinates
		    LinearCoordinate c(wcsDest, oneRel);
//
		    fixCoordinate (c, os);
		    cSys.addCoordinate(c);
		} catch (std::exception& x) {
		    os << LogIO::WARN << x.what() << LogIO::POST;
		    ok = false;
		}
	    }
	}

// Clean up

	wcsfree (&wcsDest);
	return ok;
    }


    void FITSCoordinateUtil::wcsInit (::wcsprm& wcsDest)
    { 
        wcsDest.flag = -1;
        // wcslib-4.8 introduced the following members.
        // Unfortunately it does not always initialize them.
        // In version 5 it is fixed; for older versions it is unclear.
#if WCSLIB_VERSION_MAJOR == 4 && WCSLIB_VERSION_MINOR >= 8
        wcsDest.err = 0;
        wcsDest.lin.err = 0;
        wcsDest.spc.err = 0;
        wcsDest.cel.err = 0;
        wcsDest.cel.prj.err = 0;
#endif
    }

    bool FITSCoordinateUtil::addStokesCoordinate (CoordinateSystem& cSys, 
						  int32_t& stokesAxis,  int32_t& stokesFITSValue,
						  const ::wcsprm& wcs, const IPosition& shape,
						  LogIO& os) const
    { 

// Extract wcs structure pertaining to Stokes Coordinate

	int nsub = 1;
	Block<int> axes(nsub);
	axes[0] = WCSSUB_STOKES;
//
	::wcsprm wcsDest;
    wcsInit (wcsDest);
    bool ok = do_sub_wcs(wcs, nsub, axes, wcsDest, os);

// See if we found the axis

	if (ok && nsub==1) {

// Call wcssset on new struct

	    setWCS (wcsDest);

// Try to create StokesCoordinate

	    stokesAxis = axes[0] - 1;              // 1 -> 0 rel
	    uint32_t stokesAxisShape = 1;
	    if(stokesAxis<(int32_t)shape.size()){
	      stokesAxisShape = shape(stokesAxis);
	    }
	    bool warnStokes = stokesFITSValue > 0;
	    stokesFITSValue = -1;
	    Vector<int32_t> stokes(1); stokes = 1;
	    StokesCoordinate c(stokes);                  // No default constructor
	    String errMsg;
	    if (stokesCoordinateFromWCS (os, c, stokesFITSValue, errMsg, wcsDest, 
					 stokesAxisShape, warnStokes)) {
		cSys.addCoordinate(c);
	    } else {
		os << LogIO::WARN << errMsg << LogIO::POST;
		ok = false;
	    }
	}

// Clean up

	wcsfree (&wcsDest);
	return ok;
    }

  


    bool FITSCoordinateUtil::addSpectralCoordinate (CoordinateSystem& cSys, 
						    int32_t& specAxis,
						    const ::wcsprm& wcs,
						    const IPosition& shape,
						    LogIO& os) const
    {

        // Extract wcs structure pertaining to Spectral Coordinate
	int nsub = 1;
	Block<int> axes(nsub);
	axes[0] = WCSSUB_SPECTRAL;

	::wcsprm wcsDest;
    wcsInit (wcsDest);
    bool ok = do_sub_wcs(wcs, nsub, axes, wcsDest, os);

	uint32_t nc = 1;
	if(axes[0]-1<(int32_t)shape.nelements()){
	  nc = shape(axes[0]-1); // the number of channels of the spectral axis
	}

	SpectralCoordinate::SpecType nativeSType = SpectralCoordinate::FREQ;

	// See if we found the axis
	if (ok && nsub==1) {

	    String errMsg;

	    // throws exception if wcsset() fails
	    setWCS (wcsDest);

	    String cType = wcsDest.ctype[0];
    
	    if (cType.contains("WAVE") || cType.contains("AWAV")){

		if(nc==0){
		    os << LogIO::WARN << "Will omit tabular spectral coordinate with no channels." << LogIO::POST;
		    wcsfree (&wcsDest);
		    return true;
		}

		// make a tabular frequency coordinate from the wavelengths
		MFrequency::Types freqSystem;
		specAxis = axes[0]-1;

		if (!frequencySystemFromWCS (os, freqSystem, errMsg, wcsDest)) {
		    os << LogIO::WARN << errMsg << LogIO::POST;
		    ok = false;
		}
		double cRval = wcsDest.crval[0];
		double cRpix = wcsDest.crpix[0];
		double cDelt = wcsDest.cdelt[0];
		double cPc = wcsDest.pc[0];
		Vector<double> wavelengths(nc);

		//cout << "crval " << cRval << " crpix " << cRpix << " pc " << cPc << " cdelt " << cDelt << endl;
		
		String waveUnit = String(wcsDest.cunit[0]);
		double restFrequency = wcs.restfrq;
		if (restFrequency==0.){
		    if(wcs.restwav != 0.){
			restFrequency = C::c/wcs.restwav;
		    }
		}

		for(uint32_t i=0; i<nc; i++){
		  wavelengths(i) = cRval + cDelt * cPc * (double(i + 1) - cRpix); // +1 because FITS works 1-based
		    //cout << "wave i " << i << " " << wavelengths(i) << " " << waveUnit << endl;
		}

		bool inAir = false;
		nativeSType = SpectralCoordinate::WAVE;
		if(cType.contains("AWAV")){
		  // os << LogIO::NORMAL << "Translating Air Wavelength into wavelength ..." << LogIO::POST;
		    inAir = true;
		    nativeSType = SpectralCoordinate::AWAV;
		}

		SpectralCoordinate c(freqSystem, wavelengths, waveUnit, restFrequency, inAir);
		c.setNativeType(nativeSType);

		try {
		    cSys.addCoordinate(c);
		} catch (std::exception& x) {
		    os << LogIO::WARN << x.what() << LogIO::POST;
		    ok = false;
		}     
	    }
	    else if(cType.contains("VOPT") || cType.contains("FELO")){

		if(nc==0){
		    os << LogIO::WARN << "Will omit tabular spectral coordinate with no channels." << LogIO::POST;
		    wcsfree (&wcsDest);
		    return true; 
		}

		// make a tabular frequency coordinate from the optical velocities
		MFrequency::Types freqSystem;
		specAxis = axes[0]-1;

		if (!frequencySystemFromWCS (os, freqSystem, errMsg, wcsDest)) {
		    os << LogIO::WARN << errMsg << LogIO::POST;
		    ok = false;
		}
		double cRval = wcsDest.crval[0];
		double cRpix = wcsDest.crpix[0];
		double cDelt = wcsDest.cdelt[0];
		double cPc = wcsDest.pc[0];
		double restFrequency = wcs.restfrq;
		if (restFrequency==0.){
		    if(wcs.restwav != 0.){
			restFrequency = C::c/wcs.restwav;
		    }
		    else{
			os << LogIO::WARN << "Zero or no rest frequency provided for velocity axis." << LogIO::POST;
			ok = false;
		    }	
		}
		Vector<double> frequencies(nc);

		//cout << "crval " << cRval << " crpix " << cRpix << " pc " << cPc << " cdelt " << cDelt << endl;
		//cout << "restfrq " << restFrequency << " cunit " << String(wcsDest.cunit[0]) << endl;

		Unit uCunit(String(wcsDest.cunit[0]));
		Unit mps("m/s");

		for(uint32_t i=0; i<nc; i++){
		    Quantity velQ(cRval + cDelt * cPc * (double(i+1) - cRpix), uCunit); // +1 because FITS works 1-based
		    double vel = velQ.getValue(mps);
		    if(vel>-C::c){
			frequencies(i) = restFrequency/(vel/C::c+1.); // in Hz
		    }
		    else{
			frequencies(i) = HUGE_VAL;
		    }
		    //cout << "freq i " << i << " " << frequencies(i) << " Hz"<< endl;
		}
		    
		SpectralCoordinate c(freqSystem, frequencies, restFrequency);
		nativeSType = SpectralCoordinate::VOPT;
		c.setNativeType(nativeSType);
		
		try {
		    cSys.addCoordinate(c);
		} catch (std::exception& x) {
		    os << LogIO::WARN << x.what() << LogIO::POST;
		    ok = false;
		}     
	    }
	    else{ // make a coordinate linear in frequency using wcslib
		
		// Convert the struct to a frequency base...
		// ...really convert to FREQ... casa (non-core) above depends
		// on receiving FREQ coordinates (not VELO)... if other uses
		// of addSpectralCoordinate( ) depend on retrieving VELO, then
		// FREQ conversion should be added as an option, taking advantage
		// of wcslib's conversion abilities.
		int index=0;
		char ctype[9];
		
		if (cType.contains("FREQ")){
		  strcpy(ctype,"FREQ-???");
		  nativeSType = SpectralCoordinate::FREQ;
		}
		else if(cType.contains("VELO")){
		  strcpy(ctype, "FREQ-???");
		  nativeSType = SpectralCoordinate::VRAD;
		}
		else if (cType.contains("VRAD")){
		  strcpy(ctype, "FREQ-???");
		  nativeSType = SpectralCoordinate::VRAD;
		}
		else {
		    os << LogIO::WARN << "Unrecognized frequency type" << LogIO::POST;
		    ok = false;
		}
		
		if (ok) {
		    int status = 0;
		    if ((status=wcssptr(&wcsDest, &index, ctype))) {
			os << LogIO::WARN << "Failed to convert Spectral coordinate to Frequency, error status = "
			    
			   << status << ": " << endl << "   " << wcs_errmsg[status] << endl;
			switch(status){
			case 4:
			case 5:
			case 6:
			case 7:
			    os << "Will try to continue ...";
			    break;
			default:
			    os << "Will not try to continue ...";
			    ok = false;
			}
			os << LogIO::POST;
		    } else {
			// throws exception if wcsset() fails
			setWCS (wcsDest);
		    }
		}
	      

		// Find frequency system
		MFrequency::Types freqSystem;
		if (ok) {
 		    specAxis = axes[0]-1;
		    if (!frequencySystemFromWCS (os, freqSystem, errMsg, wcsDest)) {
			os << LogIO::WARN << errMsg << LogIO::POST;
			ok = false;
		    }
		}

		// Try to create SpectralCoordinate and fix up zero 
		// increments etc and add to CoordinateSystem
		if (ok) {
		    try {
			bool oneRel = true;           // wcs structure from FITS has 1-rel pixel coordinate
			SpectralCoordinate c(freqSystem, wcsDest, oneRel);
			c.setNativeType(nativeSType);
			
			fixCoordinate (c, os);
			cSys.addCoordinate(c);
		    } catch (std::exception& x) {
			os << LogIO::WARN << x.what() << LogIO::POST;
			ok = false;
		    }     
		}
	    }
	} else {
	  //os << LogIO::DEBUG1 << "passing empty or nonexistant spectral Coordinate axis" << LogIO::POST;
	  os << "passing empty or nonexistant spectral Coordinate axis" << LogIO::POST;
	}

	// Clean up
	wcsfree (&wcsDest);
	return ok;
    }



    bool FITSCoordinateUtil::directionSystemFromWCS (LogIO& os, MDirection::Types& type,String& errMsg,
						     const ::wcsprm& wcs) const
    {

// Extract Equinox keyword

//
	bool eqIsDefined = !undefined(wcs.equinox);
	double equinox(0.0);
	if (eqIsDefined) equinox = wcs.equinox;
	bool eqIs1950(false);
	bool eqIs1950VLA(false);
	bool eqIs2000(false);
	if (eqIsDefined) {
	    eqIs1950 = casacore::near(equinox, 1950.0);
	    eqIs1950VLA = casacore::near(equinox, 1979.9);
	    eqIs2000 = casacore::near(equinox, 2000.0);
	}

// Extract RADESYS keyword

	bool sysIsDefined = wcs.radesys[0]!='\0';
	String raDecSys;
	if (sysIsDefined) {
	    String tt(wcs.radesys);
	    int32_t i1 = tt.index(RXwhite,0);
	    if (i1==-1) i1 = tt.length();
	    raDecSys = String(tt.before(i1));
	}

// Extract CTYPEs (must exist)

	String cTypeLon(wcs.ctype[0]);
	String cTypeLat(wcs.ctype[1]);
	cTypeLon.upcase();
	cTypeLat.upcase();

// See if we have xLON/xLAT pair

	String cLon(cTypeLon.at(0,4));
	String cLat(cTypeLat.at(0,4));
	ostringstream oss2;
	if (cLon=="GLON" && cLat=="GLAT") {

// galactic coordinates

	    type = MDirection::GALACTIC;   
	    return true;
	} else if (cLon=="ELON" && cLat=="ELAT") {

// ecliptic for J2000 equator and equinox
// Paper II suggests to use DATE-OBS or MJD-OBS rather than equinox ?

	    if (!eqIsDefined || (eqIsDefined && eqIs2000)) {
		type = MDirection::ECLIPTIC;   
		return true;
	    } else {
		oss2 << "Equinox " << equinox << " is invalid for Ecliptic Coordinates - must be 2000.0";
		errMsg = String(oss2);
		return false;
	    }
	} else if (cLon=="SLON" && cLat=="SLAT") {      

// supergalactic coordinates

	    type = MDirection::SUPERGAL;
	    return true;
	} else if (cLon=="HLON" && cLat=="HLAT") {
	    errMsg = String("Helioecliptic Coordinates are not supported");
	    return false;
	} else {
	    String cLon2(cTypeLon.at(1,3));
	    String cLat2(cTypeLat.at(1,3));
	    if ( (cLon2=="LON" || cLat2=="LAT") || (cLon2=="LAT" || cLat2=="LON") ) {
		oss2 << cLon << " and " << cLat << " are unsupported LON/LAT types";
		errMsg = String(oss2);
		return false;
	    }
	}

// OK we have dispensed with xLON/xLAT, let's move on to the rest
// Since we have successfully constructed a celestial wcsprm object
// we can assume the CTYPEs are correct

	if (raDecSys==String("ICRS")) {
	    if (!eqIsDefined || eqIs2000) {
		type = MDirection::ICRS;   
		return true;
	    } else {
		oss2 << "Direction system ICRS with equinox " << equinox << " is not supported";
		errMsg = String(oss2);
		return false;
	    }
	} else if (raDecSys==String("FK5")) {                
	    if (!eqIsDefined || eqIs2000) {                  // equinox always Julian for FK5
		type = MDirection::J2000;                     // Needs 
		return true;
	    } else {
		oss2 << "Direction system FK5 with equinox " << equinox << " is not supported";
		errMsg = String(oss2);
		return false;
	    }
	} else if (raDecSys==String("FK4")) {  
	    if (!eqIsDefined || eqIs1950) {                  // equinox always Besellian for FK4
		type = MDirection::B1950;   
		return true;
	    } else if (!eqIsDefined || eqIs1950VLA) {
		type = MDirection::B1950_VLA;
		return true;
	    } else {
		oss2 << "Direction system FK4 with equinox " << equinox << " is not supported";
		errMsg = String(oss2);
		return false;
	    }
	} else if (raDecSys==String("FK4-NO-E")) {
	    if (!eqIsDefined || eqIs1950) {    // equinox always Besellian
		type = MDirection::B1950;   
		return true;
	    } else if (!eqIsDefined || eqIs1950VLA) {
		type = MDirection::B1950_VLA;
		return true;
	    } else {
		oss2 << "Direction system FK4-NO-E with equinox " << equinox << " is not supported";
		errMsg = String(oss2);
		return false;        
	    }
	} else if (raDecSys==String("GAPPT")) {
	    type = MDirection::APP;   
	    errMsg = String("Direction system GAPPT is not supported");
	    return false;
	} else {
	    if (sysIsDefined) {
		oss2 << "Direction system '" << raDecSys << "' is not supported";
		errMsg = String(oss2);
		return false;
	    } else {
		if (eqIsDefined) {                            // No RaDecSys but Equinox available
		    if (equinox>=1984.0) {                     // Paper II
			type = MDirection::J2000;               // FK5
			return true;
		    } else if (casacore::near(equinox,1979.9)) {
			type = MDirection::B1950_VLA;
			return true;
		    } else {
			type = MDirection::B1950;               // FK4
			return true;
		    }
		} else {                                      // No RaDecSys or equinox
		    os << "No Direction system is defined - J2000 assumed" << LogIO::POST;
		    type = MDirection::J2000;                  // Defaults to ICRS
		    return true;
		}
	    }
	}
//
	errMsg = String("FITSCoordinateUtil::directionSystemFromWCS - logic error");
	return false;
    }


    bool FITSCoordinateUtil::frequencySystemFromWCS (LogIO& os, MFrequency::Types& type,String& errMsg,
						     const ::wcsprm& wcs) const
    //
    // After running it through the wcsFixItUp function, I can assume that
    // wcs.specsys will always be filled in and that CTYPE will be adjusted
    // appropriately.
    //
    {
        if (wcs.specsys[0]=='\0') {
            if (wcs.velref==0) { // velref was also not given
	        os << LogIO::NORMAL << "Neither SPECSYS nor VELREF keyword given, spectral reference frame not defined ..." 
		   << LogIO::POST;
	        type = MFrequency::Undefined;
	        return true;
	    }
	    else { // velref was given
	        int32_t vref = wcs.velref;
	        os << LogIO::NORMAL << "No SPECSYS but found (deprecated) VELREF keyword with value " << vref << LogIO::POST;
	        if(vref>256){
		  vref -= 256;
		}
		switch(vref){
		case 1:
		  type = MFrequency::LSRK;
		  os << LogIO::NORMAL << "  => LSRK assumed" << LogIO::POST;
		  break;
		case 2:
		  type = MFrequency::BARY;
		  os << LogIO::NORMAL << "  => BARY assumed" << LogIO::POST;
		  break;
		case 3:
		  type = MFrequency::TOPO;
		  os << LogIO::NORMAL << "  => TOPO assumed" << LogIO::POST;
		  break;
		case 4:
		  type = MFrequency::LSRD;
		  os << LogIO::NORMAL << "  => LSRD assumed" << LogIO::POST;
		  break;
		case 5:
		  type = MFrequency::GEO;
		  os << LogIO::NORMAL << "  => GEO assumed" << LogIO::POST;
		  break;
		case 6:
		  type = MFrequency::REST;
		  os << LogIO::NORMAL << "  => REST assumed" << LogIO::POST;
		  break;
		case 7:
		  type = MFrequency::GALACTO;
		  os << LogIO::NORMAL << "  => GALACTO assumed" << LogIO::POST;
		  break;
		default:
		  type = MFrequency::TOPO;
		  os << LogIO::WARN << "Undefined by AIPS convention. TOPO assumed." << LogIO::POST;
		  break;
		}
		return true;
	    }
	}
	String specSys(wcs.specsys);
	specSys.upcase();

// Extract system

	ostringstream oss;
	if (specSys=="TOPOCENT") {
	    type = MFrequency::TOPO;
	    return true;
	} else if (specSys=="GEOCENTR") {
	    type = MFrequency::GEO;
	    return true;
	} else if (specSys=="BARYCENT") {
	    type = MFrequency::BARY;
	    return true;
	} else if (specSys=="HELIOCEN") {
	    type = MFrequency::BARY;
	    os << LogIO::NORMAL << "The HELIOCENTRIC frequency system is deprecated in FITS - it is assumed BARYCENTIC was meant" << LogIO::POST;
	    return true;
	} else if (specSys=="LSRK") {
	    type = MFrequency::LSRK;
	    return true;
	} else if (specSys=="LSRD") {
	    type = MFrequency::LSRD;
	    return true;
	} else if (specSys=="GALACTOC") {
	    type = MFrequency::GALACTO;
	    return true;
	} else if (specSys=="LOCALGRP") {
	    type = MFrequency::LGROUP;
	    return true;
	} else if (specSys=="CMBDIPOL") {
	    type = MFrequency::CMB;
	    return true;
	} else if (specSys=="SOURCE") {
	    type = MFrequency::REST;
	    return true;
	} else {
	    oss << "Frequency system '" << specSys << "' is not supported";
	    errMsg = String(oss);
	    return false;
	}
//
	errMsg = String("FITSCoordinateUtil::frequencySystemFromWCS - logic error");
	return false;
    }



    bool FITSCoordinateUtil::stokesCoordinateFromWCS (LogIO& os, StokesCoordinate& coord, 
						      int32_t& stokesFITSValue, String& errMsg, 
						      const ::wcsprm& wcs, 
						      uint32_t shape, bool warnStokes) const
    {

// For the StokesCoordinate, the shape is not separable from the coordinate

	if (shape>4) {
	    os << "The Stokes axis is longer than 4 pixels.  This is not supported" 
	       << LogIO::EXCEPTION;       
	    return false;
	}
//
	if (wcs.naxis != 1) {
	    os << "The wcs structure holding the StokesAxis can only have one axis" << LogIO::EXCEPTION;
	}

// Fish out values

	double crpix = wcs.crpix[0] - 1.0;            // Make 0-rel
	double crval = wcs.crval[0];
	double cdelt = wcs.cdelt[0];
//
	Vector<int32_t> stokes(shape); 
	for (uint32_t k=0; k<shape; k++) {
	    double tmp = crval + (k - crpix)*cdelt;
	    if (tmp >= 0) {
		stokes(k) = int32_t(tmp + 0.01);
	    } else {
		stokes(k) = int32_t(tmp - 0.01);
	    }
//
	    if (stokes(k)==0) {
		if (warnStokes) {
		    os << LogIO::NORMAL 
		       << "Detected Stokes coordinate = 0; this is an unoffical" << endl;
		    os << "Convention for an image containing a beam.  Putting Stokes=Undefined" << endl;
		    os << "Better would be to write your FITS image with the correct Stokes" << LogIO::POST;
		}
//
		stokes(k) = Stokes::Undefined;
		stokesFITSValue = 0;
	    } else if (stokes(k)==5) {           
		os << LogIO::SEVERE << "The FITS image Stokes axis has the unofficial percentage polarization value." << endl;
		os << "This is not supported.  Will use fractional polarization instead " << endl;
		os << "You must scale the image by 0.01" << LogIO::POST;
		stokes(k) = Stokes::PFlinear;
	    } else if (stokes(k)==8) {
		if (warnStokes) {
		    os << LogIO::SEVERE << "The FITS image Stokes axis has the unofficial spectral index value." << endl;
		    os << "This is not supported. Putting Stokes=Undefined" << LogIO::POST;
		}
		stokes(k) = Stokes::Undefined;
		stokesFITSValue = 8;
	    } else if (stokes(k)==9) {
		if (warnStokes) {
		    os << LogIO::SEVERE << "The Stokes axis has the unofficial optical depth" << endl;
		    os << "value.  This is not supported. Putting Stokes=Undefined" << LogIO::POST;
		}
		stokes(k) = Stokes::Undefined;
		stokesFITSValue = 9;
	    } else {
		Stokes::StokesTypes type = Stokes::fromFITSValue(stokes(k));
		if (type == Stokes::Undefined) {
		    os << LogIO::SEVERE << "A Stokes coordinate of " << stokes(k) 
		       << " was detected; this is not valid. Putting Stokes=Undefined" << endl;
		}
		stokes(k) = type;
	    }
	}

// Now make StokesCoordinate

	try {
	    coord = StokesCoordinate(stokes);
	} catch (std::exception& x) {
	    errMsg = x.what();
	    return false;
	} 
//
	return true;
    }



    ObsInfo FITSCoordinateUtil::getObsInfo (LogIO& os, RecordInterface& header,
					    const ::wcsprm& wcs) const
    {
	ObsInfo oi;
   
// Observer and Telescope are in the FITS cards record.

	Vector<String> error;
	oi.fromFITS (error, header);

// Now overwrite the date info from the wcs struct

	String timeSysStr("UTC");
	if (header.isDefined("timesys")) {
	    Record subRec = header.asRecord("timesys");
	    timeSysStr = subRec.asString("value");
	}
//
	MEpoch::Types timeSystem;
	MEpoch::getType (timeSystem, timeSysStr);

// The date information is in the WCS structure
// 'mjdobs' takes precedence over 'dateobs'

	bool mjdIsDefined = !undefined(wcs.mjdobs);
	bool dateObsDefined = wcs.dateobs[0]!='\0';
	if (mjdIsDefined) {
	    double mjdObs = wcs.mjdobs;
//
	    MEpoch dateObs(Quantum<double>(mjdObs,"d"), timeSystem);
	    oi.setObsDate (dateObs);
	} else if (dateObsDefined) {
	    //      String dateObsStr(wcs.dateobs[0]);
	    String dateObsStr(wcs.dateobs);
	    MVTime time; 
	    if (FITSDateUtil::fromFITS(time, timeSystem, dateObsStr, timeSysStr)) {
		oi.setObsDate(MEpoch(time.get(), timeSystem));
	    } else {
		os << LogIO::NORMAL << "Failed to decode DATE-OBS & TIMESYS keywords - no date set" << LogIO::POST;
	    }
	}

// Remove fields from record

	Vector<String> cards = ObsInfo::keywordNamesFITS();
	for (uint32_t i=0; i<cards.nelements(); i++) {
	    if (header.isDefined(cards(i))) header.removeField(cards[i]);
	}
//
	return oi;
    }



    Vector<String> FITSCoordinateUtil::cTypeFromDirection(
    	bool& isNCP, const Projection& proj,
    	const Vector<String>& axisNames,
    	double refLat, bool printError
    ) {
    	//
    	// RefLat in radians
    	//
    	{
    		DirectionCoordinate dc(
    			MDirection::J2000, proj,
    			0, refLat, 1e-5, -1e-5,
    			Matrix<double>::identity(2), 0, 0
    		);
    		isNCP = dc.isNCP();
    	}
    	return cTypeFromDirection(proj, axisNames, printError);
    }

    Vector<String> FITSCoordinateUtil::cTypeFromDirection (
    	const Projection& proj, const Vector<String>& axisNames, bool printError
    ) {
	LogIO os(LogOrigin("FITSCoordinateUtil", "cTypeFromDirection", WHERE));
	Vector<String> ctype(2);

	for (uint32_t i=0; i<2; i++) {
	    String name = axisNames(i);
	    while (name.length() < 4) {
		name += "-";
	    }
	    switch(proj.type()) {
		// Zenithal/Azimuthal perspective.
	    case Projection::AZP:
		// Slant zenithal perspective, new
	    case Projection::SZP:
		// Gnomonic.
	    case Projection::TAN: 
		// Stereographic.
	    case Projection::STG: 
		// zenith/azimuthal equidistant.
	    case Projection::ARC: 
		// zenithal/azimuthal polynomial.
	    case Projection::ZPN: 
		// zenithal/azimuthal equal area.
	    case Projection::ZEA: 
		// Airy.
	    case Projection::AIR: 
		// Cylindrical perspective.
	    case Projection::CYP: 
		// Plate carree
	    case Projection::CAR: 
		// Mercator.
	    case Projection::MER: 
		// Cylindrical equal area.
	    case Projection::CEA:
		// Conic perspective.
	    case Projection::COP: 
		// Conic equidistant.
	    case Projection::COD: 
		// Conic equal area.
	    case Projection::COE: 
		// Conic orthomorphic.
	    case Projection::COO: 
		// Bonne.
	    case Projection::BON: 
		// Polyconic.
	    case Projection::PCO: 
		// Sanson-Flamsteed (global sinusoidal).
		// The old GLS projection is now SFL. The 'GLS'
		// string will be converted to 'SFL'
	    case Projection::SFL: 
		// Parabolic.
	    case Projection::PAR: 
		// Hammer-Aitoff.
	    case Projection::AIT: 
		// Mollweide.
	    case Projection::MOL: 
		// COBE quadrilateralized spherical cube.
	    case Projection::CSC: 
		// Quadrilateralized spherical cube.
	    case Projection::QSC:
		// Tangential spherical cube.
	    case Projection::TSC:
		// HEALPix grid, new
	    case Projection::HPX: 
		// Orthographics/synthesis.
	    case Projection::SIN:
		name = name + "-" + proj.name();
		break;       
	    default:
		if (i == 0) {
	       
// Only print the message once for long/lat
         
		    if (printError) {  
			os << LogIO::WARN << proj.name()
			   << " is not known to standard FITS (it is known to WCS)."
			   << LogIO::POST;
		    }
		}
		name = name + "-" + proj.name();
		break;
	    }
	    ctype(i) = name;
	}
	return ctype;
    }


    void FITSCoordinateUtil::setWCS (::wcsprm& wcs) const
    {
        Coordinate::set_wcs(wcs);
    }


    bool FITSCoordinateUtil::getCDFromHeader(Matrix<double>& cd, uint32_t n, const RecordInterface& header) 
    //
    // We have to read the CDj_i cards and ultimately pack them into the 
    // WCS linprm structure in the right order.  
    // The expected order in WCS linprm is 
    //
    //  lin.pc = {CD1_1, CD1_2, CD2_1, CD2_2}  ...
    //
    // You can get this via
    //
    //  pc[2][2] = {{CD1_1, CD1_2},
    //              {CD2_1, CD2_2}}
    //
    // which is to say,
    //
    //  pc[0][0] = CD1_1,
    //  pc[0][1] = CD1_2,
    //  pc[1][0] = CD2_1,
    //  pc[1][1] = CD2_2,
    //
    // for which the storage order is
    //
    //  CD1_1, CD1_2, CD2_1, CD2_2
    //
    // so linprm will be happy if you set 
    //
    //  lin.pc = *pc;
    //
    // This packing and unpacking actually happens in
    // LinearXform::set_linprm and LinearXform::pc
    //
    // as we stuff the CD matrix inro the PC matrix 
    // and set cdelt = 1 deg
    //
    {
	cd.resize(n,n);
	cd = 0.0;
	cd.diagonal() = 1.0;
//
	for (uint32_t i=0; i<n; i++) {
	    for (uint32_t j=0; j<n; j++) {
		ostringstream oss;
		oss << "cd" << j+1 << "_" << i+1;
		String field(oss);
		if (header.isDefined(field)) {         
		    header.get(field, cd(i,j));
		} else {
		    cd.resize(0,0);
		    return false;
		}
	    }
	}
	return true;
    }


    void FITSCoordinateUtil::getPCFromHeader(LogIO& os, int32_t& rotationAxis, 
					     Matrix<double>& pc, 
					     uint32_t n, 
					     const RecordInterface& header,
					     const String& sprefix)
    {
	if (header.isDefined("pc")) {

// Unlikely to encounter this, as the current WCS papers
// use the CD rather than PC matrix. The Casacore user binding
// (Image tool) does not allow the WCS definition to be written
// so probably we could remove this

	    if (header.isDefined(sprefix + "rota")) {
		os << "Ignoring redundant " << sprefix << "rota in favour of "
		    "pc matrix." << LogIO::NORMAL << LogIO::POST;
	    }
	    header.get("pc", pc);
	    if (pc.ncolumn() != pc.nrow()) {
		os << "The PC matrix must be square" << LogIO::EXCEPTION;
	    }
	} else if (header.isDefined(sprefix + "rota")) {
	    Vector<double> crota;
	    header.get(sprefix + "rota", crota);

// Turn crota into PC matrix

	    pc.resize(crota.nelements(), crota.nelements());
	    pc = 0.0;
	    pc.diagonal() = 1.0;

// We can only handle one non-zero angle

	    for (uint32_t i=0; i<crota.nelements(); i++) {
		if (!casacore::near(crota(i), 0.0)) {
		    if (rotationAxis >= 0) {
			os << LogIO::SEVERE << "Can only convert one non-zero"
			    " angle from " << sprefix << 
			    "rota to pc matrix. Using the first." << LogIO::POST;
		    } else {
			rotationAxis = i;
		    }
		}
	    }
//
	    if (rotationAxis >= 0 && pc.nrow() > 1) { // can't rotate 1D!
		if (rotationAxis > 0) {
		    pc(rotationAxis-1,rotationAxis-1) =
			pc(rotationAxis,rotationAxis) = cos(crota(rotationAxis)*C::pi/180.0);
		    pc(rotationAxis-1,rotationAxis)=
			-sin(crota(rotationAxis)*C::pi/180.0);
		    pc(rotationAxis,rotationAxis-1)=
			sin(crota(rotationAxis)*C::pi/180.0);
		} else {
		    os << LogIO::NORMAL << "Unusual to rotate about first"
			" axis." << LogIO::POST;
		    pc(rotationAxis+1,rotationAxis+1) =
			pc(rotationAxis,rotationAxis) = cos(crota(rotationAxis)*C::pi/180.0);

// Assume sign of rotation is correct although its not on the expected axis (AIPS convention)

		    pc(rotationAxis,rotationAxis+1)=-sin(crota(rotationAxis)*C::pi/180.0);
		    pc(rotationAxis+1,rotationAxis)= sin(crota(rotationAxis)*C::pi/180.0);
		}
	    }
	} else {

// Pure diagonal PC matrix

	    pc.resize(n, n);
	    pc = 0.0;
	    pc.diagonal() = 1.0;
	}
    }


    void FITSCoordinateUtil::cardsToRecord (LogIO& os, RecordInterface& rec, char* pHeader) const
    //
    // Convert the fitshdr struct to a Casacore Record for ease of later use
    //
    {

// Specific keywords to be located 

	const uint32_t nKeyIds = 0;
	::fitskeyid keyids[1];

// Parse the header

	// sanitize to avoid segfaults in fitshdr
	bool crlfwarned = false;
	for(uint32_t i=0; i<strlen(pHeader)-1; i++){
	    uint32_t headerchar = pHeader[i];
	    if( (headerchar==10) || (headerchar==13) ){
	        if( ! crlfwarned ){
		    os << LogIO::WARN << "HEADER contains LF and/or CR characters!" << LogIO::POST;
		    os << LogIO::WARN << "Will try to replace them by spaces ...." << LogIO::POST;
		    crlfwarned = true;
		}
		pHeader[i] = 32;
	    }
	}

	int nCards = strlen(pHeader) / 80;
	int nReject;
	::fitskey* keys;

	int status = fitshdr (pHeader, nCards, nKeyIds, keyids, &nReject, &keys);
	
	if (status != 0) {
	    throw(AipsError("Failed to extract non-coordinate cards from FITS header"));
	}
	
//
	for (int32_t i=0; i<nCards; i++) {
	    Record subRec;
//
	    String name(keys[i].keyword);
	    name.downcase();
//
	    int type = abs(keys[i].type);
	    switch (type) {
	    case 0:
	    {
		break;                              // No key value (e.g. HISTORY)
	    }
	    case 1:                                 // Logical
	    {
		bool value(keys[i].keyvalue.i > 0);
		subRec.define("value", value);
		break;
	    }
	    case 2:                                 // 32-bit int32_t
	    {
		int32_t value(keys[i].keyvalue.i);       
		subRec.define("value", value);
		break;
	    }
	    case 3:                                 // 64-bit int32_t
	    {
		os << LogIO::WARN << "Cannot yet handle 64-bit Ints; dropping card " << name << LogIO::POST;
		break;
	    }
	    case 4:                                 // Very long integer
	    {
		os << LogIO::WARN << "Cannot yet handle very long Ints; dropping card " << name << LogIO::POST;
		break;
	    }
	    case 5:                                 // Floating point
	    {
		double value(keys[i].keyvalue.f);
		subRec.define("value", value);
		break;
	    }
	    case 6:                                 // Integer and floating complex
	    case 7:
	    {
		Complex value(keys[i].keyvalue.c[0],keys[i].keyvalue.c[1]);
		subRec.define("value", value);
		break;
	    }
	    case 8:                                 // String
	    {
		String value(keys[i].keyvalue.s);
		subRec.define("value", value);
		break;
	    }
	    default:
	    {
		if (keys[i].type < 0) {
		    os << LogIO::WARN <<  "Failed to extract card " << keys[i].keyword << LogIO::POST;
		}
		break;
	    }
	    }

//      subRec.define("value", String("TEST"));

// If we managed to parse the keyword, then deal with Units and comments.
// Units are in inline comment in the form [m/s] (we strip the [])

	    if (subRec.isDefined("value")) {
		String comment(keys[i].comment);
		if (keys[i].ulen>0) {
		    String unit(comment, 1, keys[i].ulen-2);
		    subRec.define("unit", unit);
		} else {
		    subRec.define("comment", comment);
		}

// Define sub record 

		if (rec.isDefined(name)) {
		    os << LogIO::NORMAL << "Duplicate card '" <<  name << "'in header - only first will be used" << LogIO::POST;
		} else {
		    rec.defineRecord(name, subRec);
		}
	    }
	}
//
	free (keys);
    }


    void FITSCoordinateUtil::fixCoordinate (Coordinate& c, LogIO& os) const
    {
	return;

//
	Vector<double> cdelt = c.increment();
	Vector<double> crval = c.referenceValue();
//
	const uint32_t n = cdelt.nelements();
	Coordinate::Type type = c.type();
	String sType = c.showType();
//
	for (uint32_t i=0; i<n; i++) {
	    if (casacore::near(cdelt(i),0.0)) {
		if (type==Coordinate::DIRECTION) {
		    cdelt[i] = C::pi/180.0;        // 1 deg
		    os << LogIO::WARN << "Zero increment in coordinate of type " << sType << " setting  to 1 deg" << LogIO::POST;
		} else {
		    cdelt[i] = crval[i] * 0.1;
		    os << LogIO::WARN << "Zero increment in coordinate of type " << sType << " setting  to refVal/10" << LogIO::POST;
		}
	    }  
	}
//
	c.setIncrement(cdelt);
    }

} //# NAMESPACE CASACORE - END

