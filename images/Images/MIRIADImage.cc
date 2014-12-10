//# MIRIADImage.cc: Class providing native access to MIRIAD images
//# Copyright (C) 2001,2002,2003
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

#include <casacore/images/Images/MIRIADImage.h>

#include <casacore/images/Images/ImageInfo.h>
#include <casacore/images/Images/MaskSpecifier.h>
#include <casacore/images/Images/ImageOpener.h>
#include <casacore/lattices/Lattices/TiledShape.h>
#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/tables/DataMan/TiledFileAccess.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>

#include <casacore/coordinates/Coordinates/Coordinate.h>
#include <casacore/coordinates/Coordinates/LinearCoordinate.h>
#include <casacore/coordinates/Coordinates/DirectionCoordinate.h>
#include <casacore/coordinates/Coordinates/SpectralCoordinate.h>
#include <casacore/coordinates/Coordinates/TabularCoordinate.h>
#include <casacore/coordinates/Coordinates/StokesCoordinate.h>
#include <casacore/coordinates/Coordinates/Projection.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/Quanta/UnitMap.h>
#include <casacore/casa/Utilities/CountedPtr.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/fits/FITS/FITSSpectralUtil.h>

#include <casacore/casa/iostream.h>

#include <casacore/mirlib/maxdimc.h>
#include <casacore/mirlib/miriad.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//   set this to 1 or 0 to benchmark tiled access vs. xyio(native miriad) access
#define USE_TILE  1

MIRIADImage::MIRIADImage (const String& name)
: ImageInterface<Float>(),
  name_p      (name),
  pPixelMask_p(0),
  hasBlanks_p (False),
  dataType_p  (TpOther),
  fileOffset_p(0),
  isClosed_p  (True)
{
   setup();
}

MIRIADImage::MIRIADImage (const String& name, const MaskSpecifier& maskSpec)
: ImageInterface<Float>(),
  name_p      (name),
  maskSpec_p  (maskSpec),
  pPixelMask_p(0),
  hasBlanks_p (False),
  dataType_p  (TpOther),
  fileOffset_p(0),
  isClosed_p  (True)
{
   setup();
}

MIRIADImage::MIRIADImage (const MIRIADImage& other)
: ImageInterface<Float>(other),
  name_p      (other.name_p),
  maskSpec_p  (other.maskSpec_p),
  unit_p      (other.unit_p),
  rec_p       (other.rec_p),
  pTiledFile_p(other.pTiledFile_p),
  pPixelMask_p(0),
  shape_p     (other.shape_p),
  hasBlanks_p (other.hasBlanks_p),
  dataType_p  (other.dataType_p),
  fileOffset_p(other.fileOffset_p),
  isClosed_p  (other.isClosed_p)
{
   if (other.pPixelMask_p != 0) {
      pPixelMask_p = other.pPixelMask_p->clone();
   }
}

MIRIADImage& MIRIADImage::operator=(const MIRIADImage& other)
// 
// Assignment. Uses reference semantics
//
{
   if (this != &other) {
      ImageInterface<Float>::operator= (other);
//
      pTiledFile_p = other.pTiledFile_p;             // Counted pointer
//
      delete pPixelMask_p;
      pPixelMask_p = 0;
      if (other.pPixelMask_p != 0) {
	 pPixelMask_p = other.pPixelMask_p->clone();
      }
//
      shape_p     = other.shape_p;
      name_p      = other.name_p;
      maskSpec_p  = other.maskSpec_p;
      unit_p      = other.unit_p;
      rec_p       = other.rec_p;
      hasBlanks_p = other.hasBlanks_p;
      dataType_p  = other.dataType_p;
      fileOffset_p= other.fileOffset_p;
      isClosed_p  = other.isClosed_p;
   }
   return *this;
} 
 
MIRIADImage::~MIRIADImage()
{
  delete pPixelMask_p;
}


LatticeBase* MIRIADImage::openMIRIADImage (const String& name,
					   const MaskSpecifier& spec)
{
  return new MIRIADImage (name, spec);
}

void MIRIADImage::registerOpenFunction()
{
  ImageOpener::registerOpenImageFunction (ImageOpener::MIRIAD,
					  &openMIRIADImage);
}


ImageInterface<Float>* MIRIADImage::cloneII() const
{
   return new MIRIADImage (*this);
}


String MIRIADImage::imageType() const
{
   return "MIRIADImage";
}

Bool MIRIADImage::isMasked() const
{
   return hasBlanks_p;
}

const LatticeRegion* MIRIADImage::getRegionPtr() const
{
   return 0;
}

IPosition MIRIADImage::shape() const  
{
   return shape_p.shape();
}

uInt MIRIADImage::advisedMaxPixels() const
{
   return shape_p.tileShape().product();
}

IPosition MIRIADImage::doNiceCursorShape (uInt) const  
{
   return shape_p.tileShape();
}

void MIRIADImage::resize(const TiledShape&)
{
   throw (AipsError ("MIRIADImage::resize - a MIRIADImage is not writable"));
}

Bool MIRIADImage::doGetSlice(Array<Float>& buffer,
                           const Slicer& section)
{
   reopenIfNeeded();
   pTiledFile_p->get (buffer, section);
   return False;                            // Not a reference
} 
   

void MIRIADImage::doPutSlice (const Array<Float>&, const IPosition&,
                            const IPosition&)
{
   throw (AipsError ("MIRIADImage::putSlice - "
		     "is not possible yet as MIRIADImage is not writable"));
}
#if 0
Bool MIRIADImage::setUnits (const Unit& unit)
{  
   unit_p = unit;
   return True;
}
   
Unit MIRIADImage::units() const
{  
   return unit_p;
}
#endif

String MIRIADImage::name (Bool stripPath) const
{
   Path path(name_p);
   if (stripPath) {
      return path.baseName();
   } else {
      return path.absoluteName();
   }
}


const RecordInterface& MIRIADImage::miscInfo() const
{
   return rec_p;
}
 

Bool MIRIADImage::setMiscInfo(const RecordInterface& rec)
{ 
   rec_p = rec;
   return True;
}
 
Bool MIRIADImage::isPersistent() const
{
  return True;
}

Bool MIRIADImage::isPaged() const
{
  return True;
}

Bool MIRIADImage::isWritable() const
{  
// Its too hard to implement putMaskSlice becuase
// magic blanking is used. It means we lose
// the data values if the mask is put somewhere

   return False;
}


Bool MIRIADImage::ok() const
{
   return True;
}  

Bool MIRIADImage::doGetMaskSlice (Array<Bool>& buffer, const Slicer& section)
{
   if (!hasBlanks_p) {
      buffer.resize (section.length());
      buffer = True;
      return False;
   }
//
   reopenIfNeeded();
   return pPixelMask_p->getSlice (buffer, section);
}


Bool MIRIADImage::hasPixelMask() const
{
   return hasBlanks_p;
}  

const Lattice<Bool>& MIRIADImage::pixelMask() const
{
   if (!hasBlanks_p) {
      throw (AipsError ("MIRIADImage::pixelMask - no pixelmask used"));
   }
   return *pPixelMask_p;
}

Lattice<Bool>& MIRIADImage::pixelMask()
{
   if (!hasBlanks_p) {
      throw (AipsError ("MIRIADImage::pixelMask - no pixelmask used"));
   }
   return *pPixelMask_p;
}

void MIRIADImage::tempClose()
{
   if (! isClosed_p) {
      delete pPixelMask_p;
      pTiledFile_p = 0;
      isClosed_p = True;
   }
}

void MIRIADImage::reopen()
{
   if (isClosed_p) {
      open();
   }
}

uInt MIRIADImage::maximumCacheSize() const
{
   reopenIfNeeded();
   return pTiledFile_p->maximumCacheSize() / ValType::getTypeSize(dataType_p);
}

void MIRIADImage::setMaximumCacheSize (uInt howManyPixels)
{
   reopenIfNeeded();
   const uInt sizeInBytes = howManyPixels * ValType::getTypeSize(dataType_p);
   pTiledFile_p->setMaximumCacheSize (sizeInBytes);
}

void MIRIADImage::setCacheSizeFromPath (const IPosition& sliceShape, 
				      const IPosition& windowStart,
				      const IPosition& windowLength,
				      const IPosition& axisPath)
{
   reopenIfNeeded();
   pTiledFile_p->setCacheSize (sliceShape, windowStart,
			       windowLength, axisPath);
}

void MIRIADImage::setCacheSizeInTiles (uInt howManyTiles)  
{  
   reopenIfNeeded();
   pTiledFile_p->setCacheSize (howManyTiles);
}


void MIRIADImage::clearCache()
{
   if (! isClosed_p) {
      pTiledFile_p->clearCache();
   }
}

void MIRIADImage::showCacheStatistics (ostream& os) const
{
   reopenIfNeeded();
   os << "MIRIADImage statistics : ";
   pTiledFile_p->showCacheStatistics (os);
}



void MIRIADImage::setup()
{
   if (name_p.empty()) {
      throw AipsError("MIRIADImage: given file name is empty");
   }
   if (! maskSpec_p.name().empty()) {
      throw AipsError("MIRIADImage " + name_p + " has no named masks");
   }
   Path path(name_p);
   String fullName = path.absoluteName();

   // cerr << "MIRIAD::setup name=" << fullName << endl;

// Fish things out of the MIRIAD file

   CoordinateSystem cSys;
   IPosition shape;
   ImageInfo imageInfo;
   Unit brightnessUnit;

   getImageAttributes(cSys, shape, imageInfo, brightnessUnit, rec_p, 
                      hasBlanks_p, fullName);

   // set ImageInterface data

   setCoordsMember (cSys);
   setImageInfoMember (imageInfo);

   setUnitMember(brightnessUnit);

   // We need to put the history in some memory based LogSink
   // setLogMember(logSink);

   // Set MIRIADImage data

   unit_p = brightnessUnit;

   // MIRIAD 'image' items have an offset of 4 bytes if address directly via tiles

   fileOffset_p = 4;
   dataType_p = TpFloat;   // Miriad uses only 32bit IEEE floating point

   // See if there is a mask

   hasBlanks_p = False;    // for now....

   // Form the tile shape
   shape_p = TiledShape (shape, TiledFileAccess::makeTileShape(shape));

   // Open the image.
   open();
}


void MIRIADImage::open()
{
   Bool writable = False;
   Bool canonical = True;    
   String iname = name_p + "/image";    // fails for very small miriad images !!

   // The tile shape must not be a subchunk in all dimensions

   pTiledFile_p = new TiledFileAccess(iname, fileOffset_p,
				      shape_p.shape(), shape_p.tileShape(),
                                      dataType_p, TSMOption(),
				      writable, canonical);

   // Shares the pTiledFile_p pointer. 

   if (hasBlanks_p) {
     // pPixelMask_p = new Lattice<Bool>;
     // pPixelMask_p.resize(shape_p.shape());
   }


   // Okay, it is open now.

   isClosed_p = False;
}


void MIRIADImage::getImageAttributes (CoordinateSystem& cSys,
                                      IPosition& shape, ImageInfo& imageInfo,
                                      Unit& brightnessUnit, Record&, 
                                      Bool& hasBlanks, const String& name)
{
  LogIO os(LogOrigin("MIRIADImage", "getImageAttributes", WHERE));
  int naxis = MAXNAX, axes[MAXNAX];              // see miriad's maxdimc.h
  int i, ndim;
  // Projection projn;
  // Vector<Double>   projp;
  // Projection::Type ptype;
  Double offset = 1.0;                           // miriad crpix 'origin' is 1-based
  Int rotationAxis = -1;
  

  xyopen_c(&tno_p, const_cast<char *>(name.chars()), "old", naxis, axes);    // open miriad file
  rdhdi_c(tno_p,"naxis",&ndim,0);                // for convenience, get ndim

#if 0
  // DEBUG: output what size cube we found
  cerr << "MIRIAD::getImageAttributes: [";
  for (i=0; i<ndim; i++) {
    if (i > 0) cerr << "x";
    cerr << axes[i] ;
  }
  cerr <<  "]" << endl;
#endif

  //    crackHeader(cSys, shape, imageInfo, brightnessUnit, miscInfo, os);
  hasBlanks = FALSE;

  shape.resize(ndim);
  for (Int i=0; i<ndim; i++) shape(i) = axes[i];
  
  // get a coordinate system. MIRIAD is pretty simple,  it only knows
  // 'rectangular' coordinate systems, with the usual astronomical conventions
  // most of this code has been grabbed from CoordinateSystem::fromFITSHeader

  Vector<Double> cdelt, crval, crpix;
  Vector<Int> naxes;
  Vector<String> ctype;
  Matrix<Double> pc(2,2);
  String tmps, digit;
  char tmps64[64];
  
  cdelt.resize(ndim);
  crval.resize(ndim);
  ctype.resize(ndim);
  crpix.resize(ndim);
  
  // Units : miriad uses 'bunit' FITS like units without case cares.

  rdhda_c(tno_p, "bunit", tmps64,"",64);
  String cunit = tmps64;
  UnitMap::addFITS();
  if (UnitVal::check(cunit)) {
     brightnessUnit = UnitMap::fromFITS(Unit(cunit));
  } else {
     Unit t;
     brightnessUnit = t;
     os << "FITS unit " << cunit << " unknown to CASA - ignoring." << LogIO::POST;
  }

  // get the miriad axes descriptors
  for (i=0; i<ndim; i++) {
      tmps = "ctype" + digit.toString(i+1);
      rdhda_c(tno_p,const_cast<char*>(tmps.chars()), tmps64, "", 64);
      ctype(i) = tmps64;
      // cerr << tmps << "=>" << ctype(i) << endl;

      tmps = "crval" + digit.toString(i+1);
      rdhdd_c(tno_p,const_cast<char *>(tmps.chars()), &crval(i), 0.0);
      // cerr << tmps << "=>" << crval(i) << endl;

      tmps = "cdelt" + digit.toString(i+1);
      rdhdd_c(tno_p,const_cast<char *>(tmps.chars()), &cdelt(i), 0.0);
      // cerr << tmps << "=>" << cdelt(i) << endl;

      tmps = "crpix" + digit.toString(i+1);
      rdhdd_c(tno_p,const_cast<char*>(tmps.chars()), &crpix(i), 0.0);
      crpix(i) -= offset;
      // cerr << tmps << "=>" << crpix(i) << endl;
  }

  Int longAxis=-1, latAxis=-1, stokesAxis=-1, spectralAxis=-1;

  for (i=0; i<ndim; i++) {
        String subRA(ctype(i).at(0,2));
        String subDEC(ctype(i).at(0,3));
	if (subRA==String("RA") || ctype(i).contains("LON") || subRA==String("LL")) {
	    if (longAxis >= 0) {
		os << LogIO::SEVERE << "More than one longitude axis is "
		    "present in header!";
		// return False;
	    }
	    longAxis = i;
	} else if (subDEC==String("DEC") || ctype(i).contains("LAT") || subDEC.contains("MM")) {
	    if (latAxis >= 0) {
		os << LogIO::SEVERE << "More than one latitude axis is "
		    "present in header!";
		// return False; // we already have a latitude axis!
	    }
	    latAxis = i;
	} else if (ctype(i).contains("STOKES")) {
	    stokesAxis = i;
	} else if (ctype(i).contains("FREQ") || 
		   ctype(i).contains("FELO") ||
		   ctype(i).contains("VELO")) {
	    spectralAxis = i;
	}
  }

  // We must have longitude AND latitude
  // (really, what about certain PV diagrams ???)

  if (longAxis >= 0 && latAxis < 0) {
	os << LogIO::SEVERE << "We have a longitude axis but no latitude axis!";
	// return False; 
  }
  if (latAxis >= 0 && longAxis < 0) {
	os << LogIO::SEVERE << "We have a latitude axis but no longitude axis!";
	// return False; 
  }

  // DIRECTION

  String proj1, proj2;
  Bool isGalactic = False;
  if (longAxis >= 0) {
    proj1 = ctype(longAxis);
    proj2 = ctype(latAxis);

    if (proj1.contains("GLON")) isGalactic = True;

    // Get rid of the first 4 characters, e.g., RA--

    const Int l1 = proj1.length();
    const Int l2 = proj2.length();
    proj1 = String(proj1.at(4, l1-4));
    proj2 = String(proj2.at(4, l2-4));

    // Get rid of leading -'s

    proj1.gsub(Regex("^-*"), String(""));
    proj2.gsub(Regex("^-*"), String(""));
    
    // Get rid of spaces

    proj1.gsub(Regex(" *"), String(""));   
    proj2.gsub(String(" "), String(""));

    if (proj1=="" && proj2=="") {

      // We must abandon making a celestial coordinate if there is no
      // projection.  Defaulting to cartesian is the wrong thing to do
      // We must make a Linear Coordinate from it.
      
      os << WHERE << LogIO::WARN << 
	"No projection has been defined so cannot make a Celestial Coordinate\n"
	"from this miriad header.  Will make a LinearCoordinate instead" << LogIO::POST;
      longAxis = -1;
      latAxis = -1;
    }
  }

  if (longAxis >= 0) {
    if (proj1 != proj2) {

      // Maybe instead I should switch to CAR, or use the first?

      os << LogIO::SEVERE << "Longitude and latitude axes have different"
	" projections (" << proj1 << "!=" << proj2 << ")" << LogIO::POST;
      // return False;
    }

    // OK, let's make our Direction coordinate and add it to the
    // coordinate system. We'll worry about transposing later. MIRIAD
    // uses radians by convention
       
    // First, work out what the projection actually is.
    // Special case NCP - now SIN with  parameters
       
    Vector<Double>   projp;
    Projection::Type ptype;

    ptype =  Projection::SIN;
	
    if (proj1 == "NCP") {
	 os << LogIO::NORMAL << "NCP projection is now SIN projection in"
		" WCS.\nmiriad readers will not handle this correctly." <<
	        LogIO::POST;
	 ptype = Projection::SIN;
	 projp.resize(2);

	 // According to Greisen and Calabretta

	 projp(0) = 0.0;
	 projp(1) = 1.0/tan(crval(latAxis));
    } else {
	 ptype = Projection::type(proj1);
	 if (ptype == Projection::N_PROJ) {
	   os << LogIO::SEVERE << "Unknown projection: (" << proj1 << ")";
	   //return False;
	 }
	 // projp header keyword not used in miriad
    }
    
    // OK, now try making the projection
    
    Projection projn;

    try {
      projn = Projection(ptype, projp);
    } catch (AipsError x) {
      os << LogIO::SEVERE << "Error forming projection, maybe the "
	"wrong number of parameters\n(" << x.getMesg() << ")" << 
	LogIO::POST;
      //return False;
    } 

    // fish out LONG/LATPOLE  (use defaults, since miriad does not
    // use those in wcs headers

    Double longPole = 999.0;
    Double latPole = 999.0;
       
    // DEFAULT
       
    MDirection::Types radecsys = MDirection::J2000;
    if (isGalactic) {
	 radecsys = MDirection::GALACTIC;
    } else {
      Double epoch;
      rdhdd_c(tno_p,"epoch", &epoch, 2000.0);
      if (::casacore::near(epoch, 1950.0)) {
	radecsys = MDirection::B1950;
      } else if (::casacore::near(epoch, 2000.0)) {
	radecsys = MDirection::J2000;
      }
    }	

    // make sure this isn't a lie for miriad...
    pc(0,0) = pc(1,1) = 1.0;
    pc(0,1) = pc(1,0) = 0.0;

    Matrix<Double> dirpc(2,2);
    //cerr << "long/lat = " << longAxis << " " << latAxis << endl;
    dirpc(0,0) = pc(longAxis, longAxis);
    dirpc(0,1) = pc(longAxis, latAxis);
    dirpc(1,0) = pc(latAxis, longAxis);
    dirpc(1,1) = pc(latAxis, latAxis);
    
    // watch for cdelt=0 - its okay if that axis is degenerate
    // and (crpix+offset)=1 and rotationAxis < 0 = i.e. the only
    // pixel on that axis is the reference pixel and there is
    // no rotation specified - then cdelt=1 on that axis.  If that
    // isn't done, that coord. can't be constructed because the
    // PC matrix will be reported as singular since its first 
    // multiplied by cdelt before its used and in this case, that
    // doesn't matter since other pixels on that axis are never used.
    
    if (::casacore::near(cdelt(latAxis), 0.0) && 
	::casacore::near(crpix(latAxis)+offset, 1.0) && rotationAxis < 0) {
      cdelt(latAxis) = 1.0;            // degrees
    }
    //
    if (::casacore::near(cdelt(longAxis), 0.0) && 
	::casacore::near(crpix(longAxis)+offset, 1.0) && rotationAxis < 0) {
      cdelt(longAxis) = 1.0;          // degrees
    }

    DirectionCoordinate dir(radecsys,
			    projn,
			    crval(longAxis), crval(latAxis),
			    cdelt(longAxis), cdelt(latAxis),
			    dirpc,
			    crpix(longAxis), crpix(latAxis),
			    longPole, latPole);
    cSys.addCoordinate(dir);
  }

  // potential bug to track down:
  // - a miriad cube that has been processes with 'velsw axis=freq' has slightly
  //   wrong labels when printed with dImageSummary

  if (spectralAxis >= 0) {
    // cerr << "Hey, process spectralAxis = " << spectralAxis << endl;
    //  see       SpectralCoordinate::fromFITS(tmp, error, header, spectralAxis,os);
    //  and       FITSSpectralUtil::fromFITSHeader
    //  so, as opposed to doing it here, it should be done parallel to those places
    //

    Int velref = 2; // Default is optical + topocentric ("OBS")
    if (ctype(spectralAxis).contains("VELO")) {
      velref = 258; // radio + OBS
    }

    // Try to work out OPTICAL/RADIO/. Default to Optical
    String type(ctype(spectralAxis).before(4));
    MDoppler::Types velocityPreference = MDoppler::OPTICAL;
    if (velref > 256) {
      velocityPreference = MDoppler::RADIO;   
    }

    Double restFrequency;
    rdhdd_c(tno_p,"restfreq", &restFrequency, -1.0);
    restFrequency *= 1e9;   // miriad uses GHz

    // convert the velocity frame tag in ctype  to a reference frame
    String spectralAxisQualifier;
    if (ctype(spectralAxis).length() <= 5) {
      spectralAxisQualifier = "";
    } else {
      spectralAxisQualifier = ctype(spectralAxis).after(4);
    }

    Double referenceChannel = crpix(spectralAxis);
    Double referenceFrequency = 0.0;
    Double deltaFrequency = 0.0;
    Vector<Double> frequencies;

    MFrequency::Types refFrame;
    Bool ok = FITSSpectralUtil::frameFromTag(refFrame, 
					spectralAxisQualifier, 
						velref);

    if (!ok) {
      if (spectralAxisQualifier == "") {
	if ((velref%256) >= 0) {
	  // no tag and velref is unrecognized
	  os << LogIO::SEVERE << "Illegal value for VELREF("
		 << velref << 
	    ") assuming topocentric" << LogIO::POST;
	}
      } else {
	// unrecognized tag
	os << LogIO::SEVERE << "Unknown spectral reference frame " 
	       << spectralAxisQualifier << 
	  ". Assuming topocentric." << LogIO::POST;
      }
    }

    Int nChan = shape(spectralAxis);
    Double delt = cdelt(spectralAxis);
    Double rval = crval(spectralAxis);
    Double rpix = crpix(spectralAxis);
    
    if (ctype(spectralAxis).contains("FREQ")) {
      delt *= 1e9;
      rval *= 1e9;
      referenceFrequency = rval;
      deltaFrequency = delt;
      frequencies.resize(nChan);
      for (Int i=0; i<nChan; i++) {
	frequencies(i) = referenceFrequency + (Double(i)-referenceChannel)*delt;
      }
      if (restFrequency<0) restFrequency = 0.0;
    } else if (ctype(spectralAxis).contains("FELO")) {
      delt *= 1e3;
      rval *= 1e3;
      if (restFrequency < 0) {
	os << LogIO::SEVERE << "FELO axis does not have rest frequency "
	  "information (RESTFREQ)" << LogIO::POST;
	// return False;
      } else {
	// Have RESTFREQ, deduce freq's from velocities and rest freq
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
	    os << LogIO::SEVERE << "FELO/RADIO is illegal" <<
	      LogIO::POST;
	    // return False;
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
      delt *= 1e3;
      rval *= 1e3;
      if (restFrequency < 0) {
	os << LogIO::SEVERE << "VELO axis does not have rest frequency "
	  "information (RESTFREQ)" << LogIO::POST;
	// return False;
      } else {
	// Have RESTFREQ
	os << LogIO::NORMAL << "ALTRVAL and ALTRPIX have not been "
	  "supplied in the FITS header, so I \nwill deduce the "
	  "frequencies from the velocities and rest frequency." << 
	  LogIO::POST;
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
	    os << LogIO::SEVERE << 
	      "VELO/OPTICAL is not implemented" <<LogIO::POST;
	    // return False;
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
    } else {   // catch VELO/FELO/FREQ/....
      AlwaysAssert(0, AipsError); // NOTREACHED
    }
    // SpectralCoordinate::fromFITS
    SpectralCoordinate tmp(refFrame, referenceFrequency, deltaFrequency, 
			   referenceChannel, restFrequency);
    cSys.addCoordinate(tmp);
  }


  // STOKES.   shape is used only here as the StokesCoordinate
  // is a bit peculiar, and not really separable from the shape

  if (stokesAxis >= 0) {
      if (shape(stokesAxis)>4) {
	os << "Stokes axis longer than 4 pixels.  This is not acceptable" 
	   << LogIO::EXCEPTION;       
	//return False;
      }
      Vector<Int> stokes(shape(stokesAxis)); 
      
      for (Int k=0; k<shape(stokesAxis); k++) {
	
	// crpix is 0-relative
	
	Double tmp = crval(stokesAxis) + 
		(k - crpix(stokesAxis))*cdelt(stokesAxis);
	
	// cerr << "Stokes: tmp = " << tmp << endl;
	if (tmp >= 0) {
	  stokes(k) = Int(tmp + 0.01);
	} else {
	  stokes(k) = Int(tmp - 0.01);
	}

        switch (stokes(k)) {
        case -8: 
	   stokes(k) = Stokes::YX; 
	   break;
        case -7: 
	   stokes(k) = Stokes::XY; 
	   break;
        case -6: 
	   stokes(k) = Stokes::YY; 
	   break;
        case -5: 
	   stokes(k) = Stokes::XX; 
	   break;
        case -4: 
	   stokes(k) = Stokes::LR; 
	   break;
        case -3: 
	   stokes(k) = Stokes::RL; 
	   break;
        case -2: 
	   stokes(k) = Stokes::LL; 
	   break;
        case -1: 
	   stokes(k) = Stokes::RR; 
	   break;
        case 0:
	   {
              os << LogIO::WARN
                 << "Detected Stokes coordinate = 0; this is an unoffical" << endl;
              os << "Convention for an image containing a beam.  Putting Stokes=Undefined" << endl;
              os << "Better would be to write your FITS image with the correct Stokes" << LogIO::POST;
              stokes(k) = Stokes::Undefined;
              break;
	   }
	case 1:  
	   {
	     stokes(k) = Stokes::I; 
	     break;
	   }
	 case 2: 
	   stokes(k) = Stokes::Q; 
	   break;
	 case 3: 
	   stokes(k) = Stokes::U; 
	   break;
	 case 4: 
	   stokes(k) = Stokes::V; 
	   break;
	 case 5:
	   
	   // Percentage linear polarization not properly supported
	   
	   {
	     os << LogIO::SEVERE << "The Stokes axis has the unofficial percentage polarization value." << endl;
	     os << "This is not supported.  Will use fractional polarization instead " << endl;
	     os << "You must scale the image by 0.01" << LogIO::POST;
	     stokes(k) = Stokes::PFlinear;
	     break;
	   }
	 case 6:
	   stokes(k) = Stokes::PFlinear;
	   break;
	 case 7:
	   stokes(k) = Stokes::Pangle;
	   break;
	 case 8:
	   
	   // Spectral index not supported
	   
	   {
              os << LogIO::SEVERE << "The FITS image Stokes axis has the unofficial spectral index value." << endl;
              os << "This is not supported. Putting Stokes=Undefined" << LogIO::POST;
              stokes(k) = Stokes::Undefined;
              break;
	   }
	 case 9:
	   // Optical depth not supported

             {	   
                 os << LogIO::SEVERE << "The Stokes axis has the unofficial optical depth" << endl;
                 os << "value.  This is not supported. Putting Stokes=Undefined" << LogIO::POST;
                 stokes(k) = Stokes::Undefined;
                 break;
             }
	 default:
            {
              os << LogIO::SEVERE << "A Stokes coordinate of " << stokes(k)
                 << " was detected; this is not valid. Putting Stokes=Undefined" << endl;
              stokes(k) = Stokes::Undefined;
            }
	 }
      }
      try {
	StokesCoordinate sc(stokes);
	cSys.addCoordinate(sc);
      } catch (AipsError x) {
	os << LogIO::SEVERE << "Error forming stokes axis : " << x.getMesg() << LogIO::POST;
	//return False;
      } 
  }

// Now we need to work out the transpose order

  Vector<Int> order(ndim);
  Int nspecial = 0;
  if (longAxis >= 0) nspecial++;
  if (latAxis >= 0) nspecial++;
  if (stokesAxis >= 0) nspecial++;
  if (spectralAxis >= 0) nspecial++;
#if 0

  // I can't figure this out now, there is something wrong here for miriad
  Int linused = 0;
  for (i=0; i<ndim; i++) {
    if (i == longAxis) {
      order(i) = 0; // long is always first if it exist
    } else if (i == latAxis) {
      order(i) = 1; // lat is always second if it exists
    } else if (i == stokesAxis) {
      if (longAxis >= 0) { // stokes is axis 0 if no dir, otherwise 2
	order(i) = 3;   // 3 for MIRIAD !!!   2 for fits?
      } else {
	order(i) = 0;
      }
    } else if (i == spectralAxis) {
      if (longAxis >= 0 && stokesAxis >= 0) {
	order(i) = 2; // stokes and dir :  (3 for fits, 2 for miriad?)
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
  cSys.transpose(order, order);

#endif

  // ImageInfo. 

  String btype;
  rdhda_c(tno_p, "btype", tmps64,"",64);
  btype = tmps64;
  ImageInfo::ImageTypes type = ImageInfo::MiriadImageType (btype);
  if (type!=ImageInfo::Undefined) imageInfo.setImageType(type);
//
   Double bmaj, bmin, bpa;
   rdhdd_c(tno_p, "bmaj", &bmaj, 0.0);
   rdhdd_c(tno_p, "bmin", &bmin, 0.0);
   rdhdd_c(tno_p, "bpa", &bpa, 0.0);
   if (bmaj>0.0 && bmin>0.0 && abs(bpa)>0.0) {
      Quantity qbmaj(bmaj,Unit("rad"));
      Quantity qbmin(bmin,Unit("rad"));
      Quantity qbpa(bpa,Unit("deg"));
      imageInfo.setRestoringBeam(GaussianBeam(qbmaj, qbmin, qbpa));
   }

// ObsInfo

  ObsInfo oi;

// DATE-OBS
  Double obstime;
  rdhdd_c(tno_p, "obstime", &obstime, -1.0);
  // cerr << "obstime=" << obstime << endl;
  if (obstime > -1.0) {
     obstime -= 2400000.5;    // make it MJD ("d")
     MVEpoch mve(Quantity(obstime,"d"));
     MEpoch mep(mve,MEpoch::UTC);   // miriad uses JDN (in UTC)   -- no good
     oi.setObsDate(mep);
  }

// TELESCOP

  String telescop;
  rdhda_c(tno_p, "telescop", tmps64,"",64);
  telescop = tmps64;
  if (!telescop.empty()) {
     oi.setTelescope(telescop);
  }

//
  cSys.setObsInfo(oi);
  xyclose_c(tno_p);
}



} //# NAMESPACE CASACORE - END

