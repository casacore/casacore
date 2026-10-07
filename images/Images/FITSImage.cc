// # FITSImage.cc: Class providing native access to FITS images
// # Copyright (C) 2001,2002
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This library is free software; you can redistribute it and/or modify it
// # under the terms of the GNU Library General Public License as published by
// # the Free Software Foundation; either version 2 of the License, or (at your
// # option) any later version.
// #
// # This library is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
// # License for more details.
// #
// # You should have received a copy of the GNU Library General Public License
// # along with this library; if not, write to the Free Software Foundation,
// # Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

#include <casacore/images/Images/FITSImage.h>

#include <casacore/images/Images/FITSImgParser.h>
#include <casacore/fits/FITS/hdu.h>
#include <casacore/fits/FITS/fitsio.h>
#include <casacore/fits/FITS/FITSKeywordUtil.h>
#include <casacore/images/Images/ImageInfo.h>
#include <casacore/images/Images/ImageFITSConverter.h>
#include <casacore/images/Images/MaskSpecifier.h>
#include <casacore/images/Images/ImageOpener.h>
#include <casacore/lattices/Lattices/TiledShape.h>
#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/lattices/LRegions/FITSMask.h>
#include <casacore/tables/DataMan/TiledFileAccess.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/tables/LogTables/LoggerHolder.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>

#include <casacore/casa/iostream.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

FITSImage::FITSImage(const String& name, unsigned int whichRep, unsigned int whichHDU)
    : ImageInterface<float>(),
      name_p(name),
      fullname_p(name),
      scale_p(1.0),
      offset_p(0.0),
      shortMagic_p(0),
      uCharMagic_p(0),
      longMagic_p(0),
      hasBlanks_p(false),
      dataType_p(TpOther),
      fileOffset_p(0),
      isClosed_p(true),
      filterZeroMask_p(false),
      whichRep_p(whichRep),
      whichHDU_p(whichHDU),
      _hasBeamsTable(false) {
  setup();
}

FITSImage::FITSImage(const String& name, const MaskSpecifier& maskSpec, unsigned int whichRep,
                     unsigned int whichHDU)
    : ImageInterface<float>(),
      name_p(name),
      fullname_p(name),
      maskSpec_p(maskSpec),
      scale_p(1.0),
      offset_p(0.0),
      shortMagic_p(0),
      uCharMagic_p(0),
      longMagic_p(0),
      hasBlanks_p(false),
      dataType_p(TpOther),
      fileOffset_p(0),
      isClosed_p(true),
      filterZeroMask_p(false),
      whichRep_p(whichRep),
      whichHDU_p(whichHDU),
      _hasBeamsTable(false) {
  setup();
}

FITSImage::FITSImage(const FITSImage& other)
    : ImageInterface<float>(other),
      name_p(other.name_p),
      fullname_p(other.fullname_p),
      maskSpec_p(other.maskSpec_p),
      pTiledFile_p(other.pTiledFile_p),
      shape_p(other.shape_p),
      scale_p(other.scale_p),
      offset_p(other.offset_p),
      shortMagic_p(other.shortMagic_p),
      uCharMagic_p(other.uCharMagic_p),
      longMagic_p(other.longMagic_p),
      hasBlanks_p(other.hasBlanks_p),
      dataType_p(other.dataType_p),
      fileOffset_p(other.fileOffset_p),
      isClosed_p(other.isClosed_p),
      filterZeroMask_p(other.filterZeroMask_p),
      whichRep_p(other.whichRep_p),
      whichHDU_p(other.whichHDU_p),
      _hasBeamsTable(other._hasBeamsTable)

{
  if (other.pPixelMask_p) {
    pPixelMask_p.reset(other.pPixelMask_p->clone());
  }
}

FITSImage& FITSImage::operator=(const FITSImage& other)
//
// Assignment. Uses reference semantics
//
{
  if (this != &other) {
    ImageInterface<float>::operator=(other);
    //
    pTiledFile_p = other.pTiledFile_p;  // shared pointer
                                        //
    pPixelMask_p.reset();
    if (other.pPixelMask_p) {
      pPixelMask_p.reset(other.pPixelMask_p->clone());
    }
    //
    shape_p = other.shape_p;
    name_p = other.name_p;
    fullname_p = other.fullname_p;
    maskSpec_p = other.maskSpec_p;
    scale_p = other.scale_p;
    offset_p = other.offset_p;
    shortMagic_p = other.shortMagic_p;
    uCharMagic_p = other.uCharMagic_p;
    longMagic_p = other.longMagic_p;
    hasBlanks_p = other.hasBlanks_p;
    dataType_p = other.dataType_p;
    fileOffset_p = other.fileOffset_p;
    isClosed_p = other.isClosed_p;
    filterZeroMask_p = other.filterZeroMask_p;
    whichRep_p = other.whichRep_p;
    whichHDU_p = other.whichHDU_p;
    _hasBeamsTable = other._hasBeamsTable;
  }
  return *this;
}

LatticeBase* FITSImage::openFITSImage(const String& name, const MaskSpecifier& spec) {
  return new FITSImage(name, spec);
}

void FITSImage::registerOpenFunction() {
  ImageOpener::registerOpenImageFunction(ImageOpener::FITS, &openFITSImage);
}

//
String FITSImage::get_fitsname(const String& fullname) {
  String fullname_l;
  String fitsname;
  int close_bracepos, open_bracepos, fullname_length;

  fullname_l = fullname;
  TrimInPlace(fullname_l);
  fullname_length = fullname_l.length();

  // cerr << "Initial name: " << fullname_l << endl;
  //  check whether the strings ends with "]"
  if (fullname_l.compare(fullname_length - 1, 1, "]", 1)) {
    // check for an open brace
    open_bracepos = fullname_l.rfind("[", fullname_length);
    if (open_bracepos > 0) {
      // check for a closing brace
      close_bracepos = fullname_l.rfind("]", fullname_length);

      // an open brace at the end indicates a mal-formed name
      if (close_bracepos < 0 || (open_bracepos > close_bracepos))
        throw(AipsError(fullname_l + " has opening brace, but no closing brace."));
    }

    // just copy the input
    fitsname = fullname_l;
  } else {
    // check for the last "["
    open_bracepos = fullname_l.rfind("[", fullname_length);
    if (open_bracepos < 0) {
      throw(AipsError(fullname_l + " has closing brace, but no opening brace."));
    } else {
      // separate the filename an the extension name
      // extexpr_p = String(fullname_l, open_bracepos+1, fullname_length-open_bracepos-2);
      fitsname = String(fullname_l, 0, open_bracepos);
    }
  }
  // cerr << "FITS name: " << fitsname <<endl;

  return fitsname;
}

//
unsigned int FITSImage::get_hdunum(const String& fullname) {
  String extname = String("");

  String fullname_l;
  String fitsname;
  String extstring;
  int fullname_length, comma_pos;

  int extver = -1;
  int extindex = -1;
  int fitsindex = -1;
  unsigned int hduindex = 0;

  fullname_l = fullname;
  TrimInPlace(fullname_l);
  fullname_length = fullname_l.length();

  // determine the FITS name
  fitsname = FITSImage::get_fitsname(fullname_l);

  // check whether there is an extension
  // specification in the full name
  if (fitsname != fullname_l) {
    // isolate the extension specification
    extstring = String(fullname_l, fitsname.length() + 1, fullname_length - fitsname.length() - 2);

    // check for the comma
    comma_pos = extstring.rfind(",", extstring.length());
    if (comma_pos < 0) {
      TrimInPlace(extstring);

      // check whether an index is given
      if (int value; StringToValue<int>(extstring, value, false)) {
        // get the index
        extindex = value;
      }
      // explicitly check for the literal "0"
      else if (!extstring.compare(0, 1, "0", 1)) {
        extindex = 0;
      } else {
        // just copy the extension name
        extname = extstring;
      }
    } else {
      // separate the extension name
      extname = String(extstring, 0, comma_pos);

      // find the extension version
      const bool correctly_parsed = StringToValue<int>(
          String(extstring, comma_pos + 1, extstring.length() - 1), extver, false);

      if (!correctly_parsed) {
        throw(AipsError(String(extstring, comma_pos + 1, extstring.length() - 1) +
                        " Extension version not an integer"));
        // cerr << "Extension version not an Integer: " << String(extstring, comma_pos+1,
        // extstring.length()-1)<< endl; exit(0);
      } else if (extver < 0) {
        throw(AipsError(extstring + " Extension version must be >0."));
        // cerr << "Extension version must be >0: " << extver << endl;
        // exit(0);
      }
    }
    // make it pretty
    TrimInPlace(extname);
    ToUpperCaseInPlace(extname);
  }

  // cerr << "Opening image parser with: "<< fitsname <<endl;
  FITSImgParser fip = FITSImgParser(fitsname);

  if (extname.length() > 0 || extindex > -1) {
    FITSExtInfo fei = FITSExtInfo(fip.fitsname(true), extindex, extname, extver, true);
    fitsindex = fip.get_index(fei);
    if (fitsindex > -1)
      hduindex = (unsigned int)fitsindex;
    else
      throw(AipsError("Extension " + extstring + " does not exist in " + fitsname));
  } else {
    hduindex = fip.get_firstdata_index();
    if (hduindex > 1 || hduindex == fip.get_numhdu())
      throw(AipsError("No data in the zeroth or first extension of " + fitsname));
  }

  // return the index
  return hduindex;
}

ImageInterface<float>* FITSImage::cloneII() const { return new FITSImage(*this); }

String FITSImage::imageType() const { return className(); }

String FITSImage::className() {
  static const std::string x = "FITSImage";
  return x;
}

bool FITSImage::isMasked() const { return hasBlanks_p; }

const LatticeRegion* FITSImage::getRegionPtr() const { return nullptr; }

IPosition FITSImage::shape() const { return shape_p.shape(); }

unsigned int FITSImage::advisedMaxPixels() const { return shape_p.tileShape().product(); }

IPosition FITSImage::doNiceCursorShape(unsigned int) const { return shape_p.tileShape(); }

void FITSImage::resize(const TiledShape&) {
  throw(AipsError("FITSImage::resize - a FITSImage is not writable"));
}

bool FITSImage::doGetSlice(Array<float>& buffer, const Slicer& section) {
  reopenIfNeeded();
  if (pTiledFile_p->dataType() == TpFloat) {
    pTiledFile_p->get(buffer, section);
  } else if (pTiledFile_p->dataType() == TpDouble) {
    Array<double> tmp;
    pTiledFile_p->get(tmp, section);
    buffer.resize(tmp.shape());
    convertArray(buffer, tmp);
  } else if (pTiledFile_p->dataType() == TpInt) {
    pTiledFile_p->get(buffer, section, scale_p, offset_p, longMagic_p, hasBlanks_p);
  } else if (pTiledFile_p->dataType() == TpShort) {
    pTiledFile_p->get(buffer, section, scale_p, offset_p, shortMagic_p, hasBlanks_p);
  } else if (pTiledFile_p->dataType() == TpUChar) {
    pTiledFile_p->get(buffer, section, scale_p, offset_p, uCharMagic_p, hasBlanks_p);
  }
  return false;  // Not a reference
}

void FITSImage::doPutSlice(const Array<float>&, const IPosition&, const IPosition&) {
  throw(
      AipsError("FITSImage::putSlice - "
                "is not possible as FITSImage is not writable"));
}

String FITSImage::name(bool stripPath) const {
  Path path(name_p);
  if (stripPath) {
    return path.baseName();
  } else {
    return path.absoluteName();
  }
}

bool FITSImage::isPersistent() const { return true; }

bool FITSImage::isPaged() const { return true; }

bool FITSImage::isWritable() const {
  // Its too hard to implement putMaskSlice becuase
  // magic blanking is used. It measn we lose
  // the data values if the mask is put somewhere

  return false;
}

bool FITSImage::ok() const { return true; }

DataType FITSImage::dataType() const { return TpFloat; }

bool FITSImage::doGetMaskSlice(Array<bool>& buffer, const Slicer& section) {
  if (!hasBlanks_p) {
    buffer.resize(section.length());
    buffer = true;
    return false;
  }
  //
  reopenIfNeeded();
  return pPixelMask_p->getSlice(buffer, section);
}

bool FITSImage::hasPixelMask() const { return hasBlanks_p; }

const Lattice<bool>& FITSImage::pixelMask() const {
  if (!hasBlanks_p) {
    throw(AipsError("FITSImage::pixelMask - no pixelmask used"));
  }
  return *pPixelMask_p;
}

Lattice<bool>& FITSImage::pixelMask() {
  if (!hasBlanks_p) {
    throw(AipsError("FITSImage::pixelMask - no pixelmask used"));
  }
  return *pPixelMask_p;
}

void FITSImage::tempClose() {
  if (!isClosed_p) {
    pPixelMask_p.reset();
    pTiledFile_p.reset();
    isClosed_p = true;
  }
}

void FITSImage::reopen() {
  if (isClosed_p) {
    open();
  }
}

unsigned int FITSImage::maximumCacheSize() const {
  reopenIfNeeded();
  return pTiledFile_p->maximumCacheSize() / ValType::getTypeSize(dataType_p);
}

void FITSImage::setMaximumCacheSize(unsigned int howManyPixels) {
  reopenIfNeeded();
  const unsigned int sizeInBytes = howManyPixels * ValType::getTypeSize(dataType_p);
  pTiledFile_p->setMaximumCacheSize(sizeInBytes);
}

void FITSImage::setCacheSizeFromPath(const IPosition& sliceShape, const IPosition& windowStart,
                                     const IPosition& windowLength, const IPosition& axisPath) {
  reopenIfNeeded();
  pTiledFile_p->setCacheSize(sliceShape, windowStart, windowLength, axisPath);
}

void FITSImage::setCacheSizeInTiles(unsigned int howManyTiles) {
  reopenIfNeeded();
  pTiledFile_p->setCacheSize(howManyTiles);
}

void FITSImage::clearCache() {
  if (!isClosed_p) {
    pTiledFile_p->clearCache();
  }
}

void FITSImage::showCacheStatistics(ostream& os) const {
  reopenIfNeeded();
  os << "FITSImage statistics : ";
  pTiledFile_p->showCacheStatistics(os);
}

void FITSImage::setup() {
  // Separate the FITS filename from any
  // possible extension specification

  name_p = get_fitsname(fullname_p);

  // Determine the HDU index from the extension specification
  unsigned int HDUnum = get_hdunum(fullname_p);

  // Compare the HDU index given directly and
  // the one extracted from the name
  if (HDUnum != whichHDU_p) {
    // if an extension information was given,
    // the index extracted from it wins
    if (name_p != fullname_p) {
      whichHDU_p = HDUnum;
    } else {
      // if the index given directly is zero (which means the default),
      // the zeroth index might be emptied and the index retrieved
      // in the method above (which is 1) is used
      if (!whichHDU_p) {
        whichHDU_p = HDUnum;
      }
    }
  }

  if (name_p.empty()) {
    throw AipsError("FITSImage: given file name is empty");
  }
  //
  if (!maskSpec_p.name().empty()) {
    throw AipsError("FITSImage " + name_p + " has no named masks");
  }
  Path path(name_p);
  String fullName = path.absoluteName();

  // Fish things out of the FITS file

  CoordinateSystem cSys;
  IPosition shape;
  ImageInfo imageInfo;
  Unit brightnessUnit;
  int recno;
  int recsize;  // Should be 2880 bytes (unless blocking used)
  FITS::ValueType dataType;
  Record miscInfo;

  // hasBlanks only relevant to Integer images.  Says if 'blank' value defined in header

  getImageAttributes(cSys, shape, imageInfo, brightnessUnit, miscInfo, recsize, recno, dataType,
                     scale_p, offset_p, uCharMagic_p, shortMagic_p, longMagic_p, hasBlanks_p,
                     fullName, whichRep_p, whichHDU_p);
  // shape must be set before image info in cases of multiple beams
  shape_p = TiledShape(shape, TiledFileAccess::makeTileShape(shape));
  setMiscInfoMember(miscInfo);

  // set ImageInterface data

  setCoordsMember(cSys);

  // Set FITSImage data

  setUnitMember(brightnessUnit);

  // By default, ImageInterface makes a memory-based LoggerHolder
  // which is all we need.  We will fill it in later

  // I don't understand why I have to subtract one, as the
  // data should begin in the NEXT record. BobG surmises
  // the FITS classes read ahead...
  // MK: I think there is an additional read() and hence
  // count-up of recno when the file is first accessed and
  // then for every skipped hdu, thats where the "-1 - whichHDU comes from"
  fileOffset_p += (recno - 1 - whichHDU_p) * recsize;
  //
  dataType_p = TpFloat;
  if (dataType == FITS::DOUBLE) {
    dataType_p = TpDouble;
  } else if (dataType == FITS::SHORT) {
    dataType_p = TpShort;
  } else if (dataType == FITS::LONG) {
    dataType_p = TpInt;
  } else if (dataType == FITS::BYTE) {
    dataType_p = TpUChar;
  }

  // See if there is a mask specifier.  Defaults to apply mask.

  if (maskSpec_p.useDefault()) {
    // We would like to use any mask.  For 32 f.p. bit we don't know if there
    // are masked pixels (they are NaNs).  For Integer types we do know if there
    // the magic value has been set (suggests there are masked pixels) and
    // hasBlanks_p was set to T or F by getImageAttributes

    if (dataType_p == TpFloat || dataType_p == TpDouble) hasBlanks_p = true;
  } else {
    // We don't want to use the mask

    hasBlanks_p = false;
  }

  // Open the image.
  open();

  // Finally, read any supported extensions, like a BEAMS table
  if (_hasBeamsTable) {
    ImageFITSConverter::readBeamsTable(imageInfo, fullName, dataType_p);
  }
  setImageInfoMember(imageInfo);
}

void FITSImage::open() {
  bool writable = false;
  bool canonical = true;

  // The tile shape must not be a subchunk in all dimensions

  pTiledFile_p =
      std::make_shared<TiledFileAccess>(name_p, fileOffset_p, shape_p.shape(), shape_p.tileShape(),
                                        dataType_p, TSMOption(), writable, canonical);

  // Shares the pTiledFile_p pointer. Scale factors for integers

  FITSMask* fitsMask = nullptr;
  if (hasBlanks_p) {
    if (dataType_p == TpFloat) {
      fitsMask = new FITSMask(pTiledFile_p.get());
    } else if (dataType_p == TpDouble) {
      fitsMask = new FITSMask(pTiledFile_p.get());
    } else if (dataType_p == TpUChar) {
      fitsMask = new FITSMask(pTiledFile_p.get(), scale_p, offset_p, uCharMagic_p, hasBlanks_p);
    } else if (dataType_p == TpShort) {
      fitsMask = new FITSMask(pTiledFile_p.get(), scale_p, offset_p, shortMagic_p, hasBlanks_p);
    } else if (dataType_p == TpInt) {
      fitsMask = new FITSMask(pTiledFile_p.get(), scale_p, offset_p, longMagic_p, hasBlanks_p);
    }
    if (fitsMask) {
      pPixelMask_p.reset(fitsMask);
      fitsMask->setFilterZero(filterZeroMask_p);
    }
  }

  // Ok, it is open now.

  isClosed_p = false;
}

void FITSImage::getImageAttributes(CoordinateSystem& cSys, IPosition& shape, ImageInfo& imageInfo,
                                   Unit& brightnessUnit, RecordInterface& miscInfo, int& recordsize,
                                   int& recordnumber, FITS::ValueType& dataType, float& scale,
                                   float& offset, unsigned char& uCharMagic, short& shortMagic,
                                   int& longMagic, bool& hasBlanks, const String& name,
                                   unsigned int whichRep, unsigned int whichHDU) {
  LogIO os(LogOrigin("FITSImage", "getImageAttributes", WHERE));
  File fitsfile(name);
  if (!fitsfile.exists() || !fitsfile.isReadable() || !fitsfile.isRegular()) {
    throw(AipsError(name + " does not exist or is not readable"));
  }
  //
  ImageOpener::ImageTypes type = ImageOpener::imageType(name_p);
  if (type != ImageOpener::FITS) {
    throw(AipsError(name + " is not a FITS image"));
  }
  //
  FitsInput infile(fitsfile.path().expandedName().c_str(), FITS::Disk);
  if (infile.err()) {
    throw(AipsError("Cannot open file " + name + " (or other I/O error)"));
  }
  recordsize = infile.fitsrecsize();

  //
  // Advance to the right HDU
  //
  for (unsigned int i = 0; i < whichHDU; i++) {
    infile.skip_hdu();
    if (infile.err()) {
      throw(AipsError("Error advancing to image in file " + name));
    }
    // add the size of the skipped HDU
    // to the fileOffset
    fileOffset_p += infile.getskipsize();
  }

  // Check type
  dataType = infile.datatype();
  if (dataType != FITS::FLOAT && dataType != FITS::DOUBLE && dataType != FITS::SHORT &&
      dataType != FITS::LONG && dataType != FITS::BYTE) {
    throw AipsError("FITS file " + name + " should contain float, double, short or long data");
  }

  //
  // Make sure the current spot in the FITS file is an image
  //
  if (infile.rectype() != FITS::HDURecord ||
      (infile.hdutype() != FITS::PrimaryArrayHDU && infile.hdutype() != FITS::ImageExtensionHDU)) {
    throw(AipsError("No image at specified location in file " + name));
  }

  // Check that the header type fits to the extension number
  if (!whichHDU && infile.hdutype() != FITS::PrimaryArrayHDU) {
    throw(
        AipsError("The first extension of the image must be a PrimaryArray in "
                  "FITS file " +
                  name));
  } else if (whichHDU && infile.hdutype() != FITS::ImageExtensionHDU) {
    throw(
        AipsError("The image must be stored in an ImageExtension of"
                  "FITS file " +
                  name));
  }

  // Crack header
  if (!whichHDU_p) {
    if (dataType == FITS::FLOAT) {
      crackHeader<float>(cSys, shape, imageInfo, brightnessUnit, miscInfo, scale, offset,
                         uCharMagic, shortMagic, longMagic, hasBlanks, os, infile, whichRep);
    } else if (dataType == FITS::DOUBLE) {
      crackHeader<double>(cSys, shape, imageInfo, brightnessUnit, miscInfo, scale, offset,
                          uCharMagic, shortMagic, longMagic, hasBlanks, os, infile, whichRep);
    } else if (dataType == FITS::LONG) {
      crackHeader<int>(cSys, shape, imageInfo, brightnessUnit, miscInfo, scale, offset, uCharMagic,
                       shortMagic, longMagic, hasBlanks, os, infile, whichRep);
    } else if (dataType == FITS::SHORT) {
      crackHeader<short>(cSys, shape, imageInfo, brightnessUnit, miscInfo, scale, offset,
                         uCharMagic, shortMagic, longMagic, hasBlanks, os, infile, whichRep);
    } else if (dataType == FITS::BYTE) {
      crackHeader<unsigned char>(cSys, shape, imageInfo, brightnessUnit, miscInfo, scale, offset,
                                 uCharMagic, shortMagic, longMagic, hasBlanks, os, infile,
                                 whichRep);
    }
  } else {
    if (dataType == FITS::FLOAT) {
      crackExtHeader<float>(cSys, shape, imageInfo, brightnessUnit, miscInfo, scale, offset,
                            uCharMagic, shortMagic, longMagic, hasBlanks, os, infile, whichRep);
    } else if (dataType == FITS::DOUBLE) {
      crackExtHeader<double>(cSys, shape, imageInfo, brightnessUnit, miscInfo, scale, offset,
                             uCharMagic, shortMagic, longMagic, hasBlanks, os, infile, whichRep);
    } else if (dataType == FITS::LONG) {
      crackExtHeader<int>(cSys, shape, imageInfo, brightnessUnit, miscInfo, scale, offset,
                          uCharMagic, shortMagic, longMagic, hasBlanks, os, infile, whichRep);
    } else if (dataType == FITS::SHORT) {
      crackExtHeader<short>(cSys, shape, imageInfo, brightnessUnit, miscInfo, scale, offset,
                            uCharMagic, shortMagic, longMagic, hasBlanks, os, infile, whichRep);
    } else if (dataType == FITS::BYTE) {
      crackExtHeader<unsigned char>(cSys, shape, imageInfo, brightnessUnit, miscInfo, scale, offset,
                                    uCharMagic, shortMagic, longMagic, hasBlanks, os, infile,
                                    whichRep);
    }
  }
  //  }

  // Get recordnumber

  recordnumber = infile.recno();
}

void FITSImage::setMaskZero(bool filterZero) {
  // set the zero masking on the
  // current mask
  if (pPixelMask_p) {
    dynamic_cast<FITSMask*>(pPixelMask_p.get())->setFilterZero(true);
  }
  // set the flag, such that an later
  // mask created in 'open()' will be OK
  // as well
  filterZeroMask_p = filterZero;
}

}  // namespace casacore
