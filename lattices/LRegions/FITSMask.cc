//# FITSMask.cc: an on-the-fly mask for FITS images
//# Copyright (C) 1997,1998,1999,2000,2001,2002
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


#include <casacore/lattices/LRegions/FITSMask.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/tables/DataMan/TiledFileAccess.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Exceptions/Error.h>



namespace casacore { //# NAMESPACE CASACORE - BEGIN

FITSMask::FITSMask (TiledFileAccess* tiledFile)
: itsTiledFilePtr(tiledFile),
  itsScale(1.0),
  itsOffset(0.0),
  itsUCharMagic(0),
  itsShortMagic(0),
  itsLongMagic(0),
  itsHasIntBlanks(false),
  itsFilterZero(false)
{
   AlwaysAssert(itsTiledFilePtr->dataType()==TpFloat ||
                itsTiledFilePtr->dataType()==TpDouble,
                AipsError);
}

FITSMask::FITSMask (TiledFileAccess* tiledFile, float scale, float offset,
                    unsigned char magic, bool hasBlanks)
: itsTiledFilePtr(tiledFile),
  itsScale(scale),
  itsOffset(offset),
  itsUCharMagic(magic),
  itsShortMagic(0),
  itsLongMagic(0),
  itsHasIntBlanks(hasBlanks),
  itsFilterZero(false)
{
   AlwaysAssert(itsTiledFilePtr->dataType()==TpUChar, AipsError);
}

FITSMask::FITSMask (TiledFileAccess* tiledFile, float scale, float offset,
                    int16_t magic, bool hasBlanks)
: itsTiledFilePtr(tiledFile),
  itsScale(scale),
  itsOffset(offset),
  itsUCharMagic(0),
  itsShortMagic(magic),
  itsLongMagic(0),
  itsHasIntBlanks(hasBlanks),
  itsFilterZero(false)
{
   AlwaysAssert(itsTiledFilePtr->dataType()==TpShort, AipsError);
}

FITSMask::FITSMask (TiledFileAccess* tiledFile, float scale, float offset,
                    int32_t magic, bool hasBlanks)
: itsTiledFilePtr(tiledFile),
  itsScale(scale),
  itsOffset(offset),
  itsUCharMagic(0),
  itsShortMagic(0),
  itsLongMagic(magic),
  itsHasIntBlanks(hasBlanks),
  itsFilterZero(false)
{
   AlwaysAssert(itsTiledFilePtr->dataType()==TpInt, AipsError);
}


FITSMask::FITSMask (const FITSMask& other)
: Lattice<bool>(other),
  itsTiledFilePtr(other.itsTiledFilePtr),
  itsScale(other.itsScale),
  itsOffset(other.itsOffset),
  itsUCharMagic(other.itsUCharMagic),
  itsShortMagic(other.itsShortMagic),
  itsLongMagic(other.itsLongMagic),
  itsHasIntBlanks(other.itsHasIntBlanks),
  itsFilterZero(other.itsFilterZero)
{}

FITSMask::~FITSMask()
{}

FITSMask& FITSMask::operator= (const FITSMask& other)
{
  if (this != &other) {
    itsTiledFilePtr = other.itsTiledFilePtr;
    itsBuffer.resize();
    itsBuffer = other.itsBuffer.copy();
    itsScale = other.itsScale;
    itsOffset = other.itsOffset;
    itsUCharMagic = other.itsUCharMagic;
    itsShortMagic = other.itsShortMagic;
    itsLongMagic = other.itsLongMagic;
    itsHasIntBlanks = other.itsHasIntBlanks;
    itsFilterZero = other.itsFilterZero;
  }
  return *this;
}

Lattice<bool>* FITSMask::clone() const
{
  return new FITSMask (*this);
}

bool FITSMask::isWritable() const
{
  return false;
}

IPosition FITSMask::shape() const
{
  return itsTiledFilePtr->shape();
}

bool FITSMask::doGetSlice (Array<bool>& mask, const Slicer& section)
{
   IPosition shp = section.length();
   if (!mask.shape().isEqual(shp)) mask.resize(shp);
   if (!itsBuffer.shape().isEqual(shp)) itsBuffer.resize(shp);
//
   if (itsTiledFilePtr->dataType()==TpFloat) {
      itsTiledFilePtr->get(itsBuffer, section);
   } else if (itsTiledFilePtr->dataType()==TpDouble) {
      Array<double> tmp(shp);
      itsTiledFilePtr->get(tmp, section);
      convertArray(itsBuffer, tmp);
   } else if (itsTiledFilePtr->dataType()==TpInt) {
      itsTiledFilePtr->get(itsBuffer, section, itsScale, itsOffset, 
                           itsLongMagic, itsHasIntBlanks);
   } else if (itsTiledFilePtr->dataType()==TpShort) {
      itsTiledFilePtr->get(itsBuffer, section, itsScale, itsOffset, 
                           itsShortMagic, itsHasIntBlanks);
   } else if (itsTiledFilePtr->dataType()==TpUChar) {
      itsTiledFilePtr->get(itsBuffer, section, itsScale, itsOffset, 
                           itsUCharMagic, itsHasIntBlanks);
   }
//
   bool deletePtrD;
   const float* pData = itsBuffer.getStorage(deletePtrD);
   bool deletePtrM;
   bool* pMask = mask.getStorage(deletePtrM);
//
   // Apply the according filtering
   if (!itsFilterZero) {
     filterNaN(pMask, pData, mask.nelements());
   } else {
     filterZeroNaN(pMask, pData, mask.nelements());
   }
//
   itsBuffer.freeStorage(pData, deletePtrD);
   mask.putStorage(pMask, deletePtrM);
//
   return false;            // Not a reference
}

void FITSMask::filterNaN (bool *pMask, const float *pData, uint32_t nelems)
{
  // loop over all elements
  for (uint32_t i=0; i<nelems; i++) {
    // set defaults;
    // blanked values are NaNs.
    pMask[i] = true;
    if (isNaN(pData[i])) pMask[i] = false;
  }
}

void FITSMask::filterZeroNaN (bool *pMask, const float *pData, uint32_t nelems)
{
  // loop over all elements
  for (uint32_t i=0; i<nelems; i++) {
    // set defaults;
    // blanked values are NaNs and "0.0"
    pMask[i] = true;
    if (isNaN(pData[i]) || pData[i] == (float)0.0) pMask[i] = false;
  }
}

void FITSMask::setFilterZero(bool filterZero)
{
  itsFilterZero = filterZero;
}

void FITSMask::doPutSlice (const Array<bool>&,
                           const IPosition&, 	
                           const IPosition&)
{
   throw(AipsError("FITSMask object is not writable"));
}


} //# NAMESPACE CASACORE - END

