//# PagedImage.cc: defines the PagedImage class
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2003
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

#ifndef IMAGES_PAGEDIMAGE_TCC
#define IMAGES_PAGEDIMAGE_TCC

#include <casacore/images/Images/PagedImage.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/images/Regions/RegionHandlerTable.h>
#include <casacore/images/Images/ImageInfo.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/lattices/Lattices/LatticeNavigator.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/PagedArrIter.h>
#include <casacore/lattices/LEL/LatticeExprNode.h>
#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/lattices/LRegions/LatticeRegion.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Logging/LogMessage.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/LogiArray.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableLock.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableInfo.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Quanta/UnitMap.h>

#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T> 
PagedImage<T>::PagedImage (const TiledShape& shape, 
			   const CoordinateSystem& coordinateInfo, 
			   Table& table, uInt rowNumber)
: ImageInterface<T>(RegionHandlerTable(getTable, this)),
  map_p         (shape, table, "map", rowNumber),
  regionPtr_p   (0)
{
  attach_logtable();
  AlwaysAssert(setCoordinateInfo(coordinateInfo), AipsError);
  setTableType();
}

template <class T> 
PagedImage<T>::PagedImage (const TiledShape& shape, 
			   const CoordinateSystem& coordinateInfo, 
			   const String& filename, 
			   TableLock::LockOption lockMode,
			   uInt rowNumber)
: ImageInterface<T>(RegionHandlerTable(getTable, this)),
  regionPtr_p   (0)
{
  makePagedImage (shape, coordinateInfo, filename,
		  TableLock(lockMode), rowNumber);
}

template <class T> 
PagedImage<T>::PagedImage (const TiledShape& shape, 
			   const CoordinateSystem& coordinateInfo, 
			   const String& filename, 
			   const TableLock& lockOptions,
			   uInt rowNumber)
: ImageInterface<T>(RegionHandlerTable(getTable, this)),
  regionPtr_p   (0)
{
  makePagedImage (shape, coordinateInfo, filename, lockOptions, rowNumber);
}


template <class T> 
void PagedImage<T>::makePagedImage (const TiledShape& shape, 
				    const CoordinateSystem& coordinateInfo, 
				    const String& filename, 
				    const TableLock& lockOptions,
				    uInt rowNumber)
{
  SetupNewTable newtab (filename, TableDesc(), Table::New);
  Table tab(newtab, lockOptions);
  map_p = PagedArray<T> (shape, tab, "map", rowNumber);
  attach_logtable();
  AlwaysAssert(setCoordinateInfo(coordinateInfo), AipsError);
  setTableType();
}

template <class T> 
PagedImage<T>::PagedImage (const TiledShape& shape, 
			   const CoordinateSystem& coordinateInfo, 
			   const String& filename, 
			   uInt rowNumber)
: ImageInterface<T>(RegionHandlerTable(getTable, this)),
  regionPtr_p   (0)
{
  SetupNewTable newtab (filename, TableDesc(), Table::New);
  Table tab(newtab);
  map_p = PagedArray<T> (shape, tab, "map", rowNumber);
  attach_logtable();
  AlwaysAssert(setCoordinateInfo(coordinateInfo), AipsError);
  setTableType();
}

template <class T> 
PagedImage<T>::PagedImage (Table& table, MaskSpecifier spec, uInt rowNumber)
: ImageInterface<T>(RegionHandlerTable(getTable, this)),
  map_p         (table, "map", rowNumber),
  regionPtr_p   (0)
{
  attach_logtable();
  restoreAll (table.keywordSet());
  applyMaskSpecifier (spec);
}

template <class T> 
PagedImage<T>::PagedImage (const String& filename, MaskSpecifier spec,
			   uInt rowNumber)
: ImageInterface<T>(RegionHandlerTable(getTable, this)),
  regionPtr_p   (0)
{
  Table tab(filename);
  map_p = PagedArray<T>(tab, "map", rowNumber);
  attach_logtable();
  restoreAll (tab.keywordSet());
  applyMaskSpecifier (spec);
}

template <class T> 
PagedImage<T>::PagedImage (const String& filename,
			   const TableLock& lockOptions,
			   MaskSpecifier spec, uInt rowNumber)
: ImageInterface<T>(RegionHandlerTable(getTable, this)),
  regionPtr_p   (0)
{
  makePagedImage (filename, lockOptions, spec, rowNumber);
}

template <class T> 
PagedImage<T>::PagedImage (const String& filename,
			   TableLock::LockOption lockMode,
			   MaskSpecifier spec, uInt rowNumber)
: ImageInterface<T>(RegionHandlerTable(getTable, this)),
  regionPtr_p   (0)
{
  makePagedImage (filename, TableLock(lockMode), spec, rowNumber);
}

template <class T> 
void PagedImage<T>::makePagedImage (const String& filename,
				    const TableLock& lockOptions,
				    const MaskSpecifier& spec,
				    uInt rowNumber)
{
  Table tab(filename, lockOptions);
  map_p = PagedArray<T>(tab, "map", rowNumber);
  attach_logtable();
  restoreAll (tab.keywordSet());
  applyMaskSpecifier (spec);
}

template <class T> 
PagedImage<T>::PagedImage (const PagedImage<T>& other)
: ImageInterface<T>(other),
  map_p            (other.map_p),
  regionPtr_p      (0)
{
  if (other.regionPtr_p != 0) {
    regionPtr_p = new LatticeRegion (*other.regionPtr_p);
  }
}

template <class T> 
PagedImage<T>::~PagedImage()
{
  // Close the logger here in case the image table is going to be deleted.
  delete regionPtr_p;
  logger().tempClose();
}

template <class T> 
PagedImage<T>& PagedImage<T>::operator=(const PagedImage<T>& other)
{
  if (this != &other) {
    ImageInterface<T>::operator= (other);
    map_p = other.map_p;
    delete regionPtr_p;
    regionPtr_p = 0;
    if (other.regionPtr_p != 0) {
      regionPtr_p = new LatticeRegion (*other.regionPtr_p);
    }
  } 
  return *this;
}

template <class T> 
ImageInterface<T>* PagedImage<T>::cloneII() const
{
  return new PagedImage<T> (*this);
}

template <class T>
void PagedImage<T>::restoreAll (const TableRecord& rec)
{
  // Restore the coordinates.
  CoordinateSystem* restoredCoords = CoordinateSystem::restore(rec, "coords");
  AlwaysAssert(restoredCoords != 0, AipsError);
  setCoordsMember (*restoredCoords);
  delete restoredCoords;
  // Restore the image info.
  restoreImageInfo (rec);
  // Restore the units.
  restoreUnits (rec);
  // Restore the miscinfo.
  restoreMiscInfo (rec);
}

template<class T>
String PagedImage<T>::imageType() const
{
  return className();
}

template<class T>
Bool PagedImage<T>::isPersistent() const
{
  return True;
}

template<class T>
Bool PagedImage<T>::isPaged() const
{
  return True;
}

template <class T> 
Bool PagedImage<T>::isWritable() const
{
  return map_p.isWritable();
}

template<class T>
void PagedImage<T>::reopenRW()
{
  //# First reopen if needed.
  map_p.reopen();
  //# Open for write if not done yet and if writable.
  if (!table().isWritable()  &&  isWritable()) {
    table().reopenRW();
  }
}

template<class T>
Bool PagedImage<T>::hasPixelMask() const
{
  return (regionPtr_p != 0  &&  regionPtr_p->hasMask());
}

template<class T>
const Lattice<Bool>& PagedImage<T>::pixelMask() const
{
  if (regionPtr_p == 0) {
    throw (AipsError ("PagedImage::pixelMask - no pixelmask used"));
  }
  return *regionPtr_p;
}
template<class T>
Lattice<Bool>& PagedImage<T>::pixelMask()
{
  if (regionPtr_p == 0) {
    throw (AipsError ("PagedImage::pixelMask - no pixelmask used"));
  }
  return *regionPtr_p;
}

template<class T>
const LatticeRegion* PagedImage<T>::getRegionPtr() const
{
  return regionPtr_p;
}

template<class T>
void PagedImage<T>::setDefaultMask (const String& regionName)
{
  // Reopen for write when needed and possible.
  reopenRW();
  // Use the new region as the image's mask.
  applyMask (regionName);
  // Store the new default name.
  ImageInterface<T>::setDefaultMask (regionName);
}

template <class T> 
void PagedImage<T>::useMask (MaskSpecifier spec)
{
  applyMaskSpecifier (spec);
}

template<class T>
void PagedImage<T>::applyMaskSpecifier (const MaskSpecifier& spec)
{
  // Use default mask if told to do so.
  // If it does not exist, use no mask.
  String name = spec.name();
  if (spec.useDefault()) {
    name = getDefaultMask();
    if (! hasRegion (name, RegionHandler::Masks)) {
      name = String();
    }
  }
  applyMask (name);
}

template<class T>
void PagedImage<T>::applyMask (const String& maskName)
{
  // No region if no mask name is given.
  if (maskName.empty()) {
    delete regionPtr_p;
    regionPtr_p = 0;
    return;
  }
  // Reconstruct the ImageRegion object.
  // Turn the region into lattice coordinates.
  ImageRegion* regPtr = getImageRegionPtr (maskName, RegionHandler::Masks);
  LatticeRegion* latReg = new LatticeRegion
                          (regPtr->toLatticeRegion (coordinates(), shape()));
  delete regPtr;
  // The mask has to cover the entire image.
  if (latReg->shape() != shape()) {
    delete latReg;
    throw (AipsError ("PagedImage::setDefaultMask - region " + maskName +
		      " does not cover the full image"));
  }
  // Replace current by new mask.
  delete regionPtr_p;
  regionPtr_p = latReg;
}


template <class T> 
void PagedImage<T>::rename (const String& newName)
{
  table().rename (newName, Table::New);
}

template <class T> 
String PagedImage<T>::name (Bool stripPath) const 
{
  return map_p.name (stripPath);
}

template <class T> 
uInt PagedImage<T>::rowNumber() const
{
  return map_p.rowNumber();
}

template <class T> 
IPosition PagedImage<T>::shape() const
{
  return map_p.shape();
}

template<class T> 
void PagedImage<T>::resize (const TiledShape& newShape)
{
  if (newShape.shape().nelements() != coordinates().nPixelAxes()) {
    throw(AipsError("PagedImage<T>::resize: coordinate info is "
		    "the incorrect shape."));
  }
  map_p.resize (newShape);
}

template <class T> 
Bool PagedImage<T>::setCoordinateInfo (const CoordinateSystem& coords)
{
  Bool ok = ImageInterface<T>::setCoordinateInfo(coords);
  if (ok) {
    reopenRW();
    Table& tab = table();
    if (tab.isWritable()) {
      // Update the coordinates
      if (tab.keywordSet().isDefined("coords")) {
	tab.rwKeywordSet().removeField("coords");
      }
      if (!(coordinates().save(tab.rwKeywordSet(), "coords"))) {
        LogIO os;
	os << LogIO::SEVERE << "Error saving coordinates in image " << name()
           << LogIO::POST;
	ok = False;
      }
    } else {
      LogIO os;
      os << LogIO::SEVERE << "Image " << name()
         << " is not writable; not saving coordinates"
         << LogIO::POST;
    }
  }
  return ok;
}

  
template <class T> 
Bool PagedImage<T>::doGetSlice(Array<T>& buffer, const Slicer& theSlice)
{
  return map_p.doGetSlice(buffer, theSlice);
}

template <class T> 
void PagedImage<T>::doPutSlice(const Array<T>& sourceBuffer, 
                               const IPosition& where, 
                               const IPosition& stride)
{
    //  if (throughmask_p || !mask_p) {
  map_p.putSlice(sourceBuffer,where,stride);
    //  } else if (mask_p) {
    //    Array<T> map;
    //Array<Bool> mask;
    //IPosition shape(sourceBuffer.shape());
    //mask_p->getSlice(mask, where, shape, stride, True);
    //map_p.getSlice(map, where, shape, stride, True);
    // use maskedarrays to do all the work.
    //map(mask==False) = sourceBuffer;
    //map_p.putSlice(map,where,stride);
    //  } else {
    //    throw(AipsError("PagedImage<T>::putSlice - throughmask==False but no "
    //		    "mask exists."));
    //  }
}


// apply a function to all elements of the map
template <class T> 
void PagedImage<T>::apply(T (*function)(T)) 
{
    map_p.apply(function);
}

// apply a function to all elements of a const map;
template <class T> 
void PagedImage<T>::apply(T (*function)(const T&))
{
    map_p.apply(function);
}

template <class T> 
void PagedImage<T>::apply(const Functional<T,T>& function)
{
    map_p.apply(function);
}


template <class T> 
T PagedImage<T>::getAt(const IPosition& where) const
{
   return map_p(where);
}

template <class T> 
void PagedImage<T>::putAt(const T& value, const IPosition& where) {
    map_p.putAt (value, where);
}

template<class T> 
void PagedImage<T>::restoreMiscInfo (const TableRecord& rec)
{
  if (rec.isDefined("miscinfo")  &&
      rec.dataType("miscinfo") == TpRecord) {
    setMiscInfoMember (rec.asRecord ("miscinfo"));
  }
}

template<class T> 
Bool PagedImage<T>::setMiscInfo (const RecordInterface& newInfo)
{
  setMiscInfoMember (newInfo);
  reopenRW();
  Table& tab = table();
  if (! tab.isWritable()) {
    return False;
  }
  if (tab.keywordSet().isDefined("miscinfo")) {
    tab.rwKeywordSet().removeField("miscinfo");
  }
  tab.rwKeywordSet().defineRecord("miscinfo", newInfo);
  return True;
}

template <class T> 
LatticeIterInterface<T>* PagedImage<T>::makeIter
                                   (const LatticeNavigator& navigator,
				    Bool useRef) const
{
  return map_p.makeIter (navigator, useRef);
}

template <class T> 
Bool PagedImage<T>::ok() const
{
  Int okay = (map_p.ndim() == coordinates().nPixelAxes());
  return okay  ?  True : False;
}


template <class T> 
PagedImage<T>& PagedImage<T>::operator+= (const Lattice<T>& other)
{
  check_conformance(other);
  // Use LEL, so a mask is also handled.
  LatticeExpr<T> expr(*this + other);
  this->copyData (expr);
  return *this;
}


template<class T> 
void PagedImage<T>::attach_logtable()
{
  open_logtable();
}

template<class T> 
void PagedImage<T>::open_logtable()
{
  // Open logtable as readonly if main table is not writable.
  Table& tab = table();
  setLogMember (LoggerHolder (name() + "/logtable", tab.isWritable()));
  // Insert the keyword if possible and if it does not exist yet.
  if (tab.isWritable()  &&  ! tab.keywordSet().isDefined ("logtable")) {
    tab.rwKeywordSet().defineTable("logtable", Table(name() + "/logtable"));
  }
}

template<class T> 
Bool PagedImage<T>::setUnits(const Unit& newUnits) 
{
  setUnitMember (newUnits);
  reopenRW();
  Table& tab = table();
  if (! tab.isWritable()) {
    return False;
  }
  if (tab.keywordSet().isDefined("units")) {
    tab.rwKeywordSet().removeField("units");
  }
  tab.rwKeywordSet().define("units", newUnits.getName());
  return True;
}

template<class T> 
void PagedImage<T>::restoreUnits (const TableRecord& rec)
{
  Unit retval;
  String unitName;
  if (rec.isDefined("units")) {
    if (rec.dataType("units") != TpString) {
      LogIO os;
      os << LogOrigin("PagedImage<T>", "units()", WHERE)
	 << "'units' keyword in image table is not a string! Units not restored." 
         << LogIO::SEVERE << LogIO::POST;
    } else {
      rec.get("units", unitName);
    }
  }
  if (! unitName.empty()) {
    // OK, non-empty unit, see if it's valid, if not try some known things to
    // make a valid unit out of it.
    if (! UnitVal::check(unitName)) {
      // Beam and Pixel are the most common undefined units
      UnitMap::putUser("Pixel",UnitVal(1.0),"Pixel unit");
      UnitMap::putUser("Beam",UnitVal(1.0),"Beam area");
    }
    if (! UnitVal::check(unitName)) {
      // OK, maybe we need FITS
      UnitMap::addFITS();
    }
    if (!UnitVal::check(unitName)) {
      // I give up!
      LogIO os;
      UnitMap::putUser(unitName, UnitVal(1.0, UnitDim::Dnon), unitName);
      os << LogIO::WARN << "FITS unit \"" << unitName
         << "\" unknown to CASA - will treat it as non-dimensional."
	 << LogIO::POST;
      retval.setName(unitName);
      retval.setValue(UnitVal(1.0, UnitDim::Dnon));
    } else {
      retval = Unit(unitName);
    }
  }
  setUnitMember (retval);
}


template<class T> 
void PagedImage<T>::removeRegion (const String& name,
				  RegionHandler::GroupType type,
				  Bool throwIfUnknown)
{
  reopenRW();
  // Remove the default mask if it is the region to be removed.
  if (name == getDefaultMask()) {
    setDefaultMask (String());
  }
  ImageInterface<T>::removeRegion (name, type, throwIfUnknown);
}


template<class T> 
void PagedImage<T>::check_conformance(const Lattice<T>& other)
{
  if (! this->conform(other)) {
    throw AipsError("Shapes of image " + name() + " and other lattice do not conform");
  }
}

template<class T> 
uInt PagedImage<T>::maximumCacheSize() const
{
  return map_p.maximumCacheSize();
}

template<class T> 
void PagedImage<T>::setMaximumCacheSize(uInt howManyPixels)
{
  map_p.setMaximumCacheSize(howManyPixels);
  if (regionPtr_p != 0) {
    regionPtr_p->setMaximumCacheSize(howManyPixels);
  }
}

template<class T> 
void PagedImage<T>::setCacheSizeFromPath(const IPosition& sliceShape, 
    	                                 const IPosition& windowStart,
                                         const IPosition& windowLength,
                                         const IPosition& axisPath)
{
  map_p.setCacheSizeFromPath(sliceShape, windowStart, windowLength, axisPath);
  if (regionPtr_p != 0) {
    regionPtr_p->setCacheSizeFromPath(sliceShape, windowStart,
				      windowLength, axisPath);
  }
}

template<class T>
void PagedImage<T>::setCacheSizeInTiles (uInt howManyTiles)  
{  
  map_p.setCacheSizeInTiles (howManyTiles);
  if (regionPtr_p != 0) {
    regionPtr_p->setCacheSizeInTiles (howManyTiles);
  }
}


template<class T> 
void PagedImage<T>::clearCache()
{
  map_p.clearCache();
  if (regionPtr_p != 0) {
    regionPtr_p->clearCache();
  }
}

template<class T> 
void PagedImage<T>::showCacheStatistics(ostream& os) const
{
  os << "Pixel statistics : ";
  map_p.showCacheStatistics(os);
  if (regionPtr_p != 0) {
    os << "Pixelmask statistics : ";
    regionPtr_p->showCacheStatistics(os);
  }
}

template<class T> 
uInt PagedImage<T>::advisedMaxPixels() const
{
  return map_p.advisedMaxPixels();
}

template<class T> 
IPosition PagedImage<T>::doNiceCursorShape(uInt maxPixels) const
{
  return map_p.niceCursorShape(maxPixels);
}

template<class T> 
void PagedImage<T>::setTableType()
{
  TableInfo& info(table().tableInfo());
  const String reqdType = info.type(TableInfo::PAGEDIMAGE);
  if (info.type() != reqdType) {
    info.setType(reqdType);
  }
  const String reqdSubType = info.subType(TableInfo::PAGEDIMAGE);
  if (info.subType() != reqdSubType) {
    info.setSubType(reqdSubType);
  }
}


template<class T>
Table& PagedImage<T>::getTable (void* imagePtr, Bool writable)
{
  PagedImage<T>* im = static_cast<PagedImage<T>*>(imagePtr);
  if (writable) {
    im->reopenRW();
  }
  return im->map_p.table();
}

template<class T>
Bool PagedImage<T>::lock (FileLocker::LockType type, uInt nattempts)
{
  return map_p.lock (type, nattempts);
}
template<class T>
void PagedImage<T>::unlock()
{
  map_p.unlock();
  logger().unlock();
  if (regionPtr_p != 0) {
    regionPtr_p->unlock();
  }
}
template<class T>
Bool PagedImage<T>::hasLock (FileLocker::LockType type) const
{
  return map_p.hasLock (type);
}

template<class T>
void PagedImage<T>::resync()
{
  map_p.resync();
  logger().resync();
  if (regionPtr_p != 0
  &&  !regionPtr_p->hasLock (FileLocker::Read)) {
    regionPtr_p->resync();
  }
}

template<class T>
void PagedImage<T>::flush()
{
  itsAttrHandler.flush();
  map_p.flush();
  logger().flush();
  if (regionPtr_p != 0) {
    regionPtr_p->flush();
  }
}

template<class T>
void PagedImage<T>::tempClose()
{
  map_p.tempClose();
  logger().tempClose();
  if (regionPtr_p != 0) {
    regionPtr_p->tempClose();
  }
}

template<class T>
void PagedImage<T>::reopen()
{
  map_p.reopen();
  if (regionPtr_p != 0) {
    regionPtr_p->reopen();
  }
}

template<class T>
Bool PagedImage<T>::setImageInfo (const ImageInfo& info) 
{
  // Set imageinfo in base class.
  Bool ok = ImageInterface<T>::setImageInfo(info);
  if (ok) {
    // Make persistent in table keywords.
    reopenRW();
    Table& tab = table();
    if (tab.isWritable()) {
      // Delete existing one if there.
      if (tab.keywordSet().isDefined("imageinfo")) {
	tab.rwKeywordSet().removeField("imageinfo");
      }
      // Convert info to a record and save as keyword.
      TableRecord rec;
      String error;
      if (imageInfo().toRecord(error, rec)) {
         tab.rwKeywordSet().defineRecord("imageinfo", rec);
      } else {
        // Could not convert to record.
        LogIO os;
	os << LogIO::SEVERE << "Error saving ImageInfo in image " << name()
           << "; " << error << LogIO::POST;
	ok = False;
      }
    } else {
      // Table not writable.
      LogIO os;
      os << LogIO::SEVERE
         << "Image " << name() << " is not writable; not saving ImageInfo"
         << LogIO::POST;
    }
  }
  return ok;
}

template<class T>
void PagedImage<T>::restoreImageInfo (const TableRecord& rec)
{
  if (rec.isDefined("imageinfo")) {
    String error;
    ImageInfo info;
    Bool ok = info.fromRecord (error, rec.asRecord("imageinfo"));
    if (ok) {
      setImageInfoMember (info);
    } else {
      LogIO os;
      os << LogIO::WARN << "Failed to restore the ImageInfo in image " << name()
         << "; " << error << LogIO::POST;
    }
  }
}

template<class T>
ImageAttrHandler& PagedImage<T>::attrHandler (Bool createHandler)
{
  return itsAttrHandler.attachTable (table(), createHandler);
}

} //# NAMESPACE CASACORE - END

#endif
