//# PagedImage.cc: defines the PagedImage class
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000
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

#include <trial/Images/PagedImage.h>
#include <trial/Images/ImageRegion.h>
#include <trial/Images/RegionHandlerTable.h>
#include <trial/Images/ImageInfo.h>
#include <aips/Lattices/ArrayLattice.h>
#include <aips/Lattices/LatticeNavigator.h>
#include <aips/Lattices/LatticeStepper.h>
#include <aips/Lattices/LatticeIterator.h>
#include <aips/Lattices/PagedArrIter.h>
#include <trial/Lattices/LatticeExprNode.h>
#include <trial/Lattices/LatticeExpr.h>
#include <trial/Lattices/LatticeRegion.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/TableLogSink.h>
#include <aips/Logging/LogMessage.h>

#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/LogiArray.h>
#include <aips/Exceptions/Error.h>
#include <aips/OS/File.h>
#include <aips/OS/Path.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Arrays/Slicer.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/TableLock.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/TableInfo.h>
#include <aips/Tables/TableColumn.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <aips/Quanta/UnitMap.h>

#include <iostream.h>
#include <strstream.h>


template <class T> 
PagedImage<T>::PagedImage (const TiledShape& shape, 
			   const CoordinateSystem& coordinateInfo, 
			   Table& table, uInt rowNumber)
: ImageInterface<T>(RegionHandlerTable(getTable, this)),
  map_p         (shape, table, "map", rowNumber),
  logTablePtr_p (0),
  regionPtr_p   (0)
{
  attach_logtable();
  logSink() << LogOrigin("PagedImage<T>", 
			 "PagedImage(const TiledShape& shape,  "
			 "const CoordinateSystem& coordinateInfo, "
			 "Table& table, uInt rowNumber)", WHERE);
  
  logSink() << LogIO::DEBUGGING 
	    << "Creating an image in row " << rowNumber 
	    << " of an existing table called"
	    << " '" << name() << "'" << endl
	    << "The image shape is " << shape.shape() << endl;
  logSink() << LogIO::NORMAL;
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
  logTablePtr_p (0),
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
  logTablePtr_p (0),
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
  logSink() << LogOrigin("PagedImage<T>", 
			 "PagedImage(const TiledShape& shape, "
			 "const CoordinateSystem& coordinateInfo, "
			 "const TableLock& lockoptions, "
			 "const String& filename, "
			 "uInt rowNumber)", WHERE);
  logSink() << LogIO::DEBUGGING
	    << "Creating an image in row " << rowNumber 
	    << " of a new table called"
	    << " '" << filename << "'" << endl
	    << "The image shape is " << shape.shape() << endl;
  SetupNewTable newtab (filename, TableDesc(), Table::New);
  Table tab(newtab, lockOptions);
  map_p = PagedArray<T> (shape, tab, "map", rowNumber);
  attach_logtable();
  logSink() << LogIO::NORMAL;
  AlwaysAssert(setCoordinateInfo(coordinateInfo), AipsError);
  setTableType();
}

template <class T> 
PagedImage<T>::PagedImage (const TiledShape& shape, 
			   const CoordinateSystem& coordinateInfo, 
			   const String& filename, 
			   uInt rowNumber)
: ImageInterface<T>(RegionHandlerTable(getTable, this)),
  logTablePtr_p (0),
  regionPtr_p   (0)
{
  logSink() << LogOrigin("PagedImage<T>", 
			 "PagedImage(const TiledShape& shape,  "
			 "const CoordinateSystem& coordinateInfo, "
			 "const String& filename, "
			 "uInt rowNumber)", WHERE);
  logSink() << LogIO::DEBUGGING
	    << "Creating an image in row " << rowNumber 
	    << " of a new table called"
	    << " '" << filename << "'" << endl
	    << "The image shape is " << shape.shape() << endl;
  SetupNewTable newtab (filename, TableDesc(), Table::New);
  Table tab(newtab);
  map_p = PagedArray<T> (shape, tab, "map", rowNumber);
  attach_logtable();
  logSink() << LogIO::NORMAL;
  AlwaysAssert(setCoordinateInfo(coordinateInfo), AipsError);
  setTableType();
}

template <class T> 
PagedImage<T>::PagedImage (Table& table, MaskSpecifier spec, uInt rowNumber)
: ImageInterface<T>(RegionHandlerTable(getTable, this)),
  map_p         (table, "map", rowNumber),
  logTablePtr_p (0),
  regionPtr_p   (0)
{
  attach_logtable();
  logSink() << LogOrigin("PagedImage<T>", 
			 "PagedImage(Table& table, "
			 "MaskSpecifier, "
			 "uInt rowNumber)", WHERE);
  logSink() << LogIO::DEBUGGING
	    << "Reading an image from row " << rowNumber 
	    << " of a table called"
	    << " '" << name() << "'" << endl
	    << "The image shape is " << map_p.shape() << endl;

  TableRecord rec = table.keywordSet();
//
  CoordinateSystem* restoredCoords = CoordinateSystem::restore(rec, "coords");
  AlwaysAssert(restoredCoords != 0, AipsError);
  setCoordsMember (*restoredCoords);
  delete restoredCoords;
  applyMaskSpecifier (spec);
//
  restoreImageInfo(rec);
}

template <class T> 
PagedImage<T>::PagedImage (const String& filename, MaskSpecifier spec,
			   uInt rowNumber)
: ImageInterface<T>(RegionHandlerTable(getTable, this)),
  logTablePtr_p (0),
  regionPtr_p   (0)
{
  logSink() << LogOrigin("PagedImage<T>", 
			 "PagedImage(const String& filename, "
			 "MaskSpecifier, "
			 "uInt rowNumber)", WHERE);
  logSink() << LogIO::DEBUGGING
	    << "Reading an image from row " << rowNumber 
	    << " of a file called"
	    << " '" << filename << "'" << endl;
  Table tab(filename);
  map_p = PagedArray<T>(tab, "map", rowNumber);
  attach_logtable();
  logSink() << LogIO::DEBUGGING << "The image shape is " << map_p.shape() << endl;
  logSink() << LogIO::DEBUGGING;
//
  TableRecord rec = table().keywordSet();
//
  CoordinateSystem* restoredCoords = CoordinateSystem::restore(rec, "coords");
  AlwaysAssert(restoredCoords != 0, AipsError);
  setCoordsMember (*restoredCoords);
  delete restoredCoords;
  applyMaskSpecifier (spec);
//
  restoreImageInfo(rec);
}

template <class T> 
PagedImage<T>::PagedImage (const String& filename,
			   const TableLock& lockOptions,
			   MaskSpecifier spec, uInt rowNumber)
: ImageInterface<T>(RegionHandlerTable(getTable, this)),
  logTablePtr_p (0),
  regionPtr_p   (0)
{
  makePagedImage (filename, lockOptions, spec, rowNumber);
}

template <class T> 
PagedImage<T>::PagedImage (const String& filename,
			   TableLock::LockOption lockMode,
			   MaskSpecifier spec, uInt rowNumber)
: ImageInterface<T>(RegionHandlerTable(getTable, this)),
  logTablePtr_p (0),
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
  logSink() << LogOrigin("PagedImage<T>", 
			 "PagedImage(const String& filename, "
			 "const TableLock& lockOptions, "
			 "MaskSpecifier, "
			 "uInt rowNumber)", WHERE);
  logSink() << LogIO::DEBUGGING
	    << "Reading an image from row " << rowNumber 
	    << " of a file called"
	    << " '" << filename << "'" << endl;
  Table tab(filename, lockOptions);
  map_p = PagedArray<T>(tab, "map", rowNumber);
  attach_logtable();
  logSink() << LogIO::DEBUGGING << "The image shape is " << map_p.shape() << endl;
  logSink() << LogIO::DEBUGGING;
//
  TableRecord rec = table().keywordSet();
//
  CoordinateSystem* restoredCoords = CoordinateSystem::restore(rec, "coords");
  AlwaysAssert(restoredCoords != 0, AipsError);
  setCoordsMember (*restoredCoords);
  delete restoredCoords;
  applyMaskSpecifier (spec);
//
  restoreImageInfo(rec);
}

template <class T> 
PagedImage<T>::PagedImage (const PagedImage<T>& other)
: ImageInterface<T>(other),
  map_p            (other.map_p),
  logTablePtr_p    (other.logTablePtr_p),
  regionPtr_p      (0)
{
  if (other.regionPtr_p != 0) {
    regionPtr_p = new LatticeRegion (*other.regionPtr_p);
  }
}

template <class T> 
PagedImage<T>::~PagedImage()
{
  // Detach the LogIO from the log table in case the pixel table is going to
  // be destroyed.
  // Note that this also destructs the LogTableSink object pointed to
  // by logTablePtr_p, so we do not need to delete that pointer.
  delete regionPtr_p;
  closeLogSink (False);
}

template <class T> 
PagedImage<T>& PagedImage<T>::operator=(const PagedImage<T>& other)
{
  if (this != &other) {
    ImageInterface<T>::operator= (other);
    map_p = other.map_p;
    logTablePtr_p = other.logTablePtr_p;
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
void PagedImage<T>::doReopenRW()
{
  table().reopenRW();
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
      name = "";
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
  logSink() << LogOrigin ("PagedImage<T>", "setCoordinateInfo(const "
			  "CoordinateSystem& coords)",  WHERE);
  
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
	logSink() << LogIO::SEVERE << "Error saving coordinates in table"
		  << LogIO::POST;
	ok = False;
      }
    } else {
      logSink() << LogIO::SEVERE
		<< "Table is not writable: not saving coordinates to disk."
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
const RecordInterface& PagedImage<T>::miscInfo() const
{
  static TableRecord* use_if_none_in_table = 0; // A small leak if set
  const Table& tab = table();
  if (!tab.keywordSet().isDefined("miscinfo") ||
      tab.keywordSet().dataType("miscinfo") != TpRecord) {
    if (!use_if_none_in_table) {
      use_if_none_in_table = new TableRecord; // leak
      AlwaysAssert(use_if_none_in_table, AipsError);
    }
    return *use_if_none_in_table;
  }
  return tab.keywordSet().asRecord("miscinfo");
}

template<class T> 
Bool PagedImage<T>::setMiscInfo(const RecordInterface& newInfo)
{
  Table& tab = table();
  reopenRW();
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
                                   (const LatticeNavigator& navigator) const
{
  return map_p.makeIter (navigator);
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
  logSink() << LogOrigin("PagedImage<T>", 
			 "operator+=(const Lattice<T>& other)", WHERE) <<
    LogIO::DEBUGGING << "Adding other to our pixels" << endl;
  
  check_conformance(other);
  logSink() << LogIO::POST;
  logSink() << LogIO::NORMAL;
/*  
  IPosition cursorShape(this->niceCursorShape(this->advisedMaxPixels() - 1));
  LatticeIterator<T> toiter(*this, cursorShape);
  RO_LatticeIterator<T> otheriter(other, cursorShape);
  for (toiter.reset(), otheriter.reset(); !toiter.atEnd();
       toiter++, otheriter++) {
    toiter.rwCursor() += otheriter.cursor();
  }
  // Mask is not handled in such a loop; therefore use LEL.
*/

  LatticeExpr<T> expr(*this + other);
  copyData (expr);
  ///  copyData (LatticeExprNode(*this)+LatticeExprNode(other));
  return *this;
}



template<class T>
void PagedImage<T>::doReopenLogSink()
{
  open_logtable();
}

template<class T> 
void PagedImage<T>::attach_logtable()
{
  open_logtable();
  logSink() << LogIO::NORMAL;
}

template<class T> 
void PagedImage<T>::open_logtable()
{
  // Open logtable as readonly if main table is not writable.
  Table& tab = table();
  if (tab.isWritable()) {
    logTablePtr_p = new TableLogSink(LogFilter(), name() + "/logtable", False);
  } else {
    logTablePtr_p = new TableLogSink(name() + "/logtable");
  }
  LogSinkInterface* interface = logTablePtr_p;
  AlwaysAssert(logTablePtr_p != 0, AipsError);
  // Insert the keyword if possible and if it does not exist yet.
  if (tab.isWritable()  &&  ! tab.keywordSet().isDefined ("logtable")) {
    tab.rwKeywordSet().defineTable("logtable", logTablePtr_p->table());
  }
  LogSink tmp;
  tmp.localSink(interface);
  setLogMember (tmp);
}

template<class T> 
Bool PagedImage<T>::setUnits(const Unit& newUnits) 
{
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
Unit PagedImage<T>::units() const
{
  const Table& tab = table();
  Unit retval;
  String unitName;
  if (tab.keywordSet().isDefined("units")) {
    if (tab.keywordSet().dataType("units") != TpString) {
      LogIO os;
      os << LogOrigin("PagedImage<T>", "units()", WHERE) <<
	"'units' keyword in image table is not a string! Units not restored." 
		<< LogIO::SEVERE << LogIO::POST;
      return retval;
    } else {
      tab.keywordSet().get("units", unitName);
    }
  }
  if (unitName != "") {
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
      os << LogOrigin("PagedImage<T>", "units()", WHERE) <<
	LogIO::SEVERE << "Unit '" << unitName << "' is unknown."
	" Not restoring units" << LogIO::POST;
    } else {
      retval = Unit(unitName);
    }
  }
  return retval;
}


template<class T> 
void PagedImage<T>::removeRegion (const String& name,
				  RegionHandler::GroupType type,
				  Bool throwIfUnknown)
{
  reopenRW();
  // Remove the default mask if it is the region to be removed.
  if (name == getDefaultMask()) {
    setDefaultMask ("");
  }
  ImageInterface<T>::removeRegion (name, type, throwIfUnknown);
}


template<class T> 
void PagedImage<T>::check_conformance(const Lattice<T>& other)
{
  if (! this->conform(other)) {
    logSink() << "this and other do not conform (" << this->shape() 
	      << " != " << other.shape() << ")" << LogIO::EXCEPTION;
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
}

template<class T> 
void PagedImage<T>::setCacheSizeFromPath(const IPosition& sliceShape, 
    	                                 const IPosition& windowStart,
                                         const IPosition& windowLength,
                                         const IPosition& axisPath)
{
  map_p.setCacheSizeFromPath(sliceShape, windowStart, windowLength, axisPath);
}

template<class T>
void PagedImage<T>::setCacheSizeInTiles (uInt howManyTiles)  
{  
  map_p.setCacheSizeInTiles (howManyTiles);
}


template<class T> 
void PagedImage<T>::clearCache()
{
  map_p.clearCache();
}

template<class T> 
void PagedImage<T>::showCacheStatistics(ostream& os) const
{
  os << "Pixel statistics : ";
  map_p.showCacheStatistics(os);
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
  if (logTablePtr_p != 0) {
    logTablePtr_p->table().unlock();
  }
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
  if (logTablePtr_p != 0
  &&  !logTablePtr_p->table().hasLock (FileLocker::Read)) {
    logTablePtr_p->table().resync();
  }
  if (regionPtr_p != 0
  &&  !regionPtr_p->hasLock (FileLocker::Read)) {
    regionPtr_p->resync();
  }
}

template<class T>
void PagedImage<T>::flush()
{
  map_p.flush();
  if (logTablePtr_p != 0) {
    logTablePtr_p->table().flush();
  }
  if (regionPtr_p != 0) {
    regionPtr_p->flush();
  }
}

template<class T>
void PagedImage<T>::tempClose()
{
  map_p.tempClose();
  closeLogSink (True);
  logTablePtr_p = 0;
  if (regionPtr_p != 0) {
    regionPtr_p->tempClose();
  }
}

template<class T>
void PagedImage<T>::reopen()
{
  map_p.reopen();
  reopenLog();
  if (regionPtr_p != 0) {
    regionPtr_p->reopen();
  }
}

template<class T>
Bool PagedImage<T>::setImageInfo (const ImageInfo& info) 
{
  logSink() << LogOrigin ("PagedImage<T>", "setImageInfo(const "
			  "ImageInfo& info)",  WHERE);
  Bool ok = ImageInterface<T>::setImageInfo(info);
  if (ok) {
    reopenRW();
    Table& tab = table();
    if (tab.isWritable()) {

// Update the ImageInfo

      if (tab.keywordSet().isDefined("imageinfo")) {
	tab.rwKeywordSet().removeField("imageinfo");
      }
      TableRecord rec;
      String error;
      if (imageInfo().toRecord(error, rec)) {
         tab.rwKeywordSet().defineRecord("imageinfo", rec);
      } else {
	logSink() << LogIO::SEVERE << "Error saving ImageInfo in table because " 
          + error << LogIO::POST;
	ok = False;
      }
    } else {
      logSink() << LogIO::SEVERE
		<< "Table is not writable: not saving ImageInfo to disk."
		<< LogIO::POST;
    }
  }
  return ok;
}


template<class T>
void PagedImage<T>::mergeTableLogSink (const LogIO& other)
{
  if (logSink().localSink().isTableLogSink()) {
    if (other.localSink().isTableLogSink()) {
      reopenRW();
      logSink().localSink().castToTableLogSink().concatenate (
		      other.localSink().castToTableLogSink());
    }
  }
}
