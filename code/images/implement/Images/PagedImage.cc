//# PagedImage.cc: defines the PagedImage class
//# Copyright (C) 1994,1995,1996,1997,1998
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
#include <trial/Lattices/ArrayLattice.h>
#include <trial/Lattices/LatticeNavigator.h>
#include <trial/Lattices/LatticeStepper.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/PagedArrIter.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/TableLogSink.h>

#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/LogiArray.h>
#include <aips/Exceptions/Error.h>
#include <aips/OS/File.h>
#include <aips/OS/Path.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/TableLock.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/TableInfo.h>
#include <aips/Utilities/COWPtr.h>
#include <aips/Utilities/DefaultValue.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <aips/Quanta/UnitMap.h>

#include <aips/Logging/LogMessage.h>

#include <iostream.h>
#include <strstream.h>

template <class T> 
PagedImage<T>::PagedImage()
: logTablePtr_p (0)
{
  table_p.makePermanent();           // avoid double deletion by Cleanup
}

template <class T> 
PagedImage<T>::PagedImage(const TiledShape & shape, 
                          const CoordinateSystem & coordinateInfo, 
                          Table & table, uInt rowNumber)
: ImageInterface<T>(),
  table_p(table),
  map_p(shape, table, "map", rowNumber),
  logTablePtr_p (0)
{
  table_p.makePermanent();           // avoid double deletion by Cleanup
  attach_logtable();
  logSink() << LogOrigin("PagedImage<T>", 
			 "PagedImage(const TiledShape & shape,  "
			 "const CoordinateSystem & coordinateInfo, "
			 "Table & table, uInt)", WHERE);
  
  logSink() << LogIO::DEBUGGING 
	    << "Creating an image in row " << rowNumber 
	    << " of an existing table called"
	    << " '" << name() << "'" << endl
	    << "The image shape is " << shape.shape() << endl;
  logSink() << LogIO::NORMAL;
  AlwaysAssert(setCoordinateInfo(coordinateInfo), AipsError);
  setTableType();
};

template <class T> 
PagedImage<T>::PagedImage(const TiledShape & shape, 
                          const CoordinateSystem & coordinateInfo, 
                          const String & filename, 
                          const TableLock& lockOptions,
                          uInt rowNumber)
: ImageInterface<T>(),
  logTablePtr_p (0)
{
  logSink() << LogOrigin("PagedImage<T>", 
			 "PagedImage(const TiledShape & shape, "
			 "const CoordinateSystem & coordinateInfo, "
			 "const TableLock& lockoptions, "
			 "const String & filename, "
			 "uInt rowNumber)", WHERE);
  logSink() << LogIO::DEBUGGING
	    << "Creating an image in row " << rowNumber 
	    << " of a new table called"
	    << " '" << filename << "'" << endl
	    << "The image shape is " << shape.shape() << endl;
  SetupNewTable newtab (filename, TableDesc(), Table::New);
  table_p = Table(newtab, lockOptions);
  table_p.makePermanent();           // avoid double deletion by Cleanup
  map_p = PagedArray<T> (shape, table_p, "map", rowNumber);
  attach_logtable();
  logSink() << LogIO::NORMAL;
  AlwaysAssert(setCoordinateInfo(coordinateInfo), AipsError);
  setTableType();
};

template <class T> 
PagedImage<T>::PagedImage(const TiledShape & shape, 
                          const CoordinateSystem & coordinateInfo, 
                          const String & filename, 
                          uInt rowNumber)
: ImageInterface<T>(),
  logTablePtr_p (0)
{
  logSink() << LogOrigin("PagedImage<T>", 
			 "PagedImage(const TiledShape & shape,  "
			 "const CoordinateSystem & coordinateInfo, "
			 "const String & filename, "
			 "uInt rowNumber)", WHERE);
  logSink() << LogIO::DEBUGGING
	    << "Creating an image in row " << rowNumber 
	    << " of a new table called"
	    << " '" << filename << "'" << endl
	    << "The image shape is " << shape.shape() << endl;
  SetupNewTable newtab (filename, TableDesc(), Table::New);
  table_p = Table(newtab);
  table_p.makePermanent();           // avoid double deletion by Cleanup
  map_p = PagedArray<T> (shape, table_p, "map", rowNumber);
  attach_logtable();
  logSink() << LogIO::NORMAL;
  AlwaysAssert(setCoordinateInfo(coordinateInfo), AipsError);
  setTableType();
};

template <class T> 
PagedImage<T>::PagedImage(Table & table, uInt rowNumber)
: ImageInterface<T>(),
  table_p(table),
  map_p(table, "map", rowNumber),
  logTablePtr_p (0)
{
  table_p.makePermanent();           // avoid double deletion by Cleanup
  attach_logtable();
  logSink() << LogOrigin("PagedImage<T>", 
			 "PagedImage(Table & table, "
			 "uInt rowNumber)", WHERE);
  logSink() << LogIO::DEBUGGING
	    << "Reading an image from row " << rowNumber 
	    << " of a table called"
	    << " '" << name() << "'" << endl
	    << "The image shape is " << map_p.shape() << endl;

  CoordinateSystem* restoredCoords =
    CoordinateSystem::restore(table_p.keywordSet(), "coords");
  AlwaysAssert(restoredCoords != 0, AipsError);
  coords_p = *restoredCoords;
  delete restoredCoords;
};

template <class T> 
PagedImage<T>::PagedImage(const String & filename, uInt rowNumber)
: ImageInterface<T>(),
  logTablePtr_p (0)
{
  logSink() << LogOrigin("PagedImage<T>", 
			 "PagedImage(const String & filename, "
			 "uInt rowNumber)", WHERE);
  logSink() << LogIO::DEBUGGING
	    << "Reading an image from row " << rowNumber 
	    << " of a file called"
	    << " '" << filename << "'" << endl;
  table_p = Table(filename);
  table_p.makePermanent();           // avoid double deletion by Cleanup
  map_p = PagedArray<T>(table_p, "map", rowNumber);
  attach_logtable();
  logSink() << LogIO::DEBUGGING << "The image shape is " << map_p.shape() << endl;
  logSink() << LogIO::DEBUGGING;
  CoordinateSystem * restoredCoords =
    CoordinateSystem::restore(table_p.keywordSet(), "coords");
  AlwaysAssert(restoredCoords != 0, AipsError);
  coords_p = *restoredCoords;
  delete restoredCoords;
};

template <class T> 
PagedImage<T>::PagedImage(const String & filename, const TableLock& lockOptions,
                          uInt rowNumber)
: ImageInterface<T>(),
  logTablePtr_p (0)
{
  logSink() << LogOrigin("PagedImage<T>", 
			 "PagedImage(const String & filename, "
			 "const TableLock& lockOptions, "
			 "uInt rowNumber)", WHERE);
  logSink() << LogIO::DEBUGGING
	    << "Reading an image from row " << rowNumber 
	    << " of a file called"
	    << " '" << filename << "'" << endl;
  table_p = Table(filename, lockOptions);
  table_p.makePermanent();           // avoid double deletion by Cleanup
  map_p = PagedArray<T>(table_p, "map", rowNumber);
  attach_logtable();
  logSink() << LogIO::DEBUGGING << "The image shape is " << map_p.shape() << endl;
  logSink() << LogIO::DEBUGGING;
  CoordinateSystem * restoredCoords =
    CoordinateSystem::restore(table_p.keywordSet(), "coords");
  AlwaysAssert(restoredCoords != 0, AipsError);
  coords_p = *restoredCoords;
  delete restoredCoords;
};

template <class T> 
PagedImage<T>::PagedImage(const PagedImage<T> & other)
: ImageInterface<T>(other),
  table_p(other.table_p), 
  map_p(other.map_p),
  logTablePtr_p(other.logTablePtr_p)
{
  table_p.makePermanent();           // avoid double deletion by Cleanup
  attach_logtable();
}

template <class T> 
PagedImage<T>::~PagedImage()
{
  // Detach the LogIO from the log table in case the pixel table is going to
  // be destroyed.
  // Note that this also destructs the LogTableSink object pointed to
  // by logTablePtr_p, so we do not need to delete that pointer.
  LogIO globalOnly;
  log_p = globalOnly;
};

template <class T> 
PagedImage<T> & PagedImage<T>::operator=(const PagedImage<T> & other)
{
  if (this != &other) {
    ImageInterface<T>::operator= (other);
    table_p = other.table_p;
    table_p.makePermanent();           // avoid double deletion by Cleanup
    map_p = other.map_p;
    logTablePtr_p = other.logTablePtr_p;
  } 
  return *this;
};

template <class T> 
Lattice<T>* PagedImage<T>::clone() const
{
  return new PagedImage<T> (*this);
}
template <class T> 
ImageInterface<T>* PagedImage<T>::cloneII() const
{
  return new PagedImage<T> (*this);
}

template <class T> 
Bool PagedImage<T>::isWritable() const
{
  return map_p.isWritable();
}

template<class T>
void PagedImage<T>::doReopenRW()
{
  table_p.reopenRW();
  if (logTablePtr_p != 0) {
    logTablePtr_p->reopenRW (LogFilter());
  }
}

template <class T> 
void PagedImage<T>::rename(const String & newName)
{
  table_p.rename(newName, Table::New);
}

template <class T> 
String PagedImage<T>::name(const Bool stripPath) const 
{
   if (!stripPath) {
      // Full path included
      return table_p.tableName();
   }
   // Strip off path
   File file(table_p.tableName());
   return file.path().baseName();
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
void PagedImage<T>::resize(const TiledShape & newShape)
{
  if (newShape.shape().nelements() != coords_p.nPixelAxes()) {
    throw(AipsError("PagedImage<T>::resize: coordinate info is "
		    "the incorrect shape."));
  }
  map_p.resize(newShape);
}

template <class T> 
Bool PagedImage<T>::setCoordinateInfo(const CoordinateSystem & coords)
{
  logSink() << LogOrigin ("PagedImage<T>", "setCoordinateInfo(const "
			  "CoordinateSystem & coords)",  WHERE);
  
  Bool ok = ImageInterface<T>::setCoordinateInfo(coords);
  if (ok) {
    reopenRW();
    if (table_p.isWritable()) {
      // Update the coordinates
      if (table_p.keywordSet().isDefined("coords")) {
	table_p.rwKeywordSet().removeField("coords");
      }
      if (!(coords_p.save(table_p.rwKeywordSet(), "coords"))) {
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
Bool PagedImage<T>::doGetSlice(Array<T> & buffer, const Slicer & theSlice)
{
  return map_p.doGetSlice(buffer, theSlice);
}

template <class T> 
void PagedImage<T>::doPutSlice(const Array<T> & sourceBuffer, 
                               const IPosition & where, 
                               const IPosition & stride)
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
void PagedImage<T>::apply(T (*function)(const T &))
{
    map_p.apply(function);
}

template <class T> 
void PagedImage<T>::apply(const Functional<T,T> & function)
{
    map_p.apply(function);
}

template <class T> 
Table PagedImage<T>::table()
{
  return table_p;
}

template <class T> 
T PagedImage<T>::getAt(const IPosition & where) const
{
   return map_p(where);
}

template <class T> 
void PagedImage<T>::putAt(const T & value, const IPosition & where) {
    map_p.putAt (value, where);
}

template<class T> 
const RecordInterface & PagedImage<T>::
miscInfo() const
{
  static TableRecord* use_if_none_in_table = 0; // A small leak if set
  if (!table_p.keywordSet().isDefined("miscinfo") ||
      table_p.keywordSet().dataType("miscinfo") != TpRecord) {
    if (!use_if_none_in_table) {
      use_if_none_in_table = new TableRecord; // leak
      AlwaysAssert(use_if_none_in_table, AipsError);
    }
    return *use_if_none_in_table;
  } else {
    return table_p.keywordSet().asRecord("miscinfo");
  }
}

template<class T> 
Bool PagedImage<T>::setMiscInfo(const RecordInterface & newInfo)
{
  reopenRW();
  if (! table_p.isWritable()) {
    return False;
  }
  if (table_p.keywordSet().isDefined("miscinfo")) {
    table_p.rwKeywordSet().removeField("miscinfo");
  }
  table_p.rwKeywordSet().defineRecord("miscinfo", newInfo);
  return True;
}

template <class T> 
LatticeIterInterface<T> * PagedImage<T>::makeIter(const LatticeNavigator & navigator) const
{
  return map_p.makeIter (navigator);
}

template <class T> 
Bool PagedImage<T>::ok() const
{
  Int okay = (map_p.ndim() == coords_p.nPixelAxes());
  okay = okay && !table_p.isNull();
  return okay  ?  True : False;
}

/*
template <class T> 
PagedImage<T> & PagedImage<T>::operator+= (const PagedImage<T> & other)
{
  logSink() << LogOrigin("PagedImage<T>", 
			 "operator+=(const PagedImage<T> & other)",
			 WHERE) << LogIO::DEBUGGING;
  logSink() << "Adding other to our pixels" << endl;
  report_mask();
  check_conformance(other);
  logSink() << LogIO::POST;
  logSink() << LogIO::NORMAL;
  
  IPosition cursorShape(this->niceCursorShape(this->maxPixels() - 1));
  LatticeIterator<T> toiter(*this, cursorShape);
  RO_LatticeIterator<T> otheriter(other, cursorShape);
  
  Bool writeThis=ToBool(this->isMasked() && !this->throughmask_p);
  Bool writeOther=ToBool(other.isMasked()&& !other.throughmask_p);
  if (writeThis && writeOther) {
    LatticeIterator<Bool> maskiter(this->mask(), cursorShape);
    RO_LatticeIterator<Bool> othermaskiter(other.mask(), cursorShape);
    
    for (toiter.reset(), otheriter.reset(),
	                         maskiter.reset(), othermaskiter.reset(); 
	 !toiter.atEnd();
	 toiter++, maskiter++, otheriter++, othermaskiter++) {
      maskiter.rwCursor() = maskiter.cursor() || othermaskiter.cursor();
      toiter.rwCursor()(maskiter.cursor()) += otheriter.cursor();
    }
  } else if (!writeThis && writeOther) {
    mask_p = new PagedArray<Bool>(this->shape(), table_p, "mask", 0);
    LatticeIterator<Bool> maskiter(this->mask(), cursorShape);
    RO_LatticeIterator<Bool> othermaskiter(other.mask(), cursorShape);
    for (toiter.reset(), otheriter.reset(),
	                        maskiter.reset(), othermaskiter.reset(); 
	 !toiter.atEnd();
	 toiter++, maskiter++, otheriter++, othermaskiter++) {
      maskiter.woCursor() = othermaskiter.cursor();
      toiter.rwCursor()(othermaskiter.cursor()) += otheriter.cursor();
    }
  } else if(writeThis && !writeOther) {
    LatticeIterator<Bool> maskiter(this->mask(), cursorShape);
    for (toiter.reset(), otheriter.reset(), maskiter.reset(); 
	 !toiter.atEnd();
	 toiter++, maskiter++, otheriter++) {
      toiter.rwCursor()(maskiter.cursor()) += otheriter.cursor();
    }
  } else {
    IPosition cursorShape(this->niceCursorShape(this->maxPixels() - 1));
    LatticeIterator<T> toiter(*this, cursorShape);
    RO_LatticeIterator<T> otheriter(other, cursorShape);
    for (toiter.reset(), otheriter.reset(); 
	 !toiter.atEnd();
	 toiter++, otheriter++) {
      toiter.rwCursor() += otheriter.cursor();
    }
  }
  return *this;
}

template <class T> 
PagedImage<T> & PagedImage<T>::operator+= (const T & val)
{
  logSink() << LogOrigin("PagedImage<T>", "operator+=(const T & val)", 
			 WHERE) << LogIO::DEBUGGING;
  logSink() << "Modifying pixels, val=" << val << " ";
  
  report_mask();
  logSink() << LogIO::POST;
  logSink() << LogIO::NORMAL;
  
  IPosition cursorShape(this->niceCursorShape(this->maxPixels() - 1));
  LatticeIterator<T> toiter(*this, cursorShape);
  if(isMasked() && !throughmask_p) {
    RO_LatticeIterator<Bool> maskiter(this->mask(), cursorShape);
    for (toiter.reset(), maskiter.reset(); !toiter.atEnd();
	 toiter++, maskiter++) {
      toiter.rwCursor()(maskiter.cursor()) += val;
    }
  } else {
    for (toiter.reset(); !toiter.atEnd(); toiter++) {
      toiter.rwCursor() += val;
    }
  }
  return *this;
}
*/

template <class T> 
PagedImage<T> & PagedImage<T>::operator+= (const Lattice<T> & other)
{
  logSink() << LogOrigin("PagedImage<T>", 
			 "operator+=(const Lattice<T> & other)", WHERE) <<
    LogIO::DEBUGGING << "Adding other to our pixels" << endl;
  
  check_conformance(other);
  logSink() << LogIO::POST;
  logSink() << LogIO::NORMAL;
  
  IPosition cursorShape(this->niceCursorShape(this->maxPixels() - 1));
  LatticeIterator<T> toiter(*this, cursorShape);
  RO_LatticeIterator<T> otheriter(other, cursorShape);
  for (toiter.reset(), otheriter.reset(); !toiter.atEnd();
       toiter++, otheriter++) {
    toiter.rwCursor() += otheriter.cursor();
  }
  return *this;
}

template<class T> 
void PagedImage<T>::attach_logtable()
{
  // Open logtable as readonly if main table is not writable.
  TableLogSink *logtable = 0;
  if (table_p.isWritable()) {
    logtable = new TableLogSink(LogFilter(), name() + "/logtable");
  } else {
    logtable = new TableLogSink(name() + "/logtable");
  }
  LogSinkInterface *interface = logtable;
  AlwaysAssert(logtable != 0, AipsError);
  // Insert the keyword if it does not exist yet.
  if (! table_p.keywordSet().isDefined ("logtable")) {
    table_p.rwKeywordSet().defineTable("logtable", logtable->table());
  }
  LogSink tmp;
  tmp.localSink(interface);
  log_p = tmp;
  log_p << LogIO::NORMAL;
}

template<class T> 
Bool PagedImage<T>::setUnits(const Unit &newUnits) 
{
  reopenRW();
  if (! table_p.isWritable()) {
    return False;
  }
  if (table_p.keywordSet().isDefined("units")) {
    table_p.rwKeywordSet().removeField("units");
  }
  table_p.rwKeywordSet().define("units", newUnits.getName());
  return True;
}

template<class T> 
Unit PagedImage<T>::units() const
{
  Unit retval;
  String unitName;
  if (table_p.keywordSet().isDefined("units")) {
    if (table_p.keywordSet().dataType("units") != TpString) {
      LogIO os;
      os << LogOrigin("PagedImage<T>", "units()", WHERE) <<
	"'units' keyword in image table is not a string! Units not restored." 
		<< LogIO::SEVERE << LogIO::POST;
      return retval;
    } else {
      table_p.keywordSet().get("units", unitName);
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
void PagedImage<T>::check_conformance(const Lattice<T> & other)
{
  if (! this->conform(other)) {
    logSink() << "this and other do not conform (" << this->shape() 
	      << " != " << other.shape() << ")" << LogIO::EXCEPTION;
  }
};

template<class T> 
uInt PagedImage<T>::maximumCacheSize() const
{
  return map_p.maximumCacheSize();
};

template<class T> 
void PagedImage<T>::setMaximumCacheSize(uInt howManyPixels)
{
  map_p.setMaximumCacheSize(howManyPixels);
};

template<class T> 
void PagedImage<T>::setCacheSizeFromPath(const IPosition & sliceShape, 
    	                                 const IPosition & windowStart,
                                         const IPosition & windowLength,
                                         const IPosition & axisPath)
{
  map_p.setCacheSizeFromPath(sliceShape, windowStart, windowLength, axisPath);
};

template<class T> 
void PagedImage<T>::clearCache()
{
  map_p.clearCache();
};

template<class T> 
void PagedImage<T>::showCacheStatistics(ostream & os) const
{
  os << "Pixel statistics : ";
  map_p.showCacheStatistics(os);
};

template<class T> 
uInt PagedImage<T>::maxPixels() const
{
  return map_p.maxPixels();
};

template<class T> 
IPosition PagedImage<T>::doNiceCursorShape(uInt maxPixels) const
{
  return map_p.niceCursorShape(maxPixels);
};

template<class T> 
void PagedImage<T>::setTableType()
{
  TableInfo & info(table_p.tableInfo());
  {
    const String reqdType = info.type(TableInfo::PAGEDIMAGE);
    if (info.type() != reqdType)
      info.setType(reqdType);
  }
  {
    const String reqdSubType = info.subType(TableInfo::PAGEDIMAGE);
    if (info.subType() != reqdSubType)
      info.setSubType(reqdSubType);
  }
};
