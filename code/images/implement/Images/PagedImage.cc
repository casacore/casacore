//# PagedImage.cc: defines the PagedImage class
//# Copyright (C) 1994,1995,1996,1997
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

#include <aips/Arrays/MaskedArray.h>
#include <trial/Images/PagedImage.h>
#include <trial/Lattices/ArrayLattice.h>
#include <trial/Lattices/LatticeNavigator.h>
#include <trial/Lattices/LatticeStepper.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/PagedArrIter.h>
#include <aips/Logging/LogIO.h>

#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/LogiArray.h>
#include <aips/Arrays/MaskArrLogi.h>
#include <aips/Arrays/MaskArrMath.h>
#include <aips/Arrays/MaskedArray.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/TableInfo.h>
#include <aips/Utilities/COWPtr.h>
#include <aips/Utilities/DefaultValue.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <aips/Measures/UnitMap.h>

#include <aips/Logging/LogMessage.h>

#include <iostream.h>
#include <strstream.h>

// IPosition, CoordinateSystem and Table File name constructor
template <class T> 
PagedImage<T>::PagedImage(const IPosition &shape, 
			  const CoordinateSystem& coordinateInfo, 
			  Table &table, Bool masking, uInt)
: ImageInterface<T>(coordinateInfo, True), table_p(table),
  map_p(shape, table, "map", 0), mask_p((PagedArray<Bool> *)0)
{
    logSink() << LogOrigin("PagedImage<T>", 
			   "PagedImage(const IPosition &shape,  "
			   "const CoordinateSystem& coordinateInfo, "
			   "Table &table, Bool masking, uInt)", WHERE );

    logSink() << LogIO::DEBUGGING << "Creating image in existing table ";
    {
	logSink() << "\t" << "shape=" << shape << endl;
    }
    logSink() << "\tCoordinate information will be written later\n\n";
    logSink() << String("\tWriting to file ") + table.tableName() << endl;
    if (masking) {
        logSink() << "\tA mask is being created" << endl;
    } else {
        logSink() << "\tNO mask is being created!" << endl;
    }
    logSink() << "\tThe row number is being ignored" << LogIO::POST;

    if (masking) mask_p = new PagedArray<Bool>(shape, table, "mask", 0);
    ::defaultValue(defaultvalue_p); 
    setTableType();
};

// IPosition, CoordinateSystem and Table File name constructor
template <class T> 
PagedImage<T>::PagedImage(const IPosition &shape, 
			  const CoordinateSystem& coordinateInfo, 
			  const String &filename, Bool masking, uInt)
: ImageInterface<T>(coordinateInfo, True),
  mask_p((PagedArray<Bool> *)0)
{
  SetupNewTable newtab (filename, TableDesc("", TableDesc::Scratch),
			Table::New);
  table_p = Table (newtab);
  map_p   = PagedArray<T> (shape, table_p, "map", 0);
  if (masking) mask_p = new PagedArray<Bool>(shape, table_p, "mask", 0);
  ::defaultValue(defaultvalue_p); 
  setTableType();
};

// old file ctor
template <class T> PagedImage<T>::PagedImage(Table &table,
					     uInt)
: table_p(table), map_p(table, "map", 0), mask_p((PagedArray<Bool> *)0)
{
  CoordinateSystem *restoredCoords =
    CoordinateSystem::restore(table_p.keywordSet(), "coords");
  AlwaysAssert(restoredCoords != 0, AipsError);
  coords_p = *restoredCoords;

  if (table_p.tableDesc().isColumn("mask")) 
    mask_p = new PagedArray<Bool>(table_p, "mask", 0);
  throughmask_p = False;
  ::defaultValue(defaultvalue_p); 
  setTableType();
  restore_units();
};

// old file ctor
template <class T> PagedImage<T>::PagedImage(const String &filename, 
					     uInt)
: mask_p((PagedArray<Bool> *)0)
{
  buildTable(filename);
  map_p = PagedArray<T>(table_p, "map", 0);

  CoordinateSystem *restoredCoords =
    CoordinateSystem::restore(table_p.keywordSet(), "coords");
  AlwaysAssert(restoredCoords != 0, AipsError);
  coords_p = *restoredCoords;
  if (table_p.tableDesc().isColumn("mask")) 
    mask_p = new PagedArray<Bool>(table_p, "mask", 0);
  throughmask_p = False;
  ::defaultValue(defaultvalue_p); 
  setTableType();
  restore_units();
};

// the copy constructor: reference semantics
template <class T> PagedImage<T>::PagedImage(const PagedImage<T> &other)
: ImageInterface<T>(other.coords_p, other.throughmask_p), 
  table_p(other.table_p), map_p(other.map_p), 
  mask_p((PagedArray<Bool> *)0),
  defaultvalue_p(other.defaultvalue_p)
{
  units_p = other.units_p;
  if (other.mask_p) mask_p = new PagedArray<Bool>(*(other.mask_p));
};

// destructor
template <class T> PagedImage<T>::~PagedImage()
{
    if (mask_p) delete mask_p;

    if (table_p.isWritable()) {
	save_units();
	// Update the coordinates
	if (table_p.keywordSet().isDefined("coords")) {
	    table_p.rwKeywordSet().removeField("coords");
	}
	AlwaysAssert(coords_p.save(table_p.rwKeywordSet(), "coords"),
		     AipsError);
    }
}

// assignment operator
template <class T> 
PagedImage<T> &PagedImage<T>::operator=(const PagedImage<T> &other)
{
  if (this != &other)
    {
      coords_p = other.coords_p;
      units_p = other.units_p;
      throughmask_p = other.throughmask_p;
      table_p = other.table_p;
      map_p = other.map_p;
      if (other.mask_p) mask_p = new PagedArray<Bool>(*(other.mask_p));
      defaultvalue_p = other.defaultvalue_p;
    } 
  return *this;
}

// a default-constructed PagedImage is automatically given a temporary
// name.  If you decide to make it permanent, you will want to set
// its name.
template <class T> void PagedImage<T>::rename(const String &newName)
{
    table_p.rename(newName, Table::New);
}

// returns the current Table name
template <class T> String PagedImage<T>::name() const
{
  return table_p.tableName();
}

// a default constructed PagedImage is automatically given a temporary
// TableColumn row number.  You may change it here.
template <class T> void PagedImage<T>::changeRowNumber(uInt rowNum)
{
/*  temporarily disabled
  map_p.setRowNumber(rowNum);
  if (mask_p) mask_p->setRowNumber(rowNum);
 end of temporary disable */ 
}

// returns the current TableColumn row number 
template <class T> uInt PagedImage<T>::rowNumber() const
{
  return map_p.rowNumber();
}

// return shape of the map.
template <class T> IPosition PagedImage<T>::shape() const
{
  return map_p.shape();
}

template<class T> void PagedImage<T>::resize(const IPosition &newShape)
{
  if (newShape.nelements() != coords_p.nPixelAxes())
    throw(AipsError("PagedImage<T>::resize: coordinate info is "
		    "the incorrect shape."));
  map_p.resize(newShape);
  if (mask_p) mask_p->resize(newShape);
}

template <class T> 
Bool PagedImage<T>::setCoordinateInfo(const CoordinateSystem &coords)
{
    logSink() << LogOrigin ("PagedImage<T>", "setCoordinateInfo(const "
			    "CoordinateSystem &coords)",  WHERE);

    Bool ok = ImageInterface<T>::setCoordinateInfo(coords);
    if (ok ) {
	if (table_p.isWritable()) {
	    // Update the coordinates
	    if (table_p.keywordSet().isDefined("coords")) {
		table_p.rwKeywordSet().removeField("coords");
	    }
	    if (!(coords_p.save(table_p.rwKeywordSet(), "coords"))) {
		logSink() << LogIO::SEVERE << "Error saving coordinates in "
		    "table" << LogIO::POST;
		ok = False;
	    }
	} else {
	    logSink() << LogIO::NORMAL <<
		"Table is not writable: not saving coordinates to disk." <<
		LogIO::POST;
	}
    }
    return ok;
}

template<class T> LogIO &PagedImage<T>::logSink()
{
    return sink_p;
}
  
template <class T> 
Bool PagedImage<T>::getSlice(COWPtr<Array<T> > &buffer, 
			     const IPosition &start, const IPosition &shape, 
			     const IPosition &stride,
			     Bool removeDegenerateAxes) const
{
  Slicer theSlice(start, shape, stride, Slicer::endIsLength);
  return getSlice(buffer, theSlice, removeDegenerateAxes);
}


template <class T> 
Bool PagedImage<T>::getSlice(COWPtr<Array<T> > &buffer, 
			     const Slicer &theSlice, 
			     Bool removeDegenerateAxes) const
{
  Bool val = map_p.getSlice(buffer, theSlice, removeDegenerateAxes);
  if (mask_p) {
    COWPtr<Array<Bool> > mask;
    mask_p->getSlice(mask, theSlice, removeDegenerateAxes);
    // use maskedarrays to do all the work
    buffer.rwRef()(mask.ref() == True) = defaultvalue_p;
  }
  return val;
}

template <class T> 
Bool PagedImage<T>::getSlice(Array<T> &buffer, const IPosition &start, 
			     const IPosition &shape, const IPosition &stride,
			     Bool removeDegenerateAxes)
{
  Slicer theSlice(start, shape, stride, Slicer::endIsLength);
  return getSlice(buffer, theSlice, removeDegenerateAxes);
}

template <class T> Bool PagedImage<T>::getSlice(Array<T> &buffer, 
					   const Slicer &theSlice, 
					   Bool removeDegenerateAxes)
{
  Bool val = map_p.getSlice(buffer, theSlice, removeDegenerateAxes);
  if (mask_p) {
    Array<Bool> mask;
    mask_p->getSlice(mask, theSlice, removeDegenerateAxes);
    // use maskedarrays to do all the work
    buffer(mask == True) = defaultvalue_p;
  }
  return val;
}

template <class T> 
void PagedImage<T>::putSlice(const Array<T> &sourceBuffer, 
			     const IPosition &where,
			     const IPosition &stride)
{
  if(throughmask_p || !mask_p) map_p.putSlice(sourceBuffer,where,stride);
  else if (mask_p){
    Array<T> map;
    Array<Bool> mask;
    IPosition shape(sourceBuffer.shape());
    mask_p->getSlice(mask, where, shape, stride, True);
    map_p.getSlice(map, where, shape, stride, True);
    // use maskedarrays to do all the work.
    map(mask==False) = sourceBuffer;
    map_p.putSlice(map,where,stride);
  } else
    throw(AipsError("PagedImage<T>::putSlice - throughmask==False but no "
		    "mask exists."));
}

// apply a function to all elements of the map
template <class T> void PagedImage<T>::apply(T (*function)(T))
{
  if (throughmask_p || !mask_p) map_p.apply(function);
  else if (mask_p){
    Array<T> map;
    IPosition origin(map_p.ndim(),0);
    IPosition stride(map_p.ndim(),1);
    map_p.getSlice(map, origin, map_p.shape(), stride, True);
    map.apply(function);
    putSlice(map, origin, stride);
  } else
    throw(AipsError("PagedImage<T>::apply() - throughmask==False but no "
		    "mask exists."));
}

// apply a function to all elements of a const map;
template <class T> void PagedImage<T>::apply(T (*function)(const T &))
{
  if (throughmask_p || !mask_p) map_p.apply(function);
  else if (mask_p){
    Array<T> map;
    IPosition origin(map_p.ndim(),0);
    IPosition stride(map_p.ndim(),1);
    map_p.getSlice(map, origin, map_p.shape(), stride, True);
    map.apply(function);
    putSlice(map, origin, stride);
  } else
    throw(AipsError("PagedImage<T>::apply() - throughmask==False but no "
		    "mask exists."));
}

template <class T> void PagedImage<T>::apply(const Functional<T,T> &function)
{
  if (throughmask_p || !mask_p) map_p.apply(function);
  else if (mask_p){
    ArrayLattice<T> map;
    IPosition origin(map_p.ndim(),0);
    IPosition stride(map_p.ndim(),1);
    map_p.getSlice(map.asArray(), origin, map_p.shape(), stride, True);
    map.apply(function);
    putSlice(map.asArray(), origin, stride);
  } else
    throw(AipsError("PagedImage<T>::apply() - throughmask==False but no "
		    "mask exists."));
}

// function which returns True if the image has a mask, returns False 
// otherwise.
template <class T> Bool PagedImage<T>::isMasked() const
{
  return (mask_p) ? True : False;
}

#if defined(__GNUG__)
typedef Lattice<Bool> gpp_bug_1;
#endif

// return the whole mask Lattice to allow iteration or Lattice functions.
template <class T> const Lattice<Bool> &PagedImage<T>::mask() const
{
  if (!mask_p)
    throw(AipsError("PagedImage<T>::mask(): no mask in ArrayImage."));
  return *mask_p;
}

// return the whole mask Lattice to allow iteration or Lattice functions.
template <class T> Lattice<Bool> &PagedImage<T>::mask()
{ 
  if (!mask_p)
    throw(AipsError("PagedImage<T>::mask(): no mask in ArrayImage."));
  return *mask_p;
}

// function to set the Magic Value.  This value will be returned when 
// access to a masked element is attempted.
template <class T> void PagedImage<T>::setDefaultValue(const T &newValue)
{
  defaultvalue_p = newValue;
}

// function to return the value of the stored MagicValue.
template <class T> const T &PagedImage<T>::defaultValue() const
{
  return defaultvalue_p;
}

// return the table holding the data
template <class T> Table PagedImage<T>::table()
{
  return table_p;
}

// the rvalue subscript operator.
template <class T> T PagedImage<T>::getAt(const IPosition &where) const
{
  if (mask_p && mask_p->operator()(where))
    // mask is True
    return defaultvalue_p;
  else
    // mask is False 
    return map_p(where);
}

// temporary stand-in for the lvalue subscript operator
template <class T> 
void PagedImage<T>::putAt(const T &value, const IPosition &where) 
{
  if((throughmask_p || !mask_p) || (mask_p && !mask_p->operator()(where))) 
    map_p(where) = value;
}

template<class T>
const RecordInterface &PagedImage<T>::miscInfo() const
{
    static TableRecord *use_if_none_in_table = 0; // A small leak if set
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
Bool PagedImage<T>::setMiscInfo(const RecordInterface &newInfo)
{
    Bool writable = table_p.isWritable();
    if (!writable) {
	return False;
    }
    if (table_p.keywordSet().isDefined("miscinfo")) {
	table_p.rwKeywordSet().removeField("miscinfo");
    }
    table_p.rwKeywordSet().defineRecord("miscinfo", newInfo);
    return True;
}


// 
template <class T> 
RO_LatticeIterInterface<T> *PagedImage<T>::makeIter(
				 const LatticeNavigator &navigator) const
{
  return new RO_PagedArrIter<T>(map_p, navigator);
}

// 
template <class T> 
RO_LatticeIterInterface<T> *PagedImage<T>::makeIter(
                                      const IPosition &cursorShape) const
{
  return new RO_PagedArrIter<T>(map_p, cursorShape);
}

// 
template <class T> 
LatticeIterInterface<T> *PagedImage<T>::makeIter(
				 const LatticeNavigator &navigator)
{
  return new PagedArrIter<T>(map_p, navigator);
}

// 
template <class T> 
LatticeIterInterface<T> *PagedImage<T>::makeIter(
                                      const IPosition &cursorShape)
{
  return new PagedArrIter<T>(map_p, cursorShape);
}

template <class T> void PagedImage<T>::buildTable(const String &filename)
{
  table_p = Table(filename, Table::Update);
}

template <class T> 
void PagedImage<T>::buildTable(const String &filename) const
{
  // Logically const - we are opening 'filename' R/O
  PagedImage<T> *This = (PagedImage<T> *)this;
  This->table_p = Table(filename, Table::Old);
}

// check for symmetry across data members
template <class T> Bool PagedImage<T>::ok() const
{
  int okay = (map_p.ndim() == coords_p.nPixelAxes());
  if (mask_p) okay = okay && (mask_p->ndim() == coords_p.nPixelAxes());
  okay = okay &&  !table_p.isNull();
  return okay ? True : False;
}

// default constructor
template <class T> PagedImage<T>::PagedImage()
{
  // does nothing
}

  // addition/assignment operator. Masks are summed, i.e. if the pixel is 
  // masked in either image, it is masked in the final.
template <class T> 
PagedImage<T> &PagedImage<T>::operator+=(const PagedImage<T> &other)
{
    logSink() << LogOrigin("PagedImage<T>", 
			   "operator+=(const PagedImage<T> &other)",
			   WHERE) << LogIO::DEBUGGING;
    logSink() << "Adding other to our pixels" << endl;
    report_mask();
    check_conformance(other);
    logSink() << LogIO::POST;
  
    IPosition cursorShape(this->niceCursorShape(this->maxPixels() - 1));
    LatticeIterator<T> toiter(*this, cursorShape);
    RO_LatticeIterator<T> otheriter(other, cursorShape);

    // Have to cover all cases
    Bool writeThis=ToBool(this->isMasked()&&!this->throughmask_p);
    Bool writeOther=ToBool(other.isMasked()&&!other.throughmask_p);
    if (writeThis&&writeOther) {
	LatticeIterator<Bool> maskiter(this->mask(), cursorShape);
	RO_LatticeIterator<Bool> othermaskiter(other.mask(), cursorShape);
	
	for(toiter.reset(), 
		otheriter.reset(), 
		maskiter.reset(), 
		othermaskiter.reset(); 
	    !toiter.atEnd();
	    toiter++, maskiter++, otheriter++, othermaskiter++) {
	    maskiter.cursor().ac()=maskiter.cursor().ac() || 
		othermaskiter.cursor().ac();
	    toiter.cursor().ac()(maskiter.cursor().ac()) += 
		otheriter.cursor().ac()(maskiter.cursor().ac());
	}
    } else if(!writeThis&&writeOther) {
	mask_p = new PagedArray<Bool>(this->shape(), table_p, "mask", 0);
	LatticeIterator<Bool> maskiter(this->mask(), cursorShape);
	RO_LatticeIterator<Bool> othermaskiter(other.mask(), cursorShape);
	for(toiter.reset(), 
		otheriter.reset(), 
		maskiter.reset(), 
		othermaskiter.reset(); 
	    !toiter.atEnd();
	    toiter++, maskiter++, otheriter++, othermaskiter++) {
	    maskiter.cursor().ac()=othermaskiter.cursor().ac();
	    toiter.cursor().ac()(maskiter.cursor().ac()) += 
		otheriter.cursor().ac()(maskiter.cursor().ac());
	}
    } else if(writeThis&&!writeOther) {
	LatticeIterator<Bool> maskiter(this->mask(), cursorShape);
	for(toiter.reset(), otheriter.reset(), maskiter.reset(); 
	    !toiter.atEnd(); toiter++, maskiter++, otheriter++) {
	    toiter.cursor().ac()(maskiter.cursor().ac()) += 
		otheriter.cursor().ac()(maskiter.cursor().ac());
	}
    } else {
	IPosition cursorShape(this->niceCursorShape(this->maxPixels() - 1));
	LatticeIterator<T> toiter(*this, cursorShape);
	RO_LatticeIterator<T> otheriter(other, cursorShape);
	
	for(toiter.reset(), otheriter.reset(); 
	    !toiter.atEnd(); toiter++, otheriter++) {
	    toiter.cursor() +=otheriter.cursor();
	}
    }
    return *this;
}

  // addition/assignment operator.
template <class T> 
PagedImage<T> &PagedImage<T>::operator+=(const Lattice<T> &other)
{
    logSink() << LogOrigin("PagedImage<T>", 
			   "operator+=(const Lattice<T> &other)", WHERE) <<
	LogIO::DEBUGGING << "Adding other to our pixels" << endl;

    report_mask();
    check_conformance(other);
    logSink() << LogIO::POST;

    IPosition cursorShape(this->niceCursorShape(this->maxPixels() - 1));
    LatticeIterator<T> toiter(*this, cursorShape);
    RO_LatticeIterator<T> otheriter(other, cursorShape);
    if(isMasked()&&!throughmask_p) {
	RO_LatticeIterator<Bool> maskiter(this->mask(), cursorShape);
	for(toiter.reset(), otheriter.reset(), maskiter.reset(); 
	    !toiter.atEnd(); toiter++, maskiter++, otheriter++) {
	    toiter.cursor()(maskiter.cursor()) +=
		otheriter.cursor()(maskiter.cursor());
	}
    } else {
	for(toiter.reset(), otheriter.reset(); !toiter.atEnd();
	    toiter++, otheriter++) {
	    toiter.cursor()+=otheriter.cursor();
	}
    }
    return *this;
}

template <class T> 
PagedImage<T> &PagedImage<T>::operator+=(const T &val)
{
    logSink() << LogOrigin("PagedImage<T>", "operator+=(const T &val)", 
			   WHERE) << LogIO::DEBUGGING;
    logSink() << "Modifying pixels, val=" << val << " ";

    report_mask();
    logSink() << LogIO::POST;

    IPosition cursorShape(this->niceCursorShape(this->maxPixels() - 1));
    LatticeIterator<T> toiter(*this, cursorShape);
    if(isMasked()&&!throughmask_p) {
	RO_LatticeIterator<Bool> maskiter(this->mask(), cursorShape);
	for(toiter.reset(), maskiter.reset(); !toiter.atEnd();
	    toiter++, maskiter++) {
	    toiter.cursor()(maskiter.cursor())+=val;
	}
    } else {
	for(toiter.reset(); !toiter.atEnd(); toiter++) {
	    toiter.cursor()+=val;
	}
    }
    return *this;
}

template<class T> void PagedImage<T>::save_units()
{
    Bool writable = table_p.isWritable();
    if (!writable) {
	return;
    }
    if (table_p.keywordSet().isDefined("units")) {
	table_p.rwKeywordSet().removeField("units");
    }
    table_p.rwKeywordSet().define("units", units_p.getName());
}

template<class T> void PagedImage<T>::restore_units()
{
    logSink() << LogOrigin("PagedImage<T>", "restore_units()", WHERE);
    String unitName;
    if (table_p.keywordSet().isDefined("units")) {
	if (table_p.keywordSet().dataType("units") != TpString) {
	    logSink() << 
		"'units' keyword in image table is not a string! Units not restored." <<
		LogIO::SEVERE << LogIO::POST;
	} else {
	    table_p.keywordSet().get("units", unitName);
	}
    }

    if (unitName == "") {
	// Empty unit, but I guess we should overwrite!
	setUnits(unitName);
	return;
    }

    // OK, non-empty unit, see if it's valid, if not try some known things to make a valid
    // unit out of it.
    if (! UnitVal::check(unitName)) {
	// Beam and Pixel are the most common undefined units
	UnitMap::putUser("Pixel",UnitVal(1.0),"Pixel unit");
	UnitMap::putUser("Beam",UnitVal(1.0),"Beam area");
    }
    if (! UnitVal::check(unitName)) {
	// OK, maybe we need FITS
	UnitMap::addFITS();
    }
    if (! UnitVal::check(unitName)) {
	// I give up!
	logSink() << LogIO::SEVERE << "Unit '" << unitName << "' is unknown."
	    " Not restoring units" << LogIO::POST;
	return;
    }

    // Cool, the unit is known.
    setUnits(unitName);
}

template<class T> void PagedImage<T>::report_mask()
{
    logSink() << "\tmasking=" << isMasked() << ", " <<
	"writeThrough=" << throughmask_p;
}

template<class T> void PagedImage<T>::check_conformance(
  const Lattice<T> &other)
{
    if (! this->conform(other)) {
	logSink() << "this and other do not conform (" <<
	    this->shape() << " != " << other.shape() << ")" << LogIO::EXCEPTION;
    }
}

template<class T> uInt PagedImage<T>::cacheSize() const
{
    return map_p.cacheSize();
}

template<class T> void PagedImage<T>::setCacheSize(uInt howManyPixels)
{
    map_p.setCacheSize(howManyPixels);
    if (mask_p) {
	mask_p->setCacheSize(howManyPixels);
    }
}

template<class T>
void PagedImage<T>::setCacheSizeFromPath(const IPosition& sliceShape,
					 const IPosition& windowStart,
					 const IPosition& windowLength,
					 const IPosition& axisPath)
{
    map_p.setCacheSizeFromPath(sliceShape,windowStart,windowLength,axisPath);
    if (mask_p) {
	mask_p->setCacheSizeFromPath(sliceShape,windowStart,windowLength,
				     axisPath);
    }
}

template<class T> void PagedImage<T>::clearCache() const
{
    map_p.clearCache();
    if (mask_p) {
	mask_p->clearCache();
    }
}


template<class T>
void PagedImage<T>::showCacheStatistics(ostream &os)
{
    os << "Pixel statistics : ";
    map_p.showCacheStatistics(os);
    if (mask_p) {
	os << "Mask statistics : ";
	mask_p->showCacheStatistics(os);
    }
}

template<class T>
uInt PagedImage<T>::maxPixels() const
{
    return map_p.maxPixels();
}

template<class T>
IPosition PagedImage<T>::niceCursorShape(uInt maxPixels) const
{
    return map_p.niceCursorShape(maxPixels);
}

template<class T> void PagedImage<T>::setTableType()
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
}
