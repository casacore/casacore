//# LCPagedMask.cc: Class to define a rectangular mask of interest
//# Copyright (C) 1997,1998,1999,2000,2001,2002,2003
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

#include <casacore/lattices/LRegions/LCPagedMask.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableAttr.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LCPagedMask::LCPagedMask()
{}

LCPagedMask::LCPagedMask (const TiledShape& latticShape,
			  const String& tableName)
: LCRegionSingle (latticShape.shape()),
  itsBox (IPosition(latticShape.shape().nelements(), 0),
	  latticShape.shape()-1, latticShape.shape())
{
    setBoundingBox (itsBox.boundingBox());
    itsMask = PagedArray<Bool> (latticShape, tableName);
    setMaskPtr (itsMask);
}

LCPagedMask::LCPagedMask (const TiledShape& maskShape,
			  const LCBox& box,
			  const String& tableName)
: LCRegionSingle (box.latticeShape()),
  itsBox (box)
{
    // Check if box shape and mask shape are equal.
    if (itsBox.shape() != maskShape.shape()) {
	throw (AipsError ("LCPagedMask::LCPagedMask- "
			  "shape of mask and box differ"));
    }
    setBoundingBox (itsBox.boundingBox());
    itsMask = PagedArray<Bool> (maskShape, tableName);
    setMaskPtr (itsMask);
}

LCPagedMask::LCPagedMask (PagedArray<Bool>& mask,
			  const LCBox& box)
: LCRegionSingle (box.latticeShape()),
  itsBox (box)
{
    // Check if box shape and mask shape are equal.
    if (itsBox.shape() != mask.shape()) {
	throw (AipsError ("LCPagedMask::LCPagedMask- "
			  "shape of mask and box differ"));
    }
    setBoundingBox (itsBox.boundingBox());
    itsMask = mask;
    setMaskPtr (itsMask);
}

LCPagedMask::LCPagedMask (const LCPagedMask& other)
: LCRegionSingle (other),
  itsBox (other.itsBox),
  itsMask(other.itsMask)
{
    setMaskPtr (itsMask);
}

LCPagedMask::~LCPagedMask()
{}

LCPagedMask& LCPagedMask::operator= (const LCPagedMask& that)
{
    if (this != &that) {
	LCRegionSingle::operator= (that);
	itsBox  = that.itsBox;
	itsMask = that.itsMask;
	setMaskPtr (itsMask);
    }
    return *this;
}

Bool LCPagedMask::operator== (const LCRegion& other) const
{
    // Check if parent class matches.
    // If so, we can safely cast.
    if (! LCRegionSingle::operator== (other)) {
	return False;
    }
    const LCPagedMask& that = (const LCPagedMask&)other;
    // Check the box and mask.
    return  (itsBox == that.itsBox  &&  masksEqual (that));
}


LCRegion* LCPagedMask::cloneRegion() const
{
    return new LCPagedMask(*this);
}


uInt LCPagedMask::advisedMaxPixels() const
{
    return itsMask.advisedMaxPixels();
}

IPosition LCPagedMask::doNiceCursorShape (uInt maxPixels) const
{
    return itsMask.niceCursorShape (maxPixels);
}

uInt LCPagedMask::maximumCacheSize() const
{
    return itsMask.maximumCacheSize();
}

void LCPagedMask::setMaximumCacheSize (uInt howManyPixels)
{
    itsMask.setMaximumCacheSize (howManyPixels);
}

void LCPagedMask::setCacheSizeFromPath (const IPosition& sliceShape,
					const IPosition& windowStart,
					const IPosition& windowLength,
					const IPosition& axisPath)
{
    itsMask.setCacheSizeFromPath (sliceShape, windowStart, windowLength,
				  axisPath);
}

void LCPagedMask::setCacheSizeInTiles (uInt howManyTiles)
{
    itsMask.setCacheSizeInTiles (howManyTiles);
}

void LCPagedMask::clearCache()
{
    itsMask.clearCache();
}

void LCPagedMask::showCacheStatistics (ostream& os) const
{
    itsMask.showCacheStatistics (os);
}

LatticeIterInterface<Bool>* LCPagedMask::makeIter
                                   (const LatticeNavigator& navigator,
				    Bool useRef) const
{
    return itsMask.makeIter (navigator, useRef);
}


void LCPagedMask::handleDelete()
{
    // Test if the table can be deleted (i.e. is not used elsewhere).
    Table& tab(itsMask.table());
    if (tab.isMultiUsed (True)) {
      throw (AipsError("Cannot delete the mask (used in another process)"));
    }
    // Mark the table for delete, so the destructor will delete it.
    tab.markForDelete();
}

void LCPagedMask::handleRename (const String& newName, Bool overwrite)
{
    // Rename the underlying table.
    // Make sure the directory does not change.
    Table tab(itsMask.tableName(), Table::Update);
    String newnm = Path(tab.tableName()).dirName() + '/' + newName;
    if (overwrite) {
      tab.rename (newnm, Table::New);
    } else {
      tab.rename (newnm, Table::NewNoReplace);
    }
}


Bool LCPagedMask::lock (FileLocker::LockType type, uInt nattempts)
{
    // Llock the PagedArray containing the mask.
    return itsMask.lock (type, nattempts);
}
void LCPagedMask::unlock()
{
    // Unlock the PagedArray containing the mask.
    itsMask.unlock();
}
Bool LCPagedMask::hasLock (FileLocker::LockType type) const
{
    return itsMask.hasLock (type);
}
void LCPagedMask::resync()
{
    itsMask.resync();
}

void LCPagedMask::flush()
{
    itsMask.flush();
}

void LCPagedMask::tempClose()
{
    itsMask.tempClose();
}

void LCPagedMask::reopen()
{
    itsMask.reopen();
}


LCRegion* LCPagedMask::doTranslate (const Vector<Float>&,
				    const IPosition&) const
{
    // An LCPagedMask cannot be translated.
    throw (AipsError ("LCPagedMask::translate is not supported"));
    return 0;
}

String LCPagedMask::className()
{
    return "LCPagedMask";
}

String LCPagedMask::type() const
{
   return className();
}

TableRecord LCPagedMask::toRecord (const String& tableName) const
{
    TableRecord rec;
    defineRecordFields (rec, className());
    rec.defineTable ("mask", itsMask.table());
    rec.defineRecord ("box", itsBox.toRecord (tableName));
    return rec;
}

LCPagedMask* LCPagedMask::fromRecord (const TableRecord& rec,
				      const String& tableName)
{
    TableLock lockOptions(TableLock::AutoNoReadLocking);
    if (rec.tableAttributes("mask").lockOptions().readLocking()) {
        lockOptions = TableLock::AutoLocking;
    }
    Table table (rec.asTable ("mask", lockOptions));
    PagedArray<Bool> mask(table);
    LCBox* boxPtr = (LCBox*)(LCRegion::fromRecord (rec.asRecord("box"),
						   tableName));
    LCPagedMask* regPtr = new LCPagedMask (mask, *boxPtr);
    delete boxPtr;
    return regPtr;
}

Bool LCPagedMask::isWritable() const
{
    return itsMask.isWritable();
}

} //# NAMESPACE CASACORE - END

