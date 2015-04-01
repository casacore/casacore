//# LCHDF5Mask.cc: Class to define a rectangular mask of interest
//# Copyright (C) 2008
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

#include <casacore/lattices/LRegions/LCHDF5Mask.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  LCHDF5Mask::LCHDF5Mask()
  {}

  LCHDF5Mask::LCHDF5Mask (const TiledShape& latticeShape,
			  const CountedPtr<HDF5File>& file,
			  const String& maskName)
    : LCRegionSingle (latticeShape.shape()),
      itsBox (IPosition(latticeShape.shape().nelements(), 0),
	      latticeShape.shape()-1, latticeShape.shape())
  {
    setBoundingBox (itsBox.boundingBox());
    itsMask = HDF5Lattice<Bool> (latticeShape, file, maskName, "masks");
    setMaskPtr (itsMask);
  }

  LCHDF5Mask::LCHDF5Mask (const TiledShape& maskShape,
			  const LCBox& box,
			  const CountedPtr<HDF5File>& file,
			  const String& maskName)
    : LCRegionSingle (box.latticeShape()),
      itsBox (box)
  {
    // Check if box shape and mask shape are equal.
    if (itsBox.shape() != maskShape.shape()) {
      throw (AipsError ("LCHDF5Mask::LCHDF5Mask- "
			"shape of mask and box differ"));
    }
    setBoundingBox (itsBox.boundingBox());
    itsMask = HDF5Lattice<Bool> (box.latticeShape(), file, maskName, "masks");
    setMaskPtr (itsMask);
  }

  LCHDF5Mask::LCHDF5Mask (HDF5Lattice<Bool>& mask,
			  const LCBox& box)
  : LCRegionSingle (box.latticeShape()),
    itsBox (box)
  {
    // Check if box shape and mask shape are equal.
    if (itsBox.shape() != mask.shape()) {
      throw (AipsError ("LCHDF5Mask::LCHDF5Mask- "
			"shape of mask and box differ"));
    }
    setBoundingBox (itsBox.boundingBox());
    itsMask = mask;
    setMaskPtr (itsMask);
  }

  LCHDF5Mask::LCHDF5Mask (const LCHDF5Mask& other)
  : LCRegionSingle (other),
    itsBox (other.itsBox),
    itsMask(other.itsMask)
  {
    setMaskPtr (itsMask);
  }

  LCHDF5Mask::~LCHDF5Mask()
  {}

  LCHDF5Mask& LCHDF5Mask::operator= (const LCHDF5Mask& that)
  {
    if (this != &that) {
      LCRegionSingle::operator= (that);
      itsBox  = that.itsBox;
      itsMask = that.itsMask;
      setMaskPtr (itsMask);
    }
    return *this;
  }

  Bool LCHDF5Mask::operator== (const LCRegion& other) const
  {
    // Check if parent class matches.
    // If so, we can safely cast.
    if (! LCRegionSingle::operator== (other)) {
      return False;
    }
    const LCHDF5Mask& that = (const LCHDF5Mask&)other;
    // Check the box and mask.
    return  (itsBox == that.itsBox  &&  masksEqual (that));
  }


  LCRegion* LCHDF5Mask::cloneRegion() const
  {
    return new LCHDF5Mask(*this);
  }


  uInt LCHDF5Mask::advisedMaxPixels() const
  {
    return itsMask.advisedMaxPixels();
  }

  IPosition LCHDF5Mask::doNiceCursorShape (uInt maxPixels) const
  {
    return itsMask.niceCursorShape (maxPixels);
  }

  LatticeIterInterface<Bool>* LCHDF5Mask::makeIter
                                 (const LatticeNavigator& navigator,
				  Bool useRef) const
  {
    return itsMask.makeIter (navigator, useRef);
  }

  void LCHDF5Mask::flush()
  {
    itsMask.flush();
  }


  LCRegion* LCHDF5Mask::doTranslate (const Vector<Float>&,
				     const IPosition&) const
  {
    // An LCHDF5Mask cannot be translated.
    throw (AipsError ("LCHDF5Mask::translate is not supported"));
    return 0;
  }

  String LCHDF5Mask::className()
  {
    return "LCHDF5Mask";
  }

  String LCHDF5Mask::type() const
  {
    return className();
  }

  TableRecord LCHDF5Mask::toRecord (const String& tableName) const
  {
    TableRecord rec;
    defineRecordFields (rec, className());
    rec.define ("filename", itsMask.file()->getName());
    rec.define ("maskname", itsMask.arrayName());
    rec.defineRecord ("box", itsBox.toRecord (tableName));
    return rec;
  }

  LCHDF5Mask* LCHDF5Mask::fromRecord (const TableRecord& rec,
				      const String& tableName)
  {
    HDF5Lattice<Bool> mask(rec.asString("filename"), rec.asString("maskname"),
			   "masks");
    LCBox* boxPtr = (LCBox*)(LCRegion::fromRecord (rec.asRecord("box"),
						   tableName));
    LCHDF5Mask* regPtr = new LCHDF5Mask (mask, *boxPtr);
    delete boxPtr;
    return regPtr;
  }

  Bool LCHDF5Mask::isWritable() const
  {
    return itsMask.isWritable();
  }

} //# NAMESPACE CASACORE - END
