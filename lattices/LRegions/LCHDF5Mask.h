//# LCHDF5Mask.h: Class to define a rectangular mask of interest
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

#ifndef LATTICES_LCHDF5MASK_H
#define LATTICES_LCHDF5MASK_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/lattices/Lattices/HDF5Lattice.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary>
  // Class to define a rectangular mask as a region
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="" date="" tests="">
  // </reviewed>

  // <prerequisite>
  //   <li> <linkto class=LCRegionSingle>LCRegionSingle</linkto>
  // </prerequisite>

  // <synopsis> 
  // The LCHDF5Mask class is a specialization of class
  // <linkto class=LCRegionSingle>LCRegionSingle</linkto>.
  // It holds a mask for an HDF5Image in an HDF5Lattice<Bool> object.
  // </synopsis> 

  class LCHDF5Mask: public LCRegionSingle
  {
  public:
    LCHDF5Mask();

    // Construct an HDF5Mask object for (part of) a lattice.
    // It is put in group Masks of the HDF5 file.
    // The group is created if not existing yet.
    // The box defines the position of the mask.
    // The default mask shape is the lattice shape.
    // <group>
    LCHDF5Mask (const TiledShape& latticeShape,
		const CountedPtr<HDF5File>& file, const String& maskName);
    LCHDF5Mask (const TiledShape& maskShape, const LCBox& box,
		const CountedPtr<HDF5File>& file, const String& maskName);
    LCHDF5Mask (HDF5Lattice<Bool>& mask, const LCBox& box);
    // </group>

    // Copy constructor (copy semantics).
    LCHDF5Mask (const LCHDF5Mask& other);

    // Destructor
    virtual ~LCHDF5Mask();

    // Assignment (reference semantics).
    LCHDF5Mask& operator= (const LCHDF5Mask& other);

    // Comparison
    virtual Bool operator==(const LCRegion& other) const;

    // Make a copy of the derived object.
    virtual LCRegion* cloneRegion() const;

    // This function is used by the LatticeIterator class to generate an
    // iterator of the correct type for this Lattice. Not recommended
    // for general use. 
    virtual LatticeIterInterface<Bool>* makeIter
                                   (const LatticeNavigator& navigator,
				    Bool useRef) const;

    // Returns the maximum recommended number of pixels for a cursor.
    // This is the number of pixels in a tile. 
    virtual uInt advisedMaxPixels() const;

    // Help the user pick a cursor for most efficient access.
    virtual IPosition doNiceCursorShape (uInt maxPixels) const;

    // Flush the data (but do not unlock).
    virtual void flush();

    // Get the class name (to store in the record).
    static String className();

    // Region type. Returns class name.
    virtual String type() const;

    // Convert the (derived) object to a record.
    virtual TableRecord toRecord (const String& tableName) const;

    // Convert correct object from a record.
    static LCHDF5Mask* fromRecord (const TableRecord&,
				   const String& tablename);

    // An LCHDF5Mask is writable if the underlying HDF5Lattice is.
    virtual Bool isWritable() const;

  protected:
    // Construct another LCHDF5Mask (for e.g. another lattice) by moving
    // this one. It recalculates the bounding mask.
    // A positive translation value indicates "to right".
    virtual LCRegion* doTranslate (const Vector<Float>& translateVector,
				   const IPosition& newLatticeShape) const;

  private:
    // Create the object from a record (for an existing mask).
    LCHDF5Mask (HDF5Lattice<Bool>& mask,
		const IPosition& blc,
		const IPosition& latticeShape);


    LCBox             itsBox;
    HDF5Lattice<Bool> itsMask;
  };



} //# NAMESPACE CASACORE - END

#endif
