//# LCMask.h: Class to define a rectangular mask as a temporary region
//# Copyright (C) 2000
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

#if !defined(AIPS_LCMASK_H)
#define AIPS_LCMASK_H


//# Includes
#include <trial/Lattices/LCBox.h>
#include <aips/Lattices/Lattice.h>


// <summary>
// Class to define a rectangular mask as a temporary region
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LCRegion>LCRegion</linkto>
// </prerequisite>

// <synopsis> 
// The LCMask class is a specialization of class
// <linkto class=LCRegion>LCRegion</linkto>.
// <br>
// It can be used to define a temporary mask (e.g. for a
// <linkto class=TempImage>TempImage</linkto>).
// It is possible to define the mask for the full lattice, but one
// can also define it for part of a lattice. In the latter case a
// <linkto class=LCBox>LCBox</linkto> has to be given as well to
// define for which part of the image the mask has to be used.
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

// <todo asof="1997/11/11">
// </todo>

class LCMask: public LCRegionSingle
{
public:
  LCMask();

  // Construct an LCMask object for a full lattice with the given shape.
  // It creates a TempLattice<Bool> to hold the mask.
  explicit LCMask (const IPosition& latticeShape);

  // Construct an LCMask object for a full lattice with the shape of the mask.
  // It clones the mask object.
  explicit LCMask (Lattice<Bool>& mask);

  // Construct an LCMask object for the part of a lattice given by the box.
  // The box defines the position of the mask in the lattice.
  // The box shape and given mask shape should be equal.
  // It creates a TempImage<Bool> to hold the mask.
  LCMask (const IPosition& maskShape, const LCBox& box);

  // Construct an LCMask object for the part of a lattice given by the box.
  // The box defines the position of the mask in the lattice.
  // The box shape and given mask shape should be equal.
  // It clones the mask object.
  LCMask (Lattice<Bool>& mask, const LCBox& box);

  // Copy constructor (copy semantics).
  LCMask (const LCMask& other);

  // Destructor
  virtual ~LCMask();

  // Assignment (reference semantics).
  LCMask& operator= (const LCMask& other);

  // Comparison
  virtual Bool operator==(const LCRegion& other) const;

  // Make a copy of the derived object.
  virtual LCRegion* cloneRegion() const;

  // Handle the (un)locking.
  // <group>
  virtual Bool lock (FileLocker::LockType, uInt nattempts);
  virtual void unlock();
  virtual Bool hasLock (FileLocker::LockType) const;
  // </group>

  // Resynchronize the object with the contenta tof the possible file.
  // This function is only useful if no read-locking is used, ie.
  // if the table lock option is UserNoReadLocking or AutoNoReadLocking.
  // In that cases the table system does not acquire a read-lock, thus
  // does not synchronize itself automatically.
  virtual void resync();

  // Flush the data (but do not unlock).
  virtual void flush();

  // Temporarily close the lattice.
  // It will be reopened automatically on the next access.
  virtual void tempClose();

  // Explicitly reopen the temporarily closed lattice.
  virtual void reopen();

  // Get the class name (to store in the record).
  static String className();

  // Region type. Returns class name.
  virtual String type() const;

  // Convert the (derived) object to a record.
  // This cannot be done and results in an exception.
  virtual TableRecord toRecord (const String& tableName) const;

  // An LCMask is writable if the underlying Lattice is.
  virtual Bool isWritable() const;

protected:
  // Construct another LCMask (for e.g. another lattice) by moving
  // this one. It recalculates the bounding mask.
  // A positive translation value indicates "to right".
  virtual LCRegion* doTranslate (const Vector<Float>& translateVector,
				 const IPosition& newLatticeShape) const;

private:
  LCBox          itsBox;
  Lattice<Bool>* itsMask;
};


#endif
