//# LCLELMask.h: Class to define a mask as a LEL expression
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
//# $Id$



#ifndef LATTICES_LCLELMASK_H
#define LATTICES_LCLELMASK_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LRegions/LCRegionSingle.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/lattices/LEL/LatticeExpr.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableRecord;
class IPosition;


// <summary>
// Class to define a mask as a LEL expression
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LCRegion>LCRegion</linkto>
//   <li> <linkto class=ImageExpr>ImageExpr</linkto>
// </prerequisite>

// <synopsis> 
// The LCLELMask class is a specialization of class
// <linkto class=LCRegion>LCRegion</linkto>.
// <br>
// It can be used to define an on-the-fly mask for a lattice
// using a boolean <linkto class=LatticeExpr>LatticeExpr</linkto>.
// The contents of the mask are calculated on the fly from the expression.
// Thus the mask may change if the data in the lattice(s) used in the
// expression change.
// <note role=caution>
// This mask is not persistent, thus it cannot be saved with an image.
// Use class <linkto class=WCLELMask>WCLELMask</linkto> to have a
// persistent on-the-fly mask. It means that normally a WCLELMask should
// be used (which gets converted to an LCLELMask when applied to an image).
// </note>
// </synopsis> 

// <example>
// </example>

// <motivation>
// LCLELMask is needed to make 
// </motivation>

//# <todo asof="1998/05/20">
//#   <li> 
//# </todo>

class LCLELMask : public LCRegionSingle
{
public:
  LCLELMask();

  // Construct from vectors of world coordinates 
  // defining the box corners.  It is assumed that the
  // order of the values is in the order of the pixel axes.
  explicit LCLELMask (const LatticeExpr<Bool>& expr);

  // Copy constructor (copy semantics).
  LCLELMask (const LCLELMask& other);

  // Destructor
  virtual ~LCLELMask();

  // Assignment (copy semantics) 
  LCLELMask& operator= (const LCLELMask& other);

  // Comparison
  virtual Bool operator== (const LCRegion& other) const;

  // Clone a LCLELMask object.
  virtual LCRegion* cloneRegion() const;

  // Handle the (un)locking.
  // <group>
  virtual Bool lock (FileLocker::LockType, uInt nattempts);
  virtual void unlock();
  virtual Bool hasLock (FileLocker::LockType) const;
  // </group>

  // Resynchronize the PagedArray object with the lattice file.
  // This function is only useful if no read-locking is used, ie.
  // if the table lock option is UserNoReadLocking or AutoNoReadLocking.
  // In that cases the table system does not acquire a read-lock, thus
  // does not synchronize itself automatically.
  virtual void resync();

  // Temporarily close the lattice.
  // It will be reopened automatically on the next access.
  virtual void tempClose();

  // Explicitly reopen the temporarily closed lattice.
  virtual void reopen();

  // Returns LCLELMask
  static String className();

  // Return region type.  Returns the class name 
  virtual String type() const;
 
  // Convert the LCLELMask object to a record.
  // This cannot be done as a Lattice expression cannot be made persistent
  // (only Image expressions can, thus only WCLELMask is persistent).
  // <br>So this function throws an exception.
  virtual TableRecord toRecord (const String& tableName) const;


protected:
  // Translating an LCLELMask is not possible, so it throws an exception.
  virtual LCRegion* doTranslate (const Vector<Float>& translateVector,
				 const IPosition& newLatticeShape) const;

private:
  LCBox             itsBox;
  LatticeExpr<Bool> itsExpr;
};



} //# NAMESPACE CASACORE - END

#endif
