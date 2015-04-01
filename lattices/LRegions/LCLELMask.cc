//# LCLELMask.cc: Class to define a mask as a LEL expression
//# Copyright (C) 2000,2001
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

#include <casacore/lattices/LRegions/LCLELMask.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LCLELMask::LCLELMask()
{}

LCLELMask::LCLELMask (const LatticeExpr<Bool>& expr)
: LCRegionSingle (expr.shape()),
  itsExpr (expr)
{
  IPosition shp = expr.shape();
  itsBox = LCBox(IPosition(shp.nelements(), 0),
		 shp-1,
		 shp);
  setBoundingBox (itsBox.boundingBox());
  setMaskPtr (itsExpr);
}

LCLELMask::LCLELMask (const LCLELMask& that)
: LCRegionSingle (that),
  itsBox (that.itsBox),
  itsExpr(that.itsExpr)
{
    setMaskPtr (itsExpr);
}
 
LCLELMask::~LCLELMask()
{}

LCLELMask& LCLELMask::operator= (const LCLELMask& that)
{
  if (this != &that) {
    LCRegionSingle::operator= (that);
    itsBox  = that.itsBox;
    itsExpr = that.itsExpr;
    setMaskPtr (itsExpr);
   }
  return *this;
}

Bool LCLELMask::operator== (const LCRegion& that) const
{
  // Check if parent class matches.
  // If so, we can safely cast.
  if (! LCRegionSingle::operator== (that)) {
    return False;
  }
  const LCLELMask& That = dynamic_cast<const LCLELMask&>(that);
  // Check the box and mask.
  return  (itsBox == That.itsBox  &&  masksEqual (That));
}


LCRegion* LCLELMask::cloneRegion() const
{
   return new LCLELMask(*this);
}


Bool LCLELMask::lock (FileLocker::LockType type, uInt nattempts)
{
   return itsExpr.lock (type, nattempts);
}
void LCLELMask::unlock()
{
  itsExpr.unlock();
}
Bool LCLELMask::hasLock (FileLocker::LockType type) const
{
  return itsExpr.hasLock (type);
}
void LCLELMask::resync()
{
  itsExpr.resync();
}
void LCLELMask::tempClose()
{
  itsExpr.tempClose();
}
void LCLELMask::reopen()
{
  itsExpr.reopen();
}


LCRegion* LCLELMask::doTranslate (const Vector<Float>&,
				  const IPosition&) const
{
  // An LCLELMask cannot be translated.
  throw (AipsError ("LCLELMask::translate is not supported"));
  return 0;
}

String LCLELMask::className() 
{
   return "LCLELMask";
}

String LCLELMask::type() const
{
   return className();
}

TableRecord LCLELMask::toRecord (const String&) const
{
  throw (AipsError ("LCLELMask::toRecord is not supported"));
  return TableRecord();
}

} //# NAMESPACE CASACORE - END

