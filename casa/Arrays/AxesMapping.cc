//# AxesMapping.cc: Info about mapping array axes to another order
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
//#
//# $Id$


#include <casacore/casa/Arrays/AxesMapping.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

AxesMapping::AxesMapping()
: itsRemoved   (False),
  itsReordered (False)
{}

AxesMapping::AxesMapping (const IPosition& oldToNew)
: itsToNew     (oldToNew),
  itsToOld     (oldToNew.nelements(), -1),
  itsRemoved   (False),
  itsReordered (False)
{
  Int naxes = itsToNew.nelements();
  uInt nnew = 0;
  for (Int i=0; i<naxes; i++) {
    if (itsToNew(i) < 0) {
      itsRemoved = True;
    } else {
      AlwaysAssert (itsToNew(i)<naxes, AipsError);
      itsToOld(itsToNew(i)) = i;
      nnew++;
    }
  }
  for (uInt i=0; i<nnew; i++) {
    AlwaysAssert (itsToOld(i)>=0, AipsError);
    if (i > 0  && itsToOld(i) < itsToOld(i-1)) {
      itsReordered = True;
    }
  }
  itsToOld.resize (nnew);
}

AxesMapping::AxesMapping (const AxesMapping& that)
: itsToNew     (that.itsToNew),
  itsToOld     (that.itsToOld),
  itsRemoved   (that.itsRemoved),
  itsReordered (that.itsReordered)
{}

AxesMapping& AxesMapping::operator= (const AxesMapping& that)
{
  if (this != &that) {
    itsToNew.resize (that.itsToNew.nelements(), False);
    itsToNew = that.itsToNew;
    itsToOld.resize (that.itsToOld.nelements(), False);
    itsToOld = that.itsToOld;
    itsRemoved   = that.itsRemoved;
    itsReordered = that.itsReordered;
  }
  return *this;
}

AxesMapping::~AxesMapping()
{}

IPosition AxesMapping::posToNew (const IPosition& pos) const
{
  uInt naxes = itsToNew.nelements();
  DebugAssert (pos.nelements()==naxes, AipsError);
  IPosition newpos(itsToOld.nelements());
  for (uInt i=0; i<naxes; i++) {
    if (itsToNew(i) < 0) {
      AlwaysAssert (pos(i)==0, AipsError);
    } else {
      newpos(itsToNew(i)) = pos(i);
    }
  }
  return newpos;
}

IPosition AxesMapping::posToOld (const IPosition& pos) const
{
  uInt naxes = itsToOld.nelements();
  DebugAssert (pos.nelements()==naxes, AipsError);
  IPosition oldpos(itsToNew.nelements(), 0);
  for (uInt i=0; i<naxes; i++) {
    oldpos(itsToOld(i)) = pos(i);
  }
  return oldpos;
}

IPosition AxesMapping::shapeToNew (const IPosition& shape) const
{
  uInt naxes = itsToNew.nelements();
  DebugAssert (shape.nelements()==naxes, AipsError);
  IPosition newshape(itsToOld.nelements());
  for (uInt i=0; i<naxes; i++) {
    if (itsToNew(i) < 0) {
      AlwaysAssert (shape(i)==1, AipsError);
    } else {
      newshape(itsToNew(i)) = shape(i);
    }
  }
  return newshape;
}

IPosition AxesMapping::shapeToOld (const IPosition& shape) const
{
  uInt naxes = itsToOld.nelements();
  DebugAssert (shape.nelements()==naxes, AipsError);
  IPosition oldshape(itsToNew.nelements(), 1);
  for (uInt i=0; i<naxes; i++) {
    oldshape(itsToOld(i)) = shape(i);
  }
  return oldshape;
}

Slicer AxesMapping::slicerToNew (const Slicer& slicer) const
{
  return Slicer (posToNew (slicer.start()),
		 shapeToNew (slicer.length()),
		 shapeToNew (slicer.stride()));
}

Slicer AxesMapping::slicerToOld (const Slicer& slicer) const
{
  return Slicer (posToOld (slicer.start()),
		 shapeToOld (slicer.length()),
		 shapeToOld (slicer.stride()));
}

} //# NAMESPACE CASACORE - END

