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


#include <trial/Arrays/AxesMapping.h>
#include <aips/Arrays/Slicer.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>


AxesMapping::AxesMapping()
: itsReordered (False)
{}

AxesMapping::AxesMapping (const IPosition& oldToNew)
: itsToNew (oldToNew),
  itsToOld (oldToNew.nelements(), -1),
  itsReordered (False)
{
  Int naxes = itsToNew.nelements();
  uInt nnew = 0;
  for (Int i=0; i<naxes; i++) {
    if (itsToNew(i) < 0) {
      itsReordered = True;
    } else {
      AlwaysAssert (itsToNew(i)<naxes, AipsError);
      itsToOld(itsToNew(i)) = i;
      if (itsToNew(i) != i) {
	itsReordered = True;
      }
      nnew++;
    }
  }
  for (uInt i=0; i<nnew; i++) {
    AlwaysAssert (itsToOld(i)>=0, AipsError);
  }
  itsToOld.resize (nnew);
}

AxesMapping::AxesMapping (const AxesMapping& that)
: itsToNew (that.itsToNew),
  itsToOld (that.itsToOld),
  itsReordered (that.itsReordered)
{}

AxesMapping& AxesMapping::operator= (const AxesMapping& that)
{
  if (this != &that) {
    itsToNew.resize (that.itsToNew.nelements(), False);
    itsToNew = that.itsToNew;
    itsToOld.resize (that.itsToOld.nelements(), False);
    itsToOld = that.itsToOld;
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
