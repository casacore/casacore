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
//#        internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA


#include "AxesMapping.h"
#include "Slicer.h"

#include <cassert>
#include <stdexcept>

namespace casacore { //# NAMESPACE CASACORE - BEGIN
  
AxesMapping::AxesMapping()
: itsRemoved   (false),
  itsReordered (false)
{}

AxesMapping::AxesMapping (const IPosition& oldToNew)
: itsToNew     (oldToNew),
  itsToOld     (oldToNew.nelements(), -1),
  itsRemoved   (false),
  itsReordered (false)
{
  int naxes = itsToNew.nelements();
  size_t nnew = 0;
  for (int i=0; i<naxes; i++) {
    if (itsToNew(i) < 0) {
      itsRemoved = true;
    } else {
      if (itsToNew(i) >= naxes)
        throw std::runtime_error("itsToNew(i) >= naxes");
      itsToOld(itsToNew(i)) = i;
      nnew++;
    }
  }
  for (size_t i=0; i<nnew; i++) {
    if (itsToOld(i)<0)
      throw std::runtime_error("itsToOld(i)<0");
    if (i > 0  && itsToOld(i) < itsToOld(i-1)) {
      itsReordered = true;
    }
  }
  itsToOld.resize (nnew);
}

IPosition AxesMapping::posToNew (const IPosition& pos) const
{
  size_t naxes = itsToNew.nelements();
  assert (pos.nelements()==naxes);
  IPosition newpos(itsToOld.nelements());
  for (size_t i=0; i<naxes; i++) {
    if (itsToNew(i) < 0) {
      if (pos(i)!=0) throw std::runtime_error("pos(i)!=0");
    } else {
      newpos(itsToNew(i)) = pos(i);
    }
  }
  return newpos;
}

IPosition AxesMapping::posToOld (const IPosition& pos) const
{
  size_t naxes = itsToOld.nelements();
  assert (pos.nelements()==naxes);
  IPosition oldpos(itsToNew.nelements(), 0);
  for (size_t i=0; i<naxes; i++) {
    oldpos(itsToOld(i)) = pos(i);
  }
  return oldpos;
}

IPosition AxesMapping::shapeToNew (const IPosition& shape) const
{
  size_t naxes = itsToNew.nelements();
  assert (shape.nelements()==naxes);
  IPosition newshape(itsToOld.nelements());
  for (size_t i=0; i<naxes; i++) {
    if (itsToNew(i) < 0) {
      if (shape(i)!=1)
        throw std::runtime_error("shape(i)!=1");
    } else {
      newshape(itsToNew(i)) = shape(i);
    }
  }
  return newshape;
}

IPosition AxesMapping::shapeToOld (const IPosition& shape) const
{
  size_t naxes = itsToOld.nelements();
  assert (shape.nelements()==naxes);
  IPosition oldshape(itsToNew.nelements(), 1);
  for (size_t i=0; i<naxes; i++) {
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

