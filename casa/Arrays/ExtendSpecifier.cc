//# ExtendSpecifier.cc: Specification of new and stretched lattice axes
//# Copyright (C) 2001
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include "ExtendSpecifier.h"
#include "Slicer.h"
#include "ArrayError.h"

#include <cassert>

namespace casacore { //# NAMESPACE CASACORE - BEGIN
  
ExtendSpecifier::ExtendSpecifier()
{}

ExtendSpecifier::ExtendSpecifier (const IPosition& oldShape,
				  const IPosition& newShape,
				  const IPosition& newAxes,
				  const IPosition& stretchAxes)
: itsOldShape    (oldShape),
  itsNewShape    (newShape),
  itsNewAxes     (newAxes),
  itsStretchAxes (stretchAxes)
{
  size_t nrdim = newShape.nelements();
  // Check if axes are given correctly.
  std::unique_ptr<bool[]> flags(new bool[nrdim]);
  std::fill_n(flags.get(), nrdim, false);
  fill (flags.get(), nrdim, newAxes);
  // Make the mapping of new axes to old axes.
  IPosition newToOld(nrdim, -1);
  size_t nrold = 0;
  for (size_t i=0; i<nrdim; i++) {
    if (! flags[i]) {
      newToOld(i) = nrold++;
    }
  }
  fill (flags.get(), nrdim, stretchAxes);
  // Find the axes which are new nor stretched.
  size_t nrext = newAxes.nelements();
  if (nrdim - nrext != oldShape.nelements()) {
    throw ArrayError ("ExtendSpecifier - "
		     "#axes in oldShape,newShape,newAxes mismatch");
  }
  nrext += stretchAxes.nelements();
  if (nrext == 0) {
    throw ArrayError ("ExtendSpecifier - new nor stretch axes given");
  }
  if (nrext >= nrdim) {
    throw ArrayError ("ExtendSpecifier - no axes remaining");
  }
  itsExtendAxes.resize (nrext);
  itsOldOldAxes.resize (nrdim - nrext);
  itsOldNewAxes.resize (nrdim - nrext);
  nrext = 0;
  nrold = 0;
  // Fill the old axes (i.e. new nor stretched) in the old and new shape.
  // Check if those axes have the same length.
  // Check if stretched axes have length 1 in the old shape.
  for (size_t i=0; i<nrdim; i++) {
    if (flags[i]) {
      if (newToOld(i) >= 0  &&  oldShape(newToOld(i)) != 1) {
	throw ArrayError ("ExtendSpecifier - length of stretched axis > 1");
      }
      itsExtendAxes(nrext++) = i;
    } else {
      itsOldOldAxes(nrold) = newToOld(i);
      itsOldNewAxes(nrold++) = i;
      if (newShape(i) != oldShape(newToOld(i))) {
	throw ArrayError ("ExtendSpecifier - lengths of old axis mismatch");
      }
    }
  }
}

void ExtendSpecifier::fill (bool* flags, size_t nrdim, const IPosition& axes) const
{
  for (size_t i=0; i<axes.nelements(); i++) {
    ssize_t axis = axes(i);
    if (axis < 0  ||  axis >= ssize_t(nrdim)) {
      throw ArrayError ("ExtendSpecifier - invalid axis given (<0 or >=nrdim)");
    }
    if (flags[axis]) {
      throw ArrayError ("ExtendSpecifier - axis multiply specified");
    }
    flags[axis] = true;
  }
}

Slicer ExtendSpecifier::convert (IPosition& shape, const Slicer& section) const
{
  size_t nrdim = itsNewShape.nelements();
  assert (nrdim == section.ndim());
  size_t nrr = nrdim - itsNewAxes.nelements();
  size_t nrold = itsOldOldAxes.nelements();
  // Create a Slicer for the section without the new axes.
  // Copy the blc,trc,stride for the old axes.
  // This means we have to create a Slicer for those axes only.
  IPosition blc(nrr, 0);
  IPosition len(nrr, 1);
  IPosition inc(nrr, 1);
  shape.resize (nrdim);
  shape = 1;
  for (size_t j=0; j<nrold; j++) {
    size_t oldAxis = itsOldOldAxes(j);
    size_t newAxis = itsOldNewAxes(j);
    blc(oldAxis) = section.start()(newAxis);
    len(oldAxis) = section.length()(newAxis);
    inc(oldAxis) = section.stride()(newAxis);
    shape(newAxis) = len(oldAxis);
  }
  return Slicer(blc, len, inc);
}

IPosition ExtendSpecifier::convertNew (const IPosition& shape) const
{
  IPosition newShape (itsNewShape.nelements(), 1);
  for (size_t i=0; i<itsOldNewAxes.nelements(); i++) {
    newShape(itsOldNewAxes(i)) = shape(itsOldOldAxes(i));
  }
  return newShape;
}

} //# NAMESPACE CASACORE - END

