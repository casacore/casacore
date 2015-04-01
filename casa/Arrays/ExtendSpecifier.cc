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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#
//# $Id$

#include <casacore/casa/Arrays/ExtendSpecifier.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


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
  uInt nrdim = newShape.nelements();
  // Check if axes are given correctly.
  Block<Bool> flags(nrdim, False);
  fill (flags, newAxes);
  // Make the mapping of new axes to old axes.
  IPosition newToOld(nrdim, -1);
  uInt nrold = 0;
  for (uInt i=0; i<nrdim; i++) {
    if (! flags[i]) {
      newToOld(i) = nrold++;
    }
  }
  fill (flags, stretchAxes);
  // Find the axes which are new nor stretched.
  uInt nrext = newAxes.nelements();
  if (nrdim - nrext != oldShape.nelements()) {
    throw AipsError ("ExtendSpecifier - "
		     "#axes in oldShape,newShape,newAxes mismatch");
  }
  nrext += stretchAxes.nelements();
  if (nrext == 0) {
    throw AipsError ("ExtendSpecifier - new nor stretch axes given");
  }
  if (nrext >= nrdim) {
    throw AipsError ("ExtendSpecifier - no axes remaining");
  }
  itsExtendAxes.resize (nrext);
  itsOldOldAxes.resize (nrdim - nrext);
  itsOldNewAxes.resize (nrdim - nrext);
  nrext = 0;
  nrold = 0;
  // Fill the old axes (i.e. new nor stretched) in the old and new shape.
  // Check if those axes have the same length.
  // Check if stretched axes have length 1 in the old shape.
  for (uInt i=0; i<nrdim; i++) {
    if (flags[i]) {
      if (newToOld(i) >= 0  &&  oldShape(newToOld(i)) != 1) {
	throw AipsError ("ExtendSpecifier - length of stretched axis > 1");
      }
      itsExtendAxes(nrext++) = i;
    } else {
      itsOldOldAxes(nrold) = newToOld(i);
      itsOldNewAxes(nrold++) = i;
      if (newShape(i) != oldShape(newToOld(i))) {
	throw AipsError ("ExtendSpecifier - lengths of old axis mismatch");
      }
    }
  }
}


ExtendSpecifier::ExtendSpecifier(const ExtendSpecifier& other)
: itsOldShape    (other.itsOldShape),
  itsNewShape    (other.itsNewShape),
  itsNewAxes     (other.itsNewAxes),
  itsStretchAxes (other.itsStretchAxes),
  itsExtendAxes  (other.itsExtendAxes),
  itsOldOldAxes  (other.itsOldOldAxes),
  itsOldNewAxes  (other.itsOldNewAxes)
{}
  
ExtendSpecifier::~ExtendSpecifier()
{}

ExtendSpecifier& ExtendSpecifier::operator= (const ExtendSpecifier& other)
{
  if (this != &other) {
    itsOldShape.resize (other.itsOldShape.nelements());
    itsNewShape.resize (other.itsNewShape.nelements());
    itsNewAxes.resize  (other.itsNewAxes.nelements());
    itsStretchAxes.resize (other.itsStretchAxes.nelements());
    itsExtendAxes.resize  (other.itsExtendAxes.nelements());
    itsOldOldAxes.resize  (other.itsOldOldAxes.nelements());
    itsOldNewAxes.resize  (other.itsOldNewAxes.nelements());
    itsOldShape    = other.itsOldShape;
    itsNewShape    = other.itsNewShape;
    itsNewAxes     = other.itsNewAxes;
    itsStretchAxes = other.itsStretchAxes;
    itsExtendAxes  = other.itsExtendAxes;
    itsOldOldAxes  = other.itsOldOldAxes;
    itsOldNewAxes  = other.itsOldNewAxes;
  }
  return *this;
}

void ExtendSpecifier::fill (Block<Bool>& flags, const IPosition& axes) const
{
  Int nrdim = flags.nelements();
  for (uInt i=0; i<axes.nelements(); i++) {
    Int axis = axes(i);
    if (axis < 0  ||  axis >= nrdim) {
      throw AipsError ("ExtendSpecifier - invalid axis given (<0 or >=nrdim)");
    }
    if (flags[axis]) {
      throw AipsError ("ExtendSpecifier - axis multiply specified");
    }
    flags[axis] = True;
  }
}

Slicer ExtendSpecifier::convert (IPosition& shape, const Slicer& section) const
{
  uInt nrdim = itsNewShape.nelements();
  DebugAssert (nrdim == section.ndim(), AipsError);
  uInt nrr = nrdim - itsNewAxes.nelements();
  uInt nrold = itsOldOldAxes.nelements();
  // Create a Slicer for the section without the new axes.
  // Copy the blc,trc,stride for the old axes.
  // This means we have to create a Slicer for those axes only.
  IPosition blc(nrr, 0);
  IPosition len(nrr, 1);
  IPosition inc(nrr, 1);
  shape.resize (nrdim);
  shape = 1;
  for (uInt j=0; j<nrold; j++) {
    uInt oldAxis = itsOldOldAxes(j);
    uInt newAxis = itsOldNewAxes(j);
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
  for (uInt i=0; i<itsOldNewAxes.nelements(); i++) {
    newShape(itsOldNewAxes(i)) = shape(itsOldOldAxes(i));
  }
  return newShape;
}

} //# NAMESPACE CASACORE - END

