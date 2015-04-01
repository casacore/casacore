//# AxesSpecifier.cc: Specification of axes to keep or remove
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

#include <casacore/casa/Arrays/AxesSpecifier.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

AxesSpecifier::AxesSpecifier()
: itsKeep (True)
{}

AxesSpecifier::AxesSpecifier (Bool keepDegenerate)
: itsKeep (keepDegenerate)
{}

AxesSpecifier::AxesSpecifier (Bool keepDegenerate,
			      const IPosition& axisPath)
: itsPath (axisPath),
  itsKeep (keepDegenerate)
{}

AxesSpecifier::AxesSpecifier (const IPosition& keepAxes)
: itsAxes (keepAxes),
  itsKeep (False)
{}

AxesSpecifier::AxesSpecifier (const IPosition& keepAxes,
			      const IPosition& axisPath)
: itsAxes (keepAxes),
  itsPath (axisPath),
  itsKeep (False)
{}

AxesSpecifier::AxesSpecifier (const AxesSpecifier& that)
: itsAxes (that.itsAxes),
  itsPath (that.itsPath),
  itsKeep (that.itsKeep)
{}

AxesSpecifier& AxesSpecifier::operator= (const AxesSpecifier& that)
{
  if (this != &that) {
    itsAxes.resize (that.itsAxes.nelements(), False);
    itsAxes = that.itsAxes;
    itsPath.resize (that.itsPath.nelements(), False);
    itsPath = that.itsPath;
    itsKeep = that.itsKeep;
  }
  return *this;
}

AxesSpecifier::~AxesSpecifier()
{}

AxesMapping AxesSpecifier::apply (const IPosition& shape) const
{
  // Find the axes to be kept.
  // It also checks the keepAxes specification.
  IPosition keepAxes;
  uInt nrnew;
  if (itsKeep) {
    nrnew = shape.nelements();
    keepAxes = IPosition::otherAxes (nrnew, IPosition());
  } else {
    // First determine which axes have to be always kept.
    // To remove degenerate axes we use two passes
    // First find out how many axes have to be kept.
    Int naxes = shape.nelements();
    keepAxes.resize (naxes, False);
    keepAxes = 0;
    for (uInt i=0; i<itsAxes.nelements(); i++) {
      AlwaysAssert (itsAxes(i) < naxes, AipsError);
      keepAxes(itsAxes(i)) = 1;
    }
    // Now remove degenerate axes.
    nrnew = 0;
    for (Int i=0; i<naxes; i++) {
      if (keepAxes(i) != 0  ||  shape(i) != 1) {
	keepAxes(nrnew++) = i;
      }
    }
    keepAxes.resize (nrnew);
  }
  // Check and complete a possible partially given axes path.
  IPosition path = IPosition::makeAxisPath (nrnew, itsPath);
  // Fill the mappings original->new.
  // -1 means that the axis is not used.
  IPosition axisToNew(shape.nelements(), -1);
  for (uInt i=0; i<nrnew; i++) {
    uInt inx = keepAxes(path(i));
    axisToNew(inx) = i;
  }
  return AxesMapping(axisToNew);
}

} //# NAMESPACE CASACORE - END

