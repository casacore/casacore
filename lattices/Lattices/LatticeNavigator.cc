//# LatticeNavigator.cc: an abstract base class to steer lattice iterators
//# Copyright (C) 1994,1995,1996,1997,1999,2000
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

#include <casacore/lattices/Lattices/LatticeNavigator.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

LatticeNavigator::~LatticeNavigator()
{
    // Nothing
}

IPosition LatticeNavigator::relativePosition() const
{
  return (position() - blc()) / increment();
}

IPosition LatticeNavigator::relativeEndPosition() const
{
  return (endPosition() - blc()) / increment();
}

IPosition LatticeNavigator::subLatticeShape() const
{
  return latticeShape();
}

IPosition LatticeNavigator::hangOverBlc() const
{
  IPosition blc(relativePosition());
  const uInt ndim = blc.nelements();
  for (uInt n = 0; n < ndim; n++)
    if (blc(n) < 0)
      blc(n) = 0;
  return blc;
}

IPosition LatticeNavigator::hangOverTrc() const
{
  IPosition trc(relativeEndPosition());
  const IPosition latticeShape(subLatticeShape());
  const uInt ndim = trc.nelements();
  DebugAssert(latticeShape.nelements() == ndim, AipsError);
  for (uInt n = 0; n < ndim; n++)
    if (trc(n) >= latticeShape(n))
      trc(n) = latticeShape(n) - 1;
  return trc;
}

void LatticeNavigator::subSection(const IPosition& blc,
				  const IPosition& trc)
{
  subSection(blc, trc, IPosition(latticeShape().nelements(),1));
}

void LatticeNavigator::subSection(const IPosition&,
				  const IPosition&, 
				  const IPosition&)
{
  throw(AipsError("LatticeNavigator::subSection(blc, trc, inc)"
	" - sub-Lattice's are not supported"));
}

IPosition LatticeNavigator::blc() const
{
  return IPosition(latticeShape().nelements(), 0);
}

IPosition LatticeNavigator::trc() const
{
  return latticeShape() - 1;
}

IPosition LatticeNavigator::increment() const
{
  return IPosition(latticeShape().nelements(), 1);
}

Bool LatticeNavigator::ok() const
{
  return True;
}

} //# NAMESPACE CASACORE - END

