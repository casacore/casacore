//# Array_tmpl.cc: Explicit Array template instantiations
//# Copyright (C) 2015
//# Associated Universities, Inc. Washington DC, USA,
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

//# Includes
#include <casacore/lattices/LatticeMath/LatticeStatistics.h>
#include <casacore/lattices/Lattices/LatticeIterInterface.h>
#include <casacore/lattices/Lattices/PagedArray.h>
#include <casacore/lattices/Lattices/Lattice.h>
#include <casacore/lattices/Lattices/SubLattice.h>

//# Instantiate extern templates for often used types.
#ifdef AIPS_CXX11
namespace casacore {
  template class LatticeIterInterface<Float>;
  template class LatticeStatistics<Float>;
  template class Lattice<Float>;
  template class Lattice<Complex>;
  template class PagedArray<Float>;
  template class PagedArray<Complex>;
  template class SubLattice<Bool>;
  template class SubLattice<Float>;
}
#endif
