//# LatticeAddNoise2.tcc: add noise to a lattice
//# Copyright (C) 1997,1998,1999,2000,2001
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

#include <casacore/lattices/LatticeMath/LatticeAddNoise.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/lattices/LEL/LatticeExprNode.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/BasicSL/Complex.h> 
#include <casacore/casa/BasicMath/Random.h> 
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN
 
template <class T> void LatticeAddNoise::add (MaskedLattice<T>& lattice) {
    ThrowIf(! itsNoise, "You have not yet called function 'set'");
    LatticeIterator<T> it(lattice);
    for (it.reset(); !it.atEnd(); it++) {
        addNoiseToArray(it.rwCursor());
    }
}

template <class T> void LatticeAddNoise::add (Lattice<T>& lattice) {
    SubLattice<T> ml(lattice, true);
    add(ml);
}

} //# NAMESPACE CASACORE - END

