//# QuantumType.h: Get an integer type for a Qunatum<T>
//# Copyright (C) 2010
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

#ifndef CASA_QUANTUMTYPE_H
#define CASA_QUANTUMTYPE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/casa/BasicSL/Complex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class Quantum;

// <summary> Get an integer type for a Qunatum<T> </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tQuantumHolder" demos="">
// </reviewed>

// <synopsis>
// The old way of getting a type number for a Quantum<T> uses the Register
// class. This did not work well with shared libraries, because each shared
// library gets its own static type data. Hence a type generated in one
// python module sometimes mismatched the type for the same Quantum in
// another module. For instance, it appeared that whem first loading
// pyrap.tables and thereafter pyrap.quanta, things went wrong.
//
// Therefore this class has been developed that returns the type for the
// Quantum template types used by QuantumHolder.
// </synopsis>

// <group name=QuantumType>

 inline uint32_t quantumType (const Quantum<double>*)
   { return 1; }
 inline uint32_t quantumType (const Quantum<float>*)
   { return 2; }
 inline uint32_t quantumType (const Quantum<int32_t>*)
   { return 3; }
 inline uint32_t quantumType (const Quantum<DComplex>*)
   { return 4; }
 inline uint32_t quantumType (const Quantum<Complex>*)
   { return 5; }
 inline uint32_t quantumType (const Quantum< Vector<double> >*)
   { return 6; }
 inline uint32_t quantumType (const Quantum< Vector<float> >*)
   { return 7; }
 inline uint32_t quantumType (const Quantum< Vector<int32_t> >*)
   { return 8; }
 inline uint32_t quantumType (const Quantum< Vector<DComplex> >*)
   { return 9; }
 inline uint32_t quantumType (const Quantum< Vector<Complex> >*)
   { return 10; }
 inline uint32_t quantumType (const Quantum< Array<double> >*)
   { return 11; }
 inline uint32_t quantumType (const Quantum< Array<float> >*)
   { return 12; }
 inline uint32_t quantumType (const Quantum< Array<int32_t> >*)
   { return 13; }
 inline uint32_t quantumType (const Quantum< Array<DComplex> >*)
   { return 14; }
 inline uint32_t quantumType (const Quantum< Array<Complex> >*)
   { return 15; }
 inline uint32_t quantumType (const Quantum< Matrix<double> >*)
   { return 16; }
 inline uint32_t quantumType (const Quantum< Matrix<float> >*)
   { return 17; }
 inline uint32_t quantumType (const Quantum< Matrix<int32_t> >*)
   { return 18; }
 inline uint32_t quantumType (const Quantum< Matrix<DComplex> >*)
   { return 19; }
 inline uint32_t quantumType (const Quantum< Matrix<Complex> >*)
   { return 20; }

// </group>

} //# NAMESPACE CASACORE - END

#endif
