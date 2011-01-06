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
//#
//# $Id: QuantumHolder.h 20551 2009-03-25 00:11:33Z Malte.Marquarding $

#ifndef CASA_QUANTUMTYPE_H
#define CASA_QUANTUMTYPE_H

//# Includes
#include <casa/aips.h>
#include <casa/BasicSL/Complex.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations
template <class T> class Quantum;
template <class T> class Vector;
template <class T> class Matrix;
template <class T> class Array;

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

 inline uInt quantumType (const Quantum<Double>*)
   { return 1; }
 inline uInt quantumType (const Quantum<Float>*)
   { return 2; }
 inline uInt quantumType (const Quantum<Int>*)
   { return 3; }
 inline uInt quantumType (const Quantum<DComplex>*)
   { return 4; }
 inline uInt quantumType (const Quantum<Complex>*)
   { return 5; }
 inline uInt quantumType (const Quantum< Vector<Double> >*)
   { return 6; }
 inline uInt quantumType (const Quantum< Vector<Float> >*)
   { return 7; }
 inline uInt quantumType (const Quantum< Vector<Int> >*)
   { return 8; }
 inline uInt quantumType (const Quantum< Vector<DComplex> >*)
   { return 9; }
 inline uInt quantumType (const Quantum< Vector<Complex> >*)
   { return 10; }
 inline uInt quantumType (const Quantum< Array<Double> >*)
   { return 11; }
 inline uInt quantumType (const Quantum< Array<Float> >*)
   { return 12; }
 inline uInt quantumType (const Quantum< Array<Int> >*)
   { return 13; }
 inline uInt quantumType (const Quantum< Array<DComplex> >*)
   { return 14; }
 inline uInt quantumType (const Quantum< Array<Complex> >*)
   { return 15; }
 inline uInt quantumType (const Quantum< Matrix<Double> >*)
   { return 16; }
 inline uInt quantumType (const Quantum< Matrix<Float> >*)
   { return 17; }
 inline uInt quantumType (const Quantum< Matrix<Int> >*)
   { return 18; }
 inline uInt quantumType (const Quantum< Matrix<DComplex> >*)
   { return 19; }
 inline uInt quantumType (const Quantum< Matrix<Complex> >*)
   { return 20; }

// </group>

} //# NAMESPACE CASA - END

#endif
