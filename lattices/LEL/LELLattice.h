//# LELLattice.h:  LELLattice
//# Copyright (C) 1997,1998,1999,2000
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

#ifndef LATTICES_LELLATTICE_H
#define LATTICES_LELLATTICE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LEL/LELInterface.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class Lattice;
template <class T> class MaskedLattice;


// <summary> This LEL class handles access to Lattices </summary>
//
// <use visibility=local>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
//   <li> <linkto class="LatticeExpr"> LatticeExpr</linkto>
//   <li> <linkto class="LatticeExprNode"> LatticeExprNode</linkto>
//   <li> <linkto class="LELInterface"> LELInterface</linkto>
// </prerequisite>
//
// <etymology>
//  This derived LEL letter class handles access to the pixels in a Lattice
// </etymology>
//
// <synopsis>
// This LEL letter class is derived from LELInterface.  It
// is used to construct LEL objects that access the pixels in
// a Lattice. It works with Lattices of type 
// Float,Double,Complex,DComplex and Bool.  
//
// A description of the implementation details of the LEL classes can
// be found in
// <a href="../notes/216.html">Note 216</a>
// </synopsis> 
//
// <example>
// Examples are not very useful as the user would never use 
// these classes directly.  Look in LatticeExprNode.cc to see 
// how it invokes these classes.  An example of how the user
// would indirectly use this class (through the envelope) is:
// <srcblock>
// IPosition shape(2,5,10);
// ArrayLattice<Float> x(shape); x.set(1.0);
// ArrayLattice<Float> y(shape); 
// y.copyData(x);                 // y = x
// </srcblock>
// </example>
//
// <motivation>
// Accessing Lattices is fundamental to manipulating their pixel values
// </motivation>
//
// <todo asof="1998/01/20">
// </todo>
 



template <class T> class LELLattice : public LELInterface<T>
{
  //# Make members of parent class known.
public:
  using LELInterface<T>::getAttribute;
protected:
  using LELInterface<T>::setAttr;

public: 
// Constructor takes lattice to fetch from it
// <group>
   LELLattice (const Lattice<T>& lattice);  
   LELLattice (const MaskedLattice<T>& lattice);  
// </group>

// Destructor does nothing
  ~LELLattice();

// Evaluate the expression; this means get the chunk of the lattice.
   virtual void eval(LELArray<T>& result,
                     const Slicer& section) const;
   virtual void evalRef(LELArrayRef<T>& result,
			const Slicer& section) const;

// Getting a scalar value is not possible (throws exception).
   virtual LELScalar<T> getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual Bool prepareScalarExpr();

// Get class name
   virtual String className() const;

  // Handle locking/syncing of a lattice in a lattice expression.
  // <group>
  virtual Bool lock (FileLocker::LockType, uInt nattempts);
  virtual void unlock();
  virtual Bool hasLock (FileLocker::LockType) const;
  virtual void resync();
  // </group>

private:
   MaskedLattice<T>* pLattice_p;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LEL/LELLattice.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
