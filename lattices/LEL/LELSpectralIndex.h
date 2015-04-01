//# LELSpectralIndex.h: LEL function to calculate spectral index/
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
//# $Id$

#ifndef LATTICES_LELSPECTRALINDEX_H
#define LATTICES_LELSPECTRALINDEX_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LEL/LELFunction.h>
#include <casacore/casa/Containers/Block.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations


// <summary>
// This LEL class handles calculation of the spectral index.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
//   <li> <linkto class="LatticeExpr"> LatticeExpr</linkto>
//   <li> <linkto class="LatticeExprNode"> LatticeExprNode</linkto>
//   <li> <linkto class="LELInterface"> LELInterface</linkto>
//   <li> <linkto class="LELFunctionEnums"> LELFunctionEnums</linkto>
// </prerequisite>

// <synopsis>
// This LEL letter class is derived from LELInterface. It is used to
// construct LEL objects that calculate the sepectral index from 2 other
// LEL expression (usually images).
// It operates on real types (Float,Double) 
// </synopsis> 

// <motivation>
// This is a separate class (instead of being part of a LELFunction class),
// because the calculation of the spectral index requires extra variables
// (the frequencies) and some more complicated code.
// </motivation>

template<class T> class LELSpectralIndex : public LELInterface<T>
{
  //# Make members of parent class known.
protected:
  using LELInterface<T>::setAttr;

public: 
  // Constructor takes operation and expressions to be operated upon
  LELSpectralIndex (const Block<LatticeExprNode>& expr);

  // Destructor 
  ~LELSpectralIndex();

  // Recursively evaluate the expression 
  virtual void eval (LELArray<T>& result,
		     const Slicer& section) const;

  // Get the result of a scalar subexpression.
  // Throws an exception as it is not possible.
   virtual LELScalar<T> getScalar() const;

  // Do further preparations (e.g. optimization) on the expression.
  // Returns False.
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
  Int itsFreqAxis;
  Block<Double> itsLogFreq;       //# log(f0/f1)
  LatticeExprNode arg0_p;
  LatticeExprNode arg1_p;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LEL/LELSpectralIndex.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
