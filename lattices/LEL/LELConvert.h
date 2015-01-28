//# LELConvert.h:  Class to convert a LEL node from one numerical type to another
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

#ifndef LATTICES_LELCONVERT_H
#define LATTICES_LELCONVERT_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LEL/LELInterface.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Class to convert a LEL node from one numerical type to another
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
//   <li> <linkto class="LatticeExpr"> LatticeExpr</linkto>
//   <li> <linkto class="LatticeExprNode"> LatticeExprNode</linkto>
//   <li> <linkto class="LELInterface"> LELInterface</linkto>
// </prerequisite>

// <etymology>
//  This derived LEL letter class handles numerical type conversions
// </etymology>

// <synopsis>
// This LEL letter class is derived from LELInterface.  It
// is used to construct LEL objects that know how to convert
// between numerical types, such as Double to Float.  They 
// operate on numerical Lattices and return a numerical Lattice. 
// The LELConvert object is embedded in the tree, and the conversion
// actually happens at tree evaluation time.
// <p>
// A description of the implementation details of the LEL classes can
// be found in
// <a href="../notes/216.html">Note 216</a>
// </synopsis> 

// <example>
// Examples are not very useful as the user would never use 
// these classes directly.  Look in LatticeExprNode.cc to see 
// how it invokes these classes.  An example of how the user
// would indirectly use this class (through the envelope) is:
// <srcblock>
// IPosition shape(2,5,10);
// ArrayLattice<Float> x(shape); x.set(1.0);
// ArrayLattice<Double> y(shape); 
// y.copyData(x);                 // y = x;
// </srcblock>
// The LELConvert class is embedded in the tree at construction time
// so as to handle the conversion from Float to Double at evaluation time
// </example>

// <motivation>
//  We needed to be able to handle mixed types in the LEL classes
// </motivation>

//# <todo asof="1998/01/21">
//# </todo>


template <class T, class F> class LELConvert : public LELInterface<T>
{
public: 
   
// Constructor.  <src><F></src> is the type we are coinverting from.
// <src><T></src> is the type we are converting to.
   LELConvert (const CountedPtr<LELInterface<F> >& expr);

// Destructor does nothing
  ~LELConvert();

// Recursively evaluate the expression.
   virtual void eval (LELArray<T>& result,
                      const Slicer& section) const;

// Recursively evaluate the scalar
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
   CountedPtr<LELInterface<F> > pExpr_p;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LEL/LELConvert.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
