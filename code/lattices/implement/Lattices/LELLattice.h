//# LELLattice.h:  LELLattice
//# Copyright (C) 1997
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

#if !defined(AIPS_LELLATTICE_H)
#define AIPS_LELLATTICE_H


//# Includes
#include <trial/Lattices/LELInterface.h>

//# Forward Declarations
template <class T> class Array;
template <class T> class Lattice;
class PixelRegion;


// <summary>
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
// </prerequisite>

// <etymology>
// </etymology>
//
// <synopsis>
// </synopsis> 
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="1996/07/01">
// </todo>


template <class T> class LELLattice : public LELInterface<T>
{
public: 
   
// Constructor takes lattice and region to fetch from it
   LELLattice (const Lattice<T>& lattice);  

// Destructor does nothing
  ~LELLattice();

// Get the chunk of the lattice
   virtual void eval(Array<T>& result,
                     const PixelRegion& region) const;

// Getting a scalar value is not possible (throws exception).
   virtual T getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
   virtual void prepare();

// Get class name
   virtual String className() const;

private:
   Lattice<T>* pLattice_p;
};


#endif


