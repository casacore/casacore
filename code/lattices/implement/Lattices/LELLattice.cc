//# LELLattice.cc:  this defines LELLattice.cc
//# Copyright (C) 1997,1998
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

#include <trial/Lattices/LELLattice.h>

#include <aips/Arrays/Array.h>
#include <aips/Exceptions/Error.h> 
#include <aips/Lattices/IPosition.h>
#include <aips/Utilities/COWPtr.h>
#include <trial/Lattices/Lattice.h>
#include <trial/Lattices/PixelRegion.h>
#include <iostream.h>



template <class T>
LELLattice<T>::LELLattice(const Lattice<T>& lattice) 
: pLattice_p (lattice.clone())
{
   setAttr(LELAttribute(lattice.shape(), lattice.niceCursorShape(),
			lattice.latticeCoordinates()));

#if defined(AIPS_TRACE)
   cout << "LELLattice:: constructor, pLattice_p.nrefs() = "
	<< pLattice_p.nrefs() << endl;
#endif
}

template <class T>
LELLattice<T>::~LELLattice()
{
   delete pLattice_p;

#if defined(AIPS_TRACE)
   cout << "LELLattice:: destructor, pLattice_p.nrefs() = "
	<< pLattice_p.nrefs() << endl;
#endif
}

template <class T>
void LELLattice<T>::eval(Array<T>& result, 
			 const PixelRegion& region) const
{
#if defined(AIPS_TRACE)
   cout << "LELLattice::eval; pLattice_p.nrefs() "
	<< pLattice_p.nrefs() << endl;
#endif

// The rwRef function will make a copy when needed (i.e. when ptr
// contains a reference to the original data).

   COWPtr<Array<T> > ptr;
   pLattice_p->getSlice(ptr, region.box(), False);
   result.reference(ptr.rwRef());
}

template <class T>
T LELLattice<T>::getScalar() const
{
   throw (AipsError ("LELLattice::getScalar - cannot be used"));
   return pLattice_p->getAt (IPosition());       // to make compiler happy
}

template <class T>
void LELLattice<T>::prepare()
{}

template <class T>
String LELLattice<T>::className() const
{
   return String("LELLattice");
}
