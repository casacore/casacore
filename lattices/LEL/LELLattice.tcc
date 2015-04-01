//# LELLattice.cc:  this defines LELLattice.cc
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
//# $Id$

#ifndef LATTICES_LELLATTICE_TCC
#define LATTICES_LELLATTICE_TCC

#include <casacore/lattices/LEL/LELLattice.h>
#include <casacore/lattices/LEL/LELScalar.h>
#include <casacore/lattices/LEL/LELArray.h>
#include <casacore/lattices/Lattices/Lattice.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Exceptions/Error.h> 
#include <casacore/casa/iostream.h>



namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T>
LELLattice<T>::LELLattice(const Lattice<T>& lattice) 
: pLattice_p (new SubLattice<T> (lattice))
{
   setAttr(LELAttribute(False, 
			lattice.shape(), lattice.niceCursorShape(),
			lattice.lelCoordinates()));

#if defined(AIPS_TRACE)
   cout << "LELLattice:: constructor, pLattice_p.nrefs() = "
	<< pLattice_p.nrefs() << endl;
#endif
}

template <class T>
LELLattice<T>::LELLattice(const MaskedLattice<T>& lattice) 
: pLattice_p (lattice.cloneML())
{
   setAttr(LELAttribute(lattice.isMasked(),
			lattice.shape(), lattice.niceCursorShape(),
			lattice.lelCoordinates()));

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
void LELLattice<T>::eval(LELArray<T>& result, 
			 const Slicer& section) const
{
#if defined(AIPS_TRACE)
   cout << "LELLattice::eval; pLattice_p.nrefs() "
	<< pLattice_p.nrefs() << endl;
#endif

   Array<T> tmp = pLattice_p->getSlice (section);
   result.value().reference(tmp);
   if (getAttribute().isMasked()) {
      Array<Bool> mask = pLattice_p->getMaskSlice (section);
      result.setMask (mask);
   } else {
      result.removeMask();
   }
}

template <class T>
void LELLattice<T>::evalRef(LELArrayRef<T>& result, 
			    const Slicer& section) const
{
#if defined(AIPS_TRACE)
   cout << "LELLattice::evalRef; pLattice_p.nrefs() "
	<< pLattice_p.nrefs() << endl;
#endif

   Array<T> tmp;
   pLattice_p->getSlice (tmp, section);
   // Cast to its base class LELArray to use the non-const value function.
   ((LELArray<T>&)result).value().reference(tmp);
   if (getAttribute().isMasked()) {
      Array<Bool> mask = pLattice_p->getMaskSlice (section);
      result.setMask (mask);
   } else {
      result.removeMask();
   }
}

template <class T>
LELScalar<T> LELLattice<T>::getScalar() const
{
   throw (AipsError ("LELLattice::getScalar - cannot be used"));
   return pLattice_p->getAt (IPosition());       // to make compiler happy
}

template <class T>
Bool LELLattice<T>::prepareScalarExpr()
{
    return False;
}

template <class T>
String LELLattice<T>::className() const
{
   return String("LELLattice");
}


template<class T>
Bool LELLattice<T>::lock (FileLocker::LockType type, uInt nattempts)
{
    return pLattice_p->lock (type, nattempts);
}
template<class T>
void LELLattice<T>::unlock()
{
    pLattice_p->unlock();
}
template<class T>
Bool LELLattice<T>::hasLock (FileLocker::LockType type) const
{
    return pLattice_p->hasLock (type);
}
template<class T>
void LELLattice<T>::resync()
{
    pLattice_p->resync();
}


} //# NAMESPACE CASACORE - END


#endif
