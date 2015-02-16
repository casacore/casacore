//# RigidVector.cc: Fast Vector classes with fixed (templated) length
//# Copyright (C) 1996,1998,2001
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

#ifndef SCIMATH_RIGIDVECTOR_TCC
#define SCIMATH_RIGIDVECTOR_TCC

#include <casacore/scimath/Mathematics/RigidVector.h>
#include <casacore/scimath/Mathematics/SquareMatrix.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T, Int n>
RigidVector<T,n>& 
RigidVector<T,n>::operator*=(const SquareMatrix<T,n>& m) {
  switch (m.type_p) {
  case SquareMatrix<T,n>::ScalarId: {
    for (Int i=0; i<n; i++) v_p[i]*=m.a_p[0][0];
    return *this;
  }
  case SquareMatrix<T,n>::Diagonal: {
    for (Int i=0; i<n; i++) v_p[i]*=m.a_p[i][i];
    return *this;
  }
  //  case SquareMatrix<T,n>::General:
  default:
    {
      T v[n], tmp;
      Int i;
      for (i=0; i<n; i++) v[i]=v_p[i];
      for (i=0; i<n; i++) {
	v_p[i]=m.a_p[i][0]; v_p[i]*=v[0];
	for (Int j=1; j<n; j++) {
	  //#v_p[i]+=m.a_p[i][j]*v[j]; inlining fails
	  tmp=m.a_p[i][j]; tmp*=v[j]; v_p[i]+=tmp;
	}
      }
      return *this;
    }
  }
}
template <class T, Int n>
RigidVector<T,n> sqrt(const RigidVector<T,n>& v) {
      RigidVector<T,n> tmp;
      for (Int i=0; i<n; i++) tmp.v_p[i]=::sqrt(v.v_p[i]);
      return tmp;
    }

 
// // Needed for Image<RigidVector>
// template <class T, Int n>
// void* RigidVector<T,n>::newCopyInfo (const TableRecord&, const IPosition&)
// { return 0; }

// template <class T, Int n>
// void RigidVector<T,n>::deleteCopyInfo (void*)
// {}

// // For set and get, we don't need to do anything sophisticated
// template <class T, Int n>
// void RigidVector<T,n>::set (void* ci, void* vout,
// 			const Array<T>& in,
// 			const IPosition& shape)
// {
//     Array<RigidVector<T,n> >& out = *(Array<RigidVector<T,n> >*)vout;
//     if (shape.nelements() == 1  &&  shape(0) == n) {
// 	retypedArrayEngineSet (out, in);
//     }else{
// 	throw (DataManError ("RigidVector<T,n>::set"));
//     }
// }

// template <class T, Int n>
// void RigidVector<T,n>::get (void* ci, Array<T>& out,
// 			   const void* vin,
// 			   const IPosition& shape)
// {
//     const Array<RigidVector<T,n> >& in = *(const Array<RigidVector<T,n> >*)vin;
//     if (shape.nelements() == 1  &&  shape(0) == n) {
// 	retypedArrayEngineGet (out, in);
//     }else{
// 	throw (DataManError ("RigidVector<T,n>::get"));
//     }
// }


} //# NAMESPACE CASACORE - END


#endif
