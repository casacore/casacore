//# SquareMatrix.cc: Fast Square Matrix class with fixed (templated) size
//# Copyright (C) 1996,1998,1999,2001,2002
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

#ifndef SCIMATH_SQUAREMATRIX_TCC
#define SCIMATH_SQUAREMATRIX_TCC

//# Includes
#include <casacore/scimath/Mathematics/SquareMatrix.h>
#include <casacore/scimath/Mathematics/MatrixMathLA.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T, Int n> 
SquareMatrix<T,n>& 
SquareMatrix<T,n>::operator=(const SquareMatrix<T,n>& m) {
    type_p=m.type_p;
    switch (type_p) {
        case ScalarId: a_p[0][0]=m.a_p[0][0]; break;
        case Diagonal: {
	    for (Int i=0; i<n; i++) a_p[i][i]=m.a_p[i][i];
	    break;
	}
        case General: {
	    const T* pm=&m.a_p[0][0];
	    T* pa_p=&a_p[0][0];
	    for (Int i=0; i<n*n; i++) *pa_p++=*pm++;
	}
    }
    return *this;
}
//# not accepted out of line by native compiler- moved inline
template <class T, Int n> 
SquareMatrix<T,n>& 
SquareMatrix<T,n>::operator=(const Vector<T>& v) {
    for (Int i=0; i<n; i++) a_p[i][i]=v(i);
    type_p=Diagonal;
    return *this;
}
template <class T, Int n> 
SquareMatrix<T,n>& 
SquareMatrix<T,n>::operator=(const Matrix<T>& m) {
    for (Int i=0; i<n; i++)
	for (Int j=0; j<n; j++) a_p[i][j]=m(i,j);
    type_p=General;
    return *this;
}
template <class T, Int n> 
SquareMatrix<T,n>& 
SquareMatrix<T,n>::operator+=(const SquareMatrix<T,n>& other) {
    switch (type_p) {
    case ScalarId: 
	switch (other.type_p) {
	    case ScalarId: {
		a_p[0][0]+=other.a_p[0][0]; 
		return *this;
	    }
	    case Diagonal: {
		T tmp=a_p[0][0];
		for (Int i=0; i<n; i++) a_p[i][i]=tmp+other.a_p[i][i];
		type_p=Diagonal;
		return *this;
	    }
            case General: {
		T tmp=a_p[0][0];
		for (Int i=0; i<n; i++) {
		    a_p[i][i]=tmp+other.a_p[i][i];
		    for (Int j=0; j<n; j++) 
			if (i!=j) a_p[i][j]=other.a_p[i][j];
		}
		type_p=General;
		return *this;
	    }
	}
    case Diagonal: 
	switch (other.type_p) {
	    case ScalarId: {
		for (Int i=0; i<n; i++) a_p[i][i]+=other.a_p[0][0];
		return *this;
	    }
            case Diagonal: {
		for (Int i=0; i<n; i++) a_p[i][i]+=other.a_p[i][i];
		return *this;
	    }
	    case General: {
		for (Int i=0; i<n; i++) {
		    a_p[i][i]+=other.a_p[i][i];
		    for (Int j=0; j<n; j++) 
			if (i!=j) a_p[i][j]=other.a_p[i][j];
		}
		type_p=General;
		return *this;
	    }
	}
//  case General: 
    default:
	switch (other.type_p) {
	    case ScalarId: {
		for (Int i=0; i<n; i++) a_p[i][i]+=other.a_p[0][0];
		return *this;
	    }		
            case Diagonal: {
		for (Int i=0; i<n; i++) a_p[i][i]+=other.a_p[i][i];
		return *this;
	    }
	    case General: {
		const T* po=&other.a_p[0][0];
		T* pa_p=&a_p[0][0];
		for (Int i=0; i<n*n; i++) *(pa_p++)+=*po++;
		return *this;
	    }
	}
    }
    return *this;
}

template <class T, Int n> 
SquareMatrix<T,n>& SquareMatrix<T,n>::operator*=(const SquareMatrix<T,n>& other) {
    switch (type_p) {
    case ScalarId: 
	switch (other.type_p) {
	    case ScalarId: {
		a_p[0][0]*=other.a_p[0][0]; 
		return *this;
	    }
	    case Diagonal: {
		T tmp=a_p[0][0];
		for (Int i=0; i<n; i++) {
		    a_p[i][i]=tmp; a_p[i][i]*=other.a_p[i][i];
		}
		type_p=Diagonal;
		return *this;
	    }
            case General: {
		T tmp=a_p[0][0];
		for (Int i=0; i<n; i++) 
		    for (Int j=0; j<n; j++) {
			a_p[i][j]=tmp; a_p[i][j]*=other.a_p[i][j];
		    }
		type_p=General;
		return *this;
	    }
	}
    case Diagonal: 
	switch (other.type_p) {
	    case ScalarId: {
		for (Int i=0; i<n; i++) a_p[i][i]*=other.a_p[0][0];
		return *this;
	    }
            case Diagonal: {
		for (Int i=0; i<n; i++) a_p[i][i]*=other.a_p[i][i];
		return *this;
	    }
	    case General: {
		T a[n];
		Int i;
		for (i=0; i<n; i++) a[i]=a_p[i][i];
		for (i=0; i<n; i++) {
		    for (Int j=0; j<n; j++) {
			a_p[i][j]=a[i]; a_p[i][j]*=other.a_p[i][j];
		    }
		}
		type_p=General;
		return *this;
	    }
	}
    case General: 
	switch (other.type_p) {
	    case ScalarId: {
		for (Int i=0; i<n; i++) 
		    for (Int j=0; j<n; j++) a_p[i][j]*=other.a_p[0][0];
		return *this;
	    }		
            case Diagonal: {
		for (Int i=0; i<n; i++) 
		    for (Int j=0; j<n; j++) a_p[i][j]*=other.a_p[j][j];
		return *this;
	    }
//      case General: 
	default: {
		T a[n], tmp;
		for (Int i=0; i<n; i++) {
		    Int j;
		    for (j=0; j<n; j++) a[j]=a_p[i][j];
		    for (j=0; j<n; j++) {
			a_p[i][j]=a[0]; a_p[i][j]*=other.a_p[0][j];
			for (Int k=1; k<n; k++) {
			    //#a_p[i][j]+=a[k]*other.a_p[k][j]; inlining fails
			    tmp=a[k]; tmp*=other.a_p[k][j]; a_p[i][j]+=tmp;
			}
		    }
		}
		return *this;
	    }
	}
    }
    return *this;
}
template <class T, Int n> 
SquareMatrix<T,n>& SquareMatrix<T,n>::operator*=(Float f) 
{
    switch (type_p) {
        case ScalarId: a_p[0][0]*=f; break;
        case Diagonal: {
	    for (Int i=0; i<n; i++) a_p[i][i]*=f;
	    break;
	}
        case General: {
	    T* pa_p=&a_p[0][0];
	    for (Int i=0; i<n*n; i++) *pa_p++*=f;
	}
    }
    return *this;
}

/* fails to compile - use explicitly instantiated global function instead
template <class T, Int n> 
SquareMatrix<T,n*n>& SquareMatrix<T,n>::directProduct(SquareMatrix<T,n*n>& dp,
const SquareMatrix<T,n>& other) const
{
    switch (type_p) {
    case ScalarId: 
	switch (other.type_p) {
	    case ScalarId: {
		dp.a_p[0][0]=a_p[0][0]*other.a_p[0][0]; 
		dp.type_p=ScalarId;
		return dp;
	    }
	    case Diagonal: {
		T tmp=a_p[0][0];
		for (Int i=0; i<n*n; i++) dp.a_p[i][i]=tmp*other.a_p[i%n][i%n];
		dp.type_p=Diagonal;
		return dp;
	    }
            case General: {
		T tmp=a_p[0][0];
		for (Int i=0; i<n*n; i++) 
		    for (Int j=0; j<n*n; j++) {
			if (i/n == j/n) dp.a_p[i][j]=tmp*other.a_p[i%n][j%n];
			else dp.a_p[i][j]=T();
		    }
		dp.type_p=General
		return dp;
	    }
	}
    case Diagonal: 
	switch (other.type_p) {
	    case ScalarId: {
		T tmp=other.a_p[0][0];
		for (Int i=0; i<n*n; i++) dp.a_p[i][i]=a_p[i/n][i/n]*tmp;
		dp.type_p=Diagonal;
		return dp;
	    }
            case Diagonal: {
		for (Int i=0; i<n*n; i++) 
		    dp.a_p[i][i]=a_p[i/n][i/n]*other.a_p[i%n][i%n];
		dp.type_p=Diagonal;
		return dp;
	    }
	    case General: {
		for (Int i=0; i<n*n; i++) {
		    for (Int j=0; j<n*n; j++) {
			if (i/n == j/n) 
			    dp.a_p[i][j]=a_p[i/n][i/n]*other.a_p[i%n][j%n];
			else dp.a_p[i][j]=T();
		    }
		}
		dp.type_p=General;
		return dp;
	    }
	}
    case General: 
	switch (other.type_p) {
	    case ScalarId: {
		T tmp=other.a_p[0][0];
		for (Int i=0; i<n*n; i++) 
		    for (Int j=0; j<n*n; j++) {
			if (i%n == j%n) dp.a_p[i][j]=a_p[i/n][j/n]*tmp;
			else dp.a_p[i][j]=T();
		    }
		dp.type_p=General;
		return dp;
	    }		
            case Diagonal: {
		for (Int i=0; i<n*n; i++) 
		    for (Int j=0; j<n*n; j++) {
			if (i%n == j%n) 
			    dp.a_p[i][j]=a_p[i/n][j/n]*other.a_p[i%n][j%n];
			else dp.a_p[i][j]=T();
		    }
		dp.type_p=General;
		return dp;
	    }
	    case General: {
		for (Int i=0; i<n*n; i++) 
		    for (Int j=0; j<n*n; j++) 
			dp.a_p[i][j]=a_p[i/n][j/n]*other.a_p[i%n][j%n];
		dp.type_p=General;
		return dp;
	    }
	}
    }
}
*/
//# above instantiated for T=Complex, n=2 in SquareMatrix2.cc

template <class T, Int n> 
SquareMatrix<T,n>& SquareMatrix<T,n>::conj() {
    switch (type_p) {
        case ScalarId: {
            a_p[0][0]=std::conj(a_p[0][0]);
	    return *this;
	}
        case Diagonal: {
	    for (Int i=0; i<n; i++) a_p[i][i]=std::conj(a_p[i][i]);
	    return *this;
	}
//      case General: 
	default: {
	    for (Int i=0; i<n; i++)
		for (Int j=0; j<n; j++) a_p[i][j]=std::conj(a_p[i][j]);
	    return *this;
	}
    }
}

template <class T, Int n> 
SquareMatrix<T,n>& SquareMatrix<T,n>::adjoint() {
    switch (type_p) {
        case ScalarId: {
	    a_p[0][0]=std::conj(a_p[0][0]);
	    return *this;
	}
        case Diagonal: {
	    for (Int i=0; i<n; i++) a_p[i][i]=std::conj(a_p[i][i]);
	    return *this;
	}
        case General: {
	    for (Int i=0; i<n; i++) {
		a_p[i][i]=std::conj(a_p[i][i]);
		for (Int j=i+1; j<n; j++) {
		    T tmp=std::conj(a_p[i][j]);
		    a_p[i][j]=std::conj(a_p[j][i]);
		    a_p[j][i]=tmp;
		}
	    }
	    return *this;
	}
    }
    return *this;
}

template <class T, Int n> 
SquareMatrix<T,n>& SquareMatrix<T,n>::conj(SquareMatrix<T,n>& result) {
  result = *this;
  result.conj();
  return result;
}

template <class T, Int n> 
SquareMatrix<T,n>& SquareMatrix<T,n>::adjoint(SquareMatrix<T,n>& result) {
  result = *this;
  result.adjoint();
  return result;
}

template <class T, Int n> 
SquareMatrix<T,n>& SquareMatrix<T,n>::inverse(SquareMatrix<T,n>& result) const {
  switch (type_p) {
    case ScalarId: {
      result.a_p[0][0]=T(1)/a_p[0][0];
      result.type_p=ScalarId;
      return result;
    }
    case Diagonal: {
      for (Int i=0; i<n; i++) result.a_p[i][i]=T(1)/a_p[i][i];
      result.type_p=Diagonal;
      return result;
    }
//  case General: 
    default: {
      switch (n) {
        case 2: {
	  T det=a_p[0][0]*a_p[1][1]-a_p[1][0]*a_p[0][1];
	  result.a_p[0][0]=a_p[1][1]/det;
	  result.a_p[0][1]=-a_p[0][1]/det;
	  result.a_p[1][0]=-a_p[1][0]/det;
	  result.a_p[1][1]=a_p[0][0]/det;
	  result.type_p=General;
	  return result;
	}
        default: {
	  //		    return result=invert(matrix());
	  Matrix<T> mat=invert(matrix());
	  if (mat.nelements()==0) {
	    cerr<< "invert of singular matrix attempted:"<< 
	      matrix()
		<< endl;
	    result=T(1);
	  }
	  else result=mat;
	  return result;
	}
      }
    }
  }		    
}		

template <class T, Int n> 
Matrix<T>& SquareMatrix<T,n>::matrix(Matrix<T>& result) const {
    result.resize(n,n);
    switch (type_p) {
        case ScalarId: {
	    result=T();
	    for (Int i=0; i<n; i++) result(i,i)=a_p[0][0];
	    return result;
	}
        case Diagonal: {
	    result=T();
	    for (Int i=0; i<n; i++) result(i,i)=a_p[i][i];
	    return result;
	}
//      case General: 
        default: {
	    for (Int i=0; i<n; i++)
		for (Int j=0; j<n; j++) result(i,j)=a_p[i][j];
	    return result;
	}
    }
}

template <class T, Int n>
T& SquareMatrix<T,n>::throwInvAccess() {
    throw(AipsError("SquareMatrix - attempt to change element that is "
		    "not available for this type of matrix"));
    // following just to make signature ok.
    return a_p[0][0];
}

} //# NAMESPACE CASACORE - END


#endif
