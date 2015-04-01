//# SquareMatrix2.cc: explicit instantiation for SquareMatrix
//# Copyright (C) 1996
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

//# Includes
#include <casacore/scimath/Mathematics/SquareMatrix.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

SquareMatrix<Complex,4>& 
directProduct(SquareMatrix<Complex,4>& result,
	      const SquareMatrix<Complex,2>& left,
	      const SquareMatrix<Complex,2>& right)
{
    const Int n=2; // Instantiation for n==2
    switch (left.type_p) {
    case SquareMatrix<Complex,2>::ScalarId: 
	switch (right.type_p) {
	    case SquareMatrix<Complex,2>::ScalarId: {
		result.a_p[0][0]=left.a_p[0][0]*right.a_p[0][0]; 
		result.type_p=SquareMatrix<Complex,2>::ScalarId;
		return result;
	    }
	    case SquareMatrix<Complex,2>::Diagonal: {
		Complex tmp=left.a_p[0][0];
		for (Int i=0; i<n*n; i++) 
		    result.a_p[i][i]=tmp*right.a_p[i%n][i%n];
		result.type_p=SquareMatrix<Complex,2>::Diagonal;
		return result;
	    }
            case SquareMatrix<Complex,2>::General: {
		Complex tmp=left.a_p[0][0];
		for (Int i=0; i<n*n; i++) 
		    for (Int j=0; j<n*n; j++) {
			if (i/n == j/n) 
			    result.a_p[i][j]=tmp*right.a_p[i%n][j%n];
			else result.a_p[i][j]=Complex();
		    }
		result.type_p=SquareMatrix<Complex,2>::General;
		return result;
	    }
	}
    case SquareMatrix<Complex,2>::Diagonal: 
	switch (right.type_p) {
	    case SquareMatrix<Complex,2>::ScalarId: {
		Complex tmp=right.a_p[0][0];
		for (Int i=0; i<n*n; i++) 
		    result.a_p[i][i]=left.a_p[i/n][i/n]*tmp;
		result.type_p=SquareMatrix<Complex,2>::Diagonal;
		return result;
	    }
            case SquareMatrix<Complex,2>::Diagonal: {
		for (Int i=0; i<n*n; i++) 
		    result.a_p[i][i]=left.a_p[i/n][i/n]*right.a_p[i%n][i%n];
		result.type_p=SquareMatrix<Complex,2>::Diagonal;
		return result;
	    }
	    case SquareMatrix<Complex,2>::General: {
		for (Int i=0; i<n*n; i++) {
		    for (Int j=0; j<n*n; j++) {
			if (i/n == j/n) 
			    result.a_p[i][j]=left.a_p[i/n][i/n]*right.a_p[i%n][j%n];
			else result.a_p[i][j]=Complex();
		    }
		}
		result.type_p=SquareMatrix<Complex,2>::General;
		return result;
	    }
	}
    case SquareMatrix<Complex,2>::General: 
	switch (right.type_p) {
	    case SquareMatrix<Complex,2>::ScalarId: {
		Complex tmp=right.a_p[0][0];
		for (Int i=0; i<n*n; i++) 
		    for (Int j=0; j<n*n; j++) {
			if (i%n == j%n) 
			    result.a_p[i][j]=left.a_p[i/n][j/n]*tmp;
			else result.a_p[i][j]=Complex();
		    }
		result.type_p=SquareMatrix<Complex,2>::General;
		return result;
	    }		
            case SquareMatrix<Complex,2>::Diagonal: {
		for (Int i=0; i<n*n; i++) 
		    for (Int j=0; j<n*n; j++) {
			if (i%n == j%n) 
			    result.a_p[i][j]=left.a_p[i/n][j/n]*right.a_p[i%n][j%n];
			else result.a_p[i][j]=Complex();
		    }
		result.type_p=SquareMatrix<Complex,2>::General;
		return result;
	    }
	    case SquareMatrix<Complex,2>::General: {
		for (Int i=0; i<n*n; i++) 
		    for (Int j=0; j<n*n; j++) 
			result.a_p[i][j]=left.a_p[i/n][j/n]*right.a_p[i%n][j%n];
		result.type_p=SquareMatrix<Complex,2>::General;
		return result;
	    }
	}
    }
    // NOTREACHED
    return result;
}

SquareMatrix<Complex,2> conj(const SquareMatrix<Complex,2>& m) {
  SquareMatrix<Complex,2> result = m;
  result.conj();
  return result;
}




} //# NAMESPACE CASACORE - END

