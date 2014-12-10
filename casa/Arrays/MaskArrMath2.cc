//# MaskArrMath2.cc: Arithmetic functions defined on MaskedArrays
//# Copyright (C) 1993,1994,1995,1996
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

#include <casacore/casa/Arrays/MaskArrMath.h>
#include <casacore/casa/Arrays/ArrayError.h>
namespace casacore { //# NAMESPACE CASACORE - BEGIN

//#include <casacore/casa/Arrays/VectorIter.h>
//#include <casacore/casa/Arrays/Matrix.h>
//#include <casacore/casa/BasicMath/Math.h>

MaskedArray<Complex> operator * (const MaskedArray<Complex> &left, 
					 const Float &right) 
{ 
  MaskedArray<Complex> retval;
  retval = left;
  Bool zapIt;
  Complex* storage = retval.getRWArrayStorage(zapIt);
  Int ntotal = retval.nelements();

  Bool leftmaskDelete; 
  const LogicalArrayElem *leftmaskStorage 
    = left.getMaskStorage(leftmaskDelete); 

  for (Int i=0; i< ntotal; i++) {
    if (leftmaskStorage[i]) storage[i] *= right;
  }
  retval.putArrayStorage(storage, zapIt);
  left.freeMaskStorage(leftmaskStorage, leftmaskDelete); 
  return retval;
}

MaskedArray<Complex> operator*(const Float& left, 
			       const MaskedArray<Complex> &right)
{
  return operator*(right,left);
}






} //# NAMESPACE CASACORE - END

