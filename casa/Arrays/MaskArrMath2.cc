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

#include "MaskArrMath.h"
#include "ArrayError.h"

namespace casacore { //# NAMESPACE CASACORE - BEGIN

MaskedArray<std::complex<float>> operator * (const MaskedArray<std::complex<float>> &left, 
					 const float &right) 
{ 
  MaskedArray<std::complex<float>> retval;
  retval = left;
  bool zapIt;
  std::complex<float>* storage = retval.getRWArrayStorage(zapIt);
  int ntotal = retval.nelements();

  bool leftmaskDelete; 
  const LogicalArrayElem *leftmaskStorage 
    = left.getMaskStorage(leftmaskDelete); 

  for (int i=0; i< ntotal; i++) {
    if (leftmaskStorage[i]) storage[i] *= right;
  }
  retval.putArrayStorage(storage, zapIt);
  left.freeMaskStorage(leftmaskStorage, leftmaskDelete); 
  return retval;
}

MaskedArray<std::complex<float>> operator*(const float& left, 
			       const MaskedArray<std::complex<float>> &right)
{
  return operator*(right,left);
}

} //# NAMESPACE CASACORE - END

