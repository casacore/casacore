//# Stokesvector.cc:  this defines StokesVector
//# Copyright (C) 1996,2003
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
#include <msvis/MSVis/StokesVector.h>
// #include <tables/Tables/RetypedArrayEngine.h>
// #include <tables/Tables/RetypedArraySetGet.h>
// #include <tables/Tables/DataManError.h>
#include <casa/Exceptions/Error.h>


namespace casa { //# NAMESPACE CASA - BEGIN

// The following sqrt functions are required for Image to
// work. Ug-ly.
//CStokesVector& sqrt(const CStokesVector& v) {
//  return sqrt(v);
//}

//StokesVector& sqrt(const StokesVector& v) {
//  return sqrt(v);
//}

// StokesVector CStokesVector::real() {
//     return StokesVector(v_p[0].real(),v_p[1].real(),v_p[2].real(),
// 				 v_p[3].real());
// }
  // Compute the maximum EigenValue
Float StokesVector::maxEigenValue() const {
  Float r;
  r=v_p[1]*v_p[1]+v_p[2]*v_p[2]+v_p[3]*v_p[3];
  return Float(v_p[0])+::sqrt(r);
}
  // Compute the minimum EigenValue
Float StokesVector::minEigenValue() const {
  Float r;
  r=v_p[1]*v_p[1]+v_p[2]*v_p[2]+v_p[3]*v_p[3];
  return Float(v_p[0])-::sqrt(r);
}
  // Compute the determinant of the coherence matrix
Float StokesVector::determinant() const {
  return square(v_p[0])-(square(v_p[1])+square(v_p[2])+square(v_p[3]));
}


} //# NAMESPACE CASA - END

