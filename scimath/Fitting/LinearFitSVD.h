//# LinearFitSVD.h: Linear fit using Singular Value Decomposition method. 
//#
//# Copyright (C) 1995,1999,2000,2001,2002,2004
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

#ifndef SCIMATH_LINEARFITSVD_H
#define SCIMATH_LINEARFITSVD_H

#include <casacore/casa/aips.h>
#include <casacore/scimath/Fitting/LinearFit.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> 
// Linear least-squares fit using Singular Value Decomposition method. 
// </summary>
//
// <reviewed reviewer="wbrouw" date="2004/06/15" tests="tLinearFitSVD.cc"
//	 demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="LinearFit">LinearFit</linkto>
//   <li> <linkto module="Fitting">Fitting</linkto>
// </prerequisite>
//
// <etymology>
// Solves the linear least-squares fit problem using the singular value
// decomposition method.
// </etymology>
//
// <synopsis>
// The operation, calls and results are identical to those for the
// LinearFit class. The only difference is a collinearity default of 1e-8
// rather than 0. The actual calculations do a singular value
// decomposition solution. A method exists to get the constraints
// used in solving for missing rank.
//
// </synopsis>
//
// <motivation>
// The creation of this class was driven by the need to provide users with
// a reliable least-squares fit method.  "Numerical Recipes" recommends that
// singular value decomposition (SVD) method be always used for linear
// least-squares problems, because of its robustness.
// Not everybody agrees with this.
// </motivation>

template<class T> class LinearFitSVD: public LinearFit<T>
{
public: 
  //# Constructors
  // Create a fitter: the normal way to generate a fitter object. Necessary
  // data will be deduced from the Functional provided with
  // <src>setFunction()</src>
  LinearFitSVD();
  // Copy constructor (deep copy)
  LinearFitSVD(const LinearFitSVD &other);
  // Assignment (deep copy)
  LinearFitSVD &operator=(const LinearFitSVD &other);

  // Destructor
  virtual ~LinearFitSVD();

protected:
  //# Make members of parent classes known.
  using LinearFit<T>::svd_p;
  using LinearFit<T>::COLLINEARITY;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Fitting/LinearFitSVD.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
