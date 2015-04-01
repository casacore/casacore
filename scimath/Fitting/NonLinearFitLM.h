//# NonLinearFitLM.h: Solve non-linear fit using Levenberg-Marquardt method.
//# Copyright (C) 1995,1999-2002,2004,2006
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
//#
//# $Id$

#ifndef SCIMATH_NONLINEARFITLM_H
#define SCIMATH_NONLINEARFITLM_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Fitting/NonLinearFit.h>
namespace casacore { //# begin namespace casa
//# Forward declarations

//
// <summary>
// Solve non-linear fit with Levenberg-Marquardt method.
// </summary>
//
// <reviewed reviewer="wbrouw" date="2004/06/15" tests="tNonLinearFitLM.cc"
//	 demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="NonLinearFit">NonLinearFit</linkto>
//   <li> <linkto module="Fitting">Fitting</linkto>
// </prerequisite>
//
// <etymology>
// This class uses the Levenberg-Marquardt method to solve the non-linear
// least-squares fit problem hence NonLinearFitLM
// </etymology>
//
// <synopsis>
// NOTE: Constraints added. Documentation out of date at moment, check
// the tLinearFitSVD and tNonLinearFirLM programs for examples.
//
// See the <linkto class=NonLinearFit>NonLinearFit</linkto> class for a
// general description.
//
// This class is derived from the general NonLinearFit class. It does
// a non-linear least-squares fit using the Levenberg-Marquardt method.
//
// See Numerical Recipes for more information
// on the Levenberg-Marquardt method.
// </synopsis>
// 
// <templating arg=T>
// <li> Float
// <li> Double
// <li> Complex
// <li> DComplex   
// </templating>
//
// <motivation>
// Levenberg-Marquardt method is a standard method for non-linear
// least-squares fits.  It works well in practice over a wide range of
// problems.
// </motivation>
// 
// <example>
// </example>

template<class T> class NonLinearFitLM : public NonLinearFit<T>
{
public:
  //# Constructors
  // Create a fitter: the normal way to generate a fitter object. Necessary
  // data will be deduced from the Functional provided with
  // <src>setFunction()</src>.
  // Optionally, a fitter with SVD behaviour
  explicit NonLinearFitLM(Bool svd=False);
  // Copy constructor (deep copy)
  NonLinearFitLM(const NonLinearFitLM &other);
  // Assignment (deep copy)
  NonLinearFitLM &operator=(const NonLinearFitLM &other);
  
  // Destructor
  virtual ~NonLinearFitLM();

protected:
  //# Member functions
  // Generalised fitter
  virtual Bool fitIt
    (Vector<typename FunctionTraits<T>::BaseType> &sol, 
     const Array<typename FunctionTraits<T>::BaseType> &x, 
     const Vector<typename FunctionTraits<T>::BaseType> &y,
     const Vector<typename FunctionTraits<T>::BaseType> *const sigma,
     const Vector<Bool> *const mask=0);
  
private:
  //# Data
  // The parameter that makes this the Levenberg-Marquardt method.  
  Double lamda_p;
  // The current fit state
  Double fitit_p;

protected:
  //# Make members of parent classes known.
  using NonLinearFit<T>::curiter_p;
  using NonLinearFit<T>::maxiter_p;
  using NonLinearFit<T>::converge_p;
  using NonLinearFit<T>::pCount_p;
  using NonLinearFit<T>::ptr_derive_p;
  using NonLinearFit<T>::sol_p;
  using NonLinearFit<T>::solved_p;
  using NonLinearFit<T>::nr_p;
  using NonLinearFit<T>::svd_p;
  using NonLinearFit<T>::condEq_p;
  using NonLinearFit<T>::err_p;
  using NonLinearFit<T>::errors_p;
  using NonLinearFit<T>::valder_p;
  using NonLinearFit<T>::buildConstraint;
  using NonLinearFit<T>::setMaskedParameterValues;
  using NonLinearFit<T>::fillSVDConstraints;
  using NonLinearFit<T>::isReady;
};

} //# End namespace casacore
#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Fitting/NonLinearFitLM.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
