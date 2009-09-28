//# SpectralFit.h: Least Squares fitting of spectral elements to spectrum
//# Copyright (C) 2001,2002
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

#ifndef COMPONENTS_SPECTRALFIT_H
#define COMPONENTS_SPECTRALFIT_H

//# Includes
#include <casa/aips.h>
#include <components/SpectralComponents/SpectralList.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations
class SpectralElement;
template <class T> class Vector;

// <summary>
// Least Squares fitting of spectral elements to spectrum
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tSpectralFit" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto module=Functionals>Functionals</linkto> module
//   <li> <linkto class=SpectralElement>SpectralElement</linkto> class
// </prerequisite>
//
// <etymology>
// From spectral line and fitting
// </etymology>
//
// <synopsis>
// The SpectralFit class will do a non-linear least squares solution
// for a number of simultaneous spectral components. The initial guess
// of the elements is given in a set of SpectralElements. The final solution
// is returned in the same set.
//
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// To have a contained fitting of spectral profiles to an observed spectrum
// </motivation>
//
// <todo asof="2001/02/04">
//   <li> add more profile types
// </todo>

class SpectralFit {
 public:

  //# Constructors
  // Default constructor creates a default fitter without elements
  SpectralFit();
  // Construct for the given elements
  explicit SpectralFit(const SpectralList &in);
  // Copy constructor (deep copy)
  SpectralFit(const SpectralFit &other);
  // Destructor
  ~SpectralFit();

  //# Operators
  // Assignment (copy semantics)
  SpectralFit &operator=(const SpectralFit &other);

  //# Member functions
  // Set an element to be fitted
  // <thrown>
  //   <li> AipsError if index too large
  // </thrown>
  void setFitElement(uInt index, const SpectralElement &elem);

  // Add elements to be fitted
  // <group>
  void addFitElement(const SpectralElement &elem);
  void addFitElement(const SpectralList &elem);
  // </group>

  // Clear the list to be fitted (for a re-use of the SpectralFit object)
  void clear();

  // Get the list being fitted
  const SpectralList &list() const { return slist_p; };

  // Fit the elements as given by the specified spectral elements
  // at the frequencies x with values y. Weights of all points are equal.
  // The mask (if specified) means: use point if True. Returns 
  // the convergence status.
  // <group>
  template <class MT>
    Bool fit(const Vector<MT> &y, const Vector<MT> &x) {
       return fit(y, x, static_cast<const Vector<Bool> *const>(0)); }
  template <class MT> 
    Bool fit(const Vector<MT> &y,
	     const Vector<MT> &x, const Vector<Bool> &mask) {
    return fit(y, x, &mask); }
  // </group>

  // Fit the elements as given by the specified spectral elements
  // at the frequencies x with values y and weights sigma.
  // The mask (if specified) means: use point if True.
  // <group>
  template <class MT>
    Bool fit(const Vector<MT> &sigma,
             const Vector<MT> &y,
	     const Vector<MT> &x) {
    return fit(sigma, y, x, static_cast<const Vector<Bool> *const>(0)); }
  template <class MT> 
    Bool fit(const Vector<MT> &sigma,
             const Vector<MT> &y,
	     const Vector<MT> &x, const Vector<Bool> &mask) {
    return fit(sigma, y, x, &mask); }
  // </group>

  // Get the number of iterations last fit
  uInt nIterations() const { return iter_p; }

  // Get ChiSq of the last fit
  Double chiSq () const { return chiSq_p; }

 private:
  //#Data
  // Elements to be fitted
  SpectralList slist_p;
  // Number of iterations last fit
  uInt iter_p;
  // ChiSq of last fit
  Double chiSq_p;

  //# Member functions
  // Real fitters
  // <group>
  template <class MT>
    Bool fit(const Vector<MT> &y,
	     const Vector<MT> &x,
	     const Vector<Bool> *mask);
  template <class MT>
    Bool fit(const Vector<MT> &sigma,
             const Vector<MT> &y,
	     const Vector<MT> &x,
	     const Vector<Bool> *mask);
  // </group>
};


} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <components/SpectralComponents/SpectralFit2.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
