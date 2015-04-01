//# FunctionParam.h: Container of function parameters with masking flags
//# Copyright (C) 2001,2002,2005
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

#ifndef SCIMATH_FUNCTIONPARAM_H
#define SCIMATH_FUNCTIONPARAM_H

//# Include files
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/scimath/Functionals/FunctionTraits.h>

//# Forward declarations
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>Container of function parameters with masking flags
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="tcornwel" date="1996/02/22" tests="tGaussian2D"
//	 demos="">
// </reviewed>
//
// <synopsis>
// <src>FunctionParam</src> is used to provide an interface to an entity which
// has parameters that can be flagged.
// This is useful, for example, in implementing parameter
// fitting which operates on generic function objects.
// 
// Each parameter can be masked. The mask can, e.g., be used to indicate to a
// generic least-squares fitting routine to only adjust parameters with
// a <em>True</em> mask (the default). For that reason methods that only
// handle <em>True</em> data items have names with <em>Adjust</em> in
// the names. In general the user should not be concerned with these
// methods, but should only manipulate the parameter <src>flags</src> and
// <src>values</src>.
//
// </synopsis>
//
// <example>
// See the <linkto class=Function>Function</linkto> class for a usage
// interface.
// </example>
//
// <motivation>
// Generically manipulatable adjustable parameters are important for fitting.
// </motivation>
//
// <templating arg=T>
//  <li> <src>T</src> must have a default constructor, assignment operator,
//	 and copy constructor (for the Vector interface). 
//  <li> all standard mathematical should be applicable if the 
//	parameter interface is used for the calculation of
//	<src>Functions</src>.
// </templating>
//
// <todo asof="2001/08/28">
//   <li> Nothing I know of
// </todo>

template<class T> class FunctionParam {
 public:
  //# Constructors
  // Construct a default FunctionParam with 0 parameters
  FunctionParam();
  // Construct a FunctionParam with <src>n</src> parameters with zero value and
  // all masks <em>True</em>
  explicit FunctionParam(const uInt n);
  // Construct a FunctionParam from the given vector, with all masks 
  // <em>True</em>
  explicit FunctionParam(const Vector<T> &in);
  // Copy constructor (deep copy)
  FunctionParam(const FunctionParam<T> &other);
  // Copy from different type (deep copy)
  template <class W>
  FunctionParam(const FunctionParam<W> &other) 
    : npar_p(other.getParameters().nelements()),
    param_p(npar_p), mask_p(npar_p),
    maskedPtr_p(0) {
    for (uInt i=0; i<npar_p; ++i) {
      FunctionTraits<T>::
	setValue(param_p[i],
		 FunctionTraits<W>::getValue(other.getParameters()[i]),
		 npar_p, i);
    }
    mask_p = other.getParamMasks();
  }

  // Destructor
  virtual ~FunctionParam();

  //# Operators
  // Copy assignment (deep copy)
  FunctionParam &operator=(const FunctionParam<T> &other);
  // Manipulate the nth parameter (0-based) with no index check
  // <group>
  T &operator[](const uInt n) { return param_p[n]; }
  const T &operator[](const uInt n) const { return param_p[n]; }
  // </group>
  // Compare two parameter sets for equal size, values and masks.
  // <group>
  Bool operator==(const FunctionParam<T> &other) const;
  Bool operator!=(const FunctionParam<T> &other) const;
  // </group>

  //# Member functions
  // Return the number of parameters
  uInt nelements() const { return param_p.nelements(); }
  // Manipulate the nth parameter (0-based) with no index check
  // <group>
  T &parameter(const uInt n) { return param_p[n]; }
  const T &parameter(const uInt n) const{ return param_p[n]; }
  // </group>

  // Manipulate the mask associated with the nth parameter
  // (e.g. to indicate whether the parameter is adjustable or nonadjustable).
  // Note no index check.
  // <group>
  Bool &mask(const uInt n);
  const Bool &mask(const uInt n) const { return mask_p[n]; }
  // </group>

  // Get all parameters at once.  Returns zero length
  // Vector if there are no parameters.
  const Vector<T> &getParameters() const { return param_p; }
  
  // Set all the parameters at once. Only the minimum of the input number and
  // the object number of parameters will be set.
  void setParameters(const Vector<T> &params);

  // Get all parameter masks at once.  Returns zero length
  // Vector if there are no parameters.
  const Vector<Bool> &getParamMasks() const { return mask_p; }
    
  // Set all parameter masks at once. Only the minimum of the input number and
  // the object number of parameters will be set.
  void setParamMasks(const Vector<Bool> &masks);

  // Operations on the masked parameters only. For possible re-use the
  // results are cached.
  // <group>
  // Number of masked (<src>=True</src>) parameters
  uInt nMaskedParameters() const;
  // All masked parameters only
  // <group>
  Vector<T> &getMaskedParameters() const;
  void setMaskedParameters(Vector<T> &in);
  // </group>
  // </group>

  // Output the parameters
  ostream &print(ostream &os) const;

 private:
  //# Data
  // Number of parameters
  uInt npar_p;
  // Parameters
  Vector<T> param_p;
  // Masks
  Vector<Bool> mask_p;
  // Cached masked data
  mutable Vector<T> *maskedPtr_p;

  //# Methods
  // Create a cached version of the masked parameter list
  void createMaskedPtr() const;
  // Clear the masked parameter list
  void clearMaskedPtr() const;

};

//# Global functions
// <summary> Global functions </summary>
// <group name=Output>
// Output declaration
template<class T>
ostream &operator<<(ostream &os, const FunctionParam<T> &par);
// </group>

//# Inlines
template<class T>
inline ostream &operator<<(ostream &os, const FunctionParam<T> &par) {
  return par.print(os); }


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/FunctionParam.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif

