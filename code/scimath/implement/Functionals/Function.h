//# Function.h: Numerical functional interface class
//# Copyright (C) 2001
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

#if !defined(AIPS_FUNCTION_H)
#define AIPS_FUNCTION_H

//# Includes
#include <aips/aips.h>
#include <aips/Functionals/Functional.h>
#include <aips/Functionals/FunctionParam.h>
#include <aips/Functionals/FunctionTraits.h>
#include <aips/Arrays/Vector.h>
#include <aips/Utilities/Assert.h>

//# Forward declarations
#include <aips/iosfwd.h>

// <summary> Numerical functional interface class
// </summary>

// <use visibility=export>

// <reviewed reviewer="tcornwel" date="1996/02/22" tests="" demos="dG1D">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Functional">Functional</linkto>
//   <li> <linkto class="FunctionParam">FunctionParam</linkto>
// </prerequisite>
//
// <synopsis>
// A <src>Function</src> is used for classes which map a
// scalar or n-dimensional Vector of type <src>T</src> into a <src>T</src>.
// The object also has zero or more parameters which can be masked
// if necessary, and be used in the <src>Fitting</src> module, and, implicitly,
// in the <linkto class=AutoDiff>AutoDiff</linkto> differentiation module.
//
// The parameter interface is provided by the
// <linkto class="FunctionParam"><src>FunctionParam</src></linkto> class.
//
// The function calls implemented are:
// <ul>
// <li> <src>operator()()</src>
// <li> <src>operator()(const T &x)</src>
// <li> <src>operator()(const Vector<T> &x)</src>
// <li> <src>operator()(Function::FunctionArg x)</src>
// </ul>
// These calls are (in debug mode) tested for correct number of arguments,
// after which they call a <src>T eval(FunctionArg x) const = 0</src> to
// be implemented in derived classes. The derived class should also implement 
// an <src>uInt ndim() const = 0</src>. The derived class can access the
// nth parameter with the <src>[n]</src> operator, and the corresponding
// mask with <src>mask(n)</src> method.
// The variables are referenced with <src>x[i]</src>.
//
// </synopsis>

// <example>
// A complete implementation of say an <src>A.sin(2pi.f.x)</src> with
// parameters amplitude(<em>A</em>) and frequency(<em>f</em>) and variable
// time(<em>x</em>) could be:
// <srcblock>
//   //# Sinusoid.h
//   #include <aips/aips.h>
//   #include <aips/Functionals/Function.h>
//   #include <aips/Mathematics/Constants.h>
//   #include <aips/Mathematics/Math.h>
//   // The sinusoid class
//   template<class T> class Sinusoid : public Function<T> {
//    public:
//     // For easy reference of the parameters
//     enum { AMPL=0, FREQ };
//     // Constructors. Defaults are A=1, f=1
//     Sinusoid() : Function<T>(2) {
//         param_p[AMPL] = T(1.0); param_p[FREQ] = T(1.0); }
//     explicit Sinusoid(const T &ampl) : Function<T>(2) {
//         param_p[AMPL] = ampl; param_p[FREQ] = T(1.0); }
//     Sinusoid(const T &ampl, const T &freq) : Function<T>(2) {
//         param_p[AMPL] = ampl; param_p[FREQ] = freq; }
//     Sinusoid(const Sinusoid &other) : Function<T>(2) {
//         param_p[AMPL] = other.param_p[AMPL];
//         param_p[FREQ] = other.parameter[FREQ]; }
//     Sinusoid<T> &operator=(const Sinusoid<T> &other) {
//         if (this != &other) param_p = other.param_p;
//         return *this; }
//     virtual ~Sinusoid() {};
//     // Dimensionality
//     virtual uInt ndim() const { return 2; };
//     // Evaluate
//     virtual T eval(Function<T>::FunctionArg x) const {
//	  return param_p[AMPL]*sin(T(C::_2pi)*param_p[FREQ]*x[0]); };
//     // Copy it
//     virtual Function<T> *clone() const { return new Sinusoid<T>(param_p); };
//   };
// </srcblock>
// The following will calculate the value and the derivative for 
// <src>A=2; f=3; x=0.1;</src>
// <srcblock>
//     // The function objects for value, and for value + derivative
//     Sinusoid<Double> soid1(2.0, 3.0);
//     typedef AutoDiff<Double> Adif;
//     Sinusoid<Adif> soid2(Adif(2,2,0), Adif(3,2,1));
//     cout << "Value: " << soid1(0.1) << endl;
//     cout << "(val, deriv): " << soid2(Adif(0.1)) << endl;
// </srcblock>
//
// A shorter version, where all parameter handling is done at user level
// could be:
// <srcblock>
//   //# Sinusoid.h
//   #include <aips/aips.h>
//   #include <aips/Functionals/Function.h>
//   #include <aips/Mathematics/Constants.h>
//   #include <aips/Mathematics/Math.h>
//   template<class T> class Sinusoid : public Function<T> {
//    public:
//     enum { AMPL=0, FREQ };
//     Sinusoid() : Function<T>(2){param_p[AMPL] T(1);param_p[FREQ]=T(1);}
//     virtual ~Sinusoid() {};
//     virtual uInt ndim() const { return 2; };
//     virtual T eval(Function<T>::FunctionArg x) const {
//	  return param_p[AMPL]*sin(T(C::_2pi)*param_p[FREQ]*x[0]); };
//     virtual Function<T> *clone() const { return new Sinusoid<T>param_p; };
//   };
// </srcblock>
// The following will calculate the value and the derivative for 
// <src>A=2; f=3; x=0.1;</src>
// <srcblock>
//     // The function objects for value, and for value + derivative
//     typedef AutoDiff<Double> Adif;
//     typedef Function<Double> FD;
//     typedef Function<AutoDiff<Double> > FAdif
//     Sinusoid<Double> soid1;
//     Sinusoid<Adif> soid2;
//     soid1[FD::AMPL] = 2; soid1[FD::FREQ] = 3;
//     soid2[FAdif::AMPL] = Adif(2,2,0);
//     soid2[FAdif::FREQ] = Adif(3,2,1);
//     cout << "Value: " << soid1(0.1) << endl;
//     cout << "(val, deriv): " << soid2(Adif(0.1)) << endl;
// </srcblock>
// </example>

// <motivation>
// A function of more than one variable was required for a function which
// represents the sky brightness. Adjustable parameters were required for
// non-linear least squares fitting.
// </motivation>
//
// <templating arg=T>
//    <li> Besides the requirements set by the
//    <linkto class="Functional">Functional</linkto> base class, it must be
//    possible to form a <src>Vector<T></src>.
// </templating>
//
// <todo asof="2001/08/29">
//   <li> At some point, we may want to implement a letter-envelope class,
//   implement function arithmetic, etc.
//   <li> use maybe Poolstack for static Vector
// </todo>

template<class T> class Function :
public Functional<typename FunctionTraits<T>::ArgType, T>,
  public Functional<Vector<typename FunctionTraits<T>::ArgType>, T> {

 public:
  //# Typedefs
  typedef typename FunctionTraits<T>::ArgType ArgType;
  typedef const ArgType* FunctionArg;

  //# Constructors
  // Constructors for FunctionParam
  // <group>
  Function() : param_p() {};
  explicit Function(const uInt n) : param_p(n) {};
  explicit Function(const Vector<T> &in) : param_p(in) {};
  Function(const FunctionParam<T> &other) : param_p(other) {};
  // </group>

  // Destructor
  virtual ~Function() {};
  
  // Returns the number of dimensions of function
  virtual uInt ndim() const = 0;
  // Returns the number of parameters
  uInt nparameters() const { return param_p.nelements(); };

  //# Operators
  // Manipulate the nth parameter (0-based) with no index check
  // <group>
  T &operator[](const uInt n) { return param_p[n]; };
  const T &operator[](const uInt n) const{ return param_p[n]; };
  // </group>
  // Evaluate this function object at <src>x</src>or at <src>x, y</src>.
  // The length of <src>x</src> must be greater than or equal to
  // <src>ndim()</src>.
  // <group>
  virtual T operator()() const {
    DebugAssert(ndim()==0, AipsError); return this->eval(FunctionArg(0)); };
  virtual T operator()(const ArgType &x) const {
    DebugAssert(ndim()<=1, AipsError); return this->eval(&x); };
  virtual T operator()(const Vector<ArgType> &x) const;
  virtual T operator()(FunctionArg x) const { return this->eval(x); };
  virtual T operator()(const ArgType &x, const ArgType &y) const;
  // </group>

  //# Member functions
  // Manipulate the mask associated with the nth parameter
  // (e.g. to indicate whether the parameter is adjustable or nonadjustable).
  // Note no index check.
  // <group>
  Bool &mask(const uInt n) { return param_p.mask(n); };
  const Bool &mask(const uInt n) const { return param_p.mask(n); };
  // </group>
  // Return the parameter interface
  // <group>
  const FunctionParam<T> &parameters() const { return param_p; };
  FunctionParam<T> &parameters() { return param_p; };
  // </group>
  // Evaluate the function object
  virtual T eval(FunctionArg x) const = 0;
  // Print the function (i.e. the parameters)
  ostream &print(ostream &os) const { return param_p.print(os); };
  // Return a copy of this object from the heap. The caller is responsible 
  // for deleting the returned pointer.
  virtual Function<T> *clone() const = 0;

  protected:
  //# Data
  // The parameters and masks
  FunctionParam<T> param_p;
  // Aid for non-contiguous argument storage
  mutable Vector<typename FunctionTraits<T>::ArgType> arg_p;

};

//# Global functions
// <summary> Global functions </summary>
// <group name=Output>
// Output declaration
template<class T>
ostream &operator<<(ostream &os, const Function<T> &fun);
// </group>

//# Inlines
template<class T>
inline ostream &operator<<(ostream &os, const Function<T> &fun) {
  return fun.print(os); };

#endif
