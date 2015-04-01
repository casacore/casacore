//# Function.h: Numerical functional interface class
//# Copyright (C) 2001,2002,2003,2004,2005
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

#ifndef SCIMATH_FUNCTION_H
#define SCIMATH_FUNCTION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicMath/Functional.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/scimath/Functionals/FunctionParam.h>
#include <casacore/scimath/Functionals/FunctionTraits.h>

//# Forward declarations
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
class String;
class RecordInterface;
 
// <summary> Numerical functional interface class
// </summary>

// <use visibility=export>

// <reviewed reviewer="tcornwel" date="1996/02/22" tests="tGaussian2D"
//	 demos="">
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
// A Function can have a <src>name()</src> which can be used in generic
// interfaces.
//
// The function calls implemented are:
// <ul>
// <li> <src>operator()()</src>
// <li> <src>operator()(const T &x)</src>
// <li> <src>operator()(const Vector<T> &x)</src>
// <li> <src>operator()(Function::FunctionArg x)</src>
// <li> <src>operator()(const T &x, const T &y)</src> (for 2D)
// <li> <src>operator()(const T &x, const T &y, const T &z)</src> (for 3D)
// </ul>
// The <src>T</src> in the above is the <src>Function::ArgType</src>
// as derived from the <linkto class="FunctionTraits">FunctionTraits</linkto>
// class.
// These calls are (in debug mode) tested for the correct number of arguments,
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
//   #include <casacore/casa/aips.h>
//   #include <casacore/scimath/Functionals/Function.h>
//   #include <casacore/casa/BasicSL/Constants.h>
//   #include <casacore/casa/BasicMath/Math.h>
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
//     virtual ~Sinusoid() {}
//     // Dimensionality
//     virtual uInt ndim() const { return 2; }
//     // Evaluate
//     virtual T eval(Function<T>::FunctionArg x) const {
//	  return param_p[AMPL]*sin(T(C::_2pi)*param_p[FREQ]*x[0]); }
//     // Copy it
//     virtual Function<T> *clone() const { return new Sinusoid<T>(param_p); }
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
//   #include <casacore/casa/aips.h>
//   #include <casacore/scimath/Functionals/Function.h>
//   #include <casacore/casa/BasicSL/Constants.h>
//   #include <casacore/casa/BasicMath/Math.h>
//   template<class T> class Sinusoid : public Function<T> {
//    public:
//     enum { AMPL=0, FREQ };
//     Sinusoid() : Function<T>(2){param_p[AMPL] T(1);param_p[FREQ]=T(1);}
//     virtual ~Sinusoid() {}
//     virtual uInt ndim() const { return 2; }
//     virtual T eval(Function<T>::FunctionArg x) const {
//	  return param_p[AMPL]*sin(T(C::_2pi)*param_p[FREQ]*x[0]); }
//     virtual Function<T> *clone() const { return new Sinusoid<T>param_p; }
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
// <todo asof="2005/01/20">
//   <li> At some point, we may want to implement a letter-envelope class,
//   implement function arithmetic, etc.
//   <li> use maybe Poolstack for static Vector
// </todo>

 template<class T, class U=T> class Function :
   public Functional<typename FunctionTraits<T>::ArgType, U>,
   public Functional<Vector<typename FunctionTraits<T>::ArgType>, U> {
    
     public:
     //# Typedefs
     typedef typename FunctionTraits<T>::ArgType ArgType;
     typedef const ArgType* FunctionArg;
 
     //# Constructors
     // Constructors
     // <group>
     Function() : param_p(), arg_p(0), parset_p(False), locked_p(False) {}
     explicit Function(const uInt n) : param_p(n), arg_p(0), parset_p(False),
     locked_p(False) {}
     explicit Function(const Vector<T> &in) : param_p(in), arg_p(0),
     parset_p(False), locked_p(False) {}
     Function(const FunctionParam<T> &other) : param_p(other), arg_p(0),
     parset_p(False), locked_p(False) {}
     template <class W, class X>
     Function(const Function<W,X> &other) : param_p(other.parameters()),
     arg_p(0), parset_p(other.parsetp()), locked_p(False) {}
     Function(const Function<T,U> &other) :
       Functional<typename FunctionTraits<T>::ArgType, U>        (other),
       Functional<Vector<typename FunctionTraits<T>::ArgType>, U>(other),
       param_p(other.param_p),
       arg_p(other.arg_p),
       parset_p(other.parset_p),
       locked_p(False)
     {}
     // </group>
     
     // Destructor
     virtual ~Function() {}
     
     // Returns the number of dimensions of function
     virtual uInt ndim() const = 0;
     // Returns the number of parameters
     uInt nparameters() const { return param_p.nelements(); }
     
     // Evaluate the function object
     virtual U eval(FunctionArg x) const = 0;

     //# Operators
     // Manipulate the nth parameter (0-based) with no index check
     // <group>
     T &operator[](const uInt n) { parset_p |= !locked_p;
     return param_p[n]; }
     const T &operator[](const uInt n) const { return param_p[n]; }
     // </group>
     // Evaluate this function object at <src>x</src>or at <src>x, y</src>.
     // The length of <src>x</src> must be greater than or equal to
     // <src>ndim()</src>.
     // <group>
     virtual U operator()() const {
       DebugAssert(ndim()==0, AipsError); return eval(FunctionArg(0)); }
     virtual U operator()(const ArgType &x) const {
       DebugAssert(ndim()<=1, AipsError); return eval(&x); }
     virtual U operator()(const Vector<ArgType> &x) const;
     virtual U operator()(FunctionArg x) const { return eval(x); }
     virtual U operator()(const ArgType &x, const ArgType &y) const;
     virtual U operator()(const ArgType &x, const ArgType &y,
			  const ArgType &z) const;
     // </group>
     
     //# Member functions
     // Specify the name associated with the function (default will be
     // <src>unknown</src>)
     virtual const String &name() const;
     // Manipulate the mask associated with the nth parameter
     // (e.g. to indicate whether the parameter is adjustable or
     // nonadjustable).
     // Note: no index check.
     // <group>
     Bool &mask(const uInt n) { parset_p |= !locked_p;
     return param_p.mask(n); }
     const Bool &mask(const uInt n) const { return param_p.mask(n); }
     // </group>
     // Return the parameter interface
     // <group>
     const FunctionParam<T> &parameters() const { return param_p; }
     FunctionParam<T> &parameters() { parset_p = True; return param_p; }
     // </group>
     // Get <src>arg_p</src> and <src>parset_p</src>. Necessary for reasons
     // of protection in the copying of non-conforming Functions.
     // <group>
     const Vector<ArgType> &argp() const { return arg_p; }
     Bool parsetp() const { return parset_p; }
     // </group>
     // Compiler cannot always find the correct 'const' version of parameter
     // access. In cases where this would lead to excessive overheads in
     // moving parameters around (like in <src>CompoundFunction</src>) the
     // parameter changing can be set to be locked, and no changes are
     // assumed.
     // <group>
     void lockParam()   { locked_p = True; }
     void unlockParam() { locked_p = False; }
     // </group>
     
     // get/set the function mode.  These provide an interface to 
     // function-specific configuration or state that controls how the 
     // function calculates its values but otherwise does not qualify as 
     // a parameter.  Some part of the state, for example, might have a 
     // type different from that of T.  The state is passed as fields of a
     // record, mode--the names, types and values of which are specific to
     // the implementing function and should be documented in the implementing
     // class.  It is recommended that all possible inputs passed to this 
     // function via setMode() be considered optional such that if the 
     // record omits a legal field, that part of the state is left unchanged. 
     // Fields not recognized by the implementing class should be ignored.
     // An exception should be thrown if a recognized field contains illegal 
     // data.  The default implementations for both getMode() and setMode() 
     // ignore the input record.  
     // <group>
     virtual void setMode(const RecordInterface& mode);
     virtual void getMode(RecordInterface& mode) const;
     // </group>
     
     // return True if the implementing function supports a mode.  The default
     // implementation returns False.
     virtual Bool hasMode() const;
     
     // Print the function (i.e. the parameters)
     ostream &print(ostream &os) const { return param_p.print(os); }
     // Return a copy of this object from the heap. The caller is responsible 
     // for deleting this pointer. The <src>cloneAD</src> will return a clone
     // with an <src>AutoDef<T></src>; the <src>cloneNonAD</src> a clone
     // with <src><T></src>. An <src>AipsError</src> will be thrown if the
     // <src>cloneAD()</src> or <src>cloneNonAD()</src> is not implemented
     // for a specific function.
     // <group>
     virtual Function<T,U> *clone() const = 0;
     virtual Function<typename FunctionTraits<T>::DiffType> *cloneAD() const;
     virtual Function<typename FunctionTraits<T>::BaseType>
    *cloneNonAD() const;
  // </group>
  
protected:
  //# Data
  // The parameters and masks
  FunctionParam<T> param_p;
  // Aid for non-contiguous argument storage
  mutable Vector<ArgType> arg_p;
  // Indicate parameter written
  mutable Bool parset_p;
  // Indicate that parameters are expected to be locked from changing
  mutable Bool locked_p;
};

//# Global functions
// <summary> Global functions </summary>
// <group name=Output>
// Output declaration
template<class T, class U>
ostream &operator<<(ostream &os, const Function<T,U> &fun);
// </group>

//# Inlines
template<class T, class U>
inline ostream &operator<<(ostream &os, const Function<T,U> &fun) {
  return fun.print(os); }

} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/Function.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
