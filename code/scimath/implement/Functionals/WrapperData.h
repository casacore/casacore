//# NQWrapperData.h: Aid in constructing function objects from C++ functions 
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
//# $Id$

#if !defined(AIPS_NQWRAPPERDATA_H)
#define AIPS_NQWRAPPERDATA_H

//# Includes
#include <aips/aips.h>
#include <aips/Functionals/WrapperBase.h>

//# Forward declarations

// <summary> Aid in constructing function objects from C++ functions 
// </summary>
//
// <use visibility=local>
//
// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Function">Function</linkto> class
//   <li> <linkto class="FunctionParam">FunctionParam</linkto>
// </prerequisite>
//
// <synopsis>
// This class is provided to enable compile time selection of the
// appropriate function call. Each template incarnation represent a
// function call interface definition.
// </synopsis>
//
// <example>
// <srcblock>
// Float func(const Vector<Float>& x) {return x(0)*x(1);}        // x*y
// // Convert C++ functions to Functionals
// NQFunctionWrapper<Float> Func(func, 2);
// </srcblock>
//

template <class T, class U, class V, Bool hasX, Bool hasParam>
class NQWrapperData : public NQWrapperBase<T> {
 public:
  //# Constructors
  // Default constructor: to allow arrays of functions
  NQWrapperData();

  // Destructor
  virtual ~NQWrapperData() {};

  //# Operators    
  // Evaluate the function at <src>x</src>.
  // <group>
  virtual T eval(typename Function<T>::FunctionArg x, const V &par) const {};
  // </group>

  //# Member functions

 protected:
};

#define NQWrapperData_TT NQWrapperData

// <summary> Specialization for calls with argument and parameter
// </summary>
// <synopsis> Note that the actual name of the class is
// <src>NQWrapperData</src>. The special name is only for the use of 
// cxx2html.
// </synopsis>

template <class T>
class NQWrapperData_TT<T,T,T,True,True> : public NQWrapperBase<T> {
  typedef NQWrapperData_TT<T,T,T,True,True> myData;
 public:
  //# Constructors
  // Standard constructor
  explicit NQWrapperData_TT(T(*f)(const T&, const T&), uInt dim=1) :
    NQWrapperBase<T>(dim), pf_p(f) {};

  // Destructor
  virtual ~NQWrapperData_TT() {};

  //# Operators    
  // Evaluate the function at <src>x</src>.
  virtual T eval(typename Function<T>::FunctionArg x,
		 const Vector<T> &par) const {
    if (pf_p) {
      return
	pf_p((*static_cast<const typename Function<T>::FunctionArg>(x)),
	     par[0]);
    };
    return T(0); };

  //# Member functions

 protected:
  //# Data
  // Function to call
  T (*pf_p)(const T&, const T&);
 
 private:
  // Copy constructor and assignment (not implemented)
  // <group>
  NQWrapperData_TT(const myData &other);
  myData &operator=(const myData &other);
  // </group>

};

#undef NQWrapperData_TT

#define NQWrapperData_VT NQWrapperData

// <summary> Specialization for calls with argument and parameter
// </summary>
// <synopsis> Note that the actual name of the class is
// <src>NQWrapperData</src>. The special name is only for the use of 
// cxx2html.
// </synopsis>

template <class T>
class NQWrapperData_VT<T,Vector<T>,T,True,True> : public NQWrapperBase<T> {
  typedef NQWrapperData_VT<T,Vector<T>,T,True,True> myData;
 public:
  explicit NQWrapperData_VT(T(*f)(const Vector<T>&, const T&), uInt dim=1) :
    NQWrapperBase<T>(dim), pf_p(f) {};
  virtual ~NQWrapperData_VT() {};
  virtual T eval(typename Function<T>::FunctionArg x,
		 const Vector<T> &par) const {
    if (pf_p) {
      for (uInt i=0; i<ndim_p; ++i) arg_p[i] = x[i];
      return pf_p(arg_p, par[0]); };
    return T(0); };
 protected:
  T (*pf_p)(const Vector<T>&, const T&);
 private:
  NQWrapperData_VT(const myData &other);
  myData &operator=(const myData &other);
};

#undef NQWrapperData_VT

#define NQWrapperData_TV NQWrapperData

// <summary> Specialization for calls with argument and parameters
// </summary>
// <synopsis> Note that the actual name of the class is
// <src>NQWrapperData</src>. The special name is only for the use of 
// cxx2html.
// </synopsis>

template <class T>
class NQWrapperData_TV<T,T,Vector<T>,True,True> : public NQWrapperBase<T> {
  typedef NQWrapperData_TV<T,T,Vector<T>,True,True> myData;
 public:
  explicit NQWrapperData_TV(T(*f)(const T&, const Vector<T>&), uInt dim=1) :
    NQWrapperBase<T>(dim), pf_p(f) {};
  virtual ~NQWrapperData_TV() {};
  virtual T eval(typename Function<T>::FunctionArg x,
		 const Vector<T> &par) const {
    if (pf_p) {
      return pf_p((*static_cast<const typename Function<T>::FunctionArg>(x)),
		  par);
    };
    return T(0); };
 protected:
  T (*pf_p)(const T&, const Vector<T>&);
 private:
  NQWrapperData_TV(const myData &other);
  myData &operator=(const myData &other);
};

#undef NQWrapperData_TV

#define NQWrapperData_VV NQWrapperData

// <summary> Specialization for calls with argument and parameters
// </summary>
// <synopsis> Note that the actual name of the class is
// <src>NQWrapperData</src>. The special name is only for the use of 
// cxx2html.
// </synopsis>

template <class T>
class NQWrapperData_VV<T,Vector<T>,Vector<T>,True,True> :
public NQWrapperBase<T> {
  typedef NQWrapperData_VV<T,Vector<T>,Vector<T>,True,True> myData;
 public:
  explicit NQWrapperData_VV(T(*f)(const Vector<T>&, const Vector<T>&),
			    uInt dim=1) :
    NQWrapperBase<T>(dim), pf_p(f) {};
  virtual ~NQWrapperData_VV() {};
  virtual T eval(typename Function<T>::FunctionArg x,
		 const Vector<T> &par) const {
    if (pf_p) {
      for (uInt i=0; i<ndim_p; ++i) arg_p[i] = x[i];
      return pf_p(arg_p, par); };
    return T(0); };
 protected:
  T (*pf_p)(const Vector<T>&, const Vector<T>&);
 private:
  NQWrapperData_VV(const myData &other);
  myData &operator=(const myData &other);
};

#undef NQWrapperData_VV

#define NQWrapperData_FT NQWrapperData

// <summary> Specialization for calls with no arguments and parameter
// </summary>
// <synopsis> Note that the actual name of the class is
// <src>NQWrapperData</src>. The special name is only for the use of 
// cxx2html.
// </synopsis>

template <class T>
class NQWrapperData_FT<T,T,T,False,True> : public NQWrapperBase<T> {
  typedef NQWrapperData_FT<T,T,T,False,True> myData;
 public:
  explicit NQWrapperData_FT(T(*f)(const T&)) :
    NQWrapperBase<T>(0), pf_p(f) {};
  virtual ~NQWrapperData_FT() {};
  virtual T eval(typename Function<T>::FunctionArg x,
		 const Vector<T> &par) const {
    if (pf_p) return pf_p(par[0]);
    return T(0); };
 protected:
  T (*pf_p)(const T&);
 private:
  NQWrapperData_FT(const myData &other);
  myData &operator=(const myData &other);
};

#undef NQWrapperData_FT

#define NQWrapperData_FV NQWrapperData

// <summary> Specialization for calls with no arguments and parameters
// </summary>
// <synopsis> Note that the actual name of the class is
// <src>NQWrapperData</src>. The special name is only for the use of 
// cxx2html.
// </synopsis>

template <class T>
class NQWrapperData_FV<T,T,Vector<T>,False,True> : public NQWrapperBase<T> {
  typedef NQWrapperData_FV<T,T,Vector<T>,False,True> myData;
 public:
  explicit NQWrapperData_FV(T(*f)(const Vector<T>&)) :
    NQWrapperBase<T>(0), pf_p(f) {};
  virtual ~NQWrapperData_FV() {};
  virtual T eval(typename Function<T>::FunctionArg x,
		 const Vector<T> &par) const {
    if (pf_p) return pf_p(par);
    return T(0); };
 protected:
  T (*pf_p)(const Vector<T>&);
 private:
  NQWrapperData_FV(const myData &other);
  myData &operator=(const myData &other);
};

#undef NQWrapperData_FV

#define NQWrapperData_TF NQWrapperData

// <summary> Specialization for calls with argument and no parameters
// </summary>
// <synopsis> Note that the actual name of the class is
// <src>NQWrapperData</src>. The special name is only for the use of 
// cxx2html.
// </synopsis>

template <class T>
class NQWrapperData_TF<T,T,T,True,False> : public NQWrapperBase<T> {
  typedef NQWrapperData_TF<T,T,T,True,False> myData;
 public:
  explicit NQWrapperData_TF(T(*f)(const T&), uInt dim=1) :
    NQWrapperBase<T>(dim), pf_p(f) {};
  virtual ~NQWrapperData_TF() {};
  virtual T eval(typename Function<T>::FunctionArg x, 
		 const Vector<T> &par) const {
    if (pf_p) {
      return pf_p((*static_cast<const typename Function<T>::FunctionArg>(x)));
    };
    return T(0); };
 protected:
  T (*pf_p)(const T&);
 private:
  NQWrapperData_TF(const myData &other);
  myData &operator=(const myData &other);
};

#undef NQWrapperData_TF

#define NQWrapperData_VF NQWrapperData

// <summary> Specialization for calls with argument and no parameters
// </summary>
// <synopsis> Note that the actual name of the class is
// <src>NQWrapperData</src>. The special name is only for the use of 
// cxx2html.
// </synopsis>

template <class T>
class NQWrapperData_VF<T,Vector<T>,T,True,False> : public NQWrapperBase<T> {
  typedef NQWrapperData_VF<T,Vector<T>,T,True,False> myData;
 public:
  explicit NQWrapperData_VF(T(*f)(const Vector<T>&), uInt dim=1) :
    NQWrapperBase<T>(dim), pf_p(f) {};
  virtual ~NQWrapperData_VF() {};
  virtual T eval(typename Function<T>::FunctionArg x,
		 const Vector<T> &par) const {
    if (pf_p) {
      for (uInt i=0; i<ndim_p; ++i) arg_p[i] = x[i];
      return pf_p(arg_p); };
    return T(0); };
 protected:
  T (*pf_p)(const Vector<T>&);
 private:
  NQWrapperData_VF(const myData &other);
  myData &operator=(const myData &other);
};

#undef NQWrapperData_VF

#define NQWrapperData_FF NQWrapperData

// <summary> Specialization for calls with no arguments and no parameters
// </summary>
// <synopsis> Note that the actual name of the class is
// <src>NQWrapperData</src>. The special name is only for the use of 
// cxx2html.
// </synopsis>

template <class T>
class NQWrapperData_FF<T,T,T,False,False> : public NQWrapperBase<T> {
  typedef NQWrapperData_FF<T,T,T,True,False> myData;
 public:
  explicit NQWrapperData_FF(T(*f)()) :
    NQWrapperBase<T>(0), pf_p(f) {};
  virtual ~NQWrapperData_FF() {};
  virtual T eval(typename Function<T>::FunctionArg x,
		 const Vector<T> &par) const {
    if (pf_p) return pf_p();
    return T(0); };
 protected:
  T (*pf_p)();
 private:
  NQWrapperData_FF(const myData &other);
  myData &operator=(const myData &other);
};

#undef NQWrapperData_FF

#endif
