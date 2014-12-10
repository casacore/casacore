//# WrapperData.h: Aid in constructing function objects from C++ functions 
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

#ifndef SCIMATH_WRAPPERDATA_H
#define SCIMATH_WRAPPERDATA_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/WrapperBase.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary> Aid in constructing function objects from C++ functions 
// </summary>
//
// <use visibility=local>
//
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
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
// FunctionWrapper<Float> Func(func, 2);
// </srcblock>
//

template <class T, class U, class V, Bool hasX, Bool hasParam>
class WrapperData : public WrapperBase<T>
{
public:
  //# Constructors
  // Default constructor: to allow arrays of functions
  WrapperData();

  // Destructor
  virtual ~WrapperData() {}

  //# Operators    
  // Evaluate the function at <src>x</src>.
  // <group>
  virtual T eval(typename Function<T>::FunctionArg, const V&) const {}
  // </group>

  //# Member functions

protected:
  //# Make members of parent classes known.
  using WrapperBase<T>::ndim_p;
  using WrapperBase<T>::arg_p;
};


#define WrapperData_TT WrapperData

// <summary> Specialization for calls with argument and parameter
// </summary>
// <synopsis> Note that the actual name of the class is
// <src>WrapperData</src>. The special name is only for the use of 
// cxx2html.
// </synopsis>

template <class T>
class WrapperData_TT<T,T,T,True,True> : public WrapperBase<T>
{
  typedef WrapperData_TT<T,T,T,True,True> myData;

public:
  //# Constructors
  // Standard constructor
  explicit WrapperData_TT(T(*f)(const T&, const T&), uInt dim=1) :
    WrapperBase<T>(dim), pf_p(f) {}

  // Destructor
  virtual ~WrapperData_TT() {}

  //# Operators    
  // Evaluate the function at <src>x</src>.
  virtual T eval(typename Function<T>::FunctionArg x,
		 const Vector<T> &par) const {
    if (pf_p) {
      return
	pf_p((*static_cast<const typename Function<T>::FunctionArg>(x)),
	     par[0]);
    }
    return T(0); }

  //# Member functions

protected:
  //# Data
  // Function to call
  T (*pf_p)(const T&, const T&);
 
private:
  // Copy constructor and assignment (not implemented)
  // <group>
  WrapperData_TT(const myData &other);
  myData &operator=(const myData &other);
  // </group>

protected:
  //# Make members of parent classes known.
  using WrapperBase<T>::ndim_p;
  using WrapperBase<T>::arg_p;
};

#undef WrapperData_TT


#define WrapperData_VT WrapperData

// <summary> Specialization for calls with argument and parameter
// </summary>
// <synopsis> Note that the actual name of the class is
// <src>WrapperData</src>. The special name is only for the use of 
// cxx2html.
// </synopsis>

template <class T>
class WrapperData_VT<T,Vector<T>,T,True,True> : public WrapperBase<T>
{
  typedef WrapperData_VT<T,Vector<T>,T,True,True> myData;

public:
  explicit WrapperData_VT(T(*f)(const Vector<T>&, const T&), uInt dim=1) :
    WrapperBase<T>(dim), pf_p(f) {}
  virtual ~WrapperData_VT() {}
  virtual T eval(typename Function<T>::FunctionArg x,
		 const Vector<T> &par) const {
    if (pf_p) {
      for (uInt i=0; i<ndim_p; ++i) arg_p[i] = x[i];
      return pf_p(arg_p, par[0]); }
    return T(0); }

protected:
  T (*pf_p)(const Vector<T>&, const T&);

private:
  WrapperData_VT(const myData &other);
  myData &operator=(const myData &other);

protected:
  //# Make members of parent classes known.
  using WrapperBase<T>::ndim_p;
  using WrapperBase<T>::arg_p;
};

#undef WrapperData_VT


#define WrapperData_TV WrapperData

// <summary> Specialization for calls with argument and parameters
// </summary>
// <synopsis> Note that the actual name of the class is
// <src>WrapperData</src>. The special name is only for the use of 
// cxx2html.
// </synopsis>

template <class T>
class WrapperData_TV<T,T,Vector<T>,True,True> : public WrapperBase<T>
{
  typedef WrapperData_TV<T,T,Vector<T>,True,True> myData;

public:
  explicit WrapperData_TV(T(*f)(const T&, const Vector<T>&), uInt dim=1) :
    WrapperBase<T>(dim), pf_p(f) {}
  virtual ~WrapperData_TV() {}
  virtual T eval(typename Function<T>::FunctionArg x,
		 const Vector<T> &par) const {
    if (pf_p) {
      return pf_p((*static_cast<const typename Function<T>::FunctionArg>(x)),
		  par);
    }
    return T(0); }

protected:
  T (*pf_p)(const T&, const Vector<T>&);

private:
  WrapperData_TV(const myData &other);
  myData &operator=(const myData &other);

protected:
  //# Make members of parent classes known.
  using WrapperBase<T>::ndim_p;
  using WrapperBase<T>::arg_p;
};

#undef WrapperData_TV


#define WrapperData_VV WrapperData

// <summary> Specialization for calls with argument and parameters
// </summary>
// <synopsis> Note that the actual name of the class is
// <src>WrapperData</src>. The special name is only for the use of 
// cxx2html.
// </synopsis>

template <class T>
class WrapperData_VV<T,Vector<T>,Vector<T>,True,True> :
public WrapperBase<T>
{
  typedef WrapperData_VV<T,Vector<T>,Vector<T>,True,True> myData;

public:
  explicit WrapperData_VV(T(*f)(const Vector<T>&, const Vector<T>&),
			    uInt dim=1) :
    WrapperBase<T>(dim), pf_p(f) {}
  virtual ~WrapperData_VV() {}
  virtual T eval(typename Function<T>::FunctionArg x,
		 const Vector<T> &par) const {
    if (pf_p) {
      for (uInt i=0; i<ndim_p; ++i) arg_p[i] = x[i];
      return pf_p(arg_p, par); }
    return T(0); }

protected:
  T (*pf_p)(const Vector<T>&, const Vector<T>&);

private:
  WrapperData_VV(const myData &other);
  myData &operator=(const myData &other);

protected:
  //# Make members of parent classes known.
  using WrapperBase<T>::ndim_p;
  using WrapperBase<T>::arg_p;
};

#undef WrapperData_VV


#define WrapperData_FT WrapperData

// <summary> Specialization for calls with no arguments and parameter
// </summary>
// <synopsis> Note that the actual name of the class is
// <src>WrapperData</src>. The special name is only for the use of 
// cxx2html.
// </synopsis>

template <class T>
class WrapperData_FT<T,T,T,False,True> : public WrapperBase<T>
{
  typedef WrapperData_FT<T,T,T,False,True> myData;

public:
  explicit WrapperData_FT(T(*f)(const T&)) :
    WrapperBase<T>(0), pf_p(f) {}
  virtual ~WrapperData_FT() {}
  virtual T eval(typename Function<T>::FunctionArg,
		 const Vector<T> &par) const {
    if (pf_p) return pf_p(par[0]);
    return T(0); }

protected:
  T (*pf_p)(const T&);

private:
  WrapperData_FT(const myData &other);
  myData &operator=(const myData &other);

protected:
  //# Make members of parent classes known.
  using WrapperBase<T>::ndim_p;
  using WrapperBase<T>::arg_p;
};

#undef WrapperData_FT


#define WrapperData_FV WrapperData

// <summary> Specialization for calls with no arguments and parameters
// </summary>
// <synopsis> Note that the actual name of the class is
// <src>WrapperData</src>. The special name is only for the use of 
// cxx2html.
// </synopsis>

template <class T>
class WrapperData_FV<T,T,Vector<T>,False,True> : public WrapperBase<T>
{
  typedef WrapperData_FV<T,T,Vector<T>,False,True> myData;

public:
  explicit WrapperData_FV(T(*f)(const Vector<T>&)) :
    WrapperBase<T>(0), pf_p(f) {}
  virtual ~WrapperData_FV() {}
  virtual T eval(typename Function<T>::FunctionArg,
		 const Vector<T> &par) const {
    if (pf_p) return pf_p(par);
    return T(0); }

protected:
  T (*pf_p)(const Vector<T>&);

private:
  WrapperData_FV(const myData &other);
  myData &operator=(const myData &other);

protected:
  //# Make members of parent classes known.
  using WrapperBase<T>::ndim_p;
  using WrapperBase<T>::arg_p;
};

#undef WrapperData_FV


#define WrapperData_TF WrapperData

// <summary> Specialization for calls with argument and no parameters
// </summary>
// <synopsis> Note that the actual name of the class is
// <src>WrapperData</src>. The special name is only for the use of 
// cxx2html.
// </synopsis>

template <class T>
class WrapperData_TF<T,T,T,True,False> : public WrapperBase<T>
{
  typedef WrapperData_TF<T,T,T,True,False> myData;

public:
  explicit WrapperData_TF(T(*f)(const T&), uInt dim=1) :
    WrapperBase<T>(dim), pf_p(f) {}
  virtual ~WrapperData_TF() {}
  virtual T eval(typename Function<T>::FunctionArg x, 
		 const Vector<T>&) const {
    if (pf_p) {
      return pf_p((*static_cast<const typename Function<T>::FunctionArg>(x)));
    }
    return T(0); }

protected:
  T (*pf_p)(const T&);

private:
  WrapperData_TF(const myData &other);
  myData &operator=(const myData &other);

protected:
  //# Make members of parent classes known.
  using WrapperBase<T>::ndim_p;
  using WrapperBase<T>::arg_p;
};

#undef WrapperData_TF


#define WrapperData_VF WrapperData

// <summary> Specialization for calls with argument and no parameters
// </summary>
// <synopsis> Note that the actual name of the class is
// <src>WrapperData</src>. The special name is only for the use of 
// cxx2html.
// </synopsis>

template <class T>
class WrapperData_VF<T,Vector<T>,T,True,False> : public WrapperBase<T>
{
  typedef WrapperData_VF<T,Vector<T>,T,True,False> myData;

public:
  explicit WrapperData_VF(T(*f)(const Vector<T>&), uInt dim=1) :
    WrapperBase<T>(dim), pf_p(f) {}
  virtual ~WrapperData_VF() {}
  virtual T eval(typename Function<T>::FunctionArg x,
		 const Vector<T> &) const {
    if (pf_p) {
      for (uInt i=0; i<ndim_p; ++i) arg_p[i] = x[i];
      return pf_p(arg_p); }
    return T(0); }

protected:
  T (*pf_p)(const Vector<T>&);

private:
  WrapperData_VF(const myData &other);
  myData &operator=(const myData &other);

protected:
  //# Make members of parent classes known.
  using WrapperBase<T>::ndim_p;
  using WrapperBase<T>::arg_p;
};

#undef WrapperData_VF


#define WrapperData_FF WrapperData

// <summary> Specialization for calls with no arguments and no parameters
// </summary>
// <synopsis> Note that the actual name of the class is
// <src>WrapperData</src>. The special name is only for the use of 
// cxx2html.
// </synopsis>

template <class T>
class WrapperData_FF<T,T,T,False,False> : public WrapperBase<T>
{
  typedef WrapperData_FF<T,T,T,True,False> myData;

public:
  explicit WrapperData_FF(T(*f)()) :
    WrapperBase<T>(0), pf_p(f) {}
  virtual ~WrapperData_FF() {}
  virtual T eval(typename Function<T>::FunctionArg,
		 const Vector<T>&) const {
    if (pf_p) return pf_p();
    return T(0); }

protected:
  T (*pf_p)();

private:
  WrapperData_FF(const myData &other);
  myData &operator=(const myData &other);

protected:
  //# Make members of parent classes known.
  using WrapperBase<T>::ndim_p;
  using WrapperBase<T>::arg_p;
};

#undef WrapperData_FF

} //# NAMESPACE CASACORE - END

#endif
