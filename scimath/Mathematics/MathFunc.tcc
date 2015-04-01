//# MathFunc.cc: Templated letter/envelope classes for single dependent variable functions
//# Copyright (C) 1993,1994,1995,1996,1998,1999
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

#ifndef SCIMATH_MATHFUNC_TCC
#define SCIMATH_MATHFUNC_TCC

#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/scimath/Mathematics/MathFunc.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//
// This file implements an abstract base class of MathFunc objects
//
// Actual math functions are an inherited class from the base
// class. This approach allows one to define actual function values
// for each derived class. Then, one can pass a generic MathFunc
// pointer to other objects, but the other objects will still get
// function values from the actual inherited function.
//
// By defining each math function as an object, we can place
// parameters which will not change from one call of the member function 
// value to another in the class definition and they only have to be
// initialized once.

//
//
// Define static members of MathFunc<T>
// (for g++ 3.3 and lower they are defined in MathFunc2.cc)
//
#if !defined(__GNUC__) || __GNUC__>3 || (__GNUC__==3 && __GNUC_MINOR__>3)
template<class T>
T MathFunc<T>::defcutoff_p = T(2.0);
template<class T>
T MathFunc<T>::defwidth_p = T(1.3);
template<class T>
T MathFunc<T>::defKBwidth_p = T(2.0);
template<class T>
T MathFunc<T>::defKBparm_p = T(2.5);
template<class T>
T MathFunc<T>::defmodKBparm_p = T(3.0);
template<class T>
T MathFunc<T>::defSphcutoff_p = T(3.0);
template<class T>
T MathFunc<T>::defSphparm_p = T(1.0);
template<class T>
T MathFunc<T>::defSincparm_p = T(1.14);
// note that these defaults are equivalent to a Gaussian (exponent = 2) 
// having the same width as defwidth above, FWHM = 1.3
template<class T>
T MathFunc<T>::defExpPower_p = T(2.0);
template<class T>
T MathFunc<T>::defExpScale_p = T(1.3/sqrt(4.0*C::ln2));
#endif

template<class T>
MathFunc<T>::MathFunc(FUNCTYPE type)
{
    switch(type) {
    case MOD_KB:
	object = new Mod_KB_Conv<T>();
	break;
    case GAUSSIAN:
	object = new GaussianConv<T>();
	break;
    case KB:
	object = new KB_Conv<T>();
	break;
    case SPHEROIDAL:
	object = new Sph_Conv<T>();
	break;
    case SINC:
	object = new Sinc_Conv<T>();
	break;
    case UNARY:
	object = new Unary<T>();
	break;
    case EXP_SINC:
	object = new ExpSincConv<T>();
	break;
    default:
	throw(MathFuncError(" MathFunc::MathFunc: Invalid enumerated"
			    " type as argument" ));
	break;
    }
}	

template<class T>
MathFunc<T>::MathFunc(String &type, Vector<double> &args)
{
    if (type.matches("MOD_KB"))
	object = new Mod_KB_Conv<T>((T)args(0), (T)args(1), (T)args(2), 
				    (T)args(3));
    else if (type.matches( "GAUSSIAN"))
	object = new GaussianConv<T>((T)args(0), (T)args(1));
    else if (type.matches( "KB"))
	object = new KB_Conv<T>((T)args(0), (T)args(1), (T)args(2));
    else if (type.matches( "SPHEROIDAL"))
	object = new Sph_Conv<T>((T)args(0), (T)args(1));
    else if (type.matches( "SINC"))
	object = new Sinc_Conv<T>((T)args(0), (T)args(1));
    else if (type.matches( "UNARY"))
	object = new Unary<T>((T)args(0));
    else if (type.matches( "EXP_SINC"))
	object = new ExpSincConv<T>((T)args(0), (T)args(1), (T)args(2),
				    (T)args(3));
    else 
	throw(MathFuncError(" MathFunc::MathFunc: Invalid String value"
			    " as argument" ));
}	

//
// this function is the MathFunc intitializer
//
template<class T>
MathFunc<T>::MathFunc(FUNCTYPE type, T cut, T arg1, T arg2, T arg3)
{
    T wparm;
    T kbparm;
    T gwparm;
    T sphparm;
    T sincparm;
    T exppower;
    T expscale;

    switch(type) {
    case MOD_KB:
	wparm = arg1;    
	kbparm = arg2;  
	gwparm = arg3; 
	object = new Mod_KB_Conv<T>(cut, wparm, kbparm, gwparm);
	break;
	
    case GAUSSIAN:
	wparm = arg1;
	object = new GaussianConv<T>(cut, wparm);
	break;
	
    case KB:
	wparm = arg1;
	kbparm = arg2;
	object = new KB_Conv<T>(cut, wparm, kbparm);
	break;
	
    case SPHEROIDAL:
	
	sphparm = arg1;
	object = new Sph_Conv<T>(cut, sphparm);
	break;
	
    case SINC:
	sincparm = arg1; 
	object = new Sinc_Conv<T>(cut, sincparm);
	break;
	
    case UNARY:
	object = new Unary<T>(cut);
	break;

    case EXP_SINC:
	sincparm = arg1;
	expscale = arg2;
	exppower = arg3;
	object = new ExpSincConv<T>(cut, sincparm, expscale, exppower);
	break;

    default:
	throw(MathFuncError("MathFunc::MathFunc: Invalid enumerated"
			    " type as argument." ));
	break;
    }
}

//
// copy constructor
//
template<class T>
MathFunc<T>::MathFunc(const MathFunc<T> &other):object(static_cast<MathFunc<T> *>(0))
{ 		
  if(other.object!=static_cast<MathFunc<T> *>(0)) 
    delete object;   // thus we can't allow references to letter classes.
  object = other.object->clone();
}

//
// backdoor new type constructor without enumerated type list addition
//
template<class T>
MathFunc<T>::MathFunc(MathFunc<T> *other) : object(static_cast<MathFunc<T> *>(0)) 
{ 
  if(other->object==static_cast<MathFunc<T> *>(0)){
    object = other;
  } else 
    *this = *other;
}

//
// destructor
//
template<class T>
MathFunc<T>::~MathFunc()
{ delete object;}

//
// assignment operator
//
template<class T>
MathFunc<T>& MathFunc<T>::operator=(const MathFunc<T>& other)
{
  if(this == &other) return *this;  
  if(other.object==static_cast<MathFunc<T> *>(0))
    throw (MathFuncError("MathFunc::operator=: attempt to use derived class"
			 " in a base class only function (polymorph flaw)."));
  delete object;
  object = other.object->clone();
  return *this;
}

//
// compute and return a value of the math function
//
template<class T>
T MathFunc<T>::value(const T &i) const
{ return object->value(i);}

//
// return the value of the supported width
//
template<class T>
T MathFunc<T>::sup_value() const
{ return object->sup_value();}

template<class T>
MathFunc<T>* MathFunc<T>::newMathFunc(const MathFunc<T>& prototype)
{ return new MathFunc<T>(prototype.clone());}

template<class T>
FuncId MathFunc<T>::id() const
{ return object->id();}


template<class T>
MathFunc<T> * MathFunc<T>::clone() const
{ return new MathFunc<T>(object->clone());}

//
// Note: The inheritance schema is purely to allow polymorphism.
// Thus, the data members associated with the MathFunc base class
// are not needed as data members of the derived class.  To save
// space, the default constructor of the MathFunc base class 
// initializes its data members to zero.
//
template<class T> 
MathFunc<T>::MathFunc(): object(static_cast<MathFunc<T> *>(0))
{ 
  // nothing
}


// ==========================letter classes=========================
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


// -------------------------Unary function--------------------

// 
// default constructor
//
template<class T>
Unary<T>::Unary(T cut): MathFunc<T>(), sup_width(cut)
{
  // nothing
}

//
// copy constructor
//
template<class T>
Unary<T>::Unary(const Unary<T>& other): MathFunc<T>(),
  sup_width(other.sup_width)
{
  // nothing
}

//
// assignment operator
//
template<class T>
Unary<T>& Unary<T>::operator=(const Unary<T>& other)
{
   if(this == &other) return *this;
   sup_width = other.sup_width;

   return *this;
}

//
// this function the value 1.0 for any location 'i'
//
template<class T>
T Unary<T>::value(const T &i) const
{ 
  (void)i;
  return T(1.0);
}

template<class T>
FuncId Unary<T>::id() const
{ 
  FuncId tmp;
  tmp.name = "UNARY";
  tmp.args.resize(1);
  tmp.args(0)=sup_width;
  return tmp;
}

template<class T>
MathFunc<T> * Unary<T>::clone() const
{ return new Unary<T>(*this);}  // use Unary copy ctor


// -------------------------Gaussian Convolution--------------------

// 
// default constructor
//
template<class T>
GaussianConv<T>::GaussianConv(T cut, T wparm): MathFunc<T>(), sup_width(cut),
fw2(wparm*wparm), ln16(4.0*C::ln2)
{
  // nothing
}

//
// copy constructor
//
template<class T>
GaussianConv<T>::GaussianConv(const GaussianConv<T>& other): MathFunc<T>(),
  sup_width(other.sup_width), fw2(other.fw2), ln16(4.0*C::ln2)
{
  // nothing
}

//
// assignment operator
//
template<class T>
GaussianConv<T>& GaussianConv<T>::operator=(const GaussianConv<T>& other)
{
   if(this == &other) return *this;
   sup_width = other.sup_width;
   fw2 = other.fw2;

   return *this;
}

//
// this function computes values for a convolution vector conv_ptr
// at location 'i'
//
template<class T>
T GaussianConv<T>::value(const T &i) const
{ return exp(-ln16 * i*i / fw2);}

template<class T>
FuncId GaussianConv<T>::id() const
{ 
  FuncId tmp;
  tmp.name = "GAUSSIAN";
  tmp.args.resize(2);
  tmp.args(0)=sup_width;
  tmp.args(1)=sqrt(fw2);
  return tmp;
}

template<class T>
MathFunc<T> * GaussianConv<T>::clone() const
{ return new GaussianConv<T>(*this);}  // use GaussianConv copy ctor


// ---------------------Kaiser-Bessel convolution------------------

//
// default constructor
//
template<class T>
KB_Conv<T>::KB_Conv(T cut, T wparm, T KBparm): MathFunc<T>(),
kbparm(KBparm), fw(wparm), sup_width(cut)
{
  // nothing
}

//
// copy constructor
//
template<class T>
KB_Conv<T>::KB_Conv(const KB_Conv<T>& other): MathFunc<T>(), 
 kbparm(other.kbparm), fw(other.fw), sup_width(other.sup_width)
{
  //nothing
}

//
// assignment operator
//
template<class T>
KB_Conv<T>& KB_Conv<T>::operator=(const KB_Conv<T>& other)
{
  if(this == &other) return *this;

  kbparm = other.kbparm;
  fw = other.fw;
  sup_width = other.sup_width;
  
  return *this;
}
   
//
// this function computes values for a convolution vector conv_ptr
// at location 'i'
//
template<class T>
T KB_Conv<T>::value(const T &i) const
{
  T par2 = kbparm * kbparm;
  T x1 = C::pi * kbparm;
  T x2 = C::pi * sqrt(par2 - 1.0);
  T x3 = C::pi * sqrt(par2 - 4.0);
  T a = sinh(x1);
  T b = sinh(x2) * 2.0;
  T c = sinh(x3) * 2.0;
  T sum = a + b + c;
  a /= sum;
  b /= sum;
  c /= sum;
  T x = i * C::pi / fw;
  return (a + b * cos(x) + c * cos(2.0 * x));
}

template<class T>
FuncId KB_Conv<T>::id() const
{ 
  FuncId tmp;
  tmp.name = "KB";
  tmp.args.resize(3);
  tmp.args(0)=sup_width;
  tmp.args(1)=fw;
  tmp.args(2)=kbparm;
  return tmp;
}

template<class T>
MathFunc<T> * KB_Conv<T>::clone() const
{ return new KB_Conv<T>(*this);}     // use KB_Conv copy constructor


//----------A Kaiser-Bessel function modified by a Gaussian-----

//
// default constructor
//
template<class T>
Mod_KB_Conv<T>::Mod_KB_Conv(T cut, T wparm, T KBparm, T gwparm): MathFunc<T>(),
kbparm(KBparm), gw2(gwparm*gwparm), sup_width(cut), widthparm(wparm), 
ln16(4.0*C::ln2)
{
  // nothing
}

//
// copy constructor
//
template<class T>
Mod_KB_Conv<T>::Mod_KB_Conv(const Mod_KB_Conv<T>& other): MathFunc<T>(),
kbparm(other.kbparm), gw2(other.gw2), sup_width(other.sup_width),
  widthparm(other.widthparm), ln16(4.0*C::ln2)
{
  // nothing
}

//
// Assignment operator
//
template<class T>
Mod_KB_Conv<T> &Mod_KB_Conv<T>::operator=(const Mod_KB_Conv<T>& other)
{
  if(this == &other) return *this;

   sup_width = other.sup_width;
   widthparm = other.widthparm;
   kbparm = other.kbparm;
   gw2 = other.gw2;

   return *this;
}

//
// this function computes values for a convolution vector conv_ptr
// at location 'i'
//
template<class T>
T Mod_KB_Conv<T>::value(const T &i) const
{
  T par2 = kbparm * kbparm;
  T x1 = C::pi * kbparm;
  T x2 = C::pi * sqrt(par2 - 1.0);
  T x3 = C::pi * sqrt(par2 - 4.0);

  T a = sinh(x1);
  T b = sinh(x2) * 2.0;
  T c = sinh(x3) * 2.0;

  T sum = a + b + c;

  a /= sum;
  b /= sum;
  c /= sum;

  T i2 = i * i;
  T x = i * C::pi / widthparm;
  T fx = a + b * cos(x) + c * cos(2.0 * x);
  return (fx * exp(-ln16 * (i2 / gw2)));
}

template<class T>
FuncId Mod_KB_Conv<T>::id() const
{ 
  FuncId tmp;
  tmp.name = "MOD_KB";
  tmp.args.resize(4);
  tmp.args(0)=sup_width;
  tmp.args(1)=widthparm;
  tmp.args(2)=kbparm;
  tmp.args(3)=gw2;
  return tmp;
}

template<class T>
MathFunc<T> * Mod_KB_Conv<T>::clone() const
{ return new Mod_KB_Conv<T>(*this);}  // use Mod_KB_Conv copy ctor

// ------------------------Sinc convolution------------------------

//
// default constructor
//
template<class T>
Sinc_Conv<T>::Sinc_Conv(T cut, T sincparm): MathFunc<T>(),
Sinc_parm(sincparm), sup_width(cut)
{
  // nothing
}

//
// copy constructor
//
template<class T>
Sinc_Conv<T>::Sinc_Conv(const Sinc_Conv<T>& other): MathFunc<T>(), 
Sinc_parm(other.Sinc_parm), sup_width(other.sup_width)
{
  // nothing
}

//
// assignment operator
//
template<class T>
Sinc_Conv<T>& Sinc_Conv<T>::operator=(const Sinc_Conv<T>& other)
{
  if(this == &other) return *this;

  sup_width = other.sup_width;
  Sinc_parm = other.Sinc_parm;

  return *this;
}

//
// this function computes values for a convolution vector conv_ptr
// at location 'i'
//
template<class T>
T Sinc_Conv<T>::value(const T &i) const
{
  T ret_value;
  if (i == 0.0) ret_value = 1.0;
  else {
    T parm = C::pi * i / Sinc_parm;
    ret_value = sin(parm) / parm;
  }
  return ret_value;
}

template<class T>
FuncId Sinc_Conv<T>::id() const
{ 
  FuncId tmp;
  tmp.name = "SINC";
  tmp.args.resize(2);
  tmp.args(0)=sup_width;
  tmp.args(1)=Sinc_parm;
  return tmp;
}

template<class T>
MathFunc<T> * Sinc_Conv<T>::clone() const
{ return new Sinc_Conv<T>(*this);}  //use Sinc_Conv copy ctor


// ------------------------Spheroidal Convolution--------------------

//
// default constructor
//
template<class T>
Sph_Conv<T>::Sph_Conv(T cut, T Sphparm): MathFunc<T>(), sup_width(cut), 
sphparm(Sphparm)
{
  //nothing
}

//
// copy constructor
//
template<class T>
Sph_Conv<T>::Sph_Conv(const Sph_Conv<T>& other): MathFunc<T>(), 
sup_width(other.sup_width), sphparm(other.sphparm)
{
  // nothing
}

//
// assignment operator
//
template<class T>
Sph_Conv<T>& Sph_Conv<T>::operator=(const Sph_Conv<T>& other)
{
  if(this == &other) return *this;

  sup_width = other.sup_width;
  sphparm = other.sphparm;

  return *this;
}

//
// this function computes values for a convolution vector conv_ptr
// at location 'i'
//
template<class T>
float Sph_Conv<T>::value(const float &j) const
{
  int isupp = int(2.0 * sup_width);
  if (isupp < 4) isupp = 4;
  if (isupp > 8) isupp = 8;

  int ialpha = int(2.0 * sphparm + 1.0);
  if (ialpha < 1) ialpha = 1;
  if (ialpha > 5) ialpha = 5;

  int jmax = int(sup_width);
  if (jmax > 7) jmax = 7;

  // call Fred Schwab function
  return sphfn(ialpha, isupp, j/jmax);	
}

template<class T>
FuncId Sph_Conv<T>::id() const
{ 
  FuncId tmp;
  tmp.name = "SPHEROIDAL";
  tmp.args.resize(2);
  tmp.args(0)=sup_width;
  tmp.args(1)=sphparm;
  return tmp;
}

template<class T>
MathFunc<T> * Sph_Conv<T>::clone() const
{ return new Sph_Conv<T>(*this);}  // use Sph_Conv copy ctor



// -------------------------Exponential*Sinc Convolution----------------

// 
// default constructor
//
template<class T>
ExpSincConv<T>::ExpSincConv(T cut, T sincparm, T expscale, T exppower): 
MathFunc<T>(), sup_width(cut), scale(expscale), exponent(exppower), 
sincpByPi(sincparm/C::pi)
{
    // nothing
}

//
// copy constructor
//
template<class T>
ExpSincConv<T>::ExpSincConv(const ExpSincConv<T>& other): 
MathFunc<T>(), sup_width(other.sup_width), scale(other.scale), 
exponent(other.exponent), sincpByPi(other.sincpByPi)
{
    // nothing
}

//
// assignment operator
//
template<class T>
ExpSincConv<T>& 
ExpSincConv<T>::operator=(const ExpSincConv<T>& other)
{
   if(this == &other) return *this;
   sup_width = other.sup_width;
   scale = other.scale;
   exponent = other.exponent;
   sincpByPi = other.sincpByPi;

   return *this;
}


//
// this function computes values for a convolution vector conv_ptr
// at location 'i'
//
template<class T>
T ExpSincConv<T>::value(const T &i) const
{ 
    T absI = fabs(i);
    T ret_value = exp(- pow(absI/scale,exponent));
    if (absI != 0.0) {
	T parm = i / sincpByPi;
	ret_value = ret_value * sin(parm) / parm;
    }
    return ret_value;
}

template<class T>
FuncId ExpSincConv<T>::id() const
{ 
  FuncId tmp;
  tmp.name = "EXP_SINC";
  tmp.args.resize(4);
  tmp.args(0)=sup_width;
  tmp.args(1)=sincpByPi * C::pi;
  tmp.args(2)=scale;
  tmp.args(3)=exponent;
  return tmp;
}

template<class T>
MathFunc<T> * ExpSincConv<T>::clone() const
{ return new ExpSincConv<T>(*this);}  // use ExpSincConv copy ctor


} //# NAMESPACE CASACORE - END


#endif
