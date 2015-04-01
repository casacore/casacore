//# MathFunc.h: Templated letter/envelope classes for single dependent variable functions
//# Copyright (C) 1993,1994,1995,1996,1999,2000,2001,2003
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

#ifndef SCIMATH_MATHFUNC_H
#define SCIMATH_MATHFUNC_H

//# MathFunc: A templated letter/envelope set of classes for packaging
//# of specific single dependent variable functions.


#include <casacore/casa/aips.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Error class for <linkto class=MathFunc>MathFunc</linkto> class
// </summary>

// <synopsis>
// Error class for <linkto class=MathFunc>MathFunc</linkto> class
// </synopsis>

class MathFuncError : public AipsError
{
public:
    MathFuncError() : AipsError("MathFuncError") {}
    MathFuncError(const Char *m)   : AipsError(m) {}
    MathFuncError(const String &m) : AipsError(m) {}

    virtual ~MathFuncError() throw() {}
};

// <summary>
// Fred Schwab function to calculate spheriodal functions
// </summary>

// <synopsis>
// Fred Schwab function to calculate spheriodal functions.
// </synopsis>

// <group name="spheriodal functions">

// Fred Schwab function to calculate spheriodal functions, in C.
extern "C" { 
   Int sphfn(Int *, Int *, Int *, float *, float *, Int *);
}

// C++ wrapper to Fred Schwab function to calculate spheriodal functions.
float sphfn(Int ialf, Int im, float eta);

// </group>


// <summary>
// Enum used to identify function type for
// MathFunc class
// </summary>

// <synopsis>
// Enum used to identify function type for
// <linkto class=MathFunc>MathFunc</linkto> class
// </synopsis>

//############################################################################
//# NOTE:  Delete the following listing of the enumerations when enumerations
//#        are handled properly by the documentation extractor.
//############################################################################

// <note role=caution>
// The following enum documentation is currently
// extracted by hand, and thus
// could be out of date if this documentation was not updated when the
// enum was modified.
// </note>

// The FUNCTYPE enum is:
// <srcblock>
//
// enum FUNCTYPE { UNARY, GAUSSIAN, KB, MOD_KB, SINC, SPHEROIDAL, EXP_SINC };
//
// </srcblock>

// <group name="FUNCTYPE enum">

enum FUNCTYPE { UNARY, GAUSSIAN, KB, MOD_KB, SINC, SPHEROIDAL, EXP_SINC };

// </group>


// <summary>
// Function ID, for use by <linkto class=MathFunc>MathFunc</linkto> class
// </summary>

// <synopsis>
// Function ID, for use by <linkto class=MathFunc>MathFunc</linkto> class.
// </synopsis>

struct FuncId { String name; Vector<double> args;};


// <summary> A class to generate values of mathematical functions </summary>
//
// <synopsis>
//
// This class is the abstract base class for 1-dimensional math functions.
//
// Actual math functions are then an inherited class from the base
// class. This approach allows one to define actual function values
// for each derived class. Then, one can pass a generic MathFunc
// pointer to other objects, but the other objects will still get
// function values from the actual inherited function.	
//
// By defining each math function as an object, we can place
// parameters which will not change from one call to the function value
// to another in the class definition and they only have to be
// initialized once.
//
// </synopsis>

//
// MathFunc is the base class for 1-dimensional math functions
//
template<class T>
class MathFunc {
public:
//
// constructors
//
   MathFunc(FUNCTYPE);  
    // accept up to 4 arguments, the first being the support radius
   MathFunc(FUNCTYPE, T cutoff, T arg1 = 1.0e+30, T arg2 = 1.0e+30, 
	    T arg3 = 1.0e+30);  
   MathFunc(String &, Vector<double> &);
   MathFunc(const MathFunc<T>&);        // Copy constructor
   MathFunc(MathFunc<T> *);               

//
// Destructor
//
   virtual ~MathFunc(); 

//
// Assignment operator - Note: this function works only for envelops.
// Polymorphism flaws will let you pass a letter as an argument but an
// exception will be thrown at run time.
//
   MathFunc<T>& operator=(const MathFunc<T>&);

//
// return value of support width
//  
   virtual T sup_value() const;

//
// compute and return a value of the math function
//
   virtual T value(const T &a) const; 

//
// create a new math function
//
   static MathFunc<T> *newMathFunc(const MathFunc<T>&);

//
// return a FuncId structure for Table storage/retrieval.
//
  virtual FuncId id() const;

//
// These functions return the static constants used as default
// parameters for the various derived functions
//

// The default support radius
    static T defcutoff() {return defcutoff_p;}
// The default width for Gaussian_Conv
    static T defwidth() {return defwidth_p;}
// The default width for KB_Conv and Mod_KB_Conv
    static T defKBwidth() {return defKBwidth_p;}
// A default parameter for KB_Conv and Mod_KB_Conv
    static T defKBparm() {return defKBparm_p;}
// A default parameter for Mod_KB_Conv
    static T defmodKBparm() {return defmodKBparm_p;}
// The default support radius for Sinc_Conv and Sph_Conv
    static T defSphcutoff() {return defSphcutoff_p;}
// The default Sinc parameter for Sinc_Conv and Exp_Sinc_Conv
    static T defSincparm() {return defSincparm_p;}
// The default parameter for Sph_Conv
    static T defSphparm() {return defSphparm_p;}
// The default exponential power for Exp_Sinc_Conv
    static T defExpPower() {return defExpPower_p;}
// The default exponential scale length for Exp_Sinc_Conv
    static T defExpScale() {return defExpScale_p;}

protected:
//
// for every derived class, return new of that class with its own parameters
//
   virtual MathFunc<T> * clone() const;

//
// Default constructor (Null)
//
   MathFunc();       

//
// pointer to letter class
//
   MathFunc<T> *object;

private:
   static T defcutoff_p;
   static T defwidth_p;
   static T defKBparm_p;
   static T defKBwidth_p;
   static T defmodKBparm_p;
   static T defSphcutoff_p;
   static T defSphparm_p;
   static T defSincparm_p;
   static T defExpPower_p;
   static T defExpScale_p;
};

//# ========================================================
//# Now we define actual math classes as inherited classes of
//# the base class MathFunc
//# =========================================================

//
// <summary>Unary</summary>
//
// <synopsis>
// A Unary function (always returns the value 1.0)
// </synopsis>
//
template<class T>
class Unary: public MathFunc<T>
{
public:
  //
  //default constructor
  //
  Unary( T cut = MathFunc<T>::defcutoff());

  //
  //copy constructor
  //
  Unary(const Unary<T>&);

  Unary<T>& operator=(const Unary<T>&);

  T sup_value() const{ return sup_width;}

  T value(const T &) const;

//
// return a FuncId structure for Table storage/retrieval.
//
  FuncId id() const;

private:
  MathFunc<T> * clone() const;

  T sup_width;
};

//
// <category lib=aips sect="Math">
// <summary>Gaussian</summary>
//
// <synopsis>
// A Gaussian
// </synopsis>
//
template<class T>
class GaussianConv: public MathFunc<T>
{
public:
  //
  //default constructor
  //
  GaussianConv(   T cut = MathFunc<T>::defcutoff(), 
	       T wparm = MathFunc<T>::defwidth()); 

  //
  //copy constructor
  //
  GaussianConv(const GaussianConv<T>&);

  GaussianConv<T>& operator=(const GaussianConv<T>&);

  T sup_value() const{ return sup_width;}

  T value(const T &) const;

//
// return a FuncId structure for Table storage/retrieval.
//
  FuncId id() const;

private:
  MathFunc<T> * clone() const;

  T sup_width, fw2; 
  const T ln16;
};

//
// <category lib=aips sect="Math">
// <summary>A Kaiser-Bessel function</summary>
//
// <synopsis>
// A Kaiser-Bessel function
// </synopsis>
//
template<class T>
class KB_Conv: public MathFunc<T>
{
public:

   //default constructor
   KB_Conv(T cut = MathFunc<T>::defcutoff(), 
	   T wparm = MathFunc<T>::defKBwidth(), 
	   T kbparm = MathFunc<T>::defKBparm());

   // copy constructor
   KB_Conv(const KB_Conv<T> &);

   KB_Conv<T>& operator=(const KB_Conv<T>&);

   T sup_value() const { return sup_width;}

   T value(const T &) const;

//
// return a FuncId structure for Table storage/retrieval.
//
  FuncId id() const;

private:
   MathFunc<T> * clone() const;

   T kbparm, fw, sup_width;
};

//
// <category lib=aips sect="Math">
// <summary>A Kaiser-Bessel function multiplied by a Gaussian</summary>
//
// <synopsis>
// A Kaiser-Bessel function multiplied by a Gaussian
// </synopsis>
//

template<class T>
class Mod_KB_Conv: public MathFunc<T>
{
public:
   //default constructor
   Mod_KB_Conv   (T cut    = MathFunc<T>::defcutoff(), 
                  T wparm  = MathFunc<T>::defKBwidth(), 
                  T kbparm = MathFunc<T>::defKBparm(), 
                  T gwparm = MathFunc<T>::defmodKBparm());

   //copy constructor
   Mod_KB_Conv(const Mod_KB_Conv<T>&);

   Mod_KB_Conv<T>& operator=(const Mod_KB_Conv<T>&);

   T sup_value() const { return sup_width;}

   T value(const T &) const;

//
// return a FuncId structure for Table storage/retrieval.
//
  FuncId id() const;

private:
   MathFunc<T>* clone() const;

   T kbparm, gw2, sup_width, widthparm;
   const T ln16;
};

// <category lib=aips sect="Math">
// <summary>Sine x / x function</summary>
//
//
// <synopsis>
// Sine x / x function
// </synopsis>
//
template<class T>
class Sinc_Conv: public MathFunc<T>
{
public:
   //default constructor
   Sinc_Conv(T cut = MathFunc<T>::defSphcutoff(),
             T sincparm = MathFunc<T>::defSincparm()); 
   //copy constructor
   Sinc_Conv(const Sinc_Conv<T>&);

   Sinc_Conv<T>& operator=(const Sinc_Conv<T>&);

   T sup_value() const { return sup_width;}

   T value(const T &) const;

//
// return a FuncId structure for Table storage/retrieval.
//
  FuncId id() const;

private:
   MathFunc<T> * clone() const;

   T Sinc_parm, sup_width;
};

//
// <category lib=aips sect="Math">
// <summary>Spheroidal function</summary>
//
// <synopsis>
// Spheroidal function - calls Fred Schwab function converted by f2c
// </synopsis>
//
template<class T>
class Sph_Conv: public MathFunc<T>
//
// Spheroidal function - calls Fred Schwab function converted by f2c
//
{
public:
   //default constructor
   Sph_Conv(T cut = MathFunc<T>::defSphcutoff(),
            T Sphparm = MathFunc<T>::defSphparm()); 
   //copy constructor
   Sph_Conv(const Sph_Conv<T>&);

   Sph_Conv<T>& operator=(const Sph_Conv<T>&);

   T sup_value() const { return sup_width;}

   float value(const float &) const;

//
// return a FuncId structure for Table storage/retrieval.
//
  FuncId id() const;

private:
   MathFunc<T> * clone() const;

   T sup_width, sphparm;
};

// <category lib=aips sect="Math">
// <summary>Exponential times a Sinc</summary>
//
// <synopsis>
// An Exponential times a Sinc
//
// The <src> value(T &x) </src> is given by
// <src> Exp(-(abs(x) / expscale) ** exppow)  * Sinc( pi * x / sincparm) </src>
//
// where the 3 paramaters correspond to those in the default constructor
// Note that the default case of <src> exppow = 2 </src> is
// a Gaussian times a Sinc.
// Since this is often a useful case, that parameter appears last in the
// constructor.
// </synopsis>
//
template<class T>
class ExpSincConv: public MathFunc<T>
{
public:
    //
    // default constructor
    //
    ExpSincConv ( T cut = MathFunc<T>::defcutoff(),
		      T sincparm = MathFunc<T>::defSincparm(),
		      T exppow = MathFunc<T>::defExpPower(),
		      T expscale = MathFunc<T>::defExpScale());
    // copy constructor
    ExpSincConv (const ExpSincConv<T>&);

    // assignment operator
    ExpSincConv<T>& operator=(const ExpSincConv<T>&);

    // get access to the support width
    T sup_value() const { return sup_width; }

    // and get the value of the function
    T value(const T &) const;

    //
    // return FuncID structure for Table storage/retrieval.
    //
    FuncId id() const;
private:
    MathFunc<T> * clone() const;

    T sup_width, scale, exponent, sincpByPi;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Mathematics/MathFunc.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif //AIPS_MATHFUNC_H
