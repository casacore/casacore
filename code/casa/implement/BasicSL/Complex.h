//# Complex.h: Single and double precision complex numbers
//# Copyright (C) 2000,2001
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


#if !defined(AIPS_COMPLEX_H)
#define AIPS_COMPLEX_H


#if !defined(AIPS_USE_OLD_COMPLEX)

//# Includes
#include <aips/aips.h>
#include <aips/Mathematics/Complexfwd.h>
#include <complex>

// <summary>
// Single precision complex numbers
// </summary>

// <synopsis>
// The class <src>Complex</src> is a straight typedef as the 
// standard library <src>complex<float></src>.
// A global functions are added for historic reasons (they were present
// in the original complex implementation).
//
// In a similar way <src>DComplex</src> is typedef-ed as
// <src>complex<double></src>.
//
// <linkto class=IComplex>IComplex</linkto> is defined as a specific class.
// It is only used by <src>FITS</src> classes.
// <src>lDComplex</src> has not been defined: <src>long double</src> is not
// part of the standard aips++ data suite.
//
// See the standard library documentation for the expected behaviour of 
// the <src>Complex</src> and <src>DComplex</src> classes.
//
// Complex numbers may be constructed and used in the following ways:
// <dl>
// <dt>Complex x;</dt>
// <dd>  Declares an uninitialized Complex. </dd>
// 
// <dt>Complex x = 2; Complex y(2.0);</dt>
// <dd>  Set x and y to the Complex value (2.0, 0.0); </dd>
// 
// <dt>Complex x(2, 3);</dt>
// <dd>  Sets x to the Complex value (2, 3); </dd>
// 
// <dt>Complex u(x); Complex v = x;</dt>
// <dd>  Set u and v to the same value as x. </dd>
// 
// <dt>double real(Complex& x);</dt>
// <dd>  returns the real part of x. </dd>
// 
// <dt>double imag(Complex& x);</dt>
// <dd>  returns the imaginary part of x. </dd>
// 
// <dt>double abs(Complex& x);</dt>
// <dd>  returns the magnitude of x. </dd>
// 
// <dt>double norm(Complex& x);</dt>
// <dd>  returns the square of the magnitude of x. </dd>
// 
// <dt>double arg(Complex& x);</dt>
// <dd>  returns the argument (amplitude) of x. </dd>
// 
// <dt>Complex polar(double r, double t = 0.0);</dt>
// <dd>  returns a Complex with abs of r and arg of t. </dd>
// 
// <dt>Complex conj(Complex& x);</dt>
// <dd>  returns the complex conjugate o </dd>f x.
// <dt>Complex cos(Complex& x);</dt>
// <dd>  returns the complex cosine of x. </dd>
// 
// <dt>Complex sin(Complex& x);</dt>
// <dd>  returns the complex sine of x. </dd>
// 
// <dt>Complex cosh(Complex& x);</dt>
// <dd>  returns the complex hyperbolic cosine of x. </dd>
// 
// <dt>Complex sinh(Complex& x);</dt>
// <dd>  returns the complex hyperbolic sine of x. </dd>
// 
// <dt>Complex exp(Complex& x);</dt>
// <dd>  returns the exponential of x. </dd>
// 
// <dt>Complex log(Complex& x);</dt>
// <dd>  returns the natural log of x. </dd>
// 
// <dt>Complex pow(Complex& x, long p);</dt>
// <dd>  returns x raised to the p power. </dd>
// 
// <dt>Complex pow(Complex& x, Complex& p);</dt>
// <dd>  returns x raised to the p power. </dd>
// 
// <dt>Complex sqrt(Complex& x);</dt>
// <dd>  returns the square root of x. </dd>
// 
// <dt> Complex min(Complex x,Complex y);
// <dd> Returns the minumum of x,y (using operator<=, i.e. the norm).
//
// <dt> Complex max(Complex x,Complex y);
// <dd> Returns the maximum of x,y (using operator>=, i.e. the norm).
//
// <dt>Bool near(Complex val1, Complex val2, Double tol = 1.0e-5);</dt>
// <dd>  returns whether val1 is relatively near val2 (see Math.h). </dd>
//
// <dt>Bool nearAbs(Complex val1, Complex val2, Double tol = 1.0e-5);</dt>
// <dd>  returns whether val1 is absolutely near val2 (see Math.h). </dd>
//
// <dt>ostream << x;</dt>
// <dd>  prints x in the form (re, im). </dd>
// 
// <dt>istream >> x;</dt>
//  <dd> reads x in the form (re, im), or just (re) or re in which case the
//      imaginary part is set to zero. </dd>
// </dl> 
// </synopsis>

//# <todo asof="2000/11/27">
//# </todo>

// <group name="Complex NaN">
Bool isNaN (const Complex& val);
void setNaN(Complex& val);
// </group>

// <summary>Complex comparisons </summary>
// <group name="Complex comparisons">
//# On Linux comparing the norm does not work well in debug mode
//# for equal values. Therefore they are compared for equality first.
inline Bool operator>= (const Complex& left, const Complex& right)
  { return left==right  ?  True : norm(left) >= norm(right); }
inline Bool operator>  (const Complex& left, const Complex& right)
  { return left==right  ?  False : norm(left) > norm(right); }
inline Bool operator<= (const Complex& left, const Complex& right)
  { return left==right  ?  True : norm(left) <= norm(right); }
inline Bool operator<  (const Complex& left, const Complex& right)
  { return left==right  ?  False : norm(left) < norm(right); }
// </group>
// </group>


// <summary>
// Double precision complex numbers
// </summary>

// <synopsis>
// The class <src>DComplex</src> is a straight typedef as the 
// standard library <src>complex<double></src>.
// It is defined in a similar way as <src>Complex</src>.
// </synopsis>

// <group name="DComplex NaN">
Bool isNaN (const DComplex& val);
void setNaN(DComplex& val);
// </group>

// <summary> DComplex comparisons </summary>
// <group name="DComplex comparisons">
inline Bool operator>= (const DComplex& left, const DComplex& right)
  { return norm(left) >= norm(right); }
inline Bool operator>  (const DComplex& left, const DComplex& right)
  { return norm(left) >  norm(right); }
inline Bool operator<= (const DComplex& left, const DComplex& right)
  { return norm(left) <= norm(right); }
inline Bool operator<  (const DComplex& left, const DComplex& right)
  { return norm(left) <  norm(right); }
// </group>
// </group>


//# Global functions
// <summary> Additional complex mathematical functions </summary>
// <group name=math>
inline Double fabs(const DComplex &val) { return abs(val); };
inline Float fabs(const Complex &val) { return abs(val); };
// The log10 should be in stl
// <group>
#if !defined(AIPS_USE_NEW_SGI) 
DComplex log10(const DComplex &val);
Complex log10(const Complex &val);
#endif
// </group>
// ArrayMath::pow needs this pow function (on SGI).
inline Complex pow(const Complex& val, Double p) { return pow(val,Float(p)); }
// QMath needs these operators * and / (on SGI).
// <group>
inline Complex operator*(const Complex& val, Double f) { return val*Float(f); }
inline Complex operator/(const Complex& val, Double f) { return val/Float(f); }
// </group>
// These operators are useful, otherwise Float and Double are applicable
// for Ints.
// <group>
inline Complex operator*(const Complex& val, Int f) { return val*Float(f); }
inline Complex operator/(const Complex& val, Int f) { return val/Float(f); }
// </group>
// </group>

// <summary> The near functions </summary>
// <group name=near>
Bool near(const Complex &val1, const Complex &val2, Double tol=1.0e-5);
Bool near(const DComplex &val1, const DComplex &val2, Double tol=1.0e-13);
Bool nearAbs(const Complex &val1, const Complex &val2, Double tol=1.0e-5);
Bool nearAbs(const DComplex &val1, const DComplex &val2, Double tol=1.0e-13);
inline Bool allNear(const Complex &val1, const Complex &val2,
		    Double tol=1.0e-5)
  { return near(val1, val2, tol); }
inline Bool allNear(const DComplex &val1, const DComplex &val2, 
		    Double tol=1.0e-13)
  { return near(val1, val2, tol); }
inline Bool allNearAbs(const Complex &val1, const Complex &val2, 
		       Double tol=1.0e-5)
  { return nearAbs(val1, val2, tol); }
inline Bool allNearAbs(const DComplex &val1, const DComplex &val2, 
		       Double tol=1.0e-13)
  { return nearAbs(val1, val2, tol); }
// </group>

// <summary> Max and min functions </summary>
// <group name=maxmin>
inline Complex max(const Complex &x, const Complex &y)
  { return x >= y ? x : y; }
inline DComplex max(const DComplex &x, const DComplex &y)
  { return x >= y ? x : y; }

inline Complex min(const Complex &x, const Complex &y)
  { return x <= y ? x : y; }
inline DComplex min(const DComplex &x, const DComplex &y)
  { return x <= y ? x : y; }
// </group>


#else

#include <aips/aips.h>
#include <aips/Utilities/generic.h>
#include <aips/iosfwd.h>
#include <aips/Mathematics/Math.h>
#include <aips/stdlib.h>



#if defined(G_COMPLEX)
#undef G_COMPLEX
#endif

#define G_COMPLEX(type)		g_name2(type,G_COMPLEX)

#define G_COMPLEXdeclare2(type,CASTS)						\
class G_COMPLEX(type)								\
{										\
public:									\
										\
  type           re;								\
  type           im;								\
										\
										\
										\
  type           real() const { return re; }					\
  type           imag() const { return im; }					\
										\
  type           &real() { return re; }						\
  type           &imag() { return im; }						\
										\
                 G_COMPLEX(type)() : re((type) 0), im((type) 0) {}		\
                 G_COMPLEX(type)(const G_COMPLEX(type)& y) :re(y.re), im(y.im) {}\
                 G_COMPLEX(type)(type r, type i= (type) 0) :re(r), im(i) {}	\
										\
                 CASTS								\
										\
                 ~G_COMPLEX(type)() {}						\
										\
  G_COMPLEX(type)& operator =  (G_COMPLEX(type) y) {re = y.re; im = y.im; return *this;} \
										\
										\
  G_COMPLEX(type)& operator += (const G_COMPLEX(type) y) {re += y.re;  im += y.im; return *this;}\
  G_COMPLEX(type)& operator += (type y) { re += y; return *this; }		\
										\
  G_COMPLEX(type)& operator -= (const G_COMPLEX(type) y) { re -= y.re;  im -= y.im; return *this;}\
  G_COMPLEX(type)& operator -= (type y) { re -= y; return *this;}		\
										\
  G_COMPLEX(type)& operator *= (const G_COMPLEX(type) y) {			\
			type r = re * y.re - im * y.im;			\
			im = re * y.im + im * y.re; 			\
			re = r; 						\
			return *this; 						\
		      }								\
  G_COMPLEX(type)& operator *= (type y) { re *=  y; im *=  y; return *this;}	\
										\
  G_COMPLEX(type)& operator /= (const G_COMPLEX(type) y); 			\
  G_COMPLEX(type)& operator /= (type y); 					\
										\
  void           error(const char* msg) const;					\
};										\
										\
/*										\
 * non-inline functions								\
 */										\
G_COMPLEX(type) operator /  (const G_COMPLEX(type) x, const G_COMPLEX(type) y);\
G_COMPLEX(type) operator /  (const G_COMPLEX(type) x, type y);			\
G_COMPLEX(type) operator /  (type x, const G_COMPLEX(type) y);			\
										\
G_COMPLEX(type) cos(const G_COMPLEX(type) x);				\
G_COMPLEX(type) sin(const G_COMPLEX(type) x);				\
										\
G_COMPLEX(type) cosh(const G_COMPLEX(type) x);				\
G_COMPLEX(type) sinh(const G_COMPLEX(type) x);				\
										\
G_COMPLEX(type) exp(const G_COMPLEX(type) x);				\
G_COMPLEX(type) log(const G_COMPLEX(type) x);				\
										\
G_COMPLEX(type) sqrt(const G_COMPLEX(type) x);				\
										\
G_COMPLEX(type) log10(const G_COMPLEX(type) x);					\
										\
G_COMPLEX(type) pow(const G_COMPLEX(type) x, int p);				\
G_COMPLEX(type) pow(const G_COMPLEX(type) x, const G_COMPLEX(type) p);	\
G_COMPLEX(type) pow(const G_COMPLEX(type) x, double y);			\
   										\
istream&  operator >> (istream& s, G_COMPLEX(type)& x);				\
ostream&  operator << (ostream& s, const G_COMPLEX(type) x);			\
										\
										\
/*										\
 * ACCESSORS									\
 */										\
inline type real(const G_COMPLEX(type) x)					\
{										\
  return x.re;								\
}										\
										\
inline type imag(const G_COMPLEX(type) x)					\
{										\
  return x.im;								\
}										\
										\
/*										\
 * math.h FUNCTIONS								\
 */										\
inline double abs(const G_COMPLEX(type) x)					\
{										\
  return hypot((double)x.re, (double)x.im);				\
}										\
										\
inline double fabs(const G_COMPLEX(type) x)					\
{										\
  return abs(x);								\
}										\
										\
inline double norm(const G_COMPLEX(type) x)					\
{										\
  return (x.re * x.re + x.im * x.im);				\
}										\
										\
inline double arg(const G_COMPLEX(type) x)					\
{										\
  return atan2((double)x.im, (double)x.re);				\
}										\
										\
G_COMPLEX(type) cube(const G_COMPLEX(type) val);				\
										\
inline G_COMPLEX(type) square(const G_COMPLEX(type) val)			\
{										\
    type r = val.re;							\
    type i = val.im;							\
    return G_COMPLEX(type)( r * r - i * i, r * i * 2 );				\
}										\
										\
/*										\
 * EQUALITY OPERATORS								\
 */										\
inline int  operator == (const G_COMPLEX(type) x, const G_COMPLEX(type) y)	\
{										\
  return x.re == y.re && x.im == y.im;				\
}										\
										\
inline int  operator == (const G_COMPLEX(type) x, type y)			\
{										\
  return x.im == 0.0 && x.re == y;					\
}										\
										\
inline int  operator != (const G_COMPLEX(type) x, const G_COMPLEX(type) y)	\
{										\
  return x.re != y.re || x.im != y.im;				\
}										\
										\
inline int  operator != (const G_COMPLEX(type) x, type y)			\
{										\
  return x.im != 0.0 || x.re != y;					\
}										\
										\
/*										\
 * COMPARISON OPERATORS (using norm)						\
 */										\
inline int operator >= (const G_COMPLEX(type) x, const G_COMPLEX(type) y) {	\
  return norm(x) >= norm(y);							\
}										\
										\
inline int operator >= (const G_COMPLEX(type) x, const type y) {		\
  return norm(x) >= y*y;							\
}										\
										\
inline int operator >= (const type x, const G_COMPLEX(type) y) {		\
  return x*x >= norm(y);							\
}										\
										\
inline int operator > (const G_COMPLEX(type) x, const G_COMPLEX(type) y) {	\
  return norm(x) > norm(y);							\
}										\
										\
inline int operator > (const G_COMPLEX(type) x, const type y) {			\
  return norm(x) > y*y;								\
}										\
										\
inline int operator > (const type x, const G_COMPLEX(type) y) {			\
  return x*x > norm(y);								\
}										\
										\
inline int operator <= (const G_COMPLEX(type) x, const G_COMPLEX(type) y) {	\
  return norm(x) <= norm(y);							\
}										\
										\
inline int operator <= (const G_COMPLEX(type) x, const type y) {		\
  return norm(x) <= y*y;							\
}										\
										\
inline int operator <= (const type x, const G_COMPLEX(type) y) {		\
  return x*x <= norm(y);							\
}										\
										\
inline int operator < (const G_COMPLEX(type) x, const G_COMPLEX(type) y) {	\
  return norm(x) < norm(y);							\
}										\
										\
inline int operator < (const G_COMPLEX(type) x, const type y) {			\
  return norm(x) < y*y;								\
}										\
										\
inline int operator < (const type x, const G_COMPLEX(type) y) {			\
  return x*x < norm(y);								\
}										\
inline G_COMPLEX(type) min (const G_COMPLEX(type) x, const G_COMPLEX(type) y)   \
{										\
  return x <= y ? x : y;		                                        \
}										\
										\
inline G_COMPLEX(type) max (const G_COMPLEX(type) x, const G_COMPLEX(type) y)   \
{										\
  return x >= y ? x : y;		                                        \
}										\
										\
										\
/*										\
 * NEGATION OPERATORS								\
 */										\
inline G_COMPLEX(type) operator - (const G_COMPLEX(type) x)			\
{										\
  return G_COMPLEX(type)(-x.re, -x.im);					\
}										\
										\
inline G_COMPLEX(type) conj(const G_COMPLEX(type) x)				\
{										\
  return G_COMPLEX(type)(x.re, -x.im);					\
}										\
										\
/*										\
 * SIMPLE NUMERIC OPERATIONS							\
 */										\
inline G_COMPLEX(type) operator + (const G_COMPLEX(type) x, const G_COMPLEX(type) y)\
{										\
  return G_COMPLEX(type)(x.re + y.re, x.im + y.im);		\
}										\
										\
inline G_COMPLEX(type) operator + (const G_COMPLEX(type) x, type y)		\
{										\
  return G_COMPLEX(type)(x.re + y, x.im);				\
}										\
										\
inline G_COMPLEX(type) operator + (type x, const G_COMPLEX(type) y)		\
{										\
  return G_COMPLEX(type)(x + y.re, y.im);				\
}										\
										\
inline G_COMPLEX(type) operator - (const G_COMPLEX(type) x, const G_COMPLEX(type) y)\
{										\
  return G_COMPLEX(type)(x.re - y.re, x.im - y.im);		\
}										\
										\
inline G_COMPLEX(type) operator - (const G_COMPLEX(type) x, type y)		\
{										\
  return G_COMPLEX(type)(x.re - y, x.im);				\
}										\
										\
inline G_COMPLEX(type) operator - (type x, const G_COMPLEX(type) y)		\
{										\
  return G_COMPLEX(type)(x - y.re, -y.im);				\
}										\
										\
inline G_COMPLEX(type) operator * (const G_COMPLEX(type) x, const G_COMPLEX(type) y)\
{										\
  return G_COMPLEX(type)(x.re * y.re - x.im * y.im, 		\
                 x.re * y.im + x.im * y.re);			\
}										\
										\
inline G_COMPLEX(type) operator * (const G_COMPLEX(type) x, type y)		\
{										\
  return G_COMPLEX(type)(x.re * y, x.im * y);				\
}										\
										\
inline G_COMPLEX(type) operator * (type x, const G_COMPLEX(type) y)		\
{										\
  return G_COMPLEX(type)(x * y.re, x * y.im);				\
}

// </group>

/*
 * --   --   --   --   --   -- Implementation --   --   --   --   --   --
 */


#define G_COMPLEXimplement2(type,CTOR)						\
										\
CTOR										\
										\
void  G_COMPLEX(type)::error(const char* msg) const				\
{										\
  (*Complex_error_handler)(msg);						\
}										\
										\
/* from romine@xagsun.epm.ornl.gov */						\
G_COMPLEX(type) /* const */ operator / (const G_COMPLEX(type) x, const G_COMPLEX(type) y)\
{										\
  double den = fabs((double)y.re) + fabs((double)y.im);			\
  if (den == 0.0) x.error ("Attempted division by zero.");			\
  double xrden = x.re / den;						\
  double xiden = x.im / den;						\
  double yrden = y.re / den;						\
  double yiden = y.im / den;						\
  double nrm   = yrden * yrden + yiden * yiden;					\
  return G_COMPLEX(type)((type)((xrden * yrden + xiden * yiden) / nrm),		\
                 (type)((xiden * yrden - xrden * yiden) / nrm));		\
}										\
										\
G_COMPLEX(type)& G_COMPLEX(type)::operator /= (const G_COMPLEX(type) y)		\
{										\
  double den = fabs((double)y.re) + fabs((double)y.im);			\
  if (den == 0.0) error ("Attempted division by zero.");			\
  double xrden = re / den;							\
  double xiden = im / den;							\
  double yrden = y.re / den;						\
  double yiden = y.im / den;						\
  double nrm   = yrden * yrden + yiden * yiden;					\
  re = (type)((xrden * yrden + xiden * yiden) / nrm);				\
  im = (type)((xiden * yrden - xrden * yiden) / nrm);				\
  return *this;									\
}										\
										\
G_COMPLEX(type) /* const */ operator / (type x, const G_COMPLEX(type) y)	\
{										\
  double den = norm(y);								\
  if (den == 0.0) y.error ("Attempted division by zero.");			\
  return G_COMPLEX(type)((type)((x * y.re) / den), -(type)((x * y.im) / den));\
}										\
										\
G_COMPLEX(type) /* const */ operator / (const G_COMPLEX(type) x, type y)	\
{										\
  if (y == 0.0) x.error ("Attempted division by zero.");			\
  return G_COMPLEX(type)((type)(x.re / y), (type)(x.im / y));		\
}										\
										\
										\
G_COMPLEX(type)& G_COMPLEX(type)::operator /= (type y)				\
{										\
  if (y == 0.0) error ("Attempted division by zero.");				\
  re /= y;  im /= y;								\
  return *this;									\
}										\
										\
										\
G_COMPLEX(type) /* const */ exp(const G_COMPLEX(type) x)			\
{										\
  double r = exp((double)x.re);						\
  return G_COMPLEX(type)((type)(r * cos((double)x.im)), 			\
                 (type)(r * sin((double)x.im)));				\
}										\
										\
G_COMPLEX(type) /* const */ cosh(const G_COMPLEX(type) x)			\
{										\
  return G_COMPLEX(type)((type)(cos((double)x.im) * cosh((double)x.re)),\
                 (type)(sin((double)x.im) * sinh((double)x.re)));	\
}										\
										\
G_COMPLEX(type) /* const */ sinh(const G_COMPLEX(type) x)			\
{										\
  return G_COMPLEX(type)((type)(cos((double)x.im) * sinh((double)x.re)),\
                 (type)(sin((double)x.im) * cosh((double)x.re)));	\
}										\
										\
G_COMPLEX(type) /* const */ cos(const G_COMPLEX(type) x)			\
{										\
  return G_COMPLEX(type)((type)(cos((double)x.re) * cosh((double)x.im)),\
                 (type)(-sin((double)x.re) * sinh((double)x.im)));	\
}										\
										\
G_COMPLEX(type) /* const */ sin(const G_COMPLEX(type) x)			\
{										\
  return G_COMPLEX(type)((type)(sin((double)x.re) * cosh((double)x.im)),\
                 (type)(cos((double)x.re) * sinh((double)x.im)));	\
}										\
										\
G_COMPLEX(type) /* const */ log(const G_COMPLEX(type) x)			\
{										\
  double h = hypot((double)x.re, (double)x.im);				\
  if (h <= 0.0) x.error("attempted log of zero magnitude number.");		\
  return G_COMPLEX(type)((type)log(h), (type)(atan2((double)x.im, (double) x.re)));\
}										\
										\
/* Corrections based on reports from: thc@cs.brown.edu & saito@sdr.slb.com */	\
G_COMPLEX(type) /* const */ pow(const G_COMPLEX(type) x, const G_COMPLEX(type) p)\
{										\
  double h = hypot((double)x.re, (double)x.im);				\
  if (h <= 0.0) x.error("attempted power of zero magnitude number.");		\
										\
  double a = atan2((double)x.im, (double)x.re);				\
  double lr = pow(h, (double)p.re);						\
  double li = (double)p.re * a;						\
  if (p.im != 0.0)								\
  {										\
    lr /= exp((double)p.im * a);						\
    li += p.im * log(h);							\
  }										\
  return G_COMPLEX(type)((type)(lr * cos(li)), (type)(lr * sin(li)));		\
}										\
										\
G_COMPLEX(type) /* const */ pow(const G_COMPLEX(type) x, double p)		\
{										\
  double h = hypot((double)x.re, (double)x.im);				\
  if (h <= 0.0) x.error("attempted power of zero magnitude number.");		\
  double lr = pow(h, p);							\
  double a = atan2((double)x.im, (double)x.re);				\
  double li = p * a;								\
  return G_COMPLEX(type)((type)(lr * cos(li)), (type)(lr * sin(li)));		\
}										\
										\
										\
G_COMPLEX(type) /* const */ sqrt(const G_COMPLEX(type) x)			\
{										\
  if (x.re == 0.0 && x.im == 0.0)					\
    return G_COMPLEX(type)((type)0, (type)0);					\
  else										\
  {										\
    double s = sqrt((fabs((double)x.re) + hypot((double)x.re, (double)x.im)) * 0.5);\
    double d = (x.im / s) * 0.5;						\
    if (x.re > (type)0)							\
      return G_COMPLEX(type)((type) s, (type) d);				\
    else if (x.im >= (type) 0)						\
      return G_COMPLEX(type)((type)d, (type)s);					\
    else									\
      return G_COMPLEX(type)((type)-d, (type)-s);				\
  }										\
}										\
										\
G_COMPLEX(type) log10(const G_COMPLEX(type) x)					\
{										\
    return log(x)*(type)C::log10e;						\
}										\
										\
										\
G_COMPLEX(type) /* const */ pow(const G_COMPLEX(type) x, int p)			\
{										\
  if (p == 0)									\
    return G_COMPLEX(type)((type)1, (type)0);					\
  else if (x == (type) 0)							\
    return G_COMPLEX(type)((type)0, (type)0);					\
  else										\
  {										\
    G_COMPLEX(type) res((type)1, (type)0);					\
    G_COMPLEX(type) b = x;							\
    if (p < 0)									\
    {										\
      p = -p;									\
      b = (type) 1 / b;								\
    }										\
										\
    do {									\
      if ( p & 1 )								\
        res *= b;								\
    } while ( ( p >>= 1 ) != 0 && ((b *= b),1) );				\
    return res;									\
										\
  }										\
}										\
										\
G_COMPLEX(type) cube(const G_COMPLEX(type) val)					\
{										\
    G_COMPLEX(type) retval(val);						\
    retval *= val;								\
    retval *= val;								\
    return retval;								\
}										\
										\
ostream& operator << (ostream& s, const G_COMPLEX(type) x)			\
{										\
  return s << "(" << x.re << "," << x.im << ")" ;			\
}										\
										\
istream& operator >> (istream& s, G_COMPLEX(type)& x)				\
{										\
  if (!s.good())								\
  {										\
    return s;									\
  }										\
  type r, i;									\
  char ch;									\
  s >> ws;									\
  s.get(ch);									\
  if (ch == '(')								\
  {										\
    s >> r;									\
    s >> ws;									\
    s.get(ch);									\
    if (ch == ',')								\
    {										\
      s >> i;									\
      s >> ws;									\
      s .get(ch);								\
    }										\
    else									\
      i = 0;									\
    if (ch != ')')								\
      s.clear(ios::failbit);							\
  }										\
  else										\
  {										\
    s.putback(ch);								\
    s >> r;									\
    i = 0;									\
  }										\
  x = G_COMPLEX(type)(r, i);							\
  return s;									\
}

#define G_COMPLEX_CAST_OP(type)							\
   operator G_COMPLEX(type)() const {return G_COMPLEX(type)((type)re,(type)im);}

#define G_COMPLEX_ASSIGN_OP_DECL(type,from)					\
  G_COMPLEX(type)& operator=(from y) { re = (type) y; im = (type) 0; return *this;} \
  G_COMPLEX(type)& operator=(const G_COMPLEX(from)& y);
#define G_COMPLEX_ASSIGN_OP_DECLS(type,from)					\
  G_COMPLEX(type)& operator=(from y) { re = (type) y; im = (type) 0; return *this;}
#define G_COMPLEX_ASSIGN_OP_IMP(type,from)					\
  G_COMPLEX(type)& G_COMPLEX(type)::operator=(const G_COMPLEX(from)& y) {re = (type)y.re; im = (type)y.im; return *this;}


#define G_COMPLEX_CTOR_OP_DECL(type,intype)					\
   G_COMPLEX(type)(const G_COMPLEX(intype) &v);
#define G_COMPLEX_CTOR_OP_IMP(type,intype)					\
   G_COMPLEX(type)::G_COMPLEX(type)(const G_COMPLEX(intype) &v) : re((type)v.re) { im = (type)v.im; }

#define G_COMPLEX_OPEQ_DECL(type,from)						\
  G_COMPLEX(type)& operator += (from y) { re += (type) y; return *this; }	\
  G_COMPLEX(type)& operator += (const G_COMPLEX(from) &y);			\
  G_COMPLEX(type)& operator -= (from y) { re -= (type) y; return *this; }	\
  G_COMPLEX(type)& operator -= (const G_COMPLEX(from) &y);			\
  G_COMPLEX(type)& operator *= (from y) { re *=  (type) y; im *= (type) y; return *this;}\
  G_COMPLEX(type)& operator *= (const G_COMPLEX(from) &y);			\
  G_COMPLEX(type)& operator /= (from y);					\
  G_COMPLEX(type)& operator /= (const G_COMPLEX(from) &y);

#define G_COMPLEX_OPEQ_IMP(type,from)						\
G_COMPLEX(type)& G_COMPLEX(type)::operator += (const G_COMPLEX(from) &y)	\
		{ re += (type) y.re; im += (type) y.im; return *this; }	\
										\
G_COMPLEX(type)& G_COMPLEX(type)::operator -= (const G_COMPLEX(from) &y)	\
		{ re -= (type) y.re; im -= (type) y.im; return *this; }	\
										\
G_COMPLEX(type)& G_COMPLEX(type)::operator *= (const G_COMPLEX(from) &y) {	\
			type r = re * (type) y.re - im * (type) y.im;	\
			im = re * (type) y.im + im * (type) y.re; 	\
			re = r; 						\
			return *this; 						\
		      }								\
										\
G_COMPLEX(type)& G_COMPLEX(type)::operator /= (const G_COMPLEX(from)& y)	\
{										\
  double den = fabs((double)y.re) + fabs((double)y.im);			\
  if (den == 0.0) error ("Attempted division by zero.");			\
  double xrden = (double) re / den;						\
  double xiden = (double) im / den;						\
  double yrden = (double) y.re / den;					\
  double yiden = (double) y.im / den;					\
  double nrm   = yrden * yrden + yiden * yiden;					\
  re = (type)((xrden * yrden + xiden * yiden) / nrm);				\
  im = (type)((xiden * yrden - xrden * yiden) / nrm);				\
  return *this;									\
}										\
										\
G_COMPLEX(type)& G_COMPLEX(type)::operator /= (from y)				\
{										\
  if (y == (from) 0) error ("Attempted division by zero.");			\
  re /= (type) y;  im /= (type) y;						\
  return *this;									\
}



class G_COMPLEX(double);
class G_COMPLEX(float);

g_declare2(G_COMPLEX,double,G_COMPLEX_CTOR_OP_DECL(double,float) G_COMPLEX_ASSIGN_OP_DECL(double,float))
typedef G_COMPLEX(double) DComplex;
g_declare2(G_COMPLEX,float,G_COMPLEX_CTOR_OP_DECL(float,double))
typedef G_COMPLEX(float) Complex;

typedef Complex float_complex;
typedef DComplex double_complex;




#define G_COMPLEX_BIN_OP_DECL(comtype,realtype)					\
inline G_COMPLEX(comtype) operator + (const G_COMPLEX(comtype)& x, realtype y)		\
{										\
  return G_COMPLEX(comtype)(x.re + (comtype) y, x.im);		\
}										\
										\
inline G_COMPLEX(comtype) operator + (realtype x, const G_COMPLEX(comtype)& y)		\
{										\
  return G_COMPLEX(comtype)((comtype) x + y.re, y.im);		\
}										\
										\
inline G_COMPLEX(comtype) operator - (const G_COMPLEX(comtype)& x, realtype y)		\
{										\
  return G_COMPLEX(comtype)(x.re - (comtype) y, x.im);		\
}										\
										\
inline G_COMPLEX(comtype) operator - (realtype x, const G_COMPLEX(comtype)& y)		\
{										\
  return G_COMPLEX(comtype)((comtype) x - y.re, - y.im);		\
}										\
										\
inline G_COMPLEX(comtype) operator * (const G_COMPLEX(comtype)& x, realtype y)		\
{										\
  return G_COMPLEX(comtype)(x.re * (comtype) y, x.im * (comtype) y);	\
}										\
										\
inline G_COMPLEX(comtype) operator * (realtype x, const G_COMPLEX(comtype)& y)		\
{										\
  return G_COMPLEX(comtype)((comtype) x * y.re, (comtype) x * y.im);	\
}										\
										\
inline G_COMPLEX(comtype) operator / (const G_COMPLEX(comtype)& x, realtype y)		\
{										\
  return G_COMPLEX(comtype)(x.re / (comtype) y, x.im / (comtype) y);	\
}


G_COMPLEX_BIN_OP_DECL(double,float)
G_COMPLEX_BIN_OP_DECL(double,int)
G_COMPLEX_BIN_OP_DECL(float,double)
G_COMPLEX_BIN_OP_DECL(float,int)

									       

/*
 * POLAR
 */
inline G_COMPLEX(double)  polar(double r, double t)
{
  return G_COMPLEX(double)(r * cos(t), r * sin(t));
}

/*
 * near() functions
 */

Bool near(G_COMPLEX(float) val1, G_COMPLEX(float) val2, double tol=1.0e-5);
Bool near(G_COMPLEX(double) val1, G_COMPLEX(double) val2, double tol=1.0e-13);
Bool nearAbs(G_COMPLEX(float) val1, G_COMPLEX(float) val2, double tol=1.0e-5);
Bool nearAbs(G_COMPLEX(double) val1, G_COMPLEX(double) val2, double tol=1.0e-13);

inline Bool allNear(G_COMPLEX(float) val1, G_COMPLEX(float) val2, 
		    double tol=1.0e-5)
    { return near(val1, val2, tol); }
inline Bool allNear(G_COMPLEX(double) val1, G_COMPLEX(double) val2, 
		    double tol=1.0e-13)
    { return near(val1, val2, tol); }
inline Bool allNearAbs(G_COMPLEX(float) val1, G_COMPLEX(float) val2, 
		       double tol=1.0e-5)
    { return nearAbs(val1, val2, tol); }
inline Bool allNearAbs(G_COMPLEX(double) val1, G_COMPLEX(double) val2, 
		       double tol=1.0e-13)
    { return nearAbs(val1, val2, tol); }


// Functions to set and test for IEEE NaN's. A complex number is a NaN if either
// the real or imaginary part is a NaN. setNaN sets both the real and imaginary
// parts to NaN.
// <group>
Bool isNaN(const Complex &val);
Bool isNaN(const DComplex &val);
void setNaN(Complex &val);
void setNaN(DComplex &val);
// </group>


#endif


#endif
