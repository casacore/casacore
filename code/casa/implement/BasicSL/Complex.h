/*
Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of the GNU C++ Library.  This library is free
software; you can redistribute it and/or modify it under the terms of
the GNU Library General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.  This library is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU Library General Public License for more details.
You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*/

//# $Id$

#ifndef _Complex_h
#ifdef __GNUG__
#pragma interface
#endif
#define _Complex_h 1


#include <aips/aips.h>
#include <aips/Utilities/generic.h>
#include <iostream.h>
#include <aips/Mathematics/Math.h>
#include <stdlib.h>


// <summary> Complex numbers </summary>

// <synopsis>
// This documentation is lifted straight from the libg++ info page. Our classes
// are based on those classes, although we have made some modifications to them.
// The ANSI/ISO standard library has a complex<t> type. Until those classes are
// widely available, we will  use these complex classes. Although the 
// documentation below refers to Complex; DComplex (double precision) and
// IComplex (integral complex) also exist and freely interconvert.
// 
//    Class `Complex' is implemented in a way similar to that described by
// Stroustrup. In keeping with libg++ conventions, the class is named
// `Complex', not `complex'.  Complex arithmetic and relational operators
// are provided (`+, -, *, /, +=, -=, *=, /=, ==, !=').  Attempted
// division by (0, 0) triggers an exception.
// 
//    Complex numbers may be constructed and used in the following ways:
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
// 
// </synopsis>

// <group name="Complex numbers">

//# It seems as though the Sun Cfront compiler does not automatically
//# cast parameters to binary operators (?). By defining this, MANY
//# versions of the operators will be defined for each possibility...
#define COMPLEX_STUPID_COMPILER

#if defined(G_COMPLEX)
#undef G_COMPLEX
#endif

#define G_COMPLEX(type)		g_name2(type,G_COMPLEX)

#define G_COMPLEXdeclare2(type,CASTS)						\
imported class G_COMPLEX(type)								\
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
  return s << "(" << x.re << ", " << x.im << ")" ;			\
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



imported class G_COMPLEX(double);
imported class G_COMPLEX(float);
imported class G_COMPLEX(int);

g_declare2(G_COMPLEX,double,G_COMPLEX_CTOR_OP_DECL(double,int) G_COMPLEX_CTOR_OP_DECL(double,float) G_COMPLEX_ASSIGN_OP_DECLS(double,double) G_COMPLEX_ASSIGN_OP_DECL(double,float) G_COMPLEX_ASSIGN_OP_DECL(double,int) G_COMPLEX_OPEQ_DECL(double,float) G_COMPLEX_OPEQ_DECL(double,int))
typedef G_COMPLEX(double) DComplex;
g_declare2(G_COMPLEX,float,G_COMPLEX_CAST_OP(double) G_COMPLEX_CTOR_OP_DECL(float,int) G_COMPLEX_CTOR_OP_DECL(float,double) G_COMPLEX_ASSIGN_OP_DECL(float,double) G_COMPLEX_ASSIGN_OP_DECL(float,int) G_COMPLEX_OPEQ_DECL(float,int) G_COMPLEX_OPEQ_DECL(float,double))
typedef G_COMPLEX(float) Complex;
g_declare2(G_COMPLEX,int,G_COMPLEX_CAST_OP(float) G_COMPLEX_CAST_OP(double) G_COMPLEX_CTOR_OP_DECL(int,float) G_COMPLEX_CTOR_OP_DECL(int,double) G_COMPLEX_ASSIGN_OP_DECL(int,double) G_COMPLEX_ASSIGN_OP_DECL(int,float) G_COMPLEX_OPEQ_DECL(int,float) G_COMPLEX_OPEQ_DECL(int,double))
typedef G_COMPLEX(int) IComplex;

typedef Complex float_complex;
typedef DComplex double_complex;

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

#if defined(COMPLEX_STUPID_COMPILER)
//#***************************************************************************
//#********************* Should not be needed, casts *************************
//#********************* should take care of these   *************************
//#***************************************************************************
#define G_COMPLEX_BIN_OP_DECL(ret,left,right)					\
/*										\
 * non-inline functions								\
 */										\
G_COMPLEX(ret) operator /  (const G_COMPLEX(left)& x, const G_COMPLEX(right)& y);\
G_COMPLEX(ret) operator /  (const G_COMPLEX(left)& x, right y);			\
G_COMPLEX(ret) operator /  (left x, const G_COMPLEX(right)& y);			\
										\
/*										\
 * EQUALITY OPERATORS								\
 */										\
inline int  operator == (const G_COMPLEX(left)& x, const G_COMPLEX(right)& y)	\
{										\
  return (ret) x.re == (ret) y.re && (ret) x.im == (ret) y.im;	\
}										\
										\
inline int  operator == (const G_COMPLEX(left)& x, right y)			\
{										\
  return (ret) x.im == (ret) 0.0 && (ret) x.re == (ret) y;		\
}										\
										\
inline int  operator != (const G_COMPLEX(left)& x, const G_COMPLEX(right)& y)	\
{										\
  return (ret) x.re != (ret) y.re || (ret) x.im != (ret) y.im;	\
}										\
										\
inline int  operator != (const G_COMPLEX(left)& x, right y)			\
{										\
  return (ret) x.im != (ret) 0.0 || (ret) x.re != (ret) y;		\
}										\
										\
/*										\
 * COMPARISON OPERATORS (using norm)						\
 */										\
inline int operator >= (const G_COMPLEX(left) &x, const G_COMPLEX(right) &y) {	\
  return norm(x) >= norm(y);							\
}										\
										\
inline int operator >= (const G_COMPLEX(left) &x, const right y) {		\
  return norm(x) >= (double) y * (double) y;					\
}										\
										\
inline int operator >= (const left x, const G_COMPLEX(right) &y) {		\
  return (double) x * (double) x >= norm(y);					\
}										\
										\
inline int operator > (const G_COMPLEX(left) &x, const G_COMPLEX(right) &y) {	\
  return norm(x) > norm(y);							\
}										\
										\
inline int operator > (const G_COMPLEX(left) &x, const right y) {		\
  return norm(x) > (double) y * (double) y;					\
}										\
										\
inline int operator > (const left x, const G_COMPLEX(right) &y) {		\
  return (double) x * (double) x > norm(y);					\
}										\
										\
inline int operator <= (const G_COMPLEX(left) &x, const G_COMPLEX(right) &y) {	\
  return norm(x) <= norm(y);							\
}										\
										\
inline int operator <= (const G_COMPLEX(left) &x, const right y) {		\
  return norm(x) <= (double) y * (double)y;					\
}										\
										\
inline int operator <= (const left x, const G_COMPLEX(right) &y) {		\
  return (double) x * (double) x <= norm(y);					\
}										\
										\
inline int operator < (const G_COMPLEX(left) &x, const G_COMPLEX(right) &y) {	\
  return norm(x) < norm(y);							\
}										\
										\
inline int operator < (const G_COMPLEX(left) &x, const right y) {		\
  return norm(x) < (double) y * (double) y;					\
}										\
										\
inline int operator < (const left x, const G_COMPLEX(right) &y) {		\
  return (double) x * (double) x < norm(y);					\
}										\
										\
/*										\
 * SIMPLE NUMERIC OPERATIONS							\
 */										\
inline G_COMPLEX(ret) operator + (const G_COMPLEX(left)& x, const G_COMPLEX(right)& y)\
{										\
  return G_COMPLEX(ret)((ret) x.re + (ret) y.re, (ret) x.im + (ret) y.im);\
}										\
										\
inline G_COMPLEX(ret) operator + (const G_COMPLEX(left)& x, right y)		\
{										\
  return G_COMPLEX(ret)((ret) x.re + (ret) y, (ret) x.im);		\
}										\
										\
inline G_COMPLEX(ret) operator + (left x, const G_COMPLEX(right)& y)		\
{										\
  return G_COMPLEX(ret)((ret) x + (ret) y.re, (ret) y.im);		\
}										\
										\
inline G_COMPLEX(ret) operator - (const G_COMPLEX(left)& x, const G_COMPLEX(right)& y)\
{										\
  return G_COMPLEX(ret)((ret) x.re - (ret) y.re, (ret) x.im - (ret) y.im);\
}										\
										\
inline G_COMPLEX(ret) operator - (const G_COMPLEX(left)& x, right y)		\
{										\
  return G_COMPLEX(ret)((ret) x.re - (ret) y, (ret) x.im);		\
}										\
										\
inline G_COMPLEX(ret) operator - (left x, const G_COMPLEX(right)& y)		\
{										\
  return G_COMPLEX(ret)((ret) x - (ret) y.re, - (ret) y.im);		\
}										\
										\
inline G_COMPLEX(ret) operator * (const G_COMPLEX(left)& x, const G_COMPLEX(right)& y)\
{										\
  return G_COMPLEX(ret)((ret) x.re * (ret) y.re - (ret) x.im * (ret) y.im,\
                 (ret) x.re * (ret) y.im + (ret) x.im * (ret) y.re);\
}										\
										\
inline G_COMPLEX(ret) operator * (const G_COMPLEX(left)& x, right y)		\
{										\
  return G_COMPLEX(ret)((ret) x.re * (ret) y, (ret) x.im * (ret) y);	\
}										\
										\
inline G_COMPLEX(ret) operator * (left x, const G_COMPLEX(right)& y)		\
{										\
  return G_COMPLEX(ret)((ret) x * (ret) y.re, (ret) x * (ret) y.im);	\
}										\
										\
G_COMPLEX(ret) pow(const G_COMPLEX(left) &x, const G_COMPLEX(right) &p);


#define G_COMPLEX_BIN_OP_IMP(ret,left,right)					\
										\
/* from romine@xagsun.epm.ornl.gov */						\
G_COMPLEX(ret) /* const */ operator / (const G_COMPLEX(left)& x, const G_COMPLEX(right)& y)\
{										\
  double den = fabs((double)y.re) + fabs((double)y.im);			\
  if (den == 0.0) x.error ("Attempted division by zero.");			\
  double xrden = x.re / den;						\
  double xiden = x.im / den;						\
  double yrden = y.re / den;						\
  double yiden = y.im / den;						\
  double nrm   = yrden * yrden + yiden * yiden;					\
  return G_COMPLEX(ret)((ret)((xrden * yrden + xiden * yiden) / nrm),		\
                 (ret)((xiden * yrden - xrden * yiden) / nrm));			\
}										\
										\
G_COMPLEX(ret) /* const */ operator / (left x, const G_COMPLEX(right)& y)	\
{										\
  double den = norm(y);								\
  if (den == 0.0) y.error ("Attempted division by zero.");			\
  return G_COMPLEX(ret)((ret)((x * y.re) / den), -(ret)((x * y.im) / den));\
}										\
										\
G_COMPLEX(ret) /* const */ operator / (const G_COMPLEX(left)& x, right y)	\
{										\
  if (y == 0.0) x.error ("Attempted division by zero.");			\
  return G_COMPLEX(ret)((ret)(x.re / y), (ret)(x.im / y));		\
}										\
										\
/* Corrections based on reports from: thc@cs.brown.edu & saito@sdr.slb.com */	\
G_COMPLEX(ret) /* const */ pow(const G_COMPLEX(left) &x, const G_COMPLEX(right) &p)\
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
  return G_COMPLEX(ret)((ret)(lr * cos(li)), (ret)(lr * sin(li)));		\
}										\
										\


#define G_COMPLEX_DO_BIN_OP_DECL		\
/*G_COMPLEX_BIN_OP_DECL(double,double,double)*/	\
G_COMPLEX_BIN_OP_DECL(double,double,float)	\
G_COMPLEX_BIN_OP_DECL(double,float,double)	\
G_COMPLEX_BIN_OP_DECL(double,double,int)	\
G_COMPLEX_BIN_OP_DECL(double,int,double)	\
/*G_COMPLEX_BIN_OP_DECL(float,float,float)*/	\
G_COMPLEX_BIN_OP_DECL(float,float,int)		\
G_COMPLEX_BIN_OP_DECL(float,int,float)		\
/*G_COMPLEX_BIN_OP_DECL(int,int,int)*/

#define G_COMPLEX_DO_BIN_OP_IMP			\
/*G_COMPLEX_BIN_OP_IMP(double,double,double)*/	\
G_COMPLEX_BIN_OP_IMP(double,double,float)	\
G_COMPLEX_BIN_OP_IMP(double,float,double)	\
G_COMPLEX_BIN_OP_IMP(double,double,int)		\
G_COMPLEX_BIN_OP_IMP(double,int,double)		\
/*G_COMPLEX_BIN_OP_IMP(float,float,float)*/	\
G_COMPLEX_BIN_OP_IMP(float,float,int)		\
G_COMPLEX_BIN_OP_IMP(float,int,float)		\
/*G_COMPLEX_BIN_OP_IMP(int,int,int)*/

G_COMPLEX_DO_BIN_OP_DECL
//#***************************************************************************
//#********************* Should not be needed, casts *************************
//#********************* should take care of these   *************************
//#***************************************************************************
#endif

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
