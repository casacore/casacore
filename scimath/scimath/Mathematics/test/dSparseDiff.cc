//# dSparseDiff.cc: Demo program for AutoDiff, including 2nd derivative
//# Copyright (C) 2007
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id: dSparseDiff.cc,v 1.1 2007/11/16 04:44:13 wbrouw Exp $

//# Includes
#include <scimath/Mathematics/SparseDiff.h>
#include <scimath/Mathematics/SparseDiffA.h>
#include <scimath/Mathematics/SparseDiffMath.h>
#include <scimath/Mathematics/SparseDiffIO.h>

#include <casa/iostream.h>

#include <casa/namespace.h>
// Define a simple function a^3*b^3*x

template <class T> class f {
public:
  T operator()(const T& x) { return a_p*a_p*a_p*b_p*b_p*x; }
  void set(const T& a, const T& b) { a_p = a; b_p = b; }
private:
  T a_p;
  T b_p;
};

// Specialization

template <> class f<SparseDiffA<Double> > {
public:
  SparseDiffA<Double> operator()(const SparseDiffA<Double>& x) {
    return SparseDiffA<Double>(a_p.value()*a_p.value()*a_p.value()*
			       b_p.value()*b_p.value()*x.value(), 0,
			       3*a_p.value()*a_p.value()*b_p.value()*
			       b_p.value()*x.value()) +
      SparseDiffA<Double>(0, 1, 2*a_p.value()*a_p.value()*a_p.value()*
			  b_p.value()*x.value()) ; }
  void set(const SparseDiff<Double>& a, const SparseDiff<Double>& b) {
    a_p = a; b_p = b; }
private:
  SparseDiffA<Double> a_p;
  SparseDiffA<Double> b_p;
};

int main() {
  cout << "Test SparseDiff" << endl;
  cout << "----------------------------------------" << endl;

  // By selecting Double a,b,x; f(x) will calculate value
  Double a0(2), b0(3), x0(7);
  f<Double> f0; f0.set(a0, b0);
  cout << "Value:      " << f0(x0) << endl;
  
  // By selecting SparseDiff a,b, and x; f(x) will calculate value and
  // partial derivatives wrt a,b
  SparseDiff<Double> a1(2,0), b1(3,1), x1(7);
  f<SparseDiff<Double> > f1; f1.set(a1, b1);
  cout << "Diff a,b:   " << f1(x1) << endl;

  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   Double y(11);
   cout << "a: " << a << endl;
   cout << "b: " << b << endl;
   cout << "x: " << x << endl;
   cout << "y: " << y << endl;
  }
  // *=
  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   Double y(11);
   a *= a;
   b *= b;
   x *= x;
   y *= y;
   cout << "a*=a: " << a << endl;
   cout << "b*=b: " << b << endl;
   cout << "x*=x: " << x << endl;
   cout << "y*=y: " << y << endl;
  }
  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   a *= b;
   x *= b;
   cout << "a*=b: " << a << endl;
   cout << "x*=b: " << x << endl;
  }
  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   a *= x;
   b *= x;
   cout << "a*=x: " << a << endl;
   cout << "b*=x: " << b << endl;
  }
  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   Double y(11);
   a *= y;
   b *= y;
   x *= y;
   cout << "a*=y: " << a << endl;
   cout << "b*=y: " << b << endl;
   cout << "x*=y: " << x << endl;
  }
  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   b *= a;
   x *= a;
   cout << "b*=a: " << b << endl;
   cout << "x*=a: " << x << endl;
  }
  // +=
  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   Double y(11);
   a += a;
   b += b;
   x += x;
   y += y;
   cout << "a+=a: " << a << endl;
   cout << "b+=b: " << b << endl;
   cout << "x+=x: " << x << endl;
   cout << "y+=y: " << y << endl;
  }
  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   a += b;
   x += b;
   cout << "a+=b: " << a << endl;
   cout << "x+=b: " << x << endl;
  }
  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   a += x;
   b += x;
   cout << "a+=x: " << a << endl;
   cout << "b+=x: " << b << endl;
  }
  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   Double y(11);
   a += y;
   b += y;
   x += y;
   cout << "a+=y: " << a << endl;
   cout << "b+=y: " << b << endl;
   cout << "x+=y: " << x << endl;
  }
  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   b += a;
   x += a;
   cout << "b+=a: " << b << endl;
   cout << "x+=a: " << x << endl;
  }
  // -=
  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   Double y(11);
   a -= a;
   b -= b;
   x -= x;
   y -= y;
   cout << "a-=a: " << a << endl;
   cout << "b-=b: " << b << endl;
   cout << "x-=x: " << x << endl;
   cout << "y-=y: " << y << endl;
  }
  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   a -= b;
   x -= b;
   cout << "a-=b: " << a << endl;
   cout << "x-=b: " << x << endl;
  }
  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   a -= x;
   b -= x;
   cout << "a-=x: " << a << endl;
   cout << "b-=x: " << b << endl;
  }
  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   Double y(11);
   a -= y;
   b -= y;
   x -= y;
   cout << "a-=y: " << a << endl;
   cout << "b-=y: " << b << endl;
   cout << "x-=y: " << x << endl;
  }
  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   b -= a;
   x -= a;
   cout << "b-=a: " << b << endl;
   cout << "x-=a: " << x << endl;
  }
  // /=
  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   Double y(11);
   a /= a;
   b /= b;
   x /= x;
   y /= y;
   cout << "a/=a: " << a << endl;
   cout << "b/=b: " << b << endl;
   cout << "x/=x: " << x << endl;
   cout << "y/=y: " << y << endl;
  }
  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   a /= b;
   x /= b;
   cout << "a/=b: " << a << endl;
   cout << "x/=b: " << x << endl;
  }
  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   a /= x;
   b /= x;
   cout << "a/=x: " << a << endl;
   cout << "b/=x: " << b << endl;
  }
  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   Double y(11);
   a /= y;
   b /= y;
   x /= y;
   cout << "a/=y: " << a << endl;
   cout << "b/=y: " << b << endl;
   cout << "x/=y: " << x << endl;
  }
  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   b /= a;
   x /= a;
   cout << "b/=a: " << b << endl;
   cout << "x/=a: " << x << endl;
  }

  // Various 
  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   Double y(11);
   b *= a;
   cout << "b*=a: " << b << endl;
   b *= b;
   cout << "b*=a*=b: " << b << endl;
   b *= x;
   cout << "b*=a*=b*=x: " << b << endl;
   b *= y;
   cout << "b*=a*=b*=x*=y: " << b << endl;
  }
  {
   SparseDiff<Double> a(3,0), b(5,1), x(7);
   Double y(11);
   a *= x;
   b *= y;
   cout << "a*=x: " << a << endl;
   cout << "b*=y: " << b << endl;
  }


  // No need to use the function object, just calculate the expression:
  cout << "Same...:    " << (pow(a1,3.0)*pow(b1,2.0)*x1) << endl;
  /*

  // Use the specialization:
  f<SparseDiffA<Double> > f12; f12.set(a1, b1);
  cout << "Same...:    " << f12(x1) << endl;

  // By selecting SparseDiff x, and a,b; f(x) will calculate value and
  // partial derivative wrt x
  SparseDiff<Double> a2(2), b2(3), x2(7,0);
  f<SparseDiff<Double> > f2; f2.set(a2, b2);
  cout << "Diff x:     " << f2(x2) << endl;

  // By selecting SparseDiff<SparseDiff> a,b, and x; f(x) will calculate value and
  // (first and) 2nd order partial derivatives wrt a,b
  SparseDiff<SparseDiff<Double> > a3(SparseDiff<Double>(2,0),0),
    b3(SparseDiff<Double>(3,1),1), x3(SparseDiff<Double>(7));
  f<SparseDiff<SparseDiff<Double> > > f3; f3.set(a3, b3);
  cout << "Diff2 a,b:  " << f3(x3) << endl;

  // By selecting SparseDiff<SparseDiff> x, and a,b; f(x) will calculate value and
  // (first and) 2nd order partial derivatives wrt x
  SparseDiff<SparseDiff<Double> > a4(SparseDiff<Double>(2)),
    b4(SparseDiff<Double>(3)),
    x4(SparseDiff<Double>(7,0),0);
  f<SparseDiff<SparseDiff<Double> > > f4; f4.set(a4, b4);
  cout << "Diff2 x:    " << f4(x4) << endl;
  */
}


template class f<Double>;
template class f<SparseDiff<Double> >;
///template class f<SparseDiff<SparseDiff<Double> > >;
