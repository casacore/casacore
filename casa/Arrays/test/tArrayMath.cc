//# tArrayMath2.cc: This program tests the ArrayMath class
//# Copyright (C) 2009
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
//# $Id$

//# Includes
#include "../Cube.h"
#include "../ArrayMath.h"
#include "../ArrayLogical.h"
//#include "../ArrayIO.h"
#include "../ElementFunctions.h"

#include <cmath>

#include <boost/test/unit_test.hpp>

using namespace casacore;
using namespace std;

BOOST_AUTO_TEST_SUITE(array_math)

template<typename T>
T square(T x) { return x*x; }
template<typename T>
T cube(T x) { return x*x*x; }
//template<typename T>
//T amplitude(T x) { return std::abs<T>(x); }

#define TestBinary(OPER,NAME,T,U)                \
void NAME()\
{\
  Array<T> a(IPosition(3,4,5,6));\
  Array<U> b(IPosition(3,4,5,6));\
  Array<T> res;\
  Array<T> e1(IPosition(3,4,5,6));\
  Array<T> e2(IPosition(3,4,5,6));\
  Array<T> e3(IPosition(3,4,5,6));\
  U s1 = 3;\
  T s2 = -5;\
  T* ap = a.data();\
  U* bp = b.data();\
  T* e1p = e1.data();\
  T* e2p = e2.data();\
  T* e3p = e3.data();\
  for (size_t i=0; i<a.size(); ++i) {\
    ap[i] = i+2;\
    bp[i] = i+1;\
    e1p[i] = ap[i] OPER bp[i];\
    e2p[i] = ap[i] OPER s1;\
    e3p[i] = s2 OPER bp[i];\
  }\
  res.assign_conforming( a OPER b);\
  BOOST_CHECK (allEQ(res, e1));\
  res.assign_conforming( a OPER s1);\
  BOOST_CHECK (allEQ(res, e2));\
  res.assign_conforming( s2 OPER b);\
  BOOST_CHECK (allEQ(res, e3));\
  res OPER##= b;\
  BOOST_CHECK (allEQ(res, e3 OPER b));\
  res OPER##=s1;\
  BOOST_CHECK (allEQ(res, e3 OPER b OPER s1));\
  IPosition start(3,0,1,2);\
  IPosition end(3,2,3,4);\
  Array<T> sa(a(start,end));\
  Array<U> sb(b(start,end));\
  res.resize();\
  res.assign_conforming( sa OPER sb);\
  BOOST_CHECK (allEQ(res, e1(start,end)));\
  res.assign_conforming( sa OPER s1);\
  BOOST_CHECK (allEQ(res, e2(start,end)));\
  res.assign_conforming( s2 OPER sb);\
  BOOST_CHECK (allEQ(res, e3(start,end)));\
  res.assign_conforming( sa OPER sb);\
  sa OPER##= sb;\
  BOOST_CHECK (allEQ(sa, res));\
  res.assign_conforming( sa OPER s1);\
  sa OPER##=s1;\
  BOOST_CHECK (allEQ(sa, res));             \
}

#define TestUnary(OPER, NAME, T)\
void NAME()\
{\
  Array<T> a(IPosition(3,4,5,6));\
  Array<T> e(IPosition(3,4,5,6));\
  T* ap = a.data();\
  T* ep = e.data();\
  for (size_t i=0; i<a.size(); ++i) {\
    ap[i] = static_cast<T> ((i+1)/120.);      \
    ep[i] = OPER ap[i];\
  }\
  BOOST_CHECK (allEQ(OPER a, e));\
  IPosition start(3,0,1,2);\
  IPosition end(3,2,3,4);\
  Array<T> sa(a(start,end));\
  BOOST_CHECK (allEQ(OPER sa, e(start,end)));              \
}

#define TestReduce(FUNC, OPER, NAME, T)          \
void NAME()\
{\
  Array<int> a(IPosition(3,4,5,6));\
  int res=0;\
  int* ap = a.data();\
  for (size_t i=0; i<a.size(); ++i) {\
    ap[i] = i+1;\
    res OPER ap[i];\
  }\
  BOOST_CHECK (FUNC(a) == res);\
  IPosition start(3,0,1,2);\
  IPosition end(3,2,3,4);\
  Array<int> sa(a(start,end));\
  Array<int> sb;\
  sb.assign_conforming( sa );    /* copy to make contiguous (that's tested above) */\
  BOOST_CHECK (FUNC(sa) == FUNC(sb));\
}

#define TestMinMax(FUNC, NAME, T)          \
void NAME()\
{\
  Array<int> a(IPosition(3,4,5,6));\
  int res=10;\
  int* ap = a.data();\
  for (size_t i=0; i<a.size(); ++i) {\
    ap[i] = i+1;\
    res = std::FUNC(res, ap[i]);               \
  }\
  BOOST_CHECK (FUNC(a) == res);\
  IPosition start(3,0,1,2);\
  IPosition end(3,2,3,4);\
  Array<int> sa(a(start,end));\
  Array<int> sb;\
  sb.assign_conforming( sa );    /* copy to make contiguous (that's tested above) */\
  BOOST_CHECK (FUNC(sa) == FUNC(sb));\
}

void testMeanFloat()
{
  Array<float> a(IPosition(3,4,5,6));
  float res1=0;
  float res2=0;
  float* ap = a.data();
  for (size_t i=0; i<a.size(); ++i) {
    ap[i] = i+1;
    res1 += ap[i];
    res2 += ap[i] * ap[i];
  }
  float m = mean(a);
  BOOST_CHECK_CLOSE (m, res1/a.size(), 1e-6);
  BOOST_CHECK_CLOSE (rms(a), std::sqrt(res2/a.size()), 1e-6);
  res1 = 0;
  res2 = 0;
  for (size_t i=0; i<a.size(); ++i) {
    res1 += (ap[i] - m) * (ap[i] - m);
    res2 += std::abs(ap[i] - m);
  }
  float v = variance(a);
  float pv = pvariance(a);
  float av = avdev(a);
  BOOST_CHECK_CLOSE(v, res1/(a.size()-1), 1e-6);
  BOOST_CHECK_CLOSE(pv, res1/(a.size()), 1e-6);
  BOOST_CHECK_CLOSE(variance(a, m), v, 1e-6);
  BOOST_CHECK_CLOSE(pvariance(a, m, 1), v, 1e-6);
  BOOST_CHECK_CLOSE(pvariance(a, m, 0), pv, 1e-6);
  BOOST_CHECK_CLOSE(pvariance(a, m), pv, 1e-6);
  BOOST_CHECK_CLOSE(av, res2/a.size(), 1e-6);
  BOOST_CHECK_CLOSE(avdev(a, m), av, 1e-6);
  BOOST_CHECK_CLOSE(stddev(a), std::sqrt(v), 1e-6);
  BOOST_CHECK_CLOSE(pstddev(a), std::sqrt(pv), 1e-6);
  BOOST_CHECK_CLOSE(stddev(a, m), std::sqrt(v), 1e-6);
  BOOST_CHECK_CLOSE(pstddev(a, m), std::sqrt(pv), 1e-6);
  BOOST_CHECK_CLOSE(pstddev(a, m, 0), std::sqrt(pv), 1e-6);
  BOOST_CHECK_CLOSE(pstddev(a, m, 1), std::sqrt(v), 1e-6);
  IPosition start(3,0,1,2);
  IPosition end(3,2,3,4);
  Array<float> sa(a(start,end));
  Array<float> sb;
  sb.assign_conforming(sa);    /* copy to make contiguous (that's tested above) */
  BOOST_CHECK_CLOSE(mean(sa), mean(sb), 1e-6);
  BOOST_CHECK_CLOSE(variance(sa), variance(sb), 1e-6);
  BOOST_CHECK_CLOSE(stddev(sa), stddev(sb), 1e-6);
  BOOST_CHECK_CLOSE(avdev(sa), avdev(sb), 1e-6);
  BOOST_CHECK_CLOSE(rms(sa), rms(sb), 1e-6);
}

void check_close(const std::complex<float>& a, const std::complex<float>& b)
{
  BOOST_CHECK_CLOSE_FRACTION(a.real(), b.real(), 1e-5);
  BOOST_CHECK_CLOSE_FRACTION(a.imag(), b.imag(), 1e-5);
}

void testMeanComplex()
{
  Array<std::complex<float>> a(IPosition(3,4,5,6));
  std::complex<float> res1;
  std::complex<float> res2;
  std::complex<float>* ap = a.data();
  for (int i=0; i<int(a.size()); ++i) {
    ap[i] = std::complex<float>(i+1, 2*i-3);
    res1 += ap[i];
    res2 += ap[i] * ap[i];
  }
  std::complex<float> m = mean(a);
  check_close(m, res1/float(a.size()));
  check_close(rms(a), std::sqrt(res2/float(a.size())));
  res1 = std::complex<float>();
  res2 = std::complex<float>();
  for (size_t i=0; i<a.size(); ++i) {
    res1 += abs(ap[i] - m) * abs(ap[i] - m);
    res2 += abs(ap[i] - m);
  }
  std::complex<float> v = variance(a);
  BOOST_CHECK_EQUAL (v.imag(), 0);
  BOOST_CHECK_CLOSE_FRACTION( v.real(), variance(real(a)) + variance(imag(a)), 1e-5);
  std::complex<float> pv = pvariance(a);
  BOOST_CHECK (pv.imag() == 0);
  BOOST_CHECK_CLOSE_FRACTION( pv.real(), pvariance(real(a)) + pvariance(imag(a)), 1e-5);
  std::complex<float> av = avdev(a);
  BOOST_CHECK (av.imag() == 0);
  check_close(v, res1/float(a.size()-1));
  check_close(pv, res1/float(a.size()));
  check_close(variance(a, m), v);
  check_close(pvariance(a, m, 1), v);
  check_close(pvariance(a, m, 0), pv);
  check_close(pvariance(a, m), pv);
  check_close(av, res2/float(a.size()));
  check_close(avdev(a, m), av);
  check_close(stddev(a), std::sqrt(v));
  check_close(pstddev(a), std::sqrt(pv));
  check_close(stddev(a, m), std::sqrt(v));
  check_close(pstddev(a, m), std::sqrt(pv));
  check_close(pstddev(a, m, 0), std::sqrt(pv));
  check_close(pstddev(a, m, 1), std::sqrt(v));
  IPosition start(3,0,1,2);
  IPosition end(3,2,3,4);
  Array<std::complex<float>> sa(a(start,end));
  Array<std::complex<float>> sb;
  sb.assign_conforming( sa );    /* copy to make contiguous (that's tested above) */
  check_close(mean(sa), mean(sb));
  check_close(variance(sa), variance(sb));
  check_close(stddev(sa), stddev(sb));
  check_close(avdev(sa), avdev(sb));
  check_close(rms(sa), rms(sb));
}

#define TestFunc1(FUNC, NAME, T, TOL)               \
void NAME()\
{\
  Array<T> a(IPosition(3,4,5,6));\
  Array<T> e(IPosition(3,4,5,6));\
  T* ap = a.data();\
  T* ep = e.data();\
  for (size_t i=0; i<a.size(); ++i) {\
    ap[i] = (i+1)/120.;\
    ep[i] = FUNC(ap[i]);\
  }\
  BOOST_CHECK (allNear(FUNC(a), e, TOL));\
  IPosition start(3,0,1,2);\
  IPosition end(3,2,3,4);\
  Array<T> sa(a(start,end));\
  BOOST_CHECK (allNear(FUNC(sa), e(start,end), TOL));\
}

#define TestFunc2(FUNC,NAME,T)                        \
void NAME()\
{\
  Array<T> a(IPosition(3,4,5,6));\
  Array<T> b(IPosition(3,4,5,6));\
  Array<T> res;\
  Array<T> e1(IPosition(3,4,5,6));\
  Array<T> e2(IPosition(3,4,5,6));\
  Array<T> e3(IPosition(3,4,5,6));\
  T s1 = 3;\
  T s2 = -5;\
  T* ap = a.data();\
  T* bp = b.data();\
  T* e1p = e1.data();\
  T* e2p = e2.data();\
  T* e3p = e3.data();\
  for (size_t i=0; i<a.size(); ++i) {\
    ap[i] = (i+2);                 \
    bp[i] = (i+1);                              \
    e1p[i] = FUNC(ap[i], bp[i]);                \
    e2p[i] = FUNC(bp[i], s1);                   \
    e3p[i] = FUNC(s2, ap[i]);                   \
  }\
  res.assign_conforming( FUNC(a, b) );                             \
  BOOST_CHECK (allEQ(res, e1));\
  res.assign_conforming( FUNC(b, s1) );                            \
  BOOST_CHECK (allEQ(res, e2));\
  res.assign_conforming( FUNC(s2, a));                            \
  BOOST_CHECK (allEQ(res, e3));\
  IPosition start(3,0,1,2);\
  IPosition end(3,2,3,4);\
  Array<T> sa(a(start,end));\
  Array<T> sb(b(start,end));\
  res.resize();\
  res.assign_conforming( FUNC(sa, sb) );                           \
  BOOST_CHECK (allEQ(res, e1(start,end)));\
  res.assign_conforming( FUNC(sb, s1) );                           \
  BOOST_CHECK (allEQ(res, e2(start,end)));\
  res.assign_conforming( FUNC(s2, sa) );                           \
  BOOST_CHECK (allEQ(res, e3(start,end)));\
}

#define TestComplex(FUNC, SFUNC, NAME, T, U)\
void NAME()\
{\
  Array<T> a(IPosition(3,4,5,6));\
  Array<U> e(IPosition(3,4,5,6));\
  T* ap = a.data();\
  U* ep = e.data();\
  for (size_t i=0; i<a.size(); ++i) {\
    ap[i] = T(i+1, i+2);\
    ep[i] = SFUNC(ap[i]);\
  }\
  Array<U> z = FUNC(a); \
  for (size_t i=0; i<a.size(); ++i) {\
    BOOST_CHECK(arrays_internal::near(ep[i], z.data()[i], 1e-13)); \
    BOOST_CHECK(arrays_internal::near(ep[i], z.data()[i], 1e-13)); \
  } \
  BOOST_CHECK (allNear(FUNC(a), e, 1e-13));\
  IPosition start(3,0,1,2);\
  IPosition end(3,2,3,4);\
  Array<T> sa(a(start,end));\
  BOOST_CHECK (allNear(FUNC(sa), e(start,end), 1e-13));\
  Array<U> res(a.shape());\
  FUNC(res,a);\
  BOOST_CHECK (allNear(res, e, 1e-13));    \
  e=0;\
  Array<U> se(e(start,end));\
  Array<U> sres(res(start,end));\
  se.assign_conforming( sres );\
  res=0;   \
  FUNC(sres,sa);\
  BOOST_CHECK (allNear(res, e, 1e-13));          \
}

template<typename T, typename U>
void testSetReal()
{
  Array<T> a(IPosition(3,4,5,6));
  Array<T> e(IPosition(3,4,5,6));
  T* ap = a.data();
  T* ep = e.data();
  for (size_t i=0; i<a.size(); ++i) {
    ap[i] = T(i+1, i+2);
    ep[i] = T(i+11, i+2);
  }
  Array<T> b;
  b.assign_conforming( a );
  setReal(b, real(a+T(10,5)));
  BOOST_CHECK (allNear(b, e, 1e-13));
  IPosition start(3,0,1,2);
  IPosition end(3,2,3,4);
  Array<T> sa(a(start,end));
  setReal(sa, real(sa+T(10,5)));
  BOOST_CHECK (allNear(sa, e(start,end), 1e-13));
}

template<typename T, typename U>
void testSetImag()
{
  Array<T> a(IPosition(3,4,5,6));
  Array<T> e(IPosition(3,4,5,6));
  T* ap = a.data();
  T* ep = e.data();
  for (size_t i=0; i<a.size(); ++i) {
    ap[i] = T(i+1, i+2);
    ep[i] = T(i+1, i+7);
  }
  Array<T> b;
  b.assign_conforming( a );
  setImag(b, imag(a+T(10,5)));
  BOOST_CHECK (allNear(b, e, 1e-13));
  IPosition start(3,0,1,2);
  IPosition end(3,2,3,4);
  Array<T> sa(a(start,end));
  setImag(sa, imag(sa+T(10,5)));
  BOOST_CHECK (allNear(sa, e(start,end), 1e-13));
}

template<typename T, typename U>
void testMakeComplex()
{
  Array<T> a(IPosition(3,4,5,6));
  Array<T> b(IPosition(3,4,5,6));
  Array<U> e(IPosition(3,4,5,6));
  T* ap = a.data();
  T* bp = b.data();
  U* ep = e.data();
  for (size_t i=0; i<a.size(); ++i) {
    ap[i] = i+1;
    bp[i] = i+2;
    ep[i] = U(ap[i], bp[i]);
  }
  BOOST_CHECK (allNear(makeComplex(a,b), e, 1e-13));
  IPosition start(3,0,1,2);
  IPosition end(3,2,3,4);
  BOOST_CHECK (allNear(makeComplex(a(start,end), b(start,end)),
                            e(start,end), 1e-13));
}

void testMinMax1()
{
  Array<int> a(IPosition(3,4,5,6));
  indgen (a);
  a.data()[11] = -3;
  a.data()[66] = 1000;
  int minval, maxval;
  IPosition minpos, maxpos;
  // Unmasked minmax.
  minMax(minval, maxval, minpos, maxpos, a);
  BOOST_CHECK (minval == -3);
  BOOST_CHECK (maxval == 1000);
  BOOST_CHECK (minpos == IPosition(3,3,2,0));
  BOOST_CHECK (maxpos == IPosition(3,2,1,3));
  IPosition start(3,0,1,2);
  IPosition end(3,2,3,4);
  Array<int> sa(a(start, end));
  minMax(minval, maxval, minpos, maxpos, sa);
  BOOST_CHECK (minval == 44);
  BOOST_CHECK (maxval == 1000);
  BOOST_CHECK (minpos == IPosition(3,0,0,0));
  BOOST_CHECK (maxpos == IPosition(3,2,0,1));
  // Masked minmax without any flag set.
  Array<bool> mask(a.shape());
  Array<bool> smask(mask(start, end));
  mask = true;
  minMax(minval, maxval, minpos, maxpos, a, mask);
  BOOST_CHECK (minval == -3);
  BOOST_CHECK (maxval == 1000);
  BOOST_CHECK (minpos == IPosition(3,3,2,0));
  BOOST_CHECK (maxpos == IPosition(3,2,1,3));
  // Try without a valid element.
  bool ok=false;
  try {
    minMax(minval, maxval, minpos, maxpos, a, mask, false);
  } catch (std::exception&) {
    ok = true;
  }
  BOOST_CHECK (ok);
  // Masked minmax with some flags set.
  mask.data()[11] = false;
  mask.data()[66] = false;
  a.data()[0] = 10;
  minMax(minval, maxval, minpos, maxpos, a, mask);
  BOOST_CHECK (minval == 1);
  BOOST_CHECK (maxval == 119);
  BOOST_CHECK (minpos == IPosition(3,1,0,0));
  BOOST_CHECK (maxpos == IPosition(3,3,4,5));
  // Non-contiguous subset.
  minMax(minval, maxval, minpos, maxpos, sa, smask);
  BOOST_CHECK (minval == 44);
  BOOST_CHECK (maxval == 94);
  BOOST_CHECK (minpos == IPosition(3,0,0,0));
  BOOST_CHECK (maxpos == IPosition(3,2,2,2));
  // Masked minmax with some opposite mask.
  minMax(minval, maxval, minpos, maxpos, a, mask, false);
  BOOST_CHECK (minval == -3);
  BOOST_CHECK (maxval == 1000);
  BOOST_CHECK (minpos == IPosition(3,3,2,0));
  BOOST_CHECK (maxpos == IPosition(3,2,1,3));
  // Non-contiguous subset with a single valid element.
  minMax(minval, maxval, minpos, maxpos, sa, smask, false);
  BOOST_CHECK (minval == 1000);
  BOOST_CHECK (maxval == 1000);
  BOOST_CHECK (minpos == IPosition(3,2,0,1));
  BOOST_CHECK (maxpos == IPosition(3,2,0,1));
  // Test weighted minmax.
  Array<int> weight(a.shape());
  Array<int> sweight(weight(start, end));
  weight = 1;
  weight.data()[11] = 0;
  weight.data()[66] = 0;
  minMaxMasked(minval, maxval, minpos, maxpos, a, weight);
  BOOST_CHECK (minval == 0);
  BOOST_CHECK (maxval == 119);
  BOOST_CHECK (minpos == IPosition(3,3,2,0));
  BOOST_CHECK (maxpos == IPosition(3,3,4,5));
  // Non-contiguous subset.
  minMaxMasked(minval, maxval, minpos, maxpos, sa, sweight);
  BOOST_CHECK (minval == 0);
  BOOST_CHECK (maxval == 94);
  BOOST_CHECK (minpos == IPosition(3,2,0,1));
  BOOST_CHECK (maxpos == IPosition(3,2,2,2));
}

void testExpand()
{
  // Test linear expansion.
  Cube<int> mat1(IPosition(3,2,3,1));
  indgen(mat1);
  Cube<int> mat2(IPosition(3,8,6,2));
  for (int i=0; i<2; ++i) {
    for (int j=0; j<6; ++j) {
      for (int k=0; k<8; ++k) {
        mat2(k,j,i) = mat1(k/4, j/2, 0);
      }
    }
  }
  Array<int> out(IPosition(3,8,6,2));
  expandArray (out, mat1);
  BOOST_CHECK (allEQ (out, mat2));
  // Test alternate expansion.
  for (int i=0; i<2; ++i) {
    for (int j=0; j<6; ++j) {
      for (int k=0; k<8; ++k) {
        mat2(k,j,i) = mat1(k%2, j%3, 0);
      }
    }
  }
  out=-1;
  expandArray (out, mat1, IPosition(3,1));
  BOOST_CHECK (allEQ (out, mat2));
  // Test mixed expansion.
  for (int i=0; i<2; ++i) {
    for (int j=0; j<6; ++j) {
      for (int k=0; k<8; ++k) {
        mat2(k,j,i) = mat1(k/4, j%3, 0);
      }
    }
  }
  out=-1;
  expandArray (out, mat1, IPosition(3,0,1,1));
  BOOST_CHECK (allEQ (out, mat2));
  // Test another mixed expansion.
  for (int i=0; i<2; ++i) {
    for (int j=0; j<6; ++j) {
      for (int k=0; k<8; ++k) {
        mat2(k,j,i) = mat1(k%2, j/2, 0);
      }
    }
  }
  out=-1;
  expandArray (out, mat1, IPosition(3,1,0,0));
  BOOST_CHECK (allEQ (out, mat2));
  // Test input ndim > output ndim.
  {
    Array<int> out1(IPosition(1,8));
    expandArray (out1, mat1, IPosition(3,1,0,0));
    VectorIterator<int> iter(mat2);
    BOOST_CHECK (allEQ (out1, iter.vector()));
  }
  // Test output ndim > output ndim.
  {
    Array<int> out1(IPosition(5,8,6,2,3,2));
    expandArray (out1, mat1, IPosition(1,1));
    ArrayIterator<int> iter(out1, 3);
    int niter=0;
    while (! iter.pastEnd()) {
      BOOST_CHECK (allEQ (iter.array(), mat2));
      iter.next();
      niter++;
    }
    BOOST_CHECK (niter==6);
  }
}

// Instantiate the macro-ed functions.
TestBinary(+, testPlusInt, int, int)
TestBinary(-, testMinusInt, int, int)
TestBinary(*, testTimesInt, int, int)
TestBinary(/, testDivideInt, int, int)
TestBinary(%, testModuloInt, int, int)
TestBinary(|, testOrInt, int, int)
TestBinary(&, testAndInt, int, int)
TestBinary(^, testXorInt, int, int)
TestBinary(*, testTimesDComplex, std::complex<double>, std::complex<double>)
TestBinary(*, testTimesComplex, std::complex<float>, std::complex<float>)
TestBinary(/, testDivideCF, std::complex<float>, float)
TestBinary(*, testTimesCF, std::complex<float>, float)
TestBinary(/, testDivideCD, std::complex<double>, double)
TestBinary(*, testTimesCD, std::complex<double>, double)
TestUnary(+, testUPlusInt, int)
TestUnary(-, testUMinusInt, int)
TestUnary(~, testUNegateInt, int)
TestReduce(sum, +=, testSumInt, int)
TestReduce(product, *=, testProductInt, int)
TestMinMax(min, testMinInt, int)
TestMinMax(max, testMaxInt, int)
TestFunc1(sin, testSinDouble, double, 1e-13)
TestFunc1(sinh, testSinhDouble, double, 1e-13)
TestFunc1(cos, testCosfloat, float, 1e-6)
TestFunc1(cosh, testCoshfloat, float, 1e-6)
TestFunc1(square, testSqrDouble, double, 1e-13)
TestFunc1(cube, testCubeDouble, double, 1e-13)
TestFunc1(sqrt, testSqrtDouble, double, 1e-13)
TestFunc1(exp, testExpDouble, double, 1e-13)
TestFunc1(log, testLogDouble, double, 1e-13)
TestFunc1(log10, testLog10Double, double, 1e-13)
TestFunc1(sin, testSinComplex, std::complex<float>, 1e-6)
TestFunc1(sinh, testSinhComplex, std::complex<float>, 1e-6)
TestFunc1(cos, testCosComplex, std::complex<double>, 1e-13)
TestFunc1(cosh, testCoshComplex, std::complex<double>, 1e-13)
TestFunc1(square, testSqrComplex, std::complex<float>, 1e-6)
TestFunc1(cube, testCubeComplex, std::complex<float>, 1e-6)
TestFunc1(sqrt, testSqrtComplex, std::complex<float>, 1e-6)
TestFunc1(exp, testExpComplex, std::complex<float>, 1e-6)
TestFunc1(log, testLogComplex, std::complex<float>, 1e-6)
TestFunc1(log10, testLog10Complex, std::complex<float>, 1e-6)
TestFunc1(tan, testTanDouble, double, 1e-13)
TestFunc1(tanh, testTanhDouble, double, 1e-13)
TestFunc1(asin, testAsinDouble, double, 1e-13)
TestFunc1(acos, testAcosDouble, double, 1e-13)
TestFunc1(atan, testAtanDouble, double, 1e-13)
TestFunc1(ceil, testCeilDouble, double, 1e-13)
TestFunc1(fabs, testFabsDouble, double, 1e-13)
TestFunc1(abs, testAbsDouble, double, 1e-13)
TestFunc1(floor, testFloorDouble, double, 1e-13)
TestFunc2(atan2, testAtan2Double, double)
TestFunc2(pow, testPowDouble, double)
TestFunc2(fmod, testFmodDouble, double)
TestFunc2(min, testMin2Double, double)
TestFunc2(max, testMax2Int, int)
TestComplex(amplitude, abs, testComplexAbs, std::complex<float>, float)
TestComplex(phase, arg, testComplexPhase, std::complex<float>, float)
TestComplex(real, real, testComplexReal, std::complex<float>, float)
TestComplex(imag, imag, testComplexImag, std::complex<float>, float)
TestComplex(conj, conj, testComplexConj, std::complex<float>, std::complex<float>)
TestComplex(amplitude, abs, testDComplexAbs, std::complex<double>, double)
TestComplex(phase, arg, testDComplexPhase, std::complex<double>, double)
TestComplex(real, real, testDComplexReal, std::complex<double>, double)
TestComplex(imag, imag, testDComplexImag, std::complex<double>, double)
TestComplex(conj, conj, testDComplexConj, std::complex<double>, std::complex<double>)

BOOST_AUTO_TEST_CASE( all )
{
  testPlusInt();
  testMinusInt();
  testTimesInt();
  testDivideInt();
  testModuloInt();
  testOrInt();
  testAndInt();
  testXorInt();
  testTimesDComplex();
  testTimesComplex();
  testDivideCF();
  testTimesCF();
  testDivideCD();
  testTimesCD();
  testUPlusInt();
  testUMinusInt();
  testUNegateInt();
  testSumInt();
  testProductInt();
  testMinInt();
  testMaxInt();
  testMeanFloat();
  testMeanComplex();
  testSinDouble();
  testSinhDouble();
  testCosfloat();
  testCoshfloat();
  testSqrDouble();
  testSqrtDouble();
  testExpDouble();
  testLogDouble();
  testLog10Double();
  testSinComplex();
  testSinhComplex();
  testCosComplex();
  testCoshComplex();
  testSqrComplex();
  testSqrtComplex();
  testExpComplex();
  testLogComplex();
  testLog10Complex();
  testTanDouble();
  testTanhDouble();
  testAsinDouble();
  testAcosDouble();
  testAtanDouble();
  testCeilDouble();
  testFabsDouble();
  testAbsDouble();
  testFloorDouble();
  testAtan2Double();
  testPowDouble();
  testFmodDouble();
  testMin2Double();
  testMax2Int();
  testComplexAbs();
  testComplexPhase();
  testComplexReal();
  testComplexImag();
  testComplexConj();
  testDComplexAbs();
  testDComplexPhase();
  testDComplexReal();
  testDComplexImag();
  testDComplexConj();
  testSetReal<std::complex<float>,float>();
  testSetReal<std::complex<double>,double>();
  testSetImag<std::complex<float>,float>();
  testSetImag<std::complex<double>,double>();
  testMakeComplex<float,std::complex<float>>();
  testMakeComplex<double,std::complex<double>>();
  testMinMax1();
  testExpand();
}

BOOST_AUTO_TEST_CASE( convert_array )
{
  Array<int> a(IPosition{3, 2}, 7);
  Array<unsigned int> b(IPosition{3, 2}, 1), c(IPosition{3, 2}, 1);
  std::vector<unsigned> ref(6, 1);
  
  convertArray(a, b);
  BOOST_CHECK_EQUAL_COLLECTIONS(b.begin(), b.end(), ref.begin(), ref.end());
  
  convertArray<int, unsigned>(a, c);
  BOOST_CHECK_EQUAL_COLLECTIONS(b.begin(), b.end(), ref.begin(), ref.end());
}

BOOST_AUTO_TEST_SUITE_END()
