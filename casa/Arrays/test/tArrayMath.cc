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
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>


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
  for (uInt i=0; i<a.size(); ++i) {\
    ap[i] = i+2;\
    bp[i] = i+1;\
    e1p[i] = ap[i] OPER bp[i];\
    e2p[i] = ap[i] OPER s1;\
    e3p[i] = s2 OPER bp[i];\
  }\
  res = a OPER b;\
  AlwaysAssertExit (allEQ(res, e1));\
  res = a OPER s1;\
  AlwaysAssertExit (allEQ(res, e2));\
  res = s2 OPER b;\
  AlwaysAssertExit (allEQ(res, e3));\
  res OPER##= b;\
  AlwaysAssertExit (allEQ(res, e3 OPER b));\
  res OPER##=s1;\
  AlwaysAssertExit (allEQ(res, e3 OPER b OPER s1));\
  IPosition start(3,0,1,2);\
  IPosition end(3,2,3,4);\
  Array<T> sa(a(start,end));\
  Array<U> sb(b(start,end));\
  res.resize();\
  res = sa OPER sb;\
  AlwaysAssertExit (allEQ(res, e1(start,end)));\
  res = sa OPER s1;\
  AlwaysAssertExit (allEQ(res, e2(start,end)));\
  res = s2 OPER sb;\
  AlwaysAssertExit (allEQ(res, e3(start,end)));\
  res = sa OPER sb;\
  sa OPER##= sb;\
  AlwaysAssertExit (allEQ(sa, res));\
  res = sa OPER s1;\
  sa OPER##=s1;\
  AlwaysAssertExit (allEQ(sa, res));             \
}

#define TestUnary(OPER, NAME, T)\
void NAME()\
{\
  Array<T> a(IPosition(3,4,5,6));\
  Array<T> e(IPosition(3,4,5,6));\
  T* ap = a.data();\
  T* ep = e.data();\
  for (uInt i=0; i<a.size(); ++i) {\
    ap[i] = static_cast<T> ((i+1)/120.);      \
    ep[i] = OPER ap[i];\
  }\
  AlwaysAssertExit (allEQ(OPER a, e));\
  IPosition start(3,0,1,2);\
  IPosition end(3,2,3,4);\
  Array<T> sa(a(start,end));\
  AlwaysAssertExit (allEQ(OPER sa, e(start,end)));              \
}

#define TestReduce(FUNC, OPER, NAME, T)          \
void NAME()\
{\
  Array<Int> a(IPosition(3,4,5,6));\
  Int res=0;\
  Int* ap = a.data();\
  for (uInt i=0; i<a.size(); ++i) {\
    ap[i] = i+1;\
    res OPER ap[i];\
  }\
  AlwaysAssertExit (FUNC(a) == res);\
  IPosition start(3,0,1,2);\
  IPosition end(3,2,3,4);\
  Array<Int> sa(a(start,end));\
  Array<Int> sb;\
  sb = sa;    /* copy to make contiguous (that's tested above) */\
  AlwaysAssertExit (FUNC(sa) == FUNC(sb));\
}

#define TestMinMax(FUNC, NAME, T)          \
void NAME()\
{\
  Array<Int> a(IPosition(3,4,5,6));\
  Int res=10;\
  Int* ap = a.data();\
  for (uInt i=0; i<a.size(); ++i) {\
    ap[i] = i+1;\
    res = std::FUNC(res, ap[i]);               \
  }\
  AlwaysAssertExit (FUNC(a) == res);\
  IPosition start(3,0,1,2);\
  IPosition end(3,2,3,4);\
  Array<Int> sa(a(start,end));\
  Array<Int> sb;\
  sb = sa;    /* copy to make contiguous (that's tested above) */\
  AlwaysAssertExit (FUNC(sa) == FUNC(sb));\
}

void testMeanFloat()
{
  Array<Float> a(IPosition(3,4,5,6));
  Float res1=0;
  Float res2=0;
  Float* ap = a.data();
  for (uInt i=0; i<a.size(); ++i) {
    ap[i] = i+1;
    res1 += ap[i];
    res2 += ap[i] * ap[i];
  }
  Float m = mean(a);
  AlwaysAssertExit (near(m, res1/a.size()));
  AlwaysAssertExit (near(rms(a), std::sqrt(res2/a.size())));
  res1 = 0;
  res2 = 0;
  for (uInt i=0; i<a.size(); ++i) {
    res1 += (ap[i] - m) * (ap[i] - m);
    res2 += std::abs(ap[i] - m);
  }
  Float v = variance(a);
  Float av = avdev(a);
  AlwaysAssertExit (near(v, res1/(a.size()-1)));
  AlwaysAssertExit (near(variance(a, m), v));
  AlwaysAssertExit (near(av, res2/a.size()));
  AlwaysAssertExit (near(avdev(a, m), av));
  AlwaysAssertExit (near(stddev(a), std::sqrt(v)));
  AlwaysAssertExit (near(stddev(a, m), std::sqrt(v)));
  IPosition start(3,0,1,2);
  IPosition end(3,2,3,4);
  Array<Float> sa(a(start,end));
  Array<Float> sb;
  sb = sa;    /* copy to make contiguous (that's tested above) */
  AlwaysAssertExit (near(mean(sa), mean(sb)));
  AlwaysAssertExit (near(variance(sa), variance(sb)));
  AlwaysAssertExit (near(stddev(sa), stddev(sb)));
  AlwaysAssertExit (near(avdev(sa), avdev(sb)));
  AlwaysAssertExit (near(rms(sa), rms(sb)));
}

#define TestFunc1(FUNC, NAME, T, TOL)               \
void NAME()\
{\
  Array<T> a(IPosition(3,4,5,6));\
  Array<T> e(IPosition(3,4,5,6));\
  T* ap = a.data();\
  T* ep = e.data();\
  for (uInt i=0; i<a.size(); ++i) {\
    ap[i] = (i+1)/120.;\
    ep[i] = FUNC(ap[i]);\
  }\
  AlwaysAssertExit (allNear(FUNC(a), e, TOL));\
  IPosition start(3,0,1,2);\
  IPosition end(3,2,3,4);\
  Array<T> sa(a(start,end));\
  AlwaysAssertExit (allNear(FUNC(sa), e(start,end), TOL));\
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
  for (uInt i=0; i<a.size(); ++i) {\
    ap[i] = (i+2);                 \
    bp[i] = (i+1);                              \
    e1p[i] = FUNC(ap[i], bp[i]);                \
    e2p[i] = FUNC(bp[i], s1);                   \
    e3p[i] = FUNC(s2, ap[i]);                   \
  }\
  res = FUNC(a, b);                             \
  AlwaysAssertExit (allEQ(res, e1));\
  res = FUNC(b, s1);                            \
  AlwaysAssertExit (allEQ(res, e2));\
  res = FUNC(s2, a);                            \
  AlwaysAssertExit (allEQ(res, e3));\
  IPosition start(3,0,1,2);\
  IPosition end(3,2,3,4);\
  Array<T> sa(a(start,end));\
  Array<T> sb(b(start,end));\
  res.resize();\
  res = FUNC(sa, sb);                           \
  AlwaysAssertExit (allEQ(res, e1(start,end)));\
  res = FUNC(sb, s1);                           \
  AlwaysAssertExit (allEQ(res, e2(start,end)));\
  res = FUNC(s2, sa);                           \
  AlwaysAssertExit (allEQ(res, e3(start,end)));\
}

#define TestComplex(FUNC, SFUNC, NAME, T, U)\
void NAME()\
{\
  Array<T> a(IPosition(3,4,5,6));\
  Array<U> e(IPosition(3,4,5,6));\
  T* ap = a.data();\
  U* ep = e.data();\
  for (uInt i=0; i<a.size(); ++i) {\
    ap[i] = T(i+1, i+2);\
    ep[i] = SFUNC(ap[i]);\
  }\
  AlwaysAssertExit (allNear(FUNC(a), e, 1e-13));\
  IPosition start(3,0,1,2);\
  IPosition end(3,2,3,4);\
  Array<T> sa(a(start,end));\
  AlwaysAssertExit (allNear(FUNC(sa), e(start,end), 1e-13));\
  Array<U> res(a.shape());\
  FUNC(res,a);\
  AlwaysAssertExit (allNear(res, e, 1e-13));    \
  e=0;\
  Array<U> se(e(start,end));\
  Array<U> sres(res(start,end));\
  se = sres;\
  res=0;   \
  FUNC(sres,sa);\
  AlwaysAssertExit (allNear(res, e, 1e-13));          \
}

template<typename T, typename U>
void testSetReal()
{
  Array<T> a(IPosition(3,4,5,6));
  Array<T> e(IPosition(3,4,5,6));
  T* ap = a.data();
  T* ep = e.data();
  for (uInt i=0; i<a.size(); ++i) {
    ap[i] = T(i+1, i+2);
    ep[i] = T(i+11, i+2);
  }
  Array<T> b;
  b = a;
  setReal(b, real(a+T(10,5)));
  AlwaysAssertExit (allNear(b, e, 1e-13));
  IPosition start(3,0,1,2);
  IPosition end(3,2,3,4);
  Array<T> sa(a(start,end));
  setReal(sa, real(sa+T(10,5)));
  AlwaysAssertExit (allNear(sa, e(start,end), 1e-13));
}

template<typename T, typename U>
void testSetImag()
{
  Array<T> a(IPosition(3,4,5,6));
  Array<T> e(IPosition(3,4,5,6));
  T* ap = a.data();
  T* ep = e.data();
  for (uInt i=0; i<a.size(); ++i) {
    ap[i] = T(i+1, i+2);
    ep[i] = T(i+1, i+7);
  }
  Array<T> b;
  b = a;
  setImag(b, imag(a+T(10,5)));
  AlwaysAssertExit (allNear(b, e, 1e-13));
  IPosition start(3,0,1,2);
  IPosition end(3,2,3,4);
  Array<T> sa(a(start,end));
  setImag(sa, imag(sa+T(10,5)));
  AlwaysAssertExit (allNear(sa, e(start,end), 1e-13));
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
  for (uInt i=0; i<a.size(); ++i) {
    ap[i] = i+1;
    bp[i] = i+2;
    ep[i] = U(ap[i], bp[i]);
  }
  AlwaysAssertExit (allNear(makeComplex(a,b), e, 1e-13));
  IPosition start(3,0,1,2);
  IPosition end(3,2,3,4);
  AlwaysAssertExit (allNear(makeComplex(a(start,end), b(start,end)),
                            e(start,end), 1e-13));
}

void testMinMax1()
{
  Array<Int> a(IPosition(3,4,5,6));
  indgen (a);
  a.data()[11] = -3;
  a.data()[66] = 1000;
  Int minval, maxval;
  IPosition minpos, maxpos;
  // Unmasked minmax.
  minMax(minval, maxval, minpos, maxpos, a);
  AlwaysAssertExit (minval == -3);
  AlwaysAssertExit (maxval == 1000);
  AlwaysAssertExit (minpos == IPosition(3,3,2,0));
  AlwaysAssertExit (maxpos == IPosition(3,2,1,3));
  IPosition start(3,0,1,2);
  IPosition end(3,2,3,4);
  Array<Int> sa(a(start, end));
  minMax(minval, maxval, minpos, maxpos, sa);
  AlwaysAssertExit (minval == 44);
  AlwaysAssertExit (maxval == 1000);
  AlwaysAssertExit (minpos == IPosition(3,0,0,0));
  AlwaysAssertExit (maxpos == IPosition(3,2,0,1));
  // Masked minmax without any flag set.
  Array<Bool> mask(a.shape());
  Array<Bool> smask(mask(start, end));
  mask = True;
  minMax(minval, maxval, minpos, maxpos, a, mask);
  AlwaysAssertExit (minval == -3);
  AlwaysAssertExit (maxval == 1000);
  AlwaysAssertExit (minpos == IPosition(3,3,2,0));
  AlwaysAssertExit (maxpos == IPosition(3,2,1,3));
  // Try without a valid element.
  Bool ok=False;
  try {
    minMax(minval, maxval, minpos, maxpos, a, mask, False);
  } catch (std::exception&) {
    ok = True;
  }
  AlwaysAssertExit (ok);
  // Masked minmax with some flags set.
  mask.data()[11] = False;
  mask.data()[66] = False;
  a.data()[0] = 10;
  minMax(minval, maxval, minpos, maxpos, a, mask);
  AlwaysAssertExit (minval == 1);
  AlwaysAssertExit (maxval == 119);
  AlwaysAssertExit (minpos == IPosition(3,1,0,0));
  AlwaysAssertExit (maxpos == IPosition(3,3,4,5));
  // Non-contiguous subset.
  minMax(minval, maxval, minpos, maxpos, sa, smask);
  AlwaysAssertExit (minval == 44);
  AlwaysAssertExit (maxval == 94);
  AlwaysAssertExit (minpos == IPosition(3,0,0,0));
  AlwaysAssertExit (maxpos == IPosition(3,2,2,2));
  // Masked minmax with some opposite mask.
  minMax(minval, maxval, minpos, maxpos, a, mask, False);
  AlwaysAssertExit (minval == -3);
  AlwaysAssertExit (maxval == 1000);
  AlwaysAssertExit (minpos == IPosition(3,3,2,0));
  AlwaysAssertExit (maxpos == IPosition(3,2,1,3));
  // Non-contiguous subset with a single valid element.
  minMax(minval, maxval, minpos, maxpos, sa, smask, False);
  AlwaysAssertExit (minval == 1000);
  AlwaysAssertExit (maxval == 1000);
  AlwaysAssertExit (minpos == IPosition(3,2,0,1));
  AlwaysAssertExit (maxpos == IPosition(3,2,0,1));
  // Test weighted minmax.
  Array<Int> weight(a.shape());
  Array<Int> sweight(weight(start, end));
  weight = 1;
  weight.data()[11] = 0;
  weight.data()[66] = 0;
  minMaxMasked(minval, maxval, minpos, maxpos, a, weight);
  AlwaysAssertExit (minval == 0);
  AlwaysAssertExit (maxval == 119);
  AlwaysAssertExit (minpos == IPosition(3,3,2,0));
  AlwaysAssertExit (maxpos == IPosition(3,3,4,5));
  // Non-contiguous subset.
  minMaxMasked(minval, maxval, minpos, maxpos, sa, sweight);
  AlwaysAssertExit (minval == 0);
  AlwaysAssertExit (maxval == 94);
  AlwaysAssertExit (minpos == IPosition(3,2,0,1));
  AlwaysAssertExit (maxpos == IPosition(3,2,2,2));
}

// Instantiate the macro-ed functions.
TestBinary(+, testPlusInt, Int, Int)
TestBinary(-, testMinusInt, Int, Int)
TestBinary(*, testTimesInt, Int, Int)
TestBinary(/, testDivideInt, Int, Int)
TestBinary(%, testModuloInt, Int, Int)
TestBinary(|, testOrInt, Int, Int)
TestBinary(&, testAndInt, Int, Int)
TestBinary(^, testXorInt, Int, Int)
TestBinary(*, testTimesDComplex, DComplex, DComplex)
TestBinary(*, testTimesComplex, Complex, Complex)
TestBinary(/, testDivideCF, Complex, Float)
TestBinary(*, testTimesCF, Complex, Float)
TestBinary(/, testDivideCD, DComplex, Double)
TestBinary(*, testTimesCD, DComplex, Double)
TestUnary(+, testUPlusInt, Int)
TestUnary(-, testUMinusInt, Int)
TestUnary(~, testUNegateInt, Int)
TestReduce(sum, +=, testSumInt, Int)
TestReduce(product, *=, testProductInt, Int)
TestMinMax(min, testMinInt, Int)
TestMinMax(max, testMaxInt, Int)
TestFunc1(sin, testSinDouble, Double, 1e-13)
TestFunc1(sinh, testSinhDouble, Double, 1e-13)
TestFunc1(cos, testCosFloat, Float, 1e-6)
TestFunc1(cosh, testCoshFloat, Float, 1e-6)
TestFunc1(square, testSqrDouble, Double, 1e-13)
TestFunc1(cube, testCubeDouble, Double, 1e-13)
TestFunc1(sqrt, testSqrtDouble, Double, 1e-13)
TestFunc1(exp, testExpDouble, Double, 1e-13)
TestFunc1(log, testLogDouble, Double, 1e-13)
TestFunc1(log10, testLog10Double, Double, 1e-13)
TestFunc1(sin, testSinComplex, Complex, 1e-6)
TestFunc1(sinh, testSinhComplex, Complex, 1e-6)
TestFunc1(cos, testCosDComplex, DComplex, 1e-13)
TestFunc1(cosh, testCoshDComplex, DComplex, 1e-13)
TestFunc1(square, testSqrComplex, Complex, 1e-6)
TestFunc1(cube, testCubeComplex, Complex, 1e-6)
TestFunc1(sqrt, testSqrtComplex, Complex, 1e-6)
TestFunc1(exp, testExpComplex, Complex, 1e-6)
TestFunc1(log, testLogComplex, Complex, 1e-6)
TestFunc1(log10, testLog10Complex, Complex, 1e-6)
TestFunc1(tan, testTanDouble, Double, 1e-13)
TestFunc1(tanh, testTanhDouble, Double, 1e-13)
TestFunc1(asin, testAsinDouble, Double, 1e-13)
TestFunc1(acos, testAcosDouble, Double, 1e-13)
TestFunc1(atan, testAtanDouble, Double, 1e-13)
TestFunc1(ceil, testCeilDouble, Double, 1e-13)
TestFunc1(fabs, testFabsDouble, Double, 1e-13)
TestFunc1(abs, testAbsDouble, Double, 1e-13)
TestFunc1(floor, testFloorDouble, Double, 1e-13)
TestFunc2(atan2, testAtan2Double, Double)
TestFunc2(pow, testPowDouble, Double)
TestFunc2(fmod, testFmodDouble, Double)
TestFunc2(min, testMin2Double, Double)
TestFunc2(max, testMax2Int, Int)
TestComplex(amplitude, fabs, testComplexAbs, Complex, Float)
TestComplex(phase, arg, testComplexPhase, Complex, Float)
TestComplex(real, real, testComplexReal, Complex, Float)
TestComplex(imag, imag, testComplexImag, Complex, Float)
TestComplex(conj, conj, testComplexConj, Complex, Complex)
TestComplex(amplitude, fabs, testDComplexAbs, DComplex, Double)
TestComplex(phase, arg, testDComplexPhase, DComplex, Double)
TestComplex(real, real, testDComplexReal, DComplex, Double)
TestComplex(imag, imag, testDComplexImag, DComplex, Double)
TestComplex(conj, conj, testDComplexConj, DComplex, DComplex)

int main()
{
  try {
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
    testSinDouble();
    testSinhDouble();
    testCosFloat();
    testCoshFloat();
    testSqrDouble();
    testSqrtDouble();
    testExpDouble();
    testLogDouble();
    testLog10Double();
    testSinComplex();
    testSinhComplex();
    testCosDComplex();
    testCoshDComplex();
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
    testSetReal<Complex,Float>();
    testSetReal<DComplex,Double>();
    testSetImag<Complex,Float>();
    testSetImag<DComplex,Double>();
    testMakeComplex<Float,Complex>();
    testMakeComplex<Double,DComplex>();
    testMinMax1();
  } catch (AipsError x) {
    cout << "Unexpected exception: " << x.getMesg() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
