//# tMArrayMath.cc: test program for MArrayMath and MArrayLogical
//# Copyright (C) 2012
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

#include <casacore/tables/TaQL/MArrayMath.h>
#include <casacore/tables/TaQL/MArrayLogical.h>
#include <casacore/casa/Arrays/ArrayPartMath.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/OS/Timer.h>
#include <iostream>

using namespace casacore;
using namespace std;

template <typename T>
void check (const MArray<T>& ma, const T& v, Bool m, Bool unmasked=False)
{
  // Check if data and mask have the expected value.
  AlwaysAssertExit (allEQ(ma.array(), v));
  AlwaysAssertExit (ma.hasMask() == !unmasked);
  if (!unmasked) {
    AlwaysAssertExit (allEQ(ma.mask(), m));
  }
}
template <typename T>
void checkNear (const MArray<T>& ma, const T& v, Bool m, Bool empty=False)
{
  // Check if data is near the expected value.
  AlwaysAssertExit (allNear(ma.array(), v, 1e-5));
  AlwaysAssertExit (empty == ma.mask().empty());
  if (!empty) {
    AlwaysAssertExit (allEQ(ma.mask(), m));
  }
}

template <typename T> void doTestAll()
{
  // Test most arithmetic and logical operators as array-array,
  // array-scalar, and scalar-array (with and without mask).
  MArray<T> m1 (Vector<T>(2,16), Vector<Bool>(2,True));
  MArray<T> m2 (Vector<T>(2,8));
  check (m1+m2, T(16+8), True);
  check (m1-m2, T(16-8), True);
  check (m1*m2, T(16*8), True);
  check (m1/m2, T(16)/T(8), True);
  check (m1==m2, T(16)==T(8), True);
  check (m1>=m2, T(16)>=T(8), True);
  check (m1>m2, T(16)>T(8), True);
  check (m1<=m2, T(16)<=T(8), True);
  check (m1<m2, T(16)<T(8), True);
  check (m1!=m2, T(16)!=T(8), True);
  check (m1+T(4), T(16+4), True);
  check (m1-T(4), T(16-4), True);
  check (m1*T(4), T(16*4), True);
  check (m1/T(4), T(16)/T(4), True);
  check (m1==T(4), T(16)==T(4), True);
  check (m1>=T(4), T(16)>=T(4), True);
  check (m1>T(4), T(16)>T(4), True);
  check (m1<=T(4), T(16)<=T(4), True);
  check (m1<T(4), T(16)<T(4), True);
  check (m1!=T(4), T(16)!=T(4), True);
  check (T(4)+m2, T(4+8), True, True);
  check (T(4)-m2, T(4-8), True, True);
  check (T(4)*m2, T(4*8), True, True);
  check (T(4)/m2, T(4)/T(8), True, True);
  check (T(4)==m2, T(4)==T(8), True, True);
  check (T(4)>=m2, T(4)>=T(8), True, True);
  check (T(4)>m2, T(4)>T(8), True, True);
  check (T(4)<=m2, T(4)<=T(8), True, True);
  check (T(4)<m2, T(4)<T(8), True, True);
  check (T(4)!=m2, T(4)!=T(8), True, True);
  check (-m1, T(-16), True);
}

template <typename T> void doTestInt()
{
  // Test operators only useful for int.
  MArray<T> m1 (Vector<T>(2,11), Vector<Bool>(2,True));
  MArray<T> m2 (Vector<T>(2,7));
  check (m1%m2, T(11%7), True);
  check (m1&m2, T(11&7), True);
  check (m1|m2, T(11|7), True);
  check (m1^m2, T(11^7), True);
  check (m1%T(6), T(11%6), True);
  check (m1&T(6), T(11&6), True);
  check (m1|T(6), T(11|6), True);
  check (m1^T(6), T(11^6), True);
  check (T(5)%m2, T(5%7), True, True);
  check (T(5)&m2, T(5&7), True, True);
  check (T(5)|m2, T(5|7), True, True);
  check (T(5)^m2, T(5^7), True, True);
  check (~m1, T(~11), True);
}

void doTestBool()
{
  // Test operators only useful for bool.
  MArray<Bool> m1 (Vector<Bool>(2,True), Vector<Bool>(2,True));
  MArray<Bool> m2 (Vector<Bool>(2,False));
  check (m1||m2, True, True);
  check (m1&&m2, False, True);
  check (m1||True, True, True);
  check (m1&&True, True, True);
  check (False||m2, False, True, True);
  check (False&&m2, False, True, True);
}

template <typename T> void doTestFloat()
{
  // Test all functions for real values.
  MArray<T> m1 (Vector<T>(2,0.7), Vector<Bool>(2,False));
  MArray<T> m2 (Vector<T>(2,0.3));
  checkNear (sqrt(m1), T(sqrt(0.7)), False); 
  checkNear (square(m1), T(square(0.7)), False); 
  checkNear (cube(m1), T(cube(0.7)), False); 
  checkNear (sin(m1), T(sin(0.7)), False); 
  checkNear (cos(m1), T(cos(0.7)), False); 
  checkNear (tan(m1), T(tan(0.7)), False); 
  checkNear (asin(m1), T(asin(0.7)), False); 
  checkNear (acos(m1), T(acos(0.7)), False); 
  checkNear (atan(m1), T(atan(0.7)), False); 
  checkNear (sinh(m1), T(sinh(0.7)), False); 
  checkNear (cosh(m1), T(cosh(0.7)), False); 
  checkNear (tanh(m1), T(tanh(0.7)), False); 
  checkNear (exp(m1), T(exp(0.7)), False); 
  checkNear (log(m1), T(log(0.7)), False); 
  checkNear (log10(m1), T(log10(0.7)), False); 
  checkNear (abs(m1), T(abs(0.7)), False); 
  checkNear (sign(m1), T(sign(0.7)), False); 
  checkNear (round(m1), T(round(0.7)), False); 
  checkNear (floor(m1), T(floor(0.7)), False); 
  checkNear (ceil(m1), T(ceil(0.7)), False); 
  checkNear (atan2(m2,m1), T(atan2(0.3,0.7)), False); 
  checkNear (atan2(m2,T(5)), T(atan2(0.3,5.)), False, True); 
  checkNear (atan2(T(6),m1), T(atan2(6.,0.7)), False); 
  checkNear (pow(m2,m1), T(pow(0.3,0.7)), False); 
  checkNear (pow(m2,5.), T(pow(0.3,5.)), False, True); 
  checkNear (pow(T(6),m1), T(pow(6.,0.7)), False); 
  checkNear (fmod(m2,m1), T(fmod(0.3,0.7)), False); 
  checkNear (fmod(m2,T(5)), T(fmod(0.3,5.)), False, True); 
  checkNear (fmod(T(6),m1), T(fmod(6.,0.7)), False); 
  check (near(m1,m2,1e-5), near(T(0.7),T(0.3),1e-5), False);
  check (nearAbs(m1,m2,1e-5), nearAbs(T(0.7),T(0.3),1e-5), False);
  check (near(m1,T(4),1e-5), near(T(0.7),T(4),1e-5), False);
  check (nearAbs(m1,T(4),1e-5), nearAbs(T(0.7),T(4),1e-5), False);
  check (near(T(4),m2,1e-5), near(T(4),T(0.3),1e-5), False, True);
  check (nearAbs(T(4),m2,1e-5), nearAbs(T(4),T(0.3),1e-5), False, True);
  check (isNaN(m2), False, False, True);
  check (isInf(m1), False, False);
  check (isFinite(m2), True, False, True);
}

template <typename T> void doTestComplex()
{
  // Test functions for complex values.
  T val(0.7, -0.3);
  MArray<T> m1 (Vector<T>(2,val));
  for (int i=0; i<2; ++i) {
    checkNear (sqrt(m1), T(sqrt(val)), False, i==0);
    checkNear (square(m1), T(square(val)), False, i==0);
    checkNear (sin(m1), T(sin(val)), False, i==0);
    checkNear (cos(m1), T(cos(val)), False, i==0);
    checkNear (tan(m1), T(tan(val)), False, i==0);
    checkNear (asin(m1), T(asin(val)), False, i==0);
    checkNear (acos(m1), T(acos(val)), False, i==0);
    checkNear (atan(m1), T(atan(val)), False, i==0);
    checkNear (sinh(m1), T(sinh(val)), False, i==0);
    checkNear (cosh(m1), T(cosh(val)), False, i==0);
    checkNear (tanh(m1), T(tanh(val)), False, i==0);
    checkNear (conj(m1), conj(val), False, i==0);
    checkNear (real(m1), real(val), False, i==0);
    checkNear (imag(m1), imag(val), False, i==0);
    checkNear (amplitude(m1), abs(val), False, i==0);
    checkNear (phase(m1), arg(val), False, i==0);
    m1.setMask (Vector<Bool>(2,False));
  }
}

template<typename T> void doTestComplexReal()
{
  // Test pow function for mix of complex array and real scalar.
  T val(1.7);
  std::complex<T> valc(1,2);
  MArray<std::complex<T>> m1 (Vector<std::complex<T>>(2, valc));
  for (int i=0; i<2; ++i) {
    checkNear (pow(m1,val), std::pow(valc,val), False, i==0);
    m1.setMask (Vector<Bool>(2,False));
  }
}

void doTestMixed()
{
  // Test masking more thoroughly.
  Vector<Bool> m1(4);  m1[0]=False;  m1[1]=True; m1[2]=True; m1[3]=False;
  Vector<Bool> m2(4);  m2[0]=True; m2[1]=False;  m2[2]=True; m2[3]=False;
  Vector<Int>  v1(4);  v1[0]=3;  v1[1]=5; v1[2]=-2; v1[3]=-5;
  Vector<Int>  v2(4);  v2[0]=-4; v2[1]=8; v2[2]=9;  v2[3]=-3;
  MArray<Int> ma1(v1, m1);
  MArray<Int> ma2(v2, m2);
  MArray<Int> ma3 = ma1+ma2;
  Vector<Bool> m3(ma3.mask());
  Vector<Int>  v3(ma3.array());
  AlwaysAssertExit (m3[0] && m3[1] && m3[2] && !m3[3]);
  AlwaysAssertExit (v3[0]==-1 && v3[1]==13 && v3[2]==7 && v3[3]==-8);
  AlwaysAssertExit (sum(ma1) == -2);
  AlwaysAssertExit (sum(ma2) == 5);
  AlwaysAssertExit (sumsqr(ma1) == 34);
  AlwaysAssertExit (sumsqr(ma2) == 73);
  AlwaysAssertExit (product(ma1) == -15);
  AlwaysAssertExit (product(ma2) == -24);
  AlwaysAssertExit (min(ma1) == -5);
  AlwaysAssertExit (min(ma2) == -3);
  AlwaysAssertExit (max(ma1) == 3);
  AlwaysAssertExit (max(ma2) == 8);
}

void doTestReduce()
{
  // Test the full reduction functions.
  Vector<Double> v(20);
  indgen(v);
  Vector<Bool> m(20, False);
  MArray<Double> ma(v, m);
  Double mn = mean(ma);
  AlwaysAssertExit (near(mean(ma), mean(v)));
  AlwaysAssertExit (near(variance(ma, 1), variance(v)));
  AlwaysAssertExit (near(variance(ma, mn, 1), variance(v, mn)));
  AlwaysAssertExit (near(stddev(ma, 1), stddev(v)));
  AlwaysAssertExit (near(stddev(ma, mn, 1), stddev(v, mn)));
  AlwaysAssertExit (near(avdev(ma), avdev(v)));
  AlwaysAssertExit (near(avdev(ma, mn), avdev(v, mn)));
  AlwaysAssertExit (near(rms(ma), rms(v)));
  AlwaysAssertExit (near(median(ma), median(v)));
  AlwaysAssertExit (near(fractile(ma, 0.4), fractile(v, 0.4)));
  Vector<Double> vec = ma.flatten();
  AlwaysAssertExit (allEQ(v, vec));
  m[0] = m[18] = True;
  ma.setMask (m);
  Vector<Double> v1(18);
  indgen (v1, 1.);
  v1[17] = 19;
  mn = mean(ma);
  AlwaysAssertExit (near(mean(ma), mean(v1)));
  AlwaysAssertExit (near(variance(ma, 1), variance(v1)));
  AlwaysAssertExit (near(variance(ma, mn, 1), variance(v1, mn)));
  AlwaysAssertExit (near(stddev(ma, 1), stddev(v1)));
  AlwaysAssertExit (near(stddev(ma, mn, 1), stddev(v1, mn)));
  AlwaysAssertExit (near(avdev(ma), avdev(v1)));
  AlwaysAssertExit (near(avdev(ma, mn), avdev(v1, mn)));
  AlwaysAssertExit (near(rms(ma), rms(v1)));
  AlwaysAssertExit (near(median(ma), median(v1)));
  AlwaysAssertExit (near(fractile(ma, 0.4), fractile(v1, 0.4)));
  Vector<Double> vec1 = ma.flatten();
  AlwaysAssertExit (allEQ(v1, vec1));
}

void doTestPartial()
{
  // Test the partial reduction functions.
  Cube<Int> arr(2,3,4);
  Cube<Bool> mask(2,3,4);
  arr  = 1;
  mask = False;
  arr(1,1,1) = 101;
  // First do MArray tests without a a mask.
  // The result must be equal to the Array counterpart.
  AlwaysAssertExit (allEQ(partialSums(MArray<Int>(arr),
                                    IPosition(1,0)).array(),
                          partialSums (arr, IPosition(1,0))));
  AlwaysAssertExit (allEQ(partialSums(MArray<Int>(arr),
                                    IPosition(1,1)).array(),
                          partialSums (arr, IPosition(1,1))));
  AlwaysAssertExit (allEQ(partialSums(MArray<Int>(arr),
                                    IPosition(1,2)).array(),
                          partialSums (arr, IPosition(1,2))));
  AlwaysAssertExit (allEQ(partialSums(MArray<Int>(arr,mask),
                                    IPosition(2,0,1)).array(),
                          partialSums(arr, IPosition(2,0,1))));
  AlwaysAssertExit (allEQ(partialSums(MArray<Int>(arr,mask),
                                    IPosition(2,0,2)).array(),
                          partialSums(arr, IPosition(2,0,2))));
  AlwaysAssertExit (allEQ(partialSums(MArray<Int>(arr,mask),
                                    IPosition(2,1,2)).array(),
                          partialSums(arr, IPosition(2,1,2))));
  AlwaysAssertExit (allEQ(partialSums(MArray<Int>(arr,mask),
                                    IPosition(3,0,1,2)).array(),
                          partialSums(arr, IPosition(3,0,1,2))));
  AlwaysAssertExit (allEQ(partialSumSqrs(MArray<Int>(arr),
                                         IPosition(1,1)).array(),
                          partialSumSqrs (arr, IPosition(1,1))));
  AlwaysAssertExit (allEQ(partialProducts(MArray<Int>(arr),
                                           IPosition(1,1)).array(),
                          partialProducts (arr, IPosition(1,1))));
  AlwaysAssertExit (allEQ(partialMins(MArray<Int>(arr),
                                    IPosition(1,1)).array(),
                          partialMins (arr, IPosition(1,1))));
  AlwaysAssertExit (allEQ(partialMaxs(MArray<Int>(arr),
                                    IPosition(1,1)).array(),
                          partialMaxs (arr, IPosition(1,1))));
  AlwaysAssertExit (allEQ(partialMeans(MArray<Int>(arr),
                                       IPosition(1,1)).array(),
                          partialMeans (arr, IPosition(1,1))));
  AlwaysAssertExit (allEQ(partialVariances(MArray<Int>(arr),
                                           IPosition(1,1), 1).array(),
                          partialVariances (arr, IPosition(1,1))));
  AlwaysAssertExit (allEQ(partialStddevs(MArray<Int>(arr),
                                         IPosition(1,1), 1).array(),
                          partialStddevs (arr, IPosition(1,1), 1)));
  AlwaysAssertExit (allEQ(partialAvdevs(MArray<Int>(arr),
                                        IPosition(1,1)).array(),
                          partialAvdevs (arr, IPosition(1,1))));
  AlwaysAssertExit (allEQ(partialRmss(MArray<Int>(arr),
                                      IPosition(1,1)).array(),
                          partialRmss (arr, IPosition(1,1))));
  AlwaysAssertExit (allEQ(partialMedians(MArray<Int>(arr),
                                         IPosition(1,1)).array(),
                          partialMedians (arr, IPosition(1,1))));
  AlwaysAssertExit (allEQ(partialFractiles(MArray<Int>(arr),
                                           IPosition(1,1), 0.6).array(),
                          partialFractiles (arr, IPosition(1,1), 0.6)));
  // Now do MArray tests with a mask.
  mask(0,2,3) = mask(1,2,3) = mask(0,1,2) = True;
  Matrix<Int> ares(3,4);
  ares = 2; ares(1,1) = 102; ares(1,2) = 1; ares(2,3) = 0;
  Matrix<Bool> mres(3,4);
  mres = False; mres(2,3) = True;
  MArray<Int> ma(partialSums(MArray<Int>(arr,mask), IPosition(1,0)));
  AlwaysAssertExit (allEQ(ma.array(), ares)  &&  allEQ(ma.mask(), mres));
  ares = 2; ares(1,1) = 1+101*101; ares(1,2) = 1; ares(2,3) = 0;
  ma = partialSumSqrs(MArray<Int>(arr,mask), IPosition(1,0));
  AlwaysAssertExit (allEQ(ma.array(), ares)  &&  allEQ(ma.mask(), mres));
  ares = 1; ares(2,3) = 0;
  ma = partialMins(MArray<Int>(arr,mask), IPosition(1,0));
  AlwaysAssertExit (allEQ(ma.array(), ares)  &&  allEQ(ma.mask(), mres));
  ares = 1; ares(1,1) = 101; ares(2,3) = 0;
  ma = partialMaxs(MArray<Int>(arr,mask), IPosition(1,0));
  AlwaysAssertExit (allEQ(ma.array(), ares)  &&  allEQ(ma.mask(), mres));
  ares = 1; ares(1,1) = 51; ares(2,3) = 0;
  ma = partialMeans(MArray<Int>(arr,mask), IPosition(1,0));
  AlwaysAssertExit (allEQ(ma.array(), ares)  &&  allEQ(ma.mask(), mres));
  ares = 1; ares(1,1) = 51; ares(2,3) = 0;
  ma = partialMedians(MArray<Int>(arr,mask), IPosition(1,0), True);
  AlwaysAssertExit (allEQ(ma.array(), ares)  &&  allEQ(ma.mask(), mres));
  ares(1,1) = 1;
  ma = partialFractiles(MArray<Int>(arr,mask), IPosition(1,0), 0.5);
  AlwaysAssertExit (allEQ(ma.array(), ares)  &&  allEQ(ma.mask(), mres));
  // Do other tests with doubles.
  Array<double> arrd(arr.shape());
  convertArray (arrd, arr);
  Matrix<double> aresd(3,4);
  MArray<double> mad;
  aresd = 0.;
  aresd(1,1) = 5000.;
  mad = partialVariances(MArray<double>(arrd,mask), IPosition(1,0), 1);
  AlwaysAssertExit (allNear(mad.array(), aresd, 1e-5) && allEQ(mad.mask(), mres));
  aresd(1,1) = 70.7107;
  mad = partialStddevs(MArray<double>(arrd,mask), IPosition(1,0), 1);
  AlwaysAssertExit (allNear(mad.array(), aresd, 1e-5) && allEQ(mad.mask(), mres));
  aresd(1,1) = 50.;
  mad = partialAvdevs(MArray<double>(arrd,mask), IPosition(1,0));
  AlwaysAssertExit (allNear(mad.array(), aresd, 1e-5) && allEQ(mad.mask(), mres));
  aresd = 1.; aresd(1,1) = 71.4213; aresd(2,3) = 0.;
  mad = partialRmss(MArray<double>(arrd,mask), IPosition(1,0));
  AlwaysAssertExit (allNear(mad.array(), aresd, 1e-5) && allEQ(mad.mask(), mres));
}

void doTestBoxed()
{
  // Test the boxed reduction functions.
  Cube<Int> arr(2,3,4);
  Cube<Bool> mask(2,3,4);
  arr  = 1;
  mask = False;
  arr(1,1,1) = 101;
  AlwaysAssertExit (allEQ(boxedSums(MArray<Int>(arr),
                                    IPosition(1,1)).array(),
                          boxedArrayMath (arr, IPosition(1,1),
                                          SumFunc<Int>())));
  AlwaysAssertExit (allEQ(boxedSums(MArray<Int>(arr),
                                    IPosition(1,2)).array(),
                          boxedArrayMath (arr, IPosition(1,2),
                                          SumFunc<Int>())));
  AlwaysAssertExit (allEQ(boxedSums(MArray<Int>(arr),
                                    IPosition(2,2,2,2)).array(),
                          boxedArrayMath (arr, IPosition(2,2,2,2),
                                          SumFunc<Int>())));
  AlwaysAssertExit (allEQ(boxedSumSqrs(MArray<Int>(arr),
                                       IPosition(2,2,2,2)).array(),
                          boxedArrayMath (arr, IPosition(2,2,2,2),
                                          SumSqrFunc<Int>())));
  AlwaysAssertExit (allEQ(boxedProducts(MArray<Int>(arr),
                                        IPosition(2,2,2,2)).array(),
                          boxedArrayMath (arr, IPosition(2,2,2,2),
                                          ProductFunc<Int>())));
  AlwaysAssertExit (allEQ(boxedMins(MArray<Int>(arr),
                                    IPosition(2,2,2,2)).array(),
                          boxedArrayMath (arr, IPosition(2,2,2,2),
                                          MinFunc<Int>())));
  AlwaysAssertExit (allEQ(boxedMaxs(MArray<Int>(arr),
                                    IPosition(2,2,2,2)).array(),
                          boxedArrayMath (arr, IPosition(2,2,2,2),
                                          MaxFunc<Int>())));
  AlwaysAssertExit (allEQ(boxedMeans(MArray<Int>(arr),
                                     IPosition(2,2,2,2)).array(),
                          boxedArrayMath (arr, IPosition(2,2,2,2),
                                          MeanFunc<Int>())));
  AlwaysAssertExit (allEQ(boxedVariances(MArray<Int>(arr),
                                         IPosition(2,2,2,2), 1).array(),
                          boxedArrayMath (arr, IPosition(2,2,2,2),
                                          VarianceFunc<Int>(1))));
  AlwaysAssertExit (allEQ(boxedStddevs(MArray<Int>(arr),
                                       IPosition(2,2,2,2), 1).array(),
                          boxedArrayMath (arr, IPosition(2,2,2,2),
                                          StddevFunc<Int>(1))));
  AlwaysAssertExit (allEQ(boxedAvdevs(MArray<Int>(arr),
                                      IPosition(2,2,2,2)).array(),
                          boxedArrayMath (arr, IPosition(2,2,2,2),
                                          AvdevFunc<Int>())));
  AlwaysAssertExit (allEQ(boxedRmss(MArray<Int>(arr),
                                    IPosition(2,2,2,2)).array(),
                          boxedArrayMath (arr, IPosition(2,2,2,2),
                                          RmsFunc<Int>())));
  AlwaysAssertExit (allEQ(boxedMedians(MArray<Int>(arr),
                                       IPosition(2,2,2,2)).array(),
                          boxedArrayMath (arr, IPosition(2,2,2,2),
                                          MedianFunc<Int>())));
  AlwaysAssertExit (allEQ(boxedFractiles(MArray<Int>(arr),
                                         IPosition(2,2,2,2), 0.6).array(),
                          boxedArrayMath (arr, IPosition(2,2,2,2),
                                          FractileFunc<Int>(0.6))));
  // Now do MArray tests with a mask.
  mask(0,2,3) = mask(1,2,3) = mask(0,1,2) = True;
  Cube<Int> ares(1,3,4);
  ares = 2; ares(0,1,1) = 102; ares(0,1,2) = 1; ares(0,2,3) = 0;
  Cube<Bool> mres(1,3,4);
  mres = False; mres(0,2,3) = True;

  MArray<Int> ma(boxedSums(MArray<Int>(arr,mask), IPosition(1,2)));
  AlwaysAssertExit (allEQ(ma.array(), ares)  &&  allEQ(ma.mask(), mres));
  ares = 2; ares(0,1,1) = 1+101*101; ares(0,1,2) = 1; ares(0,2,3) = 0;

  ma = boxedSumSqrs(MArray<Int>(arr,mask), IPosition(1,2));
  AlwaysAssertExit (allEQ(ma.array(), ares)  &&  allEQ(ma.mask(), mres));
  ares = 1; ares(0,2,3) = 0;

  ma = boxedMins(MArray<Int>(arr,mask), IPosition(1,2));
  AlwaysAssertExit (allEQ(ma.array(), ares)  &&  allEQ(ma.mask(), mres));
  ares = 1; ares(0,1,1) = 101; ares(0,2,3) = 0;

  ma = boxedMaxs(MArray<Int>(arr,mask), IPosition(1,2));
  AlwaysAssertExit (allEQ(ma.array(), ares)  &&  allEQ(ma.mask(), mres));
  ares = 1; ares(0,1,1) = 51; ares(0,2,3) = 0;

  ma = boxedMeans(MArray<Int>(arr,mask), IPosition(1,2));
  AlwaysAssertExit (allEQ(ma.array(), ares)  &&  allEQ(ma.mask(), mres));
  ares = 1; ares(0,1,1) = 51; ares(0,2,3) = 0;

  ma = boxedMedians(MArray<Int>(arr,mask), IPosition(1,2), True);
  AlwaysAssertExit (allEQ(ma.array(), ares)  &&  allEQ(ma.mask(), mres));
  ares(0,1,1) = 1;

  ma = boxedFractiles(MArray<Int>(arr,mask), IPosition(1,2), 0.5);
  AlwaysAssertExit (allEQ(ma.array(), ares)  &&  allEQ(ma.mask(), mres));

  // Do other tests with doubles.
  Array<double> arrd(arr.shape());
  convertArray (arrd, arr);
  Cube<double> aresd(1,3,4);
  MArray<double> mad;
  aresd = 0.;
  aresd(0,1,1) = 5000.;

  mad = boxedVariances(MArray<double>(arrd,mask), IPosition(1,2), 1);
  AlwaysAssertExit (allNear(mad.array(), aresd, 1e-5) && allEQ(mad.mask(), mres));
  aresd(0,1,1) = 70.7107;

  mad = boxedStddevs(MArray<double>(arrd,mask), IPosition(1,2), 1);
  AlwaysAssertExit (allNear(mad.array(), aresd, 1e-5) && allEQ(mad.mask(), mres));
  aresd(0,1,1) = 50.;

  mad = boxedAvdevs(MArray<double>(arrd,mask), IPosition(1,2));
  AlwaysAssertExit (allNear(mad.array(), aresd, 1e-5) && allEQ(mad.mask(), mres));
  aresd = 1.; aresd(0,1,1) = 71.4213; aresd(0,2,3) = 0.;
  mad = boxedRmss(MArray<double>(arrd,mask), IPosition(1,2));
  AlwaysAssertExit (allNear(mad.array(), aresd, 1e-5) && allEQ(mad.mask(), mres));
}

void doTestSliding()
{
  // Test the sliding reduction functions.
  Cube<Int> arr(4,5,6);
  Cube<Bool> mask(4,5,6);
  indgen (arr);
  mask = False;
  // An empty box results in the array itself.
  AlwaysAssertExit (allEQ (slidingSums(MArray<Int>(arr,mask),
                                       IPosition(), False).array(),
                           arr));
  // But with a mask the result is 0.
  mask = True;
  MArray<Int> ma1 = slidingSums(MArray<Int>(arr,mask),
                                IPosition(), False);
  AlwaysAssertExit (allEQ (ma1.array(), 0));
  AlwaysAssertExit (allEQ (ma1.mask(), mask));
  // Test without filling the edge (with and without mask).
  mask = False;
  MArray<Int> a1(slidingSums(MArray<Int>(arr,mask),
                             IPosition(2,1,2), False));
  Array<Int> a2 (slidingArrayMath (arr, IPosition(2,1,2),
                                   SumFunc<Int>(), False));
  AlwaysAssertExit (a1.shape() == IPosition(3,2,1,6));
  AlwaysAssertExit (allEQ(a1.array(), a2));
  AlwaysAssertExit (allEQ(a1.mask(), False));
  // Test with filling the edge (with and without mask).
  a1.reference (slidingSums(MArray<Int>(arr,mask),
                            IPosition(2,1,2), True));
  a2.reference (slidingArrayMath (arr, IPosition(2,1,2),
                                  SumFunc<Int>(), True));
  AlwaysAssertExit (a1.shape() == IPosition(3,4,5,6));
  AlwaysAssertExit (allEQ(a1.array(), a2));
  Cube<Bool> expMask(4,5,6);
  expMask = True;
  expMask(IPosition(3,1,2,0), IPosition(3,2,2,5)) = False;
  AlwaysAssertExit (allEQ(a1.mask(), expMask));
  // Test with some mask bits set.
  mask(0,2,3) = mask(1,2,3) = mask(2,2,3) = mask(1,1,2) = True;
  Cube<Int> ares = slidingSums(MArray<Int>(arr),
                               IPosition(1,1), False).array();
  ares(0,2,3) = 0; ares(1,2,3) = 71;
  ares(0,1,2) = 44+46; ares(1,1,2) = 46+47;
  Cube<Bool> mres(2,5,6);
  mres = False; mres(0,2,3) = True;
  MArray<Int> ma(slidingSums(MArray<Int>(arr,mask), IPosition(1,1), False));
  AlwaysAssertExit (allEQ(ma.array(), ares)  &&  allEQ(ma.mask(), mres));
  // Now test the various functions (with and without a mask).
  ares = slidingArrayMath(arr, IPosition(1,1),
                          SumSqrFunc<Int>(), False);
  AlwaysAssertExit (allEQ(slidingSumSqrs(MArray<Int>(arr), IPosition(1,1),
                                         False).array(),
                          ares));
  ares(0,2,3) = 0; ares(1,2,3) = 71*71;
  ares(0,1,2) = 44*44+46*46; ares(1,1,2) = 46*46+47*47;
  ma = slidingSumSqrs(MArray<Int>(arr,mask), IPosition(1,1), False);
  AlwaysAssertExit (allEQ(ma.array(), ares)  &&  allEQ(ma.mask(), mres));

  ares = slidingArrayMath(arr, IPosition(1,1),
                          ProductFunc<Int>(), False);
  AlwaysAssertExit (allEQ(slidingProducts(MArray<Int>(arr), IPosition(1,1),
                                          False).array(),
                          ares));
  ares(0,2,3) = 0; ares(1,2,3) = 71;
  ares(0,1,2) = 44*46; ares(1,1,2) = 46*47;
  ma = slidingProducts(MArray<Int>(arr,mask), IPosition(1,1), False);
  AlwaysAssertExit (allEQ(ma.array(), ares)  &&  allEQ(ma.mask(), mres));

  ares = slidingArrayMath(arr, IPosition(1,1),
                          MinFunc<Int>(), False);
  AlwaysAssertExit (allEQ(slidingMins(MArray<Int>(arr), IPosition(1,1),
                                      False).array(),
                          ares));
  ares(0,2,3) = 0; ares(1,2,3) = 71;
  ares(0,1,2) = 44; ares(1,1,2) = 46;
  ma = slidingMins(MArray<Int>(arr,mask), IPosition(1,1), False);
  AlwaysAssertExit (allEQ(ma.array(), ares)  &&  allEQ(ma.mask(), mres));

  ares = slidingArrayMath(arr, IPosition(1,1),
                          MaxFunc<Int>(), False);
  AlwaysAssertExit (allEQ(slidingMaxs(MArray<Int>(arr), IPosition(1,1),
                                      False).array(),
                          ares));
  ares(0,2,3) = 0; ares(1,2,3) = 71;
  ares(0,1,2) = 46; ares(1,1,2) = 47;
  ma = slidingMaxs(MArray<Int>(arr,mask), IPosition(1,1), False);
  AlwaysAssertExit (allEQ(ma.array(), ares)  &&  allEQ(ma.mask(), mres));

  ares = slidingArrayMath(arr, IPosition(1,1),
                          MeanFunc<Int>(), False);
  AlwaysAssertExit (allEQ(slidingMeans(MArray<Int>(arr), IPosition(1,1),
                                       False).array(),
                          ares));
  ares(0,2,3) = 0; ares(1,2,3) = 71;
  ares(0,1,2) = 45; ares(1,1,2) = 46;
  ma = slidingMeans(MArray<Int>(arr,mask), IPosition(1,1), False);
  AlwaysAssertExit (allEQ(ma.array(), ares)  &&  allEQ(ma.mask(), mres));

  // Do other tests with doubles.
  Array<double> arrd(arr.shape());
  convertArray (arrd, arr);
  Cube<double> aresd(2,5,6);
  MArray<double> mad;
  aresd = slidingArrayMath(arrd, IPosition(1,1),
                           VarianceFunc<double>(1), False);
  AlwaysAssertExit (allNear(slidingVariances(MArray<double>(arrd), IPosition(1,1),
                                             1, False).array(),
                          aresd, 1e-5));
  aresd(0,2,3) = 0; aresd(1,2,3) = 0;
  aresd(0,1,2) = 2; aresd(1,1,2) = 0.5;
  mad = slidingVariances(MArray<double>(arrd,mask), IPosition(1,1), 1, False);
  AlwaysAssertExit (allNear(mad.array(), aresd, 1e-5)  &&
                    allEQ(mad.mask(), mres));

  aresd = slidingArrayMath(arrd, IPosition(1,1),
                           StddevFunc<double>(1), False);
  AlwaysAssertExit (allNear(slidingStddevs(MArray<double>(arrd), IPosition(1,1),
                                           1, False).array(),
                            aresd, 1e-5));
  aresd(0,2,3) = 0; aresd(1,2,3) = 0;
  aresd(0,1,2) = sqrt(2.); aresd(1,1,2) = sqrt(0.5);
  mad = slidingStddevs(MArray<double>(arrd,mask), IPosition(1,1), 1, False);
  AlwaysAssertExit (allNear(mad.array(), aresd, 1e-5)  &&
                    allEQ(mad.mask(), mres));

  aresd = slidingArrayMath(arrd, IPosition(1,1),
                           AvdevFunc<double>(), False);
  AlwaysAssertExit (allNear(slidingAvdevs(MArray<double>(arrd), IPosition(1,1),
                                        False).array(),
                            aresd, 1e-5));
  aresd(0,2,3) = 0; aresd(1,2,3) = 0;
  aresd(0,1,2) = 1; aresd(1,1,2) = 0.5;
  mad = slidingAvdevs(MArray<double>(arrd,mask), IPosition(1,1), False);
  AlwaysAssertExit (allNear(mad.array(), aresd, 1e-5)  &&
                    allEQ(mad.mask(), mres));

  aresd = slidingArrayMath(arrd, IPosition(1,1),
                           RmsFunc<double>(), False);
  AlwaysAssertExit (allNear(slidingRmss(MArray<double>(arrd), IPosition(1,1),
                                      False).array(),
                            aresd, 1e-5));
  aresd(0,2,3) = 0; aresd(1,2,3) = 71;
  aresd(0,1,2) = 45.01110974; aresd(1,1,2) = 46.5026881;
  mad = slidingRmss(MArray<double>(arrd,mask), IPosition(1,1), False);
  AlwaysAssertExit (allNear(mad.array(), aresd, 1e-5)  &&
                    allEQ(mad.mask(), mres));

  aresd = slidingArrayMath(arrd, IPosition(1,1),
                           MedianFunc<double>(False, True), False);
  AlwaysAssertExit (allNear(slidingMedians(MArray<double>(arrd), IPosition(1,1),
                                           True, False, False).array(),
                            aresd, 1e-5));
  aresd(0,2,3) = 0; aresd(1,2,3) = 71;
  aresd(0,1,2) = 45; aresd(1,1,2) = 46.5;
  mad = slidingMedians(MArray<double>(arrd,mask), IPosition(1,1),
                       True, False, False);
  AlwaysAssertExit (allNear(mad.array(), aresd, 1e-5)  &&
                    allEQ(mad.mask(), mres));

  aresd = slidingArrayMath(arrd, IPosition(1,1),
                           FractileFunc<double>(0.6), False);
  AlwaysAssertExit (allNear(slidingFractiles(MArray<double>(arrd), IPosition(1,1),
                                             0.6, False, False).array(),
                            aresd, 1e-5));
  aresd(0,2,3) = 0; aresd(1,2,3) = 71;
  aresd(0,1,2) = 44; aresd(1,1,2) = 46;
  mad = slidingFractiles(MArray<double>(arrd,mask), IPosition(1,1),
                         0.6, False, False);
  AlwaysAssertExit (allNear(mad.array(), aresd, 1e-5)  &&
                    allEQ(mad.mask(), mres));
}

void doTestNull()
{
  MArray<Int> m1 (Vector<Int>(2,16), Vector<Bool>(2,True));
  MArray<Int> m2;
  AlwaysAssertExit ((m1+m2).isNull());
  AlwaysAssertExit ((m2-m1).isNull());
  AlwaysAssertExit (sin(m2).isNull());
  AlwaysAssertExit ((-m2).isNull());
  AlwaysAssertExit (sum(m2) == 0);
  AlwaysAssertExit (partialSums(m2, IPosition()).isNull());
  AlwaysAssertExit (boxedMedians(m2, IPosition()).isNull());
  AlwaysAssertExit (slidingMeans(m2, IPosition()).isNull());
}

void doPerf()
{
  // Do a bit of performance testing.
  Cube<Int> a(200,200,200);
  a = 1;
  Timer timer;
  partialArrayMath(a, IPosition(1,1), SumFunc<Int>());
  timer.show("unmasked");
  Cube<Bool> mask(200,200,200);
  mask = False;
  timer.mark();
  partialSums (MArray<Int>(a,mask), IPosition(1,1));
  timer.show("masked  ");
  timer.mark();
  partialSums (a, IPosition(1,1));
  timer.show("sums unm");
}

int main()
{
  try {
    cout << "doTestAll<int>" << endl;
    doTestAll<int>();
    cout << "doTestAll<double>" << endl;
    doTestAll<double>();
    cout << "doTestInt<int>" << endl;
    doTestInt<int>();
    cout << "doTestBool" << endl;
    doTestBool();
    cout << "doTestFloat<double>" << endl;
    doTestFloat<double>();
    cout << "doTestComplex<Complex>" << endl;
    doTestComplex<Complex>();
    cout << "doTestComplex<DComplex>" << endl;
    doTestComplex<DComplex>();
    cout << "doTestComplexReal<Float>" << endl;
    doTestComplexReal<Float>();
    cout << "doTestComplexReal<Double>" << endl;
    doTestComplexReal<Double>();
    cout << "doTestMixed" << endl;
    doTestMixed();
    cout << "doTestReduce" << endl;
    doTestReduce();
    cout << "doTestPartial" << endl;
    doTestPartial();
    cout << "doTestBoxed" << endl;
    doTestBoxed();
    cout << "doTestSliding" << endl;
    doTestSliding();
    cout << "doTestNull" << endl;
    doTestNull();
    cout << "doPerf" << endl;
    doPerf();
  } catch (std::exception& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  return 0;
}
