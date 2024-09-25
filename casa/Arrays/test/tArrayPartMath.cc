//# tArrayMath2.cc: This program tests the ArrayPartMath class
//# Copyright (C) 1993,1994,1995,1996,1997,1999,2000,2002,2003
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

//# Includes
#include "../Array.h"
#include "../Vector.h"
#include "../ArrayPartMath.h"
#include "../ArrayLogical.h"
#include "../ArrayStr.h"

#include <boost/test/unit_test.hpp>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(array_part_math)

typedef Array<double> PartFunc (const Array<double>&, const IPosition& axes);
typedef double FullFunc (const Array<double>&);

Array<double> myPartialVariances (const Array<double>& array,
				  const IPosition& axes)
{
  return partialVariances (array, axes, 1);
}
double myVariance (const Array<double>& array)
{
  return pvariance (array, 1);
}
Array<double> myPartialStddevs (const Array<double>& array,
                                const IPosition& axes)
{
  return partialStddevs (array, axes, 0);
}
double myStddev (const Array<double>& array)
{
  return pstddev (array, 0);
}
Array<double> myMeanPartialMedians (const Array<double>& array,
				    const IPosition& axes)
{
  return partialMedians (array, axes, true, false);
}
double myMeanMedian (const Array<double>& array)
{
  return median (array, false, true, false);
}
Array<double> myNomeanPartialMedians (const Array<double>& array,
				      const IPosition& axes)
{
  return partialMedians (array, axes, false, false);
}
double myNomeanMedian (const Array<double>& array)
{
  return median (array, false, false, false);
}
Array<double> myMeanPartialMadfms (const Array<double>& array,
                                   const IPosition& axes)
{
  return partialMadfms (array, axes, true, false);
}
double myMeanMadfm (const Array<double>& array)
{
  return madfm (array, false, true, false);
}
Array<double> myNomeanPartialMadfms (const Array<double>& array,
				      const IPosition& axes)
{
  return partialMadfms (array, axes, false, false);
}
double myNomeanMadfm (const Array<double>& array)
{
  return madfm (array, false, false, false);
}
Array<double> myPartialFractiles (const Array<double>& array,
				  const IPosition& axes)
{
  return partialFractiles (array, axes, 0.3, false);
}
double myFractile (const Array<double>& array)
{
  return fractile (array, 0.3, false, false);
}
Array<double> myPartialHexiles (const Array<double>& array,
                                const IPosition& axes)
{
  return partialInterHexileRanges (array, axes, false);
}
double myHexile (const Array<double>& array)
{
  return interHexileRange (array, false, false);
}
Array<double> myPartialQuartiles (const Array<double>& array,
                                  const IPosition& axes)
{
  return partialInterQuartileRanges (array, axes, false);
}
double myQuartile (const Array<double>& array)
{
  return interQuartileRange (array, false, false);
}


bool doIt1 (PartFunc* partFunc, FullFunc* fullFunc, bool doExtra)
{
  bool errFlag = false;
  IPosition shape(2,3,4);
  Array<double> arr(shape);
  indgen(arr);
  for (int j=0; j<2; j++) {
    Vector<double> res(shape(j));
    IPosition st(2,0);
    IPosition end(shape-1);
    for (int i=0; i<shape(j); i++) {
      st(j) = i;
      end(j) = i;
      res(i) = fullFunc(arr(st,end));
    }
    Array<double> res2 = partFunc (arr, IPosition(1,1-j));
    if (! allNear (res, res2, 1.e-5)) {
      errFlag = true;
    }
  }
  if (doExtra) {
    {
      Array<double> res2 = partFunc (arr, IPosition());
      if (! allNear (arr, res2, 1.e-5)) {
        errFlag = true;
      }
    }
    {
      Array<double> res2 = partFunc (arr, IPosition(2,0,1));
      Vector<double> res(1, fullFunc(arr));
      if (! allEQ (res, res2)) {
        errFlag = true;
      }
    }
  }
  return !errFlag;
}

bool doIt2 (PartFunc* partFunc, FullFunc* fullFunc)
{
  bool errFlag = false;
  IPosition shape(3,3,4,5);
  Array<double> arr(shape);
  indgen(arr);
  for (int j=0; j<3; j++) {
    Vector<double> res(shape(j));
    IPosition st(3,0);
    IPosition end(shape-1);
    for (int i=0; i<shape(j); i++) {
      st(j) = i;
      end(j) = i;
      res(i) = fullFunc(arr(st,end));
    }
    Array<double> res2 = partFunc (arr,
                                    IPosition::otherAxes(3, IPosition(1,j)));
    if (! allNear (res, res2, 1.e-5)) {
      errFlag = true;
    }
  }
  for (int j=0; j<3; j++) {
    for (int k=j+1; k<3; k++) {
      IPosition resshape(2,shape(j),shape(k));
      Array<double> res(resshape);
      IPosition st(3,0);
      IPosition end(shape-1);
      for (int i0=0; i0<shape(j); i0++) {
        st(j) = i0;
        end(j) = i0;
        for (int i1=0; i1<shape(k); i1++) {
          st(k) = i1;
          end(k) = i1;
          res(IPosition(2,i0,i1)) = fullFunc(arr(st,end));
        }
      }
      Array<double> res2 = partFunc (arr,
                                      IPosition::otherAxes(3, IPosition(2,j,k)));
      if (! allNear (res, res2, 1.e-5)) {
        errFlag = true;
      }
    }
  }
  return !errFlag;
}

bool doIt3 (PartFunc* partFunc, FullFunc* fullFunc)
{
  bool errFlag = false;
  IPosition shape(4,3,4,5,6);
  Array<double> arr(shape);
  indgen(arr);
  for (int j=0; j<4; j++) {
    Vector<double> res(shape(j));
    IPosition st(4,0);
    IPosition end(shape-1);
    for (int i=0; i<shape(j); i++) {
      st(j) = i;
      end(j) = i;
      res(i) = fullFunc(arr(st,end));
    }
    Array<double> res2 = partFunc (arr,
                                    IPosition::otherAxes(4, IPosition(1,j)));
    if (! allNear (res, res2, 1.e-5)) {
      errFlag = true;
    }
  }
  for (int j=0; j<4; j++) {
    for (int k=j+1; k<4; k++) {
      IPosition resshape(2,shape(j),shape(k));
      Array<double> res(resshape);
      IPosition st(4,0);
      IPosition end(shape-1);
      for (int i0=0; i0<shape(j); i0++) {
        st(j) = i0;
        end(j) = i0;
        for (int i1=0; i1<shape(k); i1++) {
          st(k) = i1;
          end(k) = i1;
          res(IPosition(2,i0,i1)) = fullFunc(arr(st,end));
        }
      }
      Array<double> res2 = partFunc (arr,
                                      IPosition::otherAxes(4, IPosition(2,j,k)));
      if (! allNear (res, res2, 1.e-5)) {
        errFlag = true;
      }
    }
  }
  for (int j0=0; j0<4; j0++) {
    for (int j1=j0+1; j1<4; j1++) {
      for (int j2=j1+1; j2<4; j2++) {
        IPosition resshape(3,shape(j0),shape(j1),shape(j2));
        Array<double> res(resshape);
        IPosition st(4,0);
        IPosition end(shape-1);
        for (int i0=0; i0<shape(j0); i0++) {
          st(j0) = i0;
          end(j0) = i0;
          for (int i1=0; i1<shape(j1); i1++) {
            st(j1) = i1;
            end(j1) = i1;
            for (int i2=0; i2<shape(j2); i2++) {
              st(j2) = i2;
              end(j2) = i2;
              res(IPosition(3,i0,i1,i2)) = fullFunc(arr(st,end));
            }
          }
        }
        Array<double> res2 = partFunc (arr,
                                        IPosition::otherAxes(4, IPosition(3,j0,j1,j2)));
        if (! allNear (res, res2, 1.e-5)) {
          errFlag = true;
        }
      }
    }
  }
  return !errFlag;
}

bool doIt4 (PartFunc* partFunc, FullFunc* fullFunc)
{
  bool errFlag = false;
  IPosition shape(5,2,2,2,2,2);
  Array<double> arr(shape);
  indgen(arr);
  for (int j=0; j<5; j++) {
    Vector<double> res(shape(j));
    IPosition st(5,0);
    IPosition end(shape-1);
    for (int i=0; i<shape(j); i++) {
      st(j) = i;
      end(j) = i;
      res(i) = fullFunc(arr(st,end));
    }
    Array<double> res2 = partFunc (arr,
                                    IPosition::otherAxes(5, IPosition(1,j)));
    if (! allNear (res, res2, 1.e-5)) {
      errFlag = true;
    }
  }
  for (int j=0; j<5; j++) {
    for (int k=j+1; k<5; k++) {
      IPosition resshape(2,shape(j),shape(k));
      Array<double> res(resshape);
      IPosition st(5,0);
      IPosition end(shape-1);
      for (int i0=0; i0<shape(j); i0++) {
        st(j) = i0;
        end(j) = i0;
        for (int i1=0; i1<shape(k); i1++) {
          st(k) = i1;
          end(k) = i1;
          res(IPosition(2,i0,i1)) = fullFunc(arr(st,end));
        }
      }
      Array<double> res2 = partFunc (arr,
                                      IPosition::otherAxes(5, IPosition(2,j,k)));
      if (! allNear (res, res2, 1.e-5)) {
        errFlag = true;
      }
    }
  }
  for (int j0=0; j0<5; j0++) {
    for (int j1=j0+1; j1<5; j1++) {
      for (int j2=j1+1; j2<5; j2++) {
        IPosition resshape(3,shape(j0),shape(j1),shape(j2));
        Array<double> res(resshape);
        IPosition st(5,0);
        IPosition end(shape-1);
        for (int i0=0; i0<shape(j0); i0++) {
          st(j0) = i0;
          end(j0) = i0;
          for (int i1=0; i1<shape(j1); i1++) {
            st(j1) = i1;
            end(j1) = i1;
            for (int i2=0; i2<shape(j2); i2++) {
              st(j2) = i2;
              end(j2) = i2;
              res(IPosition(3,i0,i1,i2)) = fullFunc(arr(st,end));
            }
          }
        }
        Array<double> res2 = partFunc (arr,
                                        IPosition::otherAxes(5, IPosition(3,j0,j1,j2)));
        if (! allNear (res, res2, 1.e-5)) {
          errFlag = true;
        }
      }
    }
  }
  for (int j0=0; j0<5; j0++) {
    for (int j1=j0+1; j1<5; j1++) {
      for (int j2=j1+1; j2<5; j2++) {
        for (int j3=j2+1; j3<5; j3++) {
          IPosition resshape(4,shape(j0),shape(j1),shape(j2),shape(j3));
          Array<double> res(resshape);
          IPosition st(5,0);
          IPosition end(shape-1);
          for (int i0=0; i0<shape(j0); i0++) {
            st(j0) = i0;
            end(j0) = i0;
            for (int i1=0; i1<shape(j1); i1++) {
              st(j1) = i1;
              end(j1) = i1;
              for (int i2=0; i2<shape(j2); i2++) {
                st(j2) = i2;
                end(j2) = i2;
                for (int i3=0; i3<shape(j3); i3++) {
                  st(j3) = i3;
                  end(j3) = i3;
                  res(IPosition(4,i0,i1,i2,i3)) = fullFunc(arr(st,end));
                }
              }
            }
          }
          Array<double> res2 = partFunc (arr,
                                          IPosition::otherAxes(5, IPosition(4,j0,j1,j2,j3)));
          if (! allNear (res, res2, 1.e-5)) {
            errFlag = true;
          }
        }
      }
    }
  }
  return !errFlag;
}

bool doIt5 (PartFunc* partFunc, FullFunc* fullFunc)
{
  bool errFlag = false;
  IPosition shape(6,2,2,2,2,2,2);
  Array<double> arr(shape);
  indgen(arr);
  for (int j=0; j<6; j++) {
    Vector<double> res(shape(j));
    IPosition st(6,0);
    IPosition end(shape-1);
    for (int i=0; i<shape(j); i++) {
      st(j) = i;
      end(j) = i;
      res(i) = fullFunc(arr(st,end));
    }
    Array<double> res2 = partFunc (arr,
                                    IPosition::otherAxes(6, IPosition(1,j)));
    if (! allNear (res, res2, 5.e-5)) {
      errFlag = true;
    }
  }
  for (int j=0; j<6; j++) {
    for (int k=j+1; k<6; k++) {
      IPosition resshape(2,shape(j),shape(k));
      Array<double> res(resshape);
      IPosition st(6,0);
      IPosition end(shape-1);
      for (int i0=0; i0<shape(j); i0++) {
        st(j) = i0;
        end(j) = i0;
        for (int i1=0; i1<shape(k); i1++) {
          st(k) = i1;
          end(k) = i1;
          res(IPosition(2,i0,i1)) = fullFunc(arr(st,end));
        }
      }
      Array<double> res2 = partFunc (arr,
                                      IPosition::otherAxes(6, IPosition(2,j,k)));
      if (! allNear (res, res2, 1.e-5)) {
        errFlag = true;
      }
    }
  }
  for (int j0=0; j0<6; j0++) {
    for (int j1=j0+1; j1<6; j1++) {
      for (int j2=j1+1; j2<6; j2++) {
        IPosition resshape(3,shape(j0),shape(j1),shape(j2));
        Array<double> res(resshape);
        IPosition st(6,0);
        IPosition end(shape-1);
        for (int i0=0; i0<shape(j0); i0++) {
          st(j0) = i0;
          end(j0) = i0;
          for (int i1=0; i1<shape(j1); i1++) {
            st(j1) = i1;
            end(j1) = i1;
            for (int i2=0; i2<shape(j2); i2++) {
              st(j2) = i2;
              end(j2) = i2;
              res(IPosition(3,i0,i1,i2)) = fullFunc(arr(st,end));
            }
          }
        }
        Array<double> res2 = partFunc (arr,
                                        IPosition::otherAxes(6, IPosition(3,j0,j1,j2)));
        if (! allNear (res, res2, 1.e-5)) {
          errFlag = true;
        }
      }
    }
  }
  for (int j0=0; j0<6; j0++) {
    for (int j1=j0+1; j1<6; j1++) {
      for (int j2=j1+1; j2<6; j2++) {
        for (int j3=j2+1; j3<6; j3++) {
          IPosition resshape(4,shape(j0),shape(j1),shape(j2),shape(j3));
          Array<double> res(resshape);
          IPosition st(6,0);
          IPosition end(shape-1);
          for (int i0=0; i0<shape(j0); i0++) {
            st(j0) = i0;
            end(j0) = i0;
            for (int i1=0; i1<shape(j1); i1++) {
              st(j1) = i1;
              end(j1) = i1;
              for (int i2=0; i2<shape(j2); i2++) {
                st(j2) = i2;
                end(j2) = i2;
                for (int i3=0; i3<shape(j3); i3++) {
                  st(j3) = i3;
                  end(j3) = i3;
                  res(IPosition(4,i0,i1,i2,i3)) = fullFunc(arr(st,end));
                }
              }
            }
          }
          Array<double> res2 = partFunc (arr,
                                          IPosition::otherAxes(6, IPosition(4,j0,j1,j2,j3)));
          if (! allNear (res, res2, 1.e-5)) {
            errFlag = true;
          }
        }
      }
    }
  }
  for (int j0=0; j0<6; j0++) {
    for (int j1=j0+1; j1<6; j1++) {
      for (int j2=j1+1; j2<6; j2++) {
        for (int j3=j2+1; j3<6; j3++) {
          for (int j4=j3+1; j4<6; j4++) {
            IPosition resshape(5,shape(j0),shape(j1),shape(j2),shape(j3),
                                shape(j4));
            Array<double> res(resshape);
            IPosition st(6,0);
            IPosition end(shape-1);
            for (int i0=0; i0<shape(j0); i0++) {
              st(j0) = i0;
              end(j0) = i0;
              for (int i1=0; i1<shape(j1); i1++) {
                st(j1) = i1;
                end(j1) = i1;
                for (int i2=0; i2<shape(j2); i2++) {
                  st(j2) = i2;
                  end(j2) = i2;
                  for (int i3=0; i3<shape(j3); i3++) {
                    st(j3) = i3;
                    end(j3) = i3;
                    for (int i3=0; i3<shape(j3); i3++) {
                      st(j3) = i3;
                      end(j3) = i3;
                      for (int i4=0; i4<shape(j4); i4++) {
                        st(j4) = i4;
                        end(j4) = i4;
                        res(IPosition(5,i0,i1,i2,i3,i4)) =
                        fullFunc(arr(st,end));
                      }
                    }
                  }
                }
              }
            }
            Array<double> res2 = partFunc (arr,
                                            IPosition::otherAxes(6, IPosition(5,j0,j1,j2,j3,j4)));
            if (! allNear (res, res2, 1.e-5)) {
              errFlag = true;
            }
          }
        }
      }
    }
  }
  return !errFlag;
}

bool doIt (PartFunc* partFunc, FullFunc* fullFunc, bool extra)
{
  bool success = true;
  if(!doIt1(partFunc, fullFunc, extra))
    success = false;
  if(!doIt2(partFunc, fullFunc))
    success = false;
  if(!doIt3(partFunc, fullFunc))
    success = false;
  if(!doIt4(partFunc, fullFunc))
    success = false;
  if(!doIt5(partFunc, fullFunc))
    success = false;
  return success;
}

BOOST_AUTO_TEST_CASE(partial_sums)
{
  BOOST_CHECK(doIt (&partialSums, &sum, true));
}

BOOST_AUTO_TEST_CASE(partial_means)
{
  BOOST_CHECK(doIt (&partialMeans, &mean, true));
}

BOOST_AUTO_TEST_CASE(partial_variances)
{
  BOOST_CHECK(doIt (&myPartialVariances, &myVariance, false));
}
BOOST_AUTO_TEST_CASE(partial_avdevs)
{
  BOOST_CHECK(doIt (&partialAvdevs, &avdev, false));
}
BOOST_AUTO_TEST_CASE(partial_stddevs)
{
  BOOST_CHECK(doIt (&myPartialStddevs, &myStddev, false));
}
BOOST_AUTO_TEST_CASE(partial_rmss)
{
  BOOST_CHECK(doIt (&partialRmss, &rms, false));
}
BOOST_AUTO_TEST_CASE(partial_mins)
{
  BOOST_CHECK(doIt (&partialMins, &min, true));
}
BOOST_AUTO_TEST_CASE(partial_max)
{
  BOOST_CHECK(doIt (&partialMaxs, &max, true));
}
BOOST_AUTO_TEST_CASE(partial_medians_even)
{
  BOOST_CHECK(doIt (&myMeanPartialMedians, &myMeanMedian, true));
}
BOOST_AUTO_TEST_CASE(partial_medians_noteven)
{
  BOOST_CHECK(doIt (&myNomeanPartialMedians, &myNomeanMedian, true));
}
BOOST_AUTO_TEST_CASE(partial_madfms_even)
{
  BOOST_CHECK(doIt (&myMeanPartialMadfms, &myMeanMadfm, true));
}
BOOST_AUTO_TEST_CASE(partial_madfms_noteven)
{
  BOOST_CHECK(doIt (&myNomeanPartialMadfms, &myNomeanMadfm, true));
}
BOOST_AUTO_TEST_CASE(partial_fractiles)
{
  BOOST_CHECK(doIt (&myPartialFractiles, &myFractile, true));
}
BOOST_AUTO_TEST_CASE(partial_hexile)
{
  BOOST_CHECK(doIt (&myPartialHexiles, &myHexile, true));
}
BOOST_AUTO_TEST_CASE(partial_quartile)
{
  BOOST_CHECK(doIt (&myPartialQuartiles, &myQuartile, true));
}

BOOST_AUTO_TEST_SUITE_END()
