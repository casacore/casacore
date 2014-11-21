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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

//# Includes
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayPartMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/iostream.h>

// If an argument is given, some performance tests will also be done.

#include <casacore/casa/namespace.h>
typedef Array<Double> PartFunc (const Array<Double>&, const IPosition& axes);
typedef Double FullFunc (const Array<Double>&);

Array<Double> myMeanPartialMedians (const Array<Double>& array,
				    const IPosition& axes)
{
  return partialMedians (array, axes, True, False);
}
Double myMeanMedian (const Array<Double>& array)
{
  return median (array, False, True, False);
}
Array<Double> myNomeanPartialMedians (const Array<Double>& array,
				      const IPosition& axes)
{
  return partialMedians (array, axes, False, False);
}
Double myNomeanMedian (const Array<Double>& array)
{
  return median (array, False, False, False);
}
Array<Double> myMeanPartialMadfms (const Array<Double>& array,
                                   const IPosition& axes)
{
  return partialMadfms (array, axes, True, False);
}
Double myMeanMadfm (const Array<Double>& array)
{
  return madfm (array, False, True, False);
}
Array<Double> myNomeanPartialMadfms (const Array<Double>& array,
				      const IPosition& axes)
{
  return partialMadfms (array, axes, False, False);
}
Double myNomeanMadfm (const Array<Double>& array)
{
  return madfm (array, False, False, False);
}
Array<Double> myPartialFractiles (const Array<Double>& array,
				  const IPosition& axes)
{
  return partialFractiles (array, axes, 0.3, False);
}
Double myFractile (const Array<Double>& array)
{
  return fractile (array, 0.3, False, False);
}
Array<Double> myPartialHexiles (const Array<Double>& array,
                                const IPosition& axes)
{
  return partialInterHexileRanges (array, axes, False);
}
Double myHexile (const Array<Double>& array)
{
  return interHexileRange (array, False, False);
}
Array<Double> myPartialQuartiles (const Array<Double>& array,
                                  const IPosition& axes)
{
  return partialInterQuartileRanges (array, axes, False);
}
Double myQuartile (const Array<Double>& array)
{
  return interQuartileRange (array, False, False);
}


Bool doIt (PartFunc* partFunc, FullFunc* fullFunc, Bool doExtra)
{
  Bool errFlag = False;
  {
    IPosition shape(2,3,4);
    Array<Double> arr(shape);
    indgen(arr);
    for (Int j=0; j<2; j++) {
      Vector<Double> res(shape(j));
      IPosition st(2,0);
      IPosition end(shape-1);
      for (Int i=0; i<shape(j); i++) {
	st(j) = i;
	end(j) = i;
	res(i) = fullFunc(arr(st,end));
      }
      Array<Double> res2 = partFunc (arr, IPosition(1,1-j));
      if (! allNear (res, res2, 1.e-5)) {
	errFlag = True;
	cout << "for shape " << shape << ", collapse axis " << j << endl;
	cout << " result is " << res2 << endl;
	cout << " expected  " << res << endl;
      }
    }
    if (doExtra) {
      {
	Array<Double> res2 = partFunc (arr, IPosition());
	if (! allNear (arr, res2, 1.e-5)) {
	  errFlag = True;
	  cout << "for shape " << shape << ", no collapse axis " << endl;
	  cout << " result is " << res2 << endl;
	  cout << " expected  " << arr << endl;
	}
      }
      {
	Array<Double> res2 = partFunc (arr, IPosition(2,0,1));
	Vector<Double> res(1, fullFunc(arr));
	if (! allEQ (res, res2)) {
	  errFlag = True;
	  cout << "for shape " << shape << ", collapse axis 0,1" << endl;
	  cout << " result is " << res2 << endl;
	  cout << " expected  " << res << endl;
	}
      }
    }
  }
  {
    IPosition shape(3,3,4,5);
    Array<Double> arr(shape);
    indgen(arr);
    for (Int j=0; j<3; j++) {
      Vector<Double> res(shape(j));
      IPosition st(3,0);
      IPosition end(shape-1);
      for (Int i=0; i<shape(j); i++) {
	st(j) = i;
	end(j) = i;
	res(i) = fullFunc(arr(st,end));
      }
      Array<Double> res2 = partFunc (arr,
				     IPosition::otherAxes(3, IPosition(1,j)));
      if (! allNear (res, res2, 1.e-5)) {
	errFlag = True;
	cout << "for shape " << shape << ", collapse axis " << j << endl;
	cout << " result is " << res2 << endl;
	cout << " expected  " << res << endl;
      }
    }
    for (Int j=0; j<3; j++) {
      for (Int k=j+1; k<3; k++) {
	IPosition resshape(2,shape(j),shape(k));
	Array<Double> res(resshape);
	IPosition st(3,0);
	IPosition end(shape-1);
	for (Int i0=0; i0<shape(j); i0++) {
	  st(j) = i0;
	  end(j) = i0;
	  for (Int i1=0; i1<shape(k); i1++) {
	    st(k) = i1;
	    end(k) = i1;
	    res(IPosition(2,i0,i1)) = fullFunc(arr(st,end));
	  }
	}
	Array<Double> res2 = partFunc (arr,
				   IPosition::otherAxes(3, IPosition(2,j,k)));
	if (! allNear (res, res2, 1.e-5)) {
	  errFlag = True;
	  cout << "for shape " << shape
	       << ", collapse axes " << j << ',' << k << endl;
	  cout << " result is " << res2 << endl;
	  cout << " expected  " << res << endl;
	}
      }
    }
  }
  {
    IPosition shape(4,3,4,5,6);
    Array<Double> arr(shape);
    indgen(arr);
    for (Int j=0; j<4; j++) {
      Vector<Double> res(shape(j));
      IPosition st(4,0);
      IPosition end(shape-1);
      for (Int i=0; i<shape(j); i++) {
	st(j) = i;
	end(j) = i;
	res(i) = fullFunc(arr(st,end));
      }
      Array<Double> res2 = partFunc (arr,
				    IPosition::otherAxes(4, IPosition(1,j)));
      if (! allNear (res, res2, 1.e-5)) {
	errFlag = True;
	cout << "for shape " << shape
	     << ", collapse axis " << j << endl;
	cout << " result is " << res2 << endl;
	cout << " expected  " << res << endl;
      }
    }
    for (Int j=0; j<4; j++) {
      for (Int k=j+1; k<4; k++) {
	IPosition resshape(2,shape(j),shape(k));
	Array<Double> res(resshape);
	IPosition st(4,0);
	IPosition end(shape-1);
	for (Int i0=0; i0<shape(j); i0++) {
	  st(j) = i0;
	  end(j) = i0;
	  for (Int i1=0; i1<shape(k); i1++) {
	    st(k) = i1;
	    end(k) = i1;
	    res(IPosition(2,i0,i1)) = fullFunc(arr(st,end));
	  }
	}
	Array<Double> res2 = partFunc (arr,
				   IPosition::otherAxes(4, IPosition(2,j,k)));
	if (! allNear (res, res2, 1.e-5)) {
	  errFlag = True;
	  cout << "for shape " << shape
	       << ", collapse axes " << j << ',' << k << endl;
	  cout << " result is " << res2 << endl;
	  cout << " expected  " << res << endl;
	}
      }
    }
    for (Int j0=0; j0<4; j0++) {
      for (Int j1=j0+1; j1<4; j1++) {
	for (Int j2=j1+1; j2<4; j2++) {
	  IPosition resshape(3,shape(j0),shape(j1),shape(j2));
	  Array<Double> res(resshape);
	  IPosition st(4,0);
	  IPosition end(shape-1);
	  for (Int i0=0; i0<shape(j0); i0++) {
	    st(j0) = i0;
	    end(j0) = i0;
	    for (Int i1=0; i1<shape(j1); i1++) {
	      st(j1) = i1;
	      end(j1) = i1;
	      for (Int i2=0; i2<shape(j2); i2++) {
		st(j2) = i2;
		end(j2) = i2;
		res(IPosition(3,i0,i1,i2)) = fullFunc(arr(st,end));
	      }
	    }
	  }
	  Array<Double> res2 = partFunc (arr,
			       IPosition::otherAxes(4, IPosition(3,j0,j1,j2)));
	  if (! allNear (res, res2, 1.e-5)) {
	    errFlag = True;
	    cout << "for shape " << shape
		 << ", collapse axes " << j0 << ','
		 << j1 << ',' << j2 << endl;
	    cout << " result is " << res2 << endl;
	    cout << " expected  " << res << endl;
	  }
	}
      }
    }
  }
  {
    IPosition shape(5,3,4,5,6,7);
    Array<Double> arr(shape);
    indgen(arr);
    for (Int j=0; j<5; j++) {
      Vector<Double> res(shape(j));
      IPosition st(5,0);
      IPosition end(shape-1);
      for (Int i=0; i<shape(j); i++) {
	st(j) = i;
	end(j) = i;
	res(i) = fullFunc(arr(st,end));
      }
      Array<Double> res2 = partFunc (arr,
				     IPosition::otherAxes(5, IPosition(1,j)));
      if (! allNear (res, res2, 1.e-5)) {
	errFlag = True;
	cout << "for shape " << shape << ", collapse axis " << j << endl;
	cout << " result is " << res2 << endl;
	cout << " expected  " << res << endl;
      }
    }
    for (Int j=0; j<5; j++) {
      for (Int k=j+1; k<5; k++) {
	IPosition resshape(2,shape(j),shape(k));
	Array<Double> res(resshape);
	IPosition st(5,0);
	IPosition end(shape-1);
	for (Int i0=0; i0<shape(j); i0++) {
	  st(j) = i0;
	  end(j) = i0;
	  for (Int i1=0; i1<shape(k); i1++) {
	    st(k) = i1;
	    end(k) = i1;
	    res(IPosition(2,i0,i1)) = fullFunc(arr(st,end));
	  }
	}
	Array<Double> res2 = partFunc (arr,
				   IPosition::otherAxes(5, IPosition(2,j,k)));
	if (! allNear (res, res2, 1.e-5)) {
	  errFlag = True;
	  cout << "for shape " << shape
	       << ", collapse axes " << j << ',' << k << endl;
	  cout << " result is " << res2 << endl;
	  cout << " expected  " << res << endl;
	}
      }
    }
    for (Int j0=0; j0<5; j0++) {
      for (Int j1=j0+1; j1<5; j1++) {
	for (Int j2=j1+1; j2<5; j2++) {
	  IPosition resshape(3,shape(j0),shape(j1),shape(j2));
	  Array<Double> res(resshape);
	  IPosition st(5,0);
	  IPosition end(shape-1);
	  for (Int i0=0; i0<shape(j0); i0++) {
	    st(j0) = i0;
	      end(j0) = i0;
	      for (Int i1=0; i1<shape(j1); i1++) {
		st(j1) = i1;
		end(j1) = i1;
		for (Int i2=0; i2<shape(j2); i2++) {
		  st(j2) = i2;
		  end(j2) = i2;
		  res(IPosition(3,i0,i1,i2)) = fullFunc(arr(st,end));
		}
	      }
	  }
	  Array<Double> res2 = partFunc (arr,
			      IPosition::otherAxes(5, IPosition(3,j0,j1,j2)));
	  if (! allNear (res, res2, 1.e-5)) {
	    errFlag = True;
	    cout << "for shape " << shape 
		 << ", collapse axes " << j0 << ','
		 << j1 << ',' << j2 << endl;
	    cout << " result is " << res2 << endl;
	    cout << " expected  " << res << endl;
	  }
	}
      }
    }
    for (Int j0=0; j0<5; j0++) {
      for (Int j1=j0+1; j1<5; j1++) {
	for (Int j2=j1+1; j2<5; j2++) {
	  for (Int j3=j2+1; j3<5; j3++) {
	    IPosition resshape(4,shape(j0),shape(j1),shape(j2),shape(j3));
	    Array<Double> res(resshape);
	    IPosition st(5,0);
	    IPosition end(shape-1);
	    for (Int i0=0; i0<shape(j0); i0++) {
	      st(j0) = i0;
	      end(j0) = i0;
	      for (Int i1=0; i1<shape(j1); i1++) {
		st(j1) = i1;
		end(j1) = i1;
		for (Int i2=0; i2<shape(j2); i2++) {
		  st(j2) = i2;
		  end(j2) = i2;
		  for (Int i3=0; i3<shape(j3); i3++) {
		    st(j3) = i3;
		    end(j3) = i3;
		    res(IPosition(4,i0,i1,i2,i3)) = fullFunc(arr(st,end));
		  }
		}
		}
	    }
	    Array<Double> res2 = partFunc (arr,
			    IPosition::otherAxes(5, IPosition(4,j0,j1,j2,j3)));
	    if (! allNear (res, res2, 1.e-5)) {
	      errFlag = True;
	      cout << "for shape " << shape
		   << ", collapse axes " << j0 << ','
		   << j1 << ',' << j2 << ',' << j3 << endl;
	      cout << " result is " << res2 << endl;
	      cout << " expected  " << res << endl;
	    }
	  }
	}
      }
    }
  }
  {
    IPosition shape(6,3,4,5,6,7,8);
    Array<Double> arr(shape);
    indgen(arr);
    for (Int j=0; j<6; j++) {
      Vector<Double> res(shape(j));
      IPosition st(6,0);
      IPosition end(shape-1);
      for (Int i=0; i<shape(j); i++) {
	st(j) = i;
	end(j) = i;
	res(i) = fullFunc(arr(st,end));
      }
      Array<Double> res2 = partFunc (arr,
				     IPosition::otherAxes(6, IPosition(1,j)));
      if (! allNear (res, res2, 5.e-5)) {
	errFlag = True;
	cout << "for shape " << shape << ", collapse axis " << j << endl;
	cout << " result is " << res2 << endl;
	cout << " expected  " << res << endl;
      }
    }
    for (Int j=0; j<6; j++) {
      for (Int k=j+1; k<6; k++) {
	IPosition resshape(2,shape(j),shape(k));
	Array<Double> res(resshape);
	IPosition st(6,0);
	IPosition end(shape-1);
	for (Int i0=0; i0<shape(j); i0++) {
	  st(j) = i0;
	  end(j) = i0;
	  for (Int i1=0; i1<shape(k); i1++) {
	    st(k) = i1;
	    end(k) = i1;
	      res(IPosition(2,i0,i1)) = fullFunc(arr(st,end));
	  }
	}
	Array<Double> res2 = partFunc (arr,
				   IPosition::otherAxes(6, IPosition(2,j,k)));
	if (! allNear (res, res2, 1.e-5)) {
	  errFlag = True;
	  cout << "for shape " << shape
	       << ", collapse axes " << j << ',' << k << endl;
	  cout << " result is " << res2 << endl;
	  cout << " expected  " << res << endl;
	}
      }
    }
    for (Int j0=0; j0<6; j0++) {
      for (Int j1=j0+1; j1<6; j1++) {
	for (Int j2=j1+1; j2<6; j2++) {
	  IPosition resshape(3,shape(j0),shape(j1),shape(j2));
	  Array<Double> res(resshape);
	  IPosition st(6,0);
	  IPosition end(shape-1);
	  for (Int i0=0; i0<shape(j0); i0++) {
	    st(j0) = i0;
	    end(j0) = i0;
	    for (Int i1=0; i1<shape(j1); i1++) {
	      st(j1) = i1;
	      end(j1) = i1;
	      for (Int i2=0; i2<shape(j2); i2++) {
		st(j2) = i2;
		end(j2) = i2;
		res(IPosition(3,i0,i1,i2)) = fullFunc(arr(st,end));
	      }
	    }
	  }
	  Array<Double> res2 = partFunc (arr,
			       IPosition::otherAxes(6, IPosition(3,j0,j1,j2)));
	  if (! allNear (res, res2, 1.e-5)) {
	    errFlag = True;
	    cout << "for shape " << shape
		 << ", collapse axes " << j0 << ','
		 << j1 << ',' << j2 << endl;
	    cout << " result is " << res2 << endl;
	    cout << " expected  " << res << endl;
	  }
	}
      }
    }
    for (Int j0=0; j0<6; j0++) {
      for (Int j1=j0+1; j1<6; j1++) {
	for (Int j2=j1+1; j2<6; j2++) {
	  for (Int j3=j2+1; j3<6; j3++) {
	    IPosition resshape(4,shape(j0),shape(j1),shape(j2),shape(j3));
	    Array<Double> res(resshape);
	    IPosition st(6,0);
	    IPosition end(shape-1);
	    for (Int i0=0; i0<shape(j0); i0++) {
	      st(j0) = i0;
	      end(j0) = i0;
	      for (Int i1=0; i1<shape(j1); i1++) {
		st(j1) = i1;
		end(j1) = i1;
		for (Int i2=0; i2<shape(j2); i2++) {
		  st(j2) = i2;
		  end(j2) = i2;
		  for (Int i3=0; i3<shape(j3); i3++) {
		    st(j3) = i3;
		    end(j3) = i3;
		    res(IPosition(4,i0,i1,i2,i3)) = fullFunc(arr(st,end));
		  }
		}
	      }
	    }
	    Array<Double> res2 = partFunc (arr,
			   IPosition::otherAxes(6, IPosition(4,j0,j1,j2,j3)));
	    if (! allNear (res, res2, 1.e-5)) {
	      errFlag = True;
	      cout << "for shape " << shape
		   << ", collapse axes " << j0 << ','
		   << j1 << ',' << j2 << ',' << j3 << endl;
	      cout << " result is " << res2 << endl;
	      cout << " expected  " << res << endl;
	    }
	  }
	}
      }
    }
    for (Int j0=0; j0<6; j0++) {
      for (Int j1=j0+1; j1<6; j1++) {
	for (Int j2=j1+1; j2<6; j2++) {
	  for (Int j3=j2+1; j3<6; j3++) {
	    for (Int j4=j3+1; j4<6; j4++) {
	      IPosition resshape(5,shape(j0),shape(j1),shape(j2),shape(j3),
				 shape(j4));
	      Array<Double> res(resshape);
	      IPosition st(6,0);
	      IPosition end(shape-1);
	      for (Int i0=0; i0<shape(j0); i0++) {
		st(j0) = i0;
		end(j0) = i0;
		for (Int i1=0; i1<shape(j1); i1++) {
		  st(j1) = i1;
		  end(j1) = i1;
		  for (Int i2=0; i2<shape(j2); i2++) {
		    st(j2) = i2;
		    end(j2) = i2;
		    for (Int i3=0; i3<shape(j3); i3++) {
		      st(j3) = i3;
		      end(j3) = i3;
		      for (Int i3=0; i3<shape(j3); i3++) {
			st(j3) = i3;
			end(j3) = i3;
			for (Int i4=0; i4<shape(j4); i4++) {
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
	      Array<Double> res2 = partFunc (arr,
		        IPosition::otherAxes(6, IPosition(5,j0,j1,j2,j3,j4)));
	      if (! allNear (res, res2, 1.e-5)) {
		errFlag = True;
		cout << "for shape " << shape
		     << ", collapse axes " << j0 << ','
		     << j1 << ',' << j2 << ',' << j3 << ',' << j4 << endl;
		cout << " result is " << res2 << endl;
		cout << " expected  " << res << endl;
	      }
	    }
	  }
	}
      }
    }
  }
  return !errFlag;
}

int main (int argc, char* [])
{
  Bool errFlag = False;
  try {
    cout << "Testing partialSums ..." << endl;
    if (! doIt (&partialSums, &sum, True)) {
      cout << "  erronous" << endl;
      errFlag = True;
    }
    cout << "Testing partialMeans ..." << endl;
    if (! doIt (&partialMeans, &mean, True)) {
      cout << "  erronous" << endl;
      errFlag = True;
    }
    cout << "Testing partialVariances ..." << endl;
    if (! doIt (&partialVariances, &variance, False)) {
      cout << "  erronous" << endl;
      errFlag = True;
    }
    cout << "Testing partialStddevs ..." << endl;
    if (! doIt (&partialStddevs, &stddev, False)) {
      cout << "  erronous" << endl;
      errFlag = True;
    }
    cout << "Testing partialAvdevs ..." << endl;
    if (! doIt (&partialAvdevs, &avdev, False)) {
      cout << "  erronous" << endl;
      errFlag = True;
    }
    cout << "Testing partialRmsss ..." << endl;
    if (! doIt (&partialRmss, &rms, False)) {
      cout << "  erronous" << endl;
      errFlag = True;
    }
    cout << "Testing partialMins ..." << endl;
    if (! doIt (&partialMins, &min, True)) {
      cout << "  erronous" << endl;
      errFlag = True;
    }
    cout << "Testing partialMaxs ..." << endl;
    if (! doIt (&partialMaxs, &max, True)) {
      cout << "  erronous" << endl;
      errFlag = True;
    }
    cout << "Testing partialMedians (takeEvenMean=True) ..." << endl;
    if (! doIt (&myMeanPartialMedians, &myMeanMedian, True)) {
      cout << "  erronous" << endl;
      errFlag = True;
    }
    cout << "Testing partialMedians (takeEvenMean=False)..." << endl;
    if (! doIt (&myNomeanPartialMedians, &myNomeanMedian, True)) {
      cout << "  erronous" << endl;
      errFlag = True;
    }
    cout << "Testing partialMadfms (takeEvenMean=True) ..." << endl;
    if (! doIt (&myMeanPartialMadfms, &myMeanMadfm, True)) {
      cout << "  erronous" << endl;
      errFlag = True;
    }
    cout << "Testing partialMadfms (takeEvenMean=False)..." << endl;
    if (! doIt (&myNomeanPartialMadfms, &myNomeanMadfm, True)) {
      cout << "  erronous" << endl;
      errFlag = True;
    }
    cout << "Testing partialFractiles ..." << endl;
    if (! doIt (&myPartialFractiles, &myFractile, True)) {
      cout << "  erronous" << endl;
      errFlag = True;
    }
    cout << "Testing partialHexile ..." << endl;
    if (! doIt (&myPartialHexiles, &myHexile, True)) {
      cout << "  erronous" << endl;
      errFlag = True;
    }
    cout << "Testing partialQuartile ..." << endl;
    if (! doIt (&myPartialQuartiles, &myQuartile, True)) {
      cout << "  erronous" << endl;
      errFlag = True;
    }
    if (argc > 1) {
      // Test performance.
      for (Int cnt=0; cnt<2; cnt++) {
        cout << ">>>" << endl;
        IPosition shape;
        if (cnt == 0) {
          shape = IPosition(2,3000,3000);
          cout << " Performance on [3000,3000]";
        } else if (cnt == 1) {
          shape = IPosition(2,300,30000);
          cout << " Performance on [300,30000]";
        } else {
          shape = IPosition(2,30000,300);
          cout << " Performance on [30000,300]";
        }
        cout << " for collapseaxis 1 and 0..." << endl;
        Timer timer;
        Array<Double> arr(shape);
        indgen(arr);
        for (Int j=0; j<2; j++) {
          {
            timer.mark();
            Array<Double> res2 = partialSums (arr, IPosition(1,1-j));
            timer.show("partialSums   ");
            timer.mark();
            Vector<Double> res(shape(j));
            IPosition st(2,0);
            IPosition end(shape-1);
            for (Int i=0; i<shape(j); i++) {
              st(j) = i;
              end(j) = i;
              res(i) = sum(arr(st,end));
            }
            timer.show("Using sum     ");
            Array<Double> rs = abs(res-res2)/res2;
            Double mn,mx;
            IPosition mnpos, mxpos;
            minMax(mn, mx, mnpos, mxpos, rs);
            cout << "Maximum result diff = " << rs(mxpos) << " at "
                 << mxpos << " (" << res(mxpos) << " and "
                 << res2(mxpos) << ')' << endl;
            AlwaysAssertExit (allNear (res, res2, 1.e-7));
          }
          {
            timer.mark();
            Array<Double> res2 = partialMedians (arr, IPosition(1,1-j));
            timer.show("partialMedians");
            timer.mark();
            Vector<Double> res(shape(j));
            IPosition st(2,0);
            IPosition end(shape-1);
            for (Int i=0; i<shape(j); i++) {
              st(j) = i;
              end(j) = i;
              res(i) = median(arr(st,end), False, False);
            }
            timer.show("Using median  ");
            Array<Double> rs = abs(res-res2)/res2;
            Double mn,mx;
            IPosition mnpos, mxpos;
            minMax(mn, mx, mnpos, mxpos, rs);
            cout << "Maximum result diff = " << rs(mxpos) << " at " << mxpos
                 << " (" << res(mxpos) << " and " << res2(mxpos) << ')' << endl;
            AlwaysAssertExit (allNear (res, res2, 1.e-6));
          }
        }
        cout << "<<<" << endl;
      }
    }
  } catch (const AipsError& x) {
    cout << "Unexpected exception: " << x.getMesg() << endl;
    return 1;
  }
  if (errFlag) {
    cout << "  erronous run" << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
