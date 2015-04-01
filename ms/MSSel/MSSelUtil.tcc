//# MSSelUtil.cc: templated helper function for MSSelector
//# Copyright (C) 1997,1999,2000,2001
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
//#
//# $Id$

#ifndef MS_MSSELUTIL_TCC
#define MS_MSSELUTIL_TCC

#include <casacore/ms/MSSel/MSSelUtil.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> 
Array<Float> MSSelUtil<T>::diffData(const Array<T>& data,
				    const Array<Bool>& flag,
				    const Array<Bool>& flagRow,
				    Int diffAxis,
				    Int window,
				    Bool doMedian)
{
  IPosition shape=data.shape();
  Array<Float> diff(shape); diff.set(0);
  const Int nCorr=shape(0);
  const Int nChan=shape(1);
  const Int nXY=nCorr*nChan;
  Int nTime=shape(2), nIfr=1;
  if (data.ndim()==4) {
    nIfr=shape(2);
    nTime=shape(3);
  }
  const Int nOff=nXY*nIfr;
  const Int win=max(2,window);
  Bool deleteData, deleteFlag, deleteFlagRow, deleteDiff;
  const T* pdata = data.getStorage(deleteData);
  const Bool* pflag = flag.getStorage(deleteFlag);
  const Bool* pflagRow = flagRow.getStorage(deleteFlagRow);
  Float* pdiff = diff.getStorage(deleteDiff);
  T zero(0.), sum;
  Block<Float> buf(win);
  // diffAxis == 1: channel, 2: row, 3: time
  if (diffAxis!=1) {
    // do row or time difference
    Int offset=0, rowOffset=0;
    for (Int i=0; i<nTime; i++, rowOffset+=nIfr) {
      Int st=max(0,i-win/2), end=min(nTime-1,i-win/2+win-1);
      for (Int ifr=0; ifr<nIfr; ifr++) {
	if (!pflagRow[rowOffset+ifr]) {
	  for (Int j=0; j<nXY; j++) {
	    if (!pflag[offset]) {
	      if (win==2) {
		if (i>0 && !pflag[offset-nOff]) {
		  pdiff[offset]=abs(pdata[offset]-pdata[offset-nOff]);
		}
	      } else if (!doMedian) {
		Int count=0;
		sum=zero;
		for (Int k=st, koff=offset+(st-i)*nOff; k<end; k++, koff+=nOff) {
		  if (!pflag[koff]) {
		    count++;
		    sum+=pdata[koff];
		  }
		}
		if (count>1) sum/=count;
		if (count>0) pdiff[offset]=abs(pdata[offset]-sum);
	      } else { // use median
		Int count=0;
		for (Int k=st, koff=offset+(st-i)*nOff; k<end; k++, koff+=nOff) {
		  if (!pflag[koff]) {
		    buf[count++]=abs(pdata[offset]-pdata[koff]);
		  }
		}
		if (count>0) {
		  pdiff[offset]=median(Vector<Float>(buf,count));
	        }
	      }
	    }
	    offset++;
	  }
	} else {
	  offset+=nXY;
	}
      }
    }
  } else {
    // do channel difference
    Int offset=0, rowOffset=0;
    for (Int i=0; i<nTime; i++, rowOffset+=nIfr) {
      for (Int ifr=0; ifr<nIfr; ifr++) {
	if (!pflagRow[rowOffset+ifr]) {
	  for (Int j=0; j<nChan; j++) {
	    Int st=max(0,j-win/2), end=min(nChan-1,j-win/2+win-1);
	    for (Int pol=0; pol<nCorr; pol++) {
	      if (!pflag[offset]) {
		if (win==2) {
		  if (j>0 && !pflag[offset-nCorr]) {
		    pdiff[offset]=abs(pdata[offset]-pdata[offset-nCorr]);
		  }
		} else if (!doMedian) {
		  Int count=0;
		  sum=zero;
		  for (Int k=st, koff=offset+(st-j)*nCorr; k<end;
		       k++, koff+=nCorr) {
		    if (!pflag[koff]) {
		      count++;
		      sum+=pdata[koff];
		    }
		  }
		  if (count>1) sum/=count;
		  if (count>0) pdiff[offset]=abs(pdata[offset]-sum);
		} else { // use median
		  Int count=0;
		  for (Int k=st, koff=offset+(st-j)*nCorr; k<end; 
		       k++, koff+=nCorr) {
		    if (!pflag[koff]) {
		      buf[count++]=abs(pdata[offset]-pdata[koff]);
		    }
		  }
		  if (count>0) {
		    pdiff[offset]=median(Vector<Float>(buf,count));
		  }
		}
	      }
	      offset++;
	    }
	  }
	} else {
	  offset+=nXY;
	}
      }
    }
  }
  data.freeStorage(pdata,deleteData);
  flag.freeStorage(pflag,deleteFlag);
  flagRow.freeStorage(pflagRow,deleteFlagRow);
  diff.putStorage(pdiff,deleteDiff);
  return diff;
}


} //# NAMESPACE CASACORE - END


#endif
