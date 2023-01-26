//# MSSelUtil2.cc: templated helper function for MSSelector
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

#ifndef MS_MSSELUTIL2_TCC
#define MS_MSSELUTIL2_TCC

#include <casacore/ms/MSSel/MSSelUtil2.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// reorder from 3d to 4d (adding ifr axis)
template <class T>
void MSSelUtil2<T>::reorderData(Array<T>& data,
				const Vector<int32_t>& ifrSlot,
				int32_t nIfr, const Vector<int32_t>& timeSlot, 
				int32_t nTime, const T& defvalue)
{
  int32_t nPol=data.shape()(0);
  int32_t nChan=data.shape()(1);
  int64_t nRow=data.shape()(2);
  Array<T> data2(IPosition(4,nPol,nChan,nIfr,nTime));
  data2.set(defvalue);
  
  bool deleteData,deleteData2;
  const T* pdata=data.getStorage(deleteData);
  T* pdata2=data2.getStorage(deleteData2);
  int32_t n=nPol*nChan;
  for (int64_t i=0; i<nRow; i++) {
    int64_t start1=i*n, start2=(ifrSlot(i)+timeSlot(i)*nIfr)*n;
    for (int32_t j=0; j<n; j++) pdata2[start2+j]=pdata[start1+j];
  }
  data.freeStorage(pdata,deleteData);
  data2.putStorage(pdata2,deleteData2);
  data.reference(data2);
}


// reorder from 4d to 3d (removing ifr axis)
template <class T>
void MSSelUtil2<T>::reorderData(Array<T>& data, const Matrix<int64_t>& rowIndex,
			        int64_t nRow)
{
  int32_t nPol=data.shape()(0),nChan=data.shape()(1),nIfr=data.shape()(2),
    nTime=data.shape()(3);
  if (nIfr!=rowIndex.shape()(0) || nTime!=rowIndex.shape()(1)) {
    //    os<< LogIO::SEVERE << "Data array shape does not match current selection"
    //      << LogIO::POST;
    return;
  }
  Array<T> data2(IPosition(3,nPol,nChan,nRow)); 
  
  bool deleteData,deleteData2;
  const T* pData=data.getStorage(deleteData);
  T* pData2=data2.getStorage(deleteData2);
  int32_t n=nPol*nChan;
  for (int32_t i=0; i<nTime; i++) {
    for (int32_t j=0; j<nIfr; j++) {
      int64_t k=rowIndex(j,i);
      if (k>=0) {
	int64_t start2=k*n, start1=(j+i*nIfr)*n;
	for (int32_t l=0; l<n; l++) pData2[start2+l]=pData[start1+l];
      }
    }
  }
  data.freeStorage(pData,deleteData);
  data2.putStorage(pData2,deleteData2);
  data.reference(data2);
}

// average data (with flags & weights applied) over its last axis (time or
// row), return in data (overwritten), dataFlag gives new flags.
template <class T>
void MSSelUtil2<T>::timeAverage(Array<bool>& dataFlag, Array<T>& data, 
				const Array<bool>& flag, 
				const Array<float>& weight)
{
  bool delData,delFlag,delWeight;
  const T* pdata=data.getStorage(delData);
  const bool* pflag=flag.getStorage(delFlag);
  const float* pweight=weight.getStorage(delWeight);
  int32_t nPol=data.shape()(0),nChan=data.shape()(1);
  int32_t nIfr=1, nTime=data.shape()(2);
  Array<T> out;
  if (data.ndim()==4) {
    nIfr=nTime;
    nTime=data.shape()(3);
    out.resize(IPosition(3,nPol,nChan,nIfr));
  } else {
    out.resize(IPosition(2,nPol,nChan));
  }
  Array<float> wt(IPosition(3,nPol,nChan,nIfr));
  dataFlag.resize(IPosition(3,nPol,nChan,nIfr));
  dataFlag.set(true);
  bool delDataflag, delWt, delOut;
  float* pwt=wt.getStorage(delWt);
  T* pout=out.getStorage(delOut);
  bool* pdflags=dataFlag.getStorage(delDataflag);
  out=0;
  wt=0;
  int32_t offset=0,off1=0,offw=0;
  for (int32_t l=0; l<nTime; l++) {
    off1=0;
    for (int32_t k=0; k<nIfr; k++) {
      for (int32_t j=0; j<nChan; j++) {
	for (int32_t i=0; i<nPol; i++) {
	  //	  if (!flag(i,j,k,l)) {
	  if (!pflag[offset]) {
	    //	    out(i,j,k)+=weight(k,l)*data(i,j,k,l);
	    pdflags[off1]=false;
	    pout[off1]+=pweight[offw]*pdata[offset];
	    //	    wt(i,j,k)+=weight(k,l);
	    pwt[off1]+=pweight[offw];
	  }
	  off1++; offset++;
	}
      }
      offw++;
    }
  }
  for (int32_t k=0; k<nIfr*nChan*nPol; k++) {
    if (pwt[k]>0) pout[k]/=pwt[k];
  }
  data.freeStorage(pdata,delData);
  flag.freeStorage(pflag,delFlag);
  weight.freeStorage(pweight,delWeight);
  dataFlag.putStorage(pdflags,delDataflag);
  wt.putStorage(pwt,delWt);
  out.putStorage(pout,delOut);
  data.reference(out);
}


} //# NAMESPACE CASACORE - END


#endif
