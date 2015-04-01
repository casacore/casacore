//# Smooth.tcc:  perform smoothing on vectors and arrays
//# Copyright (C) 2010 by ESO (in the framework of the ALMA collaboration)
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002
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

#ifndef SCIMATH_SMOOTH_TCC
#define SCIMATH_SMOOTH_TCC
//   

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayIter.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/scimath/Mathematics/Smooth.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T>
void Smooth<T>::hanning(Vector<T>& out, Vector<Bool>& outmask, 
		        Vector<T>& in, Vector<Bool>& mask, 
			Bool TrueIsGood, Bool relaxed) {
  
  DebugAssert(out.shape().isEqual(in.shape()), AipsError);
  DebugAssert(outmask.shape().isEqual(mask.shape()), AipsError);

  Vector< Vector<Float> > weights(8);
  Vector<Float> vals(3);
  vals = 0.0; weights[0] = vals;// FFF
  vals[0] = 1.0; vals[1] = 0.0; vals[2] = 0.0; weights[1] = vals;// TFF
  vals[0] = 0.0; vals[1] = 1.0; vals[2] = 0.0; weights[2] = vals;// FTF
  vals[0] = 1.0/3.0; vals[1] = 2.0/3.0; vals[2] = 0.0; weights[3] = vals;// TTF
  vals[0] = 0.0; vals[1] = 0.0; vals[2] = 1.0;weights[4] = vals;// FFT
  vals[0] = 0.5; vals[1] = 0.0; vals[2] = 0.5; weights[5] = vals;// TFT
  vals[0] = 0.0; vals[1] = 2.0/3.0; vals[2] = 1.0/3.0; weights[6] = vals;// FTT
  vals[0] = 0.25; vals[1] = 0.5; vals[2] = 0.25; weights[7] = vals;// TTT  

  Vector<Bool> weighted(8); 
  if (relaxed) {
    weighted = False;
    weighted[7] = True;

  } else {
    weighted = True;
    weighted[0] = False;
  }
  
  // make special case for first and last

  out[0] = in[0];
  outmask[0] = mask[0];

  uInt nelm1 = in.nelements()-1;

  uInt m;
  Vector<Float>* w;

  if(nelm1>0){
    m = 2*(mask[0]==TrueIsGood) + 4*(mask[1]==TrueIsGood);
    w = &(weights[m]);
    if (weighted[m]) {
      out[0] = (*w)[1]*in[0] + (*w)[2]*in[1];
      outmask[0] = True==TrueIsGood;
    }
    else{
      if(mask[0]==TrueIsGood){
	out[0] = (*w)[1]*in[0] + (*w)[2]*in[1];
      }
      else{
	out[0] = in[0];
      }
      outmask[0] = False==TrueIsGood;
    }
    m = (mask[nelm1]==TrueIsGood) + 2*(mask[nelm1-1]==TrueIsGood);
    w = &(weights[m]);
    if (weighted[m]) {
      out[nelm1] = (*w)[1]*in[nelm1] + (*w)[0]*in[nelm1-1];
      outmask[nelm1] = True==TrueIsGood;
    }
    else{
      if(mask[nelm1]==TrueIsGood){
	out[nelm1] = (*w)[1]*in[nelm1] + (*w)[0]*in[nelm1-1];
      }
      else{
	out[nelm1] = in[nelm1];
      }
      outmask[nelm1] = False==TrueIsGood;
    }

  }

  // loop from 1..n-2

  for(uInt i=1; i < nelm1; i++){
    m = (mask[i-1]==TrueIsGood) + 2*(mask[i]==TrueIsGood) + 4*(mask[i+1]==TrueIsGood);
    w = &(weights[m]);
    if (weighted[m]) {
      out[i] = (*w)[0]*in[i-1] + (*w)[1]*in[i] + (*w)[2]*in[i+1];
      outmask[i] = True==TrueIsGood;
    }
    else{
      if(mask[i]==TrueIsGood){
	out[i] = (*w)[0]*in[i-1] + (*w)[1]*in[i] + (*w)[2]*in[i+1];
      }
      else{
	out[i] = in[i];
      }
      outmask[i] = False==TrueIsGood;
    }
  }

}

template <class T>
void Smooth<T>::hanning(Array<T>& out, Array<Bool>& outmask, 
		        Array<T>& in, Array<Bool>& mask, 
			Bool TrueIsGood, Bool relaxed) {
  
  Matrix<T> min(in);
  Matrix<T> mout(out);
  Matrix<Bool> mmask(mask);
  Matrix<Bool> moutmask(outmask);
  for(uInt i=0; i<in.shape()[0]; i++){
    Vector<T> vout(mout.row(i));
    Vector<Bool> voutMask(moutmask.row(i));
    Vector<T> vin(min.row(i));
    Vector<Bool> vinMask(mmask.row(i));
    Smooth<T>::hanning(vout, voutMask, vin, vinMask, 
		       TrueIsGood, relaxed);
  }

}

} //# NAMESPACE CASACORE - END


#endif
