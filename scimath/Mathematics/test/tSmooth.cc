//# tSmooth.cc:  this tests the Smooth class
//# Copyright (C) 2010 by ESO (in the framework of the ALMA collaboration)
//# Copyright (C) 1996,1997,1998,1999,2002
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

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/scimath/Mathematics/Smooth.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main() {
  Bool anyFailures = False;
  {
    Bool failed = False;
    // Test with Float Vectors, all flags 0
    
    vector<Float> vyin; 
    vector<Bool> vyinFlags;
    
    Vector<Float> myexp;
    Vector<Float> outv;
    Vector<Bool> outFlags;
    
    uInt vdim = 8;
    
    Float myyin[] = {1,3,1,4,2,6,3,8};
    Bool myflags1[] = {0,0,0,0,0,0,0,0};

    vyin.assign(myyin, myyin+vdim);    
    vyinFlags.assign(myflags1, myflags1+vdim);    

    Vector<Float> yin(vyin);
    Vector<Bool> yinFlags(vyinFlags);
    
    myexp.resize(vdim);
    myexp[0] = 2./3.*vyin[0] + 1./3.*vyin[1];
    myexp[vdim-1] = 1./3.* vyin[vdim-2] + 2./3.*vyin[vdim-1];
    for(uInt i=1; i<vdim-1; i++){
      myexp[i] = 0.25 * vyin[i-1] + 0.5 * vyin[i] + 0.25 * vyin[i+1];
    }
    
    Vector<Bool> myexpflags(vdim,False);
    myexpflags[0] = True;
    myexpflags[7] = True;

    outv.resize(vdim);
    outFlags.resize(vdim);

    Smooth<Float>::hanning(outv, // the output
			   outFlags, // the output mask
			   yin, // the input
			   yinFlags, // the input mask
			   False);  // for flagging: good is not true
    
    if(!allNearAbs(myexp, outv, 1.E-6)){
      for(uInt i = 0; i<vdim; i++){
	cout << i << " in " << yin[i] << endl;
	cout << i << " out " << outv[i] << endl;
	cout << i << " exp " << myexp[i] << endl;
      }
      failed = True;
    }

    if(!allEQ(myexpflags, outFlags)){
      for(uInt i = 0; i<vdim; i++){
	cout << i << " inFlags " << yinFlags[i] << endl;
	cout << i << " outFlags " << outFlags[i] << endl;
	cout << i << " expFlags " << myexpflags[i] << endl;
      }
      failed = True;
    }
    
    if (failed){
      cout << "Failed";
    }
    else{
      cout << "Passed";
    }

    cout << " the Float Vector Hanning Smooth Test, all unflagged"<< endl;
    if (failed){
      anyFailures = True;
    }
  }
  {
    Bool failed = False;
    // Test with Float Vectors, all flags 1
    
    vector<Float> vyin; 
    vector<Bool> vyinFlags;
    
    Vector<Float> myexp;
    Vector<Float> outv;
    Vector<Bool> outFlags;
    
    uInt vdim = 8;
    
    Float myyin[] = {1,3,1,4,2,6,3,8};
    Bool myflags1[] = {1,1,1,1,1,1,1,1};

    vyin.assign(myyin, myyin+vdim);    
    vyinFlags.assign(myflags1, myflags1+vdim);    

    Vector<Float> yin(vyin);
    Vector<Bool> yinFlags(vyinFlags);
    
    myexp.resize(vdim);
    myexp.assign(yin);
    
    outv.resize(vdim);
    outFlags.resize(vdim);

    Vector<Bool> myexpflags(yinFlags);

    Smooth<Float>::hanning(outv, // the output
			   outFlags, // the output mask
			   yin, // the input
			   yinFlags, // the input mask
			   False);  // for flagging: good is not true
    
    if(!allNearAbs(myexp, outv, 1.E-6)){
      for(uInt i = 0; i<vdim; i++){
	cout << i << " in " << yin[i] << endl;
	cout << i << " out " << outv[i] << endl;
	cout << i << " exp " << myexp[i] << endl;
      }
      failed = True;
    }
    
    if(!allEQ(myexpflags, outFlags)){
      for(uInt i = 0; i<vdim; i++){
	cout << i << " inFlags " << yinFlags[i] << endl;
	cout << i << " outFlags " << outFlags[i] << endl;
	cout << i << " expFlags " << myexpflags[i] << endl;
      }
      failed = True;
    }

    if (failed){
      cout << "Failed";
    }
    else{
      cout << "Passed";
    }

    cout << " the Float Vector Hanning Smooth Test, all flagged"<< endl;
    if (failed){
      anyFailures = True;
    }
  }
  {
    Bool failed = False;
    // Test with Float Vectors, mixed flags
    
    vector<Float> vyin; 
    vector<Bool> vyinFlags;
    
    Vector<Float> myexp;
    Vector<Float> outv;
    Vector<Bool> outFlags;
    
    uInt vdim = 8;
    
    Float myyin[]   = {1,3,1,4,2,6,3,8};
    Bool myflags1[] = {1,0,1,1,0,0,0,1};

    vyin.assign(myyin, myyin+vdim);    
    vyinFlags.assign(myflags1, myflags1+vdim);    

    Vector<Float> yin(vyin);
    Vector<Bool> yinFlags(vyinFlags);
    
    myexp.resize(vdim);
    myexp[0] = vyin[0];
    myexp[1] = vyin[1];
    myexp[2] = vyin[2];
    myexp[3] = vyin[3];
    myexp[4] = 2./3.*vyin[4] + 1./3.*vyin[5];
    myexp[5] = 0.25 * vyin[5-1] + 0.5 * vyin[5] + 0.25 * vyin[5+1];
    myexp[6] = 1./3.*vyin[5] + 2./3.*vyin[6];
    myexp[7] = vyin[7];

    Vector<Bool> myexpflags(vdim);
    myexpflags[0] = yinFlags[0] ||  yinFlags[1];
    myexpflags[1] = yinFlags[0] ||  yinFlags[1] || yinFlags[2];
    myexpflags[2] = yinFlags[1] ||  yinFlags[2] || yinFlags[3];
    myexpflags[3] = yinFlags[2] ||  yinFlags[3] || yinFlags[4];
    myexpflags[4] = yinFlags[3] ||  yinFlags[4] || yinFlags[5];
    myexpflags[5] = yinFlags[4] ||  yinFlags[5] || yinFlags[6];
    myexpflags[6] = yinFlags[5] ||  yinFlags[6] || yinFlags[7];
    myexpflags[7] = yinFlags[6] ||  yinFlags[7];

    outv.resize(vdim);
    outFlags.resize(vdim);

    Smooth<Float>::hanning(outv, // the output
			   outFlags, // the output mask
			   yin, // the input
			   yinFlags, // the input mask
			   False);  // for flagging: good is not true
    
    if(!allNearAbs(myexp, outv, 1.E-6)){
      for(uInt i = 0; i<vdim; i++){
	cout << i << " in " << yin[i] << endl;
	cout << i << " out " << outv[i] << endl;
	cout << i << " exp " << myexp[i] << endl;
      }
      failed = True;
    }
    
    if(!allEQ(myexpflags, outFlags)){
      for(uInt i = 0; i<vdim; i++){
	cout << i << " inFlags " << yinFlags[i] << endl;
	cout << i << " outFlags " << outFlags[i] << endl;
	cout << i << " expFlags " << myexpflags[i] << endl;
      }
      failed = True;
    }
    
    if (failed){
      cout << "Failed";
    }
    else{
      cout << "Passed";
    }

    cout << " the Float Vector Hanning Smooth Test, mixed flags"<< endl;
    if (failed){
      anyFailures = True;
    }
  }

  /////////////////////////////////////

  {
    Bool failed = False;
    // Test with Complex Vectors, all flags 0
    
    vector<Complex> vyin; 
    vector<Bool> vyinFlags;
    
    Vector<Complex> myexp;
    Vector<Complex> outv;
    Vector<Bool> outFlags;
    
    uInt vdim = 8;
    
    Complex myyin[] = {Complex(1.,1.),
		       Complex(3.,3.),
		       Complex(1.,1.),
		       Complex(4.,4.),
		       Complex(2.,2.),
		       Complex(6.,6.),
		       Complex(3.,3.),
		       Complex(8.,8.)};
    Bool myflags1[] = {0,0,0,0,0,0,0,0};

    vyin.assign(myyin, myyin+vdim);    
    vyinFlags.assign(myflags1, myflags1+vdim);    

    Vector<Complex> yin(vyin);
    Vector<Bool> yinFlags(vyinFlags);
    
    myexp.resize(vdim);
    myexp[0] = Complex(2./3.,0.)*vyin[0] + Complex(1./3.,0.)*vyin[1];
    myexp[vdim-1] = Complex(2./3.,0.)*vyin[vdim-1] + Complex(1./3.,0.)*vyin[vdim-2];
    for(uInt i=1; i<vdim-1; i++){
      myexp[i] = Complex(0.25,0.) * vyin[i-1] + Complex(0.5,0.) * vyin[i] + Complex(0.25,0) * vyin[i+1];
    }
    
    Vector<Bool> myexpflags(vdim,False);
    myexpflags[0] = True;
    myexpflags[7] = True;

    outv.resize(vdim);
    outFlags.resize(vdim);

    Smooth<Complex>::hanning(outv, // the output
			     outFlags, // the output mask
			     yin, // the input
			     yinFlags, // the input mask
			     False);  // for flagging: good is not true
    
    if(!allNearAbs(myexp, outv, 1.E-6)){
      for(uInt i = 0; i<vdim; i++){
	cout << i << " in " << yin[i] << endl;
	cout << i << " out " << outv[i] << endl;
	cout << i << " exp " << myexp[i] << endl;
      }
      failed = True;
    }

    if(!allEQ(myexpflags, outFlags)){
      for(uInt i = 0; i<vdim; i++){
	cout << i << " inFlags " << yinFlags[i] << endl;
	cout << i << " outFlags " << outFlags[i] << endl;
	cout << i << " expFlags " << myexpflags[i] << endl;
      }
      failed = True;
    }
    
    if (failed){
      cout << "Failed";
    }
    else{
      cout << "Passed";
    }

    cout << " the Complex Vector Hanning Smooth Test, all unflagged"<< endl;
    if (failed){
      anyFailures = True;
    }
  }
  {
    Bool failed = False;
    // Test with Complex Vectors, mixed flags
    
    vector<Complex> vyin; 
    vector<Bool> vyinFlags;
    
    Vector<Complex> myexp;
    Vector<Complex> outv;
    Vector<Bool> outFlags;
    
    uInt vdim = 8;
    
    Complex myyin[] = {Complex(1.,1.),
		       Complex(3.,3.),
		       Complex(1.,1.),
		       Complex(4.,4.),
		       Complex(2.,2.),
		       Complex(6.,6.),
		       Complex(3.,3.),
		       Complex(8.,8.)};
    Bool myflags1[] = {1,0,1,1,0,0,0,1};

    vyin.assign(myyin, myyin+vdim);    
    vyinFlags.assign(myflags1, myflags1+vdim);    

    Vector<Complex> yin(vyin);
    Vector<Bool> yinFlags(vyinFlags);
    
    myexp.resize(vdim);
    myexp[0] = vyin[0];
    myexp[1] = vyin[1];
    myexp[2] = vyin[2];
    myexp[3] = vyin[3];
    myexp[4] = Complex(2./3.,0.)*vyin[4] + Complex(1./3.,0.)*vyin[5];
    myexp[5] = Complex(0.25,0.) * vyin[5-1] + Complex(0.5,0.) * vyin[5] + Complex(0.25,0) * vyin[5+1];
    myexp[6] = Complex(2./3.,0.)*vyin[6] + Complex(1./3.,0.)*vyin[5];
    myexp[7] = vyin[7];

    Vector<Bool> myexpflags(vdim);
    myexpflags[0] = yinFlags[0] ||  yinFlags[1];
    myexpflags[1] = yinFlags[0] ||  yinFlags[1] || yinFlags[2];
    myexpflags[2] = yinFlags[1] ||  yinFlags[2] || yinFlags[3];
    myexpflags[3] = yinFlags[2] ||  yinFlags[3] || yinFlags[4];
    myexpflags[4] = yinFlags[3] ||  yinFlags[4] || yinFlags[5];
    myexpflags[5] = yinFlags[4] ||  yinFlags[5] || yinFlags[6];
    myexpflags[6] = yinFlags[5] ||  yinFlags[6] || yinFlags[7];
    myexpflags[7] = yinFlags[6] ||  yinFlags[7];

    outv.resize(vdim);
    outFlags.resize(vdim);

    Smooth<Complex>::hanning(outv, // the output
			     outFlags, // the output mask
			     yin, // the input
			     yinFlags, // the input mask
			     False);  // for flagging: good is not true
    
    if(!allNearAbs(myexp, outv, 1.E-6)){
      for(uInt i = 0; i<vdim; i++){
	cout << i << " in " << yin[i] << endl;
	cout << i << " out " << outv[i] << endl;
	cout << i << " exp " << myexp[i] << endl;
      }
      failed = True;
    }
    
    if(!allEQ(myexpflags, outFlags)){
      for(uInt i = 0; i<vdim; i++){
	cout << i << " inFlags " << yinFlags[i] << endl;
	cout << i << " outFlags " << outFlags[i] << endl;
	cout << i << " expFlags " << myexpflags[i] << endl;
      }
      failed = True;
    }
    
    if (failed){
      cout << "Failed";
    }
    else{
      cout << "Passed";
    }

    cout << " the Complex Vector Hanning Smooth Test, mixed flags"<< endl;
    if (failed){
      anyFailures = True;
    }
  }

  //////////////////////////////////////

  {
    Bool failed = False;
    // Test with Complex Arrays, all flags 0
    
    
    uInt vdim = 8;
    uInt sdim = 2;
    IPosition adim(2,sdim,vdim);

    Complex myyin[] = {Complex(1.,1.), // (0,0)
		       Complex(1.,1.), // (1,0)
		       Complex(3.,3.), // (0,1)
		       Complex(3.,3.), // (1,1)
		       Complex(1.,1.), // (0,2)
		       Complex(1.,1.), // etc.
		       Complex(4.,4.),
		       Complex(4.,4.),
		       Complex(2.,2.),
		       Complex(2.,2.),
		       Complex(6.,6.),
		       Complex(6.,6.),
		       Complex(3.,3.),
		       Complex(3.,3.),
		       Complex(8.,8.),  // (0,7)
		       Complex(8.,8.)}; // (1,7)
    Bool myflags[] = {0,0,0,0,0,0,0,0,
		      0,0,0,0,0,0,0,0};

    
    Array<Complex> myexp;
    Array<Complex> outv;
    Array<Bool> outFlags;

    Array<Complex> yin(adim, myyin);
    Array<Bool> yinFlags(adim, myflags);
    
    myexp.resize(adim);
    myexp(IPosition(2,0,0)) = Complex(2./3.,0.)*myyin[2*0] + Complex(1./3.,0.)*myyin[2*1];
    myexp(IPosition(2,1,0)) = Complex(2./3.,0.)*myyin[2*0+1] + Complex(1./3.,0.)*myyin[2*1+1];
    myexp(IPosition(2,0,7)) = Complex(2./3.,0.)*myyin[2*7] + Complex(1./3.,0.)*myyin[2*6];
    myexp(IPosition(2,1,7)) = Complex(2./3.,0.)*myyin[2*7+1] + Complex(1./3.,0.)*myyin[2*6+1];
    for(uInt i=1; i<vdim-1; i++){
      myexp(IPosition(2,0,i)) = Complex(0.25,0.) * myyin[2*(i-1)] + Complex(0.5,0.) * myyin[2*i] + Complex(0.25,0.) * myyin[2*(i+1)];
      myexp(IPosition(2,1,i)) = Complex(0.25,0.) * myyin[2*(i-1)+1] + Complex(0.5,0.) * myyin[2*i+1] + Complex(0.25,0.) * myyin[2*(i+1)+1];
    }
    
    Array<Bool> myexpflags(adim,False);
    myexpflags(IPosition(2,0,0)) = True;
    myexpflags(IPosition(2,1,0)) = True;
    myexpflags(IPosition(2,0,7)) = True;
    myexpflags(IPosition(2,1,7)) = True;

    outv.resize(adim);
    outFlags.resize(adim);

    Smooth<Complex>::hanning(outv, // the output
			     outFlags, // the output mask
			     yin, // the input
			     yinFlags, // the input mask
			     False);  // for flagging: good is not true
    
    if(!allNearAbs(myexp, outv, 1.E-6)){
      for(uInt i = 0; i<sdim; i++){
	for(uInt j = 0; j<vdim; j++){
	  cout << i << " " << j << " in " << yin(IPosition(2,i,j)) << endl;
	  cout << i << " " << j << " out " << outv(IPosition(2,i,j)) << endl;
	  cout << i << " " << j << " exp " << myexp(IPosition(2,i,j)) << endl;
	}
      }
      failed = True;
    }

    if(!allEQ(myexpflags, outFlags)){
      for(uInt i = 0; i<sdim; i++){
	for(uInt j = 0; j<vdim; j++){
	  cout << i << " " << j << " inFlags " << yinFlags(IPosition(2,i,j)) << endl;
	  cout << i << " " << j << " outFlags " << outFlags(IPosition(2,i,j)) << endl;
	  cout << i << " " << j << " expFlags " << myexpflags(IPosition(2,i,j)) << endl;
	}
      }
      failed = True;
    }
    
    if (failed){
      cout << "Failed";
    }
    else{
      cout << "Passed";
    }

    cout << " the Complex Array Hanning Smooth Test, all unflagged"<< endl;
    if (failed){
      anyFailures = True;
    }
  }


  //////////////////////////////////////

  if (anyFailures) {
    cout << "FAIL" << endl;
    return 1;
  }
  else {
    cout << "OK" << endl;
    return 0;
  }

}

// End:
