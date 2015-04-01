//# tStokesConverter.cc: test program for StokesConverter class
//# Copyright (C) 1997,1999,2000,2002
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

#include <casacore/casa/Arrays/MaskArrLogi.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/ms/MeasurementSets/StokesConverter.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main() 
{
  Int err=0;
  try {
    StokesConverter sc;
    {
      Vector<Int> out(7),in(4);
      
      in(0)=Stokes::RR;
      in(1)=Stokes::LL;
      in(2)=Stokes::RL;
      in(3)=Stokes::LR;

      out(0)=Stokes::I;
      out(1)=Stokes::Q;
      out(2)=Stokes::U;
      out(3)=Stokes::V;
      out(4)=Stokes::Ptotal;
      out(5)=Stokes::Pangle;
      out(6)=Stokes::PFlinear;
      
      sc.setConversion(out,in);
      
      Vector<Complex> datain(4),dataout(7);
      datain(0)=Complex(0.6,0.3);
      datain(1)=Complex(0.4,0.2);
      datain(2)=Complex(0.225,0.3);
      datain(3)=Complex(0.375,0.0);
      
      sc.convert(dataout,datain);
      if (!nearAbs(dataout(0),Complex(1.0,0.5),1.e-6)||
	  !nearAbs(dataout(1),Complex(0.6,0.3),1.e-6)||
	  !nearAbs(dataout(2),Complex(0.3,0.15),1.e-6)||
	  !nearAbs(dataout(3),Complex(0.2,0.1),1.e-6)||
	  !nearAbs(dataout(4),Complex(0.782624,0.0),1.e-6)||
	  !nearAbs(dataout(5),Complex(0.231824,0.0),1.e-6)||
	  !nearAbs(dataout(6),Complex(0.67082,0.0),1.e-6)) {
	err++;
	cerr << "dataout="<<dataout<<endl;
      }

      Vector<Bool> flagout, flagin(4);
      flagin.set(False);
      flagin(2)=True;
      sc.convert(flagout,flagin);
      if (flagout(0) || !flagout(1) || !flagout(2) || flagout(3) ||
	  !flagout(4) || !flagout(5) || !flagout(6)) {
	err++;
	cerr << "flagout="<<flagout<<endl;
      }

      sc.invert(flagin, flagout);
      if (!flagin(0) || !flagin(1) || !flagin(2) || !flagin(3)) {
	err++;
	cerr << "flagin="<<flagin<< endl;
      }
    }

    {
      Vector<Complex> datain(4),dataout(4);
      datain(0)=1.0; //I
      datain(1)=0.5; //Q
      datain(2)=0.3; //U
      datain(3)=0.1; //V
      Matrix<Complex> data(4,5);
      Vector<Int> out(4),in(4);
      for (Int i=0; i<5; i++) {
	if (i>0) datain=data.column(i);
	for (Int j=0; j<4; j++) in(j)=4*i+j+1;
	for (Int k=0; k<5; k++) {
	  for (Int l=0; l<4; l++) out(l)=4*k+l+1;
	  
	  sc.setConversion(out,in);
	  sc.convert(dataout,datain);
	  if (i==0) {
	    data.column(k)=dataout;
	  } else {
	    if (!allNearAbs(data.column(k),dataout,1.e-6)) {
	      cerr<< "Error for i="<<i<<", k="<<k<<endl;
	      cerr<< "dataout="<<dataout<<endl;
	      cerr<< "data.column(k)="<<data.column(k)<<endl;
	      err++;
	    }
	  }
	}
      }
    }
  } catch (AipsError x) {
    cout << "Exception: "<< x.getMesg() <<endl;
  } 
  if (err==0) cout<<"OK"<<endl;
  else cout << err << " errors encountered"<<endl;
  return err;
}

