//# StokesConverter.cc: convert polarizations from one frame to another
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


#include <casacore/ms/MeasurementSets/StokesConverter.h>
#include <casacore/scimath/Mathematics/SquareMatrix.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/MatrixMath.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// make a portable float to float sqrt for use in Array<Float>::apply
extern "C" {
    static float floatsqrt(float val) {return sqrt(val);}
}

StokesConverter::StokesConverter() {}

StokesConverter::~StokesConverter() {}

StokesConverter::StokesConverter(const Vector<Int>& out, const Vector<Int>& in,
				 Bool rescale)
{
  setConversion(out,in,rescale);
}

StokesConverter::StokesConverter(const StokesConverter& other)
{
  operator=(other);
}

StokesConverter& StokesConverter::operator=(const StokesConverter& other)
{
  if (this!=&other) {
    setConversion(other.out_p,other.in_p,other.rescale_p);
  }
  return *this;
}

void StokesConverter::setConversion(const Vector<Int>& out, 
				    const Vector<Int>& in,
				    Bool rescale)
{
  rescale_p=rescale;
  doIQUV_p=False;
  initConvMatrix();
  Int nIn=in.nelements();
  Int nOut=out.nelements();
  out_p.resize(nOut);
  out_p=out;
  in_p.resize(nIn);
  in_p=in;
  conv_p.resize(nOut,nIn);
  flagConv_p.resize(nOut,nIn);
  wtConv_p.resize(nOut,nIn);
  // Set up the fudge factors for crosscorrelation data that has been 
  // scaled to the level of Stokes I.
  Vector<Float> factor(Stokes::YL+1,1.0);
  if (rescale) {
    for (uInt i=Stokes::RR; i<=Stokes::YY; i++) factor(i)=0.5;
    for (uInt i=Stokes::RX; i<=Stokes::YL; i++) factor(i)=sqrt(2.0)/4.0;
  }
  // analyze the input - all inputs have to be in the same frame
  Bool linear=False, circular=False, iquv=False, circlin=False, lincirc=False;
  Int count=0;
  for (Int i=0; i<nIn; i++) {
    if (in(i)>=Stokes::I && in(i) <=Stokes::V) {
      if (!iquv) count++;
      iquv=True;
    }
    if (in(i)>=Stokes::XX && in(i) <=Stokes::YY) {
      if (!linear) count++;
      linear=True;
    }
    if (in(i)>=Stokes::RR && in(i) <=Stokes::LL) {
      if (!circular) count++;
      circular=True;
    }
    if (in(i)>=Stokes::RX && in(i) <=Stokes::LY) {
      if (!circlin) count++;
      circlin=True;
    }
    if (in(i)>=Stokes::XR && in(i) <=Stokes::YL) {
      if (!lincirc) count++;
      lincirc=True;
    }
  }
  if (count==0) {
    throw(AipsError("StokesConverter::setConversion - input polarization"
		    " frame not supported"));
  }
  if (count>1) {
    throw(AipsError("StokesConverter::setConversion - input polarizations"
		    " cannot be in a mixture of frames"));
  }
  // set up the conversion matrix
  for (Int i=0; i<nOut; i++) {
    if (out(i)>0 && out(i)<=Stokes::YL) {
      for (Int j=0; j<nIn; j++) { 
	conv_p(i,j)=polConv_p(out(i)-1,in(j)-1)*(factor(in(j))/factor(out(i)));
	flagConv_p(i,j)=(conv_p(i,j)!=Complex(0.));
	wtConv_p(i,j)=abs(conv_p(i,j));
      }
    } else {
      // if output has Ptotal, Plinear or Pangle (or PFtotal, PFlinear), we
      // also setup the matrix for conversion to Stokes.
      if (out(i)>=Stokes::Ptotal && out(i)<=Stokes::Pangle) {
	if (!doIQUV_p) {
	  doIQUV_p=True;
	  iquvConv_p.resize(4,nIn);
	  for (Int j=0; j<nIn; j++) {
	    for (Int k=0; k<4; k++) {
	      iquvConv_p(k,j)=polConv_p(k,in(j)-1)*factor(in(j));
	    }
	  }
	}
	for (Int j=0; j<nIn; j++) {
	  switch (out(i)) {
	  case Stokes::Ptotal: 
	    flagConv_p(i,j)=(iquvConv_p(1,j)!=Complex(0.) ||
				   iquvConv_p(2,j)!=Complex(0.) ||
				   iquvConv_p(3,j)!=Complex(0.));
	    wtConv_p(i,j)=(abs(iquvConv_p(1,j))+abs(iquvConv_p(2,j))+
	      abs(iquvConv_p(3,j)))/3;
	    break;
	  case Stokes::Plinear:
	  case Stokes::Pangle: 
	    flagConv_p(i,j)=(iquvConv_p(1,j)!=Complex(0.) ||
				   iquvConv_p(2,j)!=Complex(0.));
	    wtConv_p(i,j)=(abs(iquvConv_p(1,j))+abs(iquvConv_p(2,j)))/2;
	    break;
	  case Stokes::PFtotal:
	    flagConv_p(i,j)=True;
	    // not certain how to compute the weight for this one
	    wtConv_p(i,j)=(abs(iquvConv_p(1,j))+abs(iquvConv_p(2,j))+
	      abs(iquvConv_p(3,j)))/3;
	    break;
	  case Stokes::PFlinear:
	    flagConv_p(i,j)=(iquvConv_p(0,j)!=Complex(0.) ||
				   iquvConv_p(1,j)!=Complex(0.) ||
				   iquvConv_p(2,j)!=Complex(0.));
	    // not certain how to compute the weight for this one
	    wtConv_p(i,j)=(abs(iquvConv_p(1,j))+abs(iquvConv_p(2,j)))/2;
	    break;
	  default:
	    break;
	  }
	}
      }
    }
  }
}

void StokesConverter::initConvMatrix()
{
  Complex Slin[4][4] = 
  { {0.5, 0.5, 0.0, 0.0},
    {0.0, 0.0, 0.5, Complex(0.0,0.5)},
    {0.0, 0.0, 0.5, Complex(0.0,-0.5)},
    {0.5, -0.5, 0.0, 0.0}
  };
  Complex H[2][2] =
  { {1.0, Complex(0.0,1.0)},
    {1.0, Complex(0.0,-1.0)}
  };
  SquareMatrix<Complex,4> Slinear(Slin);
  SquareMatrix<Complex,2> h(H),hconj;
  h*=(1/sqrt(2.0));
  hconj=h;
  hconj.conj();
  SquareMatrix<Complex,4> Scirc;
  directProduct(Scirc,h,hconj);
  Scirc*=Slinear;

  SquareMatrix<Complex,4> Slincirc;
  SquareMatrix<Complex,2> I2;
  directProduct(Slincirc,I2,hconj);
  Slincirc*=Slinear;

  SquareMatrix<Complex,4> Scirclin;
  directProduct(Scirclin,h,I2);
  Scirclin*=Slinear;

  polConv_p.resize(20,20);
  polConv_p.set(0.0);
  polConv_p.diagonal().set(1.0);
  polConv_p(Slice(4,4),Slice(0,4))=Scirc.matrix();
  polConv_p(Slice(8,4),Slice(0,4))=Slinear.matrix();
  polConv_p(Slice(12,4),Slice(0,4))=Scirclin.matrix();
  polConv_p(Slice(16,4),Slice(0,4))=Slincirc.matrix();
  polConv_p(Slice(0,4),Slice(4,4))=Scirc.inverse().matrix();
  polConv_p(Slice(0,4),Slice(8,4))=Slinear.inverse().matrix();
  polConv_p(Slice(0,4),Slice(12,4))=Scirclin.inverse().matrix();
  polConv_p(Slice(0,4),Slice(16,4))=Slincirc.inverse().matrix();
  SquareMatrix<Complex,4> tmp;
  // circ -> lin
  tmp=Slinear; tmp*=Scirc.inverse();
  polConv_p(Slice(8,4),Slice(4,4))=tmp.matrix();
  // circ -> circlin
  tmp=Scirclin; tmp*=Scirc.inverse();
  polConv_p(Slice(12,4),Slice(4,4))=tmp.matrix();
  // circ -> lincirc
  tmp=Slincirc; tmp*=Scirc.inverse();
  polConv_p(Slice(16,4),Slice(4,4))=tmp.matrix();
  // lin -> circ
  tmp=Scirc; tmp*=Slinear.inverse();
  polConv_p(Slice(4,4),Slice(8,4))=tmp.matrix();
  // lin -> circlin
  tmp=Scirclin; tmp*=Slinear.inverse();
  polConv_p(Slice(12,4),Slice(8,4))=tmp.matrix();
  // lin -> lincirc
  tmp=Slincirc; tmp*=Slinear.inverse();
  polConv_p(Slice(16,4),Slice(8,4))=tmp.matrix();
  // circlin -> circ
  tmp=Scirc; tmp*=Scirclin.inverse();
  polConv_p(Slice(4,4),Slice(12,4))=tmp.matrix();
  // circlin -> lin
  tmp=Slinear; tmp*=Scirclin.inverse();
  polConv_p(Slice(8,4),Slice(12,4))=tmp.matrix();
  // circlin -> lincirc
  tmp=Slincirc; tmp*=Scirclin.inverse();
  polConv_p(Slice(16,4),Slice(12,4))=tmp.matrix();
  // lincirc -> circ
  tmp=Scirc; tmp*=Slincirc.inverse();
  polConv_p(Slice(4,4),Slice(16,4))=tmp.matrix();
  // lincirc -> lin
  tmp=Slinear; tmp*=Slincirc.inverse();
  polConv_p(Slice(8,4),Slice(16,4))=tmp.matrix();
  // lincirc -> circlin
  tmp=Scirclin; tmp*=Slincirc.inverse();
  polConv_p(Slice(12,4),Slice(16,4))=tmp.matrix();

  // remove roundoff
  for (Int i=0; i<20; i++) {
    for (Int j=0; j<20; j++) {
      if (nearAbs(polConv_p(i,j),Complex(0.,0.),1.e-4)) 
	polConv_p(i,j)=Complex(0.,0.);
      if (nearAbs(polConv_p(i,j),Complex(1.,0.),1.e-4)) 
	polConv_p(i,j)=Complex(1.,0.);
      if (nearAbs(polConv_p(i,j),Complex(-1.,0.),1.e-4)) 
	polConv_p(i,j)=Complex(-1.,0.);
      if (nearAbs(polConv_p(i,j),Complex(0.,1.),1.e-4)) 
	polConv_p(i,j)=Complex(0.,1.);
      if (nearAbs(polConv_p(i,j),Complex(0.,-1.),1.e-4)) 
	polConv_p(i,j)=Complex(0.,-1.);
      if (nearAbs(polConv_p(i,j),Complex(0.5,0.),1.e-4)) 
	polConv_p(i,j)=Complex(0.5,0.);
      if (nearAbs(polConv_p(i,j),Complex(-0.5,0.),1.e-4)) 
	polConv_p(i,j)=Complex(-0.5,0.);
      if (nearAbs(polConv_p(i,j),Complex(0.,0.5),1.e-4)) 
	polConv_p(i,j)=Complex(0.,0.5);
      if (nearAbs(polConv_p(i,j),Complex(0.,-0.5),1.e-4)) 
	polConv_p(i,j)=Complex(0.,-0.5);
    }
  }
}

void StokesConverter::convert(Array<Complex>& out, const Array<Complex>& in) const
{
  IPosition outShape(in.shape()); outShape(0)=out_p.nelements();
  Int nDim=in.ndim();
  out.resize(outShape);
  Int nCorrIn=in.shape()(0);
  DebugAssert(nCorrIn==Int(in_p.nelements()),AipsError);
  Matrix<Complex> inMat=in.reform(IPosition(2,nCorrIn,in.nelements()/nCorrIn));

  Matrix<Complex> outMat=out.reform(IPosition(2,outShape(0),
					      out.nelements()/outShape(0)));
  IPosition iquvShape(outMat.shape()); iquvShape(0)=4;
  Matrix<Complex> iquv;
  if (doIQUV_p) iquv.resize(iquvShape);
  IPosition outStart(nDim,0),outEnd(outShape-1);

  for (uInt i=0; i<out_p.nelements(); i++) {
    Int pol = out_p(i);
    if (pol<Stokes::PP) {
      // linear conversion
      outMat(Slice(i,1),Slice())=product(conv_p(Slice(i,1),Slice()),inMat);
    } else if (pol >= Stokes::Ptotal && pol<= Stokes::Pangle) {
      // first convert to IQUV
      for (Int j=0; j<4; j++) {
	iquv(Slice(j,1),Slice())=product(iquvConv_p(Slice(j,1),Slice()),inMat);
      }
      // now calculate required parameter
      // todo: there are some possible large temporaries to be optimized here
      switch (pol) {
      case Stokes::Ptotal:
      case Stokes::PFtotal:
	{
	  Array<Complex> tmp;
	  Vector<Float> outf;
	  for (Int j=1; j<=3; j++) {
	    tmp=iquv.row(j);
	    tmp*=conj(tmp);
	    if (j==1) outf=real(tmp);
	    else outf+=real(tmp);
	  }
	  outf.apply(floatsqrt);
	  if (pol==Stokes::PFtotal) {
	    outf/=amplitude(iquv.row(0));
	  }
	  for (uInt k=0; k<outf.nelements(); k++) outMat(i,k)=outf(k);
	}
	break;
      case Stokes::Plinear:
      case Stokes::PFlinear:
	{
	  Array<Complex> tmp;
	  Vector<Float> outf;
	  for (Int j=1; j<=2; j++) {
	    tmp=iquv.row(j);
	    tmp*=conj(tmp);
	    if (j==1) outf=real(tmp);
	    else outf+=real(tmp);
	  }
	  outf.apply(floatsqrt);
	  if (pol==Stokes::PFlinear) {
	    outf/=amplitude(iquv.row(0));
	  }
	  for (uInt k=0; k<outf.nelements(); k++) outMat(i,k)=outf(k);
	}
	break;
      case Stokes::Pangle:
	{
	  // note: angle is not well defined for complex quantities
	  // only makes sense if Q and U phase differs by 0 or 180 degrees.
	  Vector<Float> outf=atan2(real(iquv.row(2)),real(iquv.row(1)));
	  outf/=2.0f;
      	  // convertArray(outMat.row(i),outf);
	  // convertArray is broken 1997/10/09, spell it out
	  for (uInt k=0; k<outf.nelements(); k++) outMat(i,k)=outf(k);
	}
	break;
      }
    }
  }
}


void StokesConverter::convert(Array<Bool>& out, const Array<Bool>& in) const
{
  IPosition outShape(in.shape()); outShape(0)=out_p.nelements();
  out.resize(outShape);
  Int nCorrIn=in.shape()(0);
  DebugAssert(nCorrIn==Int(in_p.nelements()),AipsError);
  Matrix<Bool> inMat=in.reform(IPosition(2,nCorrIn,in.nelements()/nCorrIn));
  
  Matrix<Bool> outMat=out.reform(IPosition(2,outShape(0),
					   out.nelements()/outShape(0)));
  for (uInt i=0; i<out_p.nelements(); i++) {
    for (uInt j=0; j<inMat.ncolumn(); j++) {
      outMat(i,j)=False;
      for (Int k=0; k<nCorrIn; k++) {
	if (flagConv_p(i,k)&&inMat(k,j)) {
	  outMat(i,j)=True;
	  break;
	}
      }
    }
  }
}

void StokesConverter::convert(Array<Float>& out, const Array<Float>& in,
			      Bool sigma) const
{
  IPosition outShape(in.shape()); outShape(0)=out_p.nelements();
  out.resize(outShape);
  Int nCorrIn=in.shape()(0);
  DebugAssert(nCorrIn==Int(in_p.nelements()),AipsError);
  Matrix<Float> inMat=in.reform(IPosition(2,nCorrIn,in.nelements()/nCorrIn));
  
  Matrix<Float> outMat=out.reform(IPosition(2,outShape(0),
					   out.nelements()/outShape(0)));
  // change calculation based on sigma:
  // for weights we use Wout=1/sum(square(factor(k))*1/Win(k))
  // for sigmas  we use Sout=sqrt(sum(square(factor(k)*Sin(k))))
  for (uInt i=0; i<out_p.nelements(); i++) {
    for (uInt j=0; j<inMat.ncolumn(); j++) {
      outMat(i,j)=0;
      for (Int k=0; k<nCorrIn; k++) {
	if (inMat(k,j)!=0) outMat(i,j)+= 
			     (sigma ? square(wtConv_p(i,k)*inMat(k,j)) :
			      square(wtConv_p(i,k))/inMat(k,j));
	else { outMat(i,j)=0; break;}  // flag output if one of inputs is zero
      }
      if (outMat(i,j)!=0) outMat(i,j)=
			    (sigma ? sqrt(outMat(i,j)) : 1/outMat(i,j));
    }
  }
}

void StokesConverter::invert(Array<Bool>& out, const Array<Bool>& in) const
{
  IPosition outShape(in.shape()); outShape(0)=in_p.nelements();
  // use input if provided, else use unflagged array
  if (out.nelements()==0) {
    out.resize(outShape);
    out.set(False);
  }
  Int nCorrIn=in.shape()(0);
  DebugAssert(out.shape()==outShape,AipsError);
  DebugAssert(nCorrIn==Int(out_p.nelements()),AipsError);
  Matrix<Bool> inMat=in.reform(IPosition(2,nCorrIn,in.nelements()/nCorrIn));

  Matrix<Bool> outMat=out.reform(IPosition(2,outShape(0),
					      out.nelements()/outShape(0)));
  Matrix<Bool> first(outMat.shape(),True);
  // flag or unflag all data depending on the input.
  // output is flagged if any input is flagged, unflagged if all input unflagged
  // output is unchanged if independent of inputs.
  for (Int i=0; i<nCorrIn; i++) {
    for (uInt j=0; j<inMat.ncolumn(); j++) {
      for (Int k=0; k<outShape(0); k++) {
	if (flagConv_p(i,k)) {
	  if (first(k,j)) {
	    first(k,j)=False;
	    outMat(k,j)=inMat(i,j);
	  } else {
	    outMat(k,j)|=inMat(i,j);
	  }
	}
      }
    }
  }  
}



} //# NAMESPACE CASACORE - END

