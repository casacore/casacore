//# StokesConverter.h: convert any set of polarizations into any other one
//# Copyright (C) 1997,2001
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

#ifndef MS_STOKESCONVERTER_H
#define MS_STOKESCONVERTER_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/measures/Measures/Stokes.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// StokesConverter converts any set of polarizations into any other one
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tStokesConverter" demos="">
// </reviewed>

// <prerequisite>
//   <li> <a href="StokesConverter.html"> Stokes </a>
// </prerequisite>
//
// <etymology>
// StokesConverter is a class that converts Stokes Parameters
// </etymology>
//
// <synopsis>
// This class is used to convert polarizations from one system to
// another.
// First the conversion wanted is specified and then large blocks of data
// can be converted.
// <example>
// <srcblock>
// // create converter
//    StokesConverter sc;
//    Vector<Int> out(7),in(4);
// // set the input polarizations   
//    in(0)=Stokes::RR;
//    in(1)=Stokes::LL;
//    in(2)=Stokes::RL;
//    in(3)=Stokes::LR;
// // set the required output  
//    out(0)=Stokes::I;
//    out(1)=Stokes::Q;
//    out(2)=Stokes::U;
//    out(3)=Stokes::V;
//    out(4)=Stokes::Ptotal;
//    out(5)=Stokes::Pangle;
//    out(6)=Stokes::PFlinear;
// // initialize the conversion engine   
//    sc.setConversion(out,in);
// // set up some test data   
//    Vector<Complex> datain(4),dataout(7);
//    datain(0)=1.0;
//    datain(1)=0.9;
//    datain(2)=0.3;
//    datain(3)=0.2;
// // convert the data   
//    sc.convert(dataout,datain);
// </srcblock>
// </example>
// </synopsis>
//
// <motivation>
// Polarization conversion is needed in various places. It makes sense to
// provide all conversion in one place.
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="1997/10/09">
//   <li> cope with incomplete input polarizations sensibly
//   <li> decide what to do about factor 2 between I and RR/LL,XX/YY etc.
// </todo>

class StokesConverter
{
public:

  // default constructor, does not set up a conversion
  StokesConverter();
  
  // Set up a conversion from in to out.
  // The in and out vectors contain a list of polarization present/wanted
  // in that order. The in vector should match the data to convert.
  // (CORR_TYPE column in SPECTRAL_WINDOW table contains this info)
  // The rescale option will correct for crosscorrelation data that
  // has been scaled to the level Stokes I (common practice 
  // in radioastronomy: even though officially I=XX+YY, in practice
  // we need to do I=(XX+YY)/2, set rescale to True to do the latter).
  StokesConverter(const Vector<Int>& out, const Vector<Int>& in,
		  Bool rescale=False);
  
  // desctructor
  ~StokesConverter();

  // Copy constructor
  StokesConverter(const StokesConverter& other);
  
  // Assignment, 
  StokesConverter& operator=(const StokesConverter& other);
  
  // Change or Set the conversion. Arguments are the same as for
  // constructor above.
  void setConversion(const Vector<Int>& out, const Vector<Int>& in,
		     Bool rescale = False);
  
  // convert data, first dimension of input must match
  // that of the input conversion vector used to set up the conversion.
  // Output is resized as needed.
  void convert(Array<Complex>& out, const Array<Complex>& in) const;

  // convert flags, first dimension of input must match
  // that of the input conversion vector used to set up the conversion.
  // Output is resized as needed. All output depending on a flagged input
  // will be flagged. 
  void convert(Array<Bool>& out, const Array<Bool>& in) const;

  // convert weights, first dimension of input must match
  // that of the input conversion vector used to set up the conversion.
  // Output is resized as needed.
  // Set sigma to True when converting sigma's using this routine.
  void convert(Array<Float>& out, const Array<Float>& in,
	       Bool sigma=False) const;

  // invert flags, first dimension of input must match
  // that of the output conversion vector used to set up the conversion.
  // Output is resized as needed. All output depending on a flagged input
  // will be flagged. This does the inverse operation of convert, allowing
  // flagging of converted data to be transferred back to the original data.
  void invert(Array<Bool>& out, const Array<Bool>& in) const;

protected:

  // initialize the polarization conversion matrix
  void initConvMatrix();

private:
  Vector<Int> in_p,out_p;
  Bool rescale_p;
  //# mutable because operator Matrix(Slice,Slice) doesn't have const version
  mutable Matrix<Complex> conv_p; 
  mutable Matrix<Complex> iquvConv_p;
  Bool doIQUV_p;
  Matrix<Bool> flagConv_p;
  Matrix<Float> wtConv_p;
  Matrix<Complex> polConv_p;
};


} //# NAMESPACE CASACORE - END

#endif
