//# SofaTest.h: Wrapping of IAU SOFA Fortran routines and test class
//# Copyright (C) 2003
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

#ifndef MEASURES_SOFATEST_H
#define MEASURES_SOFATEST_H

//# Include files
#include <casacore/casa/aips.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>  Wrapping of IAU SOFA Fortran routines and test class</summary>
// <use visibility=export>
//
// <reviewed reviewer="wbrouw" date="2003/09/08" tests="tIAU2000">
// </reviewed>
//
// <synopsis>
// The definition in this file enable the use of the IAU SOFA Fortran routines
// in the C++ test routines. 
// By using the provided macro <em>IAUR</em> care is taken of SOFA prefixes
// and probable extra underscores given by some compilers.
//
// For information on SOFA see the SOFA page at
// <a href="http://ww.iau.org">IAU</a> or at the currenthome of SOFA at
// <a href="http://www.iau-sofa.rl.ac.uk/">Rutherford</a>  
//
// The SofaTest class can be used to provide histogram of test data.
// The resolution is defaulted to 500 steps, compressed to 40 in the output.
// </synopsis>
//
// <example>
// <srcblock>
// SofaTest dpsi;	// Create an histogram class
// // Loop over the following two statements to fill histogram
// // Calculate a Double dpsival
// dpsi.put(dpsival);
// // Show the result
// cout.precision(4);
// cout << "Casacore dpsi(mas):" << endl;
// dpsi.show(cout);
// dpsi.showHisto(cout);
// </srcblock>
// The result will look like:
// <srcblock>
// Casacore dpsi (mas):
// 5001 points were accumulated
// with max = 1.529e-09, and min = -1.61e-09
// and an average of 5.066e-12 and a standard deviation of 2.42e-10
//
// 23 counts per step; 0.0004 value.
//  |*
//  |*
//  |*
//  |*
//  |*
//  |*
//  |*
//  |*
//  |*
//  |*
//  |*
//  |*
//  |*
//  |*
//  |*
//  |*
//  |-*
//  |-*
//  |---*
//  |---*
//  |-----*
//  |---------*
//  |---------------------*
//  _--------------------------------------------------------*
//  |---------------------------------------------------------*
//  |----------------------*
//  |------------*
//  |------*
//  |----*
//  |--*
//  |-*
//  |-*
//  |*
//  |*
//  |*
//  |*
//  |*
//  |*
//  |*
//  |*
//  |*
//  |*
//  |*
//  |*
//  |*
// </srcblock>
// </example>
//
// <motivation>
// To enable in-line testing of the Casacore Measures conversion routines.
// </motivation>
//
// <todo asof="2003/08/31">
// Nothing (I hope!)
// </todo>
//

class SofaTest {

 public:
  // Constructors
  // Create an empty SofaTest class, ready for accumulation
  SofaTest();
  // Copy
  SofaTest(const SofaTest &other);

  // Destructor
  ~SofaTest();

  // Operators
  // Assign
  SofaTest &operator=(const SofaTest &other);

  // Methods
  // Clear the test class
  void clear();
  
  // Accumulate statistics
  void put(const Double in);

  // Show statistics
  void show(ostream &os);
  void showHisto(ostream &os);

 private:
  // Histogram resolution
  static const uInt HISTO_WIDTH = 500;
  // Data
  // Count
  uInt n_p;
  // Sum values
  Double sum_p;
  // Sum squared
  Double sq_p;
  // Max found
  Double max_p;
  // Min found
  Double min_p;
  // Step in histogram
  Double hstep_p;
  // Histogram size (really (n-1)/2)
  uInt hsize_p;
  // Histogram width
  uInt hwidth_p;
  // Histogram
  Int *histo_p;

  // Methods
  // Copy object
  void copy(const SofaTest &other);

};

//# Global function wraps
// <summary> Global Fortran function wraps </summary>
// <group name=IAU_SOFA>

#if !defined(NEED_FORTRAN_UNDERSCORES)
#define NEED_FORTRAN_UNDERSCORES 1
#endif

#if NEED_FORTRAN_UNDERSCORES
#define IAUR(x) iau_##x##_
#else
#define IAUR(x) iau_##x##
#endif
extern "C" void 
IAUR(cal2jd)(const Int &iy, const Int &im, const Int &id,
	     Double &djm0, Double &djm, Int &j);
extern "C" void 
IAUR(epj2jd)(const Double &epj, Double &djm0, Double &djm);
extern "C" void 
IAUR(prec76)(const Double &ep01, const Double &ep02,
	     const Double &ep11, const Double &ep12,
	     Double &zeta, Double &z,
	     Double &theta);
extern "C" void 
IAUR(pmat76)(const Double &epoch1, const Double &epoch2,
	     Double *rmatp);
extern "C" void 
IAUR(nut80)(const Double &epoch1, const Double &epoch2,
	    Double &dpsi, Double &deps);
extern "C" void 
IAUR(nutm80)(const Double &epoch1, const Double &epoch2,
	     Double *rmatn);
extern "C" Double 
IAUR(obl80)(const Double &epoch1, const Double &epoch2);
extern "C" void 
IAUR(pr00)(const Double &epoch1, const Double &epoch2,
	   Double &dpsi, Double &deps);
extern "C" void 
IAUR(bi00)(Double &dpsi, Double &deps, Double &dra);
extern "C" void 
IAUR(bp00)(const Double &epoch1, const Double &epoch2,
	   Double *rb, Double *rp, Double *rbp);
extern "C" void 
IAUR(pnm80)(const Double &epoch1, const Double &epoch2,
	    Double *rmatpn);
extern "C" void 
IAUR(pn00a)(const Double &epoch1, const Double &epoch2,
	    Double &dpsi, Double &deps, Double &epsa,
	    Double *rb, Double *rp, Double *rbp,
	    Double *rn, Double *rnpn);
extern "C" void 
IAUR(pn00b)(const Double &epoch1, const Double &epoch2,
	    Double &dpsi, Double &deps, Double &epsa,
	    Double *rb, Double *rp, Double *rbp,
	    Double *rn, Double *rnpn);
extern "C" void 
IAUR(pr00)(const Double &ep01, const Double &ep02,
	   Double &dpsipr, Double &depspr);
extern "C" void 
IAUR(nut00b)(const Double &epoch1, const Double &epoch2,
	     Double &dpsi, Double &deps);
extern "C" void 
IAUR(nut00a)(const Double &epoch1, const Double &epoch2,
	     Double &dpsi, Double &deps);
extern "C" void 
IAUR(num00a)(const Double &epoch1, const Double &epoch2,
	     Double *rn);
extern "C" void 
IAUR(num00b)(const Double &epoch1, const Double &epoch2,
	     Double *rn);
extern "C" void 
IAUR(c2t00a)(const Double &tta, const Double &ttb, const Double &uta,
	     const Double &utb, const Double &xp, const Double &yp,
	     Double *rc2t);
extern "C" Double 
IAUR(sp00)(const Double &date1, const Double &date2);
extern "C" void 
IAUR(pom00)(const Double &xp, const Double &yp, const Double &sp,
	    Double *rpom);
extern "C" Double 
IAUR(gmst00)(const Double &uta, const Double &utb,
	     const Double &tta, const Double &ttb);
extern "C" Double 
IAUR(era00)(const Double &uta, const Double &utb);
extern "C" Double 
IAUR(gmst82)(const Double &dj1, const Double &dj2);
extern "C" Double 
IAUR(ee00a)(const Double &date1, const Double &date2);
extern "C" Double 
IAUR(eect00)(const Double &date1, const Double &date2);
extern "C" Double 
IAUR(eqeq94)(const Double &date1, const Double &date2);
extern "C" void 
IAUR(pnm00a)(const Double &date1, const Double &date2, Double *rbpn);
extern "C" void 
IAUR(c2teqx)(Double *rbpn, const Double &gst, Double *rpom, Double *rc2t);
extern "C" void 
IAUR(rz)(const Double &psi, Double *r);
extern "C" void 
IAUR(cr)(Double *r, Double *c);

// </group>


} //# NAMESPACE CASACORE - END

#endif
