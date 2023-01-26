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
// // Calculate a double dpsival
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
  void put(const double in);

  // Show statistics
  void show(ostream &os);
  void showHisto(ostream &os);

 private:
  // Histogram resolution
  static const uint32_t HISTO_WIDTH = 500;
  // Data
  // Count
  uint32_t n_p;
  // Sum values
  double sum_p;
  // Sum squared
  double sq_p;
  // Max found
  double max_p;
  // Min found
  double min_p;
  // Step in histogram
  double hstep_p;
  // Histogram size (really (n-1)/2)
  uint32_t hsize_p;
  // Histogram width
  uint32_t hwidth_p;
  // Histogram
  int32_t *histo_p;

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
IAUR(cal2jd)(const int32_t &iy, const int32_t &im, const int32_t &id,
	     double &djm0, double &djm, int32_t &j);
extern "C" void 
IAUR(epj2jd)(const double &epj, double &djm0, double &djm);
extern "C" void 
IAUR(prec76)(const double &ep01, const double &ep02,
	     const double &ep11, const double &ep12,
	     double &zeta, double &z,
	     double &theta);
extern "C" void 
IAUR(pmat76)(const double &epoch1, const double &epoch2,
	     double *rmatp);
extern "C" void 
IAUR(nut80)(const double &epoch1, const double &epoch2,
	    double &dpsi, double &deps);
extern "C" void 
IAUR(nutm80)(const double &epoch1, const double &epoch2,
	     double *rmatn);
extern "C" double 
IAUR(obl80)(const double &epoch1, const double &epoch2);
extern "C" void 
IAUR(pr00)(const double &epoch1, const double &epoch2,
	   double &dpsi, double &deps);
extern "C" void 
IAUR(bi00)(double &dpsi, double &deps, double &dra);
extern "C" void 
IAUR(bp00)(const double &epoch1, const double &epoch2,
	   double *rb, double *rp, double *rbp);
extern "C" void 
IAUR(pnm80)(const double &epoch1, const double &epoch2,
	    double *rmatpn);
extern "C" void 
IAUR(pn00a)(const double &epoch1, const double &epoch2,
	    double &dpsi, double &deps, double &epsa,
	    double *rb, double *rp, double *rbp,
	    double *rn, double *rnpn);
extern "C" void 
IAUR(pn00b)(const double &epoch1, const double &epoch2,
	    double &dpsi, double &deps, double &epsa,
	    double *rb, double *rp, double *rbp,
	    double *rn, double *rnpn);
extern "C" void 
IAUR(pr00)(const double &ep01, const double &ep02,
	   double &dpsipr, double &depspr);
extern "C" void 
IAUR(nut00b)(const double &epoch1, const double &epoch2,
	     double &dpsi, double &deps);
extern "C" void 
IAUR(nut00a)(const double &epoch1, const double &epoch2,
	     double &dpsi, double &deps);
extern "C" void 
IAUR(num00a)(const double &epoch1, const double &epoch2,
	     double *rn);
extern "C" void 
IAUR(num00b)(const double &epoch1, const double &epoch2,
	     double *rn);
extern "C" void 
IAUR(c2t00a)(const double &tta, const double &ttb, const double &uta,
	     const double &utb, const double &xp, const double &yp,
	     double *rc2t);
extern "C" double 
IAUR(sp00)(const double &date1, const double &date2);
extern "C" void 
IAUR(pom00)(const double &xp, const double &yp, const double &sp,
	    double *rpom);
extern "C" double 
IAUR(gmst00)(const double &uta, const double &utb,
	     const double &tta, const double &ttb);
extern "C" double 
IAUR(era00)(const double &uta, const double &utb);
extern "C" double 
IAUR(gmst82)(const double &dj1, const double &dj2);
extern "C" double 
IAUR(ee00a)(const double &date1, const double &date2);
extern "C" double 
IAUR(eect00)(const double &date1, const double &date2);
extern "C" double 
IAUR(eqeq94)(const double &date1, const double &date2);
extern "C" void 
IAUR(pnm00a)(const double &date1, const double &date2, double *rbpn);
extern "C" void 
IAUR(c2teqx)(double *rbpn, const double &gst, double *rpom, double *rc2t);
extern "C" void 
IAUR(rz)(const double &psi, double *r);
extern "C" void 
IAUR(cr)(double *r, double *c);

// </group>


} //# NAMESPACE CASACORE - END

#endif
