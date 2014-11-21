//# tIAU2000.cc: Test the IAU2000 conversions against SOFA library
//# Copyright (C) 2003,2004
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

#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/SofaTest.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/measures/Measures/MCDirection.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/measures/Measures/MeasData.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MeasFrame.h>
#include <casacore/measures/Measures/MeasRef.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/measures/Measures/Nutation.h>
#include <casacore/measures/Measures/Precession.h>
#include <casacore/casa/OS/Time.h>
#include <casacore/casa/Quanta/RotMatrix.h>
#include <casacore/casa/System/AipsrcValue.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// Print separation line of given (default 75) length
void SEPAR(const uInt l=75) {
  cout << String(l, '-') << endl;
}

// Fill RotMat with Fortran Matrix
RotMatrix fillRM(const Double rm[3][3]) {
  RotMatrix trm;
  for (uInt i=0; i<3; ++i) {
    for (uInt j=0; j<3; ++j) trm(i,j) = rm[i][j];
  };
  return trm;
}

// Check rotation angle around arbitrary axis between two matrices (mas)
Double checkRot(const RotMatrix &rm1, RotMatrix rm2) {
  rm2.transpose();
  RotMatrix r = rm1 * rm2;
  Double x, y, z, s, c, phi;
  x = r(1,2) - r(2,1);
  y = r(2,0) - r(0,2);
  z = r(0,1) - r(1,0);
  s = sqrt(x*x + y*y + z*z);
  if (s != 0.) {      	 
    c = r(0,0) + r(1,1) + r(2,2) - 1.;
    phi = atan2(s, c);
    return phi/C::arcsec;
  };
  return 0.;
}

int main() {
  try {
    // Init
    cout.precision(12);
    cout << "Test IAU2000 coordinate conversions ..." << endl;
    SEPAR();
    // Default date
    Time t0(2003, 3, 1);
    Double MJD0 = 2400000.5;
    Double tJD = t0.julianDay();
    Double tMJD = t0.modifiedJulianDay();
    cout << "A default date of 2003/03/01 is used (JD " << tJD <<
      ", MJD " << tMJD << ")" << endl;
    SEPAR();
    
    // Precession and nutation
    cout << "Precession" << endl;
    SEPAR();
    Double padpsi, padeps, paepsa, pbdpsi, pbdeps, pbepsa;
    Double parb[3][3], parp[3][3], parpb[3][3], parn[3][3], parpn[3][3];
    Double pbrb[3][3], pbrp[3][3], pbrpb[3][3], pbrn[3][3], pbrpn[3][3];
    Precession p00a(Precession::IAU2000A);
    Precession p00b(Precession::IAU2000B);
    IAUR(pn00a)(MJD0, tMJD, padpsi, padeps, paepsa,
		&parb[0][0], &parp[0][0], &parpb[0][0], &parn[0][0],
		&parpn[0][0]);
    IAUR(pn00b)(MJD0, tMJD, pbdpsi, pbdeps, pbepsa,
		&pbrb[0][0], &pbrp[0][0], &pbrpb[0][0], &pbrn[0][0],
		&pbrpn[0][0]);
    RotMatrix carm(p00a(tMJD));
    RotMatrix cbrm(p00b(tMJD));
    RotMatrix parm = fillRM(parp);
    RotMatrix pbrm = fillRM(pbrp);
    RotMatrix nbparm = fillRM(parpb);
    cout << "SOFA precession 2000A and bias:\n" << nbparm << endl;
    cout << "Casacore precession and bias:\n" <<
      MeasTable::frameBias00()*carm << endl;
    cout << "SOFA precession     (no bias):\n" << parm << endl;
    cout << "Casacore precession (no bias):\n" << carm << endl;
    Double depspr, dpsipr;
    IAUR(pr00)(MJD0, tMJD, dpsipr, depspr);
    cout << "SOFA precession corrections:     " << dpsipr << ", " <<
      depspr << endl;
    cout << "Casacore precession corrections: " <<
      MeasTable::precRate00(0)*(tMJD-MeasData::MJD2000)/MeasData::JDCEN <<
      ", " <<
      MeasTable::precRate00(1)*(tMJD-MeasData::MJD2000)/MeasData::JDCEN <<
      endl;
    RotMatrix pparb = fillRM(parb);
    cout << "\nSOFA 2000A bias matrix:\n" << pparb << endl;
    cout << "Casacore 2000A bias matrix:\n" << MeasTable::frameBias00() << endl;
    cout.precision(9);
    cout.setf(ios::fixed);
    cout << "Difference (arcsec) SOFA 2000A - Casacore nobias:  " <<
      checkRot(parm, carm) << endl;
    cout << "Difference (arcsec) SOFA 2000A - Casacore   bias:  " <<
      checkRot(nbparm, MeasTable::frameBias00()*carm) << endl;
    Double fbrm[3][3], prrm[3][3], fbprrm[3][3];
    IAUR(bp00)(MJD0, tMJD, &fbrm[0][0], &prrm[0][0], &fbprrm[0][0]);
    RotMatrix prm = fillRM(prrm);
    cout << "Difference (arcsec) SOFA 2000A - Casacore nobias:  " <<
      checkRot(prm, carm) << endl;
    cout << "(The above must all 3 be .002 uas)" << endl;
    SEPAR();
    Nutation n00a(Nutation::IAU2000A);
    Nutation n00b(Nutation::IAU2000B);
    RotMatrix narm = fillRM(parn);
    RotMatrix nbrm = fillRM(pbrn);
    RotMatrix cnarm(n00a(tMJD));
    RotMatrix cnbrm(n00b(tMJD));
    cout << "Nutation" << endl;
    SEPAR();
    cout.precision(9);
    cout.setf(ios::fixed);
    cout << "Difference (arcsec) SOFA 2000A - SOFA 2000B:         " <<
      checkRot(narm, nbrm) << endl;
    cout << "Difference (arcsec) Casacore 2000A - Casacore 2000B: " <<
      checkRot(cnarm, cnbrm) << endl;
    cout << "(The above two differences are 0.512mas (note next note))" <<
      endl;
    cout << "Difference (arcsec) SOFA 2000A - Casacore 2000A:   " <<
      checkRot(narm, cnarm) << endl;
    cout << "Difference (arcsec) SOFA 2000B - Casacore 2000B:   " <<
      checkRot(nbrm, cnbrm) << endl;
    cout << "(This should be 0.422 uas, due to different definition of\n"
      "\tthe fundamental arguments)" << endl;
    cout << "Equation of equinoxes (IAU2000A)(SOFA, Casacore, diff): " <<
      endl;
    cout << "    " << IAUR(ee00a)(MJD0, tMJD) << ", " <<
      n00a.eqox(tMJD) << ", " <<
      IAUR(ee00a)(MJD0, tMJD) - n00a.eqox(tMJD) << endl;
    SEPAR();

    Double y,tta,ttb,uta,utb;
    {
      cout << "SOFA method comparisons ..." << endl;
      SEPAR();
      cout.precision(9);
      cout.setf(ios::fixed);
      Double dat = 32;
      Double dut1 = -0.3;
      Double dtt = 32.184 + dat -dut1;
      Double rmceo[3][3],rmequ[3][3],rmold[3][3];
      y=1935.;
      IAUR(epj2jd)(y,tta,ttb);
      uta=tta;
      utb=ttb-dtt/86400.;
      Double xp=0;
      Double yp=0;
      IAUR(c2t00a)(tta,ttb,uta,utb,xp,yp,&rmceo[0][0]);
      Double sp = IAUR(sp00)(tta,ttb);
      Double rpom[3][3], gst, rbpn[3][3];
      IAUR(pom00)(xp,yp,sp,&rpom[0][0]);
      gst = IAUR(gmst00)(uta,utb,tta,ttb) + IAUR(ee00a)(tta,ttb);
      IAUR(pnm00a)(tta,ttb,&rbpn[0][0]);
      IAUR(c2teqx)(&rbpn[0][0],gst,&rpom[0][0],&rmequ[0][0]);
      Double rm[3][3];
      IAUR(pnm80)(tta,ttb,&rm[0][0]);
      gst = IAUR(gmst82)(uta,utb)+ IAUR(eqeq94)(tta,ttb);
      IAUR(rz)(gst, &rm[0][0]);
      IAUR(cr)(&rm[0][0],&rmold[0][0]);
      RotMatrix rm1 = fillRM(rmceo);
      RotMatrix rm2 = fillRM(rmequ);
      RotMatrix rm3 = fillRM(rmold);
      cout << "Difference (arcsec) SOFA CEO and Equinox based in 1935:   " <<
	checkRot(rm1, rm2) << endl;
      cout << "Difference (arcsec) SOFA CEO based and old J2000 in 1935: " <<
	checkRot(rm1, rm3) << endl;
      cout << "(The above should be 0.119 uas and 65.034 mas respectively)" <<
	endl;
      SEPAR();
    }

    {
      cout << "IAU2000A/B comparisons ..." << endl;
      SEPAR();
      uInt iau2000_reg =
	AipsrcValue<Bool>::registerRC(String("measures.iau2000.b_use"),
				      False);
      uInt iau2000a_reg =
	AipsrcValue<Bool>::registerRC(String("measures.iau2000.b_use2000a"),
				      False);
      cout << "Registrations old: " << iau2000_reg << ", " <<
	iau2000a_reg << endl;
      MDirection md(Quantity(30., "deg"), Quantity(50., "deg"),
		    MDirection::J2000);
      MEpoch ep(Quantity(50083.,"d"));
      MeasFrame frame(ep);
      MDirection::Convert mcv(md, MDirection::Ref(MDirection::APP, frame));
      AipsrcBool::set(iau2000_reg, False);
      AipsrcBool::set(iau2000a_reg, False);
      cout << "New J2000 " << AipsrcBool::get(iau2000_reg) << ", " <<
	"J2000A " << AipsrcBool::get(iau2000a_reg) << endl;
      cout << mcv() << endl;
      MDirection mdcv = mcv();
      AipsrcBool::set(iau2000_reg, True);
      MDirection::Convert mcv1(md, MDirection::Ref(MDirection::APP, frame));
      cout << "New J2000 " << AipsrcBool::get(iau2000_reg) << ", " <<
	"J2000A " << AipsrcBool::get(iau2000a_reg) << endl;
      cout << mcv1() << endl;
      cout << "Difference: " << (mcv1().getValue().getValue()
	- mdcv.getValue().getValue())*200000. << endl;

      AipsrcBool::set(iau2000a_reg, True);
      MDirection::Convert mcv2(md, MDirection::Ref(MDirection::APP, frame));
      cout << "New J2000 " << AipsrcBool::get(iau2000_reg) << ", " <<
	"J2000A " << AipsrcBool::get(iau2000a_reg) << endl;
      cout << mcv2() << endl;
      cout << "Difference: " << (mcv2().getValue().getValue()
				 - mdcv.getValue().getValue())*200000. << endl;
      SEPAR();
    }

    {
      cout << "Test of some details ..." << endl;
      SEPAR();
      Double era  = IAUR(era00)(uta,utb); 
      Double gmst = IAUR(gmst00)(uta,utb,tta,ttb);
      Double sp   = IAUR(sp00)(tta,ttb);
      Double eect = IAUR(eect00)(tta,ttb);
      Nutation nuta(Nutation::IAU2000A);
      cout << "UT: " << (uta-2451545.0)+utb << ", " <<
	utb-MeasData::MJD2000 << endl; 
      cout << "TT: " << (tta-2451545.0)+ttb << ", " <<
	ttb-MeasData::MJD2000 << endl; 
      cout << "s'   (Sofa, Casacore, diff): " <<
	sp << ", " << MeasTable::sprime00(ttb) << ", " <<
	sp - MeasTable::sprime00(ttb) << endl;
      cout << "ERA  (Sofa, Casacore, diff): " <<
	era << ", " << MeasTable::ERA00(utb) << ", " <<
	era - MeasTable::ERA00(utb) << endl;
      cout << "GMST (Sofa, Casacore, diff): " <<
	gmst << ", " <<
	fmod((ttb+6713.)*C::_2pi + MeasTable::GMST00(utb, ttb),
	     C::_2pi) << ", " << gmst -
	fmod((ttb+6713.)*C::_2pi + MeasTable::GMST00(utb, ttb),
	     C::_2pi) << endl;
      cout << "GMST82 (GMST82, GMST00, diff): " <<
	utb+MeasTable::GMST0(utb)/MeasData::SECinDAY + 6713. << ", " <<
	ttb+MeasTable::GMST00(utb, ttb)/C::_2pi + 6713. <<", " <<
	utb+MeasTable::GMST0(utb)/MeasData::SECinDAY -
	ttb-MeasTable::GMST00(utb, ttb)/C::_2pi << endl;
      cout << "EqEqCT00 (Sofa, Casacore, diff): " <<
	eect << ", " << nuta.eqoxCT(ttb) << ", " <<
	eect-nuta.eqoxCT(ttb) << endl;
      SEPAR();
    }

    {
      cout << "Test Aipsrc value cross talk ..." << endl;
      SEPAR();
      uInt iau2000_r =
	AipsrcValue<Bool>::registerRC(String("measures.iau2000.b_use"),
				      False);
      uInt iau2000a_r =
	AipsrcValue<Bool>::registerRC(String("measures.iau2000.b_use2000a"),
				      False);
      cout << "Registrations now: " << iau2000_r << ", " <<
	iau2000a_r << endl;
      cout << "New J2000 " << AipsrcBool::get(iau2000_r) << ", " <<
	"J2000A " << AipsrcBool::get(iau2000a_r) << endl;
      SEPAR();
    }

  } catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } catch (...) {
    cerr << "Exception not derived from AipsError" << endl;
    cout << "FAIL" << endl;
    return 2;
  };

  cout << "OK" << endl;
  return 0;
}
