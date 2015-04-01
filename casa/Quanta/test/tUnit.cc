//# tUnit.cc: test program for Unit section of Measures module
//# Copyright (C) 1994-1996,1998-2000,2002,2008
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

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/Quanta/UnitMap.h>
#include <casacore/casa/Quanta/UnitVal.h>
#include <casacore/casa/Quanta/UnitName.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main () {
    try {
	cout << "Test units class (Unit)..." << endl;
	cout << "--------------------------" << endl;

	UnitName localName;
	UnitVal localVal;

	cout << "Define user unit symbols:" << endl;
	cout << " beam = 0.1*pi \"_2  ; Jypb = 1 Jy/beam" << endl;
	
	UnitMap::putUser("beam",UnitVal(C::pi * 0.1,"\"_2"),"telescope beam");
	UnitName jypb("Jypb",UnitVal(1.,"Jy/beam"),"Jansky/beam");
	UnitMap::putUser(jypb);
	
	cout << endl << "--------------------------" << endl;
	cout << "List all defined symbols" << endl << endl;
	
	UnitMap::list();
	
	cout << endl << "--------------------------" << endl;
	cout << "List contents of Cache" << endl << endl;
	
	UnitMap::listCache();
	
	cout << endl << "--------------------------" << endl;
	cout << "Check if prefixes known" << endl << endl;

	cout << "'k' prefix = " << UnitMap::getPref("k",localName) <<
	    " ( " << localName << ")" << endl;
	cout << "'K' prefix = " << UnitMap::getPref("K",localName) <<
	    " ( " << localName << ")" << endl;

	cout << endl << "--------------------------" << endl;
	cout << "Check if cached unit" << endl << endl;

	localVal = UnitVal();
	cout << "'Jy/beam' = " <<
	    UnitMap::getCache("Jy/beam",localVal) <<
	    " ( " << localVal << ")" << endl;
	cout << "'Jy/beam' = " <<
	    UnitMap::getCache("Jy/beam",localVal) <<
	    " ( " << localVal << ")" << endl;
	localVal = UnitVal();
	cout << "'JY/beam' = " <<
	    UnitMap::getCache("JY/beam",localVal) <<
	    " ( " << localVal << ")" << endl;
	cout << "'JY/beam' = " <<
	    UnitMap::getCache("JY/beam",localVal) <<
	    " ( " << localVal << ")" << endl;
	
	cout << endl << "--------------------------" << endl;
	cout << "Check if known unit" << endl << endl;

	cout << "'Jy' = " <<
	    UnitMap::getUnit("Jy",localName) <<
	    " ( " << localName << ")" << endl;
	cout << "'JY' = " <<
	    UnitMap::getUnit("JY",localName) <<
	    " ( " << localName << ")" << endl;

	cout << endl << "--------------------------" << endl;
	cout << "Get value and name from a unit name" << endl << endl;

	cout << "Jypb: Value = " << jypb.getVal() << " Name = " <<
	    jypb.getName() << endl;

	cout << endl << "--------------------------" << endl;
	cout << "Create and manipulate a unit value" << endl << endl;

	UnitVal myVal(3.2,"W");
	UnitVal myVal1(2.1,"mg");
	UnitVal myVal2 = myVal;

	cout << "A            = UnitVal(3.2,\"W\") = " << myVal << endl;
	cout << "B            = UnitVal(2.1,\"mg\") = " << myVal1 << endl;
	cout << "A*B          = " << myVal*myVal1 << endl;
	cout << "2*A          = " << 2*myVal << endl;
	cout << "A*2          = " << myVal*2 << endl;
	cout << "A/B          = " << myVal/myVal1 << endl;	cout << "A^3          = " << myVal.pow(3) << endl;
	cout << "A^-2         = " << myVal.pow(-2) << endl;
	cout << "A==B         = " << (myVal==myVal1) << endl;
	cout << "A==A         = " << (myVal==myVal) << endl;
	cout << "A!=B         = " << (myVal!=myVal1) << endl;
	cout << "check(A)     = " << UnitVal::check("W") << endl;
	cout << "check(\"KpH\") = " << UnitVal::check("KpH",myVal2) <<
	    " = " << myVal2 << endl;
	cout << "factor(A)    = " << myVal.getFac() << endl;
	cout << "dim(A)       = " << myVal.getDim() << endl;

	cout << endl << "--------------------------" << endl;
	cout << "Test FITS values" << endl << endl;

	cout << "User list before FITS:" << endl;
	UnitMap::listUser();
	UnitMap::removeUser("beam");	
	UnitMap::addFITS();
	cout << "User list after FITS:" << endl;
	UnitMap::listUser();	
	UnitVal myF1(4.0,"mJY/BEAM");
	cout << "A = 4 mJY/BEAM   : " << myF1 << endl;

	Unit Fstr("M(JY5/SEC2.(YEAR.HZ).KM)");
	cout << "A FITS unit to translate: " << Fstr.getName() << endl;
	cout << "it translates to:         " << 
	  UnitMap::fromFITS(Fstr).getName() << endl;
	cout << "or:                       " << Fstr.getValue() << endl;
	Unit Fstr2(UnitMap::fromFITS(Fstr));
	cout << "and back to:              " << 
	  UnitMap::toFITS(Fstr2).getName() << endl;
	cout << "or:                       " << Fstr2.getValue() << endl;

	Fstr = Unit("M(JY5/SEC**2.(YEAR*HZ)*KM)");
	cout << "A FITS unit to translate: " << Fstr.getName() << endl;
	cout << "it translates to:         " << 
	  UnitMap::fromFITS(Fstr).getName() << endl;
	cout << "or:                       " << Fstr.getValue() << endl;
	Fstr2 = (UnitMap::fromFITS(Fstr));
	cout << "and back to:              " << 
	  UnitMap::toFITS(Fstr2).getName() << endl;
	cout << "or:                       " << Fstr2.getValue() << endl;

	Fstr = Unit("M(JY5/SEC^2.(YEAR*HZ).KM)");
	cout << "A FITS unit to translate: " << Fstr.getName() << endl;
	cout << "it translates to:         " << 
	  UnitMap::fromFITS(Fstr).getName() << endl;
	cout << "or:                       " << Fstr.getValue() << endl;
	Fstr2 = (UnitMap::fromFITS(Fstr));
	cout << "and back to:              " << 
	  UnitMap::toFITS(Fstr2).getName() << endl;
	cout << "or:                       " << Fstr2.getValue() << endl;

	UnitMap::clearFITS();
	cout << "User list after FITS removal:" << endl;
	UnitMap::listUser();	

    } catch (AipsError x) {
	cout << "Unexpected: " << x.getMesg() << endl;
    } 
    
    cout << endl << "--------------------------" << endl;
    cout << "Try illegal Unit values" << endl << endl;

    try {
	Unit ca="Kpm/s";
    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 

    try {
	UnitVal errval(2.,"KpH");
    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 
    
    cout << endl << "--------------------------" << endl;
    
    return(0);
}
