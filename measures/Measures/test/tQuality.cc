//# tQuality.cc: This program tests Quality interface class to table data
//# Copyright (C) 1994,1995,2000,2001
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

//# Includes

#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/Quality.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

void roundtrip(Int &int_in, Int &int_out);
Int check_str_type(String &qualstr);

int main() {

	try {
		Int qualint;
		Int myInt;
		String qualstr;

		// try a round trip for the first QualityType
		qualint=1;
		roundtrip(qualint, myInt);
		AlwaysAssert(myInt == qualint, AipsError)

		// try a round trip for the second  QualityType
		qualint=2;
		roundtrip(qualint, myInt);
		AlwaysAssert(myInt == qualint, AipsError)

		// try a round trip for the third,
		// non-existing QualityType
		qualint=3;
		roundtrip(qualint, myInt);
		AlwaysAssert(myInt == 0, AipsError)

		// a type -1 does not exist
		// but should not shock the system
		qualint=-1;
		roundtrip(qualint, myInt);
		AlwaysAssert(myInt == 0, AipsError)

		// make sure there is the type "DATA"
		qualstr="DATA";
		qualint = check_str_type(qualstr);
		AlwaysAssert(qualint, AipsError);

		// make sure there is the type "ERROR
		qualstr="ERROR";
		qualint = check_str_type(qualstr);
		AlwaysAssert(qualint, AipsError);

		// make sure that everything else is zero
		qualstr="whatever else";
		qualint = check_str_type(qualstr);
		AlwaysAssert(!qualint, AipsError);


		// make sure there are (currently) just three types
		// two defined plus the undefined one
		AlwaysAssert(Quality::NumberOfTypes==3, AipsError);

		// check the functioning of in/excluding the
		// undefined type
		AlwaysAssert(Quality::allNames(False).size() == Quality::NumberOfTypes - 1, AipsError);
		AlwaysAssert(Quality::allNames(True).size() == Quality::NumberOfTypes, AipsError);

		// just some eye-candy: present all names
		Vector<String> allNames = Quality::allNames(True);
		cout << "All names: ";
		for (uInt i=0; i<allNames.size(); i++) {
			cout << allNames[i] << " ";
		}
		cout << endl;

		cout << "ok" << endl;
	}
	catch (AipsError x) {
		cout << "fail "<< x.getMesg()<<endl;
		return 1;
	}
	return 0;
}


void roundtrip(Int &int_in, Int &int_out){
	Quality::QualityTypes myType;
	String myTypeName;

	// try a round trip for the given int
	myType = Quality::type(int_in);
	myTypeName  = Quality::name(myType);
	int_out = Quality::type(myTypeName);
	cout << "int_in =  " << int_in
			<< " --> type: " << myType
			<< " --> name: " << myTypeName
			<< " --> int_out:  " << int_out <<  endl;
}

Int check_str_type(String &qualstr){
	Quality::QualityTypes myType;
	String myTypeName;

	// convert the string to a type
	// and back to string again
	myType = Quality::type(qualstr);
	myTypeName = Quality::name(myType);

	// output all
	cout << "name: =  " << qualstr
			<< " --> type: " << myType
			<< " --> name: " << myTypeName
			<< endl;

	// return just the type
	return (Int)myType;
}
