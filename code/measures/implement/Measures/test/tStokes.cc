//# tStokes.cc: This program tests Stokes interface class to table data
//# Copyright (C) 1994,1995
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

#include <aips/aips.h>
#include <aips/Measures/Stokes.h>
#include <aips/Utilities/String.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Exceptions/Error.h>

int main() 
{
    
    Int polint;
    String polstr;
    
    polint=5;
    cout << "polint = " << polint
	<< ", Stokes::type(polint) = " << Stokes::type(polint)
	<<",  Stokes::type(Stokes::type(polint)) = "
	<< Stokes::type(Stokes::type(polint)) 
	<< endl  
	<<  " --- receptor1 = "<<Stokes::receptor1(Stokes::type(polint)) 
	<<  " --- receptor2 = "<<Stokes::receptor2(Stokes::type(polint)) 
	<< endl;

    polint=-1;
    try {
	cout << "polint = " << polint;
	cout << ", Stokes::type(polint) = " << Stokes::type(polint);
	cout << ", Stokes::type(Stokes::type(polint)) = ";
	cout << Stokes::type(Stokes::type(polint));
	cout << endl;
	cout <<  " --- receptor1 = ";
	cout << Stokes::receptor1(Stokes::type(polint));
	cout <<  " --- receptor2 = ";
	cout << Stokes::receptor2(Stokes::type(polint));
	cout << endl;
    } catch(AipsError x) {
	cout << " Caught exception of receptor correctly: "<<x.getMesg()<<endl;
    } end_try;
    
    polstr="XY";
    cout << "polstr = " << polstr
	 << ", Stokes::type(polstr) = " << Stokes::type(polstr)
	 << ", Stokes::type(Stokes::type(polstr)) = "
         << Stokes::type(Stokes::type(polstr))
         << endl  
	 <<  " --- receptor1 = "<<Stokes::receptor1(Stokes::type(polstr)) 
	 <<  " --- receptor2 = "<<Stokes::receptor2(Stokes::type(polstr)) 
	 << endl;

    polstr="LX";
    cout << "polstr = " << polstr
         << ", Stokes::type(polstr) = " << Stokes::type(polstr)
         << ", Stokes::type(Stokes::type(polstr)) = "
         << Stokes::type(Stokes::type(polstr))
         << endl  
	 <<  " --- receptor1 = "<<Stokes::receptor1(Stokes::type(polstr)) 
	 <<  " --- receptor2 = "<<Stokes::receptor2(Stokes::type(polstr)) 
	 << endl;
    
	     
    try {
    polstr="AB";
    cout << "polstr = " << polstr;
    cout << ", Stokes::type(polstr) = " << Stokes::type(polstr);
    cout << ", Stokes::type(Stokes::type(polstr)) = ";
    cout << Stokes::type(Stokes::type(polstr));
    cout << endl  ;
    cout <<  " --- receptor1 = ";
    cout << Stokes::receptor1(Stokes::type(polstr)) ;
    cout <<  " --- receptor2 = ";
    cout << Stokes::receptor2(Stokes::type(polstr)) ;
    cout << endl;
    } catch(AipsError x) {
	cout << " Caught exception of receptor correctly: "<<x.getMesg()<<endl;
    } end_try;
    for (uInt i=0;i<Stokes::NumberOfTypes;i++) {
	if (Stokes::fromFITSValue(Stokes::FITSValue(Stokes::type(i)))
	    != Stokes::type(i)) {
	    cerr << "Stokes FITS value conversion failed" << endl;
	    cerr << "  FITSValue(" << i << ") = " << Stokes::FITSValue(Stokes::type(i)) << endl;
	    cerr << "  fromFITSValue of that value = "
		 << Stokes::fromFITSValue(Stokes::FITSValue(Stokes::type(i))) << endl;
	}
    }
 return 0;
}
