//# dMUString.cc: test of MUString class
//# Copyright (C) 1996,1997,1998
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
#include <aips/Exceptions/Error.h>
#include <aips/Measures/Quantum.h>
#include <aips/Measures/MVAngle.h>
#if defined(__GNUG__)
typedef Quantum<Double> gpp_dmustringbug_1;
#endif

main()
{
    try {
	cout << "Demonstrate MVAngle input  " << endl;
	cout << "----------------------------------------------------" << endl;
	String in = "12.13.15.9";
	Quantity res;
	cout << MVAngle::read(res, in) << endl;
	cout << in << " = " << res << endl;
	in = "12d13m15.9";
	cout << MVAngle::read(res, in) << endl;
	cout << in << " = " << res << endl;
	in = "12h13m15.9";
	cout << MVAngle::read(res, in) << endl;
	cout << in << " = " << res << endl;
	in = "12d15.9";
	cout << MVAngle::read(res, in) << endl;
	cout << in << " = " << res << endl;
	in = "12:13:15.9";
	cout << MVAngle::read(res, in) << endl;
	cout << in << " = " << res << endl;
 	in = "12:15.9";
	cout << MVAngle::read(res, in) << endl;
	cout << in << " = " << res << endl;
	in = "15.9";
	cout << MVAngle::read(res, in) << endl;
	cout << in << " = " << res << endl;
	in = "15.9 \"";
	cout << MVAngle::read(res, in) << endl;
	cout << in << " = " << res << endl;
	in = "15.9 '";
	cout << MVAngle::read(res, in) << endl;
	cout << in << " = " << res << endl;
	in = "15.9 s";
	cout << MVAngle::read(res, in) << endl;
	cout << in << " = " << res << endl;
	in = "15 s";
	cout << MVAngle::read(res, in) << endl;
	cout << in << " = " << res << endl;
   } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } end_try;

     try {
 	String in = "12.13.15.9p";
	Quantity res;
	cout << MVAngle::read(res, in) << endl;
	cout << in << " = " << res << endl;
  } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } end_try;

     try {
       String in = "12.5pm";
       String bb;
       Int ptr = 0;
       Int l = in.length();
       Double res = 0.0;
       if (ptr < l) {
	 String loc0 = in;		// non-const string
	 Int p = in.index(Regex("[ 	]"),ptr);
	 p = (p<0) ? l : p;
	 String loc = loc0.at(ptr,p-ptr);
	 ptr = p;
	 istrstream instr(loc);
	 streampos stt(instr.tellg());
	 cout << "Pos0: " << instr.tellg()-stt << instr.rdstate() << endl;
	 instr >> res;
	 cout << "Pos0a:" << instr.tellg()-stt << instr.rdstate() << endl;
	 cout << "ios::failbit: " << ios::failbit << endl;
	 instr.clear(~ios::failbit & instr.rdstate());
	 cout << "Pos0b:" << instr.tellg()-stt << instr.rdstate() << endl;
	 instr >> bb;
	 cout << "Pos1: " << instr.tellg()-stt << endl;
	 cout << in << " = " << res << " : " << bb << endl;
       };
     } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } end_try;
  
   return(0);
}
