//# tRegex_1.cc: Regex test program
//# Copyright (C) 1996,1998
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

#include <aips/Utilities/String.h>
#include <aips/IO/AipsIO.h>

main (void)
{
    const ntests = 28;
    String p[ntests];
    p[0]  = "^().+|$";
    p[1]  = "\\,";
    p[2]  = "{a,{ab,cd}efg,b}";
    p[3]  = "{(),test}";
    p[4]  = "}{a,b}";
    p[5]  = "}{a,b}}";
    p[6]  = "}{a,\\}}}";
    p[7]  = "[[]";
    p[8]  = "[]]";
    p[9]  = "[]]]";
    p[10] = "[]]*";
    p[11] = "[]*]";
    p[12] = "[[]*";
    p[13] = "[[*]";
    p[14] = "dfg*";
    p[15] = "abc?vf??skkre*";
    p[16] = "[^^]";
    p[17] = "[!^]";
    p[18] = "[^!]";
    p[19] = "[!!]";
    p[20] = "[{a,b}*^[?()+|]";
    p[21] = "{a,bc*^?()+|]}";
    p[22] = "s{t{aa,bb},c{dd,ee}f}";
    p[23] = "{[{}]}";
    p[24] = "a\\,b";
    p[25] = "\n\t";
    p[26] = "\\?\\**";
    p[27] = "\\";

    cout << "Pattern --> Regular Expression" << endl;
    cout << "------------------------------" << endl;
    int i;
    for (i=0; i<ntests; i++) {
	cout << p[i] << " --> " << Regex::fromPattern(p[i]) << endl;
    }

    cout << endl << "String --> Regular Expression" << endl;
    cout << "-----------------------------" << endl;
    for (i=0; i<ntests; i++) {
	cout << p[i] << " --> " << Regex::fromString(p[i]) << endl;
    }
    return 0;
}
