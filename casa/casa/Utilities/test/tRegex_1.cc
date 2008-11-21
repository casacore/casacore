//# tRegex_1.cc: Regex test program
//# Copyright (C) 1996,1998,2000,2001,2003
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

#include <casa/BasicSL/String.h>
#include <casa/Utilities/Regex.h>
#include <casa/Utilities/Assert.h>
#include <casa/iostream.h>

#include <casa/namespace.h>

int main ()
{
    const Int ntests = 29;
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
    p[27] = "\\\\";
    p[28] = "__3%a%*";

    cout << "Pattern --> Regular Expression" << endl;
    cout << "------------------------------" << endl;
    int i;
    for (i=0; i<ntests; i++) {
	cout << p[i] << " --> " << Regex::fromPattern(p[i]) << endl;
    }

    cout << endl << "SQLPattern --> Regular Expression" << endl;
    cout << "---------------------------------" << endl;
    for (i=0; i<ntests; i++) {
	cout << p[i] << " --> " << Regex::fromSQLPattern(p[i]) << endl;
    }

    cout << endl << "String --> Regular Expression" << endl;
    cout << "-----------------------------" << endl;
    for (i=0; i<ntests; i++) {
	cout << p[i] << " --> " << Regex::fromString(p[i]) << endl;
        Regex exp(Regex::fromString(p[i]));
        AlwaysAssertExit (String(p[i]).matches(exp));
    }

#define CHECKPATT(i,str) \
     AlwaysAssertExit (String(str).matches (Regex(Regex::fromPattern(p[i]))))
#define CHECKNPATT(i,str) \
     AlwaysAssertExit (!String(str).matches (Regex(Regex::fromPattern(p[i]))))

    CHECKPATT (0,  "^().+|$");
    CHECKPATT (1,  ",");
    CHECKPATT (2,  "a");
    CHECKPATT (2,  "abefg");
    CHECKPATT (2,  "cdefg");
    CHECKNPATT(2,  "ab");
    CHECKNPATT(2,  "abdefg");
    CHECKPATT (3,  "()");
    CHECKPATT (3,  "test");
    CHECKPATT (4,  "}a");
    CHECKPATT (5,  "}a}");
    CHECKNPATT(5,  "}a");
    CHECKPATT (6,  "}a}");
    CHECKPATT (6,  "}}}");
    CHECKNPATT(6,  "}a");
    CHECKNPATT(6,  "}}");
    CHECKPATT (7,  "[");
    CHECKPATT (8,  "]");
    CHECKPATT (9,  "]]");
    CHECKNPATT(9,  "]");
    CHECKPATT (10, "]]]]]");
    CHECKPATT (11, "]");
    CHECKPATT (11, "*");
    CHECKNPATT(11, "**");
    CHECKNPATT(11, "]*");
    CHECKPATT (12, "[");
    CHECKPATT (12, "[[[[");
    CHECKPATT (13, "[");
    CHECKPATT (13, "*");
    CHECKNPATT(13, "[*");
    CHECKPATT (14, "dfg");
    CHECKPATT (14, "dfgxyz");
    CHECKNPATT(14, "df");
    CHECKPATT (15, "abcxvfxxskkrexxx");
    CHECKPATT (16, "x");
    CHECKNPATT(16, "^");
    CHECKPATT (17, "x");
    CHECKNPATT(17, "^");
    CHECKPATT (18, "x");
    CHECKNPATT(18, "!");
    CHECKPATT (19, "x");
    CHECKNPATT(19, "!");
    CHECKPATT (20, "{");
    CHECKPATT (20, "a");
    CHECKPATT (20, ",");
    CHECKPATT (20, "b");
    CHECKPATT (20, "}");
    CHECKPATT (20, "*");
    CHECKPATT (20, "^");
    CHECKPATT (20, "[");
    CHECKPATT (20, "?");
    CHECKPATT (20, "(");
    CHECKPATT (20, ")");
    CHECKPATT (20, "+");
    CHECKPATT (20, "|");
    CHECKNPATT(20, "x");
    CHECKPATT (21, "a");
    CHECKPATT (21, "bc^x()+|]");
    CHECKPATT (21, "bcxxx^x()+|]");
    CHECKNPATT(21, "bcxxx^xx)+|]");
    CHECKPATT (22, "staa");
    CHECKPATT (22, "stbb");
    CHECKPATT (22, "scddf");
    CHECKPATT (22, "sceef");
    CHECKNPATT(22, "scee");
    CHECKPATT (23, "{");
    CHECKPATT (23, "}");
    CHECKNPATT(23, "x");
    CHECKPATT (24, "a,b");
    CHECKPATT (25, "\n\t");
    CHECKNPATT(25, "\n ");
    CHECKPATT (26, "?*");
    CHECKPATT (26, "?*xx");
    CHECKNPATT(26, "x*xx");
    CHECKNPATT(26, "?xxx");
    CHECKPATT (27, "\\");
    CHECKPATT (28, "__3%a%");

    return 0;
}
