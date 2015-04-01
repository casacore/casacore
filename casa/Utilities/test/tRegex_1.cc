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

#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

int main ()
{
    const Int ntests = 31;
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
    p[14] = "DfG*";
    p[15] = "abc?vf??skkre*";
    p[16] = "[^^]";
    p[17] = "[!^]";
    p[18] = "[^!]";
    p[19] = "[!!]";
    p[20] = "[{a,b}*^[?()+|]";
    p[21] = "{a,bc*^?()+|]}";
    p[22] = "s{t{aa,bb},c{dd,ee}f}";
    p[23] = "{[{,}]}";
    p[24] = "a\\,b";
    p[25] = "\n\t";
    p[26] = "\\?\\**";
    p[27] = "\\\\";
    p[28] = "__3%a%*";
    p[29] = "*.{h,hpp,c,cc,cpp}";
    p[30] = "*.{[hc]{,pp},cc}";

    cout << "Pattern --> Regular Expression" << endl;
    cout << "------------------------------" << endl;
    int i;
    for (i=0; i<ntests; i++) {
	cout << p[i] << " --> " << Regex::fromPattern(p[i]) << endl;
    }

    cout << endl << "Pattern --> Case Insensitive Regular Expression" << endl;
    cout << "-----------------------------------------------" << endl;
    for (i=0; i<ntests; i++) {
	cout << p[i] << " --> "
             << Regex::makeCaseInsensitive(Regex::fromPattern(p[i])) << endl;
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
        AlwaysAssertExit (p[i].matches(exp));
    }

#define CHECKPATT(i,str) \
     AlwaysAssertExit (String(str).matches (Regex(Regex::fromPattern(p[i]))))
#define CHECKNPATT(i,str) \
     AlwaysAssertExit (!String(str).matches (Regex(Regex::fromPattern(p[i]))))
#define CHECKPATTCI(i,str) \
    AlwaysAssertExit (String(str).matches (Regex(Regex::makeCaseInsensitive(Regex::fromPattern(p[i])))))
#define CHECKNPATTCI(i,str) \
    AlwaysAssertExit (!String(str).matches (Regex(Regex::makeCaseInsensitive(Regex::fromPattern(p[i])))))

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
    CHECKPATT (14, "DfG");
    CHECKPATT (14, "DfGxyz");
    CHECKNPATT(14, "Df");
    CHECKNPATT(14, "Dfg");
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
    CHECKPATT (23, ",");
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

    CHECKPATTCI (0,  "^().+|$");
    CHECKPATTCI (1,  ",");
    CHECKPATTCI (2,  "a");
    CHECKPATTCI (2,  "abefg");
    CHECKPATTCI (2,  "cdefg");
    CHECKNPATTCI(2,  "ab");
    CHECKNPATTCI(2,  "abdefg");
    CHECKPATTCI (3,  "()");
    CHECKPATTCI (3,  "test");
    CHECKPATTCI (4,  "}a");
    CHECKPATTCI (5,  "}a}");
    CHECKNPATTCI(5,  "}a");
    CHECKPATTCI (6,  "}a}");
    CHECKPATTCI (6,  "}}}");
    CHECKNPATTCI(6,  "}a");
    CHECKNPATTCI(6,  "}}");
    CHECKPATTCI (7,  "[");
    CHECKPATTCI (8,  "]");
    CHECKPATTCI (9,  "]]");
    CHECKNPATTCI(9,  "]");
    CHECKPATTCI (10, "]]]]]");
    CHECKPATTCI (11, "]");
    CHECKPATTCI (11, "*");
    CHECKNPATTCI(11, "**");
    CHECKNPATTCI(11, "]*");
    CHECKPATTCI (12, "[");
    CHECKPATTCI (12, "[[[[");
    CHECKPATTCI (13, "[");
    CHECKPATTCI (13, "*");
    CHECKNPATTCI(13, "[*");
    CHECKPATTCI (14, "dfg");
    CHECKPATTCI (14, "DFG");
    CHECKPATTCI (14, "dfgxyz");
    CHECKNPATTCI(14, "df");
    CHECKPATTCI (15, "abcxvfxxskkrexxx");
    CHECKPATTCI (16, "x");
    CHECKNPATTCI(16, "^");
    CHECKPATTCI (17, "x");
    CHECKNPATTCI(17, "^");
    CHECKPATTCI (18, "x");
    CHECKNPATTCI(18, "!");
    CHECKPATTCI (19, "x");
    CHECKNPATTCI(19, "!");
    CHECKPATTCI (20, "{");
    CHECKPATTCI (20, "a");
    CHECKPATTCI (20, ",");
    CHECKPATTCI (20, "b");
    CHECKPATTCI (20, "}");
    CHECKPATTCI (20, "*");
    CHECKPATTCI (20, "^");
    CHECKPATTCI (20, "[");
    CHECKPATTCI (20, "?");
    CHECKPATTCI (20, "(");
    CHECKPATTCI (20, ")");
    CHECKPATTCI (20, "+");
    CHECKPATTCI (20, "|");
    CHECKNPATTCI(20, "x");
    CHECKPATTCI (21, "a");
    CHECKPATTCI (21, "bc^x()+|]");
    CHECKPATTCI (21, "bcxxx^x()+|]");
    CHECKNPATTCI(21, "bcxxx^xx)+|]");
    CHECKPATTCI (22, "staa");
    CHECKPATTCI (22, "stbb");
    CHECKPATTCI (22, "scddf");
    CHECKPATTCI (22, "sceef");
    CHECKNPATTCI(22, "scee");
    CHECKPATTCI (23, "{");
    CHECKPATTCI (23, "}");
    CHECKNPATTCI(23, "x");
    CHECKPATTCI (24, "a,b");
    CHECKPATTCI (25, "\n\t");
    CHECKNPATTCI(25, "\n ");
    CHECKPATTCI (26, "?*");
    CHECKPATTCI (26, "?*xx");
    CHECKNPATTCI(26, "x*xx");
    CHECKNPATTCI(26, "?xxx");
    CHECKPATTCI (27, "\\");
    CHECKPATTCI (28, "__3%a%");
    CHECKPATTCI (29, "file.h");
    CHECKPATTCI (29, "file.hpp");
    CHECKPATTCI (29, "file.c");
    CHECKPATTCI (29, "file.cc");
    CHECKPATTCI (29, "file.cpp");
    CHECKNPATTCI(29, "file.hh");
    CHECKNPATTCI(29, "file.hp");
    CHECKNPATTCI(29, "file.cp");
    CHECKPATTCI (30, "file.h");
    CHECKPATTCI (30, "file.hpp");
    CHECKPATTCI (30, "file.c");
    CHECKPATTCI (30, "file.cc");
    CHECKPATTCI (30, "file.cpp");
    CHECKNPATTCI(30, "file.hh");
    CHECKNPATTCI(30, "file.hp");
    CHECKNPATTCI(30, "file.cp");

    AlwaysAssertExit (Regex::makeCaseInsensitive("[ab]") == "[aAbB]");
    AlwaysAssertExit (Regex::makeCaseInsensitive("[]AB]") == "[]AaBb]");
    AlwaysAssertExit (Regex::makeCaseInsensitive("[^]AB]") == "[^]AaBb]");
    AlwaysAssertExit (Regex::makeCaseInsensitive("[B\\a]b]") == "[Bb\\aA][bB]]");
    AlwaysAssertExit (Regex::makeCaseInsensitive("a-c") == "[aA]-[cC]");
    AlwaysAssertExit (Regex::makeCaseInsensitive("[a-c]") == "[a-cA-C]");
    AlwaysAssertExit (Regex::makeCaseInsensitive("[a-]\\w") == "[aA-]\\w");
    AlwaysAssertExit (Regex::fromPattern("[:alpha:]") == "[:alpha:]");
    AlwaysAssertExit (Regex::fromPattern("[[:alpha:]]") == "[[:alpha:]]");
    AlwaysAssertExit (Regex::makeCaseInsensitive("[:alpha:]") == "[:aAlLpPhHaA:]");
    AlwaysAssertExit (Regex::makeCaseInsensitive("[[:alpha:]]") == "[[:alpha:]]");
    AlwaysAssertExit (Regex::makeCaseInsensitive
      (Regex::fromPattern("[[:alpha:]x[:bb:]]")) == "[[:alpha:]xX[:bb:]]");
    // Omitting the " makes a bid difference.
    AlwaysAssertExit (Regex::makeCaseInsensitive
      (Regex::fromPattern("[[alpha:]x[:bb:]]")) == "[[aAlLpPhHaA:][xX][:bBbB:]]");

    return 0;
}
