//# tRegex.cc: Test program for the Regex class
//# Copyright (C) 1993,1994,1995,1996,1999,2000,2001
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

#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
//# Forward Declarations

void a();
void b();

// <summary> Test program for the Regex class </summary>

// This program tests the class Regex.
// It also tests if a Vector of Regex objects works fine.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.

int main () {
  try {
    a();
    b();
  } catch (AipsError x) {
    cout << x.getMesg() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}

// Make sure the return value for a non-match is the same
// for 32 and 64 bit machines.
String::size_type doMatch (const Regex& exp, const char* str, uInt pos)
{
  String::size_type k = exp.match (str, pos);
  if (k == String::npos) {
    return 4294967295u;
  }
  return k;
}

// First do some simple Regex things.
void a() {
    Regex exp("a?bcd(bcdcdd)?");
    cout << doMatch (exp, "bdc",3) << " ";
    cout << doMatch (exp, "abcd",3) << " ";
    cout << doMatch (exp, "abcd",4) << " ";
    cout << doMatch (exp, "abcdbcdcdd",10) << endl;

    cout << doMatch (RXalpha, "bcd",0) << " ";
    cout << doMatch (RXalpha, "bcd",1) << " ";
    cout << doMatch (RXalpha, "bcd",2) << " ";
    cout << doMatch (RXalpha, "bcd",3) << " ";
    cout << doMatch (RXalpha, "bcd",4) << " ";
    cout << doMatch (RXalpha, "bcd",100) << endl;

    cout << String("1").matches(RXdouble) << " ";
    cout << String("-1").matches(RXdouble) << " ";
    cout << String("+1").matches(RXdouble) << " ";
    cout << String("1.").matches(RXdouble) << " ";
    cout << String("1.1").matches(RXdouble) << " ";
    cout << String(".1").matches(RXdouble) << " ";
    cout << String("1.e1").matches(RXdouble) << " ";
    cout << String("1.1e+1").matches(RXdouble) << " ";
    cout << String(".1E-1").matches(RXdouble) << " ";
    cout << String(".1.e-1").matches(RXdouble) << " ";
    cout << endl;

    Regex exp2(exp);
    cout << doMatch(exp2, "abcdbcdcdd",10) << endl;
    cout << String("abcdbcdcdd").matches(exp2) << " ";
    cout << String("abcdb").matches(exp2) << " ";
    cout << String("abcd").matches(exp2) << " ";
    cout << String("bcd").matches(exp2) << "   ";
    cout << exp2.regexp() << endl;

    Regex exp5(".+");
    cout << doMatch (exp5, "",0) << " ";
    cout << doMatch (exp5, "",1) << " ";
    cout << doMatch (exp5, "",2) << " ";
    cout << doMatch (exp5, "",10) << " ";
    cout << doMatch (exp5, "a",1) << " ";
    cout << doMatch (exp5, "a",2) << " ";
    cout << doMatch (exp5, "\0\0",2) << endl;

    Vector<Regex> vec(3);
    vec(0) = exp;
    vec(1) = exp5;
    vec(2) = RXalpha;
    Vector<Int> veci(10);
    for (Int i=0; i<10; i++) veci(i) = i;
    cout << vec << endl;
    cout << veci << endl;

    exp5 = exp2;
    cout << doMatch (exp5, "abcdbcdcdd",10) << endl;
    cout << String("abcdbcdcdd").matches(exp5) << " ";
    cout << String("abcdb").matches(exp5) << " ";
    cout << String("abcd").matches(exp5) << "   ";
    cout << String("bcd").matches(exp5) << " ";
    cout << exp5.regexp() << endl;

    cout << "end of a" << endl;
}

// Do some more fancy things.
void b() {
    Regex exp5("a?bcd(bcdcdd)?");
    Regex exp2(".+");
    cout << exp5.regexp() << "   " << exp2.regexp() << endl;
    Vector<Regex> vec(3);
    vec(0) = exp5;
    vec(1) = exp2;
    vec(2) = RXalpha;
    Vector<Int> veci(10);
    for (Int i=0; i<10; i++) veci(i) = i;
    cout << vec << endl;
    cout << veci << endl;

    cout << doMatch (exp5, "abcdbcdcdd",10) << endl;
    cout << String("abcdbcdcdd").matches(exp5) << " ";
    cout << String("abcdb").matches(exp5) << " ";
    cout << String("abcd").matches(exp5) << "   ";
    cout << String("bcd").matches(exp5) << "   ";
    cout << exp5.regexp() << endl;

    cout << "end of b" << endl;
}
