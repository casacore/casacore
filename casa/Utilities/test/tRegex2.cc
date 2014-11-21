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

// <summary> Test program for the Regex class </summary>

// This program tests the class Regex.
// It also tests if a Vector of Regex objects works fine.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.

int main () {
  try {
    a();
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
  //  Regex exp(Regex::fromPattern("a"));
  Regex exp22(Regex::fromPattern("[CR]T[^ab]*"));
  cout << "RTa " << String("RTa").matches(exp22) << endl;
  cout << "RTb " << String("RTb").matches(exp22) << endl;
  cout << "RTc " << String("RTc").matches(exp22) << endl;
  Regex exp(Regex::fromPattern("{a,b,c,d,e,f,g}"));
  cout << String("a").matches(exp) << endl;
  cout << String("b").matches(exp) << endl;
  cout << String("c").matches(exp) << endl;
  cout << String("d").matches(exp) << endl;
  cout << String("e").matches(exp) << endl;
  cout << String("f").matches(exp) << endl;
  cout << String("g").matches(exp) << endl;
  Regex exp1(Regex::fromPattern("x@{a,b}@y@{c,d}@{h,i}"));
  cout << "exp1  " << String("x@a@y@c@h").matches(exp1) << endl;
  Regex exp1b("xx@(aa|bb)@yy@(cc|dd|ee|ff|gg)@(hh|ii)");
  cout << "exp1b " << String("xx@aa@yy@cc@hh").matches(exp1b) << endl;
  cout << "exp1b " << String("xx@aab@yy@cceegg@hhi").matches(exp1b) << endl;
  Regex exp1c("xx@(a(a|b)b)@yy@(c(c|d)(d|e)(e|f)(f|g)g)@(h(h|i)i)");
  cout << "exp1c " << String("xx@aa@yy@cc@hh").matches(exp1c) << endl;
  cout << "exp1c " << String("xx@aab@yy@cceegg@hhi").matches(exp1c) << endl;
  Regex exp1a("xx@((aa)|(bb))@yy@((cc)|(dd)|(ee)|(ff)|gg)@((hh)|(ii))");
  cout << "exp1a " << String("xx@aa@yy@cc@hh").matches(exp1a) << endl;
  cout << Regex::fromPattern("Gain:{11,22}:Real:{CS010_HBA0,CS010_HBA1,CS010_HBA2,CS008_HBA0,CS010_HBA3}:{CasA,CygA}") << endl;
  cout << Regex::fromPattern("Gain:{11,22}:Real:{CS010_HBA0,CS010_HBA1,CS010_HBA2,CS010_HBA3}:{CasA,CygA}") << endl;
  Regex exp2(Regex::fromPattern("Gain:{11,22}:Real:{CS010_HBA0,CS010_HBA1,CS010_HBA2,CS010_HBA3}:{CasA,CygA}"));
  cout << String("Gain:11:Real:CS010_HBA0:CasA").matches(exp2) << endl;
}
