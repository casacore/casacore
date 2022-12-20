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

//# Includes

#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>
#include <sstream>

#include <casacore/casa/namespace.h>
//# Forward Declarations

// <summary> Test program for the Regex class </summary>


// First do some simple Regex things.
void testBasic()
{
    // Do some basic tests for a simple regex.
    Regex exp("a?bcd(bcdcdd)?");
    AlwaysAssertExit (exp.match ("bdc",3) == String::npos);
    AlwaysAssertExit (exp.match ("abcd",3) == String::npos);
    AlwaysAssertExit (exp.match ("abcd",4) == 4);
    AlwaysAssertExit (exp.match ("abcdbcdcdd",10) == 10);
    AlwaysAssertExit (exp.match ("abcdbcdcdd",10) == 10);
    AlwaysAssertExit (exp.match ("bcdxxx", 6) == 3);
    AlwaysAssertExit (! exp.fullMatch ("bdc",3));
    AlwaysAssertExit (! exp.fullMatch ("abcd",3));
    AlwaysAssertExit (exp.fullMatch ("abcd",4));
    AlwaysAssertExit (exp.fullMatch ("abcdbcdcdd",10));
    AlwaysAssertExit (! exp.fullMatch ("bcdxxx", 6));

    // Test with a given start position (from start and from end).
    // Test valid start positions.
    AlwaysAssertExit (exp.match ("abcde",5,0) == 4);
    AlwaysAssertExit (exp.match ("abcde",5,1) == 3);
    AlwaysAssertExit (exp.match ("abcde",5,2) == String::npos);
    AlwaysAssertExit (exp.match ("abcde",5,-5) == 4);
    AlwaysAssertExit (exp.match ("abcde",5,-4) == 3);
    AlwaysAssertExit (exp.match ("abcde",5,-3) == String::npos);
    // Test invalid start positions.
    AlwaysAssertExit (exp.match ("abcde",5,-6) == String::npos);
    AlwaysAssertExit (exp.match ("abcde",5,5) == String::npos);

    // Test some predefined regex-es.
    AlwaysAssertExit (RXalpha.match ("bcd",0) == String::npos);
    AlwaysAssertExit (RXalpha.match ("bcd",1) == 1);
    AlwaysAssertExit (RXalpha.match ("bcd",2) == 2);
    AlwaysAssertExit (RXalpha.match ("bcd",3) == 3);
    AlwaysAssertExit (RXalpha.match ("bcd",4) == 3);
    // Also test the String's matches function for a regex.
    AlwaysAssertExit (String("1").matches(RXdouble));
    AlwaysAssertExit (String("-1").matches(RXdouble));
    AlwaysAssertExit (String("+1").matches(RXdouble));
    AlwaysAssertExit (String("1.").matches(RXdouble));
    AlwaysAssertExit (String("1.1").matches(RXdouble));
    AlwaysAssertExit (String(".1").matches(RXdouble));
    AlwaysAssertExit (String("1.e1").matches(RXdouble));
    AlwaysAssertExit (String("1.1e+1").matches(RXdouble));
    AlwaysAssertExit (String(".1E-1").matches(RXdouble));
    AlwaysAssertExit (! String(".1.e-1").matches(RXdouble));

    // Some more basic tests using the copy constructor.
    Regex exp2(exp);
    AlwaysAssertExit (exp2.match ("abcdbcdcdd",10) == 10);
    AlwaysAssertExit (String("abcdbcdcdd").matches(exp2));
    AlwaysAssertExit (! String("abcdb").matches(exp2));
    AlwaysAssertExit (String("abcd").matches(exp2));
    AlwaysAssertExit (String("bcd").matches(exp2));
    AlwaysAssertExit (exp2.regexp() == "a?bcd(bcdcdd)?");

    // The same using the assignment operator.
    Regex exp3("any");
    exp3 = exp2;
    AlwaysAssertExit (exp3.match ("abcdbcdcdd",10) == 10);
    AlwaysAssertExit (String("abcdbcdcdd").matches(exp3));
    AlwaysAssertExit (! String("abcdb").matches(exp3));
    AlwaysAssertExit (String("abcd").matches(exp3));
    AlwaysAssertExit (String("bcd").matches(exp3));
    AlwaysAssertExit (exp3.regexp() == "a?bcd(bcdcdd)?");

    // Arbitrary strings match.
    Regex exp5(".+");
    AlwaysAssertExit (exp5.match ("", 0) == String::npos);
    AlwaysAssertExit (exp5.match ("", 1) == 1);
    AlwaysAssertExit (exp5.match ("a", 1) == 1);
    AlwaysAssertExit (exp5.match ("0123456789", 10) == 10);
    AlwaysAssertExit (exp5.match ("a", 2) == 2);
    AlwaysAssertExit (exp5.match ("\0\0", 2) == 2);

    // Check that an empty string matches .* but not .+
    AlwaysAssertExit (Regex(".*").match ("", 0) == 0);
    AlwaysAssertExit (Regex(".+").match ("", 0) == String::npos);
    AlwaysAssertExit (Regex(".*").fullMatch ("", 0));
    AlwaysAssertExit (! Regex(".+").fullMatch ("", 0));

    cout << "end of testBasic" << endl;
}

// Test the output operator.
void testIO()
{
    Regex exp5("a?bcd(bcdcdd)?");
    Regex exp2(".+");
    std::ostringstream oss;
    oss << exp5;
    AlwaysAssertExit (String(oss.str()) == exp5.regexp());
    cout << "end of testIO" << endl;
}

void testSearch()
{
  Int matchlen;
  Regex exp1("abc");
  Regex exp2("(abc)+");
  AlwaysAssertExit (exp1.search("12abc345", 8, matchlen) == 2);
  AlwaysAssertExit (matchlen == 3);
  AlwaysAssertExit (exp1.search("abcabc345", 9, matchlen) == 0);
  AlwaysAssertExit (matchlen == 3);
  AlwaysAssertExit (exp1.search("ab345", 5, matchlen) == String::npos);
  AlwaysAssertExit (exp2.search("12abc345", 8, matchlen) == 2);
  AlwaysAssertExit (matchlen == 3);
  AlwaysAssertExit (exp2.search("abcabc345", 9, matchlen) == 0);
  AlwaysAssertExit (matchlen == 6);
  AlwaysAssertExit (exp2.search("ab345", 5, matchlen) == String::npos);

  // Test search backwards.
  AlwaysAssertExit (exp1.searchBack("12abc345", 8, matchlen, 0) == 2);
  AlwaysAssertExit (matchlen == 3);
  AlwaysAssertExit (exp1.searchBack("12abcabc345", 8, matchlen, 0) == 5);
  AlwaysAssertExit (matchlen == 3);
  AlwaysAssertExit (exp2.searchBack("12abc345", 8, matchlen, 0) == 2);
  AlwaysAssertExit (matchlen == 3);
  AlwaysAssertExit (exp2.searchBack("12abcabc345", 8, matchlen, 0) == 5);
  AlwaysAssertExit (matchlen == 3);

  cout << "end of testSearch" << endl;
}

// Test a Regex in parallel.
void testParallel()
{
#ifdef HAVE_OPENMP
#pragma omp parallel
#endif
  for (int i=0; i<32; ++i) {
    Regex rx(".*");
    AlwaysAssert (String("ab").matches(rx), AipsError);
  }
}


int main () {
  try {
    // Test parallel first to ensure initialization works fine.
    testParallel();
    // Test some specific pattern conversions.
    AlwaysAssertExit (Regex::toEcma("[][]") == String("[\\]\\[]"));
    AlwaysAssertExit (Regex::toEcma("[\\]\\[]") == String("[\\]\\[]"));
    AlwaysAssertExit (Regex::toEcma("[[:alpha:]]") == String("[[:alpha:]]"));
    AlwaysAssertExit (Regex::toEcma("(the)\\15a") == String("(the)\\1[5]a"));
    // Test the Regex functions.
    testBasic();
    testIO();
    testSearch();
  } catch (const std::exception& x) {
    cout << x.what() << endl;
    return 1;
  }
  return 0;                           // exit with success status
}
