//# tString.cc: This program tests Strings
//# Copyright (C) 1993-1999,2000,2001,2002,2003
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

#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/Utilities/Assert.h>
// Next one for atoi and atof
#include <casacore/casa/stdlib.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>

#include <casacore/casa/namespace.h>
// Generally used variables
String X = "Hello";
String Y = "world";
String N = "123";
String c;
const Char*  s = ",";
Regex  r = String("e[a-z]*o");

void decltest() {
  String x;
  cout << "an empty String:" << x << endl;
  AlwaysAssertExit(x == "");

  String y = "Hello";
  cout << "A string initialized to Hello:" << y << endl;
  AlwaysAssertExit(y == "Hello");

  if (y[y.length()-1] == 'o') y = y + '\n';
  AlwaysAssertExit(y == "Hello\n");
  y = "Hello";

  String a = y;
  cout << "A string initialized to previous string:" << a << endl;
  AlwaysAssertExit(a == "Hello");
  AlwaysAssertExit(a == y);

  String b (a.at(1, 2));
  cout << "A string initialized to previous string.at(1, 2):" << b << endl;
  AlwaysAssertExit(b == "el");

  Char ch = '@';
  String z(ch);
  cout << "A string initialized to @:" << z << endl;
  AlwaysAssertExit(z == "@");

  String n = "20";
  cout << "A string initialized to dec(20):" << n << endl;
  AlwaysAssertExit(n == "20");

  Int i = atoi(n.chars());
  Double f = atof(n.chars());
  cout << "n = " << n << " atoi(n) = " << i << " atof(n) = " << f << endl;
  AlwaysAssertExit(i == 20);
  AlwaysAssertExit(f == 20);

  /*
  AlwaysAssertExit(X.OK());
  AlwaysAssertExit(Y.OK());
  AlwaysAssertExit(x.OK());
  AlwaysAssertExit(y.OK());
  AlwaysAssertExit(z.OK());
  AlwaysAssertExit(n.OK());
  AlwaysAssertExit(r.OK());
  */
}

void cattest() {
  String x = X;
  String y = Y;
  String z = x + y;
  cout << "z = x + y = " << z << endl;
  AlwaysAssertExit(z == "Helloworld");

  x += y;
  cout << "x += y; x = " << x << endl;
  AlwaysAssertExit(x == "Helloworld");

  y = Y;
  x = X;
  y.prepend(x);
  cout << "y.prepend(x); y = " << y << endl;
  AlwaysAssertExit(y == "Helloworld");

  x = X;
  y = Y;
  z = x + s + ' ' + y.at("w") + y.after("w") + ".";
  cout << "z = x + s +  + y.at(w) + y.after(w) + . = " << z << endl;
  AlwaysAssertExit(z == "Hello, world.");
}

void comparetest() {
  String x = X;
  String y = Y;
  String n = N;
  String z = x + y;

  AlwaysAssertExit(x != y);
  AlwaysAssertExit(x == "Hello");
  AlwaysAssertExit(x != z.at(0, 4));
  AlwaysAssertExit (x < y);
  AlwaysAssertExit(!(x >= z.at(0, 6)));
  AlwaysAssertExit(x.contains("He"));
  AlwaysAssertExit (z.contains(x));
  AlwaysAssertExit(x.contains(r));
  
  AlwaysAssertExit(!(x.matches(r)));
  AlwaysAssertExit(x.matches(RXalpha));
  AlwaysAssertExit(!(n.matches(RXalpha)));
  AlwaysAssertExit(n.matches(RXint));
  AlwaysAssertExit(n.matches(RXdouble));
  
  AlwaysAssertExit(x.index("lo") == 3);
  AlwaysAssertExit(x.index("l", 2) == 2);
  AlwaysAssertExit(x.index("l", -1) == 3); // negative pos!
  AlwaysAssertExit(x.index(r)  == 1);
  AlwaysAssertExit(x.index(r, -2) == 1);

  AlwaysAssertExit(x.contains("el", 1));
  AlwaysAssertExit(x.contains("el"));
  
  AlwaysAssertExit(common_prefix(x, "Help") == "Hel");
  AlwaysAssertExit(common_suffix(x, "to") == "o");
  
  AlwaysAssertExit(fcompare(x, "hello") == 0);
  AlwaysAssertExit(fcompare(x, "hellox") < 0);
  AlwaysAssertExit(fcompare(x, "hell") > 0);
  AlwaysAssertExit(fcompare(x, "hELlo") == 0);
  AlwaysAssertExit(fcompare(x, "hElp") < 0);
}

void substrtest() {
  String x = X;

  Char ch = x[0];
  cout << "ch = x[0] = " << ch << endl;
  AlwaysAssertExit(ch == 'H');

  String z = x.at(2, 3);
  cout << "z = x.at(2, 3) = " << z << endl;
  AlwaysAssertExit(z.length() == 3);
  AlwaysAssertExit(z == "llo");

  String z1 = x.at(2, 4);
  cout << "z1 = x.at(2, 4) = " << z1 << endl;
  AlwaysAssertExit(z1.length() == 3);
  AlwaysAssertExit(z1 == "llo");

  String z2 = x.at(5, 3);
  cout << "z2 = x.at(5, 3) = " << z2 << endl;
  AlwaysAssertExit(z2.length() == 0);
  AlwaysAssertExit(z2 == "");

  x.at(2, 2) = "r";
  cout << "x.at(2, 2) = r; x = " << x << endl;
  AlwaysAssertExit(x == "Hero");

  x = X;
  x.at(0, 1) = "j";
  cout << "x.at(0, 1) = j; x = " << x << endl;
  AlwaysAssertExit(x == "jello");

  x = X;
  x.at("He") = "je";
  cout << "x.at(He) = je; x = " << x << endl;
  AlwaysAssertExit(x == "jello");
  
  x = X;
  x.at("l", -1) = "i";
  cout << "x.at(l, -1) = i; x = " << x << endl;
  AlwaysAssertExit(x == "Helio");
  
  x = X;
  z = x.at(r);
  cout << "z = x.at(r) = " << z << endl;
  AlwaysAssertExit(z == "ello");
  
  z = x.before("o");
  cout << "z = x.before(o) = " << z << endl;
  AlwaysAssertExit(z == "Hell");
  x.before("ll") = "Bri";
  cout << "x.before(ll) = Bri; x = " << x << endl;
  AlwaysAssertExit(x == "Brillo");

  x = X;
  z = x.before(2);
  cout << "z = x.before(2) = " << z << endl;
  AlwaysAssertExit(z == "He");

  z = x.after("Hel");
  cout << "z = x.after(Hel) = " << z << endl;
  AlwaysAssertExit(z == "lo");
  x.after("Hel") = "p";
  cout << "x.after(Hel) = p; x = " << x << endl;
  AlwaysAssertExit(x == "Help");

  x = X;
  z = x.after(3);
  cout << "z = x.after(3) = " << z << endl;
  AlwaysAssertExit(z == "o");

  z = "  a bc";
  z  = z.after(RXwhite);
  cout << "z =   a bc; z = z.after(RXwhite); z =" << z << endl;
  AlwaysAssertExit(z == "a bc");
}

void utiltest() {
  String x = X;
  
  Int matches = x.gsub("l", "ll");
  
  cout << "x.gsub(l, ll); x = " << x << endl;
  AlwaysAssertExit(matches == 2);
  AlwaysAssertExit(x == "Hellllo");

  x = X;
  matches = x.gsub(r, "ello should have been replaced by this string");
  cout << "x.gsub(r, ...); x = " << x << endl;
  AlwaysAssertExit(matches == 1);
  AlwaysAssertExit(x == "Hello should have been replaced by this string");

  matches = x.gsub(RXwhite, "#");
  cout << "x.gsub(RXwhite, #); x = " << x << endl;
  AlwaysAssertExit(matches == 7);
  
  String z = X + Y;
  z.del("loworl");
  cout << "z = x+y; z.del(loworl); z = " << z << endl;
  AlwaysAssertExit(z == "Held");

  x = X;
  z = reverse(x);
  cout << "reverse(x) = " << z << endl;
  AlwaysAssertExit(z == "olleH");

  x.reverse();
  cout << "x.reverse() = " << x << endl;
  AlwaysAssertExit(x == z);

  x = X;
  z = upcase(x);
  cout << "upcase(x) = " << z << endl;
  AlwaysAssertExit(z == "HELLO");

  z = downcase(x);
  cout << "downcase(x) = " << z << endl;
  AlwaysAssertExit(z == "hello");

  z = capitalize(x);
  cout << "capitalize(x) = " << z << endl;
  AlwaysAssertExit(z == "Hello");

  z = replicate('*', 10);
  cout << "z = replicate(*, 10) = " << z << endl;
  AlwaysAssertExit(z == "**********");
  AlwaysAssertExit(z.length() == 10);
}

void splittest() {
  String z = "This string\thas\nfive words";
  cout << "z = " << z << endl;
  String w[10];
  Int nw = split(z, w, 10, RXwhite);
  AlwaysAssertExit(nw == 5);
  cout << "from split(z, RXwhite, w, 10), n words = " << nw << ":\n";
  for (Int i = 0; i < nw; ++i) {
    cout << w[i] << endl;
  }
  AlwaysAssertExit(w[0] == "This");
  AlwaysAssertExit(w[1] == "string");
  AlwaysAssertExit(w[2] == "has");
  AlwaysAssertExit(w[3] == "five");
  AlwaysAssertExit(w[4] == "words");
  AlwaysAssertExit(w[5] == "");

  z = join(w, nw, "/");
  cout << "z = join(w, nw, /); z =" << z << endl;
  AlwaysAssertExit(z == "This/string/has/five/words");
}

void iotest() {
  String z = "Della and the Dealer\n and a dog named Jake,\n and a cat named Kalamazoo";
  cout << "word =" << z << " ";
  cout << "length = " << z.length() << endl;
  ostringstream os;
  os << "Test stream: " << "length = " << z.length();
  String zx(os);
  cout << zx << endl;
}

void identitytest(String a, String b) {
  String x = a;
  String y = b;
  x += b;
  y.prepend(a);
  AlwaysAssertExit((a + b) == x);
  AlwaysAssertExit((a + b) == y);
  AlwaysAssertExit(x == y);
  AlwaysAssertExit(x.after(a) == b);
  AlwaysAssertExit(x.before(b, 4) == a);
  AlwaysAssertExit(x.from(a) == x);
  AlwaysAssertExit(x.through(b, x.size()) == x);
  AlwaysAssertExit(x.at(a) == a);
  AlwaysAssertExit(x.at(b) == b);

  AlwaysAssertExit(reverse(x) == reverse(b) + reverse(a));

  AlwaysAssertExit((a + b + a) == (a + (b + a)));
  
  ///  x.del(b, -1);
  x.del(b);
  AlwaysAssertExit(x == a);

  y.before(b, 2) = b;
  AlwaysAssertExit(y == (b + b));
  y.at(b) = a;
  AlwaysAssertExit(y == (a + b));
  
  x = a + reverse(a);
  for (Int i = 0; i < 7; ++i) {
    y = x;
    x += x;
    AlwaysAssertExit(x == reverse(x));
    AlwaysAssertExit(x.index(y) == 0);
  }
}


void freqtest() {
  String x = "Hello World";
  String y = x.at(0,5);
  AlwaysAssertExit(x.freq('l') == 3);	// Char
  AlwaysAssertExit(x.freq("lo") == 1);	// Char*
  AlwaysAssertExit(x.freq(x) == 1);	// String
  AlwaysAssertExit(x.freq(y) == 1);	// SubString
}

void toDouble() {
    String x = "1.5";
    Double y = String::toDouble(x);
    AlwaysAssertExit(y == 1.5);
    x = "frodo";
    AlwaysAssertExit (String::toDouble(x) == 0);
    bool ok = false;
    try {
      y = String::toDouble(x, True);
    } catch (const AipsError&) {
      ok = true;
    }
    AlwaysAssertExit(ok);
}

void toFloat() {
    String x = "1.5";
    Float y = String::toFloat(x);
    AlwaysAssertExit(y == 1.5);
    x = "1.5 aa";
    AlwaysAssertExit(String::toFloat(x) == 1.5);
    bool ok = false;
    try {
      y = String::toFloat(x, True);
    } catch (const AipsError&) {
      ok = true;
    }
    AlwaysAssertExit(ok);
}

void toInt() {
    String x = "4";
    Int y = String::toInt(x);
    AlwaysAssertExit(y == 4);
    x = "-12";
    y = String::toInt(x);
    AlwaysAssertExit(y == -12);
    x = "6.9999";
    AlwaysAssertExit (String::toInt(x) == 6);
    bool ok = false;
    try {
      y = String::toInt(x, True);
    } catch (const AipsError&) {
      ok = true;
    }
    AlwaysAssertExit(ok);
}

void trim() {
    String myString = "\t  \t  \n\r  my string \n\r \t ";
    myString.trim();
    AlwaysAssertExit(myString == "my string");
    myString = "\t  \t  \n\r  my string";
    myString.trim();
    AlwaysAssertExit(myString == "my string");
    myString = "my string \n\r \t ";
    myString.trim();
    AlwaysAssertExit(myString == "my string");
    myString = "\n \t\t\r  ";
    myString.trim();
    AlwaysAssertExit(myString.empty());
    myString = "    ";
    myString.trim();
    AlwaysAssertExit(myString.empty());
}

void startsWith() {
    String myString = "Gozer the Destroyer";
    AlwaysAssertExit(myString.startsWith("G"));
    AlwaysAssertExit(myString.startsWith("Gozer t"));
    AlwaysAssertExit(! myString.startsWith("oz"));
}

/* void hashtest()
{
  String *xp, a, x[] = {
   "Hello World",
   "Me and you and the dog named boo",
   "a",
   "b",
   "aa",
   "bb",
   "A",
   ""
  };

  //# remove cout's of hash values because they can differ between arch's
  for (xp=x; xp->length() > 0; xp++)  xp->hash();
  //  cout << "hash(" << *xp << ") = " << xp->hash() << endl;


  a = ""; a.hash();
  //  cout << "hash() = " << a.hash() << endl;

}
*/

int main() {
  decltest();
  cattest();
  comparetest();
  substrtest();
  utiltest();
  splittest();
  freqtest();
  identitytest(X, X);
  identitytest(X, Y);
  identitytest(X+Y+N+X+Y+N,
	       "A string that will be used in identitytest but is otherwise "
	       "just another useless string.");
  ///  hashtest();
  iotest();
  toDouble();
  toFloat();
  toInt();
  trim();
  startsWith();
  cout << "\nEnd of test\n";
  return(0);
}
