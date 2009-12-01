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

#include <casa/BasicSL/String.h>
#include <casa/Utilities/Regex.h>
#include <assert.h>
// Next one for atoi and atof
#include <casa/stdlib.h>
#include <casa/iostream.h>
#include <casa/sstream.h>

#include <casa/namespace.h>
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
  assert(x == "");

  String y = "Hello";
  cout << "A string initialized to Hello:" << y << endl;
  assert(y == "Hello");

  if (y[y.length()-1] == 'o') y = y + '\n';
  assert(y == "Hello\n");
  y = "Hello";

  String a = y;
  cout << "A string initialized to previous string:" << a << endl;
  assert(a == "Hello");
  assert(a == y);

  String b (a.at(1, 2));
  cout << "A string initialized to previous string.at(1, 2):" << b << endl;
  assert(b == "el");

  Char ch = '@';
  String z(ch);
  cout << "A string initialized to @:" << z << endl;
  assert(z == "@");

  String n = "20";
  cout << "A string initialized to dec(20):" << n << endl;
  assert(n == "20");

  Int i = atoi(n.chars());
  Double f = atof(n.chars());
  cout << "n = " << n << " atoi(n) = " << i << " atof(n) = " << f << endl;
  assert(i == 20);
  assert(f == 20);

  /*
  assert(X.OK());
  assert(Y.OK());
  assert(x.OK());
  assert(y.OK());
  assert(z.OK());
  assert(n.OK());
  assert(r.OK());
  */
}

void cattest() {
  String x = X;
  String y = Y;
  String z = x + y;
  cout << "z = x + y = " << z << endl;
  assert(z == "Helloworld");

  x += y;
  cout << "x += y; x = " << x << endl;
  assert(x == "Helloworld");

  y = Y;
  x = X;
  y.prepend(x);
  cout << "y.prepend(x); y = " << y << endl;
  assert(y == "Helloworld");

  x = X;
  y = Y;
  z = x + s + ' ' + y.at("w") + y.after("w") + ".";
  cout << "z = x + s +  + y.at(w) + y.after(w) + . = " << z << endl;
  assert(z == "Hello, world.");
}

void comparetest() {
  String x = X;
  String y = Y;
  String n = N;
  String z = x + y;

  assert(x != y);
  assert(x == "Hello");
  assert(x != z.at(0, 4));
  assert (x < y);
  assert(!(x >= z.at(0, 6)));
  assert(x.contains("He"));
  assert (z.contains(x));
  assert(x.contains(r));
  
  assert(!(x.matches(r)));
  assert(x.matches(RXalpha));
  assert(!(n.matches(RXalpha)));
  assert(n.matches(RXint));
  assert(n.matches(RXdouble));
  
  assert(x.index("lo") == 3);
  assert(x.index("l", 2) == 2);
  assert(x.index("l", -1) == 3); // negative pos!
  assert(x.index(r)  == 1);
  assert(x.index(r, -2) == 1);

  assert(x.contains("el", 1));
  assert(x.contains("el"));
  
  assert(common_prefix(x, "Help") == "Hel");
  assert(common_suffix(x, "to") == "o");
  
  assert(fcompare(x, "hELlo") == 0);
  assert(fcompare(x, "hElp") < 0);
}

void substrtest() {
  String x = X;

  Char ch = x[0];
  cout << "ch = x[0] = " << ch << endl;
  assert(ch == 'H');

  String z = x.at(2, 3);
  cout << "z = x.at(2, 3) = " << z << endl;
  assert(z.length() == 3);
  assert(z == "llo");

  String z1 = x.at(2, 4);
  cout << "z1 = x.at(2, 4) = " << z1 << endl;
  assert(z1.length() == 3);
  assert(z1 == "llo");

  String z2 = x.at(5, 3);
  cout << "z2 = x.at(5, 3) = " << z2 << endl;
  assert(z2.length() == 0);
  assert(z2 == "");

  x.at(2, 2) = "r";
  cout << "x.at(2, 2) = r; x = " << x << endl;
  assert(x == "Hero");

  x = X;
  x.at(0, 1) = "j";
  cout << "x.at(0, 1) = j; x = " << x << endl;
  assert(x == "jello");

  x = X;
  x.at("He") = "je";
  cout << "x.at(He) = je; x = " << x << endl;
  assert(x == "jello");
  
  x = X;
  x.at("l", -1) = "i";
  cout << "x.at(l, -1) = i; x = " << x << endl;
  assert(x == "Helio");
  
  x = X;
  z = x.at(r);
  cout << "z = x.at(r) = " << z << endl;
  assert(z == "ello");
  
  z = x.before("o");
  cout << "z = x.before(o) = " << z << endl;
  assert(z == "Hell");
  x.before("ll") = "Bri";
  cout << "x.before(ll) = Bri; x = " << x << endl;
  assert(x == "Brillo");

  x = X;
  z = x.before(2);
  cout << "z = x.before(2) = " << z << endl;
  assert(z == "He");

  z = x.after("Hel");
  cout << "z = x.after(Hel) = " << z << endl;
  assert(z == "lo");
  x.after("Hel") = "p";
  cout << "x.after(Hel) = p; x = " << x << endl;
  assert(x == "Help");

  x = X;
  z = x.after(3);
  cout << "z = x.after(3) = " << z << endl;
  assert(z == "o");

  z = "  a bc";
  z  = z.after(RXwhite);
  cout << "z =   a bc; z = z.after(RXwhite); z =" << z << endl;
  assert(z == "a bc");
}

void utiltest() {
  String x = X;
  
  Int matches = x.gsub("l", "ll");
  
  cout << "x.gsub(l, ll); x = " << x << endl;
  assert(matches == 2);
  assert(x == "Hellllo");

  x = X;
  matches = x.gsub(r, "ello should have been replaced by this string");
  cout << "x.gsub(r, ...); x = " << x << endl;
  assert(matches == 1);
  assert(x == "Hello should have been replaced by this string");

  matches = x.gsub(RXwhite, "#");
  cout << "x.gsub(RXwhite, #); x = " << x << endl;
  assert(matches == 7);
  
  String z = X + Y;
  z.del("loworl");
  cout << "z = x+y; z.del(loworl); z = " << z << endl;
  assert(z == "Held");

  x = X;
  z = reverse(x);
  cout << "reverse(x) = " << z << endl;
  assert(z == "olleH");

  x.reverse();
  cout << "x.reverse() = " << x << endl;
  assert(x == z);

  x = X;
  z = upcase(x);
  cout << "upcase(x) = " << z << endl;
  assert(z == "HELLO");

  z = downcase(x);
  cout << "downcase(x) = " << z << endl;
  assert(z == "hello");

  z = capitalize(x);
  cout << "capitalize(x) = " << z << endl;
  assert(z == "Hello");

  z = replicate('*', 10);
  cout << "z = replicate(*, 10) = " << z << endl;
  assert(z == "**********");
  assert(z.length() == 10);
}

void splittest() {
  String z = "This string\thas\nfive words";
  cout << "z = " << z << endl;
  String w[10];
  Int nw = split(z, w, 10, RXwhite);
  assert(nw == 5);
  cout << "from split(z, RXwhite, w, 10), n words = " << nw << ":\n";
  for (Int i = 0; i < nw; ++i) {
    cout << w[i] << endl;
  }
  assert(w[0] == "This");
  assert(w[1] == "string");
  assert(w[2] == "has");
  assert(w[3] == "five");
  assert(w[4] == "words");
  assert(w[5] == "");

  z = join(w, nw, "/");
  cout << "z = join(w, nw, /); z =" << z << endl;
  assert(z == "This/string/has/five/words");
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
  assert((a + b) == x);
  assert((a + b) == y);
  assert(x == y);
  assert(x.after(a) == b);
  assert(x.before(b, -1) == a);
  assert(x.from(a) == x);
  assert(x.through(b, -1) == x);
  assert(x.at(a) == a);
  assert(x.at(b) == b);

  assert(reverse(x) == reverse(b) + reverse(a));

  assert((a + b + a) == (a + (b + a)));
  
  ///  x.del(b, -1);
  x.del(b);
  assert(x == a);

  y.before(b, -1) = b;
  assert(y == (b + b));
  y.at(b) = a;
  assert(y == (a + b));
  
  x = a + reverse(a);
  for (Int i = 0; i < 7; ++i) {
    y = x;
    x += x;
    assert(x == reverse(x));
    assert(x.index(y) == 0);
  }
}


void freqtest() {
  String x = "Hello World";
  String y = x.at(0,5);
  assert(x.freq('l') == 3);	// Char
  assert(x.freq("lo") == 1);	// Char*
  assert(x.freq(x) == 1);	// String
  assert(x.freq(y) == 1);	// SubString
}

void toDouble() {
    String x = "1.5";
    Double y = String::toDouble(x);
    assert(y == 1.5);
    x = "frodo";
    y = String::toDouble(x);
    // should be 0, but account for finite machine precision
    assert(y < 1e-316 && y > -1e-316);
}

void toFloat() {
    String x = "1.5";
    Float y = String::toFloat(x);
    assert(y == 1.5);
    x = "frodo";
    y = String::toFloat(x);
    // should be 0, but account for finite machine precision
    assert(y < 1e-316 && y > -1e-316);
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
  cout << "\nEnd of test\n";
  return(0);
}
