//# MUString.cc: Pointed String class to ais analysis of quantity strings
//# Copyright (C) 1996,1997,1998,1999,2001,2002,2003,2004
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

//# Includes
#include <casacore/casa/Utilities/MUString.h>
#include <casacore/casa/sstream.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Regex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Constructors
MUString::MUString() :
  str(), ptr(0), len(0), stack(0), stpt(0), stat(True), lget() {}

MUString::MUString(const String &in) :
  str(in), ptr(0), len(in.length()), stack(0), stpt(0), stat(True), lget() {
  }

MUString::MUString(const Char *in) :
  str(in), ptr(0), len(0), stack(0), stpt(0), stat(True), lget() {
    len = str.length();
  }

MUString::MUString(Char in) :
  str(in), ptr(0), len(0), stack(0), stpt(0), stat(True), lget() {
    len = str.length();
  }

MUString::MUString(const MUString &other) :
  str(other.str), ptr(other.ptr), len(other.len), 
  stack(0), stpt(0), stat(True), lget() {}

MUString &MUString::operator=(const MUString &other) {
  if (this != &other) {
    str = other.str;
    ptr = other.ptr;
    len = other.len;
    stack = Block<uInt>(0);
    stpt = 0;
    stat = True;
    lget = String();
  }
  return *this;
}

// Destructor
MUString::~MUString() {}

// Operators
String MUString::operator()() {
  return get();
}

// General member functions
void MUString::push() {
  while (stpt >= stack.nelements()) stack.resize(2*stpt + 1);
  stack[stpt++] = ptr;
}

void MUString::pop() {
  ptr = stpt > 0 ? stack[--stpt] : 0;
}

void MUString::unpush() {
  if (stpt > 0) --stpt;
}

void MUString::skipBlank() {
  while (ptr < len && testBlank())  ptr++;
}

Bool MUString::testBlank() const {
  static Regex ex("[ \t]");
  return (ptr >= len || testChar(ex));
}

Bool MUString::tSkipBlank() {
  return ((ptr < len && testBlank()) ? (skipBlank(), True) : False);
}

Bool MUString::testSign() const {
  static Regex ex("[-+]");
  return testChar(ex);
}

void MUString::skipSign() {
  while (testSign()) ptr++;
}

Bool MUString::tSkipSign() {
  return (testSign() ? (skipSign(), True) : False);
}

Int MUString::getSign() {
  Int t = 1; Int p = initLast();
  if (testSign()) {
    while (testSign()) {
      if (str[ptr++] == '-') t = -t;
    }
    setLast(p);
  }
  return t;
}

Bool MUString::testInt() const {
  static Regex ex("[-+]*[0-9]");
  return testString(ex);
}

Bool MUString::tSkipInt() {
  return (testInt() ? (skipInt(), True) : False);;
}

Bool MUString::testuInt() const {
  return testNum();
}

Bool MUString::tSkipuInt() {
  return (testuInt() ? (skipuInt(), True) : False);
}

Int MUString::getInt() {
  Int s = 0; Int p = initLast();
  if (testInt()) {
    s = getSign();
    s *= getuInt();
    setLast(p);
  }
  return s;
}

void MUString::skipInt() {
  getInt();
}

uInt MUString::getuInt() {
  Int t = 0; Int p = initLast();
  if (testuInt()) {
    while (testNum()) {
      t *= 10; 
      t += str[ptr++] - '0';
    }
    setLast(p);
  }
  return t;
}

void MUString::skipuInt() {
  getuInt();
}

Bool MUString::testDouble() const {
  static Regex ex
    ("[-+]?(([0-9]+\\.[0-9]*)|([0-9]+)|(\\.[0-9]+))([eE][+-]?[0-9]+)?");
  return testString(ex);
}

void MUString::skipDouble() {
  getDouble();
}

Bool MUString::tSkipDouble() {
  return (testDouble() ? (skipDouble(), True) : False);
}

Double MUString::getDouble() {
  static Regex ex
    ("[-+]?(([0-9]+\\.[0-9]*)|([0-9]+)|(\\.[0-9]+))([eE][+-]?[0-9]+)?");
  Double res = 0.0;
  if (ptr < len && testDouble()) {
    istringstream instr(str.at(ex, ptr));
    instr >> res;
    skipString(ex);
  }
  return res;
}

void MUString::skipChar(Int n) {
  adjustPtr(ptr + n);
}

void MUString::skipChar(Char ch) {
  while (testChar(ch))  ptr++;
}

Bool MUString::tSkipChar(Char ch) {
  return (testChar(ch) ? (skipChar(ch), True) : False);
}

void MUString::skipCharNC(Char ch) {
  while (testCharNC(ch)) ptr++;
}

Bool MUString::tSkipCharNC(Char ch) {
  return (testCharNC(ch) ? (skipCharNC(ch), True) : False);
}

Bool MUString::tSkipOneChar(Char ch) {
  if (testChar(ch)) {
    ptr++;
    return True;
  }
  return False;
}

Bool MUString::tSkipOneCharNC(Char ch) {
  if (testCharNC(ch)) {
    ptr++;
    return True;
  }
  return False;
}

void MUString::skipChar(const Regex &ex) {
  while (testChar(ex)) ptr++;
}

Bool MUString::tSkipChar(const Regex &ex) {
  return (testChar(ex) ? (skipChar(ex), True) : False);
}

void MUString::skipAlpha() {
  while (testAlpha()) ptr++;
}

Bool MUString::tSkipAlpha() {
  return (testAlpha() ? (skipAlpha(), True) : False);
}

void MUString::skipAlphaNum() {
  if (testAlpha()) {
    ptr++;
    while (testAlphaNum()) ptr++;
  }
}

Bool MUString::tSkipAlphaNum() {
  return (testAlpha() ? (skipAlphaNum(), True) : False);
}

void MUString::skipNum() {
  while (testNum()) ptr++;
}

Bool MUString::tSkipNum() {
  return (testNum() ? (skipNum(), True) : False);
}

Bool MUString::testChar(Char ch) const {
  return (ptr < len && str[ptr] == ch);
}

Bool MUString::testCharNC(Char ch) const {
  Regex ex(String("[") + downcase(String(ch)) +
	   upcase(String(ch)) + String("]"));
  return testChar(ex);
}

Bool MUString::testChar(const Regex &ex) const {
  return (ptr < len && String(str[ptr]).index(ex) == 0);
}

Bool MUString::testAlpha() const {
  static Regex ex("[a-zA-Z_]");
  return testChar(ex);
}

Bool MUString::testNum() const {
  static Regex ex("[0-9]");
  return testChar(ex);
}

Bool MUString::testAlphaNum() const {
  static Regex ex ("[a-zA-Z_0-9]");
  return testChar(ex);
}

Char MUString::getChar() {
  return (ptr < len ? str[ptr++] : ' ');
}

String MUString::getAlpha() {
  Int p = initLast();
  if (tSkipAlpha()) setLast(p);
  return lget;;
}

String MUString::getAlphaNum() {
  Int p = initLast();
  if (tSkipAlphaNum()) setLast(p);
  return lget;
}

Bool MUString::testString(const Regex &ex) const {
  return (ptr < len &&
	  String(String(str).from(Int(ptr))).index(ex) == 0);
}

Bool MUString::testString(const String &ex) const {
  if (ptr < len) {
    Int tl = (len-ptr < ex.length()) ? len-ptr : ex.length();
    String t = String(str)(ptr,tl); 
    return (t.matches(ex));
  }
  return False;
}

Bool MUString::testStringNC(const String &ex) const {
  if (ptr < len) {
    Int tl = (len-ptr < ex.length()) ? len-ptr : ex.length();
    String t = String(str)(ptr,tl); t.downcase();
    String u = ex; u.downcase();
    return (t.matches(u));
  }
  return False;
}

Bool MUString::tSkipString(const Regex &ex) {
  return (testString(ex) ? (skipString(ex), True) : False);
}

Bool MUString::tSkipString(const String &ex) {
  return (testString(ex) ? (skipString(ex), True) : False);
}

Bool MUString::tSkipStringNC(const String &ex) {
  return (testStringNC(ex) ? (skipStringNC(ex), True) : False);
}

void MUString::skipString(const Regex &ex) {
  if (testString(ex)) adjustPtr(ptr + str.at(ex, ptr).length());
}

void MUString::skipString(const String &ex) {
  if (testString(ex)) adjustPtr(ptr + ex.length());
}

void MUString::skipStringNC(const String &ex) {
  if (testStringNC(ex)) adjustPtr(ptr + ex.length());
}

String MUString::getString(const Regex &ex) {
  Int p = initLast();
  if (tSkipString(ex)) setLast(p);
  return lget;
}

String MUString::getString(const String &ex) {
  Int p = initLast();
  if (tSkipString(ex)) setLast(p);
  return lget;
}

String MUString::getStringNC(const String &ex) {
  Int p = initLast();
  if (tSkipStringNC(ex)) setLast(p);
  return lget;
}

Bool MUString::matchPair(Char nd) {
  Char st = getChar();
  Int cnt = 1;
  Int p = initLast();
  while (ptr < len) {
    if (testChar(st)) {
      cnt++;
    } else if (testChar(nd)) {
      cnt--;
      if (cnt == 0) break;
    }
    skipChar();
  }
  if (cnt == 0) {
    setLast(p);
    skipChar();
    return True;
  }
  adjustPtr(p-1);
  return False;
}

Int MUString::freqChar(Char ch) const {
  Int c = 0;
  for (uInt i = ptr; i < len; i++) {
    if (str[i] == ch) c++;
  }
  return c;
}

String MUString::get() {
  return get(ptr, len);
}

String MUString::get(uInt st) {
  return get(st, len);
}

String MUString::get(uInt st, uInt nd) {
  push(); 
  adjustPtr(st);
  Int p = initLast();
  adjustPtr(nd);
  setLast(p);
  pop();
  return lget;
}

Int MUString::getPtr() const {
  return ptr;
}

void MUString::setPtr(Int in) {
  adjustPtr(in);
}

Bool MUString::eos() const {
  return (ptr >= len);
}

Bool MUString::status() const {
  return stat;
}

const String &MUString::lastGet() const {
  return lget;
}

void MUString::adjustPtr(Int in) {
  ptr = in<0 ? 0 : (in>(Int)len ? len : in);
}
 
Int MUString::initLast() {
  static String em;
  stat = False; lget = em;
  return ptr;
}

void MUString::setLast(Int st) {
  if (st < (Int)ptr) {
    stat = True; lget = str(st, ptr-st);
  }
}

uInt MUString::minimaxNC(const String &in, Int N_name, 
			const String tname[]) {
    String a;
    String b;
    Int i;
    a = upcase(in);
// Exact fit?
    for (i=0; i<N_name; i++) {
	if (a == upcase(tname[i])) break;
    }
// Now look for partial
    if (i >= N_name) {
	Int ia, ib;
	ia = a.length();
	for (i=0; i<N_name; i++) {
	    ib = tname[i].length();
	    ib = ia < ib ? ia : ib;
	    b = upcase(tname[i]);
	    if (a.at(0,ib) == b.at(0,ib)) {
		Int j;
// Look for more partials
		for (j=i+1; j<N_name; j++) {
		    ib = tname[j].length();
		    ib = ia < ib ? ia : ib;
		    b = upcase(tname[j]);
		    if (a.at(0,ib) == b.at(0,ib)) break;
		}
// Found duplicate
		if (j<N_name) i=N_name;
		break;
	    }
	}
    }
    return i;
}

uInt MUString::minimaxNC(const String &in,
			const Vector<String> &tname) {
  Bool delIt; const String *stor = tname.getStorage(delIt);
  uInt rt = minimaxNC(in, tname.nelements(), stor);
  tname.freeStorage(stor, delIt);
  return rt;
}

ostream &operator<<(ostream &os, const MUString &in) {
  if (in.ptr < in.len) os << String(in.str).from(Int(in.ptr));
  return os;
}

} //# NAMESPACE CASACORE - END

