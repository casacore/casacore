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

//# Includes
#include <casacore/casa/Utilities/MUString.h>
#include <casacore/casa/sstream.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Regex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Constructors
MUString::MUString() :
  str(), ptr(0), len(0), stack(0), stpt(0), stat(true), lget() {}

MUString::MUString(const String &in) :
  str(in), ptr(0), len(in.length()), stack(0), stpt(0), stat(true), lget() {
  }

MUString::MUString(const char *in) :
  str(in), ptr(0), len(0), stack(0), stpt(0), stat(true), lget() {
    len = str.length();
  }

MUString::MUString(char in) :
  str(in), ptr(0), len(0), stack(0), stpt(0), stat(true), lget() {
    len = str.length();
  }

MUString::MUString(const MUString &other) :
  str(other.str), ptr(other.ptr), len(other.len), 
  stack(0), stpt(0), stat(true), lget() {}

MUString &MUString::operator=(const MUString &other) {
  if (this != &other) {
    str = other.str;
    ptr = other.ptr;
    len = other.len;
    stack = Block<uint32_t>(0);
    stpt = 0;
    stat = true;
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

bool MUString::testBlank() const {
  return (ptr >= len || str[ptr] == ' ' || str[ptr] == '\t');
}

bool MUString::tSkipBlank() {
  return ((ptr < len && testBlank()) ? (skipBlank(), true) : false);
}

bool MUString::testSign() const {
  return (ptr < len && (str[ptr] == '-' || str[ptr] == '+'));
}

void MUString::skipSign() {
  while (testSign()) ptr++;
}

bool MUString::tSkipSign() {
  return (testSign() ? (skipSign(), true) : false);
}

int32_t MUString::getSign() {
  int32_t t = 1; int32_t p = initLast();
  if (testSign()) {
    while (testSign()) {
      if (str[ptr++] == '-') t = -t;
    }
    setLast(p);
  }
  return t;
}

bool MUString::testInt() const {
  static Regex ex("[-+]*[0-9]");
  return testString(ex);
}

bool MUString::tSkipInt() {
  return (testInt() ? (skipInt(), true) : false);;
}

bool MUString::testuInt() const {
  return testNum();
}

bool MUString::tSkipuInt() {
  return (testuInt() ? (skipuInt(), true) : false);
}

int32_t MUString::getInt() {
  int32_t s = 0; int32_t p = initLast();
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

uint32_t MUString::getuInt() {
  int32_t t = 0; int32_t p = initLast();
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

bool MUString::testDouble() const {
  static Regex ex
    ("[-+]?(([0-9]+\\.[0-9]*)|([0-9]+)|(\\.[0-9]+))([eE][+-]?[0-9]+)?");
  return testString(ex);
}

void MUString::skipDouble() {
  getDouble();
}

bool MUString::tSkipDouble() {
  return (testDouble() ? (skipDouble(), true) : false);
}

double MUString::getDouble() {
  static Regex ex
    ("[-+]?(([0-9]+\\.[0-9]*)|([0-9]+)|(\\.[0-9]+))([eE][+-]?[0-9]+)?");
  double res = 0.0;
  if (ptr < len && testDouble()) {
    istringstream instr(str.at(ex, ptr));
    instr >> res;
    skipString(ex);
  }
  return res;
}

void MUString::skipChar(int32_t n) {
  adjustPtr(ptr + n);
}

void MUString::skipChar(char ch) {
  while (testChar(ch))  ptr++;
}

bool MUString::tSkipChar(char ch) {
  return (testChar(ch) ? (skipChar(ch), true) : false);
}

void MUString::skipCharNC(char ch) {
  while (testCharNC(ch)) ptr++;
}

bool MUString::tSkipCharNC(char ch) {
  return (testCharNC(ch) ? (skipCharNC(ch), true) : false);
}

bool MUString::tSkipOneChar(char ch) {
  if (testChar(ch)) {
    ptr++;
    return true;
  }
  return false;
}

bool MUString::tSkipOneCharNC(char ch) {
  if (testCharNC(ch)) {
    ptr++;
    return true;
  }
  return false;
}

void MUString::skipChar(const Regex &ex) {
  while (testChar(ex)) ptr++;
}

bool MUString::tSkipChar(const Regex &ex) {
  return (testChar(ex) ? (skipChar(ex), true) : false);
}

void MUString::skipAlpha() {
  while (testAlpha()) ptr++;
}

bool MUString::tSkipAlpha() {
  return (testAlpha() ? (skipAlpha(), true) : false);
}

void MUString::skipAlphaNum() {
  if (testAlpha()) {
    ptr++;
    while (testAlphaNum()) ptr++;
  }
}

bool MUString::tSkipAlphaNum() {
  return (testAlpha() ? (skipAlphaNum(), true) : false);
}

void MUString::skipNum() {
  while (testNum()) ptr++;
}

bool MUString::tSkipNum() {
  return (testNum() ? (skipNum(), true) : false);
}

bool MUString::testChar(char ch) const {
  return (ptr < len && str[ptr] == ch);
}

bool MUString::testCharNC(char ch) const {
  return (ptr < len && (str[ptr] == toupper(ch)  ||  str[ptr] == tolower(ch)));
}

bool MUString::testChar(const Regex &ex) const {
  return (ptr < len && String(str[ptr]).index(ex) == 0);
}

bool MUString::testAlpha() const {
  static Regex ex("[a-zA-Z_]");
  return testChar(ex);
}

bool MUString::testNum() const {
  static Regex ex("[0-9]");
  return testChar(ex);
}

bool MUString::testAlphaNum() const {
  static Regex ex ("[a-zA-Z_0-9]");
  return testChar(ex);
}

char MUString::getChar() {
  return (ptr < len ? str[ptr++] : ' ');
}

String MUString::getAlpha() {
  int32_t p = initLast();
  if (tSkipAlpha()) setLast(p);
  return lget;;
}

String MUString::getAlphaNum() {
  int32_t p = initLast();
  if (tSkipAlphaNum()) setLast(p);
  return lget;
}

bool MUString::testString(const Regex &ex) const {
  return (ptr < len &&
	  String(String(str).from(int32_t(ptr))).index(ex) == 0);
}

bool MUString::testString(const String &ex) const {
  if (ptr < len) {
    int32_t tl = (len-ptr < ex.length()) ? len-ptr : ex.length();
    String t = String(str)(ptr,tl); 
    return (t.matches(ex));
  }
  return false;
}

bool MUString::testStringNC(const String &ex) const {
  if (ptr < len) {
    int32_t tl = (len-ptr < ex.length()) ? len-ptr : ex.length();
    String t = String(str)(ptr,tl); t.downcase();
    String u = ex; u.downcase();
    return (t.matches(u));
  }
  return false;
}

bool MUString::tSkipString(const Regex &ex) {
  return (testString(ex) ? (skipString(ex), true) : false);
}

bool MUString::tSkipString(const String &ex) {
  return (testString(ex) ? (skipString(ex), true) : false);
}

bool MUString::tSkipStringNC(const String &ex) {
  return (testStringNC(ex) ? (skipStringNC(ex), true) : false);
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
  int32_t p = initLast();
  if (tSkipString(ex)) setLast(p);
  return lget;
}

String MUString::getString(const String &ex) {
  int32_t p = initLast();
  if (tSkipString(ex)) setLast(p);
  return lget;
}

String MUString::getStringNC(const String &ex) {
  int32_t p = initLast();
  if (tSkipStringNC(ex)) setLast(p);
  return lget;
}

bool MUString::matchPair(char nd) {
  char st = getChar();
  int32_t cnt = 1;
  int32_t p = initLast();
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
    return true;
  }
  adjustPtr(p-1);
  return false;
}

int32_t MUString::freqChar(char ch) const {
  int32_t c = 0;
  for (uint32_t i = ptr; i < len; i++) {
    if (str[i] == ch) c++;
  }
  return c;
}

String MUString::get() {
  return get(ptr, len);
}

String MUString::get(uint32_t st) {
  return get(st, len);
}

String MUString::get(uint32_t st, uint32_t nd) {
  push(); 
  adjustPtr(st);
  int32_t p = initLast();
  adjustPtr(nd);
  setLast(p);
  pop();
  return lget;
}

int32_t MUString::getPtr() const {
  return ptr;
}

void MUString::setPtr(int32_t in) {
  adjustPtr(in);
}

bool MUString::eos() const {
  return (ptr >= len);
}

bool MUString::status() const {
  return stat;
}

const String &MUString::lastGet() const {
  return lget;
}

void MUString::adjustPtr(int32_t in) {
  ptr = in<0 ? 0 : (in>(int32_t)len ? len : in);
}
 
int32_t MUString::initLast() {
  static String em;
  stat = false; lget = em;
  return ptr;
}

void MUString::setLast(int32_t st) {
  if (st < (int32_t)ptr) {
    stat = true; lget = str(st, ptr-st);
  }
}

uint32_t MUString::minimaxNC(const String &in, int32_t N_name, 
			const String tname[]) {
    int32_t i;
    String a = upcase(in);
// Exact fit?
    for (i=0; i<N_name; i++) {
	if (a == upcase(tname[i])) break;
    }
// Now look for partial
    if (i >= N_name) {
	size_t ia = a.length();
	for (i=0; i<N_name; i++) {
	    String b = upcase(tname[i]);
	    size_t ib = b.length();
	    ib = ia < ib ? ia : ib;
	    if (a.at(0,ib) == b.at(0,ib)) {
		int32_t j;
// Look for more partials
		for (j=i+1; j<N_name; j++) {
		    b = upcase(tname[j]);
		    ib = b.length();
		    ib = ia < ib ? ia : ib;
		    if (a(0,ib) == b.at(0,ib)) break;
		}
// Found duplicate
		if (j<N_name) i=N_name;
		break;
	    }
	}
    }
    return i;
}

uint32_t MUString::minimaxNC(const String &in,
			const Vector<String> &tname) {
  bool delIt; const String *stor = tname.getStorage(delIt);
  uint32_t rt = minimaxNC(in, tname.nelements(), stor);
  tname.freeStorage(stor, delIt);
  return rt;
}

ostream &operator<<(ostream &os, const MUString &in) {
  if (in.ptr < in.len) os << String(in.str).from(int32_t(in.ptr));
  return os;
}

} //# NAMESPACE CASACORE - END

