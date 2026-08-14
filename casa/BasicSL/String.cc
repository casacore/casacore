//# String.cc: String class
//# Copyright (C) 2001,2002,2003
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/casa/BasicSL/String.h>

#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/Exceptions/Error.h>

#include <algorithm>
#include <cstdarg>              // for va_start/end
#include <cstdio>		// for vsnprintf( )
#include <iostream>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

void TrimInPlace(std::string& str, std::string_view characters) {
  std::string::iterator iter = str.begin();
  while (iter != str.end()  &&  std::find(characters.begin(), characters.end(), *iter) != characters.end()) {
    ++iter;
  }
  str.erase (str.begin(), iter);
  if (! str.empty()) {
    iter = str.end() - 1;
    while (iter != str.begin()  &&  std::find(characters.begin(), characters.end(), *iter) != characters.end()) {
        --iter;
    }
    ++iter;
    str.erase (iter, str.end());
  }
}

std::string FormatString (const char* picture, ...)
{
  constexpr int BufferSize = 16384;
  char buffer [BufferSize];
  va_list vaList;
  va_start (vaList, picture);
  const int nUsed = vsnprintf (buffer, BufferSize, picture, vaList);
  va_end (vaList);
  std::string result(buffer);
  if (nUsed >= BufferSize){
      result += "*TRUNCATED*";
  }
  return result;
}

// Special constructors
String::String(std::ostringstream &os) {
  *this = os.str();
}

// Count occurrences
int String::freq(Char c) const {
  size_type p(0);
  int found(0);
  while (p < length()) {
    if ((p = find(c, p)) == npos) break;
    found++;
    p++;
  }
  return found;
}

int String::freq(const std::string &str) const {
  size_type p(0);
  int found(0);
  while (p < length()) {
    if ((p = find(str, p)) == npos) break;
    found++;
    p++;
  }
  return found;
}

int String::freq(const Char *s) const {
  size_type p(0);
  int found(0);
  while (p < length()) {
    if ((p = find(s, p)) == npos) break;
    found++;
    p++;
  }
  return found;
}

void String::throwFromStringError() const
{
  throw AipsError ("fromString failure for string '" + *this + "'");
}

String String::format (const char* picture, ...)
{
    const int BufferSize = 16384;
    char buffer [BufferSize];
    va_list vaList;
    va_start (vaList, picture);
    int nUsed = vsnprintf (buffer, BufferSize, picture, vaList);
    va_end (vaList);
    String result = buffer;
    if (nUsed >= BufferSize){
        result += "*TRUNCATED*";
    }
    return result;
}

void String::trim()
{
  TrimInPlace(*this);
}

void String::trim(char c[], unsigned n) {
  TrimInPlace(*this, std::string_view(c, n));
}

void String::ltrim(char c) {
  LTrimInPlace(*this, c);
}

void String::rtrim(char c) {
  RTrimInPlace(*this, c);
}

// Obtain a (separate) 'sub'-string
SubString String::at(size_type pos, size_type len) {
  return _substr(pos, len);
}

SubString String::at(const std::string &str, int startpos) {
  return _substr(IndexString(*this, str, startpos), str.length());
}

SubString String::at(const Char *s, int startpos) {
  return _substr(IndexString(*this, s, startpos), traits_type::length(s));
}

SubString String::at(Char c, int startpos) {
  return _substr(IndexString(*this, c, startpos), 1);
}

SubString String::before(size_type pos) {
  return _substr(0, pos);
}

SubString String::before(const std::string &str, size_type startpos) {
  return _substr(0, IndexString(*this, str, startpos));
}

SubString String::before(const Char *s, size_type startpos) {
  return _substr(0, IndexString(*this, s, startpos));
}

SubString String::before(Char c, size_type startpos) {
  return _substr(0, find(c, startpos));
}

SubString String::through(size_type pos) {
  return _substr(0, pos+1);
}

SubString String::through(const std::string &str, size_type startpos) {
  size_type last(IndexString(*this, str, startpos));
  if (last != npos) last += str.length();
  return _substr(0, last);
}

SubString String::through(const Char *s, size_type startpos) {
  size_type last(IndexString(*this, s, startpos));
  if (last != npos) last +=  traits_type::length(s);
  return _substr(0, last);
}

SubString String::through(Char c, size_type startpos) {
  size_type last(find(c, startpos));
  if (last != npos) last += 1;
  return _substr(0, last);
}

SubString String::from(size_type pos) {
  return _substr(pos, length()-pos);
}

SubString String::from(const std::string &str, size_type startpos) {
  size_type first(IndexString(*this, str, startpos));
  return _substr(first, length()-first);
}

SubString String::from(const Char *s, size_type startpos) {
  size_type first(IndexString(*this, s, startpos));
  return _substr(first, length()-first);
}

SubString String::from(Char c, size_type startpos) {
  size_type first(find(c, startpos));
  return _substr(first, length()-first);
}

SubString String::after(size_type pos) {
  return _substr(pos+1, length()-(pos+1));
}

SubString String::after(const std::string &str, size_type startpos) {
  size_type first(IndexString(*this, str, startpos));
  if (first != npos) first += str.length();
  return _substr(first, length()-first);
}

SubString String::after(const Char *s, size_type startpos) {
  size_type first(IndexString(*this, s, startpos));
  if (first != npos) first += traits_type::length(s);
  return _substr(first, length()-first);
}

SubString String::after(Char c, size_type startpos) {
  size_type first(find(c, startpos));
  if (first != npos) first += 1;
  return _substr(first, length()-first);
}

// Prepend string
void String::prepend(const std::string &str) {
  insert(size_type(0), str);
}

void String::prepend(const Char *s) {
  insert(size_type(0), s);
}

void String::prepend(Char c) {
  insert(size_type(0), 1, c);
}

// Delete
void String::del(size_type pos, size_type len) {
  erase(pos, len);
}

void String::del(const std::string &str, size_type startpos) {
  erase(IndexString(*this, str, startpos), str.length());
}

void String::del(const Char *s, size_type startpos) {
  erase(IndexString(*this, s, startpos), traits_type::length(s));
}

void String::del(Char c, size_type startpos) {
  erase(find(c, startpos), 1);
}

// Global substitution
int String::gsub(const std::string &pat, const std::string &repl) {
  return ReplaceAllInPlace(*this, pat, repl);
}

int String::gsub(const Char *pat, const std::string &repl) {
  return ReplaceAllInPlace(*this, String(pat), repl);
}

int String::gsub(const Char *pat, const Char *repl) {
  return ReplaceAllInPlace(*this, String(pat), String(repl));
}

// Member utilities
void String::reverse() {
  std::reverse(begin(), end());
}
#if defined(AIPS_SUN_NATIVE)
int ToUpper(int a){return toupper(a);}
int ToLower(int a){return tolower(a);}
#else
#define ToUpper toupper
#define ToLower tolower
#endif

void String::upcase() {
  std::transform(begin(), end(), begin(), ToUpper);
}

void String::downcase() {
  std::transform(begin(), end(), begin(), ToLower);
}

void String::capitalize() {
  CapitalizeStringInPlace(*this);
}

// Regex related functions
String::size_type String::find(const Regex &r, size_type pos) const {
  int unused;
  return r.find(c_str(), length(), unused, pos);
}

bool String::matches(const std::string &str, int pos) const {
  bool rstat(false);
  if (pos < 0) {
    if (IndexString(*this, str,pos) == 0) {
      rstat = true;
    }
  } else {
    if (length() != 0 && str.length() != 0 &&
        length() == pos+str.length() &&
        static_cast<size_type>(pos) < length() &&
        IndexString(*this, str, pos) == static_cast<size_type>(pos)) {
      rstat = true;
    }
  }
  return rstat;
}

bool String::contains(const Regex &r) const {
  int unused;
  return (r.find(c_str(), length(), unused, 0)) != npos;
}

bool RegexMatches(const std::string& str, const Regex &r, int pos) {
  const size_t l = (pos < 0) ? -pos : str.length() - pos;
  if (l>str.length()) return false;
  if (pos<0) return r.fullMatch(str.c_str(), l);
  return r.fullMatch(str.c_str()+pos, l);
}

size_t RegexIndex(const std::string& str, const Regex& r, size_t startpos) {
  int unused;
  return r.search(str.c_str(), str.length(), unused, startpos);
}

String::size_type String::index(const Regex &r, int startpos) const {
  int unused;
  return r.search(c_str(), length(), unused, startpos);
}

std::string RegexSubStr(const std::string& str, const Regex& r, size_t startpos) {
  int mlen;
  size_t first = r.search(str.c_str(), str.length(), mlen, startpos);
  return str.substr(first, mlen);
}

SubString String::at(const Regex &r, int startpos) {
  int mlen;
  size_type first = r.search(c_str(), length(), mlen, startpos);
  return _substr(first, mlen);
}

SubString String::before(const Regex &r, size_type startpos) {
  int mlen;
  size_type first = r.search(c_str(), length(), mlen, startpos);
  return _substr(0, first);
}

SubString String::through(const Regex &r, size_type startpos) {
  int mlen;
  size_type first = r.search(c_str(), length(), mlen, startpos);
  if (first != npos) first += mlen;
  return _substr(0, first);
}

SubString String::from(const Regex &r, size_type startpos) {
  int mlen;
  size_type first = r.search(c_str(), length(), mlen, startpos);
  return _substr(first, length()-first);
}

SubString String::after(const Regex &r, size_type startpos) {
  int mlen;
  size_type first = r.search(c_str(), length(), mlen, startpos);
  if (first != npos) first += mlen;
  return _substr(first, length()-first);
}

void String::del(const Regex &r, size_type startpos) {
  int mlen;
  size_type first = r.find(c_str(), length(), mlen, startpos);
  if (mlen > 0) {
    erase(first, mlen);
  }
}

int RegexReplaceAll(std::string& str, const Regex &pat, const std::string &repl) {
  int nmatches(0);
  if (str.length() == 0) return nmatches;
  int pl;
  size_t si(0);
  int rl(repl.length());
  while (str.length() > si) {
    size_t pos = pat.find(str.c_str(), str.length(), pl, si);
    if (pos >= std::string::npos-1 || pl <= 0) break;
    else {
      nmatches++;
      si = pos + rl;
      if (pos == 0 && si == 0) { 	// could be problem with anchor at begin
	int pls;
	size_t ps = pat.find(str.c_str(), str.length(), pls, pl); // try for begin
	if (ps >= std::string::npos-1 || pls <= 0) {
	  str.replace(pos, pl, repl);	// finish off if no more (anchored) match
	  break;
	}
      }
      // Continue global substitution
      str.replace(pos, pl, repl);
    }
  }
  return nmatches;
}

// Global functions
String reverse(const std::string& str) {
  String s(str);
  std::reverse(s.begin(), s.end());
  return s;
}

String upcase(const std::string& str) {
  String s(str);
  std::transform(s.begin(), s.end(), s.begin(), ToUpper);
  return s;
}

String downcase(const std::string& str) {
  String s(str);
  std::transform(s.begin(), s.end(), s.begin(), ToLower);
  return s;
}

String capitalize(const std::string& str) {
  String s(str);
  CapitalizeStringInPlace(s);
  return s;
}

String trim(const std::string& str) {
  String s(str);
  TrimInPlace(s);
  return s;
}

String replicate(Char c, String::size_type n) {
  return String(n, c);
}

String replicate(const std::string &str, String::size_type n) {
  String t(str);
  t.reserve(n*str.length());
  while (--n > 0) t += str;
  return t;
}

int split(const std::string &str, std::string res[], int maxn,
	  const std::string &sep) {
  int i(0);
  String::size_type pos(0);
  while (i < maxn && pos < str.length()) {
    String::size_type p = str.find(sep, pos);
    if (p == String::npos) p = str.length();
    res[i] = String(str, pos, p-pos);
    i++;
    pos = p + sep.length();
  }
  return i;
}

int split(const std::string &str, std::string res[], int maxn,
	  const Regex &sep) {
  int i(0);
  String::size_type pos(0);
  int matchlen;
  while (i < maxn && pos < str.length()) {
    String::size_type p = sep.find(str.c_str(), str.length(), matchlen, pos);
    if (p == String::npos) p = str.length();
    res[i] = String(str, pos, p-pos);
    i++;
    pos = p + matchlen;
  }
  return i;
}

int split(const std::string &str, std::string res[], int maxn,
	  const Char sep) {
  return split(str, res, maxn, String(sep));
}

String common_prefix(const std::string &x, const std::string &y,
		     int startpos) {
  if (static_cast<String::size_type>(startpos) == String::npos ||
      static_cast<String::size_type>(startpos) >= x.length() ||
      static_cast<String::size_type>(startpos) >= y.length()) return String();
  String::const_iterator xs(x.begin() + startpos);
  String::const_iterator ys(y.begin() + startpos);
  String::size_type l(0);
  while (xs != x.end() && ys != y.end() && *xs++ == *ys++) l++;
  return String(x, startpos, l);
}

String common_suffix(const std::string &x, const std::string &y,
		     int startpos) {
  if (startpos >= 0 ||
      startpos + Int(x.length()) < 0 ||
      startpos + Int(y.length()) < 0) return String();
  String::const_iterator xs(x.end() + startpos+1);
  String::const_iterator ys(y.end() + startpos+1);
  String::size_type l(0);
  while (xs != x.begin() && ys != y.begin() && *--xs == *--ys) l++;
  return String(x, x.length()+startpos+1-l, l);
}

String join(std::string src[], int n, const std::string& sep) {
  String x;
  for (int i=0; i<n; i++) {
    x += src[i];
    if (i != n-1) x += sep;
  }
  return x;
}

int fcompare(const String& x, const String& y) {
  // Determine minimum size and result in case characters compare equal.
  int res = 0;
  std::string::size_type sz = x.size();
  if (x.size() < y.size()) {
    res = -1;
  } else if (x.size() > y.size()) {
    res = 1;
    sz  = y.size();
  }
  for (std::string::size_type i=0; i<sz; ++i) {
    // Maybe it makes no sense to first test x[i] != y[i].
    char xc = tolower(x[i]);
    char yc = tolower(y[i]);
    if (xc < yc) {
      return -1;
    } else if (xc > yc) {
      return 1;
    }
  }
  return res;
  ///  x.downcase();
  ///  y.downcase();
  ///  return x.compare(y);
}

// SubString
SubString &SubString::operator=(const SubString &str) {
  const_cast<std::string &>(ref_p).replace(pos_p, len_p, String(str));
  return *this;
}

SubString &SubString::operator=(const String &str) {
  const_cast<std::string &>(ref_p).replace(pos_p, len_p, str);
  return *this;
}

SubString &SubString::operator=(const Char *s) {
  const_cast<std::string &>(ref_p).replace(pos_p, len_p, s);
  return *this;
}

SubString &SubString::operator=(const Char c) {
  const_cast<std::string &>(ref_p).replace(pos_p, len_p, 1, c);
  return *this;
}

} //# NAMESPACE CASACORE - END


