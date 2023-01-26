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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/casa/BasicSL/String.h>

#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <algorithm>
#include <casacore/casa/string.h>
#include <casacore/casa/sstream.h>
#include <stdio.h>		// for vsnprintf( )
#include <cstdarg>              // for va_start/end

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Special constructors
String::String(ostringstream &os) {
  *this = os.str();
}

// Count occurrences
int32_t String::freq(char c) const {
  size_type p(0);
  int32_t found(0);
  while (p < length()) {
    if ((p = find(c, p)) == npos) break;
    found++;
    p++;
  }
  return found;
}

int32_t String::freq(const string &str) const {
  size_type p(0);
  int32_t found(0);
  while (p < length()) {
    if ((p = find(str, p)) == npos) break;
    found++;
    p++;
  }
  return found;
}

int32_t String::freq(const char *s) const {
  size_type p(0);
  int32_t found(0);
  while (p < length()) {
    if ((p = find(s, p)) == npos) break;
    found++;
    p++;
  }
  return found;
}

int32_t String::toInt (const String& s, bool chk)
  { int32_t v=0; s.fromString(v, chk); return v; }
float String::toFloat (const String& s, bool chk)
  { float v=0; s.fromString(v, chk); return v; }
double String::toDouble (const String& s, bool chk)
  { double v=0; s.fromString(v, chk); return v; }

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
    char ws[4];
    ws[0] = ' ';
    ws[1] = '\t';
    ws[2] = '\n';
    ws[3] = '\r';
    trim(ws, 4);
}

void String::trim(char c[], uint32_t n) {
    iterator iter = begin();
    while (iter != end()  &&  std::find(c, c+n, *iter) != c+n) {
        ++iter;
    }
    erase (begin(), iter);
    if (! empty()) {
        iter = end() - 1;
        while (iter != begin()  &&  std::find(c, c+n, *iter) != c+n) {
            --iter;
        }
        ++iter;
        erase (iter, end());
    }
}

void String::ltrim(char c) {
    iterator iter = begin();
    while (iter != end()  &&  *iter ==c) {
         ++iter;
    }
    erase (begin(), iter);
}

void String::rtrim(char c) {
    if (! empty()) {
      iterator iter = end() - 1;
        while (iter != begin()  &&  *iter == c) {
            --iter;
        }
        ++iter;
        erase (iter, end());
    }
}

// Obtain a (separate) 'sub'-string
SubString String::at(size_type pos, size_type len) {
  return _substr(pos, len);
}

SubString String::at(const string &str, int32_t startpos) {
  return _substr(index(str, startpos), str.length());
}

SubString String::at(const char *s, int32_t startpos) {
  return _substr(index(s, startpos), traits_type::length(s));
}

SubString String::at(char c, int32_t startpos) {
  return _substr(index(c, startpos), 1);
}

SubString String::before(size_type pos) const {
  return _substr(0, pos);
}

SubString String::before(const string &str, size_type startpos) const {
  return _substr(0, index(str, startpos));
}

SubString String::before(const char *s, size_type startpos) const {
  return _substr(0, index(s, startpos));
}

SubString String::before(char c, size_type startpos) const {
  return _substr(0, index(c, startpos));
}

SubString String::through(size_type pos) {
  return _substr(0, pos+1);
}

SubString String::through(const string &str, size_type startpos) {
  size_type last(index(str, startpos));
  if (last != npos) last += str.length();
  return _substr(0, last);
}

SubString String::through(const char *s, size_type startpos) {
  size_type last(index(s, startpos));
  if (last != npos) last +=  traits_type::length(s);
  return _substr(0, last);
}

SubString String::through(char c, size_type startpos) {
  size_type last(index(c, startpos));
  if (last != npos) last += 1;
  return _substr(0, last);
}

SubString String::from(size_type pos) {
  return _substr(pos, length()-pos);
}

SubString String::from(const string &str, size_type startpos) {
  size_type first(index(str, startpos));
  return _substr(first, length()-first);
}

SubString String::from(const char *s, size_type startpos) {
  size_type first(index(s, startpos));
  return _substr(first, length()-first);
}

SubString String::from(char c, size_type startpos) {
  size_type first(index(c, startpos));
  return _substr(first, length()-first);
}

SubString String::after(size_type pos) {
  return _substr(pos+1, length()-(pos+1));
}

SubString String::after(const string &str, size_type startpos) {
  size_type first(index(str, startpos));
  if (first != npos) first += str.length();
  return _substr(first, length()-first);
}

SubString String::after(const char *s, size_type startpos) {
  size_type first(index(s, startpos));
  if (first != npos) first += traits_type::length(s);
  return _substr(first, length()-first);
}

SubString String::after(char c, size_type startpos) {
  size_type first(index(c, startpos));
  if (first != npos) first += 1;
  return _substr(first, length()-first);
}

// Prepend string
void String::prepend(const string &str) {
  insert(size_type(0), str);
}

void String::prepend(const char *s) {
  insert(size_type(0), s);
}

void String::prepend(char c) {
  insert(size_type(0), c);
}

// Delete
void String::del(size_type pos, size_type len) {
  erase(pos, len);
}

void String::del(const string &str, size_type startpos) {
  erase(index(str, startpos), str.length());
}

void String::del(const char *s, size_type startpos) {
  erase(index(s, startpos), traits_type::length(s));
}

void String::del(char c, size_type startpos) {
  erase(index(c, startpos), 1);
}

// Global substitution
int32_t String::gsub(const string &pat, const string &repl) {
  int32_t nmatches(0);
  if (length() == 0 || pat.length() == 0 ||
      length() < pat.length()) return nmatches;
  size_type si(0);
  int32_t rl(repl.length());
  while (length()-si >= pat.length()) {
    size_type pos = find(pat, si);
    if (pos == npos) break;
    else {
      nmatches++;
      replace(pos, pat.length(), repl);
      si = pos + rl;
    }
  }
  return nmatches;
}

int32_t String::gsub(const char *pat, const string &repl) {
  return gsub(String(pat), repl);
}

int32_t String::gsub(const char *pat, const char *repl) {
  return gsub(String(pat), String(repl));
}

// Member utilities
void String::reverse() {
  std::reverse(begin(), end());
}
#if defined(AIPS_SUN_NATIVE)
int32_t ToUpper(int32_t a){return toupper(a);}
int32_t ToLower(int32_t a){return tolower(a);}
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
  for (iterator p=begin(); p < end(); p++) {
    bool at_word;
    if ((at_word = islower(*p))) *p = ToUpper(*p);
    else at_word = isupper(*p) || isdigit(*p);
    if (at_word) {
      while (++p < end()) {
        if (isupper(*p)) *p = ToLower(*p);
        else if (!islower(*p) && !isdigit(*p)) break;
      }
    }
  }
}

// Regex related functions
String::size_type String::find(const Regex &r, size_type pos) const {
  int32_t unused;
  return r.find(c_str(), length(), unused, pos);
}

bool String::matches(const string &str, int32_t pos) const {
  bool rstat(false);
  if (pos < 0) {
    if (this->index(str,pos) == 0) {
      rstat = true;
      ///    } else {
      ///      cerr << "No Match " << this->index(str, pos) << endl;
    }
  } else {
    if (length() != 0 && str.length() != 0 &&
        length() == pos+str.length() &&
        static_cast<size_type>(pos) < length() &&
        index(str, pos) == static_cast<size_type>(pos)) {
      rstat = true;
    }
  }
  return rstat;
}

bool String::contains(const Regex &r) const {
  int32_t unused;
  return (r.find(c_str(), length(), unused, 0)) != npos;
}

bool String::matches(const Regex &r, int32_t pos) const {
  String::size_type l = (pos < 0) ? -pos : length() - pos;
  if (l>length()) return false;
  if (pos<0) return r.fullMatch(c_str(), l);
  return r.fullMatch(c_str()+pos, l);
}

String::size_type String::index(const Regex &r, int32_t startpos) const {
  int32_t unused;
  return r.search(c_str(), length(), unused, startpos);
}

SubString String::at(const Regex &r, int32_t startpos) {
  int32_t mlen;
  size_type first = r.search(c_str(), length(), mlen, startpos);
  return _substr(first, mlen);
}

SubString String::before(const Regex &r, size_type startpos) const {
  int32_t mlen;
  size_type first = r.search(c_str(), length(), mlen, startpos);
  return _substr(0, first);
}

SubString String::through(const Regex &r, size_type startpos) {
  int32_t mlen;
  size_type first = r.search(c_str(), length(), mlen, startpos);
  if (first != npos) first += mlen;
  return _substr(0, first);
}

SubString String::from(const Regex &r, size_type startpos) {
  int32_t mlen;
  size_type first = r.search(c_str(), length(), mlen, startpos);
  return _substr(first, length()-first);
}

SubString String::after(const Regex &r, size_type startpos) {
  int32_t mlen;
  size_type first = r.search(c_str(), length(), mlen, startpos);
  if (first != npos) first += mlen;
  return _substr(first, length()-first);
}

void String::del(const Regex &r, size_type startpos) {
  int32_t mlen;
  size_type first = r.find(c_str(), length(), mlen, startpos);
  if (mlen > 0) {
    erase(first, mlen);
  }
}

int32_t String::gsub(const Regex &pat, const string &repl) {
  int32_t nmatches(0);
  if (length() == 0) return nmatches;
  int32_t pl;
  size_type si(0);
  int32_t rl(repl.length());
  while (length() > si) {
    size_type pos = pat.find(c_str(), length(), pl, si);
    if (pos >= npos-1 || pl <= 0) break;
    else {
      nmatches++;
      si = pos + rl;
      if (pos == 0 && si == 0) { 	// could be problem with anchor at begin
	int32_t pls;
	size_type ps = pat.find(c_str(), length(), pls, pl); // try for begin
	if (ps >= npos-1 || pls <= 0) {
	  replace(pos, pl, repl);	// finish off if no more (anchored) match
	  break;
	}
      }
      // Continue global substitution
      replace(pos, pl, repl);
    }
  }
  return nmatches;
}

// Global functions
String reverse(const string& str) {
  String s(str);
  std::reverse(s.begin(), s.end());
  return s;
}

String upcase(const string& str) {
  String s(str);
  std::transform(s.begin(), s.end(), s.begin(), ToUpper);
  return s;
}

String downcase(const string& str) {
  String s(str);
  std::transform(s.begin(), s.end(), s.begin(), ToLower);
  return s;
}

String capitalize(const string& str) {
  String s(str);
  s.capitalize();
  return s;
}

String trim(const string& str) {
  String s(str);
  s.trim();
  return s;
}

String replicate(char c, String::size_type n) {
  return String(n, c);
}

String replicate(const string &str, String::size_type n) {
  String t(str);
  t.reserve(n*str.length());
  while (--n > 0) t += str;
  return t;
}

int32_t split(const string &str, string res[], int32_t maxn,
	  const string &sep) {
  int32_t i(0);
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

int32_t split(const string &str, string res[], int32_t maxn,
	  const Regex &sep) {
  int32_t i(0);
  String::size_type pos(0);
  int32_t matchlen;
  while (i < maxn && pos < str.length()) {
    String::size_type p = sep.find(str.c_str(), str.length(), matchlen, pos);
    if (p == String::npos) p = str.length();
    res[i] = String(str, pos, p-pos);
    i++;
    pos = p + matchlen;
  }
  return i;
}

int32_t split(const string &str, string res[], int32_t maxn,
	  const char sep) {
  return split(str, res, maxn, String(sep));
}

String common_prefix(const string &x, const string &y, 
		     int32_t startpos) {
  if (static_cast<String::size_type>(startpos) == String::npos ||
      static_cast<String::size_type>(startpos) >= x.length() ||
      static_cast<String::size_type>(startpos) >= y.length()) return String();
  String::const_iterator xs(x.begin() + startpos);
  String::const_iterator ys(y.begin() + startpos);
  String::size_type l(0);
  while (xs != x.end() && ys != y.end() && *xs++ == *ys++) l++;
  return String(x, startpos, l);
}

String common_suffix(const string &x, const string &y, 
		     int32_t startpos) {
  if (startpos >= 0 ||
      startpos + int32_t(x.length()) < 0 ||
      startpos + int32_t(y.length()) < 0) return String();
  String::const_iterator xs(x.end() + startpos+1);
  String::const_iterator ys(y.end() + startpos+1);
  String::size_type l(0);
  while (xs != x.begin() && ys != y.begin() && *--xs == *--ys) l++;
  return String(x, x.length()+startpos+1-l, l);
}

String join(string src[], int32_t n, const string& sep) {
  String x;
  for (int32_t i=0; i<n; i++) {
    x += src[i];
    if (i != n-1) x += sep;
  }
  return x;
}

int32_t fcompare(const String& x, const String& y) {
  // Determine minimum size and result in case characters compare equal.
  int32_t res = 0;
  string::size_type sz = x.size();
  if (x.size() < y.size()) {
    res = -1;
  } else if (x.size() > y.size()) {
    res = 1;
    sz  = y.size();
  }
  for (string::size_type i=0; i<sz; ++i) {
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
  if (this != &str) *this = String(str);
  return *this;
}

SubString &SubString::operator=(const String &str) {
  const_cast<string &>(ref_p).replace(pos_p, len_p, str);
  return *this;
}

SubString &SubString::operator=(const char *s) {
  const_cast<string &>(ref_p).replace(pos_p, len_p, s);
  return *this;
}

SubString &SubString::operator=(const char c) {
  const_cast<string &>(ref_p).replace(pos_p, len_p, 1, c);
  return *this;
}

} //# NAMESPACE CASACORE - END


