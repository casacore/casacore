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
//#
//# $Id$

#include <casa/BasicSL/String.h>

#include <casa/BasicSL/RegexBase.h>
#include <algorithm>
#include <casa/string.h>
#include <casa/sstream.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// Special constructors
String::String(ostringstream &os) {
  *this = os.str();
}

// Count occurrences
Int String::freq(Char c) const {
  size_type p(0);
  Int found(0);
  while (p < length()) {
    if ((p = find(c, p)) == npos) break;
    found++;
    p++;
  }
  return found;
}

Int String::freq(const string &str) const {
  size_type p(0);
  Int found(0);
  while (p < length()) {
    if ((p = find(str, p)) == npos) break;
    found++;
    p++;
  }
  return found;
}

Int String::freq(const Char *s) const {
  size_type p(0);
  Int found(0);
  while (p < length()) {
    if ((p = find(s, p)) == npos) break;
    found++;
    p++;
  }
  return found;
}

Double String::toDouble(const String& string) {
    istringstream instr(string);
    Double var;
    instr >> var;
    if (instr.fail()) {
      var = 0.0;
    }
    return var;
}

Float String::toFloat(const String& string) {
    istringstream instr(string);
    Float var;
    // Initialize in case the string is empty or non-numeric.
    instr >> var;
    if (instr.fail()) {
      var = 0.0;
    }
    return var;
}

Int String::toInt(const String& string) {
    istringstream instr(string);
    Int var;
    // Initialize in case the string is empty or non-numeric.
    instr >> var;
    if (instr.fail()) {
      var = 0.0;
    }
    return var;
}

void String::trim() {
    iterator iter = begin();
    while (iter != end()  &&
           (*iter == ' '  ||  *iter == '\t'  ||
            *iter == '\n' ||  *iter == '\r')) {
        ++iter;
    }
    erase (begin(), iter);
    if (! empty()) {
        iter = end() - 1;
        while (iter != begin()  &&
               (*iter == ' '  ||  *iter == '\t'  ||
                *iter == '\n' ||  *iter == '\r')) {
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

SubString String::at(const string &str, Int startpos) {
  return _substr(index(str, startpos), str.length());
}

SubString String::at(const Char *s, Int startpos) {
  return _substr(index(s, startpos), traits_type::length(s));
}

SubString String::at(Char c, Int startpos) {
  return _substr(index(c, startpos), 1);
}

SubString String::before(size_type pos) {
  return _substr(0, pos);
}

SubString String::before(const string &str, Int startpos) {
  return _substr(0, index(str, startpos));
}

SubString String::before(const Char *s, Int startpos) {
  return _substr(0, index(s, startpos));
}

SubString String::before(Char c, Int startpos) {
  return _substr(0, index(c, startpos));
}

SubString String::through(size_type pos) {
  return _substr(0, pos+1);
}

SubString String::through(const string &str, Int startpos) {
  size_type last(index(str, startpos));
  if (last != npos) last += str.length();
  return _substr(0, last);
}

SubString String::through(const Char *s, Int startpos) {
  size_type last(index(s, startpos));
  if (last != npos) last +=  traits_type::length(s);
  return _substr(0, last);
}

SubString String::through(Char c, Int startpos) {
  size_type last(index(c, startpos));
  if (last != npos) last += 1;
  return _substr(0, last);
}

SubString String::from(size_type pos) {
  return _substr(pos, length()-pos);
}

SubString String::from(const string &str, Int startpos) {
  size_type first(index(str, startpos));
  return _substr(first, length()-first);
}

SubString String::from(const Char *s, Int startpos) {
  size_type first(index(s, startpos));
  return _substr(first, length()-first);
}

SubString String::from(Char c, Int startpos) {
  size_type first(index(c, startpos));
  return _substr(first, length()-first);
}

SubString String::after(size_type pos) {
  return _substr(pos+1, length()-(pos+1));
}

SubString String::after(const string &str, Int startpos) {
  size_type first(index(str, startpos));
  if (first != npos) first += str.length();
  return _substr(first, length()-first);
}

SubString String::after(const Char *s, Int startpos) {
  size_type first(index(s, startpos));
  if (first != npos) first += traits_type::length(s);
  return _substr(first, length()-first);
}

SubString String::after(Char c, Int startpos) {
  size_type first(index(c, startpos));
  if (first != npos) first += 1;
  return _substr(first, length()-first);
}

// Prepend string
void String::prepend(const string &str) {
  insert(size_type(0), str);
}

void String::prepend(const Char *s) {
  insert(size_type(0), s);
}

void String::prepend(Char c) {
  insert(size_type(0), c);
}

// Delete
void String::del(size_type pos, size_type len) {
  erase(pos, len);
}

void String::del(const string &str, size_type startpos) {
  erase(index(str, startpos), str.length());
}

void String::del(const Char *s, size_type startpos) {
  erase(index(s, startpos), traits_type::length(s));
}

void String::del(Char c, size_type startpos) {
  erase(index(c, startpos), 1);
}

// Global substitution
Int String::gsub(const string &pat, const string &repl) {
  Int nmatches(0);
  if (length() == 0 || pat.length() == 0 ||
      length() < pat.length()) return nmatches;
  size_type si(0);
  Int rl(repl.length());
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

Int String::gsub(const Char *pat, const string &repl) {
  return gsub(String(pat), repl);
}

Int String::gsub(const Char *pat, const Char *repl) {
  return gsub(String(pat), String(repl));
}

// Member utilities
void String::reverse() {
  std::reverse(begin(), end());
}
#if defined(AIPS_SUN_NATIVE)
Int ToUpper(Int a){return toupper(a);}
Int ToLower(Int a){return tolower(a);}
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
    Bool at_word;
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

// RegexBase related functions
String::size_type String::find(const RegexBase &r, size_type pos) const {
  Int unused;
  return r.find(c_str(), length(), unused, pos);
}

String::size_type String::rfind(const RegexBase &r, size_type pos) const {
  Int unused;
  return r.rfind(c_str(), length(), unused, pos-length());
}

Bool String::matches(const string &str, Int pos) const {
  Bool rstat(False);
  if (pos < 0) {
    if (this->index(str,pos) == 0) {
      rstat = True;
      ///    } else {
      ///      cerr << "No Match " << this->index(str, pos) << endl;
    }
  } else {
    if (length() != 0 && str.length() != 0 &&
        length() == pos+str.length() &&
        static_cast<size_type>(pos) < length() &&
        index(str, pos) == static_cast<size_type>(pos)) {
      rstat = True;
    }
  }
  return rstat;
}

Bool String::contains(const RegexBase &r) const {
  Int unused;
  return (r.find(c_str(), length(), unused, 0)) != npos;
}

Bool String::matches(const RegexBase &r, Int pos) const {
  String::size_type l = (pos < 0) ? -pos : length() - pos;
  if (l>length()) return False;
  if (pos<0) return r.match(c_str(), l, 0) == l;
  return r.match(c_str(), length(), pos) == l;
}

String::size_type String::index(const RegexBase &r, Int startpos) const {
  Int unused;
  return r.search(c_str(), length(), unused, startpos);
}

SubString String::at(const RegexBase &r, Int startpos) {
  Int mlen;
  size_type first = r.search(c_str(), length(), mlen, startpos);
  return _substr(first, mlen);
}

SubString String::before(const RegexBase &r, Int startpos) {
  Int mlen;
  size_type first = r.search(c_str(), length(), mlen, startpos);
  return _substr(0, first);
}

SubString String::through(const RegexBase &r, Int startpos) {
  Int mlen;
  size_type first = r.search(c_str(), length(), mlen, startpos);
  if (first != npos) first += mlen;
  return _substr(0, first);
}

SubString String::from(const RegexBase &r, Int startpos) {
  Int mlen;
  size_type first = r.search(c_str(), length(), mlen, startpos);
  return _substr(first, length()-first);
}

SubString String::after(const RegexBase &r, Int startpos) {
  Int mlen;
  size_type first = r.search(c_str(), length(), mlen, startpos);
  if (first != npos) first += mlen;
  return _substr(first, length()-first);
}

void String::del(const RegexBase &r, size_type startpos) {
  Int mlen;
  size_type first = r.find(c_str(), length(), mlen, startpos);
  erase(first, mlen);
}

Int String::gsub(const RegexBase &pat, const string &repl) {
  Int nmatches(0);
  if (length() == 0) return nmatches;
  Int pl;
  size_type si(0);
  Int rl(repl.length());
  while (length() > si) {
    size_type pos = pat.find(c_str(), length(), pl, si);
    if (pos >= npos-1 || pl <= 0) break;
    else {
      nmatches++;
      si = pos + rl;
      if (pos == 0 && si == 0) { 	// could be problem with anchor at begin
	Int pls;
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
String reverse(string str) {
  std::reverse(str.begin(), str.end());
  return str;
}

String upcase(string str) {
  std::transform(str.begin(), str.end(), str.begin(), ToUpper);
  return str;
}

String downcase(string str) {
  std::transform(str.begin(), str.end(), str.begin(), ToLower);
  return str;
}

String capitalize(string str) {
  static_cast<String>(str).capitalize();
  return str;
}

String replicate(Char c, String::size_type n) {
  return String(n, c);
}

String replicate(const string &str, String::size_type n) {
  String t(str);
  t.reserve(n*str.length());
  while (--n > 0) t += str;
  return t;
}

Int split(const string &str, string res[], Int maxn,
	  const string &sep) {
  Int i(0);
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

Int split(const string &str, string res[], Int maxn,
	  const RegexBase &sep) {
  Int i(0);
  String::size_type pos(0);
  Int matchlen;
  while (i < maxn && pos < str.length()) {
    String::size_type p = sep.find(str.c_str(), str.length(), matchlen, pos);
    if (p == String::npos) p = str.length();
    res[i] = String(str, pos, p-pos);
    i++;
    pos = p + matchlen;
  }
  return i;
}

Int split(const string &str, string res[], Int maxn,
	  const Char sep) {
  return split(str, res, maxn, String(sep));
}

String common_prefix(const string &x, const string &y, 
		     Int startpos) {
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
		     Int startpos) {
  if (startpos >= 0 ||
      startpos + Int(x.length()) < 0 ||
      startpos + Int(y.length()) < 0) return String();
  String::const_iterator xs(x.end() + startpos+1);
  String::const_iterator ys(y.end() + startpos+1);
  String::size_type l(0);
  while (xs != x.begin() && ys != y.begin() && *--xs == *--ys) l++;
  return String(x, x.length()+startpos+1-l, l);
}

String join(string src[], Int n, const string& sep) {
  String x;
  for (Int i=0; i<n; i++) {
    x += src[i];
    if (i != n-1) x += sep;
  }
  return x;
}

Int fcompare(String x, String y) {
  x.downcase();
  y.downcase();
  return x.compare(y);
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

SubString &SubString::operator=(const Char *s) {
  const_cast<string &>(ref_p).replace(pos_p, len_p, s);
  return *this;
}

SubString &SubString::operator=(const Char c) {
  const_cast<string &>(ref_p).replace(pos_p, len_p, 1, c);
  return *this;
}

} //# NAMESPACE CASA - END


// Create some needed templates
#if !defined(AIPS_AUTO_STL)

#if defined(__GNUG__)
 template void std::__reverse<char*>(char *, char *, std::random_access_iterator_tag);
#endif

#if defined(AIPS_SUN_NATIVE)
 template void std::__reverse(char *, char *, std::random_access_iterator_tag);
 template std::istream& __rwstd::rw_extract_string ( std::istream&, std::string&, std::char_traits<char>);
#endif


template char *std::transform<char *>(char *, char *, char *, int (*)(int));
#if defined(AIPS_GCC3)
template void std::__reverse<string::iterator>(string::iterator, string::iterator, std::random_access_iterator_tag);
template string::iterator std::transform<string::iterator>(string::iterator, string::iterator, string::iterator, int (*)(int));
#endif

#endif


