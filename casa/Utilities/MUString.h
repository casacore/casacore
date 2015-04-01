//# MUString.h: Pointed String class to aid analysis of quantity strings
//# Copyright (C) 1996,1997,1999,2000,2001
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

#ifndef CASA_MUSTRING_H
#define CASA_MUSTRING_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Containers/Block.h>

//# Forward Declarations
#include <casacore/casa/iosfwd.h>
namespace casacore { //# NAMESPACE CASACORE - BEGIN

class Regex;
template <class T> class Vector; 

// <summary>
// Pointed String class to aid analysis of quantity strings
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasure" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto classString>String</linkto>
// </prerequisite>
//
// <etymology>
// From Measure Utility String
// </etymology>
//
// <synopsis>
// The MUString is a class with a String and an embedded pointer. It can be used
// to linearly analyse a string for its semantics. Imagine for instance a
// string that represents an angle. It could be formatted as 
// <src>[+-]hh:mm:ss.ttt</src>
// or as <src>[+-]hh[hH]mm[mM]</src> or as 
// <src>[+-]dd.mm.ss.ttt</src>  or with <src>.'s</src> replaced with
// <src>dms</src> or as <src>[+-]ddd.fff deg</src> etc.<br>
// The available methods aid in analysing this string (see example).<br>
// The following analysis method classes are avaible:
// <ul>
//   <li> construct   -- all constructors create a string with the pointer
//		starting at the beginning of the string
//   <li> testX(arg)  -- all test methods test if the next available
//		character(s) fulfill the specified argument test. E.g.
//		<src>Bool testSign()</src> test if current character is + or -.
//		If at end of string; False is returned, except for 
//		<src>testBlank()</src>. No pointer update. Any method with
//		<em>NC</em> at the end (for no-case) will test irrespective
//		of the case.
//   <li> skipX(arg)  -- all skip methods skip all available character(s)
//		fulfilling the specified argument. E.g.
//		<src>void skipSign()</src> will skip all + and - found at
//		the current and subsequent positions. Pointer updated.
//   <li> tSkipX(arg) -- will skip as skipX, and return Bool as in testX.
//		Pointer updated
//   <li> getX(arg)   -- will get the indicated X value from the string.
//		Pointer updated. A get will always return a valid result.
//		However, if the value did not exist (e.g. 
//		<src>Double getDouble()</src> form a string like <src>"abc"</src>
//		will return 0.0) a False status will be saved. It can be
//		interrogated by the <src>Bool status()</src> function.
//		The string part used in producing the value is also
//		saved, and can be obtained with 
//		<src>const String &lastGet()</src>.
//		No saving in case of a simple getChar() is done.
//   <li> stack -- if it is necessary to save the current position of the
//		pointer (for maybe later restoration) a <src>void push()</src>
//		and <src>void pop()</src> are available
//   <li> pointer -- the pointer can be manipulated with <src>void setPtr()</src>
//		 and <src>Int getPtr()</src>. Pointers are always protected in
//		their value.
// </ul> 
// The following types (<em>X</em> in the above list) are available
// <ul>
//   <li> Char -- a single character
//   <li> CharNC -- a single character with no case
//   <li> String -- a string
//   <li> StringNC -- a string without case
//   <li> Alpha -- a through z and A through Z and _ (underscore)
//   <li> Num -- digits 0 through 9
//   <li> AlphaNum -- a field staring with Alpha, and remainder (if any)
//		Alpha or Num
//   <li> Sign -- a plus or minus
//   <li> Blank -- a space ar a tab
//   <li> uInt -- unsigned integer
//   <li> Int -- an optionally signed integer
//   <li> Double -- a double value
// </ul>
// General string aids are available. The main one a minimax, caseless
// check of an input String against a vector:
// <src>static uInt minimaxNC(String in, Int N_name, String name[])</src>
// and its vector equivalent: 
// <src>static uInt minimaxNC(String in, Vector<String> name)</src>.
// Success is indicated by a return value less than N_name or the
// vector length.
// </synopsis>
//
// <example>
// See <linkto class=MVAngle>MVAngle</linkto> class for example background.
// The following example is the conversion of different input angle formats
// to a <linkto class=Quantum>Quantity</linkto>. A full blown example,
// but gives some idea of intricacies involved.
// <srcblock>
//  res = Quantity(0.0, "rad");	// result
//  MUString tmp(in);		// Pointed non-const String
//  tmp.skipBlank();
//  Double s = tmp.getSign();	// sign
//  tmp.push();			// Save position to rescan
//  Double r = tmp.getuInt();	// first field
//  Int tp = 0;			// distributor
//  if (tmp.tSkipChar('.')) {	// if more than one ., dms format
//    Double r1 = tmp.getuInt();
//    if (tmp.tSkipChar('.')) {
//      r += r1/60.0 + tmp.getDouble()/3600.0;
//      tp = 4;
//    } else {			// else value with units
//      tmp.pop();		// Reset position
//      r = tmp.getDouble();
//    };
//  } else if (tmp.tSkipCharNC('d')) {	// dms
//    tp = 1;
//  } else if (tmp.tSkipCharNC('h')) {	// hms
//    tp = 2;
//  } else if (tmp.tSkipChar(':')) {	// hms
//    tp = 3;
//  };
//  switch (tp) {
// case 0: {
//    UnitVal u; String us;
//    if (!MVAngle::unitString(u,us,tmp)) return False;
//    r *= s;
//    if (u == UnitVal::NODIM) {	// check correct dimension
//      res = Quantity(r,"rad");
//      return True;
//    };
//    if (u == UnitVal::ANGLE) {
//      res = Quantity(r,us);
//      return True;
//    };
//    if (u == UnitVal::TIME) {
//      res = Quantity(Quantity(r/240.,us).getBaseValue(), "deg");
//      return True;
//    };
//    return False;
//  };
//  break;
//
//  case 1:
//  case 2:
//  case 3: {			// get remainder od ms and hms formats
//    Char tc = 'm';
//    if (tp == 3) tc = ':';
//    tmp.push();
//    Double r1 = tmp.getuInt();
//    if (tmp.tSkipChar('.')) {
//      tmp.pop();
//      r += tmp.getDouble()/3600.;
//    } else if (tmp.tSkipCharNC(tc)) {
//      r += r1/60.0 + tmp.getDouble()/3600.;
//    } else {
//      r += r1/3600.0;
//    };
//    r *= s;
//  };
//  break;
//
//  default:
//    break;
//  };
//
//  switch (tp) {		// make correct units
//
//  case 1:
//  case 4:
//    res = Quantity(r,"deg");
//    break;
//
//  case 2:
//  case 3:
//    res = Quantity(Quantity(r/240.,"h").getBaseValue(), "deg");
//    break;
//
//  default:
//    break;
//
//  };
//  return True;
// </srcblock>
// </example>
//
// <motivation>
// The class was written to be able to analyse an input string for its
// <linkto class=Quantum>Quantum</linkto> representation as value with
// units, or os a date/time or as an angle.
// </motivation>
//
// <todo asof="1996/11/14">
//   <li> nothing I know of
// </todo>

class MUString
{
public:

//# Friends
  // Output String starting at pointer
  friend ostream &operator<<(ostream &os, const MUString &in);
//# Enumerations

//# Constructors
  // Default constructor creates an empty string
  MUString();
  // Create from String; setting pointer at start
  // <group>
  MUString(const String &in);
  MUString(const Char *in);
  MUString(char in);
  // </group>
  // Copy constructor; new pointer will be same as old
  MUString(const MUString &other);
  // Copy assignment; new pointer will be same as old
  MUString &operator=(const MUString &other);

  // Destructor
  ~MUString();

//# Operators
  // Obtain remaining string (same as <src>get()</src>).
  String operator()();

//# General Member Functions
  // Save current pointer on internal stack
  void push();
  // Restore pointer from stack (or set to start if stack empty)
  void pop();
  // Restore stack for one level
  void unpush();

  // Act on whitespace; adjusting pointer if skip
  // <group>
  void skipBlank();
  Bool testBlank() const;
  Bool tSkipBlank();
  // </group>

  // Act on sign; return +1 or -1 depending on signs found (-- == +)
  // <group>
  void skipSign();
  Bool testSign() const;
  Bool tSkipSign();
  Int getSign();
  // </group>

  // Act on integer field. If no integer found in 0 returned; and False
  // <group>
  void skipInt();
  Bool testInt() const;
  Bool tSkipInt();
  Int getInt();
  void skipuInt();
  Bool tSkipuInt();
  Bool testuInt() const;
  uInt getuInt();
  // </group>

  // Act on Double field. If no value 0 returned and False.
  // <group>
  void skipDouble();
  Bool testDouble() const;
  Bool tSkipDouble();
  Double getDouble();
  // </group>

  // Act on character(s)
  // <group>
  void skipChar(Int n=1);
  void skipChar(Char ch);
  Bool tSkipChar(Char nc);
  void skipCharNC(Char ch);
  Bool tSkipCharNC(Char ch);
  Bool tSkipOneChar(Char ch);
  Bool tSkipOneCharNC(Char ch);
  void skipChar(const Regex &ex);
  Bool tSkipChar(const Regex &ex);
  void skipAlpha();
  Bool tSkipAlpha();
  void skipNum();
  Bool tSkipNum();
  void skipAlphaNum();
  Bool tSkipAlphaNum();
  Bool testChar(Char ch) const;
  Bool testCharNC(Char ch) const;
  Bool testChar(const Regex &ex) const;
  Bool testAlpha() const;
  Bool testNum() const;
  Bool testAlphaNum() const;
  Char getChar();
  String getAlpha();
  String getAlphaNum();
  // </group>

  // Act on series of characters
  // <group>
  Bool testString(const Regex &ex) const;
  Bool testString(const String &ex) const;
  Bool testStringNC(const String &ex) const;
  Bool tSkipString(const Regex &ex);
  Bool tSkipString(const String &ex);
  Bool tSkipStringNC(const String &ex);
  void skipString(const Regex &ex);
  void skipString(const String &ex);
  void skipStringNC(const String &ex);
  String getString(const Regex &ex);
  String getString(const String &ex);
  String getStringNC(const String &ex);
  // </group>

  // Match a pair of opening(at pointer)/closing characters (e.g. ( and )).
  // Return False if wrong semantics. The string between the pair 
  // (excluding them)
  // will be put in Last. If false, the ptr will be as originally; if True
  // it will point beyond the matched closing character
  Bool matchPair(Char nd);

  // Get frequency of occurrence
  Int freqChar(Char ch) const;

  // Get part of string
  // <group>
  String get();
  String get(uInt st);
  String get(uInt st, uInt nd);
  // </group>

  // Get pointer
  Int getPtr() const;

  // (Re-)set pointer
  void setPtr(Int in=0);

  // test for end of string
  Bool eos() const;

  // Get status last get
  Bool status() const;

  // Get String found at last get
  const String &lastGet() const;

  // Do minimax check on list of Strings
  // <group>
  static uInt minimaxNC(const String &in, Int N_name, 
			const String tname[]);
  static uInt minimaxNC(const String &in, const Vector<String> &tname);
  // </group>

private:
 // Data
 // String value
  String str;
  // 0-based pointer into string
  uInt ptr;
  // Length of string
  uInt len;
  // Pointer stack
  Block<uInt> stack;
  // Pointer into stack
  uInt stpt;
  // Status of last get
  Bool stat;
  // String found at last get
  String lget;

  // Member functions
  // Make a new pointer between 0 and len inclusive
  void adjustPtr(Int in);

  // Initialise last settings; return pointer
  Int initLast();
  // Set last settings
  void setLast(Int st);

};

// Global functions
// <summary> Output global functions </summary>
// Output
// <group name=output>  
ostream &operator<<(ostream &os, const MUString &in);
// </group>


} //# NAMESPACE CASACORE - END

#endif
