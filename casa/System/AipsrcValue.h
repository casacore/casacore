//# AipsrcValue.h: Class to read values from the  Aipsrc general resource files
//# Copyright (C) 1995,1996,1997,1999,2002,2003
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

#ifndef CASA_AIPSRCVALUE_H
#define CASA_AIPSRCVALUE_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/System/Aipsrc.h>

#include <mutex>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
class Unit;

// <summary> Class to read values from the Aipsrc general resource files
// </summary>

// <use visibility=export>

// <reviewed reviewer="mhaller" date="1997/10/08" tests="tAipsrcValue" demos="">
// </reviewed>

// <prerequisite>
//  <li> <linkto class=Aipsrc>Aipsrc</linkto>
// </prerequisite>
//
// <etymology>
// A class for getting values from the Aipsrc files
// </etymology>
//
// <synopsis>
// The static AipsrcValue class can get typed values from the Aipsrc
// resource files.<br>
// The basic interaction with the class is with the static keyword match
// functions:
// <srcblock>
// bool AipsrcValue<Type>::find(Type &result, const String &keyword)
// bool AipsrcValue<Type>::find(Type &result, const String &keyword,
//				const Type &deflt)
// </srcblock>
// comparable to the standard (String) <linkto class=Aipsrc>Aipsrc</linkto>
// find.<br>
// If the resource file contains a multi-valued keyword, use the
// <linkto class=AipsrcVector>AipsrcVector</linkto> class instead.
//
// The class is templated. For ease of use typedefs are provided for:
// <srcblock>
// AipsrcDouble, AipsrcInt, AipsrcBool, AipsrcString
// AipsrcVDouble, AipsrcVInt, AipsrcVBool, AipsrcVString
// </srcblock>
// In addition to the above finds, special finds:
// <srcblock>
// bool AipsrcValue<Type>::find(Type &result, const String &keyword,
//				const Unit &defun, const Unit &resun)
// bool AipsrcValue<Type>::find(Type &result, const String &keyword,
//				const Unit &defun, const Unit &resun,
//				const Type &deflt)
// </srcblock>
// are provided. These finds will read the keyword value as a Quantity.
// If no units are given, the defun are assumed. The result is converted
// to the resun, before the value is returned. E.g.
// <srcblock>
//	double x;
//	find(x, "time.offset", "h", "d");
// </srcblock>
// will return:
// <ul>
//  <li> 2.5/24 for a value specified as 2.5 in resource file
//  <li> 2.5/24 for 2:30:00
//  <li> 0.5/24 for 30min
//  <li> 0.5 for 0.5d
// </ul>
//
// The class has <src>registerRC, get, set</src> functions as described in
// <linkto class=Aipsrc>Aipsrc</linkto>. Note that registration is on a
// per Type basis, and hence registration of the same keyword in different
// types (and possible sets) act on different values, but with the same
// result if no set has been done.
//
// Specialisation exists for <src>bool</src>, where <src>true</src> is
// any value string starting with one of 'yYtT123456789', and false in
// all other cases, and no finds with Units are provided. Strings are
// supposed to be handled by standard <linkto class=Aipsrc>Aipsrc</linkto>
// class for single values, and a specialisation exists for the
// <linkto class=AipsrcVector>AipsrcVector</linkto> case.
//
// </synopsis>
//
// <example>
// <srcblock>
//  String tzoff;		// result of keyword find
//  if (!AipsrcValue<double>::find(tzoff, "time.zone.offset")) {	// look for key
//    tzoff = -5;
//  };
// </srcblock>
// A more convenient way of accomplishing the same result is:
// <srcblock>
//    AipsrcDouble::find(tzoff, "time.zone.offset", -5);
// </srcblock>
// or even:
// <srcblock>
//    AipsrcDouble::find(tzoff, "time.zone.offset",
//			 "h", "h", -5);
// </srcblock>
// Here the final argument is the default to use if the keyword is not found
// at all.
// </example>
//
// 
// <templating>
//  <li> All types with a <src>>></src> defined. 
// <note role=warning>
// Since interpretation of the keyword value string is done with the standard
// input right-shift operator, specialisations are necessary for non-standard
// cases like bool. They are provided. String is supposed to be handled by
// standard Aipsrc.
// </note>
// </templating>
//
// <motivation>
// Programs need a way to interact with the AipsrcValue files.
// </motivation>
//
// <thrown>
//    <li>AipsError if the environment variables HOME and/or AIPSPATH not set.
// </thrown>
//
// <todo asof="1997/08/07">
// </todo>

template <class T> class AipsrcValue : public Aipsrc {

public:
  //# Constructors
  // Default constructor
  // <note role=tip>
  // A constructor (and destructor) have been provided to be able to generate
  // a (routine-level) static register list. This had to be done since
  // static data members are not yet implemented in the gcc compiler for
  // templated classes. Once they are available the <tt>tlist</tt> and
  // <tt>ntlst</tt> data can become static, constructor and desctructor and
  // all references to the init() method can disappear.
  // </note>
  AipsrcValue();
  //# Destructor
  // See note with constructor
  ~AipsrcValue();

  //# Member functions
  // The <src>find()</src> functions will, given a keyword, return the value
  // of a matched keyword found in the files. If no match found the
  // function will be false, and the default returned if specified.
  // <group>
  static bool find(T &value, const String &keyword);
  static bool find(T &value, const String &keyword, const T &deflt);
  // </group>
  // These <src>find()</src> functions will, given a keyword, read the value
  // of a matched keyword as a Quantity. If no unit has been given in the
  // keyword value, the defun Unit will be assumed. The value returned
  // will be converted to the resun Unit. If no match found, the default
  // value is returned (see example above).
  // <group>
  static bool find(T &value, const String &keyword,
		   const Unit &defun, const Unit &resun);
  static bool find(T &value, const String &keyword,
		   const Unit &defun, const Unit &resun,
		   const T &deflt);
  // </group>
  // Functions to register keywords for later use in get() and set(). The
  // returned value is the index for get() and set().
  // <group>
  static uint32_t registerRC(const String &keyword,
			 const T &deflt);
  static uint32_t registerRC(const String &keyword,
			 const Unit &defun, const Unit &resun,
			 const T &deflt);
  // </group>

  // Gets are like find, but using registered integers rather than names. The
  // aipsrc file is read only once, and values can be set as well.
  // <group>
  static const T &get(uint32_t keyword);
  // </group>

  // Sets allow registered values to be set
  // <group>
  static void set(uint32_t keyword, const T &deflt);
  // </group>

  // Save registered value to <src>$HOME/.aipsrc</src>
  static void save(uint32_t keyword);

private:
  //# Data
  // The global AipsrcValue object
  static AipsrcValue myp_p;
  static std::mutex theirMutex;
  // Register list
  // <group>
  Block<T> tlst;
  Block<String> ntlst;
  // </group>

  //# Constructors
  // Copy constructor (not implemented)
  AipsrcValue<T> &operator=(const AipsrcValue<T> &other);

  //# Copy assignment (not implemented)
  AipsrcValue(const AipsrcValue<T> &other);

  //# General member functions
};

template <> 
bool AipsrcValue<String>::find(String &value,
			       const String &keyword,
			       const Unit &defun, const Unit &resun);


// <summary> Specialization of AipsrcValue for bool </summary>

// <synopsis>
// </synopsis>

template <> class AipsrcValue<bool> : public Aipsrc {
public:
  AipsrcValue();
  ~AipsrcValue();
  static bool find(bool &value, const String &keyword);
  static bool find(bool &value, const String &keyword, const bool &deflt);
  static uint32_t registerRC(const String &keyword, const bool &deflt);
  static const bool &get(uint32_t keyword);
  static void set(uint32_t keyword, const bool &deflt);
  static void save(uint32_t keyword);
private:
  static AipsrcValue myp_p;
  static std::mutex theirMutex;
  Block<bool> tlst;
  Block<String> ntlst;
  AipsrcValue<bool> &operator=(const AipsrcValue<bool> &other);
  AipsrcValue(const AipsrcValue<bool> &other);
};


//# Declare extern templates for often used types.
  extern template class AipsrcValue<bool>;
  extern template class AipsrcValue<int32_t>;
  extern template class AipsrcValue<double>;
  extern template class AipsrcValue<String>;

} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/System/AipsrcValue.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
