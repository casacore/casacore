//# AipsrcVector.h: Read multiple values from the  Aipsrc resource files
//# Copyright (C) 1995,1996,1997,1999
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

#if !defined(AIPS_AIPSRCVECTOR_H)
#define AIPS_AIPSRCVECTOR_H

#include <aips/aips.h>
#include <aips/Utilities/String.h>
#include <aips/Containers/Block.h>
#include <aips/Tasking/Aipsrc.h>

//# Forward declarations
template<class T> class Vector;
class Unit;

// <summary> Read multiple values from the  Aipsrc resource files </summary>

// <use visibility=export>

// <reviewed reviewer="mhaller" date="1997/10/08" tests="tAipsrcValue" demos="">
// </reviewed>

// <prerequisite>
//  <li> <linkto class=AipsrcValue>AipsrcValue</linkto>
// </prerequisite>
//
// <etymology>
// A class for getting multiple values from the Aipsrc files
// </etymology>
//
// <synopsis>
// The available functions (and notes) are the same as in
// <linkto class=AipsrcValue>AipsrcValue</linkto>, but with a Vector result.
// </synopsis>
// 
// <templating>
//  <li> All types with a <src>>></src> defined. 
// <note role=warning>
// Since interpretation of the keyword value string is done with the standard
// input right-shift operator, specialisations are necessary for non-standard
// cases like Bool and String. They are provided.
// </note>
// </templating>
//
// <example>
// </example>
//
// <motivation>
// Programs need a way to get multi-valued keywords from the Aipsrc files.
// </motivation>
//
// <thrown>
//    <li>AipsError if the environment variables HOME and/or AIPSPATH not set.
// </thrown>
//
// <todo asof="1997/08/07">
// </todo>

template <class T> class AipsrcVector : public Aipsrc {

public:
  //# Destructor
  ~AipsrcVector();

  //# Member functions
  // The <src>find()</src> functions will, given a keyword, return the value
  // of a matched keyword found in the files. If no match found the
  // function will be False, and the default returned if specified.
  // <group>
  static Bool find(Vector<T> &value, const String &keyword);
  static Bool find(Vector<T> &value, const String &keyword, 
		   const Vector<T> &deflt);
  // </group>
  // These <src>find()</src> functions will, given a keyword, read the values
  // of a matched keyword as a Quantity. If no unit has been given in the
  // keyword value, the defun Unit will be assumed. The value returned
  // will be converted to the resun Unit. If no match found, the default
  // value is returned (see example above).
  // <group>
  static Bool find(Vector<T> &value, const String &keyword,
		   const Unit &defun, const Unit &resun);
  static Bool find(Vector<T> &value, const String &keyword,
		   const Unit &defun, const Unit &resun,
		   const Vector<T> &deflt);
  // </group>
  // Functions to register keywords for later use in get() and set(). The
  // returned value is the index for get() and set().
  // <group>
  static uInt registerRC(const String &keyword,
			 const Vector<T> &deflt);
  static uInt registerRC(const String &keyword,
			 const Unit &defun, const Unit &resun,
			 const Vector<T> &deflt);
  // </group>
  
  // Gets are like find, but using registered integers rather than names.
  // <group>
  static const Vector<T> &get(uInt keyword);
  // </group>
  
  // Sets allow registered values to be set
  // <group>
  static void set(uInt keyword, const Vector<T> &deflt);
  // </group>
  
  // Save registered value to <src>$HOME/.aipsrc</src>
  static void save(uInt keyword);

private:
  //# Data
  static AipsrcVector<T> &init();
  // register list
  // <group>
  Block<Vector<T> > tlst;
  Block<String> ntlst;
  // </group>
  
  //# Constructors
  // Default constructor
  AipsrcVector();
  // Copy constructor (not implemented)
  AipsrcVector<T> &operator=(const AipsrcVector<T> &other);
  
  //# Copy assignment (not implemented)
  AipsrcVector(const AipsrcVector<T> &other);
  
  //# General member functions
};


#endif
