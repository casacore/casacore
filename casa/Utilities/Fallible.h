//# Fallible.h: Identifies a value as valid or invalid
//# Copyright (C) 1994,1995,1999
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

#ifndef CASA_FALLIBLE_H
#define CASA_FALLIBLE_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# The following function is to be found in Fallible2.cc not Fallible.cc
//# because it's a non-templated function and template instantiators normally
//# do not like them in the same .cc file with templated functions.
//
// <summary> throw exception on access of an invalid object </summary>
//
// This function gets called when an invalid object is accessed. It
// just throws an exception. Since we have inline functions, let's keep
// the throw out of them to keep them from moving out of line.
// <thrown>
//  <li> AipsError
// </thrown>
//
// <group name=invalid_access>
void AccessInvalidFallibleObject();
// </group>

// <summary> Mark a value as valid or invalid. </summary>
// <use visibility=export>
// <reviewed reviewer="Gareth Hunt" date="1994/09/14" tests="tFallible,TestCenter" demos="">
// </reviewed>

// <etymology>
// This is to be used for values which might be fallible, i.e. might not
// be valid.
// </etymology>

// <synopsis> 
// This class resembles the one in <em>Scientific and Engineering C++</em>
// by Barton and Nackman. While it was written with that book closed, the
// class is simple enough that resemblances likely remain.
//
// This class essentially just holds a value (with automatic conversion)
// and allows inquiry as to whether the value is valid. If the value is
// used and is indeed invalid an exception will be thrown.
//
// A copy of the value is stored in the <src>Fallible<T></src> object, so
// making copies shouldn't be too expensive. It is anticipated that this
// class will most often be used with built in, or other small, types.
// </synopsis> 

// <example>
// Suppose we write some code that turns a day/month/year into a day
// of the week:
// <srcblock>
//    enum DayName {Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, 
//                  Saturday};
//    Fallible<DayName> dayFromDate(uInt day, uInt month, uInt year); // a func.
// </srcblock>
// And we also have some other function that needs a day name, for example
// just prints it:
// <srcblock>
//    ostream &operator<<(ostream &os, DayName day); // Print name as string
// </srcblock>
//
// Since the automatic conversions are defined, if we are certain that the
// dates are valid, we can just go ahead and use the value:
// <srcblock>
//    cout << dayFromData(2, 1, 1962) << endl; // A valid date
// </srcblock>
// If, by some chance, you are wrong and a date fails, then an exception will
// be thrown and a run-time error will occur.
//
// If, as is more likely the case, you don't know a priori whether a test will
// succeed, you can check it:
// <srcblock>
//    Fallible<DayName> result = dayFromDate(d,m,y); // who knows if valid?
//    if (result.isValid()) {
//       cout << result << endl;
//    } else {
//      // some corrective action
//    }
// </srcblock>
// </example>

// <motivation>
// The alternatives are to have "special values" (e.g. have an "undefined
// day" in the enumeration) or return a Boolean, or change a Boolean. While
// those solutions are often adequate, <src>Fallible<T></src> can often be
// more natural.
// </motivation>

// <templating arg=T>
//   <LI>  default constructor
//   <LI>  copy constructor
// </templating>

template<class T> class Fallible
{
public: 
    // The default constructor creates an invalid object.
    Fallible() : value_p(T()), isValid_p(False) {}

    // Create a valid object
    Fallible(const T &value) : value_p(value), isValid_p(True) {}

    //# Actually, the default copy ctor and assignment operator would work
    Fallible(const Fallible<T> &other) : value_p(other.value_p),
                                         isValid_p(other.isValid_p) {}
    
    Fallible<T> &operator=(const Fallible<T> &other) 
              {value_p = other.value_p; isValid_p = other.isValid_p; 
	       return *this;}

    ~Fallible() {}

    // Automatically convert a <src>Fallible<T></src> to a <src>T</src>.
    operator T() const  { if (! isValid_p) AccessInvalidFallibleObject();
			  return value_p; }

    // Sometimes it's more convenient to not rely on a compiler supplied
    // conversion, especially when the compiler is confused.
    T value() const { if (! isValid_p) AccessInvalidFallibleObject();
		      return value_p; }

    Bool isValid() const {return isValid_p;}
private:
    T value_p;
    Bool isValid_p;
};


} //# NAMESPACE CASACORE - END

#endif

