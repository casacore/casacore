//# Register.h: Templated function to provide simple type identification
//# Copyright (C) 1993,1994,1995,1999,2001
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

#ifndef CASA_REGISTER_H
#define CASA_REGISTER_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/RegSequence.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> 
//    Primitive Run Time Type Information (<em>RTTI</em>)
// </summary>
//
// <use visibility=export>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos=""></reviewed>
//
// <prerequisite>
//   <li> <em>none</em>
// </prerequisite>
//
// <etymology>
//    This function is called Register because the user is <em>registering</em>
//    a type with the run-time type mechanism.
// </etymology>
//
// <synopsis> 
//    This function returns a unique unsigned integer (<b>uInt</b>) for each
//    type of pointer the user passes in as a parameter. It will always return
//    the same number for a given type during a particular execution. The
//    unsigned integer which is returned can be use to identify the particular
//    type.
//
//    <note role=warning> This function does not work correctly for
//          <em>multiple inheritance</em>, but <b>ONLY</b> for 
//          <em>single inheritance</em>. In addition, the type <em>number</em>
//          which is returned is <b>NOT</b> unique across program executions.
//    </note>
//
//    This RTTI mechanism is simple, it does not require extra functions to be
//    added to the classes which are to be <em>identified</em>, and it is 
//    similar to the RTTI mechanism which will be a part of the C++ language
//    in the future.
//
//    To be useful, however, this mechanism must be used as part of the
//    implementation of a <em>virtual</em> member function. For example:
//    <srcblock>
//    #include <casacore/casa/Utilities/Register.h>
// #include <iostream>
//    
//    class foo { public: virtual uInt type() { return Register(this);}};
//    class bar : public foo { public: uInt type() { return Register(this);}};
//    main() {
//        foo *b = new bar();
//        foo *f = new foo();
//    
//        cout << "f: type()=" << f->type() << " Register()=" << Register(f) << endl;
//        cout << "b: type()=" << b->type() << " Register()=" << Register(b) << endl;
//    }
//    </srcblock>
//    The output of this code would look something like:
//    <pre>
//        f: type()=1 Register()=1
//        b: type()=2 Register()=1
//    </pre>
//    Without the virtual function, <src>type()</src>, the output of 
//    <src>Register()</src> is deceiving and of little use.
// </synopsis> 
//
// <motivation>
//      Needed a simple type identification mechanism for the
//      <linkto class=Notice>Notice</linkto> class. This was necessary so that
//      multiple notices could be distinguished.
//      It can be replaced by the future standard RTTI.
// </motivation>
//
// <templating arg=t>
//    <li> <em>none</em>
// </templating>
//
// <group name=register>

// This is a templated function which takes a pointer to any class as a parameter.
// The parameter's type is then used to generate a unique id. The parameter is
// a pointer rather than a <em>value</em> for efficiency reasons. With a 
// <em>value</em> parameter, it would be difficult to do things like:
// <srcblock>
//    Register(static_cast<MyClass*>(0));
// </srcblock>
// to find the <src>Register</src> type id for a random class.
template<class t> uInt Register(const t *);

// </group>

// Bother!!
// template<class t> inline uInt Register(const t *) {
//   static uInt type = 0;
//   if (!type) type = RegSequence::SgetNext();
//   return type;
// }

// BOTHER!! BOTHER!! BOTHER!!
// template<class t> inline uInt Register(const t &v) {
//   return Register(&v);
// }


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Utilities/Register.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
