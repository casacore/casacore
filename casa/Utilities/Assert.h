//# Assert.h: Throw exceptions when Assertions fail.
//# Copyright (C) 1993,1994,1995,1999,2000,2002
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

#ifndef CASA_ASSERT_H
#define CASA_ASSERT_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>Utility function for Assert macros.</summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/13" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> module <linkto module=Exceptions>Exceptions</linkto>
// </prerequisite>

// <etymology>
// Templated function <src>assert_</src> is the basis for the macros
// <src>DebugAssertExit</src>, <src>DebugAssert</src>, 
// <src>AlwaysAssertExit</src>, and <src>AlwaysAssert</src> which 
// form the "public interface" to the Assertion mechanism.
// </etymology>

// <synopsis>
// The present Assertion mechanism uses the exception 
// handling mechanism to throw the errors when an Assertion 
// fails. It can be used in two ways:
// <dl>
//  <dt> <src>DebugAssertExit(expr)</src>
//  <dt> <src>AlwaysAssertExit(expr)</src>
//  <dd> cause the program to abort if <src>expr</src> evaluates to false.
//       This form is intended for the <em>end users</em>
//       because presumabily at their level there is no way to recover
//       from errors.
//  <dt> <src>DebugAssert(expr, exception)</src>
//  <dt> <src>AlwaysAssert(expr, exception)</src>
//  <dd> throw the specified exception if the <src>expr</src> is false.
//       This form is designed to be used by <em>library 
//       elements</em> because it actually raises an exception which
//       can be later caught in the regular way.
// </dl>
//
// <note role=tip> <src>DebugAssertExit</src> and 
// <src>DebugAssert</src> are only invoked in
// debug mode (i.e. when <src>AIPS_DEBUG</src> is defined); otherwise
//  they preprocess to null statements. <src>AlwaysAssertExit</src> 
// and <src>AlwaysAssert</src> are always invoked.
// </note>
//
// </synopsis>

// <example>
// The implementation of the <linkto module=Arrays>Array classes</linkto>
// contains many examples of the Assertion mechanism. The following
// application of the Assertion mechanism is taken from the archive of
// the aips2-workers@nrao.edu mail group (Brian Glendenning, 1994/03/23):
//
// I thought I'd readvertise a technique I use that helps me find
// problems in the classes I write. I have found this to be an
// EXTREMELY useful way of discovering bugs automatically (so the users
// of your class don't have to manually).
//
// In your class, write an <src>ok()</src> member function that
// returns a <src>Bool</src>.  Allow for inheritance and make it a
// virtual function (in fact, the derived class's <src>ok()</src> would
// probably call the <src>ok()</src> from its parent, as well as doing
// specific stuff for the derived class).
//
// Then in every member function, place a call to <src>ok()</src> in
// an Assertion.  Like this:
// <srcblock>
// DebugAssert(ok(), AipsError);  // include aips/Assert.h in your .cc file
// </srcblock>
//
// The second argument is the exception you want to throw. 
// <src>AipsError</src> will always do, although you can throw a
// more particular one if you want to. This Assertion will not be in
// production code -- i.e. if <src>AIPS_DEBUG</src> is not defined, the
// above line will be a null statement. I place these lines at the entry
// to all member functions (except I place them at the <em>end</em> of a
// constructor!). (I normally don't put an Assertion in an inline
// function).
//
// In the <src>ok()</src> function you should Assert a class's
// invariants. This is more or less the same as Asserting that an
// object's private and protected data are <em>consistent</em>. For
// example, one of the simple tests I do in the array classes is Assert
// that the number of elements (which I cache) is indeed equal to the
// product of its shape (I do ~15 tests in the <src>ok()</src> for the
// new <src>Array<T></src> class).
// </example>

// this templated function is called from Assert macros
template<typename t>
void assert_(bool expr, const char *msg, const char* file, int line);

//  These marcos are provided for use instead of simply using the
//  <src>assert_</src> function directly.
//
// <src>DebugAssert</src> and <src>AlwaysAssert</src> are designed to
// be used by library elements because they actually raise an exception
// which can later be later caught.
// <src>DebugAssertExit</src> and <src>AlwaysAssertExit</src> are
// intended to be used by the applications writer, because they cause an
// <src>exit(0)</src>.

#define AlwaysAssert(expr, exception) \
    {casacore::assert_<exception >(static_cast<bool>(expr), "Failed AlwaysAssert " #expr,__FILE__,static_cast<int>(__LINE__)); }
#define AlwaysAssertExit(expr) \
    {casacore::assert_<casacore::AbortError>(static_cast<bool>(expr), "Unrecoverable AlwaysAssertExit: " #expr,__FILE__,static_cast<int>(__LINE__));}

#if defined(AIPS_DEBUG)

#define DebugAssert(expr, exception) \
    {casacore::assert_<exception >(static_cast<bool>(expr), "Failed Assertion: " #expr,__FILE__,static_cast<int>(__LINE__));}
#define DebugAssertExit(expr) \
    {casacore::assert_<casacore::AbortError>(static_cast<bool>(expr), "Unrecoverable Assertion: " #expr,__FILE__,static_cast<int>(__LINE__));}

#else

#define DebugAssert(expr, exception)
#define DebugAssertExit(expr)

#endif


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Utilities/Assert.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
