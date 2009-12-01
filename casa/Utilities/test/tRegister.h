//# <ClassFileName.h>: this defines <ClassName>, which ...
//# Copyright (C) 1996,1999
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

#ifndef CASA_TREGISTER_H
#define CASA_TREGISTER_H

// <summary>
// Example classes for test of Register class
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> Register
// </prerequisite>

// <synopsis> 
// Make some classes to test the templated class Register.
// </synopsis> 


class foo {};

class bar {};

class foobar : public foo, public bar {};

class barfoo : public bar, public foo {};

class foo2 : public foo {};

class bar2 : public bar {};

class foo2bar : public foo2, public bar {};

class bar2foo : public bar2, public foo {};

class foobar2 : public foo, public bar2 {};

class barfoo2 : public bar, public foo2 {};

class foo2bar2 : public foo2, public bar2 {};

class bar2foo2 : public bar2, public foo2 {};

template<class t> class mytmp : public foo2bar2 {};

template<class t> class mytmp2 : public bar2foo2 {};

#endif
