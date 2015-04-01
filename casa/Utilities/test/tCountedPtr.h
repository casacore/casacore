//# tCountedPtr.cc: helps building test program tCountedPtr
//# Copyright (C) 1993,1994,1995,2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

// Split off from the original tCountedPtr.cc
// in order to satisfy the automated building of the test program.

#ifndef CASA_TCOUNTEDPTR_H
#define CASA_TCOUNTEDPTR_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/BasicSL/String.h>

#include <casacore/casa/namespace.h>
// <summary>
// Example class for test of CountedPtr class
// </summary>

// <use visibility=local>

// <reviewed reviewer="Friso Olnon" date="1995/03/15" tests="tCountedPtr" demos="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> CountedPtr
// </prerequisite>

// <synopsis> 
// Make a class to test the templated class CountedPtr.
// </synopsis> 


class myobj {
protected:
    String store;
public:
    myobj(const char *str) : store(str) { 
	cout << "myobj(" << store << ") ctor" << endl;
    }
    virtual ~myobj() { cout << "myobj(" << store << ") dtor" << endl;}
    virtual String name() const { return store; }
};

class myobj1 :public myobj {
public:
    myobj1(const char *str) : myobj(str) { 
	cout << " myobj1(" << store << ") ctor" << endl;
    }
    virtual ~myobj1() { cout << " myobj1(" << store << ") dtor" << endl;}
    virtual String name() const { return "myobj1_" + store; }
};


#endif
