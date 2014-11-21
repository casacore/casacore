//# tTableDesc.h: Example class for tTableDesc.cc
//# Copyright (C) 1994,1995,1996,2004
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


#ifndef TABLES_TTABLEDESC_H
#define TABLES_TTABLEDESC_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>

#include <casacore/casa/namespace.h>
// <summary>
// Example for a non-standard column in a table description
// </summary>

// <use visibility=local>

// <reviewed reviewer="GvD" date="2004/07/09" tests="">

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> TableDesc
// </prerequisite>

// <synopsis> 
// ExampleDesc is an example showing how to use a column with
// a non-standard data type in a table description.
// </synopsis> 

class ExampleDesc
{
public:
    ExampleDesc(): x_p(0), y_p(0) {}
    ExampleDesc(Int x, float y) : x_p(x), y_p(y) {}
    ExampleDesc(const ExampleDesc& that): x_p(that.x_p), y_p(that.y_p) {}
    static String dataTypeId()
	{ return "ExampleDesc"; }
    Int x() const
	{ return x_p; }
    float y() const
	{ return y_p; }
    Int& x()
	{ return x_p; }
    float& y()
	{ return y_p; }
    int operator== (const ExampleDesc& that) const
	{ return x_p==that.x_p && y_p==that.y_p; }
    int operator< (const ExampleDesc& that) const
	{ return x_p<that.x_p || (x_p==that.x_p && y_p<that.y_p); }
private:
    Int   x_p;
    float y_p;
};


#endif
