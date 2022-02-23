//# tMultiton.cc: This program tests the Multiton class
//# Copyright (C) 2022
//# NRF South African Radio Astronomy Observatory and
//# Simon Perkins
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

#include <casacore/casa/Containers/Multiton.h>
#include <iostream>
#include <string>
class MyClass
{
public:
	MyClass(int a, bool b, std::string&& c) : 
		a(a),b(b),c(std::move(c)) {}
	int a;
	bool b;
	std::string c;
};

int main()
{
    using namespace casacore;
 	auto p1 = multiton_get<double>(5.0);
	auto p2 = multiton_get<double>(5.0);

	assert(p1 == p2);

	auto p3 = multiton_get<MyClass>(5,true,std::string("bla"));
	auto p4 = multiton_get<MyClass>(5,true,std::string("bla"));

	assert(p3 == p4);

    std::cout << "OK\n";
    return 0;
}