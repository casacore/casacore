//# tSubImage.cc: Test program for class SubImage
//# Copyright (C) 1998,1999,2000,2001,2003
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

#include <casa/Inputs/Input.h>
#include <images/Images/ImageReorderer.h>
#include <casa/namespace.h>

Int main(Int argc, char *argv[]) {
	Input input(1);
	input.version("$ID:$");
	input.create("imagename");
	input.create("order", "");
	input.create("output", "");


	input.readArguments(argc, argv);
	String imagename = input.getString("imagename");
	String order = input.getString("order");
	String output = input.getString("output");
	Regex intRegex("^[0-9]+$");
	Vector<String> orderVec;
	if (! order.matches(intRegex)) {
		orderVec = stringToVector(order);
	}
	ImageReorderer reorderer = orderVec.size() == 0
			? ImageReorderer(imagename, order, output)
			: ImageReorderer(imagename, orderVec, output);
	reorderer.reorder();
    return 0;
}



