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
#include <images/Images/ImageFitter.h>
#include <casa/namespace.h>

Int main(Int argc, char *argv[]) {
	Input input(1);
	input.version("$ID:$");
	input.create("imagename");
	input.create("box", "");
	input.create("region", "");
	input.create("chan", "0");
	input.create("stokes", "I");
	input.create("mask","");
	input.create("includepix", "");
	input.create("excludepix", "");
	input.create("residual", "");
	input.create("model", "");
	input.create("estimates", "");
	input.create("logfile", "");
	input.create("append","false");
	input.create("newestimates","");

	input.readArguments(argc, argv);
	String imagename = input.getString("imagename");

	String box = input.getString("box");
	String region = input.getString("region");
	uInt chan = input.getInt("chan");
	String stokes = input.getString("stokes");
	String mask = input.getString("mask");
	String residual = input.getString("residual");
	String model = input.getString("model");
	String estimatesFilename = input.getString("estimates");
	String logfile = input.getString("logfile");
	Bool append = input.getBool("append");
	String newEstimatesFileName = input.getString("newestimates");

	Vector<String> includePixParts = stringToVector(input.getString("includepix"));
	Vector<String> excludePixParts = stringToVector(input.getString("excludepix"));
	Vector<Float> includePixelRange(includePixParts.nelements());
	Vector<Float> excludePixelRange(excludePixParts.nelements());
	for (uInt i = 0; i < includePixelRange.nelements(); i++) {
		includePixelRange[i] = String::toFloat(includePixParts[i]);
	}
	for (uInt i = 0; i < excludePixelRange.nelements(); i++) {
		excludePixelRange[i] = String::toFloat(excludePixParts[i]);
	}
	ImageFitter imFitter(
		imagename, region, box, chan, stokes, mask, includePixelRange,
		excludePixelRange, residual, model, estimatesFilename, logfile,
		append, newEstimatesFileName
	);
    imFitter.fit();
    return 0;
}



