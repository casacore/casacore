//# j2convert.cc: This program demonstrates conversion of UVW for WSRT
//# Copyright (C) 1998,1999,2000,2001,2002,2003
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

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Inputs.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/msfits/MSFits/MSFitsOutput.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
int main (int argc, const char* argv[])
{
    try {
	// enable input in no-prompt mode
	Input inputs(1);

	// define the input structure
	inputs.version("20080124APS");
	inputs.create ("ms", "",
		       "Name of input MeasurementSet", "string");
	inputs.create ("in", "",
		       "Name of input MeasurementSet (synonym of ms)",
		       "string");
	inputs.create ("fitsfile", "",
		       "Name of output FITS file", "string");
	inputs.create ("out", "",
		       "Name of output FITS file (synonym of out)",
		       "string");
	inputs.create ("column", "DATA",
		       "Name of data column to write", "string");
	inputs.create ("writesyscal", "T",
		       "write SYSCAL info (TY and GC)", "bool");
	inputs.create ("multisource", "T",
		       "write multi-source FITS", "bool");
	inputs.create ("combinespw", "T",
		       "Combine spectral windows as one IF group", "bool");
	inputs.create ("writestation", "F",
		       "Write station names instead of antenna names", "bool");
	inputs.create ("sensitivity", "0.1",
		       "Sensitivity", "double");

	// Fill the input structure from the command line.
	inputs.readArguments (argc, argv);

	// get and check the input file specification
	String msin (inputs.getString("ms"));
	if (msin == "") {
	  msin = inputs.getString("in");
	}
	if (msin == "") {
	    throw (AipsError(" the MeasurementSet ms must be given"));
	}
        Path measurementSet (msin);
	cout << "The input MeasurementSet is: " 
	    << measurementSet.absoluteName() << endl;
	if (!measurementSet.isValid()) {
	    throw (AipsError(" The MeasurementSet path is not valid"));
	}
	if (!File(measurementSet).exists()) {
	    throw (AipsError(" The MeasurementSet file does not exist"));
	}

	// Get the fitsfile name.
	String fitsfile(inputs.getString("fitsfile"));
	if (fitsfile == "") {
	  fitsfile = inputs.getString("out");
	}
	if (fitsfile == "") {
	  fitsfile = msin;
	  fitsfile = fitsfile.before(Regex("\\.MS$")) + ".UVF";
	}

	// Get the column name.
	String column(inputs.getString("column"));

	// Get the writesyscal.
	Bool writeSyscal(inputs.getBool("writesyscal"));

	// Get the multisource.
	Bool multisource(inputs.getBool("multisource"));

	// Get the multisource.
	Bool combinespw(inputs.getBool("combinespw"));

      	// Get the writestation.
	Bool writestation(inputs.getBool("writestation"));

	// Get the sensitivity.
	Double sensitivity(inputs.getDouble("sensitivity"));

	// Now write the fits file.
	MSFitsOutput::writeFitsFile(fitsfile, MeasurementSet(msin),
				    column, -1, -1, -1,
				    writeSyscal, multisource,
				    combinespw, writestation, sensitivity);
    } catch (AipsError x) {
	cout << x.getMesg() << endl;
	return 1;
    } 

    cout << "ms2uvfits normally ended" << endl;
    return 0;
}
