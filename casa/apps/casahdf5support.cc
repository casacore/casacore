//# casahdf5support.cc: test if casacore is build with HDF5 support
//# Copyright (C) 2009
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

#include <casacore/casa/HDF5/HDF5Object.h>

using namespace casacore;

// Test if casacore is built with support for HDF5.
// It reurns 0 if so, otherwise 1.
// A message is printed unless -s is given as the first argument.

int main(int argc, char*[])
{
  if (HDF5Object::hasHDF5Support()) {
    if (argc < 2) {
      cout << "casacore built with HDF5 support" << endl;
    }
    return 0;
  }
  if (argc < 2) {
    cout << "casacore built without HDF5 support" << endl;
  }
  return 1;
}
