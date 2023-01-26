//# tDataConversion.cc: test program for data conversion classes
//# Copyright (C) 1996,1999,2001,2002
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


#include <casacore/casa/aips.h>
#include <casacore/casa/OS/DataConversion.h>
#include <casacore/casa/OS/CanonicalDataConversion.h>
#include <casacore/casa/OS/LECanonicalDataConversion.h>
#include <casacore/casa/OS/IBMDataConversion.h>
#include <casacore/casa/OS/VAXDataConversion.h>
#include <casacore/casa/OS/RawDataConversion.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
// This program tests the data conversion classes.


void showConv (const DataConversion& conv)
{
    cout << "  char externalSize = " << conv.externalSize (static_cast<char*>(0)) << endl;
    cout << " unsigned char externalSize = " << conv.externalSize (static_cast<unsigned char*>(0)) << endl;
    cout << " int16_t externalSize = " << conv.externalSize (static_cast<int16_t*>(0)) << endl;
    cout << "uShort externalSize = " << conv.externalSize (static_cast<uint16_t*>(0)) <<endl;
    cout << "   int32_t externalSize = " << conv.externalSize (static_cast<int32_t*>(0)) << endl;
    cout << "  uint32_t externalSize = " << conv.externalSize (static_cast<uint32_t*>(0)) << endl;
    cout << " int64_t externalSize = " << conv.externalSize (static_cast<int64_t*>(0)) << endl;
    cout << "uInt64 externalSize = " << conv.externalSize (static_cast<uint64_t*>(0)) << endl;
    cout << " float externalSize = " << conv.externalSize (static_cast<float*>(0)) << endl;
    cout << "Double externalSize = " << conv.externalSize (static_cast<double*>(0)) <<endl;
    cout << "  char canCopy = " << conv.canCopy (static_cast<char*>(0)) << endl;
    cout << " unsigned char canCopy = " << conv.canCopy (static_cast<unsigned char*>(0)) << endl;
    cout << " int16_t canCopy = " << conv.canCopy (static_cast<int16_t*>(0)) << endl;
    cout << "uShort canCopy = " << conv.canCopy (static_cast<uint16_t*>(0)) << endl;
    cout << "   int32_t canCopy = " << conv.canCopy (static_cast<int32_t*>(0)) << endl;
    cout << "  uint32_t canCopy = " << conv.canCopy (static_cast<uint32_t*>(0)) << endl;
    cout << " int64_t canCopy = " << conv.canCopy (static_cast<int64_t*>(0)) << endl;
    cout << "uInt64 canCopy = " << conv.canCopy (static_cast<uint64_t*>(0)) << endl;
    cout << " float canCopy = " << conv.canCopy (static_cast<float*>(0)) << endl;
    cout << "Double canCopy = " << conv.canCopy (static_cast<double*>(0)) << endl;
}

int main()
{
    cout << ">>>" << endl;
#if defined(AIPS_LITTLE_ENDIAN)
    cout << "This is a little-endian machine" << endl;
#else
    cout << "This is a big-endian machine" << endl;
#endif
    CanonicalDataConversion d1;
    cout << endl << "Canonical:" << endl;
    showConv (d1);
    LECanonicalDataConversion d1le;
    cout << endl << "LECanonical:" << endl;
    showConv (d1le);
    IBMDataConversion d2;
    cout << endl << "IBM:" << endl;
    showConv (d2);
    VAXDataConversion d3;
    cout << endl << "VAX:" << endl;
    showConv (d3);
    RawDataConversion d4;
    cout << endl << "Raw:" << endl;
    showConv (d4);
    cout << "<<<" << endl;
    cout << "OK" << endl;
    return 0;
}
