//# ByteSinkSource.h: Class for read/write access to data in a given format
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

#ifndef CASA_BYTESINKSOURCE_H
#define CASA_BYTESINKSOURCE_H

#include <casacore/casa/aips.h>
#include <casacore/casa/IO/ByteSink.h>
#include <casacore/casa/IO/ByteSource.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class TypeIO;

// <summary>Class for read/write access to data in a given format.</summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tByteSinkSource" demos="">
// </reviewed>

// <prerequisite> 
//    <li> <linkto class=ByteSink>ByteSink</linkto> class
//    <li> <linkto class=ByteSource>ByteSource</linkto> class
//    <li> <linkto class=TypeIO>TypeIO</linkto> class and derived classes
// </prerequisite> 

// <etymology> 
// This class combines ByteSink and ByteSource.
// </etymology>

// <synopsis> 
// ByteSinkSource provides read/write access to a typed byte stream in the
// Casacore IO framework. It is derived from the classes <src>ByteSink</src>
// and <src>ByteSource</src>, so it combines their functionality.
// <p>
// The object is constructed using a typed byte stream. This stream
// is an instance of a class derived from class
// <linkto class=TypeIO>TypeIO</linkto>. This makes it possible to
// store the data in any format (e.g. CanonicalIO or RawIO).
// <br> In its turn TypeIO uses an instance of a class derived from class
// <linkto class=ByteIO>ByteIO</linkto>. This makes it possible to
// use any output stream (e.g. file, memory).
// </synopsis>

// <example>
// <srcblock>
// main 
// {
//     Bool valb = True;
//     RegularFileIO regularFileIO ("test.dat", ByteIO::New);
//     CanonicalIO canonicalIO(&regularFileIO);
//     ByteSinkSource  sinkSource(&canonicalIO);
//     sinkSource << valb;     // Write a boolean
//     sinkSource.seek (0);    // Reset to begin of IO stream
//     sinkSource >> valb;     // Read a boolean
//     cout << valb << endl;   // Print the boolean  
// }
// </srcblock>
// </example>

// <motivation> 
// This class makes it transparant to do IO with different devices and
// in different ways.
// </motivation>


class ByteSinkSource: public ByteSink, public ByteSource
{
public: 
    // Default constructor.
    // This creates an invalid object, but is present for convenience.
    ByteSinkSource();

    // Construct from given TypeIO object.  The constructor does not copy the
    // object, but only keeps a pointer to it. If takeOver is true the this
    // class will delete the supplied pointer. Otherwise the caller is
    // responsible for this.
    ByteSinkSource (TypeIO* typeIO, Bool takeOver=False);

    // The copy constructor uses reference semantics
    ByteSinkSource (const ByteSinkSource& sinkSource);

    // The assignment operator uses reference semantics
    ByteSinkSource& operator= (const ByteSinkSource& sinkSource);

    ~ByteSinkSource();
};



} //# NAMESPACE CASACORE - END

#endif
