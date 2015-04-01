//# RegularFileIO.h: Class for IO on a regular file
//# Copyright (C) 1996,1997,1999,2001,2002
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

#ifndef CASA_REGULARFILEIO_H
#define CASA_REGULARFILEIO_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/FilebufIO.h>
#include <casacore/casa/OS/RegularFile.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> 
// Class for IO on a regular file.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tByteIO" demos="">
// </reviewed>

// <prerequisite> 
//    <li> <linkto class=FilebufIO>FilebufIO</linkto> class
// </prerequisite>

// <synopsis> 
// This class is a specialization of class
// <linkto class=ByteIO>ByteIO</linkto>. It uses a
// <linkto class=RegularFile>regular file</linkto> as the data store.
// <p>
// The class is derived from <linkto class=FilebufIO>FilebufIO</linkto>,
// which contains all functions to access the file. The description of
// this class explains the use of the <src>filebufSize</src> argument
// in the constructor.
// </synopsis>

// <example>
// <srcblock>
//    // Create a file (which should not exist yet).
//    RegularFileIO regio (RegularFile("file.name"), ByteIO::NewNoReplace);
//    // Use that as the sink of AipsIO.
//    AipsIO stream (&regio);
//    // Write values.
//    stream << (Int)10;
//    stream << True;
//    // Seek to beginning of file and read data in.
//    stream.setpos (0);
//    Int vali;
//    Bool valb;
//    stream >> vali >> valb;
// </srcblock>
// </example>


class RegularFileIO: public FilebufIO
{
public: 
    // Create an IO stream object for a regular file with the given name.
    // The ByteIO option determines if the file will be created
    // or opened for input and/or output.
    // <br>
    // The argument <src>filebufSize</src> defines the length of
    // the internal buffer in the underlying <linkto class=FilebufIO>
    // FilebufIO</linkto> object. A zero length uses an appropriate default.
    explicit RegularFileIO (const RegularFile& regularFile,
                            ByteIO::OpenOption = ByteIO::Old,
                            uInt filebufSize=0);

    ~RegularFileIO();
    
    // Reopen the underlying file for read/write access.
    // Nothing will be done if the stream is writable already.
    // Otherwise it will be reopened and an exception will be thrown
    // if it is not possible to reopen it for read/write access.
    virtual void reopenRW();

    // Get the file name of the file attached.
    virtual String fileName() const;

    // Convenience function to open or create a file.
    // Optionally it is checked if the file does not exist yet.
    // It returns the file descriptor.
    static int openCreate (const RegularFile& file, ByteIO::OpenOption);

private:
    OpenOption  itsOption;
    RegularFile itsRegularFile;

    // Copy constructor, should not be used.
    RegularFileIO (const RegularFileIO& that);

    // Assignment, should not be used.
    RegularFileIO& operator= (const RegularFileIO& that);
};



} //# NAMESPACE CASACORE - END

#endif
