//# MFFileIO.h: Class for IO on a virtual file in a MultiFileBase
//# Copyright (C) 2014
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
//# $Id: RegularFileIO.h 20551 2009-03-25 00:11:33Z Malte.Marquarding $

#ifndef CASA_MFFILEIO_H
#define CASA_MFFILEIO_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/ByteIO.h>
#include <casacore/casa/IO/MultiFileBase.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary> 
  // Class for IO on a virtual file in a MultiFileBase
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tByteIO" demos="">
  // </reviewed>

  // <synopsis> 
  // This class is a specialization of class
  // <linkto class=ByteIO>ByteIO</linkto>. It uses a
  // <linkto class=MultiFileBase>MultiFileBase</linkto> as the data store.
  // <p>
  // Similar to a regular file it is possible to read and write data and to
  // seek in the file. The object keeps track of the current file offset.
  // </synopsis>

  // <example>
  // <srcblock>
  //    // Create a new MultiFile using a block size of 1 MB.
  //    MultiFile mfile("file.mf', ByteIO::New, 1048576);
  //    // Create a virtual file in it.
  //    MFFileIO mf1(mfile, "mf1", ByteIO::New);
  //    // Use it (for example) as the sink of AipsIO.
  //    AipsIO stream (&mf1);
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


  class MFFileIO: public ByteIO
  {
  public:
    // Open or create a virtual file with the given name. Note that only the
    // basename of the file name is actually used.
    // It is created in the given MultiFileBase.
    MFFileIO (MultiFileBase&, const String& name,
              ByteIO::OpenOption = ByteIO::Old);

    // The destructor flushes and closes the file.
    virtual ~MFFileIO();

    // Read <src>size</src> bytes from the byte stream. Returns the number of
    // bytes actually read, or a negative number if an error occurred. Will also
    // throw an Exception (AipsError) if the requested number of bytes could
    // not be read unless throwException is set to False.
    virtual Int64 read (Int64 size, void* buf, Bool throwException=True);

    // Write a block at the given offset.
    virtual void write (Int64 size, const void* buffer);

    // Reopen the file (and possibly underlying MultiFileBase) for read/write access.
    // Nothing will be done if the stream is writable already.
    // An exception will be thrown if it is not possible to reopen it for
    // read/write access.
    virtual void reopenRW();

    // Remove the file from the MultiFileBase object.
    // It makes the object invalid by setting the fileId to -1.
    void remove();

    // Flush the file by writing all dirty data and all header info.
    virtual void flush();

    // Get the length of the file.
    virtual Int64 length();
       
    // The file is always readable.
    virtual Bool isReadable() const;

    // Is the file writable?
    virtual Bool isWritable() const;

    // The file is always seekable.
    virtual Bool isSeekable() const;

    // Get the file name of the file attached.
    virtual String fileName() const;

    // Fsync the file (i.e. force the data to be physically written).
    virtual void fsync();

    // Reset the position pointer to the given value. It returns the
    // new position.
    virtual Int64 doSeek (Int64 offset, ByteIO::SeekOption);

  private:
    //# Data members
    MultiFileBase& itsFile;
    Int64          itsPosition;
    String         itsName;
    Int            itsId;
  };


} //# NAMESPACE CASACORE - END

#endif
