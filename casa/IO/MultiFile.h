//# MultiFile.h: Class to combine multiple files in a single one
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

#ifndef CASA_MULTIFILE_H
#define CASA_MULTIFILE_H

//# Includes
#include <casa/aips.h>
#include <casa/IO/LargeFiledesIO.h>
#include <casa/vector.h>


namespace casa { //# NAMESPACE CASA - BEGIN

  //# Forward declaration.
  class AipsIO;

  // <summary>
  // Helper class for MultiFile containing info per internal file
  // </summary>
  // <use visibility=local>
  struct MultiFileInfo {
    vector<Int64> blockNrs;     // physical blocknrs for this logical file
    vector<char>  buffer;
    Int64         curBlock;     // the logical block held in buffer
    String        name;         // the logical file name
    Bool          dirty;        // has data in buffer been changed?
  };
  void operator<< (AipsIO&, const MultiFileInfo&);
  void operator>> (AipsIO&, MultiFileInfo&);

  // <summary> 
  // Class for IO on a regular file.
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tByteIO" demos="">
  // </reviewed>

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
  //    RegularFileIO regio (RegularFile("file.name"), ByeIO::NewNoReplace);
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


  class MultiFile
  {
  public:
    // Open or create a MultiFile with the given name.
    // Upon creation the block size can be given. If 0, it uses the block size
    // of the file system the file is on.
    MultiFile (const String& name, ByteIO::OpenOption, Int blockSize=0);

    // The destructor flushes and closes the file.
    ~MultiFile();

    // Add a file to the MultiFile object. It returns the file id.
    // The given name must be a basename or have the same directory as the MultiFile object.
    Int add (const String& name);

    // Read a block at the given offset. It returns the actual size read.
    Int64 read (Int fileId, char* buffer, Int64 size, Int64 offset);

    // Write a block at the given offset. It returns the actual size written.
    Int64 write (Int fileId, const char* buffer, Int64 size, Int64 offset);

    // Reopen the underlying file for read/write access.
    // Nothing will be done if the stream is writable already.
    // Otherwise it will be reopened and an exception will be thrown
    // if it is not possible to reopen it for read/write access.
    void reopenRW()
      { itsIO.reopenRW(); }

    // Flush the file by writing all dirty data and all header info.
    void flush();

    // Get the file name of the file attached.
    String fileName() const
      { return itsIO.fileName(); }

    // Get the block size used.
    Int64 blockSize() const
      { return itsBlockSize; }

    // Get the nr of blocks used.
    Int64 size() const
      { return itsNrBlock; }

  private:
    void close();
    void writeHeader();
    void readHeader();

    //# Data members
    Int   itsBlockSize;  // The blocksize used
    Int64 itsNrBlock;    // The total nr of blocks actually used
    vector<MultiFileInfo> itsInfo;
    LargeFiledesIO        itsIO;
    int                   itsFD;
  };


} //# NAMESPACE CASA - END

#endif
