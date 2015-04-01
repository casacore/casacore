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
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/MultiFileBase.h>
#include <casacore/casa/IO/FiledesIO.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary> 
  // Class to combine multiple files in a single one.
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="" date="" tests="tMultiFile" demos="">
  // </reviewed>

  // <synopsis> 
  // This class is a container file holding multiple virtual files. It is
  // primarily meant as a container file for the storage manager files of a
  // table to reduce the number of files used (especially for Lustre) and to
  // reduce the number of open files (especially when concatenating tables).
  // <br>A secondary goal is offering the ability to use an IO buffer size
  // that matches the file system well (large buffer size for e.g. ZFS).
  //
  // The SetupNewTable constructor has a StorageOption argument to define
  // if a MultiFile has to be used and if so, the buffer size to use.
  // It is also possible to specify that through aipsrc variables.
  //

  // A virtual file is spread over multiple (fixed size) data blocks in the
  // MultiFile. A data block is never shared by multiple files.
  // For each virtual file MultiFile keeps a MultiFileInfo object telling
  // the file size and the blocks numbers used for the file. When flushing
  // the MultiFile, this meta info is written into the header block. If it
  // does not fit in the header block, the rest is written in a separate "-ext"
  // file.
  // if needed, continuation blocks. On open and resync, it is read back.
  // <br>
  //
  // A virtual file is represented by an MFFileIO object, which is derived
  // from ByteIO and as such part of the casacore IO framework. It makes it
  // possible for applications to access a virtual file in the same way as
  // a regular file.
  //
  // It is possible to delete a virtual file. Its blocks will be added to
  // the free block list (which is also stored in the meta info).
  // </synopsis>

  // <example>
  // In principle it is possible to use the MultiFile functions directly.
  // However, in general it is much easier to use an MFFileIO object
  // per virtual file as shown below.
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

  // <todo>
  //  <li> write headers at alternating file positions (for robustness)
  //  <li> possibly write headers entirely at the end if larger than blocksize
  // </todo>


  class MultiFile: public MultiFileBase
  {
  public:
    // Open or create a MultiFile with the given name.
    // Upon creation the block size can be given. If 0, it uses the block size
    // of the file system the file is on.
    MultiFile (const String& name, ByteIO::OpenOption, Int blockSize=0);

    // The destructor flushes and closes the file.
    virtual ~MultiFile();

    // Reopen the underlying file for read/write access.
    // Nothing will be done if the file is writable already.
    // Otherwise it will be reopened and an exception will be thrown
    // if it is not possible to reopen it for read/write access.
    virtual void reopenRW();

    // Fsync the file (i.e., force the data to be physically written).
    virtual void fsync();

  private:
    // Do the class-specific actions on adding a file.
    virtual void doAddFile (MultiFileInfo&);
    // Do the class-specific actions on deleting a file.
    virtual void doDeleteFile (MultiFileInfo&);
    // Flush the file itself.
    virtual void flushFile();
    // Flush and close the file.
    virtual void close();
    // Write the header info.
    virtual void writeHeader();
    // Read the header info. If always==False, the info is only read if the
    // header counter has changed.
    virtual void readHeader (Bool always=True);
    // Extend the virtual file to fit lastblk.
    virtual void extend (MultiFileInfo& info, Int64 lastblk);
    // Write a data block.
    virtual void writeBlock (MultiFileInfo& info, Int64 blknr,
                             const void* buffer);
    // Read a data block.
    virtual void readBlock (MultiFileInfo& info, Int64 blknr,
                            void* buffer);

  private:
    //# Data members
    FiledesIO itsIO;
    int       itsFD;
  };


} //# NAMESPACE CASACORE - END

#endif
