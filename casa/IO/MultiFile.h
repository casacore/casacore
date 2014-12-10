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
#include <casacore/casa/IO/FiledesIO.h>
#include <casacore/casa/vector.h>
#include <casacore/casa/ostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward declaration.
  class AipsIO;

  // <summary>
  // Helper class for MultiFile containing info per internal file
  // </summary>
  // <use visibility=local>
  struct MultiFileInfo {
    explicit MultiFileInfo (Int64 bufSize=0);
    vector<Int64> blockNrs;     // physical blocknrs for this logical file
    vector<char>  buffer;       // buffer holding a data block
    Int64         curBlock;     // the data block held in buffer (<0 is none)
    Int64         size;         // file size (in bytes)
    String        name;         // the virtual file name
    Bool          dirty;        // has data in buffer been changed?
  };
  void operator<< (ostream&, const MultiFileInfo&);
  void operator<< (AipsIO&, const MultiFileInfo&);
  void operator>> (AipsIO&, MultiFileInfo&);


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
  // the MultiFile, this meta info is written into a header block and,
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
    // Only the base name of the given file name is used. In this way the
    // MultiFile container file can be moved.
    Int addFile (const String& name);

    // Return the file id of a file in the MultiFile object.
    // If the name is unknown, an exception is thrown if throwExcp is set.
    // Otherwise it returns -1.
    Int fileId (const String& name, Bool throwExcp=True) const;

    // Delete a file. It adds its blocks to the free block list.
    void deleteFile (Int fileId);

    // Read a block at the given offset. It returns the actual size read.
    Int64 read (Int fileId, void* buffer, Int64 size, Int64 offset);

    // Write a block at the given offset. It returns the actual size written.
    Int64 write (Int fileId, const void* buffer, Int64 size, Int64 offset);

    // Reopen the underlying file for read/write access.
    // Nothing will be done if the file is writable already.
    // Otherwise it will be reopened and an exception will be thrown
    // if it is not possible to reopen it for read/write access.
    void reopenRW();

    // Flush the file by writing all dirty data and all header info.
    void flush();

    // Fsync the file (i.e., force the data to be physically written).
    void fsync()
      { itsIO.fsync(); }

    // Resync with another process by clearing the buffers and rereading
    // the header. The header is only read if its counter has changed.
    void resync();

    // Get the file name of the MultiFile.
    String fileName() const
      { return itsIO.fileName(); }

    // Is the file writable?
    Bool isWritable() const
      { return itsIO.isWritable(); }

    // Get the block size used.
    Int64 blockSize() const
      { return itsBlockSize; }

    // Get the nr of virtual files.
    uInt nfile() const;

    // Get the total nr of data blocks used.
    Int64 size() const
      { return itsNrBlock; }

    // Get the info object (for test purposes mainly).
    const vector<MultiFileInfo>& info() const
      { return itsInfo; }

    // Get the free blocks (for test purposes mainly).
    const vector<Int64>& freeBlocks() const
      { return itsFreeBlocks; }

  private:
    // Flush and close the file.
    void close();
    // Write the header info.
    void writeHeader();
    // Read the header info. If always==False, the info is only read if the
    // header counter has changed.
    void readHeader (Bool always=True);
    // Write a changed data block.
    void writeDirty (MultiFileInfo& info);

    //# Data members
    Int64 itsBlockSize;  // The blocksize used
    Int64 itsNrBlock;    // The total nr of blocks actually used
    Int64 itsHdrCounter; // Counter of header changes
    FiledesIO             itsIO;
    vector<MultiFileInfo> itsInfo;
    int                   itsFD;
    Bool                  itsChanged; // Has header info changed since last flush?
    vector<Int64>         itsFreeBlocks;
  };


} //# NAMESPACE CASACORE - END

#endif
