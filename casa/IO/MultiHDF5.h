//# MultiHDF5.h: Class to combine multiple files in a single HDF5 file
//# Copyright (C) 2015
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

#ifndef CASA_MULTIHDF5_H
#define CASA_MULTIHDF5_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/MultiFile.h>
#include <casacore/casa/HDF5/HDF5File.h>
#include <casacore/casa/HDF5/HDF5Group.h>
#include <memory>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary> 
  // Class to combine multiple files in a single HDF5 file.
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="" date="" tests="tMultiHDF5" demos="">
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

  class MultiHDF5 : public MultiFileBase
  {
  public:
    // Open or create a MultiHDF5 with the given name.
    // Upon creation the block size can be given. If 0, it uses the block size
    // of the file system the file is on.
    MultiHDF5 (const String& name, ByteIO::OpenOption, Int blockSize=0);


    // Open or create a MultiHDF5 which is nested in the given parent.
    // The data are read/written in a group with the given name in the parent.
    // Upon creation the block size can be given. If 0, it uses the block size
    // of the parent.
    MultiHDF5 (const String& name,
               const std::shared_ptr<MultiFileBase>& parent,
               ByteIO::OpenOption, Int blockSize=0);

    // The destructor flushes and closes the file.
    virtual ~MultiHDF5();

    // Make the correct MultiFileBase object for a nested file.
    // It creates a new group under which the virtual files are created.
    virtual std::shared_ptr<MultiFileBase> makeNested
    (const std::shared_ptr<MultiFileBase>& parent, const String& name,
     ByteIO::OpenOption = ByteIO::Old, Int blockSize=0) const;
                                                  
    // Open the given logical file and return its file id.
    // If the name is unknown, an exception is thrown.
    // It creates the HDF5 group and dataset opbject.
    // Reopen the underlying file for read/write access.
    // Nothing will be done if the file is writable already.
    // Otherwise it will be reopened and an exception will be thrown
    // if it is not possible to reopen it for read/write access.
    virtual void reopenRW();

    // Fsync the file (i.e., force the data to be physically written).
    virtual void fsync();

    // Get the HDF5File object.
    const std::shared_ptr<HDF5File>& getHDF5File() const
      { return itsFile; }
    const HDF5Object& getHDF5Object() const
      { return *itsHDF5; }

  private:
    // Copy constructor and assignment not possible.
    MultiHDF5 (const MultiHDF5&);
    MultiHDF5& operator= (const MultiHDF5&);

    // Initialize the MultiFile object.
    void init (ByteIO::OpenOption option);
    // Do the class-specific actions on opening a file.
    virtual void doOpenFile (MultiFileInfo&);
    // Do the class-specific actions on closing a file.
    virtual void doCloseFile (MultiFileInfo&);
    // Do the class-specific actions on adding a file.
    virtual void doAddFile (MultiFileInfo&);
    // Do the class-specific actions on deleting a file.
    virtual void doDeleteFile (MultiFileInfo&);
    // Truncate the file to <src>nrblk</src> blocks (does nothing).
    virtual void doTruncateFile (MultiFileInfo& info, uInt64 nrblk);
    // Flush the file itself.
    virtual void doFlushFile();
    // Flush and close the file.
    virtual void close();
    // Write the header info.
    virtual Bool writeHeader();
    // Read the header info. If always==False, the info is only read if the
    // header counter has changed.
    virtual void readHeader (Bool always=True);
    // Extend the virtual file to fit lastblk.
    virtual void extend (MultiFileInfo& info, Int64 lastblk);
    // Read a data block.
    virtual void readBlock (MultiFileInfo& info, Int64 blknr,
                            void* buffer);
    // Write a data block.
    virtual void writeBlock (MultiFileInfo& info, Int64 blknr,
                             const void* buffer);

    //# Data members
    std::shared_ptr<HDF5File>  itsFile;
    std::shared_ptr<HDF5Group> itsGroup;
    HDF5Object*                itsHDF5;
  };


} //# NAMESPACE CASACORE - END

#endif
