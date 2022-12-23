//# MultiFileBase.h: Abstract base class to combine multiple files in a single one
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

#ifndef CASA_MULTIFILEBASE_H
#define CASA_MULTIFILEBASE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/ByteIO.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/ostream.h>
#include <vector>
#include <memory>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward declaration.
  class AipsIO;
  class HDF5Group;
  class HDF5DataSet;


  // <summary>
  // Helper class for MultiFileInfo holding a data buffer
  // </summary>
  // <synopsis>
  // The buffer can be allocated with posix_memalign (for O_DIRECT support).
  // Hence the memory must be freed using free, which makes it impossible
  // to use a shared_ptr to that memory. Hence it is encapsulated in this class.
  // </synopsis>
  class MultiFileBuffer {
  public:
    MultiFileBuffer (size_t bufSize, Bool useODirect);
    ~MultiFileBuffer()
      { if (itsData) free(itsData); }
    // Forbid copy constructor.
    MultiFileBuffer (const MultiFileBuffer&) = delete;
    // Forbid assignment.
    MultiFileBuffer& operator= (const MultiFileBuffer&) = delete;
    char* data()
      { return itsData; }
  private:
    // Data members
    char* itsData;
  };

  // <summary>
  // Helper class for MultiFileBase containing info per logical file.
  // </summary>
  // <synopsis>
  // This struct defines the basic fields describing a logical file in a
  // class derived from MultiFileBase (such as MultiFile or MultiHDF5).
  // </synopsis>
  // <use visibility=local>
  struct MultiFileInfo {
    // Initialize the object. The buffer is created when the file is opened.
    explicit MultiFileInfo();
    // Allocate the buffer.
    void allocBuffer (size_t bufSize, Bool useODirect)
      { buffer = std::make_shared<MultiFileBuffer>(bufSize, useODirect); }
    //# Data members.
    std::vector<Int64> blockNrs;     // physical blocknrs for this logical file
    Int64         curBlock;     // the data block held in buffer (<0 is none)
    Int64         fsize;        // file size (in bytes)
    String        name;         // the virtual file name
    Bool          nested;       // is the file a nested MultiFile?
    Bool          dirty;        // has data in buffer been changed?
    std::shared_ptr<MultiFileBuffer> buffer; // buffer holding a data block
    std::shared_ptr<HDF5Group> group;
    std::shared_ptr<HDF5DataSet> dataSet;
  };
  ostream& operator<< (ostream&, const MultiFileInfo&);
  AipsIO& operator<< (AipsIO&, const MultiFileInfo&);
  AipsIO& operator>> (AipsIO&, MultiFileInfo&);
  void getInfoVersion1 (AipsIO&, std::vector<MultiFileInfo>&);


  // <summary> 
  // Abstract base class to combine multiple logical files in a single one.
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="" date="" tests="tMultiFile" demos="">
  // </reviewed>

  // <synopsis> 
  // This class is the abstract base class for classes defining a container
  // file holding multiple logical files. These classes are meant as a
  // container files for the storage manager files of a table to reduce the
  // number of files used (especially for Lustre) and to reduce the number
  // of open files (especially when concatenating tables).
  // The derived classes MultiFile and MultiHDF5 implement such container
  // files using a regular file and HDF5, resp.
  //
  // MultiFileBase implements several functions with common functionality
  // for the derived classes.
  //
  // A logical file is represented by an MFFileIO object, which is derived
  // from ByteIO and as such part of the casacore IO framework. It makes it
  // possible for applications to access a logical file in the same way as
  // a regular file.
  // </synopsis>

  class MultiFileBase
  {
  public:
    // Create a MultiFileBase object with the given name.
    // <br>Upon creation of the container file the block size can be given.
    // If <=0, it uses the block size of the file system the file is on,
    // but it will not be less than the absolute value of the given block size.
    // <br>If useODIrect=True, it means that O_DIRECT is used. If the OS does not
    // support it (as determined at configure time), the flag will always be
    // set to False. If True, the data buffers will have a proper alignment
    // and size (as needed by O_DIRECT).
    MultiFileBase (const String& name, Int blockSize, Bool useODirect);

    // The destructor flushes dirty blocks and closes the container file.
    virtual ~MultiFileBase();

    // Forbid copy constructor.
    MultiFileBase (const MultiFileBase&) = delete;

    // Forbid assignment.
    MultiFileBase& operator= (const MultiFileBase&) = delete;

    // Open the correct MultiFileBase (as plain or HDF5).
    static std::shared_ptr<MultiFileBase> openMF (const String& fileName);

    // Make the correct MultiFileBase object for a nested file.
    virtual std::shared_ptr<MultiFileBase> makeNested
    (const std::shared_ptr<MultiFileBase>& parent, const String& name,
     ByteIO::OpenOption, Int blockSize) const = 0;

    // Get the file name of the MultiFileBase container file.
    String fileName() const
      { return itsName; }

    // Is the container file writable?
    Bool isWritable() const
      { return itsWritable; }

    // Open the given logical file and return its file id.
    // If the name is unknown, an exception is thrown.
    // It allocates the internal buffer of the logical file.
    Int openFile (const String& name);

    // Create a new logical file and return its file id.
    // Only the base name of the given file name is used. In this way the
    // MultiFileBase container file can be moved.
    // If the logical file already exists, it is deleted if ByteIO::New is
    // given. Otherwise an exception is thrown.
    // It allocates the internal buffer of the logical file.
    Int createFile (const String& name, ByteIO::OpenOption = ByteIO::New);

    // Flush the possible dirty buffer of the given logical file.
    void flushFile (Int fileId);
    
    // Close a logical file.
    // It flushes and deallocates its buffer.
    void closeFile (Int fileId);
    
    // Delete a logical file. It adds its blocks to the free block list.
    void deleteFile (Int fileId);

    // Get the size of a logical file.
    Int64 fileSize (Int fileId) const;

    // Read a block at the given offset in the logical file.
    // It returns the actual size read.
    Int64 read (Int fileId, void* buffer, Int64 size, Int64 offset);

    // Write a block at the given offset in the logical file.
    // It returns the actual size written.
    Int64 write (Int fileId, const void* buffer, Int64 size, Int64 offset);

    // Truncate the logical file to the given size.
    void truncate (Int fileId, Int64 size);

    // Reopen the underlying file for read/write access.
    // Nothing will be done if the file is writable already.
    // Otherwise it will be reopened and an exception will be thrown
    // if it is not possible to reopen it for read/write access.
    virtual void reopenRW() = 0;

    // Flush the file by writing all dirty data and all header info.
    void flush();

    // Get the block size used.
    Int64 blockSize() const
      { return itsBlockSize; }

    // Get the nr of logical files.
    uInt nfile() const;

    // Get the total nr of data blocks used.
    Int64 nblock() const
      { return itsNrBlock; }

    // Get the info object (for test purposes mainly).
    const std::vector<MultiFileInfo>& info() const
      { return itsInfo; }

    // Get the free blocks (for test purposes mainly).
    const std::vector<Int64>& freeBlocks() const
      { return itsFreeBlocks; }

    // Return the file id of a file in the MultiFileBase object.
    // If the name is unknown, an exception is thrown if throwExcp is set.
    // Otherwise it returns -1.
    Int fileId (const String& name, Bool throwExcp=True) const;

    // Is O_DIRECT used?
    Bool useODirect() const
      { return itsUseODirect; }
    
  protected:
    // Resync with another process by clearing the buffers and rereading
    // the header. The header is only read if its counter has changed.
    void resync();

    // Fsync the file (i.e., force the data to be physically written).
    virtual void fsync() = 0;

  private:
    // Write the dirty block and clear dirty flag.
    void writeDirty (MultiFileInfo& info)
    {
      writeBlock (info, info.curBlock, info.buffer->data());
      info.dirty = False;
    }

    // Add a file to the MultiFileBase object. It returns the file id.
    // Only the base name of the given file name is used. In this way the
    // MultiFileBase container file can be moved.
    Int addFile (const String& name);

    // Do the class-specific actions on opening a logical file.
    virtual void doOpenFile (MultiFileInfo&) = 0;
    // Do the class-specific actions on closing a logical file.
    virtual void doCloseFile (MultiFileInfo&) = 0;
    // Do the class-specific actions on adding a logical file.
    virtual void doAddFile (MultiFileInfo&) = 0;
    // Do the class-specific actions on deleting a logical file.
    virtual void doDeleteFile (MultiFileInfo&) = 0;
    // Truncate the container file to <src>nrblk</src> blocks.
    virtual void doTruncateFile (MultiFileInfo& info, uInt64 nrblk) = 0;
    // Flush the container file.
    virtual void doFlushFile() = 0;
    // Flush and close the container file.
    virtual void close() = 0;
    // Write the header info.
    virtual void writeHeader() = 0;
    // Read the header info. If always==False, the info is only read if the
    // header counter has changed.
    virtual void readHeader (Bool always=True) = 0;
    // Extend a logical file to fit lastblk.
    virtual void extend (MultiFileInfo& info, Int64 lastblk) = 0;
    // Write a data block of a logical file into the container file.
    virtual void writeBlock (MultiFileInfo& info, Int64 blknr,
                             const void* buffer) = 0;
    // Read a data block of a logical file from the container file.
    virtual void readBlock (MultiFileInfo& info, Int64 blknr,
                            void* buffer) = 0;

  protected:
    // Set the flags and blockSize for a new MultiFile/HDF5.
    void setNewFile();

    //# Data members
    String itsName;
    Int64  itsBlockSize;  // The blocksize used
    Int64  itsNrBlock;    // The total nr of blocks actually used
    Int64  itsHdrCounter; // Counter of header changes
    std::vector<MultiFileInfo> itsInfo;
    std::shared_ptr<MultiFileBuffer> itsBuffer;  
    Bool          itsUseODirect; // use O_DIRECT?
    Bool          itsWritable;   // Is the file writable?
    Bool          itsChanged;    // Has header info changed since last flush?
    std::vector<Int64> itsFreeBlocks;
  };


} //# NAMESPACE CASACORE - END

#endif
