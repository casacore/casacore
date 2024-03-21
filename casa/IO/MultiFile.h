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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef CASA_MULTIFILE_H
#define CASA_MULTIFILE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/MultiFileBase.h>
#include <memory>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward declarations.
  class ByteIO;
  class CanonicalIO;
  class MemoryIO;


  // <summary> 
  // Class to combine multiple files in a single one.
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="" date="" tests="tMultiFile" demos="">
  // </reviewed>

  // <synopsis> 
  // This class (derived from MultiFileBase) is a container file holding
  // multiple virtual files in a regular file.
  // It is primarily meant as a container file for the storage manager files
  // of a table to reduce the number of files used (especially for Lustre) and
  // to reduce the number of open files (especially when concatenating tables).
  // <br> MultiFile has the following properties:
  // <ul>
  //  <li> It can choose an IO buffer size that matches the file system well
  //       (e.g., to support a large buffer size on ZFS or Lustre).
  //  <li> O_DIRECT (if supported by the OS) can be used to tell the OS kernel
  //       to bypass its file cache. It does not speed up the I/O, but it makes
  //       I/O behaviour more predictable which a real-time system might need.
  //  <li> Often the data to be read from MultiFile will not exactly match the
  //       block size and offset. MultiFile will buffer the data and copy the
  //       part that is needed (similar to stdio). However, when matching
  //       block size and offset are used, data will directly be read into the
  //       user's buffer to achieve zero-copy behaviour.
  //  <li> It is possible to nest MultiFile's. Thus a MultiFile can be a file
  //       in a parent MultiFile. In this way it is easily possible to store
  //       a main table and its subtables (such as an MS) in a single file. 
  //  <li> Optionally each block is stored with a 32-bit CRC to check if the
  //       data in a block are correctly read. The CRC values are stored as
  //       part of the header, thus not in each individual block. This is done
  //       to make the zero-copy behaviour possible (as described above).
  //  <li> The header and the index are stored in the first block. If too large,
  //       continuation blocks are used. There are two sets of continuation
  //       blocks between which is alternated. This is done for robustness
  //       purposes; there is always a valid one in case of a crash in the
  //       middle of writing the continuation blocks. Note that the first
  //       header block is written after the continuation blocks, so it always
  //       points to a valid set of continuation blocks.
  // </ul>
  //
  // The SetupNewTable constructor has a StorageOption argument to define
  // if a MultiFile has to be used and if so, the buffer size to use.
  // It is also possible to specify that through aipsrc variables.
  //
  // A virtual file is spread over multiple (fixed size) data blocks in the
  // MultiFile. A data block is never shared by multiple files.
  // For each virtual file MultiFile keeps a MultiFileInfo object telling
  // the file size and the block numbers used for the file. When flushing
  // the MultiFile, this meta info is written into the header block. If it
  // does not fit in the header block, the rest is written in continuation blocks.
  // On open and resync, it is read back. There are two sets of continuation
  // blocks which are alternately used when the header is written. This is done
  // to have a valid header in case of a crash in the middle of writing the header.
  //
  // A virtual file is represented by an MFFileIO object, which is derived
  // from ByteIO and as such part of the casacore IO framework. It makes it
  // possible for applications to access a virtual file in the same way as
  // a regular file.
  //
  // It is possible to delete a virtual file. Its blocks will be added to
  // the free block list (which is also stored in the meta info).
  // The MultiFile is truncated when blocks are deleted at the end of the file.
  // </synopsis>

  // <example>
  // In principle it is possible to use the MultiFile functions directly.
  // However, in general it is much easier to use an MFFileIO object
  // per virtual file as shown below.
  // <srcblock>
  //    // Create a new MultiFile using a block size of 1 MB.
  //    std::shared_ptr<MultiFileBase> mfile
  //           (new MultiFile("file.mf", ByteIO::New, 1048576));
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
  //  <li> MultiFile can be optimized how cont.blocks are used. In case of
  //       file truncation, it could check if only cont.blocks are present
  //       after the blocks to be removed. In such a case they can be moved
  //       backwards. Also the nr of cont.blocks can shrink. In such a case
  //       the unused blocks are not added to the free list. Only the nr of
  //       actually used cont.blocks is decremented. They could be added to
  //       the free list later.
  //       The reason for above is that the free list is written into the
  //       header blocks before the required nr of continuation blocks is known.
  //  <li> Keep a journal file telling which files are created and which
  //       blocks are allocated for a virtual file.
  // </todo>

  class MultiFile: public MultiFileBase
  {
  public:
    // Open or create a MultiFile with the given name.
    // Upon creation the block size can be given. If 0, it uses the block size
    // of the file system the file is on.
    // <br>If useODirect=True, the O_DIRECT flag is used (if supported).
    // It tells the kernel to bypass its file cache to have more predictable
    // I/O behaviour.
    // <br>If useCRC=True, 32-bit CRC values are calculated and stored for
    // each data block. Note that useCRC is only used for new files.
    explicit MultiFile (const String& name, ByteIO::OpenOption, Int blockSize=0,
                        Bool useODirect=False, Bool useCRC=False);

    // Open or create a MultiFile with the given name which is nested in the
    // given parent. Thus data are read/written in the parent file.
    // Upon creation the block size can be given. If 0, it uses the block size
    // of the parent.
    explicit MultiFile (const String& name,
                        const std::shared_ptr<MultiFileBase>& parent,
                        ByteIO::OpenOption, Int blockSize=0);

    // The destructor flushes and closes the file.
    ~MultiFile() override;

    // Copy constructor and assignment not possible.
    MultiFile (const MultiFile&) = delete;
    MultiFile& operator= (const MultiFile&) = delete;

    // Make a nested MultiFile.
    std::shared_ptr<MultiFileBase> makeNested
    (const std::shared_ptr<MultiFileBase>& parent, const String& name,
     ByteIO::OpenOption, Int blockSize) const override;

    // Reopen the underlying file for read/write access.
    // Nothing will be done if the file is writable already.
    // Otherwise it will be reopened and an exception will be thrown
    // if it is not possible to reopen it for read/write access.
    void reopenRW() override;

    // Fsync the file (i.e., force the data to be physically written).
    void fsync() override;

    // Show some info.
    void show (std::ostream&) const;

    // Compress a block index by looking for subsequent block numbers.
    static std::vector<Int64> packIndex (const std::vector<Int64>& blockNrs);

    // Decompress a block index by inserting subsequent block numbers.
    static std::vector<Int64> unpackIndex (const std::vector<Int64>& blockNrs);

  private:
    // Initialize the MultiFile object.
    void init (ByteIO::OpenOption option);
    // Read the file info for the new version 2.
    void getInfoVersion2 (Int64 contBlockNr, CanonicalIO& aio);
    // Write a vector of Int64.
    void writeVector (CanonicalIO& cio, const std::vector<Int64>& index);
    void writeVector (CanonicalIO& cio, const std::vector<uInt>& index);
    // Read a vector of Int64.
    void readVector (CanonicalIO& cio, std::vector<Int64>& index);
    void readVector (CanonicalIO& cio, std::vector<uInt>& index);
    // Write the remainder of the header (in case exceeding 1 block).
    // <src>iobuf</src> should be large enough
    void writeRemainder (MemoryIO& mio, CanonicalIO&, MultiFileBuffer& mfbuf);
    // Read the remainder of the header into the buffer.
    void readRemainder (Int64 headerSize, Int64 blockNr, std::vector<char>& buf);
    // Truncate the file if blocks are freed at the end.
    void truncateIfNeeded();
    // Header writing hooks (meant for derived test classes).
    virtual void writeHeaderShow (Int64 ncont, Int64 todo) const;
    virtual void writeHeaderTest();
    // </group>
    
    // Do the class-specific actions on opening a file.
    void doOpenFile (MultiFileInfo&) override;
    // Do the class-specific actions on closing a file.
    void doCloseFile (MultiFileInfo&) override;
    // Do the class-specific actions on adding a file.
    void doAddFile (MultiFileInfo&) override;
    // Do the class-specific actions on deleting a file.
    void doDeleteFile (MultiFileInfo&) override;
    // Truncate the file to <src>nrblk</src> blocks.
    void doTruncateFile (MultiFileInfo& info, uInt64 nrblk) override;
    // Flush the file itself.
    void doFlushFile() override;
    // Flush and close the file.
    void close() override;
    // Write the header info.
    void writeHeader() override;
    // Read the header info. If always==False, the info is only read if the
    // header counter has changed.
    void readHeader (Bool always=True) override;
    // Extend the virtual file to fit lastblk.
    void extend (MultiFileInfo& info, Int64 lastblk) override;

  protected:
    // Store the CRC of a data block in the index.
    void storeCRC (const void* buffer, Int64 blknr);
    // Check the CRC of a data block read.
    void checkCRC (const void* buffer, Int64 blknr) const;
    // Calculate the CRC of a data block.
    uInt calcCRC (const void* buffer, Int64 size) const;
    // Extend the virtual file to fit lastblk.
    // Optionally the free blocks are not used.
    virtual void extendVF (MultiFileInfo& info, Int64 lastblk, Bool useFreeBlocks);
    // Write a data block.
    void writeBlock (MultiFileInfo& info, Int64 blknr,
                     const void* buffer) override;
    // Read a data block.
    void readBlock (MultiFileInfo& info, Int64 blknr,
                    void* buffer) override;
    // Read the version 1 header.
    void readHeaderVersion1 (Int64 headerSize, std::vector<char>& buf);
    // Read the version 2 and higher header.
    void readHeaderVersion2 (std::vector<char>& buf);

    //# Data members
    // Define two continuation sets where the header overflow can be stored
    MultiFileInfo itsHdrCont[2];
    uInt  itsNrContUsed[2];     // nr of cont.blocks actually used
    uInt  itsHdrContInx;        // Continuation set last used (0 or 1)
    Bool  itsUseCRC;
    std::vector<uInt> itsCRC;   // CRC value per block (empty if useCRC=False)
    std::unique_ptr<ByteIO> itsIO;   // A regular file or nested MFFileIO
  };



} //# NAMESPACE CASACORE - END

#endif
