//# MultiFile.cc: Class to combine multiple files in a single one
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

//# Includes
#include <casacore/casa/IO/MultiFile.h>
#include <casacore/casa/IO/MFFileIO.h>
#include <casacore/casa/IO/FileUnbufferedIO.h>
#include <casacore/casa/IO/MemoryIO.h>
#include <casacore/casa/IO/CanonicalIO.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/OS/CanonicalConversion.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <cstdlib>
#include <memory>
#include <array>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // This function creates the CRC lookup table.
  // It is executed thread-safe on the first time called.
  static const std::array<uInt, 256>& CRCTable() 
  {
    static std::array<uInt, 256> result = [] () -> std::array<uInt, 256> {
      std::array<uInt, 256> res;
      
      const uInt polynom = 0x4c11db7;
      const uInt highbit = (uInt)1<<(32-1);
      // make CRC lookup table used by table algorithms
      for (int i=0; i<256; i++) {
        uInt crc = i;
        crc<<= 32-8;
        for (int j=0; j<8; j++) {
          uInt bit = crc & highbit;
          crc<<= 1;
          if (bit) crc^= polynom;
        }			
        res[i]= crc;    
      }
      return res;
    }();
    return result;
  }

/*
  MultiFile keeps a map of blocks in each individual file to the
  blocks in the overall file.
  This map is kept in the MultiFile header. If too large, the
  remainder is written at the end of the file. A possible problem
  with this scheme is that when the MultiFile gets extended, the new
  blocks overwrite the remainder of the map if the blocks occupied
  by the remainder are not counted. This leads to file corruption
  in case the program or system crashes.
  In order to write the remainder in a robust way, the header keeps
  two lists of continuation blocks which are used alternately. The
  first header block (written at offset 0) tells which continuation
  block is used. It is written after the continuation blocks, so
  there is always a valid header.
  In case the header shrinks, it is possible less continuation blocks
  are needed. The superfluous blocks are kept and will be used when the
  header grows again. If not growing, they are 'lost' which is no problem
  as there are far less header blocks than data blocks.
  A potential problem is that if data blocks get removed at the end, the
  file cannot be truncated because a continuation block is stored after
  those data blocks. In the future it might be wise to move such a block
  upward. However, in practice this will hardly ever occur. Note that
  the header size implicitly tells how many continuation blocks are
  actually used.
  Another small problem is that new continuation blocks are put at the end
  of the file and are not taken from the free list to avoid that the free
  list needs to be serialized again. Again, in practice this is hardly a
  problem because data blocks will be removed very seldomly.
 */

  MultiFile::MultiFile (const String& name, ByteIO::OpenOption option,
                        Int blockSize, Bool useODirect, Bool useCRC)
    : MultiFileBase (name, blockSize, useODirect),
      itsNrContUsed {0,0},
      itsHdrContInx (0),     // Start using the first continuation block
      itsUseCRC     (useCRC)
  {
    itsIO.reset (new FileUnbufferedIO (name, option, useODirect));
    init (option);
  }

  MultiFile::MultiFile (const String& name,
                        const std::shared_ptr<MultiFileBase>& parent,
                        ByteIO::OpenOption option, Int blockSize)
    // Use parent's block size if not specified.
    // A child MultiFile does not use CRC.
    : MultiFileBase (name, blockSize>0 ? blockSize:parent->blockSize(), False),
      itsNrContUsed {0,0},
      itsHdrContInx (0),     // Start using the first continuation block
      itsUseCRC     (False)
  {
    itsIO.reset (new MFFileIO (parent, name, option));
    init (option);
  }

  std::shared_ptr<MultiFileBase> MultiFile::makeNested
  (const std::shared_ptr<MultiFileBase>& parent, const String& name,
   ByteIO::OpenOption option, Int blockSize) const
  {
    return std::shared_ptr<MultiFileBase> (new MultiFile (name, parent,
                                                          option, blockSize));
  }

  void MultiFile::init (ByteIO::OpenOption option)
  {
    if (option == ByteIO::New  ||  option == ByteIO::NewNoReplace) {
      // New file; first block is for administration.
      setNewFile();
      itsNrBlock = 1;
    } else {
      readHeader();
    }
    itsWritable = itsIO->isWritable();
  }

  MultiFile::~MultiFile()
  {
    close();
  }

  void MultiFile::doFlushFile()
  {
    itsIO->flush();
  }

  void MultiFile::close()
  {
    // Flush.
    flush();
    // Clear all file info.
    itsInfo.clear();
    // Delete the file object.
    itsIO.reset (0);
  }

  void MultiFile::reopenRW()
  {
    if (isWritable()) {
      return;
    }
    itsIO->reopenRW();
    itsWritable = True;
  }

  void MultiFile::fsync()
  {
    itsIO->fsync();
  }

  void MultiFile::writeHeader()
  {
    // Write all header info in canonical format into a memory buffer.
    // This buffer will be written into the file.
    // If too large, the remainder is written into continuation blocks.
    // There are 2 sets of continuation blocks to avoid that the header
    // gets corrupted in case of a crash while writing the header.
    MemoryIO mio(itsBlockSize, itsBlockSize);
    CanonicalIO cio(&mio);
    AipsIO aio(&cio);
    itsHdrCounter++;
    Int64 zero64 = 0;
    uInt  zero32 = 0;
    char  char8[8] = {0,0,0,0,0,0,0,0};
    Int   version = 2;
    // Start with a zero to distinguish it from version 1.
    // The first value in version 1 is always > 0.
    cio.write (1, &zero64);
    cio.write (1, &zero64);        // room for first cont.block (writeRemainder)
    cio.write (1, &itsHdrCounter);
    cio.write (1, &version);
    cio.write (1, &zero32);        // headerCRC
    cio.write (1, &zero64);        // header size
    cio.write (1, &itsBlockSize);
    cio.write (1, &itsNrBlock);
    if (itsUseCRC) char8[0] = 1;
    cio.write (8, char8);
    AlwaysAssert (mio.length() == 64, AipsError);
    // First write general info and file names, etc.
    aio.putstart ("MultiFile", version);
    aio << itsInfo;
    aio.putend();
    // Write the used and free blocknrs in packed format but outside
    // the AipsIO object because that is limited to 4 GB.
    for (const MultiFileInfo& fileInfo : info()) {
      writeVector (cio, packIndex(fileInfo.blockNrs));
    }
    writeVector (cio, packIndex(freeBlocks()));
    writeVector (cio, itsCRC);
    // Calculate the size including the continuation blocknrs and number of
    // actually used blocknrs.
    // If continuation is needed, they might change and cannot
    // be written yet.
    Int64 todo = mio.length();
    DebugAssert (static_cast<uInt64>(todo) <= mio.allocated(), AipsError);
    Int64 totalSize = todo + 2*sizeof(uInt) + (2 + itsHdrCont[0].blockNrs.size() +
                              itsHdrCont[1].blockNrs.size()) * sizeof(Int64);
    // Allocate a temp buffer if header too large or if O_DIRECT.
    // This buffer is used by writeRemainder and 
    Bool hasRemainder = (totalSize > itsBlockSize);
    MultiFileBuffer mfbuf(itsUseODirect || hasRemainder  ?  itsBlockSize:0,
                          itsUseODirect);
    // First write the remainder and adjust the header as needed.
    if (hasRemainder) {
      writeRemainder (mio, cio, mfbuf);
    } else {
      // No remainder, so write the continuation blocknrs here.
      // None are used in the current set.
      writeVector (cio, itsHdrCont[0].blockNrs);
      writeVector (cio, itsHdrCont[1].blockNrs);
      itsNrContUsed[itsHdrContInx] = 0;
      cio.write (2, itsNrContUsed);
    }
    // Writing the first part of the header is done as the last step.
    uChar* buf = const_cast<uChar*>(mio.getBuffer());
    todo = mio.length();
    CanonicalConversion::fromLocal (buf+32, todo);  // header size
    if (itsUseCRC) {
      uInt crc = calcCRC (buf, todo);
      crc = calcCRC (buf, todo);
      CanonicalConversion::fromLocal (buf+28, crc);
    }
    writeHeaderTest();            // hook for tMultiFileLarge.cc
    if (itsUseODirect) {
      char* iobuf = mfbuf.data();
      memcpy (iobuf, buf, itsBlockSize);
      itsIO->pwrite (itsBlockSize, 0, iobuf);
    } else {
      itsIO->pwrite (itsBlockSize, 0, buf);
    }
  }

  void MultiFile::writeRemainder (MemoryIO& mio, CanonicalIO& cio,
                                  MultiFileBuffer& mfbuf)
  {
    char* iobuf = mfbuf.data();
    // Get the size of the remainder.
    Int64 todo = mio.length() - itsBlockSize;
    // Use the other continuation set.
    int newContInx = 1 - itsHdrContInx;
    // Determine the nr of continuation blocks to be used.
    // The first 8 bytes per block are used to point to the next one.
    // Furthermore, the old and new cont-block vector have to be counted
    // as well (length and all entries).
    // So it can happen that an extra block is needed which in its turn
    // might require an extra block, etc.
    // To avoid that the free list is changed and needs to be rewritten,
    // new continuation blocks are created by adding a new block.
    Int64 contBlkSize = itsBlockSize - sizeof(Int64);
    Int64 ncontOther = itsHdrCont[itsHdrContInx].blockNrs.size();
    Int64 ncontOld   = itsHdrCont[newContInx].blockNrs.size();
    Int64 ncont      = (todo + contBlkSize - 1) / contBlkSize;
    Int64 ncontNew   = std::max(ncont, ncontOld);
    while (True) {
      Int64 szCont = (todo + 2*sizeof(uInt) +
                      (ncontOther + ncontNew + 2) * sizeof(Int64));
      ncont = (szCont + contBlkSize - 1) / contBlkSize;
      if (ncont <= ncontNew) {
        break;
      }
      ncontNew = ncont;
    }
    // Extend the virtual file.
    if (ncontNew > ncontOld) {
      extendVF (itsHdrCont[newContInx], ncontNew, False);
    }
    // Write the continuation blocknrs.
    writeVector (cio, itsHdrCont[0].blockNrs);
    writeVector (cio, itsHdrCont[1].blockNrs);
    itsNrContUsed[newContInx] = ncontNew;
    cio.write (2, itsNrContUsed);
    // Write the continuation blocks.
    const std::vector<Int64>& hdrContNrs = itsHdrCont[newContInx].blockNrs;
    const uChar* remHdr = mio.getBuffer() + itsBlockSize;
    todo = mio.length() - itsBlockSize;
    writeHeaderShow (ncont, todo);
    for (Int64 i=0; i<ncont; ++i) {
      Int64 next = (i==ncont-1 ? Int64(0) : hdrContNrs[i+1]);
      CanonicalConversion::fromLocal (iobuf, next);
      memcpy (iobuf + sizeof(Int64), remHdr, std::min(todo,contBlkSize));
      remHdr += contBlkSize;
      todo -= contBlkSize;
      itsIO->pwrite (itsBlockSize, hdrContNrs[i]*itsBlockSize, iobuf);
    }
    // Store first continuation blocknr in header.
    uChar* buf = const_cast<uChar*>(mio.getBuffer());
    CanonicalConversion::fromLocal (buf+8, hdrContNrs[0]);
    // Swap continuation sets.
    itsHdrContInx = newContInx;
    // Store possibly changed nr of blocks.
    CanonicalConversion::fromLocal (buf+48, itsNrBlock);
  }

  void MultiFile::writeVector (CanonicalIO& cio, const std::vector<Int64>& index)
  {
    // Write in the same way as AipsIO is doing.
    Int64 sz = index.size();
    cio.write (1, &sz);
    if (sz > 0) {
      cio.write (index.size(), index.data());
    }
  }

  void MultiFile::writeVector (CanonicalIO& cio, const std::vector<uInt>& index)
  {
    // Write in the same way as AipsIO is doing.
    Int64 sz = index.size();
    cio.write (1, &sz);
    if (sz > 0) {
      cio.write (index.size(), index.data());
    }
  }

  void MultiFile::readVector (CanonicalIO& cio, std::vector<Int64>& index)
  {
    Int64 sz;
    cio.read (1, &sz);
    if (sz > 0) {
      index.resize (sz);
      cio.read (sz, index.data());
    } else {
      index.clear();
    }
  }

  void MultiFile::readVector (CanonicalIO& cio, std::vector<uInt>& index)
  {
    Int64 sz;
    cio.read (1, &sz);
    if (sz > 0) {
      index.resize (sz);
      cio.read (sz, index.data());
    } else {
      index.clear();
    }
  }

  void MultiFile::readHeader (Bool always)
  {
    /*  header layout is described in Casacore note 260, but repeated here.
        version 1
          Int64  header size
          Int64  blockSize
          Int64  hdrCounter
          AipsIO 'MultiFile' version 1
              Int64   nr of blocks used
              nfile*fileinfo
                  String   file name
                  Int64[n] index (MultiFile block containing file block i)
                  Int64    file size (bytes)
              Note that .hdrext is used if header does not fit in first block
        version 2
          Int64  0
          Int64  first block of header continuation (<0=none)
          Int64  hdrCounter
          Int32  version
          uInt32 headerCRC
          Int64  header size
          Int64  blockSize
          char   useCRC
          char[7] spare
          AipsIO 'MultiFile' with same version as above
              Int64   nr of blocks used
              nfile*fileinfo
                  String   file name
                  Int64    file size (bytes)
                  Bool     nested MultiFile?
          nfile*index
              Int64        size of packed index
              Int64[size]  packed index (MultiFile block of file block i)
          index      packed index of free blocks
          Int32[nblock]    CRC value of each block (only if useCRC=1)
          Int64[ncont0]    block numbers of header continuation buffer 0
          Int64[ncont1]    block numbers of header continuation buffer 1
              Note that 'first block of header' tells if cont.block 0 or 1
              is used by comparing it with the first entry.

    When writing index:
    - Determine nr of blocks needed. If > 1 perform next steps:
    - Re-determine nr of header blocks needed
    - Take the other cont.block and extend that as needed
           take new blocks from EOF (otherwise free list changes)
    - Make CRC of entire header (with 0 in headerCRC)
    - First write cont.blocks and finally first block (reset cont.blocknr)
    */
    // Read the first 24 bytes (3x Int64) of the header.
    std::vector<char> buf(3*sizeof(Int64));
    itsIO->pread (buf.size(), 0, buf.data());
    // First get the header change count.
    Int64 hdrCounter;
    CanonicalConversion::toLocal (hdrCounter, &(buf[16]));
    // Only if needed, read the rest of the header.
    if (always  ||  hdrCounter != itsHdrCounter) {
      itsHdrCounter = hdrCounter;
      // In version 1 the headerSize is in the first 8 bytes.
      // For version 2 and higher it is 0.
      Int64 headerSize;
      CanonicalConversion::toLocal (headerSize, buf.data());
      if (headerSize == 0) {
        readHeaderVersion2 (buf);
      } else {
        readHeaderVersion1 (headerSize, buf);
      }
      // Initialize remaining info fields.
      for (MultiFileInfo& info : itsInfo) {
        info.curBlock = -1;
        info.dirty    = False;
      }
    }
  }

  void MultiFile::readHeaderVersion1 (Int64 headerSize, std::vector<char>& buf)
  {
    Int64 leadSize = buf.size();
    AlwaysAssert (leadSize==24, AipsError);
    CanonicalConversion::toLocal (itsBlockSize, &(buf[8]));
    buf.resize (headerSize);
    if (headerSize <= itsBlockSize) {
      // Only one header block; read only the part that is needed.
      itsIO->pread (headerSize - leadSize, leadSize, &(buf[leadSize]));
    } else {
      // Read the first header block and the remainder.
      itsIO->pread (itsBlockSize - leadSize, leadSize, &(buf[leadSize]));
      // For version 1 the remaining header info is in the .hdrext file.
      FileUnbufferedIO iohdr(itsName + "_hdrext", ByteIO::Old);
      iohdr.read (headerSize - itsBlockSize, &(buf[itsBlockSize]));
    }
    // Read all header info from the memory buffer.
    MemoryIO mio(&(buf[leadSize]), headerSize - leadSize);
    CanonicalIO cio(&mio);
    AipsIO aio(&cio);
    Int version = aio.getstart ("MultiFile");
    AlwaysAssert (version==1, AipsError);
    aio >> itsNrBlock;
    getInfoVersion1 (aio, itsInfo);
    aio >> itsFreeBlocks;
    aio.getend();
  }

  void MultiFile::readHeaderVersion2 (std::vector<char>& buf)
  {
    Int64 leadSize = buf.size();
    AlwaysAssert (leadSize==24, AipsError);
    Int64 contBlockNr = 0;
    Int64 headerSize;
    uInt  headerCRC = 0;
    Int   version;
    // For version 2 and higher the next 40 bytes have to be read.
    buf.resize (leadSize + 40);
    itsIO->pread (40, leadSize, &(buf[leadSize]));
    leadSize += 40;
    CanonicalConversion::toLocal (contBlockNr, &(buf[8]));
    CanonicalConversion::toLocal (version, &(buf[24]));
    // This version of MultiFile can only handle version 2.
    // Future versions might use higher version numbers.
    if (version != 2) {
      throw AipsError("This version of Casacore supports up to MultiFile "
                      "version 2, not version " + String::toString(version));
    }
    CanonicalConversion::toLocal (headerCRC, &(buf[28]));
    CanonicalConversion::toLocal (headerSize, &(buf[32]));
    CanonicalConversion::toLocal (itsBlockSize, &(buf[40]));
    CanonicalConversion::toLocal (itsNrBlock, &(buf[48]));
    char tmpc;
    CanonicalConversion::toLocal (tmpc, &(buf[56]));
    itsUseCRC = (tmpc!=0);
    buf.resize (headerSize);
    if (headerSize <= itsBlockSize) {
      // Only one header block; read only the part that is needed.
      itsIO->pread (headerSize - leadSize, leadSize, &(buf[leadSize]));
    } else {
      // Read the first header block and the remainder.
      itsIO->pread (itsBlockSize - leadSize, leadSize, &(buf[leadSize]));
      readRemainder (headerSize, contBlockNr, buf);
    }
    // Check the CRC if needed.
    if (itsUseCRC) {
      // Set headerCRC in buffer to 0 to calculate the correct CRC.
      uInt zero32 = 0;
      CanonicalConversion::fromLocal (&(buf[28]), zero32);
      uInt crc = calcCRC (buf.data(), headerSize);
      if (crc != headerCRC) {
        throw AipsError("MultiFile header CRC mismatch in " + fileName());
      }
    }
    // Read all header info from the memory buffer.
    MemoryIO mio(&(buf[leadSize]), headerSize - leadSize);
    CanonicalIO cio(&mio);
    AipsIO aio(&cio);
    Int vers = aio.getstart ("MultiFile");
    AlwaysAssert (vers==version, AipsError);
    aio >> itsInfo;
    aio.getend();
    getInfoVersion2 (contBlockNr, cio);
  }

  void MultiFile::getInfoVersion2 (Int64 contBlockNr, CanonicalIO& cio)
  {
    std::vector<Int64> bl;
    for (MultiFileInfo& fileInfo: itsInfo) {
      readVector (cio, bl);
      fileInfo.blockNrs = unpackIndex(bl);
    }
    readVector (cio, bl);
    itsFreeBlocks = unpackIndex(bl);
    readVector (cio, itsCRC);
    if (! itsUseCRC) {
      AlwaysAssert (itsCRC.size() == 0, AipsError);
    }
    // Read the header continuation info.
    // Determine which of them is in use.
    readVector (cio, itsHdrCont[0].blockNrs);
    readVector (cio, itsHdrCont[1].blockNrs);
    cio.read (2, itsNrContUsed);
    itsHdrContInx = 0;
    if (! itsHdrCont[1].blockNrs.empty()  &&
        contBlockNr == itsHdrCont[1].blockNrs[0]) {
      itsHdrContInx = 1;
    }
  }

  void MultiFile::readRemainder (Int64 headerSize, Int64 blockNr,
                                 std::vector<char>& buf)
  {
    MultiFileBuffer mfbuf(itsBlockSize, itsUseODirect);
    char* iobuf = mfbuf.data();
    size_t off = itsBlockSize;
    Int64 blknr = blockNr;
    Int64 todo = headerSize - itsBlockSize;
    Int64 contBlkSize = itsBlockSize - sizeof(Int64);
    while (off < buf.size()) {
      AlwaysAssert (blknr!=0, AipsError);
      itsIO->pread (itsBlockSize, blknr*itsBlockSize, iobuf);
      memcpy (&(buf[off]), iobuf + sizeof(Int64),
              std::min(todo,contBlkSize));
      off += contBlkSize;
      todo -= contBlkSize;
      CanonicalConversion::toLocal (blknr, iobuf);
    }
    // No continuation block should be given at the end.
    AlwaysAssert (blknr==0, AipsError);
  }

  void MultiFile::doOpenFile (MultiFileInfo&)
  {}

  void MultiFile::doCloseFile (MultiFileInfo&)
  {}

  void MultiFile::doAddFile (MultiFileInfo&)
  {}

  void MultiFile::doDeleteFile (MultiFileInfo& info)
  {
    // Add all file blocks to the free list.
    doTruncateFile (info, 0);
  }
  
  void MultiFile::doTruncateFile (MultiFileInfo& info, uInt64 nrblk)
  {
    if (nrblk < info.blockNrs.size()) {
      // Add the blocknrs to the free list.
      // Later we can merge them in order and leave out blocks past last block used.
      itsFreeBlocks.reserve (itsFreeBlocks.size() + info.blockNrs.size() - nrblk);
      for (size_t i=nrblk; i<info.blockNrs.size(); ++i) {
        itsFreeBlocks.push_back (info.blockNrs[i]);
        // Set CRC for deleted blocks to 0.
        if (itsUseCRC) {
          itsCRC[info.blockNrs[i]] = 0;
        }
      }
      // Sort them in descending order, so free blocks can be taken from the tail.
      genSort (itsFreeBlocks.data(), itsFreeBlocks.size(),
               Sort::Descending, Sort::QuickSort);
      // The file can be truncated if blocks are freed at the end.
      truncateIfNeeded();
    }
  }

  void MultiFile::truncateIfNeeded()
  {
    // Possibly check here if cont.blocks are stored at the end. If so,
    // move them upfront if possible by using first free blocks. !!!!!
    // Truncate the file for free blocks at the end of the file.
    Int lastBlock = itsNrBlock-1;
    size_t i=0;
    for (; i<itsFreeBlocks.size(); ++i) {
      if (itsFreeBlocks[i] != lastBlock) {
        break;
      }
      lastBlock--;
    }
    if (i > 0) {
      itsFreeBlocks.erase (itsFreeBlocks.begin(), itsFreeBlocks.begin() + i);
      itsNrBlock -= i;
      itsIO->truncate (itsNrBlock * itsBlockSize);
      if (itsUseCRC) {
        itsCRC.resize (itsNrBlock);
      }
    }
  }

  void MultiFile::extend (MultiFileInfo& info, Int64 lastblk)
  {
    extendVF (info, lastblk, True);
  }

  void MultiFile::extendVF (MultiFileInfo& info, Int64 lastblk,
                            Bool useFreeBlocks)
  {
    Int64 curnrb = info.blockNrs.size();
    info.blockNrs.resize (lastblk);
    Int64 nfree = itsFreeBlocks.size();
    for (Int64 i=curnrb; i<lastblk; ++i) {
      if (!useFreeBlocks  ||  nfree==0) {
        info.blockNrs[i] = itsNrBlock;
        itsNrBlock++;
      } else {
        info.blockNrs[i] = itsFreeBlocks[--nfree];
      }
    }
    if (nfree != Int64(itsFreeBlocks.size())) {
      itsFreeBlocks.resize (nfree);
    }
  }

  void MultiFile::writeHeaderShow(Int64, Int64) const
  {}
  void MultiFile::writeHeaderTest()
  {}
  
  void MultiFile::readBlock (MultiFileInfo& info, Int64 blknr,
                             void* buffer)
  {
    itsIO->pread (itsBlockSize, info.blockNrs[blknr] * itsBlockSize, buffer);
    if (itsUseCRC) {
      checkCRC (buffer, info.blockNrs[blknr]);
    }
  }

  void MultiFile::writeBlock (MultiFileInfo& info, Int64 blknr,
                              const void* buffer)
  {
    itsIO->pwrite (itsBlockSize, info.blockNrs[blknr] * itsBlockSize, buffer);
    if (itsUseCRC) {
      storeCRC (buffer, info.blockNrs[blknr]);
    }
  }

  void MultiFile::storeCRC (const void* buffer, Int64 blknr)
  {
    if (blknr >= Int64(itsCRC.size())) {
      itsCRC.resize (blknr+1);
    }
    itsCRC[blknr] = calcCRC (buffer, itsBlockSize);
  }

  void MultiFile::checkCRC (const void* buffer, Int64 blknr) const
  {
    AlwaysAssert (blknr < Int64(itsCRC.size()), AipsError);
    uInt crc = calcCRC (buffer, itsBlockSize);
    if (crc != itsCRC[blknr]) {
      throw AipsError ("Mismatch in CRC of MultiFile block " +
                       String::toString(blknr));
    }
  }

  uInt MultiFile::calcCRC (const void* buffer, Int64 size) const
  {
    // The algorithm is taken from crctester.c by Sven Reifegerste
    // (www.zorc/reflex)
    // See also https://www.lammertbies.nl/comm/info/crc-calculation
    const uChar* buf = static_cast<const uChar*>(buffer);
    const uInt crcinit = 0x46af6449;
    //# The above value is calculated from:
    //#         const uInt highbit = (uInt)1<<(32-1);
    //#		crcinit = 0xffffffff;
    //#		for (i=0; i<32; i++) {
    //#			bit = crcinit & 1;
    //#			if (bit) crcinit^= polynom;
    //#			crcinit >>= 1;
    //#			if (bit) crcinit|= highbit;
    //#		}	
    // Normal lookup table algorithm with augmented zero bytes.
    uInt crc = crcinit;
    for (Int64 i=0; i<size; ++i) {
      crc = ((crc << 8) | buf[i]) ^ CRCTable()[ (crc >> (32-8))  & 0xff];
    }
    // Augment zero bytes.
    for (size_t i=0; i<32/8; ++i) {
      crc = (crc << 8) ^ CRCTable()[ (crc >> (32-8))  & 0xff];
    }
    crc^= 0xffffffff;
    return crc;
  }

  void MultiFile::show (std::ostream& os) const
  {
    os << fileName() << ": blocksize=" << blockSize()
       << "  nfile="  << nfile() << "  nblock=" << nblock()
       << "  nCRC=" << itsCRC.size() << endl
       << "  ncont=" << itsNrContUsed[0] << ',' << itsNrContUsed[1]
       << "  cont=" << itsHdrContInx
       << ' ' << itsHdrCont[itsHdrContInx].blockNrs
       << "  free=" << freeBlocks() << endl;
  }

  std::vector<Int64> MultiFile::packIndex (const std::vector<Int64>& blockNrs)
  {
    // See if subsequent block numbers are used, so the index can be
    // compressed.
    // Compression is done by counting the nr of subsequent blocknrs
    // and writing the negative count if > 0.
    // Note that the count does not include the first block number.
    std::vector<Int64> buf;
    if (blockNrs.empty()) {
      return std::vector<Int64>();
    }
    buf.reserve (blockNrs.size());
    buf.push_back (blockNrs[0]);
    Int64 next = blockNrs[0] + 1;
    for (size_t j=1; j<blockNrs.size(); ++j) {
      if (blockNrs[j] != next) {
        Int64 nr = next - buf.back() - 1;
        if (nr > 0) {
          buf.push_back (-nr);
        }
        next = blockNrs[j];
        buf.push_back (next);
      }
      next++;
    }
    Int64 nr = next - buf.back() - 1;
    if (nr > 0) {
      buf.push_back (-nr);
    }
    return buf;
  }

  std::vector<Int64> MultiFile::unpackIndex (const std::vector<Int64>& blockNrs)
  {
    // Expand if subsequent block numbers are used indicated by a
    // negative value.
    std::vector<Int64> buf;
    buf.reserve (blockNrs.size());
    for (auto bl : blockNrs) {
      if (bl < 0) {
        Int64 next = buf.back() + 1;
        size_t nr = -bl;
        for (size_t i=0; i<nr; ++i) {
          buf.push_back (next+i);
        }
      } else {
        buf.push_back (bl);
      }
    }
    return buf;
  }


} //# NAMESPACE CASACORE - END
