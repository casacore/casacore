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
//#
//# $Id: RegularFileIO.h 20551 2009-03-25 00:11:33Z Malte.Marquarding $

//# Includes
#include <casacore/casa/IO/MultiFile.h>
#include <casacore/casa/IO/RegularFileIO.h>
#include <casacore/casa/IO/MemoryIO.h>
#include <casacore/casa/IO/CanonicalIO.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/OS/CanonicalConversion.h>
#include <casacore/casa/OS/File.h>           // for fileFSTAT
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <unistd.h>
#include <sys/stat.h>               // needed for stat or stat64

namespace casa { //# NAMESPACE CASA - BEGIN

  void operator<< (ostream& ios, const MultiFileInfo& info)
    { ios << info.name << ' ' << info.blockNrs << ' ' << info.size << ' '
          << info.curBlock << ' ' << info.dirty << endl; }
  void operator<< (AipsIO& ios, const MultiFileInfo& info)
    { ios << info.name << info.blockNrs << info.size; }
  void operator>> (AipsIO& ios, MultiFileInfo& info)
    { ios >> info.name >> info.blockNrs >> info.size; }


  MultiFile::MultiFile (const String& name, ByteIO::OpenOption option,
                        Int blockSize)
    : itsBlockSize  (blockSize),
      itsHdrCounter (0),
      itsChanged    (False)
  {
    itsFD = RegularFileIO::openCreate (name, option);
    itsIO.attach (itsFD, name);
    if (option == ByteIO::New  ||  option == ByteIO::NewNoReplace) {
      // New file; first block is for administration.
      itsNrBlock = 1;
      itsChanged = True;
      // Use file system block size, but not less than given size.
      if (itsBlockSize <= 0) {
        struct fileSTAT sfs;
        fileFSTAT (itsFD, &sfs);
        Int64 blksz = sfs.st_blksize;
        itsBlockSize = std::max (-itsBlockSize, blksz);
      }
    } else {
      readHeader();
    }
    AlwaysAssert (itsBlockSize > 0, AipsError);
  }

  MultiFile::~MultiFile()
  {
    close();
  }

  uInt MultiFile::nfile() const
  {
    Int nf = 0;
    for (vector<MultiFileInfo>::const_iterator iter=itsInfo.begin();
         iter!=itsInfo.end(); ++iter) {
      if (! iter->name.empty()) {
        nf++;
      }
    }
    return nf;
  }

  void MultiFile::close()
  {
    flush();
    itsInfo.clear();
    FiledesIO::close (itsFD);
  }

  void MultiFile::reopenRW()
  {
    if (isWritable()) {
      return;
    }
    String fname = itsIO.fileName();
    // First try if the file can be opened as read/write.
    int fd = RegularFileIO::openCreate (fname, ByteIO::Update);
    // Now close the readonly file and reset fd.
    FiledesIO::close (itsFD);
    itsIO.detach();
    itsFD = fd;
    itsIO.attach (itsFD, fname);
    itsIO.setWritable();
}

  void MultiFile::flush()
  {
    // Flush all buffers if needed.
    for (vector<MultiFileInfo>::iterator iter=itsInfo.begin();
         iter!=itsInfo.end(); ++iter) {
      if (iter->dirty) {
        writeDirty (*iter);
      }
    }
    // Header only needs to be written if blocks were added since last flush.
    if (itsChanged) {
      writeHeader();
      itsChanged = False;
    }
  }

  void MultiFile::resync()
  {
    AlwaysAssert (!itsChanged, AipsError);
    // Clear all blocknrs.
    for (vector<MultiFileInfo>::iterator iter=itsInfo.begin();
         iter!=itsInfo.end(); ++iter) {
      AlwaysAssert (!iter->dirty, AipsError);
      iter->curBlock = -1;
    }
    readHeader();
  }

  void MultiFile::writeHeader()
  {
    // Write all header info into a memory buffer.
    // This buffer will be written into the file.
    // If needed, spread over multiple blocks.
    MemoryIO mio(itsBlockSize, itsBlockSize);
    CanonicalIO cio(&mio);
    AipsIO aio(&cio);
    Int64 next=0;
    itsHdrCounter++;
    cio.write (1, &next);         // possible link to subsequent header block
    cio.write (1, &next);         // reserve space for header size
    cio.write (1, &itsBlockSize);
    cio.write (1, &itsHdrCounter);
    aio.putstart ("MultiFile", 1);
    aio << itsNrBlock << itsInfo << itsFreeBlocks;
    aio.putend();
    Int64 todo = mio.length();
    uChar* buf = const_cast<uChar*>(mio.getBuffer());
    CanonicalConversion::fromLocal (buf + sizeof(next), todo);    // header size
    // If the buffer does not fit in a single block, next has to be filled.
    if (todo > itsBlockSize) {
      next = itsNrBlock;
      CanonicalConversion::fromLocal (buf, next);
    }
    // Write the first part of the buffer at the beginning of the file.
    itsIO.seek (0);
    itsIO.write (itsBlockSize, buf);
    todo -= itsBlockSize;
    if (todo > 0) {
      // The rest is written at the end of the file.
      itsIO.seek (itsNrBlock*itsBlockSize);
      itsIO.write (todo, buf+itsBlockSize);
    }
  }

  void MultiFile::readHeader (Bool always)
  {
    // Read the first part of the header.
    vector<char> buf(4*sizeof(Int64));
    itsIO.seek (0);
    itsIO.read (buf.size(), &(buf[0]));
    // Extract the required info.
    Int64 next, headerSize, hdrCounter;
    CanonicalConversion::toLocal (next, &(buf[0]));
    CanonicalConversion::toLocal (headerSize, &(buf[sizeof(Int64)]));
    CanonicalConversion::toLocal (itsBlockSize, &(buf[2*sizeof(Int64)]));
    CanonicalConversion::toLocal (hdrCounter, &(buf[3*sizeof(Int64)]));
    // Only if needed, read the rest of the header.
    if (hdrCounter == itsHdrCounter  &&  !always) {
      return;
    }
    itsHdrCounter = hdrCounter;
    Int64 leadSize = 4*sizeof(Int64);
    buf.resize (headerSize);
    if (headerSize > itsBlockSize) {
      itsIO.read (itsBlockSize - leadSize, &(buf[leadSize]));
      itsIO.seek (next*itsBlockSize);
      itsIO.read (headerSize - itsBlockSize, &(buf[itsBlockSize]));
    } else {
      itsIO.read (headerSize - leadSize, &(buf[leadSize]));
    }
    // Read all header info from the memory buffer.
    MemoryIO mio(&(buf[leadSize]), headerSize - leadSize);
    CanonicalIO cio(&mio);
    AipsIO aio(&cio);
    Int version = aio.getstart ("MultiFile");
    AlwaysAssert (version==1, AipsError);
    aio >> itsNrBlock >> itsInfo >> itsFreeBlocks;
    aio.getend();
    // Initialize remaining info fields.
    for (vector<MultiFileInfo>::iterator iter=itsInfo.begin();
         iter!=itsInfo.end(); ++iter) {
      iter->curBlock = -1;
      iter->dirty    = False;
      iter->buffer.resize (itsBlockSize);
    }
  }

  Int MultiFile::addFile (const String& fname)
  {
    if (fname.empty()) {
      throw AipsError("MultiFile::addFile - empty file name given");
    }
    // Only use the basename part (to avoid directory rename problems).
    String bname = Path(fname).baseName();
    // Check that file name is not used yet.
    // Also determine (last) free file slot.
    uInt inx = itsInfo.size();
    uInt i = 0;
    for (vector<MultiFileInfo>::iterator iter=itsInfo.begin();
         iter!=itsInfo.end(); ++iter, ++i) {
      if (iter->name.empty()) {
        inx = i;      // free file slot
      } else if (bname == iter->name) {
        throw AipsError ("MultiFile::addFile - file name " + bname +
                         " already in use");
      }
    }
    // Add a new file entry if needed.
    if (inx == itsInfo.size()) {
      itsInfo.resize (inx+1);
    }
    itsInfo[inx] = MultiFileInfo(itsBlockSize);
    itsInfo[inx].name = bname;
    itsChanged = True;
    return inx;
  }

  Int MultiFile::fileId (const String& fname, Bool throwExcp) const
  {
    // Only use the basename part (to avoid directory rename problems).
    String bname = Path(fname).baseName();
    for (size_t i=0; i<itsInfo.size(); ++i) {
      if (bname == itsInfo[i].name) {
        return i;
      }
    }
    if (throwExcp) {
      throw AipsError ("MultiFile::fileId - file name " + fname +
                       " is unknown");
    }
    return -1;
  }

  void MultiFile::deleteFile (Int fileId)
  {
    if (fileId >= Int(itsInfo.size())  ||  itsInfo[fileId].name.empty()) {
      throw AipsError ("MultiFile::deleteFile - invalid fileId given");
    }
    // Add the blocknrs to the free list.
    // Later we can merge them in order and leave out blocks past last block used.
    MultiFileInfo& info = itsInfo[fileId];
    itsFreeBlocks.reserve (itsFreeBlocks.size() + info.blockNrs.size());
    for (size_t i=0; i<info.blockNrs.size(); ++i) {
      itsFreeBlocks.push_back (info.blockNrs[i]);
    }
    // Sort them in descending order, so free blocks can be taken from the tail.
    genSort (&(itsFreeBlocks[0]), itsFreeBlocks.size(),
             Sort::Descending, Sort::QuickSort);
    // Clear this slot.
    info = MultiFileInfo();
    itsChanged = True;
  }

  void MultiFile::writeDirty (MultiFileInfo& info)
  {
    itsIO.seek (info.blockNrs[info.curBlock] * itsBlockSize);
    itsIO.write (itsBlockSize, &(info.buffer[0]));
    info.dirty = False;
  }

  Int64 MultiFile::read (Int fileId, void* buf, Int64 size, Int64 offset)
  {
    if (fileId >= Int(itsInfo.size())  ||  itsInfo[fileId].name.empty()) {
      throw AipsError ("MultiFile::read - invalid fileId given");
    }
    char* buffer = static_cast<char*>(buf);
    MultiFileInfo& info = itsInfo[fileId];
    // Determine the logical block to read and the start offset in that block.
    Int64 nrblk = info.blockNrs.size();
    Int64 blknr = offset/itsBlockSize;
    Int64 start = offset - blknr*itsBlockSize;
    Int64 done  = 0;
    Int64 szdo  = std::min(size, info.size - offset);  // not past EOF
    // Read until done.
    while (done < szdo) {
      AlwaysAssert (blknr<nrblk, AipsError);
      Int64 todo = std::min(szdo-done, itsBlockSize-start);
      // If already in buffer, copy from there.
      if (blknr == info.curBlock) {
        memcpy (buffer, &(info.buffer[start]), todo);
      } else {
        // Read directly into buffer if it fits exactly.
        if (todo == itsBlockSize) {
          itsIO.seek (info.blockNrs[blknr] * itsBlockSize);
          itsIO.read (itsBlockSize, buffer);
        } else {
          if (info.dirty) {
            writeDirty (info);
          }
          // Read into file buffer and copy correct part.
          itsIO.seek (info.blockNrs[blknr] * itsBlockSize);
          itsIO.read (itsBlockSize, &(info.buffer[0]));
          info.curBlock = blknr;
          memcpy (buffer, &(info.buffer[start]), todo);
        }
      }
      // Increment counters.
      done += todo;
      buffer += todo;
      blknr++;
      start = 0;
    }
    return done;
  }

  Int64 MultiFile::write (Int fileId, const void* buf, Int64 size, Int64 offset)
  {
    if (fileId >= Int(itsInfo.size())  ||  itsInfo[fileId].name.empty()) {
      throw AipsError ("MultiFile::read - invalid fileId given");
    }
    const char* buffer = static_cast<const char*>(buf);
    AlwaysAssert (itsIO.isWritable(), AipsError);
    MultiFileInfo& info = itsInfo[fileId];
    // Determine the logical block to write and the start offset in that block.
    Int64 blknr = offset/itsBlockSize;
    Int64 start = offset - blknr*itsBlockSize;
    Int64 done  = 0;
    // If beyond EOF, add blocks as needed.
    Int64 lastblk = blknr + (start+size+itsBlockSize-1) / itsBlockSize;
    Int64 curnrb = info.blockNrs.size();
    if (lastblk >= curnrb) {
      info.blockNrs.resize (lastblk);
      for (Int64 i=curnrb; i<lastblk; ++i) {
        if (itsFreeBlocks.empty()) {
          info.blockNrs[i] = itsNrBlock;
          itsNrBlock++;
        } else {
          info.blockNrs[i] = itsFreeBlocks[itsFreeBlocks.size() - 1];
          itsFreeBlocks.resize (itsFreeBlocks.size() - 1);
        }
      }
      itsChanged = True;
    }
    // Write until all done.
    while (done < size) {
      Int64 todo = std::min(size-done, itsBlockSize-start);
      // Favor sequential writing, thus write current buffer first.
      if (blknr == info.curBlock) {
        memcpy (&(info.buffer[start]), buffer, todo);
        info.dirty = True;
        if (done+todo > size) {
          writeDirty (info);
        }
      } else if (todo == itsBlockSize) {
        // Write directly from buffer if it fits exactly.
        itsIO.seek (info.blockNrs[blknr] * itsBlockSize);
        itsIO.write (itsBlockSize, buffer);
      } else {
        // Write into temporary buffer and copy correct part.
        // First write possibly dirty buffer.
        if (info.dirty) {
          writeDirty (info);
        }
        if (blknr >= curnrb) {
          memset (&(info.buffer[0]), 0, itsBlockSize);
        } else {
          itsIO.seek (info.blockNrs[blknr] * itsBlockSize);
          itsIO.read (itsBlockSize, &(info.buffer[0]));
        }
        info.curBlock = blknr;
        memcpy (&(info.buffer[start]), buffer, todo);
        info.dirty = True;
      }
      done += todo;
      buffer += todo;
      blknr++;
      start = 0;
    }
    if (offset+size > info.size) {
      info.size = offset+size;
    }
    return done;
  }


  MultiFileInfo::MultiFileInfo (Int64 bufSize)
    : curBlock (-1),
      size     (0),
      dirty    (False)
  {
    buffer.resize (bufSize);
  }


} //# NAMESPACE CASA - END
