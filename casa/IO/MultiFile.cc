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
#include <casa/IO/MultiFile.h>
#include <casa/IO/LargeRegularFileIO.h>
#include <casa/IO/MemoryIO.h>
#include <casa/IO/CanonicalIO.h>
#include <casa/IO/AipsIO.h>
#include <casa/BasicSL/STLIO.h>
#include <casa/OS/CanonicalConversion.h>
#include <casa/OS/File.h>           // for fileFSTAT
#include <casa/Utilities/Assert.h>
#include <casa/Exceptions/Error.h>
#include <unistd.h>
#include <sys/stat.h>               // needed for stat or stat64

namespace casa { //# NAMESPACE CASA - BEGIN

  void operator<< (AipsIO& ios, const MultiFileInfo& info)
    { ios << info.name << info.blockNrs; }
  void operator>> (AipsIO& ios, MultiFileInfo& info)
    { ios >> info.name >> info.blockNrs; }


  MultiFile::MultiFile (const String& name, ByteIO::OpenOption option, Int blockSize)
    : itsBlockSize (blockSize)
  {
    itsFD = LargeRegularFileIO::openCreate (name, option);
    itsIO.attach (itsFD, name);
    if (option == ByteIO::New  ||  option == ByteIO::NewNoReplace) {
      // New file; first block is for administration.
      itsNrBlock = 1;
      // Use file system block size, but not less than given size.
      if (blockSize <= 0) {
        struct fileSTAT sfs;
        fileFSTAT (itsFD, &sfs);
        Int blksz = sfs.st_blksize;
        blockSize = std::min (-blockSize, blksz);
      }
    } else {
      readHeader();
    }
  }

  MultiFile::~MultiFile()
  {
    close();
  }

  void MultiFile::close()
  {
    flush();
    itsInfo.clear();
    LargeFiledesIO::close (itsFD);
  }

  void MultiFile::flush()
  {
    // Flush all buffers.
    for (vector<MultiFileInfo>::iterator iter=itsInfo.begin();
         iter!=itsInfo.end(); ++iter) {
      if (iter->dirty) {
        itsIO.seek (iter->curBlock * itsBlockSize);
        itsIO.write (itsBlockSize, &(iter->buffer[0]));
        iter->dirty = False;
      }
    }
    writeHeader();
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
    cio.write (1, &next);         // possible link to subsequent header block
    cio.write (1, &next);         // reserve space for header size
    cio.write (1, &itsBlockSize);
    aio.putstart ("MultiFile", 1);
    aio << itsNrBlock << itsInfo;
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

  void MultiFile::readHeader()
  {
    // Read the first part of the header.
    vector<char> buf(3*sizeof(Int64));
    itsIO.seek (0);
    itsIO.read (sizeof(buf), &(buf[0]));
    // Extract the required info.
    Int64 next, headerSize;
    CanonicalConversion::toLocal (next, &(buf[0]));
    CanonicalConversion::toLocal (headerSize, &(buf[sizeof(next)]));
    CanonicalConversion::toLocal (itsBlockSize, &(buf[2*sizeof(next)]));
    // Now read the rest of the header.
    buf.resize (headerSize);
    if (headerSize > itsBlockSize) {
      itsIO.read (itsBlockSize - 3*sizeof(Int64), &(buf[3*sizeof(Int64)]));
      itsIO.seek (next*itsBlockSize);
      itsIO.read (headerSize - itsBlockSize, &(buf[itsBlockSize]));
    } else {
      itsIO.read (headerSize - 3*sizeof(Int64), &(buf[3*sizeof(Int64)]));
    }
    // Read all header info from the memory buffer.
    MemoryIO mio(&(buf[3*sizeof(Int64)]), headerSize - 3*sizeof(Int64));
    CanonicalIO cio(&mio);
    AipsIO aio(&cio);
    Int version = aio.getstart ("MultiFile");
    AlwaysAssert (version==1, AipsError);
    aio >> itsNrBlock >> itsInfo;
    aio.putend();
  }

  Int MultiFile::add (const String& fname)
  {
    // Check that file name is not used yet.
    for (vector<MultiFileInfo>::iterator iter=itsInfo.begin();
         iter!=itsInfo.end(); ++iter) {
      if (fname == iter->name) {
        throw AipsError ("MultiFile: file name " + fname + " already in use");
      }
    }
    // Add a new file entry.
    Int inx = itsInfo.size();
    itsInfo.push_back (MultiFileInfo());
    itsInfo[inx].buffer.resize (itsBlockSize);
    itsInfo[inx].curBlock = -1;
    itsInfo[inx].name = fname;
    return inx;
  }

  Int64 MultiFile::read (Int fileId, char* buffer, Int64 size, Int64 offset)
  {
    DebugAssert (fileId < itsInfo.size(), AipsError);
    MultiFileInfo& info = itsInfo[fileId];
    // Determine the logical block to read and the start offset in that block.
    Int64 nrblk = info.blockNrs.size();
    Int64 blknr = offset/itsBlockSize;
    Int64 start = offset - blknr*itsBlockSize;
    Int64 done  = 0;
    // Read until all done or EOF.
    while (done < size  &&  blknr < nrblk) {
      Int64 todo  = std::min(size, itsBlockSize-start);
      // If already in buffer, copy from there.
      if (blknr == info.curBlock) {
        memcpy (buffer, &(info.buffer[start]), todo);
      } else {
        // Seek to the correct physical offset.
        itsIO.seek (info.blockNrs[blknr] * itsBlockSize);
        // Read directly into buffer if it fits exactly.
        if (todo == itsBlockSize  &&  start == 0) {
          itsIO.read (itsBlockSize, buffer);
        } else {
          // Read into file buffer and copy correct part.
          itsIO.read (itsBlockSize, &(info.buffer[0]));
          info.curBlock = blknr;
          memcpy (buffer, &(info.buffer[start]), todo);
        }
        // Increment counters.
        done += todo;
        buffer += todo;
        blknr++;
        start = 0;
      }
    }
    return done;
  }

  Int64 MultiFile::write (Int fileId, const char* buffer, Int64 size, Int64 offset)
  {
    DebugAssert (fileId < itsInfo.size(), AipsError);
    MultiFileInfo& info = itsInfo[fileId];
    // Determine the logical block to write and the start offset in that block.
    Int64 blknr = offset/itsBlockSize;
    Int64 start = offset - blknr*itsBlockSize;
    Int64 done  = 0;
    // If beyond EOF, add blocks as needed.
    Int64 lastblk = blknr + (start+size+itsBlockSize-1) / itsBlockSize;
    Int64 curnrb = info.blockNrs.size();
    if (lastblk >= curnrb) {
      info.blockNrs.resize (lastblk - curnrb + 1);
      for (size_t i=curnrb; i<=info.blockNrs.size(); ++i) {
        itsNrBlock++;
        info.blockNrs[i] = itsNrBlock;
      }
    }
    // Write until all done.
    while (done < size) {
      Int64 todo = std::min(size, itsBlockSize-start);
      // If already in buffer, copy to there.
      if (blknr == info.curBlock) {
        memcpy (&(info.buffer[start]), buffer, todo);
        info.dirty = True;
      } else {
        // Favor sequential writes, thus write current buffer first.
        if (blknr == info.curBlock) {
          memcpy (&(info.buffer[start]), buffer, todo);
          info.dirty = True;
          if (done+todo >= size) {
            itsIO.seek (info.curBlock * itsBlockSize);
            itsIO.write (itsBlockSize, &(info.buffer[0]));
            info.dirty = False;
            info.curBlock = -1;
          }
        } else if (todo == itsBlockSize  &&  start == 0) {
          // Write directly from buffer if it fits exactly.
          itsIO.seek (info.blockNrs[blknr] * itsBlockSize);
          itsIO.write (itsBlockSize, buffer);
        } else {
          // Write into temporary buffer and copy correct part.
          // First write possibly dirty buffer.
          if (info.dirty) {
            itsIO.seek (info.curBlock * itsBlockSize);
            itsIO.write (itsBlockSize, &(info.buffer[0]));
          }
          itsIO.seek (info.blockNrs[blknr] * itsBlockSize);
          itsIO.read (itsBlockSize, &(info.buffer[0]));
          info.curBlock = blknr;
          memcpy (&(info.buffer[start]), buffer, todo);
          info.dirty = True;
        }
        done += todo;
        buffer += todo;
        blknr++;
        start = 0;
      }
    }
    return done;
  }

} //# NAMESPACE CASA - END
