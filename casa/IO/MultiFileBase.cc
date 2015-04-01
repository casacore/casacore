//# MultiFileBase.cc: Class to combine multiple files in a single one
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
#include <casacore/casa/IO/MultiFileBase.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/OS/File.h>     // for fileFSTAT
#include <sys/stat.h>                  // needed for stat or stat64
#include <string.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  void operator<< (ostream& ios, const MultiFileInfo& info)
    { ios << info.name << ' ' << info.blockNrs << ' ' << info.fsize << ' '
          << info.curBlock << ' ' << info.dirty << endl; }
  void operator<< (AipsIO& ios, const MultiFileInfo& info)
    { ios << info.name << info.blockNrs << info.fsize; }
  void operator>> (AipsIO& ios, MultiFileInfo& info)
    { ios >> info.name >> info.blockNrs >> info.fsize; }


  MultiFileBase::MultiFileBase (const String& name, Int blockSize)
    : itsBlockSize  (blockSize),
      itsNrBlock    (0),
      itsHdrCounter (0),
      itsChanged    (False)
  {
    itsName = Path(name).expandedName();
  }

  void MultiFileBase::setNewFile()
  {
    // New file.
    itsChanged = True;
    // Use file system block size, but not less than given size.
    if (itsBlockSize <= 0) {
      struct fileSTAT sfs;
      fileSTAT (itsName.c_str(), &sfs);
      Int64 blksz = sfs.st_blksize;
      itsBlockSize = std::max (-itsBlockSize, blksz);
    }
    AlwaysAssert (itsBlockSize > 0, AipsError);
  }

  MultiFileBase::~MultiFileBase()
  {
    itsInfo.clear();
  }

  uInt MultiFileBase::nfile() const
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

  void MultiFileBase::flush()
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
    flushFile();
  }

  Int64 MultiFileBase::read (Int fileId, void* buf,
                             Int64 size, Int64 offset)
  {
    if (fileId >= Int(itsInfo.size())  ||  itsInfo[fileId].name.empty()) {
      throw AipsError ("MultiFileBase::read - invalid fileId given");
    }
    char* buffer = static_cast<char*>(buf);
    MultiFileInfo& info = itsInfo[fileId];
    // Determine the logical block to read and the start offset in that block.
    Int64 nrblk = (info.fsize + itsBlockSize - 1) / itsBlockSize;
    Int64 blknr = offset/itsBlockSize;
    Int64 start = offset - blknr*itsBlockSize;
    Int64 done  = 0;
    Int64 szdo  = std::min(size, info.fsize - offset);  // not past EOF
    // Read until done.
    while (done < szdo) {
      AlwaysAssert (blknr < nrblk, AipsError);
      Int64 todo = std::min(szdo-done, itsBlockSize-start);
      // If already in buffer, copy from there.
      if (blknr == info.curBlock) {
        memcpy (buffer, &(info.buffer[start]), todo);
      } else {
        // Read directly into buffer if it fits exactly.
        if (todo == itsBlockSize) {
          readBlock (info, blknr, buffer);
        } else {
          if (info.dirty) {
            writeDirty (info);
          }
          // Read into file buffer and copy correct part.
          readBlock (info, blknr, &(info.buffer[0]));
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

  Int64 MultiFileBase::write (Int fileId, const void* buf,
                              Int64 size, Int64 offset)
  {
    if (fileId >= Int(itsInfo.size())  ||  itsInfo[fileId].name.empty()) {
      throw AipsError ("MultiFileBase::write - invalid fileId given");
    }
    const char* buffer = static_cast<const char*>(buf);
    AlwaysAssert (itsWritable, AipsError);
    MultiFileInfo& info = itsInfo[fileId];
    // Determine the logical block to write and the start offset in that block.
    Int64 blknr = offset/itsBlockSize;
    Int64 start = offset - blknr*itsBlockSize;
    Int64 done  = 0;
    // If beyond EOF, add blocks as needed.
    Int64 lastblk = blknr + (start+size+itsBlockSize-1) / itsBlockSize;
    Int64 curnrb = (info.fsize+itsBlockSize-1) / itsBlockSize;
    if (lastblk >= curnrb) {
      extend (info, lastblk);
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
        writeBlock (info, blknr, buffer);
      } else {
        // Write into temporary buffer and copy correct part.
        // First write possibly dirty buffer.
        if (info.dirty) {
          writeDirty (info);
        }
        if (blknr >= curnrb) {
          memset (&(info.buffer[0]), 0, itsBlockSize);
        } else {
          readBlock (info, blknr, &(info.buffer[0]));
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
    if (offset+size > info.fsize) {
      info.fsize = offset+size;
    }
    return done;
  }

  void MultiFileBase::resync()
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

  Int MultiFileBase::addFile (const String& fname)
  {
    if (fname.empty()) {
      throw AipsError("MultiFileBase::addFile - empty file name given");
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
        throw AipsError ("MultiFileBase::addFile - file name " + bname +
                         " already in use");
      }
    }
    // Add a new file entry if needed.
    if (inx == itsInfo.size()) {
      itsInfo.resize (inx+1);
    }
    itsInfo[inx] = MultiFileInfo(itsBlockSize);
    itsInfo[inx].name = bname;
    doAddFile (itsInfo[inx]);
    itsChanged = True;
    return inx;
  }

  Int MultiFileBase::fileId (const String& fname, Bool throwExcp) const
  {
    // Only use the basename part (to avoid directory rename problems).
    String bname = Path(fname).baseName();
    for (size_t i=0; i<itsInfo.size(); ++i) {
      if (bname == itsInfo[i].name) {
        return i;
      }
    }
    if (throwExcp) {
      throw AipsError ("MultiFileBase::fileId - file name " + fname +
                       " is unknown");
    }
    return -1;
  }

  void MultiFileBase::deleteFile (Int fileId)
  {
    if (fileId >= Int(itsInfo.size())  ||  itsInfo[fileId].name.empty()) {
      throw AipsError ("MultiFileBase::deleteFile - invalid fileId given");
    }
    MultiFileInfo& info = itsInfo[fileId];
    doDeleteFile (info);
    // Clear this slot.
    info = MultiFileInfo();
    itsChanged = True;
  }



  MultiFileInfo::MultiFileInfo (Int64 bufSize)
    : curBlock (-1),
      fsize    (0),
      dirty    (False)
  {
    buffer.resize (bufSize);
  }


} //# NAMESPACE CASACORE - END
