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

//# Includes
#include <casacore/casa/IO/MultiFileBase.h>
#include <casacore/casa/IO/MultiFile.h>
#include <casacore/casa/IO/MultiHDF5.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/OS/File.h>     // for fileSTAT
#include <sys/stat.h>                  // needed for stat or stat64
#include <string.h>
#include <stdlib.h>                    // for posix_memalign

//# The alignment needed for O_DIRECT.
#define mfb_od_align (size_t(4096))


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  ostream& operator<< (ostream& ios, const MultiFileInfo& info)
    { ios << info.name << ' ' << info.blockNrs << endl
          << "  fsize=" << info.fsize
          << " cur=" << info.curBlock << " nest=" << info.nested
          << " dirty=" << info.dirty << endl;
      return ios; }
  AipsIO& operator<< (AipsIO& ios, const MultiFileInfo& info)
    { ios << info.name << info.fsize << info.nested;
      return ios; }
  AipsIO& operator>> (AipsIO& ios, MultiFileInfo& info)
    { ios >> info.name >> info.fsize >> info.nested;
      return ios; }
  void getInfoVersion1 (AipsIO& ios, std::vector<MultiFileInfo>& info)
  {
    // Get the old MultiFileInfo not containing the 'nested' field.
    // The following code is a copy of STLIO.tcc.
    ios.getstart ("Block");
    uInt nr;
    ios >> nr;
    info.resize(nr);
    for (uInt i=0; i<nr; ++i) {
      ios >> info[i].name >> info[i].blockNrs >> info[i].fsize;
    }
    ios.getend();
  }


  MultiFileBase::MultiFileBase (const String& name, Int blockSize,
                                Bool useODirect)
    : itsBlockSize  (blockSize),
      itsNrBlock    (0),
      itsHdrCounter (0),
      itsUseODirect (useODirect),
      itsWritable   (False),         // usually reset by derived class
      itsChanged    (False)
  {
    // Unset itsUseODirect if the OS does not support it.
#ifndef HAVE_O_DIRECT
    itsUseODirect = False;
#endif
    itsName = Path(name).expandedName();
  }

  std::shared_ptr<MultiFileBase> MultiFileBase::openMF (const String& fileName)
  {
    if (HDF5File::isHDF5 (fileName)) {
      return std::make_shared<MultiHDF5>(fileName, ByteIO::Old);
    }
    return std::make_shared<MultiFile>(fileName, ByteIO::Old);
  }

  void MultiFileBase::setNewFile()
  {
    // The container file is new.
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
    // Note: do not call flush() here, otherwise the virtual function
    // doFlushFile in the already destructed derived class is called
    // giving the error 'pure virtual function called'.
    itsInfo.clear();
  }

  Int MultiFileBase::openFile (const String& name)
  {
    Int id = fileId (name, True);
    if (itsInfo[id].buffer) {
      throw AipsError ("MFFileIO: logical file " + name +
                         " already opened in " + itsName);
    }
    itsInfo[id].allocBuffer (itsBlockSize, itsUseODirect);
    doOpenFile (itsInfo[id]);
    return id;
  }

  Int MultiFileBase::createFile (const String& name, ByteIO::OpenOption opt)
  {
    Int id = fileId (name, False);
    if (id >= 0) {
      if (opt == ByteIO::NewNoReplace) {
        throw AipsError ("MFFileIO: logical file " + name +
                         " already exists in " + itsName);
      }
      deleteFile (id);
    }
    id = addFile(name);
    itsInfo[id].allocBuffer (itsBlockSize, itsUseODirect);
    return id;
  }
  
  uInt MultiFileBase::nfile() const
  {
    Int nf = 0;
    for (const MultiFileInfo& info : itsInfo) {
      if (! info.name.empty()) {
        nf++;
      }
    }
    return nf;
  }

  void MultiFileBase::flush()
  {
    // Flush all buffers if needed.
    for (MultiFileInfo& info : itsInfo) {
      if (info.dirty) {
        writeDirty (info);
      }
    }
    // Header only needs to be written if blocks were added since last flush.
    // If it does not need to be written, no further flush is needed.
    if (itsChanged) {
      writeHeader();
      itsChanged = False;
    }
    doFlushFile();
  }

  void MultiFileBase::flushFile (Int fileId)
  {
    if (fileId >= Int(itsInfo.size())  ||  itsInfo[fileId].name.empty()) {
      throw AipsError ("MultiFileBase::write - invalid fileId given");
    }
    if (itsInfo[fileId].dirty) {
        writeDirty (itsInfo[fileId]);
      }
  }
  
  void MultiFileBase::closeFile (Int fileId)
  {
    // Flush the file (as needed) and delete the buffer.
    flushFile (fileId);
    itsInfo[fileId].buffer.reset();
    itsInfo[fileId].curBlock = -1;
    doCloseFile (itsInfo[fileId]);
  }
  
  Int64 MultiFileBase::read (Int fileId, void* buf,
                             Int64 size, Int64 offset)
  {
    if (fileId >= Int(itsInfo.size())  ||  itsInfo[fileId].name.empty()) {
      throw AipsError ("MultiFileBase::read - invalid fileId given");
    }
    char* buffer = static_cast<char*>(buf);
    MultiFileInfo& info = itsInfo[fileId];
    char* infoBuffer = info.buffer->data();
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
        memcpy (buffer, infoBuffer+start, todo);
      } else {
        // Read directly into buffer if it fits exactly and
        // no O_DIRECT or buffer aligned properly.
        if (todo == itsBlockSize  &&
            (!itsUseODirect  ||
             ((uintptr_t)buffer & (uintptr_t)(mfb_od_align - 1)) == 0)) {
          readBlock (info, blknr, buffer);
        } else {
          if (info.dirty) {
            writeDirty (info);
          }
          // Read into file buffer and copy correct part.
          readBlock (info, blknr, infoBuffer);
          info.curBlock = blknr;
          memcpy (buffer, infoBuffer+start, todo);
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
    char* infoBuffer = info.buffer->data();
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
        memcpy (infoBuffer+start, buffer, todo);
        info.dirty = True;
        if (done+todo > size) {
          writeDirty (info);
        }
        // Write directly from buffer if it fits exactly and
        // no O_DIRECT or buffer aligned properly.
      } else if (todo == itsBlockSize  &&
                 (!itsUseODirect  ||
                  ((uintptr_t)buffer & (uintptr_t)(mfb_od_align - 1)) == 0)) {
        writeBlock (info, blknr, buffer);
      } else {
        // Write into temporary buffer and copy correct part.
        // First write possibly dirty buffer.
        if (info.dirty) {
          writeDirty (info);
        }
        if (blknr >= curnrb) {
          memset (infoBuffer, 0, itsBlockSize);
        } else {
          readBlock (info, blknr, infoBuffer);
        }
        info.curBlock = blknr;
        memcpy (infoBuffer+start, buffer, todo);
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

  void MultiFileBase::truncate (Int fileId, Int64 size)
  {
    if (fileId >= Int(itsInfo.size())  ||  itsInfo[fileId].name.empty()) {
      throw AipsError ("MultiFileBase::truncate - invalid fileId given");
    }
    AlwaysAssert (itsWritable, AipsError);
    MultiFileInfo& info = itsInfo[fileId];
    AlwaysAssert (size >= 0  &&  size <= info.fsize, AipsError);
    // Determine nr of remaining blocks.
    size_t nrblk = (size + itsBlockSize - 1) / itsBlockSize;
    if (nrblk < info.blockNrs.size()) {
      // Clear current block if it is one of the blocks to be freed.
      for (size_t i=nrblk; i<info.blockNrs.size(); ++i) {
        if (info.curBlock == info.blockNrs[i]) {
          info.curBlock = -1;
          info.dirty    = False;
          break;
        }
      }
      // Add latter blocks to free list.
      doTruncateFile (info, nrblk);
      info.blockNrs.resize (nrblk);
    }
    // Set file size to the new size.
    info.fsize = size;
  }

  void MultiFileBase::resync()
  {
    AlwaysAssert (!itsChanged, AipsError);
    // Clear all blocknrs.
    for (MultiFileInfo& info : itsInfo) {
      AlwaysAssert (!info.dirty, AipsError);
      info.curBlock = -1;
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
    for (std::vector<MultiFileInfo>::iterator iter=itsInfo.begin();
         iter!=itsInfo.end(); ++iter, ++i) {
      if (iter->name.empty()) {
        inx = i;      // free file slot
      } else if (bname == iter->name) {
        throw AipsError ("MultiFileBase::addFile - logical file name "
                         + bname + " already exists in " + itsName);
      }
    }
    // Add a new file entry if needed.
    if (inx == itsInfo.size()) {
      itsInfo.resize (inx+1);
    }
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
      throw AipsError ("MultiFileBase::fileId - logical file name " +
                       fname + " is unknown in " + itsName);
    }
    return -1;
  }

  void MultiFileBase::deleteFile (Int fileId)
  {
    if (fileId >= Int(itsInfo.size())  ||  itsInfo[fileId].name.empty()) {
      throw AipsError ("MultiFileBase::deleteFile - invalid fileId given");
    }
    MultiFileInfo& info = itsInfo[fileId];
    info.dirty = False;     // no need to write when deleting
    closeFile (fileId);
    doDeleteFile (info);
    // Clear this slot.
    info = MultiFileInfo();
    itsChanged = True;
  }

  Int64 MultiFileBase::fileSize (Int fileId) const
  {
    if (fileId >= Int(itsInfo.size())  ||  itsInfo[fileId].name.empty()) {
      throw AipsError ("MultiFileBase::fileSize - invalid fileId given");
    }
    return itsInfo[fileId].fsize;
  }

  MultiFileInfo::MultiFileInfo()
    : curBlock (-1),
      fsize    (0),
      nested   (False),
      dirty    (False)
  {}

  
  MultiFileBuffer::MultiFileBuffer (size_t bufSize, Bool useODirect)
    : itsData (0)
  {
    const size_t align = 4096;
    if (bufSize > 0) {
      if (useODirect  &&  bufSize%align != 0) {
        throw AipsError("MultiFile bufsize " + String::toString(bufSize) +
                        " must be a multiple of " +
                        String::toString(mfb_od_align) +
                        " when using O_DIRECT");
      }
      // Note that the error messages do a malloc as well, but small
      // compared to the requested malloc, so they'll probably succeed.
      void* ptr;
      if (useODirect) {
        if (posix_memalign (&ptr, mfb_od_align, bufSize) != 0) {
          throw AllocError("MultiFileBuffer: failed to allocate aligned buffer",
                           bufSize);
        }
      } else {
        ptr = malloc (bufSize);
        if (!ptr) {
          throw AllocError("MultiFileBuffer: failed to allocate buffer", bufSize);
        }
      }
      itsData = static_cast<char*>(ptr);
    }
  }


} //# NAMESPACE CASACORE - END
