//# MultiHDF5.cc: Class to combine multiple files in a single HDF5 file
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

//# Includes
#include <casacore/casa/IO/MultiHDF5.h>
#include <casacore/casa/IO/MFFileIO.h>
#include <casacore/casa/HDF5/HDF5Group.h>
#include <casacore/casa/HDF5/HDF5DataSet.h>
#include <casacore/casa/HDF5/HDF5Record.h>
#include <casacore/casa/Containers/Record.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  MultiHDF5::MultiHDF5 (const String& name, ByteIO::OpenOption option,
                        Int blockSize)
    : MultiFileBase (name, blockSize, False),     //# no O_DIRECT in HDF5
      itsFile       (new HDF5File(itsName, option)),
      itsHDF5       (itsFile.get())
  {
    init (option);
  }

  MultiHDF5::MultiHDF5 (const String& name,
                        const std::shared_ptr<MultiFileBase>& parent,
                        ByteIO::OpenOption option, Int blockSize)
    // Use parent's block size if not specified.
    : MultiFileBase (name, blockSize>0 ? blockSize:parent->blockSize(), False)
  {
    // Get the overall HDF5 file object.
    MultiHDF5* parentHDF5 = dynamic_cast<MultiHDF5*>(parent.get());
    if (! parentHDF5) {
      throw AipsError("No MultiHDF5 parent given to nested MultiHDF5 constructor");
    }
    itsFile = parentHDF5->getHDF5File();
    // Create or open the file.
    // Note that creation (in doAddFile) also creates a dataset which is
    // not used, but it does not harm.
    MFFileIO file(parent, name, option);
    itsGroup = file.getInfo().group;    // make sure HDF5Group object is kept
    itsHDF5  = itsGroup.get();
    init (option);
  }

  std::shared_ptr<MultiFileBase> MultiHDF5::makeNested
  (const std::shared_ptr<MultiFileBase>& parent, const String& name,
   ByteIO::OpenOption option, Int blockSize) const
  {
    return std::make_shared<MultiHDF5>(name, parent, option, blockSize);
  }

  void MultiHDF5::init (ByteIO::OpenOption option)
  {
    if (option == ByteIO::New  ||  option == ByteIO::NewNoReplace) {
      setNewFile();
    } else {
      readHeader();
    }
    itsWritable = itsFile->isWritable();
  }

  MultiHDF5::~MultiHDF5()
  {
    close();
  }

  void MultiHDF5::doOpenFile (MultiFileInfo& info)
  {
    DebugAssert (! info.group, AipsError);
    info.group.reset (new HDF5Group (*itsHDF5, info.name, true, false));
    info.dataSet.reset (new HDF5DataSet (*info.group, "FileData",
                                         (const uChar*)0));
  }

  void MultiHDF5::doCloseFile (MultiFileInfo& info)
  {
    DebugAssert (info.group->isValid(), AipsError);
    info.dataSet.reset();
    info.group.reset();
  }

  void MultiHDF5::doFlushFile()
  {
    itsFile->flush();
  }

  void MultiHDF5::close()
  {
    flush();
    // Close all datasets and groups.
    itsInfo.clear();
    itsHDF5->close();
  }

  void MultiHDF5::reopenRW()
  {
    // Close all datasets and groups.
    itsInfo.clear();
    itsFile->reopenRW();
    readHeader (True);
    itsWritable = True;
  }
  
  void MultiHDF5::fsync()
  {}

  void MultiHDF5::writeHeader()
  {
    Record rec;
    itsHdrCounter++;
    rec.define ("blockSize", itsBlockSize);
    rec.define ("hdrCounter", itsHdrCounter);
    Vector<String> names(itsInfo.size());
    Vector<Int64> sizes(itsInfo.size());
    for (uInt i=0; i<itsInfo.size(); ++i) {
      names[i] = itsInfo[i].name;
      sizes[i] = itsInfo[i].fsize;
    }
    rec.define ("names", names);
    rec.define ("sizes", sizes);
    HDF5Record::writeRecord (*itsHDF5, "__MultiHDF5_Header__", rec);
  }

  void MultiHDF5::readHeader (Bool always)
  {
    Record rec = HDF5Record::readRecord (*itsHDF5, "__MultiHDF5_Header__");
    itsBlockSize  = rec.asInt64 ("blockSize");
    Int64 hdrCounter = rec.asInt64 ("hdrCounter");
    // Only if needed, interpret the rest of the header.
    if (hdrCounter == itsHdrCounter  &&  !always) {
      return;
    }
    itsHdrCounter = hdrCounter;
    Vector<String> names (rec.asArrayString("names"));
    Vector<Int64> sizes(rec.asArrayInt64("sizes"));
    // Set info fields.
    itsInfo.reserve (names.size());
    for (uInt i=0; i<names.size(); ++i) {
      MultiFileInfo info;
      info.name  = names[i];
      info.fsize = sizes[i];
      itsInfo.push_back (info);
    }
  }

  void MultiHDF5::doAddFile (MultiFileInfo& info)
  {
    // Create a group and dataset for the file.
    info.group.reset (new HDF5Group (*itsHDF5, info.name, false, true));
    info.dataSet.reset (new HDF5DataSet (*info.group, "FileData",
                                         IPosition(2, itsBlockSize, 0),
                                         IPosition(2, itsBlockSize, 1),
                                         (const uChar*)0));
  }

  void MultiHDF5::doDeleteFile (MultiFileInfo& info)
  {
    // Close the group and dataset.
    info.dataSet.reset();
    info.group.reset();
    // Delete the group.
    HDF5Group::remove (*itsHDF5, info.name);
  }

  void MultiHDF5::doTruncateFile (MultiFileInfo&, uInt64)
  {}
  
  void MultiHDF5::extend (MultiFileInfo& info, Int64 lastblk)
  {
    info.dataSet->extend (IPosition(2, itsBlockSize, lastblk+1));
    itsNrBlock = lastblk+1;
  }

  void MultiHDF5::readBlock (MultiFileInfo& info, Int64 blknr,
                             void* buffer)
  {
    Slicer slicer(IPosition(2, 0, blknr),
                  IPosition(2, itsBlockSize, 1));
    info.dataSet->get (slicer, buffer);
  }

  void MultiHDF5::writeBlock (MultiFileInfo& info, Int64 blknr,
                              const void* buffer)
  {
    Slicer slicer(IPosition(2, 0, blknr),
                  IPosition(2, itsBlockSize, 1));
    info.dataSet->put (slicer, buffer);
  }


} //# NAMESPACE CASACORE - END
