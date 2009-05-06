//# LofarStMan.cc: Base class of the Standard Storage Manager
//# Copyright (C) 2009
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
//# Inc., 675 Massachusettes Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <tables/Tables/LofarStMan.h>
#include <tables/Tables/LofarColumn.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/DataManError.h>
#include <casa/Containers/Record.h>
#include <casa/Containers/BlockIO.h>
#include <casa/IO/MMapIO.h>
#include <casa/IO/AipsIO.h>
#include <casa/OS/CanonicalConversion.h>
#include <casa/OS/HostInfo.h>
#include <casa/OS/DOos.h>
#include <casa/Utilities/Assert.h>
#include <casa/iostream.h>


namespace casa { //# NAMESPACE CASA - BEGIN

LofarStMan::LofarStMan (const String& dataManName)
: DataManager    (),
  itsDataManName (dataManName),
  itsFile        (0)
{}

LofarStMan::LofarStMan (const String& dataManName,
                        const Record& spec)
: DataManager    (),
  itsDataManName (dataManName),
  itsFile        (0)
{}

LofarStMan::LofarStMan (const LofarStMan& that)
: DataManager    (),
  itsDataManName (that.itsDataManName),
  itsFile        (0)
{}

LofarStMan::~LofarStMan()
{
  for (uInt i=0; i<ncolumn(); i++) {
    delete itsColumns[i];
  }
  delete itsFile;
}

DataManager* LofarStMan::clone() const
{
  return new LofarStMan (*this);
}

String LofarStMan::dataManagerType() const
{
  return "LofarStMan";
}

String LofarStMan::dataManagerName() const
{
  return itsDataManName;
}

Record LofarStMan::dataManagerSpec() const
{
  return Record();
}


DataManagerColumn* LofarStMan::makeScalarColumn (const String& name,
                                                 int dtype,
                                                 const String&)
{
  LofarColumn* col;
  if (name == "TIME"  ||  name == "TIME_CENTROID") {
    col = new TimeColumn(this, dtype);
  } else if (name == "ANTENNA1") {
    col = new Ant1Column(this, dtype);
  } else if (name == "ANTENNA2") {
    col = new Ant2Column(this, dtype);
  } else if (name == "INTERVAL"  ||  name == "EXPOSURE") {
    col = new IntervalColumn(this, dtype);
  } else if (name == "FLAG_ROW") {
    col = new FalseColumn(this, dtype);
  } else {
    col = new ZeroColumn(this, dtype);
  }
  itsColumns.push_back (col);
  return col;
}

DataManagerColumn* LofarStMan::makeDirArrColumn (const String& name,
                                                 int dataType,
                                                 const String& dataTypeId)
{
  return makeIndArrColumn (name, dataType, dataTypeId);
}

DataManagerColumn* LofarStMan::makeIndArrColumn (const String& name,
                                                 int dtype,
                                                 const String&)
{
  LofarColumn* col;
  if (name == "UVW") {
    col = new UvwColumn(this, dtype);
  } else if (name == "DATA") {
    col = new DataColumn(this, dtype);
  } else if (name == "FLAG") {
    col = new FlagColumn(this, dtype);
  } else if (name == "FLAG_CATEGORY") {
    col = new FlagCatColumn(this, dtype);
  } else if (name == "WEIGHT") {
    col = new WeightColumn(this, dtype);
  } else if (name == "SIGMA") {
    col = new SigmaColumn(this, dtype);
  } else {
    throw DataManError (name + " is unknown column for LofarStMan");
  }
  itsColumns.push_back (col);
  return col;
}

DataManager* LofarStMan::makeObject (const String& group, const Record& spec)
{
  // This function is called when reading a table back.
  return new LofarStMan (group, spec);
}

void LofarStMan::registerClass()
{
  DataManager::registerCtor ("LofarStMan", makeObject);
}

Bool LofarStMan::canAddRow() const
{
  return False;
}
Bool LofarStMan::canRemoveRow() const
{
  return False;
}
Bool LofarStMan::canAddColumn() const
{
  return False;
}
Bool LofarStMan::canRemoveColumn() const
{
  return True;
}

void LofarStMan::addRow (uInt)
{
  throw DataManError ("LofarStMan cannot add rows");
}
void LofarStMan::removeRow (uInt)
{
  throw DataManError ("LofarStMan cannot remove rows");
}
void LofarStMan::addColumn (DataManagerColumn*)
{
  throw DataManError ("LofarStMan cannot add columns");
}
void LofarStMan::removeColumn (DataManagerColumn*)
{}

Bool LofarStMan::flush (AipsIO&, Bool)
{
  return False;
}

void LofarStMan::resync (uInt nrows)
{
  itsNrRows = nrows;
}

void LofarStMan::create (uInt nrows)
{
  itsNrRows = nrows;
}

void LofarStMan::open (uInt, AipsIO&)
{
  throw DataManError ("LofarStMan::open should never be called");
}
uInt LofarStMan::open1 (uInt, AipsIO&)
{
  // Read meta info.
  init();
  // Memory-map the data file.
  if (table().isWritable()) {
    itsFile = new MMapIO (fileName() + "data", ByteIO::Update);
  } else {
    itsFile = new MMapIO (fileName() + "data");
  }
  // Set correct number of rows.
  itsNrRows = itsFile->getFileSize() / itsBlockSize * itsAnt1.size();
  return itsNrRows;
}

void LofarStMan::reopenRW()
{
  delete itsFile;
  itsFile = new MMapIO (fileName() + "data", ByteIO::Update);
}

void LofarStMan::deleteManager()
{
  delete itsFile;
  itsFile = 0;
  DOos::remove (fileName()+"meta", False, False);
  DOos::remove (fileName()+"data", False, False);
}

void LofarStMan::init()
{
  AipsIO aio(fileName() + "meta");
  uInt vers = aio.getstart ("LofarStMan");
  if (vers != 1) {
    throw DataManError ("LofarStMan can only handle version 1");
  }
  Bool asBigEndian;
  uInt maxNsample, alignment;
  aio >> itsAnt1 >> itsAnt2 >> itsStartTime >> itsTimeIntv >> itsNChan
      >> itsNPol >> maxNsample >> alignment >> asBigEndian;
  aio.getend();
  AlwaysAssert (itsAnt1.size() == itsAnt2.size(), AipsError);
  itsDoSwap = (asBigEndian != HostInfo::bigEndian());
  // A block contains an Int seqnr, Complex data per baseline,chan,pol and
  // uShort nsample per baseline,chan. Align it as needed.
  itsBLDataSize = itsNChan * itsNPol * 8;    // #bytes/baseline
  itsBlockSize  = 4 + itsAnt1.size() * (itsBLDataSize + itsNChan*2);
  if (alignment > 1) {
    itsBlockSize  = ((itsBlockSize + alignment-1) / alignment) * alignment;
  }
  if (itsDoSwap) {
    itsNSampleBuf.resize (itsNChan * 2);
  }
  itsMaxNrSample = maxNsample;
}

Double LofarStMan::time (uInt blocknr)
{
  Int seqnr;
  const void* ptr = itsFile->getReadPointer (blocknr * itsBlockSize);
  if (itsDoSwap) {
    CanonicalConversion::reverse4 (&seqnr, ptr);
  } else {
    seqnr = *static_cast<const Int*>(ptr);
  }
  return itsStartTime + seqnr*itsTimeIntv;
}

void LofarStMan::getData (uInt rownr, Complex* buf)
{
  uInt blocknr = rownr / itsAnt1.size();
  uInt baseline = rownr - blocknr*itsAnt1.size();
  uInt offset  = 4 + baseline * itsBLDataSize;
  const void* ptr = itsFile->getReadPointer (blocknr * itsBlockSize + offset);
  if (itsDoSwap) {
    const char* from = (const char*)ptr;
    char* to = (char*)buf;
    const char* fromend = from + itsBLDataSize;
    while (from < fromend) {
      CanonicalConversion::reverse4 (to, from);
      to += 4;
      from += 4;
    }
  } else {
    memcpy (buf, ptr, itsBLDataSize);
  }
}

void LofarStMan::putData (uInt rownr, const Complex* buf)
{
  uInt blocknr = rownr / itsAnt1.size();
  uInt baseline = rownr - blocknr*itsAnt1.size();
  uInt offset  = 4 + baseline * itsBLDataSize;
  void* ptr = itsFile->getWritePointer (blocknr * itsBlockSize + offset);
  if (itsDoSwap) {
    const char* from = (const char*)buf;
    char* to = (char*)ptr;
    const char* fromend = from + itsBLDataSize;
    while (from < fromend) {
      CanonicalConversion::reverse4 (to, from);
      to += 4;
      from += 4;
    }
  } else {
    memcpy (ptr, buf, itsBLDataSize);
  }
}

const uShort* LofarStMan::getNSample (uInt rownr, Bool swapIfNeeded)
{
  uInt blocknr = rownr / itsAnt1.size();
  uInt baseline = rownr - blocknr*itsAnt1.size();
  uInt offset  = 4 + itsBLDataSize*itsAnt1.size() + baseline * itsNChan*2;
  const void* ptr = itsFile->getReadPointer (blocknr * itsBlockSize + offset);
  const uShort* from = (const uShort*)ptr;
  if (!swapIfNeeded || !itsDoSwap) {
    return from;
  }
  uShort* to = itsNSampleBuf.storage();
  for (uInt i=0; i<itsNChan; ++i) {
    CanonicalConversion::reverse2 (to+i, from+i);
  }
  return to;
}

} //# NAMESPACE CASA - END
