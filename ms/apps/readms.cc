//# readms.cc : this program reads one or more MeasurementSets
//# Copyright (C) 2017
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

//# Includes

#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MSSel/MSSelection.h>
#include <casacore/tables/Tables/TableIter.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/TaQL/TableParse.h>
#include <casacore/tables/DataMan/TiledStManAccessor.h>
#include <casacore/casa/HDF5/HDF5File.h>
#include <casacore/casa/HDF5/HDF5Group.h>
#include <casacore/casa/HDF5/HDF5Record.h>
#include <casacore/casa/HDF5/HDF5DataSet.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/OS/OMP.h>

using namespace casacore;


// Define the global variables shared between the main functions.
bool   myIsHDF5;
bool   myReadWeightSpectrum;
bool   myReadData;
bool   myReadFloatData;
bool   myReadFlag;
bool   myReadRowWise;
bool   myDoSinglePart;
int    myNPart;
int    myNPol;
int    myStartChan;
int    myChanSize;
int    myNChan;
int    myCacheSizeData;
int    myCacheSizeFlag;
int    myCacheSizeWeight;
String myMsName;
String myBaselines;
String mySelection;
Vector<String> myIterCols1;
Vector<String> myIterCols2;
Vector<String> mySortCols;


void showHelp()
{
  cout << "The program reads one or more MeasurementSets" << endl;
  cout << "Run as:" << endl;
  cout << "      readms parm=value parm=value ..." << endl;
  cout << "Use   readms -h   to see the possible parameters." << endl;
}


bool readParms (int argc, char* argv[])
{
  // enable input in no-prompt mode
  Input params(1);
  // define the input structure
  params.version("2017Oct31GvD");
  params.create ("nms", "0",
                 "Number of MeasurementSets to read (0 means 1 MS without suffix)",
                 "int");
  params.create ("msname", "",
                 "Name of the MeasurementSet (suffix _p<i> is added if nms>0)",
                 "string");
  params.create ("data", "true",
                 "Read DATA or FLOAT_DATA column?",
                 "bool");
  params.create ("flag", "true",
                 "Read FLAG column?",
                 "bool");
  params.create ("weightspectrum", "false",
                 "Read WEIGHT_SPECTRUM column?",
                 "bool");
  params.create ("npol", "0",
                 "Number of polarizations to read (0=all, 1=XX, 2=XX/YY)",
                 "int");
  params.create ("startchan", "0",
                 "First channel to read",
                 "int");
  params.create ("nchan", "0",
                 "Number of channels to read (0=all)",
                 "int");
  params.create ("chansize", "0",
                 "Number of channels to read jointly; allows channel iteration (0=all)",
                 "int");
  params.create ("baselines", "",
                 "Baselines to read (CASA baseline selection)",
                 "string");
  params.create ("selection", "",
                 "TaQL selection string",
                 "string");
  params.create ("iteration1", "DATA_DESC_ID",
                 "Columns to iterate on before channel iteration",
                 "string vector");
  params.create ("iteration2", "TIME",
                 "Columns to iterate on after channel iteration",
                 "string vector");
  params.create ("sort", "",
                 "Columns to sort on (default none); . is iteration columns",
                 "string vector");
  params.create ("rowwise", "false",
                 "Read the data row wise (thus a get per row)",
                 "bool");
  params.create ("datacachesize", "0",
                 "TiledStMan cache size for DATA column (in tiles)",
                 "int");
  params.create ("flagcachesize", "0",
                 "TiledStMan cache size for FLAG column (in tiles)",
                 "int");
  params.create ("weightcachesize", "0",
                 "TiledStMan cache size for WEIGHT column (in tiles)",
                 "int");
  // Fill the input structure from the command line.
  params.readArguments (argc, argv);
  // Get the various parameters.
  myMsName = params.getString ("msname");
  if (myMsName.empty()) {
    showHelp();
    return false;
  }
  myStartChan = params.getInt ("startchan");
  myChanSize  = params.getInt ("chansize");
  myNChan = params.getInt ("nchan");
  myNPol  = params.getInt ("npol");
  myNPart = params.getInt ("nms");
  myDoSinglePart = (myNPart == 0);
  if (myDoSinglePart) {
    myNPart = 1;
  }
  // Determine nr of bands per part (i.e., ms).
  if (myNPol == 0) myNPol = 4;
  AlwaysAssert (myNPol==1  ||  myNPol==2  ||  myNPol==4, AipsError);
  // Get remaining parameters.
  myBaselines          = params.getString ("baselines");
  mySelection          = params.getString ("selection");
  myReadData           = params.getBool   ("data");
  myReadFloatData      = myReadData;
  myReadFlag           = params.getBool   ("flag");
  myReadWeightSpectrum = params.getBool   ("weightspectrum");
  myReadRowWise        = params.getBool   ("rowwise");
  myCacheSizeData      = params.getInt    ("datacachesize");
  myCacheSizeFlag      = params.getInt    ("flagcachesize");
  myCacheSizeWeight    = params.getInt    ("weightcachesize");
  myIterCols1 = stringToVector(params.getString ("iteration1"));
  myIterCols2 = stringToVector(params.getString ("iteration2"));
  mySortCols  = stringToVector(params.getString ("sort"));
  if (mySortCols.size() == 1  &&  mySortCols[0] == ".") {
    mySortCols.resize (myIterCols1.size() + myIterCols2.size());
    std::copy (myIterCols1.begin(), myIterCols1.end(), mySortCols.begin());
    Vector<String> scols(mySortCols(Slice(myIterCols1.size(),
                                          mySortCols.size() - myIterCols1.size())));
    std::copy (myIterCols2.begin(), myIterCols2.end(), scols.begin());
  }
  return true;
}

String makeMSName (int seqnr, const String& msName)
{
  String name;
  if (msName.find ("%d") != String::npos) {
    name = String::format (msName.c_str(), seqnr);
  } else {
    name = msName;
    if (!myDoSinglePart) {
      name += String::format ("_p%d", seqnr);
    }
  }
  return name;
}

void showParmsHDF5 (const IPosition& tileShape, uInt tileSize, uInt nspw,
                    const Record& attr)
{
  cout << " nms       = " << myNPart << "    " << myMsName << endl;
  cout << " nthread   = " << OMP::maxThreads() << endl;
  cout << " nant      = " << attr.asInt("NAntenna")
       << "   (" << attr.asInt("NBaseline") << " baselines)" << endl;
  cout << " nspw      = " << nspw << endl;
  cout << " nfield    = " << attr.asInt("NField");
  int ntimefield = attr.asInt("NTimeField");
  if (ntimefield > 0) {
    cout << "   (" << ntimefield << "times per field)";
  }
  cout << endl;
  cout << " nchan     = " << myNChan << endl;
  cout << " startchan = " << myStartChan << endl;
  if (myChanSize > 0) {
    cout << " chansize  = " << myChanSize << endl;
  }
  cout << " npol      = " << myNPol << endl;
  cout << " rowwise             = " << myReadRowWise << endl;
  cout << " readdata            = " << myReadData << endl;
  cout << " readfloatdata       = " << myReadFloatData << endl;
  cout << " readflag            = " << myReadFlag << endl;
  cout << " readweightspectrum  = " << myReadWeightSpectrum << endl;
  cout << " data tileshape      = " << tileShape
       << "   (tilesize = " << tileSize << " bytes)" << endl;
}

void showParms()
{
  String name = makeMSName (0, myMsName);
  if (!Table::isReadable(name)) {
    myIsHDF5 = True;
    return;
  }
  Table tab(name);
  if (! tab.tableDesc().isColumn ("DATA")) {
    myReadData = False;
  }
  if (! tab.tableDesc().isColumn ("FLOAT_DATA")) {
    myReadFloatData = False;
  }
  if (! tab.tableDesc().isColumn ("WEIGHT_SPECTRUM")) {
    myReadWeightSpectrum = False;
  }
  Block<String> parts = tab.getPartNames();
  cout << " nms       = " << myNPart << "    " << myMsName;
  if (parts.size() > 1) {
    cout << "   (MultiMS with " << parts.size() << " MSs)";
  }
  cout << endl;
  cout << " nthread   = " << OMP::maxThreads() << endl;

  // Since all parts are the same, only use the first one to determine sizes.
  MeasurementSet ms(parts[0]);
  TableIterator iter(ms, "TIME", TableIterator::Ascending,
                     TableIterator::NoSort);
  Table tab1 = iter.table();
  Int64 ntime  = ms.nrow() / tab1.nrow();
  Int64 nspw   = tableCommand("select unique DATA_DESC_ID from $1", tab1).table().nrow();
  Int64 nbl    = tableCommand("select unique ANTENNA1,ANTENNA2 from $1", tab1).table().nrow();
  Int64 nant   = ms.antenna().nrow();
  Int64 nfield = ms.field().nrow();
  Int64 ntimefield = 0;
  if (Int64(tab1.nrow()) != nbl*nspw*nfield) {
    TableIterator iterfld(ms, "FIELD_ID", TableIterator::Ascending,
                          TableIterator::NoSort);
    ntimefield = iterfld.table().nrow() / tab1.nrow();
  }
  cout << " nant      = " << nant
       << "   (" << nbl << " baselines)" << endl;
  cout << " nspw      = " << ms.spectralWindow().nrow()
       << "   (" << nspw << " per ms)" << endl;
  cout << " nfield    = " << ms.field().nrow();
  if (ntimefield > 0) {
    cout << "   (" << ntimefield << "times per field)";
  }
  cout << endl;
  cout << " ntime     = " << ntime << endl;
  IPosition dataShape;
  if (ms.tableDesc().isColumn("DATA")) {
    dataShape = ArrayColumn<Complex>(ms, "DATA").shape(0);
  } else {
    dataShape = ArrayColumn<Float>(ms, "FLOAT_DATA").shape(0);
  }
  cout << " nchan     = " << dataShape[1] << endl;
  cout << " startchan = " << myStartChan << endl;
  cout << " chansize  = " << myChanSize << endl;
  cout << " npol      = " << dataShape[0] << endl;
  cout << " rowwise             = " << myReadRowWise << endl;
  cout << " readdata            = " << myReadData << endl;
  cout << " readfloatdata       = " << myReadFloatData << endl;
  cout << " readflag            = " << myReadFlag << endl;
  cout << " readweightspectrum  = " << myReadWeightSpectrum << endl;
  try {
    ROTiledStManAccessor acc(ms, ms.tableDesc().isColumn("DATA") ? "DATA" : "FLOAT_DATA", True);
    cout << " data tileshape      = " << acc.tileShape(0)
         << "   (tilesize = " << acc.bucketSize(0) << " bytes)" << endl;
  } catch (const AipsError&) {
  }
  const StorageOption& opt = ms.storageOption();
  if (opt.option() == StorageOption::MultiFile  ||
      opt.option() == StorageOption::MultiHDF5) {
    cout << " multi"
         << (opt.option() == StorageOption::MultiFile ? "file" : "hdf5");
    cout << "    (blocksize = " << opt.blockSize() << " bytes)" << endl;
  }
  if (! myBaselines.empty()) {
    cout << "baseline selection   = " << myBaselines << endl;
  }
  if (! mySelection.empty()) {
    cout << "TaQL selection       = " << mySelection << endl;
  }
  cout << " iteration1 columns  = " << myIterCols1 << endl;
  cout << " iteration2 columns  = " << myIterCols2 << endl;
  if (mySortCols.size() > 0) {
    cout << " sort columns        = " << mySortCols << endl;
  }
  ///possibly show ntimes, etc. of selection subset
}

void readRows (ArrayColumn<Complex>& dataCol,
               ArrayColumn<float>& floatDataCol,
               ArrayColumn<Bool>& flagCol,
               ArrayColumn<float>& weightCol)
{
  if (myReadRowWise) {
    for (rownr_t row=0; row<flagCol.nrow(); ++row) {
      if (myReadData) {
        dataCol.get (row);
      }
      if (myReadFloatData) {
        floatDataCol.get (row);
      }
      if (myReadFlag) {
        flagCol.get (row);
      }
      if (myReadWeightSpectrum) {
        weightCol.get (row);
      }
    }
  } else {
    if (myReadData) {
      dataCol.getColumn();
    }
    if (myReadFloatData) {
      floatDataCol.getColumn();
    }
    if (myReadFlag) {
      flagCol.getColumn();
    }
    if (myReadWeightSpectrum) {
      weightCol.getColumn();
    }
  }
}

void readRows (ArrayColumn<Complex>& dataCol,
               ArrayColumn<float>& floatDataCol,
               ArrayColumn<Bool>& flagCol,
               ArrayColumn<float>& weightCol,
               const Slicer& slicer)
{
  if (myReadRowWise) {
    for (rownr_t row=0; row<flagCol.nrow(); ++row) {
      if (myReadData) {
        dataCol.getSlice (row, slicer);
      }
      if (myReadFloatData) {
        floatDataCol.getSlice (row, slicer);
      }
      if (myReadFlag) {
        flagCol.getSlice (row, slicer);
      }
      if (myReadWeightSpectrum) {
        weightCol.getSlice (row, slicer);
      }
    }
  } else {
    if (myReadData) {
      dataCol.getColumn (slicer);
    }
    if (myReadFloatData) {
      floatDataCol.getColumn (slicer);
    }
    if (myReadFlag) {
      flagCol.getColumn (slicer);
    }
    if (myReadWeightSpectrum) {
      weightCol.getColumn (slicer);
    }
  }
}

Int64 readNoIter (MeasurementSet& tab, Int64& niter)
{
  Array<Complex> data;
  Array<Float> floatData;
  Array<Bool> flags;
  Array<Float> weights;
  ArrayColumn<Complex> dataCol;
  if (myReadData) dataCol.attach(tab, "DATA");
  ArrayColumn<Float> floatDataCol;
  if (myReadFloatData) floatDataCol.attach(tab, "FLOAT_DATA");
  ArrayColumn<Bool> flagCol(tab, "FLAG");
  ArrayColumn<Float> weightSpectrumCol;
  if (myReadWeightSpectrum) weightSpectrumCol.attach(tab, "WEIGHT_SPECTRUM");
  const RecordInterface& attr = tab.keywordSet().asRecord ("ATTR");
  rownr_t ntoread = attr.asInt("NBaseline");
  if (attr.asInt("NTimeField") == 0) ntoread *= attr.asInt("NField");
  // Determine if to read by channel groups.
  niter = 0;
  IPosition shape = flagCol.shape(0);
  int lastchan = myStartChan + myNChan;
  if (myNChan == 0) {
    lastchan = shape[1];
  }
  int chansize = myChanSize;
  if (chansize == 0) {
    chansize = shape[1] - myStartChan;
  }
  for (int fchan=myStartChan; fchan<lastchan; fchan+=chansize) {
    int lchan = fchan + chansize;
    for (rownr_t row=0; row<tab.nrow(); row+=ntoread) {
      Slicer rowRange(IPosition(1,row),
                      IPosition(1,std::min(ntoread, tab.nrow()-row)));
      if (fchan > 0  ||  lchan < shape[1]  ||  myNPol < shape[0]) {
        AlwaysAssert (fchan < shape[1], AipsError);
        int nchan = std::min(lchan, int(shape[1])) - fchan;
        IPosition stride(2,1,1);
        int npol = myNPol;
        if (npol < shape[0]) {
          stride[0] = 3;             // achieves XX,YY if 4 pols are present
        } else {
          npol = shape[0];
        }
        Slicer slicer(IPosition(2,0,fchan),
                      IPosition(2,npol,nchan), stride);
        // Read the data per time step.
        if (myReadData) dataCol.getColumnRange (rowRange, slicer, data, True);
        if (myReadFloatData) floatDataCol.getColumnRange (rowRange, slicer, floatData, True);
        if (myReadFlag) flagCol.getColumnRange (rowRange, slicer, flags, True);
        if (myReadWeightSpectrum) weightSpectrumCol.getColumnRange (rowRange, slicer, weights, True);
        niter++;
      } else {
        if (myReadData) dataCol.getColumnRange (rowRange, data, True);
        if (myReadFloatData) floatDataCol.getColumnRange (rowRange, floatData, True);
        if (myReadFlag) flagCol.getColumnRange (rowRange, flags, True);
        if (myReadWeightSpectrum) weightSpectrumCol.getColumnRange (rowRange, weights, True);
        niter++;
      }
    }
  }
  // Do not iterate, but read the data per time step.
  for (rownr_t row=0; row<tab.nrow(); row+=ntoread) {
  }
  return tab.nrow();
}

Int64 readSteps (MeasurementSet& ms, Int64& niter)
{
  niter = 0;
  Table tab(ms);
  if (myIterCols1.empty()  &&  myIterCols2.empty()) {
    return readNoIter (ms, niter);
  }
  // Read with iteration (outer and/or inner).
  Int64 nrow = 0;
  Block<String> itercols1(myIterCols1.size());
  std::copy (myIterCols1.begin(), myIterCols1.end(), itercols1.begin());
  Block<String> itercols2(myIterCols2.size());
  std::copy (myIterCols2.begin(), myIterCols2.end(), itercols2.begin());
  TableIterator iter1(ms, itercols1, TableIterator::Ascending,
                      TableIterator::NoSort);
  while (!iter1.pastEnd()) {
    Table tab1 (iter1.table());
    IPosition shape = ArrayColumn<Bool>(tab1, "FLAG").shape(0);
    int lastchan = myStartChan + myNChan;
    if (myNChan == 0) {
      lastchan = shape[1];
    }
    int chansize = myChanSize;
    if (chansize == 0) {
      chansize = shape[1] - myStartChan;
    }
    for (int fchan=myStartChan; fchan<lastchan; fchan+=chansize) {
      TableIterator iter2(tab1, itercols2, TableIterator::Ascending,
                          TableIterator::NoSort);
      while (!iter2.pastEnd()) {
        Table tab2 (iter2.table());
        ArrayColumn<Bool> flagCol(tab2, "FLAG");
        ArrayColumn<Complex> dataCol;
        if (myReadData) {
          dataCol.attach (tab2, "DATA");
        }
        ArrayColumn<float> floatDataCol;
        if (myReadFloatData) {
          floatDataCol.attach (tab2, "FLOAT_DATA");
        }
        ArrayColumn<float> weightCol;
        if (myReadWeightSpectrum) {
          weightCol.attach (tab2, "WEIGHT_SPECTRUM");
        }
        IPosition shape = flagCol.shape(0);
        int lchan = fchan + chansize;
        if (fchan > 0  ||  lchan < shape[1]  ||  myNPol < shape[0]) {
          AlwaysAssert (fchan < shape[1], AipsError);
          int nchan = std::min(lchan, int(shape[1])) - fchan;
          IPosition stride(2,1,1);
          int npol = myNPol;
          if (npol < shape[0]) {
            stride[0] = 3;             // achieves XX,YY if 4 pols are present
          } else {
            npol = shape[0];
          }
          Slicer slicer(IPosition(2,0,fchan), IPosition(2,npol,nchan), stride);
          readRows (dataCol, floatDataCol, flagCol, weightCol, slicer);
        } else {
          readRows (dataCol, floatDataCol, flagCol, weightCol);
        }
        iter2.next();
        niter++;
      }
    }
    nrow += tab1.nrow();
    iter1.next();
  }
  return nrow;
}

void showCacheStatistics (const MeasurementSet& ms)
{
  // Ignore exceptions (because datamanagers might be called differently).
  try {
    if (myReadData) {
      cout << "DATA: ";
      RODataManAccessor(ms, "DATA", True).showCacheStatistics (cout);
    }
  } catch (std::exception&) {
  }
  try {
    if (myReadFloatData) {
      cout << "FLOAT_DATA: ";
      RODataManAccessor(ms, "FLOAT_DATA", True).showCacheStatistics (cout);
    }
  } catch (std::exception&) {
  }
  try {
    if (myReadFlag) {
      cout << "FLAG: ";
      RODataManAccessor(ms, "FLAG", True).showCacheStatistics (cout);
    }
  } catch (std::exception&) {
  }
  try {
    if (myReadWeightSpectrum) {
      cout << "WEIGHT_SPECTRUM: ";
      RODataManAccessor(ms, "WEIGHT_SPECTRUM", True).showCacheStatistics (cout);
    }
  } catch (std::exception&) {
  }
}

void setTSMCacheSize (const Table& tab, const String& columnName,
                      int cacheSize)
{
  if (cacheSize > 0) {
    ROTiledStManAccessor acc(tab, columnName, True);
    for (uInt i=0; i<acc.nhypercubes(); ++i) {
      acc.setHypercubeCacheSize (i, cacheSize);
    }
  }
}

void readRowsHDF5 (const CountedPtr<HDF5DataSet>& hflag,
                   const CountedPtr<HDF5DataSet>& hdata,
                   const CountedPtr<HDF5DataSet>& hfloatdata,
                   const CountedPtr<HDF5DataSet>& hweight,
                   Int64 row,
                   Int64 nrow)
{
  if (myReadRowWise) {
    IPosition shp(hflag->shape());
    shp[2] = 1;
    Array<Complex> data(shp);
    Array<Float> fdata(shp);
    Array<Bool> flags(shp);
    Array<Float> weights(shp);
    IPosition s(3,0);
    IPosition e(shp-1);
    Slicer slicer(s, shp);
    for (Int64 r=0; r<nrow; ++r) {
      s[2] = row+r;
      e[2] = row+r;
      slicer.setStart (s);
      slicer.setEnd (e);
      if (myReadData) {
        hdata->get(slicer, data);
      }
      if (myReadFloatData) {
        hfloatdata->get(slicer, fdata);
      }
      if (myReadFlag) {
        hflag->get(slicer, flags);
      }
      if (myReadWeightSpectrum) {
        hweight->get(slicer, weights);
      }
    }
  } else {
    IPosition shp(hflag->shape());
    shp[2] = nrow;
    Slicer slicer(IPosition(3,0,0,row), shp);
    if (myReadData) {
      Array<Complex> data(shp);
      hdata->get (slicer, data);
    }
    if (myReadFloatData) {
      Array<Float> fdata(shp);
      hfloatdata->get (slicer, fdata);
    }
    if (myReadFlag) {
      Array<Bool> flags(shp);
      hflag->get (slicer, flags);
    }
    if (myReadWeightSpectrum) {
      Array<Float> weights(shp);
      hweight->get (slicer, weights);
    }
  }
}

void readRowsHDF5 (const CountedPtr<HDF5DataSet>& hflag,
                   const CountedPtr<HDF5DataSet>& hdata,
                   const CountedPtr<HDF5DataSet>& hfloatdata,
                   const CountedPtr<HDF5DataSet>& hweight,
                   const Slicer& slicer)
{
  IPosition shp(slicer.length());
  IPosition s(slicer.start());
  IPosition e(slicer.end());
  Int64 srow = s[2];
  Int64 erow = e[2];
  if (myReadRowWise) {
    shp[2] = 1;
    Array<Complex> data(shp);
    Array<Float> fdata(shp);
    Array<Bool> flags(shp);
    Array<Float> weights(shp);
    // Make a slicer with length 1.
    s[2] = 1;
    Slicer slicer(s, shp);
    for (Int64 row=srow; row<=erow; ++row) {
      s[2] = row;
      e[2] = row;
      slicer.setStart (s);
      slicer.setEnd (e);
      if (myReadData) {
        hdata->get (slicer, data);
      }
      if (myReadFloatData) {
        hfloatdata->get (slicer, fdata);
      }
      if (myReadFlag) {
        hflag->get (slicer, flags);
      }
      if (myReadWeightSpectrum) {
        hweight->get (slicer, weights);
      }
    }
  } else {
    if (myReadData) {
      Array<Complex> data(shp);
      hdata->get (slicer, data);
    }
    if (myReadFloatData) {
      Array<Float> fdata(shp);
      hfloatdata->get (slicer, fdata);
    }
    if (myReadFlag) {
      Array<Bool> flags(shp);
      hflag->get (slicer, flags);
    }
    if (myReadWeightSpectrum) {
      Array<Float> weights(shp);
      hweight->get (slicer, weights);
    }
  }
}

Int64 readStepsHDF5 (const CountedPtr<HDF5DataSet>& hflag,
                     const CountedPtr<HDF5DataSet>& hdata,
                     const CountedPtr<HDF5DataSet>& hfloatdata,
                     const CountedPtr<HDF5DataSet>& hweightspectrum,
                     Int64 nrow,
                     Int64& niter)
{
  niter = 0;
  IPosition shape = hflag->shape();
  int lastchan = myStartChan + myNChan;
  if (myNChan == 0) {
    lastchan = shape[1];
  }
  int chansize = myChanSize;
  if (chansize == 0) {
    chansize = shape[1] - myStartChan;
  }
  for (int fchan=myStartChan; fchan<lastchan; fchan+=chansize) {
    int lchan = fchan + chansize;
    for (Int64 row=0; row<shape[2]; row+=nrow) {
      if (fchan > 0  ||  lchan < shape[1]  ||  myNPol < shape[0]) {
        AlwaysAssert (fchan < shape[1], AipsError);
        int nchan = std::min(lchan, int(shape[1])) - fchan;
        IPosition stride(3,1,1,1);
        int npol = myNPol;
        if (npol < shape[0]) {
          stride[0] = 3;             // achieves XX,YY if 4 pols are present
        } else {
          npol = shape[0];
        }
        Slicer slicer(IPosition(3,0,fchan,row),
                      IPosition(3,npol,nchan,nrow), stride);
        // Read the data per time step.
        readRowsHDF5 (hflag, hdata, hfloatdata, hweightspectrum, slicer);
      } else {
        readRowsHDF5 (hflag, hdata, hfloatdata, hweightspectrum, row, nrow);
      }
      niter++;
    }
  }
  return shape[2];
}

std::vector<Int64> doHDF5 (int seqnr, const String& name)
{
  Timer timer;
  std::vector<Int64> res(2);
  res[0] = 0;
  HDF5File hfile(name, ByteIO::Old);
  // There is a group per spectral window. Get all groups.
  std::vector<String> groupNames (HDF5Group::linkNames (hfile));
  for (uInt i=0; i<groupNames.size(); ++i) {
    HDF5Group hspw(hfile, groupNames[i]);
    // Get the attributes.
    Record attr (HDF5Record::readRecord (hspw, "ATTR"));
    CountedPtr<HDF5DataSet> hdata;
    CountedPtr<HDF5DataSet> hfloatdata;
    CountedPtr<HDF5DataSet> hflag;
    CountedPtr<HDF5DataSet> hweightspectrum;
    hflag = new HDF5DataSet (hspw, "FLAG", (Bool*)0);
    IPosition shape = hflag->shape();
    IPosition tileShape = hflag->tileShape();
    uInt tileSize = 0;
    uInt cacheSize = (shape[1] + tileShape[1] - 1) / tileShape[1];
    if (myReadFlag) {
      hflag->setCacheSize (myCacheSizeFlag==0 ? cacheSize:myCacheSizeFlag);
    }
    try {
      hdata = new HDF5DataSet (hspw, "DATA", (Complex*)0);
      tileShape = hdata->tileShape();
      if (myReadData) {
        hdata->setCacheSize (myCacheSizeData==0 ? cacheSize:myCacheSizeData);
        tileSize = tileShape.product() * sizeof(Complex);
        myReadFloatData = False;
      }
    } catch (const std::exception&) {
      myReadData = False;
    }
    if (! myReadData) {
      try {
        hfloatdata = new HDF5DataSet (hspw, "FLOAT_DATA", (Complex*)0);
        tileShape = hdata->tileShape();
        if (myReadFloatData) {
          hfloatdata->setCacheSize (myCacheSizeData==0 ? cacheSize:myCacheSizeData);
          tileSize = tileShape.product() * sizeof(Float);
        }
      } catch (const std::exception&) {
        myReadFloatData = False;
      }
    }
    if (myReadWeightSpectrum) {
      hweightspectrum = new HDF5DataSet (hspw, "WEIGHT_SPECTRUM", (float*)0);
      hweightspectrum->setCacheSize (myCacheSizeWeight==0 ? cacheSize:myCacheSizeWeight);
    }
    // Show some parms for the very first spw.
    if (seqnr == 0  &&  i == 0) {
      if (myNChan == 0) {
        myNChan = shape[1];
      }
      showParmsHDF5 (tileShape, tileSize, groupNames.size(), attr);
      cout << "HDF5 cache size = " << cacheSize << endl;
    }
    // Determine nr of rows to read per time step.
    int ntoread = attr.asInt("NBaseline");
    if (attr.asInt("NTimeField") > 0) ntoread *= attr.asInt("NField");
    Int64 niter;
    Int64 nrow = readStepsHDF5 (hflag, hdata, hfloatdata, hweightspectrum,
                                ntoread, niter);
    res[0] += nrow;
    timer.show ("Read " + String::toString(nrow) + " rows (in " +
                String::toString(niter) + " iterations) from MS " + name);
  }
  res[1] = res[0];
  return res;
}

std::vector<Int64> doOne (int seqnr, const String& msName)
{
  // Form the MS name.
  // If it contains %d, use that to fill in the seqnr.
  // Otherwise append _seqnr to the name (unless a single part is done).
  String name = makeMSName (seqnr, msName);
  if (myIsHDF5) {
    return doHDF5 (seqnr, name);
  }
  vector<Int64> res(2);
  // Open the MS.
  Timer timer;
  Table tab(name);
  // Set cache sizes where applicable.
  if (myReadData) {
    setTSMCacheSize (tab, "DATA", myCacheSizeData);
  }
  if (myReadFloatData) {
    setTSMCacheSize (tab, "FLOAT_DATA", myCacheSizeData);
  }
  if (myReadFlag) {
    setTSMCacheSize (tab, "FLAG", myCacheSizeFlag);
  }
  if (myReadWeightSpectrum) {
    setTSMCacheSize (tab, "WEIGHT_SPECTRUM", myCacheSizeWeight);
  }
  // Set the cache sizes if needed.
  res[0] = tab.nrow();
  timer.show ("Opened MS " + name);
  timer.mark();
  // Do the selection (if given).
  if (!mySelection.empty()  ||  !myBaselines.empty()) {
    MeasurementSet ms(tab);
    MSSelection mssel(ms);
    if (!mySelection.empty()) {
      mssel.setTaQLExpr (mySelection);
    }
    if (! myBaselines.empty()) {
      mssel.setAntennaExpr (myBaselines);
    }
    tab = tab(mssel.getTEN());    // do the actual selection
    timer.show ("selection");
    timer.mark();
  }
  res[1] = tab.nrow();
  if (! mySortCols.empty()) {
    Block<String> keys(mySortCols.size());
    std::copy (mySortCols.begin(), mySortCols.end(), keys.begin());
    tab = tab.sort (keys);
    timer.show ("sort    ");
    timer.mark();
  }
  MeasurementSet ms(tab);
  Int64 niter;
  Int64 nrow = readSteps (ms, niter);
  timer.show ("Read " + String::toString(nrow) + " rows (in " +
              String::toString(niter) + " iterations) from MS " + msName);
  if (seqnr == 0) {
    showCacheStatistics (ms);
  }
  return res;
}

void doAll()
{
  Int64 nrow, selNrow;
  int nthread = OMP::maxThreads();
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
  for (int i=0; i<myNPart; ++i) {
    vector<Int64> res = doOne (i, myMsName);
    if (i == 0) {
      nrow = res[0];
      selNrow = res[1];
    }
  }
  if (myNPart == 1) {
    cout << "Read 1 MS part" << endl;
  } else {
    cout << "Read " << myNPart << " MS parts in "
         << nthread << " threads" << endl;
  }
  cout << "For each part " << selNrow << " rows out of " << nrow
       << " have been read" << endl;
}

int main (int argc, char* argv[])
{
#ifdef HAVE_MPI
  MPI_Init(0,0);
#endif
  try {
    if (readParms (argc, argv)) {
      showParms();
      doAll();
    }
  } catch (const std::exception& x) {
    std::cerr << x.what() << std::endl;
    return 1;
  }
#ifdef HAVE_MPI
  MPI_Finalize();
#endif
  return 0;
}
