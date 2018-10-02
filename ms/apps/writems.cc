//# writems.cc: Create one or MeasurementSets
//# Copyright (C) 2017
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
//# $Id$

#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/Quanta/MVAngle.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/TaQL/RecordGram.h>

#include <casacore/ms/MeasurementSets.h>
#include <casacore/tables/DataMan/IncrementalStMan.h>
#include <casacore/tables/DataMan/StandardStMan.h>
#include <casacore/tables/DataMan/TiledColumnStMan.h>
#include <casacore/tables/DataMan/TiledShapeStMan.h>
#include <casacore/tables/DataMan/BitFlagsEngine.h>
#include <casacore/tables/DataMan/DataManAccessor.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableCopy.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MBaseline.h>
#include <casacore/measures/Measures/Muvw.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/measures/Measures/Stokes.h>
#include <casacore/measures/Measures/MCBaseline.h>
#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Quanta/MVEpoch.h>
#include <casacore/casa/Quanta/MVDirection.h>
#include <casacore/casa/Quanta/MVPosition.h>
#include <casacore/casa/Quanta/MVBaseline.h>
#include <casacore/casa/OS/Time.h>
#include <casacore/casa/OS/OMP.h>
#include <casacore/casa/Utilities/CountedPtr.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

#ifdef HAVE_MPI
#ifdef HAVE_ADIOS2
#include <casacore/tables/DataMan/Adios2StMan.h>
#endif
#endif

#include <casacore/casa/HDF5/HDF5File.h>
#include <casacore/casa/HDF5/HDF5Group.h>
#include <casacore/casa/HDF5/HDF5Record.h>
#include <casacore/casa/HDF5/HDF5DataType.h>
#include <casacore/casa/HDF5/HDF5DataSet.h>

#include <iostream>
#include <fstream>
#include <sstream>

using namespace casacore;
using namespace std;

// This struct contains the data items needed to fill a spectral window in
// the main table in HDF5.
struct HDF5Spw {
  CountedPtr<HDF5Group> spw;
  CountedPtr<HDF5DataSet> data;
  CountedPtr<HDF5DataSet> floatData;
  CountedPtr<HDF5DataSet> modelData;
  CountedPtr<HDF5DataSet> corrData;
  CountedPtr<HDF5DataSet> flag;
  CountedPtr<HDF5DataSet> weightSpectrum;
  CountedPtr<HDF5DataSet> metaData;
};

struct HDF5MetaData
{
  double time;
  double timeCentroid;
  double interval;
  double exposure;
  double uvw[3];
  float weight[4];
  float sigma[4];
  Int antenna1;
  Int antenna2;
  Int arrayId;
  Int fieldId;
  Int dataDescId;
  Int stateId;
  Int flagRow;
  Int feed1;
  Int feed2;
  Int processorId;
  Int scanNumber;
  Int observationId;

  HDF5MetaData()
    : time(0),
      timeCentroid(0),
      interval(0),
      exposure(0),
      //      uvw ({0,0,0}),   // only possible in C++11
      //weight ({1,1,1,1}),
      //sigma ({1,1,1,1}),
      antenna1(0),
      antenna2(0),
      arrayId(0),
      fieldId(0),
      dataDescId(0),
      stateId(0),
      flagRow(False),
      feed1(0),
      feed2(0),
      processorId(0),
      scanNumber(0),
      observationId(0)
  {}
};

// Define the global variables shared between the main functions.
vector<double> myRa;
vector<double> myDec;
Matrix<double> myAntPos;
bool   myCalcUVW;
bool   myWriteAutoCorr;
bool   myWriteFloatData;
bool   myWriteWeightSpectrum;
bool   myCreateImagerColumns;
bool   myWriteRowWise;
bool   myDoSinglePart;
int    myNPart;
int    myTotalNBand;
int    myFirstBand;
int    myNBand;
Vector<int> myNPol;
Vector<int> myNChan;
int    myNTime;
int    myNTimeField;
int    myTileSizePol;
int    myTileSizeFreq;
int    myTileSize;     //# in bytes
int    myNFlagBits;
Vector<double> myStartFreq;
Vector<double> myStepFreq;
double myStartTime;
double myStepTime;
String myMsName;
String myAntennaTableName;
String myFlagColumn;
int    myUseMultiFile;      //# 0=not 1=multifile 2=multihdf5
int    myMultiBlockSize;    //# in bytes
bool   myWriteHDF5;
bool   myUseAdios2;


// <synopsis>
// Class for creating and filling one or more MeasurementSets.
// The number of time slots, baselines, spectral windows, channels and fields
// can be given. It can be specified if the WEIGHT_SPECTRUM column has to be
// created. The data, flags and weights are set to zero, false and 1.
// All required meta info (like UVW) are filled in.
// Simulator software (like BBS or MeqTrees) can be used to write actual data.
// </synopsis>
class MSCreate
{
public:
  MSCreate();

  // Initialize and construct the MS with a given name.
  // The timeStep (in sec) is used by the write function
  // to calculate the time from the starting time and the timeCounter.
  // If the antenna table name is not empty, the ANTENNA table will be
  // copied instead of putting default values in that table.
  // The antenna positions have to be given in ITRF coordinates as XYZ.
  // So antPos must have shape [3,nantennas].
  // If flagColumn is given and nFlagBits>0, an integer flag column is
  // created and column FLAG is mapped to it
  void init (const vector<double>& ra,
             const vector<double>& dec,
             const Matrix<double>& antPos,
             bool  calcUVW,
             bool  writeAutoCorr,
             bool  writeFloatData,
             bool  writeWeightSpectrum,
             bool  createImagerColumns,
             const Vector<int>& npol,
             const Vector<int>& nfreq,
             const Vector<double>& startFreq,
             const Vector<double>& stepFreq,
             int spw, int nspw, int ntimeField,
             double startTime,
             double stepTime,
             const String& msName,
             const String& antennaTableName,
             int   nflagBits,
             const String& flagColumn,
             const IPosition& dataTileShape,
             int   useMultiFile,      //# 0=not 1=multifile 2=multihdf5
             int   multiBlockSize);

  // Destructor
  virtual ~MSCreate();

  // Fill the various subtables.
  // <group>
  virtual void fillAntenna (const Block<MPosition>& antMPos,
                            const String& antennaTableName) = 0;
  virtual void fillSpwPol() = 0;
  virtual void fillField() = 0;
  virtual void fillFeed() = 0;
  virtual void fillObservation() = 0;
  virtual void fillProcessor() = 0;
  virtual void fillState() = 0;
  // </group>

  // Close all subtables to limit the nr of open files.
  // This can be done once all subtables have been written.
  virtual void closeSubTables() = 0;

  // Write all rows for a single time step.
  // It sets the shape of the data array.
  // All flags are set to False.
  void writeTimeStep (int ntimeField, bool perRow);

  // Write a spectral window row by row.
  virtual void writeTimeStepRows (int band, int field,
                                  const vector<Vector<Double> >& antuvw) = 0;

  // Write a spectral window as a block.
  virtual void writeTimeStepSpw (int band, int field,
                                 const vector<Vector<Double> >& antuvw) = 0;

  // Extend the MS with the given nr of rows.
  virtual void addRows (int nbasel, int nfield) = 0;

  // Flush and fsync the MS.
  virtual void flush() = 0;

  // Return the nr of rows in the MS.
  virtual Int64 nrow() const = 0;

  // Show the cache statistics.
  virtual void showCacheStatistics() const = 0;

  // Get the number of baselines.
  int nbaselines() const;

private:
  // Forbid copy constructor and assignment by making them private.
  // <group>
  MSCreate (const MSCreate&);
  MSCreate& operator= (const MSCreate&);
  // </group>

  // Create the MS and fill its subtables as much as possible.
  virtual void createMS (const String& msName, int ntimeField,
                         int useMultiFile, int multiBlockSize,
                         bool createImagerColumns,
                         const String& flagColumn, int flagBits) = 0;

  // Add a polarization to the subtable.
  // Return the row number where it is added.
  int addPolarization (int npolarizations);

  // Fill the vector of baselines itsAntBL.
  void fillBaseLines (const Matrix<double>& antPos);

  // Update the times in various subtables at the end of the observation.
  virtual void updateTimes() = 0;

protected:
  //# Define the data.
  int            itsNrTimes;      //# Nr of time slots written
  vector<double> itsRa;
  vector<double> itsDec;
  int  itsNrAnt;                  //# Nr of antennae
  bool itsCalcUVW;                //# calculate UVW coordinates?
  bool itsWriteAutoCorr;          //# write autocorrelations?
  bool itsWriteFloatData;         //# write floatdata and only autocorr?
  bool itsWriteWeightSpectrum;
  Vector<Int> itsNFreq;           //# nr of freq channels for each band
  Vector<int> itsNPol;            //# nr of polarizations for each band
  Vector<double> itsStartFreq;
  Vector<double> itsStepFreq;
  int    itsSpw;
  int    itsNSpw;
  double itsStartTime;            //# start time of observation (sec)
  double itsStepTime;             //# duration of each exposure (sec)
  String itsMsName;
  IPosition itsDataTileShape;
  vector<Int> itsPolnr;           //# rownr in POL subtable for each band
  Block<MBaseline> itsAntBL;      //# Baseline vector for each antenna
  MPosition       itsArrayPos;    //# Position of array center
  MeasFrame       itsFrame;       //# Frame to convert to apparent coordinates
  Block<MDirection> itsPhaseDir;  //# Phase directions of fields
};



class MSCreateCasa: public MSCreate
{
public:
  MSCreateCasa();

  // Destructor
  virtual ~MSCreateCasa();

  // Write a spectral window row by row.
  virtual void writeTimeStepRows (int band, int field,
                                  const vector<Vector<Double> >& antuvw);

  // Write a spectral window as a block.
  virtual void writeTimeStepSpw (int band, int field,
                                 const vector<Vector<Double> >& antuvw);

  // Extend the MS with the given nr of rows.
  virtual void addRows (int nbasel, int nfield);

  // Flush and fsync the MS.
  virtual void flush()
    { itsMS.flush(True); }

  // Return the nr of rows in the MS.
  virtual Int64 nrow() const
    { return itsMS.nrow(); }

  // Show the cache statistics.
  virtual void showCacheStatistics() const;

private:
  // Forbid copy constructor and assignment by making them private.
  // <group>
  MSCreateCasa (const MSCreateCasa&);
  MSCreateCasa& operator= (const MSCreateCasa&);
  // </group>

  // Create the MS and fill its subtables as much as possible.
  virtual void createMS (const String& msName, int ntimeField,
                         int useMultiFile, int multiBlockSize,
                         bool createImagerColumns,
                         const String& flagColumn, int flagBits);

  // Close all subtables to limit the nr of open files.
  // This can be done once all subtables have been written.
  virtual void closeSubTables();

  // Write the never changing columns only once.
  void writeSimpleMainColumns();

  // Update the times in various subtables at the end of the observation.
  virtual void updateTimes();

  // Add a band to the SPW subtable.
  void addBand (int band, int npolarizations, int nchannels,
                double startFreq, double chanWidth);

  // Add a field to the FIELD subtable.
  void addField (int field);

  // Add a polarization to the subtable.
  // Return the row number where it is added.
  int addPolarization (int npolarizations);

  // Add the extra columns needed for the imager for every column not existing.
  // These are CORRECTED_DATA and MODEL_DATA.
  // Furthermore it sets the CHANNEL_SELECTION keyword for VisSet.
  void addImagerColumns();

  // Fill the various subtables.
  // <group>
  virtual void fillAntenna (const Block<MPosition>& antMPos,
                            const String& antennaTableName);
  virtual void fillSpwPol();
  virtual void fillField();
  virtual void fillFeed();
  virtual void fillObservation();
  virtual void fillProcessor();
  virtual void fillState();
  // </group>

  //# Define the data.
  Int64           itsNrRow;
  MeasurementSet  itsMS;
  MSMainColumns*  itsMSCol;
};



class MSCreateHDF5: public MSCreate
{
public:
  MSCreateHDF5();

  // Destructor
  virtual ~MSCreateHDF5();

  // Write a spectral window row by row.
  virtual void writeTimeStepRows (int band, int field,
                                  const vector<Vector<Double> >& antuvw);

  // Write a spectral window as a block.
  virtual void writeTimeStepSpw (int band, int field,
                                 const vector<Vector<Double> >& antuvw);

  // Extend the MS with the given nr of rows.
  virtual void addRows (int nbasel, int nfield);

  // Flush and fsync the MS.
  virtual void flush();

  // Return the nr of rows in the MS.
  virtual Int64 nrow() const;

  // Show the cache statistics.
  virtual void showCacheStatistics() const;

private:
  // Forbid copy constructor and assignment by making them private.
  // <group>
  MSCreateHDF5 (const MSCreateHDF5&);
  MSCreateHDF5& operator= (const MSCreateHDF5&);
  // </group>

  // Create the MS and fill its subtables as much as possible.
  virtual void createMS (const String& msName, int ntimeField,
                         int useMultiFile, int multiBlockSize,
                         bool createImagerColumns,
                         const String& flagColumn, int flagBits);

  // Close all subtables to limit the nr of open files.
  // This can be done once all subtables have been written.
  virtual void closeSubTables();

  // Update the times in various subtables at the end of the observation.
  virtual void updateTimes();

  // Add a polarization to the subtable.
  // Return the row number where it is added.
  int addPolarization (int npolarizations);

  // Fill the various subtables.
  // <group>
  virtual void fillAntenna (const Block<MPosition>& antMPos,
                            const String& antennaTableName);
  virtual void fillSpwPol();
  virtual void fillField();
  virtual void fillFeed();
  virtual void fillObservation();
  virtual void fillProcessor();
  virtual void fillState();
  // </group>

  // Create the HDF5 meta data type.
  void makeMetaType();

  //# Define the data.
  Int64                itsNrRow;
  HDF5DataType         itsMetaType;
  CountedPtr<HDF5File> itsFile;
  vector<HDF5Spw>      itsSpws;
};



//# Implementation of the classes.

MSCreate::MSCreate()
  : itsNrTimes (0)
{}

MSCreate::~MSCreate()
{}

void MSCreate::init (const vector<double>& ra,
                     const vector<double>& dec,
                     const Matrix<double>& antPos,
                     bool  calcUVW,
                     bool  writeAutoCorr,
                     bool  writeFloatData,
                     bool  writeWeightSpectrum,
                     bool  createImagerColumns,
                     const Vector<int>& npol,
                     const Vector<int>& nfreq,
                     const Vector<double>& startFreq,
                     const Vector<double>& stepFreq,
                     int spw, int nspw, int ntimeField,
                     double startTime,
                     double stepTime,
                     const String& msName,
                     const String& antennaTableName,
                     int   nflagBits,
                     const String& flagColumn,
                     const IPosition& dataTileShape,
                     int   useMultiFile,
                     int   multiBlockSize)
{
  itsRa = ra;
  itsDec = dec;
  itsCalcUVW        = calcUVW;
  itsWriteAutoCorr  = writeAutoCorr;
  itsWriteFloatData = writeFloatData;
  itsWriteWeightSpectrum = writeWeightSpectrum;
  itsNPol      = npol;
  itsNFreq     = nfreq;
  itsStartFreq = startFreq;
  itsStepFreq  = stepFreq;
  itsSpw       = spw;
  itsNSpw      = nspw;
  itsStartTime = startTime;
  itsStepTime  = stepTime;
  itsDataTileShape = dataTileShape;

  itsNrAnt     = antPos.ncolumn();
  AlwaysAssert (itsNrAnt > 0, AipsError);
  AlwaysAssert (itsNFreq.size() > 0, AipsError);
  AlwaysAssert (itsNPol.size() == itsNFreq.size(), AipsError);
  AlwaysAssert (itsStartFreq.size() == itsNFreq.size(), AipsError);
  AlwaysAssert (itsStepFreq.size() == itsNFreq.size(), AipsError);
  // Keep the antenna positions in ITRF coordinates.
  Block<MPosition> antMPos(itsNrAnt);
  for (Int i=0; i<itsNrAnt; i++) {
    antMPos[i] = MPosition (MVPosition(antPos(0,i), antPos(1,i), antPos(2,i)),
                            MPosition::ITRF);
  }
  // Use the first antenna as the array position.
  // Setup the frame for the UVW calculations.
  itsArrayPos = antMPos[0];
  itsFrame = MeasFrame(itsArrayPos);
  itsPhaseDir.resize (itsRa.size());
  for (uInt i=0; i<itsRa.size(); ++i) {
    MVDirection radec (Quantity(itsRa[i],"rad"), Quantity(itsDec[i],"rad"));
    itsPhaseDir[i] = MDirection(radec, MDirection::J2000);
  }
  // Create the MS.
  createMS (msName, ntimeField, useMultiFile, multiBlockSize,
            createImagerColumns, flagColumn, nflagBits);
  // Fill the baseline vector for each antenna pair.
  fillBaseLines (antPos);
  // Fill various subtables.
  fillAntenna (antMPos, antennaTableName);
  fillSpwPol();
  fillField();
  fillFeed();
  fillObservation();
  fillProcessor();
  fillState();
}

int MSCreate::nbaselines() const
{
  int nrbasel = itsNrAnt*(itsNrAnt-1)/2;
  if (itsWriteFloatData) {
    nrbasel = itsNrAnt;             // only autocorr
  } else if (itsWriteAutoCorr) {
    nrbasel += itsNrAnt;            // crosscorr and autocorr
  }
  return nrbasel;
}

void MSCreate::writeTimeStep (int ntimeField, bool rowWise)
{
  int nrbasel = nbaselines();
  int nrfield = itsRa.size();
  // Extend for the number of fields, spectral windows and baselines.
  if (ntimeField <= 0) {
    // All fields for all times.
    addRows (nrbasel, nrfield);
  } else {
    addRows (nrbasel, 1);
  }
  // Write each field.
  // Calculate the UVW for all stations.
  // First store time in frame.
  Double time = itsStartTime + itsNrTimes*itsStepTime + itsStepTime/2;
  Quantity qtime(time, "s");
  itsFrame.set (MEpoch(qtime, MEpoch::UTC));
  for (int field=0; field<nrfield; ++field) {
    if (ntimeField <= 0  ||  field == (itsNrTimes/ntimeField)%nrfield) {
      itsFrame.set (itsPhaseDir[field]);
      vector<Vector<Double> > antuvw(itsNrAnt);
      if (itsCalcUVW) {
        for (int j=0; j<itsNrAnt; ++j) {
          MBaseline& mbl = itsAntBL[j];
          mbl.getRefPtr()->set(itsFrame);      // attach frame
          MBaseline::Convert mcvt(mbl, MBaseline::J2000);
          MVBaseline bas = mcvt().getValue();
          MVuvw jvguvw(bas, itsPhaseDir[field].getValue());
          antuvw[j] = Muvw(jvguvw, Muvw::J2000).getValue().getVector();
        }
      }
      // Write each spectral window.
      for (int band=itsSpw; band<itsSpw+itsNSpw; ++band) {
        if (rowWise) {
          writeTimeStepRows (band, field, antuvw);
        } else {
          writeTimeStepSpw (band, field, antuvw);
        }
      }
    }
  }
  itsNrTimes++;
}

void MSCreate::fillBaseLines (const Matrix<double>& antPos)
{
  uInt nr = antPos.ncolumn();
  itsAntBL.resize (nr);
  for (uInt j=0; j<nr; j++) {
    MVPosition blpos(antPos(0,j), antPos(1,j), antPos(2,j));
    itsAntBL[j] = MBaseline (MVBaseline(blpos), MBaseline::ITRF);
  }
}



MSCreateCasa::MSCreateCasa()
  : itsNrRow (0),
    itsMSCol (0)
{}

MSCreateCasa::~MSCreateCasa()
{
  if (! itsMS.isNull()) {
    updateTimes();
  }
  delete itsMSCol;
}


void MSCreateCasa::createMS (const String& msName, int ntimeField,
                             int useMultiFile, int multiBlockSize,
                             bool createImagerColumns,
                             const String& flagColumn, int nflagBits)
{
  // Create an integer flag column?
  if (flagColumn.empty()) {
    nflagBits = 0;
  }
  // Get the MS main default table description.
  TableDesc td = MS::requiredTableDesc();
  // Add the data or floatdata column and its unit.
  if (itsWriteFloatData) {
    MS::addColumnToDesc(td, MS::FLOAT_DATA, 2);
  } else {
    MS::addColumnToDesc(td, MS::DATA, 2);
    td.rwColumnDesc(MS::columnName(MS::DATA)).rwKeywordSet().
      define("UNIT","Jy");
  }
  // Store the data and flags in two separate files.
  // TiledColumnStMan is used if a single band is given, otherwise
  // TiledShapeStMan.
  IPosition dataShape(2, itsNPol[itsSpw], itsNFreq[itsSpw]);
  if (itsNSpw == 1) {
    if (itsWriteFloatData) {
      td.rwColumnDesc(MS::columnName(MS::FLOAT_DATA)).setShape (dataShape);
    } else {
      td.rwColumnDesc(MS::columnName(MS::DATA)).setShape (dataShape);
    }
    td.rwColumnDesc(MS::columnName(MS::FLAG)).setShape (dataShape);
  }
  if (nflagBits > 1) {
    if (nflagBits == 8) {
      td.addColumn(ArrayColumnDesc<uChar>(flagColumn, 2));
    } else if (nflagBits == 16) {
      td.addColumn(ArrayColumnDesc<Short>(flagColumn, 2));
    } else {
      td.addColumn(ArrayColumnDesc<Int>(flagColumn, 2));
    }
    if (itsNSpw == 1) {
      td.rwColumnDesc(flagColumn).setShape (dataShape);
    }
  }
  // Create WEIGHT_SPECTRUM if needed.
  if (itsWriteWeightSpectrum) {
    td.addColumn (ArrayColumnDesc<float>(MS::columnName(MS::WEIGHT_SPECTRUM), 2));
    if (itsNSpw == 1) {
      td.rwColumnDesc(MS::columnName(MS::WEIGHT_SPECTRUM)).setShape (dataShape);
    }
  }
  // Add imager columns if needed.
  if (createImagerColumns) {
    addImagerColumns();
  }
  // Set the reference frame of UVW to J2000.
  {
    ColumnDesc& col(td.rwColumnDesc("UVW"));
    TableRecord rec = col.keywordSet().asRecord ("MEASINFO");
    rec.define ("Ref", "J2000");
    col.rwKeywordSet().defineRecord ("MEASINFO", rec);
  }
  int ts = itsDataTileShape.product()*8;   // tilesize
  // Setup the new table.
  // Use the required StorageOption.
  // Most columns use the IncrStMan; some use others.
  StorageOption stopt (StorageOption::SepFile);
  if (useMultiFile == 1) {
    stopt = StorageOption (StorageOption::MultiFile, multiBlockSize);
  } else if (useMultiFile == 2) {
    stopt = StorageOption (StorageOption::MultiHDF5, multiBlockSize);
  }
  SetupNewTable newTab(msName, td, Table::New, stopt);
  if(myUseAdios2){
#ifdef HAVE_ADIOS2
    Adios2StMan a2man;
    newTab.bindAll (a2man);
#else
    throw(std::runtime_error("ADIOS 2 is not built."));
#endif
  }
  else{
    IncrementalStMan incrStMan("ISMData", ts);
    newTab.bindAll (incrStMan);
    StandardStMan stanStMan("SSMData", ts);
    newTab.bindColumn(MS::columnName(MS::TIME), stanStMan);
    newTab.bindColumn(MS::columnName(MS::TIME_CENTROID), stanStMan);
    newTab.bindColumn(MS::columnName(MS::ANTENNA1), stanStMan);
    newTab.bindColumn(MS::columnName(MS::ANTENNA2), stanStMan);
    newTab.bindColumn(MS::columnName(MS::DATA_DESC_ID), stanStMan);
    newTab.bindColumn(MS::columnName(MS::FIELD_ID), stanStMan);
    newTab.bindColumn(MS::columnName(MS::UVW), stanStMan);
    // Use a TiledColumnStMan or TiledShapeStMan for the data and flags.
    if (itsNSpw == 1) {
      TiledColumnStMan tiledData("TiledData", itsDataTileShape);
      if (itsWriteFloatData) {
        newTab.bindColumn(MS::columnName(MS::FLOAT_DATA), tiledData);
      } else {
        newTab.bindColumn(MS::columnName(MS::DATA), tiledData);
      }
    } else {
      TiledShapeStMan tiledData("TiledData", itsDataTileShape);
      if (itsWriteFloatData) {
        newTab.bindColumn(MS::columnName(MS::FLOAT_DATA), tiledData);
      } else {
        newTab.bindColumn(MS::columnName(MS::DATA), tiledData);
      }
    }
    // Create the FLAG column.
    // Only needed if bit flags engine is not used.
    String dmName = "TiledFlag";
    if (nflagBits <= 1) {
      IPosition tileShape(itsDataTileShape);
      tileShape[2] *= 8;
      if (itsNSpw == 1) {
        TiledColumnStMan tiledFlag(dmName, tileShape);
        newTab.bindColumn(MS::columnName(MS::FLAG), tiledFlag);
      } else {
        TiledShapeStMan tiledFlag(dmName, tileShape);
        newTab.bindColumn(MS::columnName(MS::FLAG), tiledFlag);
      }
      dmName = "TiledFlagBits";
    } else {
      // Create the flag bits column.
      if (itsNSpw == 1) {
        TiledColumnStMan tiledFlagBits(dmName, itsDataTileShape);
        newTab.bindColumn(flagColumn, tiledFlagBits);
      } else {
        TiledShapeStMan tiledFlagBits(dmName, itsDataTileShape);
        newTab.bindColumn(flagColumn, tiledFlagBits);
      }
      if (nflagBits > 1) {
        // Map the flag bits column to the FLAG column.
        if (nflagBits == 8) {
          BitFlagsEngine<uChar> fbe(MS::columnName(MS::FLAG), flagColumn);
          newTab.bindColumn(MS::columnName(MS::FLAG), fbe);
        } else if (nflagBits == 16) {
          BitFlagsEngine<Short> fbe(MS::columnName(MS::FLAG), flagColumn);
          newTab.bindColumn(MS::columnName(MS::FLAG), fbe);
        } else {
          BitFlagsEngine<Int> fbe(MS::columnName(MS::FLAG), flagColumn);
          newTab.bindColumn(MS::columnName(MS::FLAG), fbe);
        }
      }
    }
    if (itsWriteWeightSpectrum) {
      if (itsNSpw == 1) {
        TiledColumnStMan tiledWSpec("TiledWeightSpectrum", itsDataTileShape);
        newTab.bindColumn(MS::columnName(MS::WEIGHT_SPECTRUM), tiledWSpec);
      } else {
        TiledShapeStMan tiledWSpec("TiledWeightSpectrum", itsDataTileShape);
        newTab.bindColumn(MS::columnName(MS::WEIGHT_SPECTRUM), tiledWSpec);
      }
    }
  }

  // Create the MS and its subtables.
  // Get access to its columns.
  itsMS = MeasurementSet(newTab);
  itsMSCol = new MSMainColumns(itsMS);
  // Create all subtables.
  // Do this after the creation of optional subtables,
  // so the MS will know about those optional sutables.
  itsMS.createDefaultSubtables (Table::New);
  // Store some attributes defining nr of baselines and fields.
  Record rec;
  rec.define ("NAntenna", itsNrAnt);
  rec.define ("NBaseline", nbaselines());
  rec.define ("NField", int(itsRa.size()));
  rec.define ("NTimeField", ntimeField);
  itsMS.rwKeywordSet().defineRecord ("ATTR", rec);
}

void MSCreateCasa::closeSubTables()
{
  // Don't do this yet.
  // It requires at least that the throw is removed from NullTable::flush
  // or that MS::flush tests if a subtable is null.
  // Furthermore, probably the Tables in the TableKeyword objects have to
  // be set to an empty Table object too to really close the subtables.
  // That could be checked by activating TableTrace in the .casarc file.
  return;
  itsMS.antenna() = MSAntenna();
  itsMS.dataDescription() = MSDataDescription();
  itsMS.doppler() = MSDoppler();
  itsMS.feed() = MSFeed();
  itsMS.field() = MSField();
  itsMS.flagCmd() = MSFlagCmd();
  itsMS.freqOffset() = MSFreqOffset();
  itsMS.history() = MSHistory();
  itsMS.observation() = MSObservation();
  itsMS.pointing() = MSPointing();
  itsMS.polarization() = MSPolarization();
  itsMS.processor() = MSProcessor();
  itsMS.source() = MSSource();
  itsMS.spectralWindow() = MSSpectralWindow();
  itsMS.state() = MSState();
  itsMS.sysCal() = MSSysCal();
  itsMS.weather() = MSWeather();
  itsMS.keywordSet().closeTables();
}

void MSCreateCasa::fillAntenna (const Block<MPosition>& antMPos,
                                const String& antennaTableName)
{
  // If a column is contained in the input ANTENNA table, copy it from there.
  // Otherwise fill in a default value.
  // Only the array positions are written directly.
  // Fill the ANTENNA subtable.
  Table antTab;
  if (! antennaTableName.empty()) {
    antTab = Table(antennaTableName, TableLock(TableLock::AutoNoReadLocking));
  }
  MSAntenna msant = itsMS.antenna();
  msant.addRow (itsNrAnt);
  Vector<Double> antOffset(3);
  antOffset = 0;
  MSAntennaColumns msantCol(msant);
  // First copy the possible input columns.
  TableCopy::copyRows (msant, antTab);
  // Write default values if there was no such input column.
  if (! antTab.tableDesc().isColumn("NAME")) {
    for (Int i=0; i<itsNrAnt; i++) {
      msantCol.name().put (i, "ST_" + String::toString(i));
    }
  }
  if (! antTab.tableDesc().isColumn("STATION")) {
    for (Int i=0; i<itsNrAnt; i++) {
      msantCol.station().put (i, "LOFAR");
    }
  }
  if (! antTab.tableDesc().isColumn("TYPE")) {
    for (Int i=0; i<itsNrAnt; i++) {
      msantCol.type().put (i, "GROUND-BASED");
    }
  }
  if (! antTab.tableDesc().isColumn("MOUNT")) {
    for (Int i=0; i<itsNrAnt; i++) {
      msantCol.mount().put (i, "ALT-AZ");
    }
  }
  if (! antTab.tableDesc().isColumn("OFFSET")) {
    for (Int i=0; i<itsNrAnt; i++) {
      msantCol.offset().put (i, antOffset);
    }
  }
  if (! antTab.tableDesc().isColumn("DISH_DIAMETER")) {
    for (Int i=0; i<itsNrAnt; i++) {
      msantCol.dishDiameter().put (i, 150);
    }
  }
  if (! antTab.tableDesc().isColumn("FLAG_ROW")) {
    for (Int i=0; i<itsNrAnt; i++) {
      msantCol.flagRow().put (i, False);
    }
  }
  // Always write the position.
  for (Int i=0; i<itsNrAnt; i++) {
    msantCol.positionMeas().put (i, antMPos[i]);
  }
  msant.flush();
}

void MSCreateCasa::fillSpwPol()
{
  for (uInt i=0; i<itsNFreq.size(); ++i) {
    addBand (i, itsNPol[i], itsNFreq[i],
             itsStartFreq[i], itsStepFreq[i]);
  }
}

void MSCreateCasa::addBand (int band, int npolarizations, int nchannels,
                            double startFreq, double chanWidth)
{
  AlwaysAssert (nchannels > 0, AipsError);
  AlwaysAssert (npolarizations==1 || npolarizations==2 || npolarizations==4,
                AipsError);
  double refFreq = startFreq + 0.5*(nchannels+1)*chanWidth;
  Vector<double> chanWidths(nchannels);
  Vector<double> chanFreqs(nchannels);
  chanWidths = chanWidth;
  indgen (chanFreqs, startFreq + chanWidth/2., chanWidth);
  // Find out if this nr of polarizations has already been given.
  Int polnr = -1;
  for (int i=0; i<band; i++) {
    if (npolarizations == itsNPol[i]) {
      polnr = itsPolnr[i];
      break;
    }
  }
  // If not, add an entry to the POLARIZATION subtable.
  if (polnr < 0) {
    polnr = addPolarization (npolarizations);
  }
  // Add a row to the DATA_DESCRIPTION subtable.
  MSDataDescription msdd = itsMS.dataDescription();
  MSDataDescColumns msddCol(msdd);
  uInt rownr = msdd.nrow();
  msdd.addRow();
  msddCol.spectralWindowId().put (rownr, rownr);
  msddCol.polarizationId().put (rownr, polnr);
  msddCol.flagRow().put (rownr, False);
  // Add a row to the SPECTRAL_WINDOW subtable.
  // Find the total bandwidth from the minimum and maximum.
  Vector<double> stFreqs = chanFreqs - chanWidths/2.;
  Vector<double> endFreqs = chanFreqs + chanWidths/2.;
  double totalBW = max(endFreqs) - min(stFreqs);
  MSSpectralWindow msspw = itsMS.spectralWindow();
  MSSpWindowColumns msspwCol(msspw);
  msspw.addRow();
  msspwCol.numChan().put (rownr, nchannels);
  msspwCol.name().put (rownr, "");
  msspwCol.refFrequency().put (rownr, refFreq);
  msspwCol.chanFreq().put (rownr, chanFreqs);
  msspwCol.chanWidth().put (rownr, chanWidths);
  msspwCol.measFreqRef().put (rownr, MFrequency::TOPO);
  msspwCol.effectiveBW().put (rownr, chanWidths);
  msspwCol.resolution().put (rownr, chanWidths);
  msspwCol.totalBandwidth().put (rownr, totalBW);
  msspwCol.netSideband().put (rownr, 0);
  msspwCol.ifConvChain().put (rownr, 0);
  msspwCol.freqGroup().put (rownr, 0);
  msspwCol.freqGroupName().put (rownr, "");
  msspwCol.flagRow().put (rownr, False);
  // Now add the band to the internal blocks.
  itsPolnr.push_back (polnr);
}

int MSCreateCasa::addPolarization (int npolarizations)
{
  MSPolarization mspol = itsMS.polarization();
  MSPolarizationColumns mspolCol(mspol);
  uInt rownr = mspol.nrow();
  Vector<Int> corrType(npolarizations);
  corrType(0) = Stokes::XX;
  if (npolarizations == 2) {
    corrType(1) = Stokes::YY;
  } else if (npolarizations == 4) {
    corrType(1) = Stokes::XY;
    corrType(2) = Stokes::YX;
    corrType(3) = Stokes::YY;
  }
  Matrix<Int> corrProduct(2, npolarizations);
  for (Int i=0; i<npolarizations; i++) {
    corrProduct(0,i) = Stokes::receptor1(Stokes::type(corrType(i)));
    corrProduct(1,i) = Stokes::receptor2(Stokes::type(corrType(i)));
  }
  // Fill the columns.
  mspol.addRow();
  mspolCol.numCorr().put (rownr, npolarizations);
  mspolCol.corrType().put (rownr, corrType);
  mspolCol.corrProduct().put (rownr, corrProduct);
  mspolCol.flagRow().put (rownr, False);
  mspol.flush();
  return rownr;
}

void MSCreateCasa::fillField()
{
  for (uInt i=0; i<itsRa.size(); ++i) {
    addField (i);
  }
}

void MSCreateCasa::addField (int field)
{
  Vector<MDirection> outdir(1);
  outdir[0] = itsPhaseDir[field];
  // Put the direction into the FIELD subtable.
  {
    MSField msfield = itsMS.field();
    MSFieldColumns msfieldCol(msfield);
    uInt rownr = msfield.nrow();
    msfield.addRow();
    msfieldCol.name().put (rownr, "BEAM_" + String::toString(rownr));
    msfieldCol.code().put (rownr, "");
    msfieldCol.time().put (rownr, itsStartTime); // really startTime; everywhere else it is midpoint with an interval
    msfieldCol.numPoly().put (rownr, 0);
    msfieldCol.delayDirMeasCol().put (rownr, outdir);
    msfieldCol.phaseDirMeasCol().put (rownr, outdir);
    msfieldCol.referenceDirMeasCol().put (rownr, outdir);
    msfieldCol.sourceId().put (rownr, -1);
    msfieldCol.flagRow().put (rownr, False);
  }
  // Put the direction for each antenna into the POINTING subtable.
  {
    MSPointing mspointing = itsMS.pointing();
    MSPointingColumns mspointingCol(mspointing);
    uInt rownr = mspointing.nrow();
    mspointing.addRow(itsNrAnt);
    for (Int i=0; i<itsNrAnt; i++) {
      mspointingCol.antennaId().put (rownr, i);
      mspointingCol.time().put (rownr, itsStartTime); // actually midpoint (as in updateTimes()), but interval is still 0.0 (unknown) at creation
      mspointingCol.interval().put (rownr, 0.);
      mspointingCol.name().put (rownr, "");
      mspointingCol.numPoly().put (rownr, 0);
      mspointingCol.timeOrigin().put (rownr, itsStartTime);
      mspointingCol.directionMeasCol().put (rownr, outdir);
      mspointingCol.targetMeasCol().put (rownr, outdir);
      mspointingCol.tracking().put (rownr, False);
      rownr++;
    }
  }
}

void MSCreateCasa::fillFeed()
{
  // Determine constants for the FEED subtable.
  Int nRec = 2;
  Matrix<Double> feedOffset(2,nRec);
  feedOffset = 0;
  Matrix<Complex> feedResponse(nRec,nRec);
  feedResponse = Complex(0.0,0.0);
  for (Int rec=0; rec<nRec; rec++) {
    feedResponse(rec,rec) = Complex(1.0,0.0);
  }
  Vector<String> feedType(nRec);
  feedType(0) = "X";
  feedType(1) = "Y";
  Vector<Double> feedPos(3);
  feedPos = 0.0;
  Vector<Double> feedAngle(nRec);
  feedAngle = -C::pi_4;                      // 0 for parallel dipoles
  // Fill the FEED subtable.
  MSFeed msfeed = itsMS.feed();
  MSFeedColumns msfeedCol(msfeed);
  msfeed.addRow (itsNrAnt);
  for (Int i=0; i<itsNrAnt; i++) {
    msfeedCol.antennaId().put (i, i);
    msfeedCol.feedId().put (i, 0);
    msfeedCol.spectralWindowId().put (i, -1);
    msfeedCol.time().put (i, itsStartTime + itsNrTimes*itsStepTime/2.);
    msfeedCol.interval().put (i, itsNrTimes*itsStepTime);
    msfeedCol.beamId().put (i, -1);
    msfeedCol.beamOffset().put (i, feedOffset);
    msfeedCol.polarizationType().put (i, feedType);
    msfeedCol.polResponse().put (i, feedResponse);
    msfeedCol.position().put (i, feedPos);
    msfeedCol.receptorAngle().put (i, feedAngle);
    msfeedCol.numReceptors().put (i, 2);
  }
  msfeed.flush();
}

void MSCreateCasa::fillObservation()
{
  MSObservation msobs = itsMS.observation();
  MSObservationColumns msobsCol(msobs);
  Vector<String> corrSchedule(1);
  corrSchedule = "corrSchedule";
  Vector<Double> timeRange(2);
  timeRange(0) = itsStartTime;
  timeRange(1) = itsStartTime + itsNrTimes*itsStepTime;
  // Data is public one year after end of observation.
  Double releaseDate = timeRange(1) + 365.25*24*60*60;
  // Fill the columns
  msobs.addRow();
  msobsCol.telescopeName().put (0, "LOFAR");
  msobsCol.timeRange().put (0, timeRange);
  msobsCol.observer().put (0, "MSCreate");
  msobsCol.scheduleType().put (0, "LOFAR");
  msobsCol.schedule().put (0, corrSchedule);
  msobsCol.project().put (0, "MSCreate");
  msobsCol.releaseDate().put (0, releaseDate);
  msobsCol.flagRow().put (0, False);
  msobs.flush();
}

void MSCreateCasa::fillProcessor()
{
  MSProcessor msproc = itsMS.processor();
  MSProcessorColumns msprocCol(msproc);
  // Fill the columns
  msproc.addRow();
  msprocCol.type().put (0, "CORRELATOR");
  msprocCol.subType().put (0, "");
  msprocCol.typeId().put (0, -1);
  msprocCol.modeId().put (0, -1);
  msprocCol.flagRow().put (0, False);
  msproc.flush();
}

void MSCreateCasa::fillState()
{
  MSState msstate = itsMS.state();
  MSStateColumns msstateCol(msstate);
  // Fill the columns
  msstate.addRow();
  msstateCol.sig().put (0, True);
  msstateCol.ref().put (0, False);
  msstateCol.cal().put (0, 0.);
  msstateCol.load().put (0, 0.);
  msstateCol.subScan().put (0, 0);
  msstateCol.obsMode().put (0, "");
  msstateCol.flagRow().put (0, False);
  msstate.flush();
}

void MSCreateCasa::updateTimes()
{
  // Calculate the interval, end, and central time.
  Double interval = itsNrTimes*itsStepTime;
  Double endTime = itsStartTime + interval;
  Double midTime = (itsStartTime + endTime) / 2;
  // Update all rows in FEED subtable.
  {
    MSFeed mssub (itsMS.keywordSet().asTable("FEED"));
    MSFeedColumns mssubCol(mssub);
    Vector<Double> val(mssub.nrow());
    val = midTime;
    mssubCol.time().putColumn (val);
    val = interval;
    mssubCol.interval().putColumn (val);
  }
  // Update all rows in POINTING subtable.
  {
    MSPointing mssub (itsMS.keywordSet().asTable("POINTING"));
    MSPointingColumns mssubCol(mssub);
    Vector<Double> val(mssub.nrow());
    val = midTime;
    mssubCol.time().putColumn (val);
    val = interval;
    mssubCol.interval().putColumn (val);
  }
  // Update all rows in OBSERVATION subtable.
  {
    MSObservation msobs (itsMS.keywordSet().asTable("OBSERVATION"));
    MSObservationColumns msobsCol(msobs);
    Vector<Double> timeRange(2);
    timeRange(0) = itsStartTime;
    timeRange(1) = itsStartTime + itsNrTimes*itsStepTime;
    for (uInt i=0; i<msobs.nrow(); i++) {
      msobsCol.timeRange().put (i, timeRange);
    }
  }
}

void MSCreateCasa::addRows (int nbasel, int nfield)
{
  int nrow = nbasel*nfield*itsNSpw;
  itsMS.addRow (nrow);
}

void MSCreateCasa::writeSimpleMainColumns()
{
  // Columns with the same val everywhere and IncrStMan. Write once in row 0.
  itsMSCol->feed1().put(0, 0);
  itsMSCol->feed2().put(0, 0);
  itsMSCol->processorId().put(0, 0);
  itsMSCol->scanNumber().put(0, 0);
  itsMSCol->arrayId().put(0, 0);
  itsMSCol->observationId().put(0, 0);
  itsMSCol->stateId().put(0, 0);
  itsMSCol->interval().put(0, itsStepTime);
  itsMSCol->exposure().put(0, itsStepTime);
  itsMSCol->flagRow().put(0, False);
  Vector<float> ones(4, 1.0f);
  itsMSCol->weight().put(0, ones);
  itsMSCol->sigma().put(0, ones);
}

void MSCreateCasa::writeTimeStepRows (int band, int field,
                                      const vector<Vector<Double> >& antuvw)
{
  if (itsNrRow == 0) {
    writeSimpleMainColumns();
  }
  // Find the shape of the data array in each table row.
  IPosition shape(2, itsNPol[band], itsNFreq[band]);
  Array<Bool> defFlags(shape, False);
  Array<Complex> defData;
  Array<float> defFloatData;
  if (itsWriteFloatData) {
    defFloatData.resize (shape);
    // Make data non-zero to avoid possible file system optimizations.
    indgen (defFloatData, 0.0f, 0.03f);
  } else {
    defData.resize (shape);
    indgen (defData, Complex(), Complex(0.01, 0.02));
  }
  Array<Float> sigma(IPosition(1, shape(0)));
  sigma = 1;
  Array<Float> weight(IPosition(1, shape(0)));
  weight = 1;
  Array<float> weightSpectrum;
  if (itsWriteWeightSpectrum) {
    weightSpectrum.resize (shape);
    weightSpectrum = 1;
  }
  Double time = itsStartTime + itsNrTimes*itsStepTime + itsStepTime/2;
  Vector<double> myuvw(3, 0);
  for (int j=0; j<itsNrAnt; ++j) {
    int st = (itsWriteAutoCorr ? j : j+1);
    int end= (itsWriteFloatData ? j+1 : itsNrAnt);
    for (int i=st; i<end; ++i) {
      if (itsCalcUVW) {
        myuvw = antuvw[i] - antuvw[j];
      }
      if (itsWriteFloatData) {
        itsMSCol->floatData().put(itsNrRow, defFloatData);
      } else {
        itsMSCol->data().put(itsNrRow, defData);
      }
      itsMSCol->flag().put(itsNrRow, defFlags);
      if (itsWriteWeightSpectrum) {
        itsMSCol->weightSpectrum().put(itsNrRow, weightSpectrum);
      }
      itsMSCol->time().put (itsNrRow, time);
      itsMSCol->timeCentroid().put (itsNrRow, time);
      itsMSCol->antenna1().put (itsNrRow, j);
      itsMSCol->antenna2().put (itsNrRow, i);
      itsMSCol->dataDescId().put (itsNrRow, band);
      itsMSCol->fieldId().put (itsNrRow, field);
      itsMSCol->uvw().put (itsNrRow, myuvw);
      itsNrRow++;
    }
  }
}

void MSCreateCasa::writeTimeStepSpw (int band, int field,
                                     const vector<Vector<Double> >& antuvw)
{
  if (itsNrRow == 0) {
    writeSimpleMainColumns();
  }
  int nrbasel = nbaselines();
  // Find the shape of the data array in each table row.
  IPosition shape(3, itsNPol[band], itsNFreq[band], nrbasel);
  Double time = itsStartTime + itsNrTimes*itsStepTime + itsStepTime/2;
  Vector<double> times(nrbasel, time);
  Vector<int> vecint(nrbasel);
  Vector<int> vecint2(nrbasel);
  Matrix<double> myuvw(3, nrbasel, 0);
  RefRows rows (itsNrRow, itsNrRow+nrbasel-1);
  VectorIterator<double> uvwiter(myuvw);
  int inx=0;
  for (int j=0; j<itsNrAnt; ++j) {
    int st = (itsWriteAutoCorr ? j : j+1);
    int end= (itsWriteFloatData ? j+1 : itsNrAnt);
    for (int i=st; i<end; ++i) {
      vecint[inx] = j;
      vecint2[inx] = i;
      inx++;
      if (itsCalcUVW) {
        uvwiter.vector() = antuvw[i] - antuvw[j];
        uvwiter.next();
      }
    }
  }
  if (itsWriteFloatData) {
    Array<float> arr(shape);
    indgen (arr, 0.0f, 0.03f);
    itsMSCol->floatData().putColumnCells(rows, arr);
  } else {
    Array<Complex> arr(shape);
    indgen (arr, Complex(), Complex(0.01, 0.02));
    itsMSCol->data().putColumnCells(rows, Array<Complex>(shape));
  }
  itsMSCol->flag().putColumnCells(rows, Array<Bool>(shape, False));
  if (itsWriteWeightSpectrum) {
    itsMSCol->weightSpectrum().putColumnCells(rows, Array<float>(shape, 1));
  }
  itsMSCol->time().putColumnCells (rows, times);
  itsMSCol->timeCentroid().putColumnCells (rows, times);
  itsMSCol->antenna1().putColumnCells (rows, vecint);
  itsMSCol->antenna2().putColumnCells (rows, vecint2);
  vecint = band;
  itsMSCol->dataDescId().putColumnCells (rows, vecint);
  vecint = field;
  itsMSCol->fieldId().putColumnCells (rows, vecint);
  itsMSCol->uvw().putColumnCells (rows, myuvw);
  itsNrRow += nrbasel;
}

void MSCreateCasa::addImagerColumns()
{
  // Find data shape from FLAG column.
  // Make tiles of appr. 1 MB.
  IPosition shape = ROTableColumn(itsMS, MS::columnName(MS::FLAG)).shapeColumn();
  String colName = MS::columnName(MS::CORRECTED_DATA);
  if (! itsMS.tableDesc().isColumn(colName)) {
    TableDesc td;
    if (shape.empty()) {
      td.addColumn (ArrayColumnDesc<Complex>(colName, "corrected data"));
    } else {
      td.addColumn (ArrayColumnDesc<Complex>(colName, "corrected data", shape,
                                             ColumnDesc::FixedShape));
    }
    TiledColumnStMan stMan("TiledCorrectedData", itsDataTileShape);
    itsMS.addColumn (td, stMan);
  }
  colName = MS::columnName(MS::MODEL_DATA);
  if (! itsMS.tableDesc().isColumn(colName)) {
    TableDesc td;
    if (shape.empty()) {
      td.addColumn (ArrayColumnDesc<Complex>(colName, "model data"));
    } else {
      td.addColumn (ArrayColumnDesc<Complex>(colName, "model data", shape,
                                             ColumnDesc::FixedShape));
    }
    TiledColumnStMan stMan("TiledModelData", itsDataTileShape);
    itsMS.addColumn (td, stMan);
    // Set MODEL_DATA keyword for casa::VisSet.
    // Sort out the channel selection.
    if (itsMS.spectralWindow().isNull()) {
      itsMS.spectralWindow() =
        MSSpectralWindow(itsMS.keywordSet().asTable("SPECTRAL_WINDOW"));
    }
    MSSpWindowColumns msSpW(itsMS.spectralWindow());
    Matrix<Int> selection(2, msSpW.nrow());
    // Fill in default selection (all bands and channels).
    selection.row(0) = 0;    //start
    selection.row(1) = msSpW.numChan().getColumn();
    ArrayColumn<Complex> mcd(itsMS, colName);
    mcd.rwKeywordSet().define ("CHANNEL_SELECTION",selection);
  }
}

void MSCreateCasa::showCacheStatistics() const
{
  cout << (itsWriteFloatData ? "FLOAT_DATA: " : "DATA: ");
  RODataManAccessor(itsMS, "TiledData", False).showCacheStatistics (cout);
  RODataManAccessor(itsMS, "SSMData", False).showCacheStatistics (cout);
  RODataManAccessor(itsMS, "ISMData", False).showCacheStatistics (cout);
}



MSCreateHDF5::MSCreateHDF5()
  : itsNrRow (0)
{
  // Create the meta data type.
  makeMetaType();
}

MSCreateHDF5::~MSCreateHDF5()
{
}

void MSCreateHDF5::createMS (const String& msName, int ntimeField,
                             int /*useMultiFile*/, int /*multiBlockSize*/,
                             bool createImagerColumns,
                             const String& /*flagColumn*/, int /*nflagBits*/)
{
  Timer timer;
  // Create the file.
  itsFile = new HDF5File(msName, ByteIO::New);
  int nrbasel = nbaselines();
  // Store some attributes defining nr of baselines and fields.
  Record rec;
  rec.define ("NAntenna", itsNrAnt);
  rec.define ("NBaseline", nrbasel);
  rec.define ("NField", int(itsRa.size()));
  rec.define ("NTimeField", ntimeField);
  // Create a group per spectral window.
  for (int band=itsSpw; band<itsSpw+itsNSpw; ++band) {
    HDF5Spw spw;
    spw.spw = new HDF5Group(*itsFile, "SPW_"+String::toString(band));
    // Write the attributes.
    HDF5Record::writeRecord (*(spw.spw), "ATTR", rec);
    // Create the data in the spw.
    IPosition shape(3, itsNPol[band], itsNFreq[band], 0);
    IPosition shape1(1, 0);
    IPosition tileShape1(1, nrbasel);
    uInt freqPerTile = itsDataTileShape[1];
    uInt cacheSize = (itsNFreq[band] + freqPerTile - 1) / freqPerTile;
    cout << "HDF5 cacheSize = " << cacheSize << endl;
    if (itsWriteFloatData) {
      spw.floatData = new HDF5DataSet (*spw.spw, "FLOAT_DATA", shape,
                                       itsDataTileShape, (float*)0);
      spw.floatData->setCacheSize (cacheSize);
    } else {
      spw.data = new HDF5DataSet (*spw.spw, "DATA", shape,
                                  itsDataTileShape, (Complex*)0);
      spw.data->setCacheSize (cacheSize);
    }
    IPosition tileShape(itsDataTileShape);
    tileShape[2] *= 8;
    spw.flag = new HDF5DataSet (*spw.spw, "FLAG", shape, tileShape,
                                (Bool*)0);
    spw.flag->setCacheSize (cacheSize);
    if (itsWriteWeightSpectrum) {
      spw.weightSpectrum = new HDF5DataSet (*spw.spw, "WEIGHT_SPECTRUM", shape,
                                            itsDataTileShape, (float*)0);
      spw.weightSpectrum->setCacheSize (cacheSize);
    }
    spw.metaData = new HDF5DataSet (*spw.spw, "METADATA", shape1,
                                    tileShape1, itsMetaType);
    if (createImagerColumns) {
      spw.modelData = new HDF5DataSet (*spw.spw, "MODEL_DATA", shape, tileShape,
                                       (Complex*)0);
      spw.corrData = new HDF5DataSet (*spw.spw, "CORRECTED_DATA", shape, tileShape,
                                      (Complex*)0);
      // Not written, so no need to set their cache sizes.
    }
    itsSpws.push_back (spw);
  }
}

void MSCreateHDF5::makeMetaType()
{
  // Push the fields in the same order as defined in the HDF5MetaData struct.
  vector<HDF5DataType> types;
  vector<String> names;
  types.push_back (HDF5DataType((double*)0));
  names.push_back ("time");
  types.push_back (HDF5DataType((double*)0));
  names.push_back ("timeCentroid");
  types.push_back (HDF5DataType((double*)0));
  names.push_back ("interval");
  types.push_back (HDF5DataType((double*)0));
  names.push_back ("exposure");
  types.push_back (HDF5DataType (HDF5DataType((double*)0), IPosition(1,3)));
  names.push_back ("uvw");
  types.push_back (HDF5DataType (HDF5DataType((float*)0), IPosition(1,4)));
  names.push_back ("weight");
  types.push_back (HDF5DataType (HDF5DataType((float*)0), IPosition(1,4)));
  names.push_back ("sigma");
  types.push_back (HDF5DataType((Int*)0));
  names.push_back ("antenna1");
  types.push_back (HDF5DataType((Int*)0));
  names.push_back ("antenna2");
  types.push_back (HDF5DataType((Int*)0));
  names.push_back ("arrayId");
  types.push_back (HDF5DataType((Int*)0));
  names.push_back ("fieldId");
  types.push_back (HDF5DataType((Int*)0));
  names.push_back ("dataDescId");
  types.push_back (HDF5DataType((Int*)0));
  names.push_back ("stateId");
  types.push_back (HDF5DataType((Int*)0));
  names.push_back ("flagRow");
  types.push_back (HDF5DataType((Int*)0));
  names.push_back ("feed1");
  types.push_back (HDF5DataType((Int*)0));
  names.push_back ("feed2");
  types.push_back (HDF5DataType((Int*)0));
  names.push_back ("processorId");
  types.push_back (HDF5DataType((Int*)0));
  names.push_back ("scanNumber");
  types.push_back (HDF5DataType((Int*)0));
  names.push_back ("observationId");
  itsMetaType = HDF5DataType (names, types);
}

void MSCreateHDF5::addRows (int nbasel, int nfield)
{
  int nrow = nbasel*nfield;
  for (int band=itsSpw; band<itsSpw+itsNSpw; ++band) {
    // Define the new shape of the data arrays.
    IPosition newShape3(3, itsNPol[band], itsNFreq[band], itsNrRow+nrow);
    IPosition newShape1(1, itsNrRow+nrow);
    // Extend the data arrays.
    itsSpws[band].data->extend (newShape3);
    itsSpws[band].flag->extend (newShape3);
    if (itsWriteWeightSpectrum) {
      itsSpws[band].weightSpectrum->extend (newShape3);
    }
    itsSpws[band].metaData->extend (newShape1);
  }
}

void MSCreateHDF5::writeTimeStepSpw (int band, int field,
                                     const vector<Vector<Double> >& antuvw)
{
  int nrbasel = nbaselines();
  // Get the time.
  Double time = itsStartTime + itsNrTimes*itsStepTime + itsStepTime/2;
  // Fill meta data.
  Vector<HDF5MetaData> meta(nrbasel);
  RefRows rows (itsNrRow, itsNrRow+nrbasel-1);
  int inx=0;
  for (int j=0; j<itsNrAnt; ++j) {
    int st = (itsWriteAutoCorr ? j : j+1);
    int end= (itsWriteFloatData ? j+1 : itsNrAnt);
    for (int i=st; i<end; ++i) {
      meta[inx].antenna1 = j;
      meta[inx].antenna2 = i;
      Vector<double> uvw (antuvw[i] - antuvw[j]);
      for (int i=0; i<3; ++i) meta[inx].uvw[i] = uvw[i];
      for (int i=0; i<4; ++i) meta[inx].weight[i] = 1;
      for (int i=0; i<4; ++i) meta[inx].sigma[i] = 1;
      meta[inx].time = time;
      meta[inx].timeCentroid = time;
      meta[inx].interval = itsStepTime;
      meta[inx].exposure = itsStepTime;
      meta[inx].dataDescId = band;
      meta[inx].fieldId = field;
      inx++;
    }
  }
  // Define the shape of the data arrays.
  IPosition shape3(3, itsNPol[band], itsNFreq[band], nrbasel);
  IPosition shape1(1, nrbasel);
  // Define the slicers to put the data arrays.
  Slicer slicer3(IPosition(3,0,0,itsNrRow), shape3);
  Slicer slicer1(IPosition(1,itsNrRow), shape1);
  // Put the data.
  if (itsWriteFloatData) {
    Array<float> arr(shape3);
    indgen (arr, 0.0f, 0.03f);
    itsSpws[band].floatData->put (slicer3, arr);
  } else {
    Array<Complex> arr(shape3);
    indgen (arr, Complex(), Complex(0.01, 0.02));
    itsSpws[band].data->put (slicer3, arr);
  }
  itsSpws[band].flag->put (slicer3, Array<Bool>(shape3, False));
  if (itsWriteWeightSpectrum) {
    itsSpws[band].weightSpectrum->put (slicer3, Array<float>(shape3, 1));
  }
  itsSpws[band].metaData->put (slicer1, meta);
  // Increase nr of rows.
  itsNrRow += nrbasel;
}

void MSCreateHDF5::writeTimeStepRows (int band, int field,
                                      const vector<Vector<Double> >& antuvw)
{
  // Find the shape of the data array in each table row.
  IPosition shape3(3, itsNPol[band], itsNFreq[band], 1);
  IPosition shape2(2, itsNPol[band], 1);
  IPosition shapeu(2, 3, 1);
  IPosition shape1(1, 1);
  Array<Bool> defFlags(shape3, False);
  Array<Complex> defData;
  Array<float> defFloatData;
  if (itsWriteFloatData) {
    defFloatData.resize (shape3);
    // Make data non-zero to avoid possible file system optimizations.
    indgen (defFloatData, 0.0f, 0.03f);
  } else {
    defData.resize (shape3);
    indgen (defData, Complex(), Complex(0.01, 0.02));
  }
  Matrix<Float> weightsigma(shape3[0], 1, 1.);
  Array<float> weightSpectrum;
  if (itsWriteWeightSpectrum) {
    weightSpectrum.resize (shape3);
    weightSpectrum = 1;
  }
  Double time = itsStartTime + itsNrTimes*itsStepTime + itsStepTime/2;
  // Fill meta data.
  Vector<HDF5MetaData> meta(1);
  for (int i=0; i<4; ++i) meta[0].weight[i] = 1;
  for (int i=0; i<4; ++i) meta[0].sigma[i] = 1;
  meta[0].time = time;
  meta[0].timeCentroid = time;
  meta[0].interval = itsStepTime;
  meta[0].exposure = itsStepTime;
  meta[0].dataDescId = band;
  meta[0].fieldId = field;
  // Define the slicers to put the data arrays.
  for (int j=0; j<itsNrAnt; ++j) {
    int st = (itsWriteAutoCorr ? j : j+1);
    int end= (itsWriteFloatData ? j+1 : itsNrAnt);
    for (int i=st; i<end; ++i) {
      Slicer slicer3(IPosition(3,0,0,itsNrRow), shape3);
      Slicer slicer1(IPosition(1,itsNrRow), shape1);
      meta[0].uvw[0] = antuvw[i][0] - antuvw[j][0];
      meta[0].uvw[1] = antuvw[i][1] - antuvw[j][1];
      meta[0].uvw[2] = antuvw[i][2] - antuvw[j][2];
      if (itsWriteFloatData) {
        itsSpws[band].floatData->put (slicer3, defFloatData);
      } else {
        itsSpws[band].data->put (slicer3, defData);
      }
      itsSpws[band].flag->put (slicer3, defFlags);
      if (itsWriteWeightSpectrum) {
        itsSpws[band].weightSpectrum->put (slicer3, Array<float>(shape3, 1));
      }
      meta[0].antenna1 = j;
      meta[0].antenna2 = i;
      itsSpws[band].metaData->put (slicer1, meta);
      itsNrRow++;
    }
  }
}

void MSCreateHDF5::fillAntenna (const Block<MPosition>& /*antMPos*/,
                                const String& /*antennaTableName*/)
{}
void MSCreateHDF5::fillSpwPol()
{}
void MSCreateHDF5::fillField()
{}
void MSCreateHDF5::fillFeed()
{}
void MSCreateHDF5::fillObservation()
{}
void MSCreateHDF5::fillProcessor()
{}
void MSCreateHDF5::fillState()
{}
void MSCreateHDF5::updateTimes()
{}
void MSCreateHDF5::closeSubTables()
{}
void MSCreateHDF5::showCacheStatistics() const
{}
void MSCreateHDF5::flush()
{
  itsFile->flush();
}
Int64 MSCreateHDF5::nrow() const
{
  return itsNrRow;
}





IPosition formTileShape (int tileSize, int tileNPol, int tileNFreq,
                         bool writeFloatData,
                         const Vector<int>& npol,
                         const Vector<int>& nfreq)
{
  // Determine the tile size to use.
  // Store all polarisations in a single tile.
  // Flags are stored as bits, so take care each tile has multiple of 8 flags.
  int tsp = tileNPol;
  int tsf = tileNFreq;
  int ts  = tileSize;
  if (tsp <= 0) {
    tsp = max(npol);     // default is all polarizations
  }
  if (tsf <= 0) {
    tsf = max(nfreq);     // default is all channels
  }
  if (ts <= 0) {
    ts = 1024*1024;       // default is 1 MByte
  }
  int tsr = std::max (1, ts / (tsp*tsf*8));
  if (writeFloatData) {
    tsr = std::max (1, ts / (tsp*tsf*4));
  }
  return IPosition(3,tsp,tsf,tsr);
}

void showHelp()
{
  cout << "The program creates one or more MeasurementSets with a given number of" <<endl;
  cout << "baselines, times, fields, spectral windows, channels and polarizations." << endl;
  cout << "Run as:" << endl;
  cout << "      writems parm=value parm=value ..." << endl;
  cout << "Use   writems -h   to see the possible parameters." << endl;
}

Int64 parmInt (Input& params, const String& name, const Record& vars=Record())
{
  return RecordGram::expr2Int (params.getString(name), vars);
}

Array<Int64> parmArrayInt (Input& params, const String& name)
{
  return RecordGram::expr2ArrayInt (params.getString(name));
}

Array<double> parmArrayDouble (Input& params, const String& name,
                               const String& unit=String())
{
  return RecordGram::expr2ArrayDouble (params.getString(name), Record(), unit);
}


bool readParms (int argc, char* argv[])
{
  // enable input in no-prompt mode
  Input params(1);
  // define the input structure
  params.version("2017Oct31GvD");
  params.create ("nms", "0",
                 "Number of MeasurementSets to create (0 means 1 MS without suffix, >0 also creates MultiMS)",
                 "int");
  params.create ("msname", "",
                 "Name of the output MeasurementSet (suffix _p<i> is added if nms>0)",
                 "string");
  params.create ("ra", "",
                 "One or more J2000 Right Ascensions (as hh:mm:ss.sss); defines the fields",
                 "string");
  params.create ("dec", "",
                 "One or more J2000 Declinations (as dd.mm.ss.sss); defines the fields",
                 "string");
  params.create ("anttab", "",
                 "Name of the ANTENNA table giving the antenna parameters to use (zero/empty values if not given)",
                 "string");
  params.create ("startfreq", "1 GHz",
                 "Start frequency (Hz) per spw (1 value counts on for other spws)",
                 "double");
  params.create ("chanwidth", "1 MHz",
                 "Channel frequency width (Hz) per spw (1 value applies to all spws)",
                 "double");
  params.create ("starttime", "",
                 "Start time (e.g., 23Mar2016/12:00:00)",
                 "string");
  params.create ("timestep", "1",
                 "Time interval (sec)",
                 "double");
  params.create ("ntime", "1",
                 "Number of time steps",
                 "int");
  params.create ("ntimefield", "0",
                 "Number of time steps per field (0=all fields for all times)",
                 "int");
  params.create ("totalspw", "nspw",
                 "Total number of spectral windows (to write in SPECTRAL_WINDOW)",
                 "int");
  params.create ("firstspw", "0",
                 "First spectral window to write in this set of MSs",
                 "int");
  params.create ("nspw", "1",
                 "Number of spectral windows to write in this set of MSs",
                 "int");
  params.create ("npol", "4",
                 "Number of polarizations per spectral window; 1 value applies to all spws",
                 "int");
  params.create ("nchan", "256",
                 "Number of channels per spectral window; 1 value applies to all spws",
                 "int");
  params.create ("nant", "0",
                 "Number of antennae to use; it anttab is given maximum to use is its size",
                 "int");
  params.create ("calcuvw", "false",
                 "Calculate UVW coordinates?",
                 "bool");
  params.create ("autocorr", "true",
                 "Write autocorrelations?",
                 "bool");
  params.create ("floatdata", "false",
                 "Write only autocorrelations and FLOAT_DATA instead of DATA?",
                 "bool");
  params.create ("weightspectrum", "false",
                 "Write WEIGHT_SPECTRUM column?",
                 "bool");
  params.create ("imagercolumns", "false",
                 "Write imager columns (MODEL_DATA, CORRECTED_DATA)?",
                 "bool");
  params.create ("rowwise", "false",
                 "Write the data row wise (thus a put per row)",
                 "bool");
  params.create ("nflagbits", "0",
                 "Write multiple flag bits (0, 8, 16 or 32) mapped to FLAG",
                 "int");
  params.create ("flagcolumn", "FLAG_BITS",
                 "Name of the FlagBits column if nflagbits>0",
                 "string");
  params.create ("tilesize", "-1",
                 "Size of data tiles (bytes); default is 1024*1024",
                 "int");
  params.create ("tilesizepol", "-1",
                 "Number of polarizations in data tiles; default is all",
                 "int");
  params.create ("tilesizefreq", "-1",
                 "Number of channels in data tiles; default is all",
                 "int");
  params.create ("multifile", "false",
                 "Use the MultiFile feature?",
                 "bool");
  params.create ("multihdf5", "false",
                 "Use the MultiHDF5 feature?",
                 "bool");
  params.create ("mfsize", "-1",
                 "MultiFile/HDF5 block size (bytes); default is tilesize",
                 "int");
  params.create ("ashdf5", "false",
                 "Write the data in HDF5 format",
                 "bool");
  params.create ("useadios2", "false",
                 "Use Adios2StMan for all columns",
                 "bool");
  // Fill the input structure from the command line.
  params.readArguments (argc, argv);
  // Get the various parameters.
  myMsName = params.getString ("msname");
  if (myMsName.empty()) {
    showHelp();
    return false;
  }
  myStepFreq  = Vector<double> (parmArrayDouble (params, "chanwidth", "Hz"));
  myStartFreq = Vector<double> (parmArrayDouble (params, "startfreq", "Hz"));
  myStepTime  = params.getDouble ("timestep");
  String startTimeStr = params.getString ("starttime");
  Quantity qn;
  AlwaysAssertExit (MVTime::read (qn, startTimeStr, true));
  myStartTime = qn.getValue ("s");
  Vector<String> raStr  = stringToVector (params.getString ("ra"));
  Vector<String> decStr = stringToVector (params.getString ("dec"));
  AlwaysAssertExit (raStr.size() > 0  &&  raStr.size() == decStr.size());
  for (uint i=0; i<raStr.size(); ++i) {
    AlwaysAssertExit (MVAngle::read (qn, raStr[i], true));
    myRa.push_back  (qn.getValue ("rad"));
    AlwaysAssertExit (MVAngle::read (qn, decStr[i], true));
    myDec.push_back (qn.getValue ("rad"));
  }
  myNBand      = params.getInt ("nspw");
  myNPart = params.getInt ("nms");
  myDoSinglePart = (myNPart == 0);
  if (myDoSinglePart) {
    myNPart = 1;
  }
  // Determine nr of bands per part (i.e., ms).
  AlwaysAssertExit (myNPart > 0);
  AlwaysAssertExit (myNBand > 0);
  if (myNBand > myNPart) {
    // Multiple bands per part.
    AlwaysAssertExit (myNBand%myNPart == 0);
  } else {
    // If fewer bands than parts, bands are spread over parts which is the
    // same as having as many bands as parts.
    AlwaysAssertExit (myNPart%myNBand == 0);
    myNBand = myNPart;
  }
  // firstspw and totalspw can be an expression of nspw.
  Record vars;
  vars.define ("nspw", myNBand);
  myFirstBand  = parmInt (params, "firstspw", vars);
  myTotalNBand = parmInt (params, "totalspw", vars);
  AlwaysAssertExit (myTotalNBand >= myNBand);
  myNChan = Vector<Int> (params.getIntArray ("nchan"));
  myNPol  = Vector<Int> (params.getIntArray ("npol"));
  myNTime = params.getInt ("ntime");
  myNTimeField = params.getInt ("ntimefield");
  // Determine possible tile size. Default is no tiling.
  myTileSizePol  = parmInt (params, "tilesizepol");
  myTileSizeFreq = parmInt (params, "tilesizefreq");
  myTileSize = parmInt (params, "tilesize");
  AlwaysAssertExit (myNPol.size() == 1  ||
                    myNPol.size() == uInt(myTotalNBand));
  if (myNPol.size() != uInt(myTotalNBand)) {
    int np = myNPol[0];
    myNPol.resize (myTotalNBand, True);
    myNPol = np;
  }
  AlwaysAssertExit (myNChan.size() == 1  ||
                    myNChan.size() == uInt(myTotalNBand));
  if (myNChan.size() != uInt(myTotalNBand)) {
    int nf = myNChan[0];
    myNChan.resize (myTotalNBand);
    myNChan = nf;
  }
  // Determine start and step frequency per band.
  AlwaysAssertExit (myStepFreq.size() == 1  ||
                    myStepFreq.size() == uInt(myTotalNBand));
  if (myStepFreq.size() != uInt(myTotalNBand)) {
    double f = myStepFreq[0];
    myStepFreq.resize (myTotalNBand, True);
    myStepFreq = f;
  }
  AlwaysAssertExit (myStartFreq.size() == 1  ||
                    myStartFreq.size() == uInt(myTotalNBand));
  if (myStartFreq.size() != uInt(myTotalNBand)) {
    myStartFreq.resize (myTotalNBand, True);
    for (int i=1; i<myTotalNBand; ++i) {
      myStartFreq[i] = (myStartFreq[i-1] + myNChan[i-1] * myStepFreq[i-1] +
                         0.5 * (myStepFreq[i] - myStepFreq[i-1]));
    }
  }
  for (int i=0; i<myTotalNBand; ++i) {
    AlwaysAssertExit (myStepFreq[i] > 0);
    AlwaysAssertExit (myStartFreq[i] > 0);
  }
  AlwaysAssertExit (myStepTime > 0);
  // Get remaining parameters.
  myWriteAutoCorr       = params.getBool   ("autocorr");
  myWriteFloatData      = params.getBool   ("floatdata");
  if (myWriteFloatData) {
    myWriteAutoCorr = True;
  }

  myCalcUVW             = params.getBool   ("calcuvw");
  myWriteWeightSpectrum = params.getBool   ("weightspectrum");
  myCreateImagerColumns = params.getBool   ("imagercolumns");
  myWriteRowWise        = params.getBool   ("rowwise");
  bool useMultiFile     = params.getBool   ("multifile");
  bool useMultiHDF5     = params.getBool   ("multihdf5");
  myUseMultiFile = 0;
  if (useMultiFile) {
    myUseMultiFile = 1;
  } else if (useMultiHDF5) {
    myUseMultiFile = 2;
  }
  myMultiBlockSize      = params.getInt    ("mfsize");
  myWriteHDF5           = params.getBool   ("ashdf5");
  myUseAdios2           = params.getBool   ("useadios2");
  myFlagColumn          = params.getString ("flagcolumn");
  myNFlagBits           = params.getInt    ("nflagbits");
  // Get the station info from the given antenna table.
  uInt nant = params.getInt ("nant");
  myAntennaTableName = params.getString ("anttab");
  if (myAntennaTableName.empty()) {
    myAntPos.resize (3, nant);
    myAntPos = 0.;
  } else {
    Table tab(myAntennaTableName, TableLock(TableLock::AutoNoReadLocking));
    if (nant == 0  ||  nant >= tab.nrow()) {
      nant = tab.nrow();
    } else {
      Vector<uInt> rows(nant);
      indgen (rows);
      tab = tab(rows);
    }
    AlwaysAssert (nant>0, AipsError);
    ROArrayColumn<double> posCol(tab, "POSITION");
    posCol.getColumn (myAntPos);
  }
  return true;
}

void showParms()
{
  cout << " nms      = " << myNPart << "   " << myMsName << endl;
  cout << " nthread  = " << OMP::maxThreads() << endl;
  int nant = myAntPos.ncolumn();
  cout << " nant     = " << nant << "   (";
  if (myWriteFloatData) {
    cout << nant;
  } else if (myWriteAutoCorr) {
    cout << nant*(nant+1)/2;
  } else {
    cout << nant*(nant-1)/2;
  }
  cout << " baselines)" << endl;
  cout << " totalspw = " << myTotalNBand
       << "   (firstspw = " << myFirstBand << ')' << endl;
  cout << " nspw     = " << myNBand
       << "   (" << myNBand/myNPart << " per ms)" << endl;
  cout << " nfield   = " << myRa.size();
  if (myNTimeField > 0) {
    cout << "   (" << myNTimeField << "times per field)";
  }
  cout << endl;
  cout << " ntime    = " << myNTime << endl;
  cout << " nchan    = " << myNChan << endl;
  cout << " npol     = " << myNPol << endl;
  cout << " rowwise             = " << myWriteRowWise << endl;
  cout << " writeweightspectrum = " << myWriteWeightSpectrum << endl;
  cout << " createimagercolumns = " << myCreateImagerColumns << endl;
  if (!myFlagColumn.empty()  &&  myNFlagBits > 0) {
    cout << " FLAG written as " << myNFlagBits << " bits per flag" << endl;
  }
  IPosition tileShape = formTileShape(myTileSize, myTileSizePol, myTileSizeFreq,
                                      myWriteFloatData, myNPol, myNChan);
  int tileSize = tileShape.product() * 8;
  if (myWriteFloatData) {
    tileSize /= 2;
  }
  cout << " data tileshape      = " << tileShape
       <<   "  (tilesize = " << tileSize << " bytes)" << endl;
  if (!myWriteHDF5  &&  myUseMultiFile) {
    cout << " multi" << (myUseMultiFile==1 ? "file" : "hdf5");
    int multiBlockSize = myMultiBlockSize;
    if (multiBlockSize <= 0) {
      multiBlockSize = tileSize;
    }
    cout << "    (blocksize = " << multiBlockSize << " bytes)" << endl;
  }
}


String doOne (int seqnr, const String& msName)
{
  int nbpp = myNBand / myNPart;
  // Form the MS name.
  // If it contains %d, use that to fill in the seqnr.
  // Otherwise append _seqnr to the name (unless a single part is done).
  String name;
  if (msName.find ("%d") != String::npos) {
    name = String::format (msName.c_str(), seqnr);
  } else {
    name = msName;
    if (!myDoSinglePart) {
      name += String::format ("_p%d", seqnr);
    }
  }
  // Create the MS.
  Timer timer;
  CountedPtr<MSCreate> msmaker;
  if (myWriteHDF5) {
    msmaker = new MSCreateHDF5();
  } else {
    msmaker = new MSCreateCasa();
  }
  IPosition dataTileShape = formTileShape (myTileSize, myTileSizePol,
                                           myTileSizeFreq, myWriteFloatData,
                                           myNPol, myNChan);
  myTileSize = dataTileShape.product() * 8;
  if (myMultiBlockSize < 0) {
    myMultiBlockSize = myTileSize;
  }
  msmaker->init (myRa, myDec, myAntPos, myCalcUVW,
                 myWriteAutoCorr, myWriteFloatData, myWriteWeightSpectrum,
                 myCreateImagerColumns,
                 myNPol, myNChan, myStartFreq, myStepFreq,
                 myFirstBand+seqnr*nbpp, nbpp, myNTimeField,
                 myStartTime, myStepTime,
                 name, myAntennaTableName,
                 myNFlagBits, myFlagColumn, dataTileShape,
                 myUseMultiFile, myMultiBlockSize);
  // Close all subtables to reduce nr of open files.
  msmaker->closeSubTables();
  timer.show ("Created MS " + msName);
  timer.mark();
  for (int i=0; i<myNTime; ++i) {
    msmaker->writeTimeStep (myNTimeField, myWriteRowWise);
  }
  msmaker->flush();
  timer.show ("Wrote " + String::toString(msmaker->nrow()) + " rows into MS "
              + msName);
  if (seqnr == 0) {
    msmaker->showCacheStatistics();
  }
  return name;
}

void doAll()
{
  int nthread = OMP::maxThreads();
  Block<String> msnames(myNPart);
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
  for (int i=0; i<myNPart; ++i) {
    msnames[i] = doOne (i, myMsName);
  }
  if (myNPart == 1) {
    cout << "Created 1 MS part" << endl;
  } else {
    cout << "Created " << myNPart << " MS parts in "
         << nthread << " threads" << endl;
  }
  // Create the concatenated MS.
  if (!myDoSinglePart  &&  !myWriteHDF5) {
    Table tab = Table(msnames);
    tab.rename (myMsName, Table::New);
    cout << "Created MultiMS " << myMsName << " containing all parts" << endl;
  }
}

int main (int argc, char** argv)
{
#ifdef HAVE_MPI
    MPI_Init(0,0);
#endif
  try {
    if (readParms (argc, argv)) {
      showParms();
      doAll();
    }
  } catch (std::exception& ex) {
    cerr << "Unexpected exception in " << argv[0] << ": " << ex.what() << endl;
#ifdef HAVE_MPI
    MPI_Finalize();
#endif
    return 1;
  }
#ifdef HAVE_MPI
  MPI_Finalize();
#endif
  return 0;
}
