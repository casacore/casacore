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
//#
//# $Id: readms.cc 21451 2014-06-10 07:48:08Z gervandiepen $

//# Includes

#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/tables/Tables/TableIter.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/OS/Timer.h>

using namespace casacore;


// Define the global variables shared between the main functions.
bool   myReadWeightSpectrum;
bool   myReadData;
bool   myReadFlag;
bool   myReadRowWise;
bool   myDoSinglePart;
int    myNPart;
int    myNPol;
int    myNChan;
int    myNTime;
Vector<int> myField;
Vector<int> mySpw;
String myMsName;
String myBaselines;


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
  params.version("2017Oct14GvD");
  params.create ("nms", "0",
                 "Number of MeasurementSets to read (0 means 1 MS without suffix)",
                 "int");
  params.create ("msname", "",
                 "Name of the MeasurementSet (suffix _p<i> is added if nms>0)",
                 "string");
  params.create ("data", "true",
                 "Read DATA column?",
                 "bool");
  params.create ("flag", "true",
                 "Read FLAG column?",
                 "bool");
  params.create ("weightspectrum", "false",
                 "Read WEIGHT_SPECTRUM column?",
                 "bool");
  params.create ("fields", "",
                 "Fields to read ",
                 "int vector");
  params.create ("ntime", "0",
                 "Number of time steps to read (0=all)",
                 "int");
  params.create ("spw", "",
                 "Spectral window numbers to read",
                 "int vector");
  params.create ("npol", "0",
                 "Number of polarizations to read (0=all)",
                 "int");
  params.create ("nchan", "0",
                 "Number of channels to read (0=all)",
                 "int");
  params.create ("baselines", "",
                 "Baselines to read (CASA baseline selection)",
                 "string");
  params.create ("rowwise", "true",
                 "Read the data row wise (thus a get per row)",
                 "bool");
  // Fill the input structure from the command line.
  params.readArguments (argc, argv);
  // Get the various parameters.
  myMsName = params.getString ("msname");
  if (myMsName.empty()) {
    showHelp();
    return false;
  }
  myNChan = params.getInt ("nchan");
  myNPol  = params.getInt ("npol");
  myNTime = params.getInt ("ntime");
  mySpw   = Vector<Int>(params.getIntArray ("spw"));
  myField = Vector<Int>(params.getIntArray ("fields"));
  myNPart = params.getInt ("nms");
  myDoSinglePart = (myNPart == 0);
  if (myDoSinglePart) {
    myNPart = 1;
  }
  // Determine nr of bands per part (i.e., ms).
  if (myNPol  == 0) myNPol  = 4;
  if (myNChan == 0) myNChan = 32768 * 32768;    // very high number
  if (myNTime == 0) myNTime = 32768 * 32768;
  // Get remaining parameters.
  myBaselines          = params.getString ("baselines");
  myReadData           = params.getBool   ("data");
  myReadFlag           = params.getBool   ("flag");
  myReadWeightSpectrum = params.getBool   ("weightspectrum");
  myReadRowWise        = params.getBool   ("rowwise");
  return true;
}

void showParms()
{
  cout << "readms parameters:" << endl;
  cout << " nms    = " << myNPart << "   " << myMsName << endl;
  /*
  int nant = myAntPos.ncolumn();
  cout << " nant   = " << nant << "   (nbaseline = ";
  if (myWriteAutoCorr) {
    cout << nant*(nant+1) / 2;
  } else {
    cout << nant*(nant-1) / 2;
  }
  cout << ')' << endl;
  cout << " nspw   = " << myNBand
       << "   (" << myNBand/myNPart << " per ms)" << endl;
  cout << " nfield = " << myRa.size();
  if (myNTimeField > 0) {
    cout << "   (" << myNTimeField << "times per field)";
  }
  cout << endl;
  cout << " ntime  = " << myNTime << endl;
  cout << " nchan  = " << myNChan << endl;
  cout << " npol   = " << myNPol << endl;
  cout << " rowwise             = " << myWriteRowWise << endl;
  cout << " writeweightspectrum = " << myWriteWeightSpectrum << endl;
  cout << " createimagercolumns = " << myCreateImagerColumns << endl;
  if (!myFlagColumn.empty()  &&  myNFlagBits > 0) {
    cout << " FLAG written as " << myNFlagBits << " bits per flag" << endl;
  }
  IPosition tileShape = formTileShape(myTileSize, myTileSizeFreq,
                                      myNPol, myNChan);
  int tileSize = tileShape.product() * 8 / 1024;
  cout << " tilesize = " << tileSize << " KBytes" << endl;
  if (!myWriteHDF5  &&  myUseMultiFile) {
    cout << " multi" << (myUseMultiFile==1 ? "file" : "hdf5");
    int multiBlockSize = myMultiBlockSize;
    if (multiBlockSize <= 0) {
      multiBlockSize = tileSize;
    }
    cout << "    (blocksize = " << multiBlockSize << " KBytes)" << endl;
  }
  */
}

Int64 readTimeSteps (MeasurementSet& ms)
{
  Int64 nrow = 0;
  Block<String> itercol(2);
  itercol[0] = "TIME";
  itercol[1] = "DATA_DESC_ID";
  TableIterator iter(ms, itercol, TableIterator::Ascending,
                     TableIterator::NoSort);
  int ntime = 0;
  while (!iter.pastEnd()  &&  ntime < myNTime) {
    Table tab (iter.table());
    if (myReadData) {
      ArrayColumn<Complex>(tab, "DATA").getColumn();
    }
    if (myReadFlag) {
      ArrayColumn<Bool>(tab, "FLAG").getColumn();
    }
    if (myReadWeightSpectrum) {
      ArrayColumn<Float>(tab, "WEIGHT_SPECTRUM").getColumn();
    }
    nrow += tab.nrow();
    iter.next();
    ntime++;
  }
  return nrow;
}

void doOne (int seqnr, const String& msName)
{
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
  // Open the MS.
  Timer timer;
  MS ms(name);
  timer.show ("Opened MS " + name);
  timer.mark();
  Int64 nrow = readTimeSteps (ms);
  timer.show ("Read " + String::toString(nrow) + " rows from MS "
              + msName);
  //if (seqnr == 0) {
  //msmaker->showCacheStatistics();
  //}
}

void doAll()
{
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
  for (int i=0; i<myNPart; ++i) {
    doOne (i, myMsName);
  }
  if (myNPart == 1) {
    cout << "Read 1 MS part" << endl;
  } else {
    cout << "Read " << myNPart << " MS parts" << endl;
  }
}

int main (int argc, char* argv[])
{
  try {
    if (readParms (argc, argv)) {
      showParms();
      doAll();
    }
  } catch (const std::exception& x) {
    std::cerr << x.what() << std::endl;
    return 1;
  } 
  return 0;
}
