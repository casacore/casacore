//# MSFitsIDI.cc: Implementation of MSFitsIDI.h
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002
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
//# $Id: 
//----------------------------------------------------------------------------

#include <msfits/MSFits/MSFitsIDI.h>
#include <msfits/MSFits/FitsIDItoMS.h>
#include <fits/FITS/fitsio.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/Utilities/Regex.h>
#include <casa/Logging/LogIO.h>
#include <casa/OS/File.h>
#include <casa/OS/Directory.h>
#include <casa/IO/TapeIO.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//----------------------------------------------------------------------------

MSFitsIDI::MSFitsIDI(const Path& tapeDevice, const String& msOut, 
		     const Bool& overWrite) :
  itsDataSource(""),
  itsDeviceType(FITS::Tape9),
  itsMSOut(""),
  itsMS(0),
  itsMSExists(False),
  itsOverWrite(False),
  itsSelectedFiles(0),
  itsAllFilesSelected(True)
{
// Construct from a tape device and output MS file name
// Input:
//    tapeDevice           const String&      Tape device name
//    msOut                const String&      Output MS name
//    overWrite            const Bool&        True if existing MS is to 
//                                            be overwritten
// Output to private data:
//    itsDataSource        String             Tape name or input file name
//    itsDeviceType        FITS::DeviceType   FITS device type (disk or tape)
//    itsMSOut             String             Output MS name
//    itsMS                MeasurementSet*    Pointer to output MS
//    itsMSExists          Bool               True if output MS already exists
//    itsOverWrite         Bool               True if existing MS is to 
//                                            be overwritten
//    itsSelectedFiles     Vector<Int>        Input file numbers selected
//    itsAllFilesSelected  Bool               True if all files selected
//
  init(tapeDevice.absoluteName(), FITS::Tape9, msOut, overWrite);
//
};

//----------------------------------------------------------------------------

MSFitsIDI::MSFitsIDI(const String& inFile, const String& msOut, 
		     const Bool& overWrite) :
  itsDataSource(""),
  itsDeviceType(FITS::Disk),
  itsMSOut(""),
  itsMS(0),
  itsMSExists(False),
  itsOverWrite(False),
  itsSelectedFiles(0),
  itsAllFilesSelected(True)
{
// Construct from an input FITS-IDI file name and an output MS file name
// Input:
//    inFile               const String&      Input FITS-IDI file name
//    msOut                const String&      Output MS name
//    overWrite            const Bool&        True if existing MS is to 
//                                            be overwritten
// Output to private data:
//    itsDataSource        String             Tape name or input file name
//    itsDeviceType        FITS::DeviceType   FITS device type (disk or tape)
//    itsMSOut             String             Output MS name
//    itsMS                MeasurementSet*    Pointer to output MS
//    itsMSExists          Bool               True if output MS already exists
//    itsOverWrite         Bool               True if existing MS is to 
//                                            be overwritten
//    itsSelectedFiles     Vector<Int>        Input file numbers selected
//    itsAllFilesSelected  Bool               True if all files selected
//
  init(inFile, FITS::Disk, msOut, overWrite);
//
};

//----------------------------------------------------------------------------

MSFitsIDI::~MSFitsIDI()
{
// Default desctructor
// Output to private data:
//    itsMS                MeasurementSet*    Pointer to output MS
//
  if (itsMS) {
    delete (itsMS);
  };
};

//----------------------------------------------------------------------------

void MSFitsIDI::selectFiles(const Vector<Int>& files)
{
// Select input tape files by number (1-relative)
// Input:
//    files                const Vector<Int>  List of selected file numbers
// Output to private data:
//    itsSelectedFiles     Vector<Int>        Input file numbers selected
//    itsAllFilesSelected  Bool               True if all files selected
//
  itsSelectedFiles.resize(files.nelements());
  itsSelectedFiles = files;
  if (itsSelectedFiles.nelements() > 0) {
    itsAllFilesSelected = False;
  };
};

//----------------------------------------------------------------------------

Bool MSFitsIDI::fillMS()
{
// Convert the FITS-IDI data to MS format
//
  LogIO os(LogOrigin("MSFitsIDI", "fillMS()", WHERE));
  
  // Delete the MS if it already exits and overwrite selected
  if (itsMSExists && itsOverWrite) {
    Table::deleteTable(itsMSOut);
  };

  // Create a new MS or attach to the existing MS
  if (!itsMSExists || itsOverWrite) {
    createOutputMS();
  } else {
    //    itsMS = new MeasurementSet(itsMSOut);
  };

  //
  // Tape input: loop over all selected input files
  //
  Bool atEnd = False;
  if (itsDeviceType == FITS::Tape9) {
    Int fileIndex = 0;
    Int currentFile = 1;
    Int fileno = currentFile;

    while (!atEnd) {
      // Skip to next file selected
      if (itsAllFilesSelected) {
	fileno = currentFile;
      } else {
	atEnd = (fileIndex >= itsSelectedFiles.nelements()-1);
	if (!atEnd) fileno = itsSelectedFiles(fileIndex);
      };

      if (!atEnd) {
	// Advance tape if necessary
	Int nskip = fileno - currentFile;
	if (nskip > 0) {
	  TapeIO tapeDev(itsDataSource);
	  tapeDev.skip(nskip);
	  currentFile = currentFile + nskip;
	};

	// Read and process the selected input file
	readFITSFile(atEnd);

	// Increment file counter
	currentFile = currentFile + 1;
      };
    }; 
      
    //
    // Disk input:
    //
  } else if (itsDeviceType == FITS::Disk) {
    readFITSFile(atEnd);
  };
  return True;
};

//----------------------------------------------------------------------------

void MSFitsIDI::init(const String& dataSource, 
		     const FITS::FitsDevice& deviceType, const String& msOut,
		     const Bool& overWrite) 
{
// Initialization (called by all constructors)
// Input:
//    dataSource    const String&            Input file name or tape device
//    deviceType    const FITS::FitsDevice   FITS device type (tape or disk)
//    msOut         const String&            Output MS name
//    overWrite     const Bool&              True if existing MS is to 
//                                           be overwritten
// Output to private data:
//    itsDataSource        String             Tape name or input file name
//    itsDeviceType        FITS::DeviceType   FITS device type (disk or tape)
//    itsMSOut             String             Output MS name
//    itsMS                MeasurementSet*    Pointer to output MS
//    itsMSExists          Bool               True if output MS already exists
//    itsOverWrite         Bool               True if existing MS is to 
//                                            be overwritten
//    itsSelectedFiles     Vector<Int>        Input file numbers selected
//    itsAllFilesSelected  Bool               True if all files selected
//
  LogIO os(LogOrigin("MSFitsIDI", "init()", WHERE));
  
  // Check for valid FITS-IDI data source
  Path sourcePath(dataSource);
  if (!sourcePath.isValid() || !File(sourcePath).exists() || 
      !File(sourcePath).isReadable()) {
    os << LogIO::SEVERE << "FITS-IDI data source is not readable"
       << LogIO::EXCEPTION;
  };

  itsDataSource = sourcePath.absoluteName();
  itsDeviceType = deviceType;

  // Check for valid output MS specification
  Path msPath(msOut);
  itsMSExists = File(msPath).exists();

  if (itsMSExists && !File(msPath).isWritable()) {
    os << LogIO::SEVERE << "Output MS is not writable" << LogIO::EXCEPTION;
  };

  if (!itsMSExists && !File(msPath).canCreate()) {
    os << LogIO::SEVERE << "Output MS cannot be created" << LogIO::EXCEPTION;
  };
  itsMSOut = msOut;
  itsOverWrite = overWrite;

  // Set remaining default parameters
  itsAllFilesSelected = True;
};

//----------------------------------------------------------------------------

void MSFitsIDI::readFITSFile(Bool& atEnd)
{
// Read and process the current FITS-IDI input file (on tape or disk)
// Output:
//    atEnd                Bool               True if at EOF
//
  LogIO os(LogOrigin("MSFitsIDI", "readFITSFile()", WHERE));
  atEnd = False;

  // Construct a FitsInput object
  FitsInput infits(itsDataSource.chars(), itsDeviceType);
  if (infits.err() != FitsIO::OK) {
    os << LogIO::SEVERE << "Error reading FITS input" << LogIO::EXCEPTION;
  };

  // Regular expression for trailing blanks
  Regex trailing(" *$");

  // Create a temporary work directory for the sub-tables
  Directory tmpDir(itsMSOut + "_tmp");
  tmpDir.create();

  // Vector of sub-table names
  Vector<String> subTableName;
  Int subTableNr = -1;
  Table maintab;
  
  // Loop over all HDU in the FITS-IDI file
  while (infits.err() == FitsIO::OK && !infits.eof()) {

    // Skip non-binary table HDU's
    if (infits.hdutype() != FITS::BinaryTableHDU) {
      os << LogIO::WARN << "Skipping non-binary table HDU" << LogIO::POST;
      infits.skip_hdu();

    } else if (infits.rectype() == FITS::SpecialRecord) {
      os << LogIO::WARN << "Skipping FITS special record" << LogIO::POST;
      infits.read_sp();

    } else {
      // Process the FITS-IDI input from the position of this binary table
      FITSIDItoMS1 bintab(infits);
      String hduName = bintab.extname();
      hduName = hduName.before(trailing);
      String tableName = itsMSOut;
      if (hduName != "") {
	if (hduName != "UV_DATA") {
	  tableName = tableName + "_tmp/" + hduName;
	  subTableNr++;
	  subTableName.resize(subTableNr+1, True);
	  subTableName(subTableNr) = hduName;
	};

	// Process the FITS-IDI input
	bintab.readFitsFile(tableName);
	if (infits.err() != FitsIO::OK) {
	  os << LogIO::SEVERE << "Error reading FITS input" 
	     << LogIO::EXCEPTION;
	};
      };
    };
  }; // end while

  // Move the subtables in the proper place and add the subtable
  // references to the main table description.
  //
  cout << "Subtables found: " << subTableName << endl;
  // Open the main table to be updated.
  Table msmain (itsMSOut, Table::Update);
  // Loop over all subtables.
  cout << "Nr of subtables = " << subTableNr+1 << endl;
  for (Int isub=0; isub<=subTableNr; isub++) {
    //cout << "renaming subtable " << subTableName(isub) << endl;
    // Open the subtable to be updated.
    if (subTableName(isub)=="ARRAY_GEOMETRY") {
      Table mssub(itsMSOut+"_tmp/"+subTableName(isub)+"/ANTENNA",Table::Update);
      // Rename the subtable.
      mssub.rename (itsMSOut+"/ANTENNA",Table::Update);
      // Attach the subtable to the main table.
      msmain.rwKeywordSet().defineTable("ANTENNA",mssub);
    }
    if (subTableName(isub)=="SOURCE") {
      Table mssub(itsMSOut+"_tmp/"+subTableName(isub)+"/FIELD",Table::Update);
      mssub.rename (itsMSOut+"/FIELD",Table::Update);
      msmain.rwKeywordSet().defineTable("FIELD",mssub);
    }
    if (subTableName(isub)=="FREQUENCY") {
      Table mssub(itsMSOut+"_tmp/"+subTableName(isub)+"/SPECTRAL_WINDOW",Table::Update);
      mssub.rename (itsMSOut+"/SPECTRAL_WINDOW",Table::Update);
      msmain.rwKeywordSet().defineTable("SPECTRAL_WINDOW",mssub);
      
      Table mssub2(itsMSOut+"_tmp/"+subTableName(isub)+"/DATA_DESCRIPTION",Table::Update);
      mssub2.rename (itsMSOut+"/DATA_DESCRIPTION",Table::Update);
      msmain.rwKeywordSet().defineTable("DATA_DESCRIPTION",mssub2);

      Table mssub3(itsMSOut+"_tmp/"+subTableName(isub)+"/POLARIZATION",Table::Update);
      mssub3.rename (itsMSOut+"/POLARIZATION",Table::Update);
      msmain.rwKeywordSet().defineTable("POLARIZATION",mssub3);
      
    }
    if (subTableName(isub)=="ANTENNA") {
      Table mssub(itsMSOut+"_tmp/"+subTableName(isub)+"/FEED",Table::Update);
      mssub.rename (itsMSOut+"/FEED",Table::Update);
      msmain.rwKeywordSet().defineTable("FEED",mssub);
    }
    if (subTableName(isub)=="POINTING_DATA") {
      Table mssub(itsMSOut+"_tmp/"+subTableName(isub)+"/POINTING",Table::Update);
      mssub.rename (itsMSOut+"/POINTING",Table::Update);
      msmain.rwKeywordSet().defineTable("POINTING",mssub);
    }
    
  }
  //tmpDir.remove();
  //commentwas here
};
  
//----------------------------------------------------------------------------

void MSFitsIDI::createOutputMS()
{
// Create a new, empty output MS
//
  LogIO os(LogOrigin("MSFitsIDI", "createOutputMS()", WHERE));
};

//----------------------------------------------------------------------------



} //# NAMESPACE CASA - END

