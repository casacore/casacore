//# FITSIDItoMS.h: Convert a FITS-IDI binary table to an AIPS++ Table.
//# Copyright (C) 1995,1996,2000,2001
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
//# Mod 2010: DP

#ifndef MS_FITSIDITOMS_H
#define MS_FITSIDITOMS_H

#include <casacore/casa/aips.h>
#include <casacore/fits/FITS/hdu.h>
#include <casacore/tables/Tables/Table.h> //
#include <casacore/tables/Tables/TableDesc.h> //
#include <casacore/tables/Tables/TableRecord.h> //
#include <casacore/tables/Tables/TableColumn.h> //
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/casa/BasicSL/String.h>
#include <map>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MSColumns;
class FitsInput;


// <summary> 
// FITSIDItoMS converts a FITS-IDI file to a CASA Measurement Set
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> FitsInput
//   <li> HeaderDataUnit
//   <li> BinaryTableExtension
//   <li> Tables module
// </prerequisite>

// <etymology>

// FITSIDItoMS inherits from the FITS BinaryTableExtension class and
// its primary use is to convert such an object to a CASA Table.
// This explains it's use but not its name.  A better name should be
// found.

// </etymology>

// <synopsis> 
// The class starts with an already existing FitsInput object, which
// should be set at a BinaryTableExtension HDU.  Member functions
// provide a TableDesc appropriate for the FITS data (to help in
// constructing a CASA Table compatible with the
// BinaryTableExtension), a Table containing the current row of FITS
// data and a Table containing the next row of FITS data (which can be
// used to step through the FitsInput, copying each row using the
// RowCopier class), and a Table containin the entire FITS binary
// table from the current row to the end of the table.
// </synopsis> 

// <motivation>
// We need a way to get FITS-IDI data (typically from VLBI observations) into CASA.
// </motivation>

// <example>
// Open a FitsInput from a disk file, if the HDU is a
// BinaryTableExtension, then instantiate a MSBinaryTable object and
// get the entire table.  A fair amount of error checking has been
// eliminated from this example.
// <srcblock>
//    FitsInput infits("myFITSFile", FITS::Disk);
//    switch (infits.hdutype()) {
//       case FITS::BinaryTableHDU:
//          MSBinaryTable bintab(infits);
//          Table tab = bintab.fullTable("myTable");
//          break;
//    }
// </srcblock>
// There would obviously be other cases to the switch to deal with any
// other HDUs (e.g. skip them via infits.skip_hdu()).  The Table
// destructor would write "myTable" to disk.
// </example>

// <todo>
//
// </todo>

class FITSIDItoMS1 : public BinaryTableExtension
{
public: 

  //
  // The only constructor is from a FitsInput.  The correlat string
  // should be set to the correlator name/type as contained in the
  // CORRELAT keyword from the FITS-IDI primary header.  If it is set
  // to "DIFX" (case-sensitive) additional digital corrections
  // appropriate for the DiFX software correlator will be applied.
  // Other valid values include "VLBA" (for the original VLBA hardware
  // correlator; currently unsupported) and "SFXC" (for the SFXC
  // software correlator used by the EVN).
  //

  FITSIDItoMS1(FitsInput& in, const String& correlat,
	       const int32_t& obsType=0, const bool& initFirstMain=true,
	       const float& vanVleck=0.0, const int32_t& zeroPad=0);

  ~FITSIDItoMS1();
  
  //
  // Get the full table, using the supplied arguments to construct
  // the table.  The table will contain all data from the current
  // row to the end of the BinaryTableExtension.
  //
  
  Table oldfullTable(const String& tabName);
  
  
  // Fill the Observation and ObsLog tables
  void fillObsTables();
  
  // Read a binary table extension of type ANTENNA and create an antenna table
  //void fillAntennaTable(BinaryTable& bt);
  void fillAntennaTable();
  
  // fill the Feed table with minimal info needed for synthesis processing
  void fillFeedTable();
  
  //fill the Field table
  //void fillFieldTable(int32_t nField);
  void fillFieldTable();
  
  //fill the Spectral Window table with the content of FREQUENCY
  void fillSpectralWindowTable();
  
  //fill the optional Correlator Model table with the content of INTERFEROMETER_MODEL
  bool fillCorrelatorModelTable();

  //fill the optional SysCal table with the content of SYSTEM_TEMPERATURE
  bool fillSysCalTable();

  //fill the optional FlagCmd table with the content of FLAG
  bool fillFlagCmdTable();

  //fill the optional Weather table with the content of WEATHER
  bool fillWeatherTable();

  //store the information from the GAIN_CURVE table in a calibration table
  bool handleGainCurve();

  //store the information from the PHASE-CAL table in a calibration table
  bool handlePhaseCal();

  //store the information from the MODEL_COMPS table 
  bool handleModelComps();

  // fix up the EPOCH MEASURE_REFERENCE keywords
  void fixEpochReferences();
  
  //update the Polarization table
  void updateTables(const String& tabName);
  
  
  //
  // Get an appropriate TableDesc (this is the same TableDesc used
  // to construct any Table objects returned by this class.
  //
  const TableDesc& getDescriptor();
  
  //
  // Return the Table keywords (this is the same TableRecord used in
  // any Table objects returned by this class.
  //
  TableRecord& getKeywords();
  
  //
  // Get a Table with a single row, the current row of the FITS
  // table.  The returned Table is a Scratch table.  The standard
  // BinaryTableExtension manipulation functions are available to
  // position the FITS input at the desired location.
  //
  const Table &thisRow();
  
  //
  // Get a Table with a single row, the next row of the FITS table.
  // The returned Table is a Scratch table.  The FITS input is
  // positioned to the next row and the values translated and
  // returned in a Table object.
  //
  const Table &nextRow();
  
  // Get the version of the archived MS. 
  float msVersion() const
  { return itsVersion; }
  
  // Read all the data from the FITS file and create the MeasurementSet. Throws
  // an exception when it has severe trouble interpreting the FITS file.
  // Returns false if it encounters an unsupported extension.
  bool readFitsFile(const String& msFile);
  
  //is this the first UV_DATA extension
  bool isfirstMain(){return firstMain;}
  
protected:
  // Read the axis info, throws an exception if required axes are missing.
  void getAxisInfo();
  
  // Set up the MeasurementSet, including StorageManagers and fixed columns.
  // If useTSM is true, the Tiled Storage Manager will be used to store
  // DATA, FLAG and WEIGHT_SPECTRUM
  void setupMeasurementSet(const String& MSFileName, bool useTSM=true, 
			   bool mainTbl=false, bool addCorrMod=false,
			   bool addSyscal=false, bool addWeather=false,
			   bool addGainCurve=false, bool addPhaseCal=false);
  
  // Fill the main table from the Primary group data
  void fillMSMainTable(const String& MSFileName, int32_t& nField, int32_t& nSpW);
  
 private:
  //
  //# Data Members
  //
  
  // The scratch table containing the current row
  Table itsCurRowTab;
  
  // The number of elements for each column of the
  // BinaryTableExtension
  Vector<int32_t> itsNelem;
  
  // For each column: is it an array?
  Vector<bool> itsIsArray; 
  
  // Table keyword set
  TableRecord itsKwSet;
  
  // Table descriptor for construction
  TableDesc itsTableDesc;
  
  // Table info
  TableInfo itsTableInfo;
  
  // The MS version.
  float itsVersion;
  
  //
  // Buffer for storing the MSK's, MS-specific FITS keywords.
  //
  uint32_t itsNrMSKs;
  Vector<String> itsMSKC;
  Vector<String> itsMSKN;
  Vector<String> itsMSKV;
  Vector<bool>   itsgotMSK;
  
  
  //# FitsInput &infile_p;
  String msFile_p;
  Vector<int32_t> nPixel_p,corrType_p;
  Block<int32_t> corrIndex_p, corrSwapIndex_p;
  Matrix<int32_t> corrProduct_p;
  Vector<String> coordType_p;
  Vector<double> refVal_p, refPix_p, delta_p; 
  static String array_p;
  String object_p,timsys_p;
  double epoch_p;
  static double rdate;
  int32_t nAnt_p;
  Vector<double> receptorAngle_p;
  MFrequency::Types freqsys_p;
  double restfreq_p;
  LogIO* itsLog;
  //# int32_t nIF_p;
  double startTime_p;
  double lastTime_p;
  int32_t itsObsType;
  String itsCorrelat;
  float itsVanVleck;
  int32_t itsZeroPad;
  MeasurementSet ms_p;
  MSColumns* msc_p;
  static bool firstMain;
  static bool firstSyscal;
  static bool firstWeather;
  static bool firstGainCurve;
  static bool firstPhaseCal;
  bool weather_hasWater_p;
  bool weather_hasElectron_p;
  bool uv_data_hasWeights_p;
  bool weightypKwPresent_p;
  String weightyp_p;
  int32_t nStokes_p;
  int32_t nBand_p;
  static std::map<int32_t,int32_t> antIdFromNo;
  static std::map<int32_t,int32_t> digiLevels;
  static Vector<double> effChBw;

  //
  //# Member Functions
  //
  
  // Fill in each row as needed
  void fillRow();
  
  // Build part of the keywords of the itsCurRowTab
  void convertKeywords();
  
  // Convert FITS field descriptions to TableColumn descriptions.
  void describeColumns();
  
  // Convert the MS-specific keywords in the FITS binary table.
  void convertMSKeywords();
};
 

} //# NAMESPACE CASACORE - END

#endif


