// # MSFitsInput:  simple uvfits (random group) to MeasurementSet conversion
// # Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This program is free software; you can redistribute it and/or modify
// # it under the terms of the GNU General Public License as published by
// # the Free Software Foundation; either version 2 of the License, or
// # (at your option) any later version.
// #
// # This program is distributed in the hope that it will be useful,
// # but WITHOUT ANY WARRANTY; without even the implied warranty of
// # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// # GNU General Public License for more details.
// #
// # You should have received a copy of the GNU General Public License
// # along with this program; if not, write to the Free Software
// # Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

#ifndef MS_MSFITSINPUT_H
#define MS_MSFITSINPUT_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/fits/FITS/fits.h>
#include <casacore/fits/FITS/hdu.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/ms/MeasurementSets/MSTileLayout.h>
#include <casacore/tables/Tables/BaseTable.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

class FitsInput;
class BinaryTable;
class MSColumns;
template <class T>
class ScalarColumn;

// <summary>
// A helper class for MSFitsInput
// </summary>
// <use visibility=local>
// <etymology>
// This class can hold a primary array of several datatypes
// </etymology>
// <synopsis>
// This is a helper class to avoid cumbersome switch statements on the
// template type of the primary array
// It forwards all the PrimaryArray member functions we need in the filler.
// </synopsis>
class MSPrimaryTableHolder {
  // This is a helper class to avoid cumbersome switch statements on the
  // template type of the primary array
  // It forwards all the PrimaryTable member function we need in the filler.
 public:
  // Construct an empty holder, used to attach to later
  MSPrimaryTableHolder();

  // Construct from an input file containing a FITS primary group hdu.
  // Throws an exception if the datatype is not Short, FitsLong or Float
  MSPrimaryTableHolder(FitsInput& infile);

  ~MSPrimaryTableHolder();

  // Attach to the input file, create the appropriate PrimaryArray.
  // Throws an exception if the datatype is not Short, FitsLong or Float
  void attach(FitsInput& infile);

  // Detach from the input file
  void detach();

  // # forwarding functions

  // Number of dimensions
  int dims() { return hdu_p->dims(); }

  // Length of i'th axis
  int dim(int i) { return hdu_p->dim(i); }

  // Coordinate type
  char* ctype(int i) { return pf ? pf->ctype(i) : (pl ? pl->ctype(i) : ps->ctype(i)); }

  // Coordinate reference value
  double crval(int i) { return pf ? pf->crval(i) : (pl ? pl->crval(i) : ps->crval(i)); }

  // Coordinate reference pixel
  double crpix(int i) { return pf ? pf->crpix(i) : (pl ? pl->crpix(i) : ps->crpix(i)); }

  // Coordinate delta
  double cdelt(int i) { return pf ? pf->cdelt(i) : (pl ? pl->cdelt(i) : ps->cdelt(i)); }

  // Keyword of given type
  const FitsKeyword* kw(const FITS::ReservedName& n) { return hdu_p->kw(n); }

  // All keywords
  ConstFitsKeywordList& kwlist() { return hdu_p->kwlist(); }

  // Advance to next keyword
  const FitsKeyword* nextkw() { return hdu_p->nextkw(); }

  // Read the next group
  int read() {
    if (pf)
      return pf->read();
    else if (pl)
      return pl->read();
    else if (ps)
      return ps->read();
    else if (pb)
      return pb->read();
    else
      cout << "can not read the table" << endl;
    return 0;
  }

 private:
  HeaderDataUnit* hdu_p;
  PrimaryTable<short>* ps;
  PrimaryTable<FitsLong>* pl;
  PrimaryTable<float>* pf;
  PrimaryTable<unsigned char>* pb;
};

// <summary>
// A helper class for MSFitsInput
// </summary>
// <use visibility=local>
// <etymology>
// This class can hold a primary group of several datatypes
// </etymology>
// <synopsis>
// This is a helper class to avoid cumbersome switch statements on the
// template type of the primary group
// It forwards all the PrimaryGroup member functions we need in the filler.
// </synopsis>
class MSPrimaryGroupHolder {
  // This is a helper class to avoid cumbersome switch statements on the
  // template type of the primary group
  // It forwards all the PrimaryGroup member function we need in the filler.
 public:
  // Construct an empty holder, used to attach to later
  MSPrimaryGroupHolder();

  // Construct from an input file containing a FITS primary group hdu.
  // Throws an exception if the datatype is not Short, FitsLong or Float
  MSPrimaryGroupHolder(FitsInput& infile);

  ~MSPrimaryGroupHolder();

  // Attach to the input file, create the appropriate PrimaryGroup.
  // Throws an exception if the datatype is not Short, FitsLong or Float
  void attach(FitsInput& infile);

  // Detach from the input file
  void detach();

  // # forwarding functions

  // Number of dimensions
  int dims() { return hdu_p->dims(); }

  // Length of i'th axis
  int dim(int i) { return hdu_p->dim(i); }

  // Coordinate type
  char* ctype(int i) { return pf ? pf->ctype(i) : (pl ? pl->ctype(i) : ps->ctype(i)); }

  // Coordinate reference value
  double crval(int i) { return pf ? pf->crval(i) : (pl ? pl->crval(i) : ps->crval(i)); }

  // Coordinate reference pixel
  double crpix(int i) { return pf ? pf->crpix(i) : (pl ? pl->crpix(i) : ps->crpix(i)); }

  // Coordinate delta
  double cdelt(int i) { return pf ? pf->cdelt(i) : (pl ? pl->cdelt(i) : ps->cdelt(i)); }

  // Keyword of given type
  const FitsKeyword* kw(const FITS::ReservedName& n) { return hdu_p->kw(n); }

  // All keywords
  ConstFitsKeywordList& kwlist() { return hdu_p->kwlist(); }

  // Advance to next keyword
  const FitsKeyword* nextkw() { return hdu_p->nextkw(); }

  // Number of groups
  int gcount() const { return pf ? pf->gcount() : (pl ? pl->gcount() : ps->gcount()); }

  // Number of parameters
  int pcount() const { return pf ? pf->pcount() : (pl ? pl->pcount() : ps->pcount()); }

  // Parameter type
  char* ptype(int i) const { return pf ? pf->ptype(i) : (pl ? pl->ptype(i) : ps->ptype(i)); }

  // Read the next group
  int read() { return pf ? pf->read() : (pl ? pl->read() : ps->read()); }

  // Get i'th parameter
  double parm(int i) { return pf ? pf->parm(i) : (pl ? pl->parm(i) : ps->parm(i)); }

  // Get group data with index i, scaled and converted to Double
  double operator()(int i) const { return pf ? (*pf)(i) : (pl ? (*pl)(i) : (*ps)(i)); }

 private:
  HeaderDataUnit* hdu_p;
  PrimaryGroup<short>* ps;
  PrimaryGroup<FitsLong>* pl;
  PrimaryGroup<float>* pf;
};

// <summary>
// UV FITS to MeasurementSet filler
// </summary>

// <use visibility=export>

// <prerequisite>
//   <li> MeasurementSet
//   <li> FITS classes
// </prerequisite>
//
// <etymology>
// MSFitsInput handles the conversion of FITS files to MeasurementSets
// </etymology>
//
// <synopsis>
// UV FITS to MeasurementSet filler. This can handle single source fits and
// multi source fits as written by classic AIPS. Also copes with multiple
// arrays (i.e. multiple AN tables) but doesn't correct for 5 day offsets
// introduced by DBCON.
// </synopsis>

class MSFitsInput {
  // This is an implementation helper class used to store 'local' data
  // during the filling process.
 public:
  MSFitsInput() = delete;

  // Create from output and input file names. This function opens the input
  // file, and checks the output file is writable.
  MSFitsInput(const String& msFile, const String& fitsFile, const bool NewNameStyle = false);

  MSFitsInput(const MSFitsInput& other) = delete;

  // The destructor is fairly trivial.
  ~MSFitsInput();

  MSFitsInput& operator=(const MSFitsInput& other) = delete;

  // Read all the data from the FITS file and create the MeasurementSet. Throws
  // an exception when it has severe trouble interpreting the FITS file.
  //
  void readFitsFile(int obsType = MSTileLayout::Standard);

 private:
  FitsInput* _infile;
  String _msFile;
  MSPrimaryGroupHolder _priGroup;
  MSPrimaryTableHolder _priTable;
  MeasurementSet _ms;
  MSColumns* _msc;
  int _nIF;
  Vector<int> _nPixel, _corrType;
  Block<int> _corrIndex;
  Matrix<int> _corrProduct;
  Vector<String> _coordType;
  Vector<double> _refVal, _refPix, _delta;
  String _array, _object, _timsys;
  double _epoch;
  MDirection::Types _epochRef;  // This is a direction measure reference code
                                // determined by epoch_p, hence the name and type.
  // unique antennas found in the visibility data
  // NOTE These are 1-based
  std::set<int> _uniqueAnts;
  // number of rows in the created MS ANTENNA table
  int _nAntRow;
  int _nArray;
  Vector<double> _receptorAngle;
  MFrequency::Types _freqsys;
  double _restfreq;  // used for images
  bool _addSourceTable;
  LogIO _log;
  Record _header;
  double _refFreq;
  bool _useAltrval;
  Vector<double> _chanFreq;
  bool _newNameStyle;
  Vector<double> _obsTime;

  Matrix<double> _restFreq;  // used for UVFITS
  Matrix<double> _sysVel;
  bool _msCreated;

  // Check that the input is a UV fits file with required contents.
  // Returns false if not ok.
  bool _checkInput(FitsInput& infile);

  // Read the axis info of the primary group, throws an exception if required
  // axes are missing.
  void getPrimaryGroupAxisInfo();

  // Set up the MeasurementSet, including StorageManagers and fixed columns.
  // If useTSM is true, the Tiled Storage Manager will be used to store
  // DATA, FLAG and WEIGHT_SPECTRUM. Use obsType to choose the tiling
  // scheme.
  void setupMeasurementSet(const String& MSFileName, bool useTSM = true,
                           int obsType = MSTileLayout::Standard);

  ///////////////fillers for primary table form uvfits//////////////////////
  // Read a binary table extension of type AIPS AN and create an antenna table
  void fillAntennaTable(BinaryTable& bt);

  // Read a binary table extension and update history table
  void fillHistoryTable(ConstFitsKeywordList& kwl);

  // Read a binary table extension and update history table
  void fillObservationTable(ConstFitsKeywordList& kwl);

  // extract axis information
  void getAxisInfo(ConstFitsKeywordList&);

  // extract axis information
  void sortPolarizations();

  void fillPolarizationTable();

  // verify that the fits contains visibility data
  void checkRequiredAxis();

  void fillSpectralWindowTable(BinaryTable& bt);

  // fill Field table
  void fillFieldTable(BinaryTable& bt);
  void fillFieldTable(double, double, String);

  void fillMSMainTable(BinaryTable& bt);

  void fillPointingTable();

  void fillSourceTable();

  // fill the Feed table with minimal info needed for synthesis processing
  void fillFeedTable();

  ///////////////fillers for primary table form uvfits//////////////////////
  // Fill the Observation and ObsLog tables
  void fillObsTables();

  // Fill the main table from the Primary group data
  // if we have enough memory try to do it in mem
  void fillMSMainTableColWise(int& nField, int& nSpW);
  // else do it row by row
  void fillMSMainTable(int& nField, int& nSpW);

  // fill spectralwindow table from FITS FQ table + header info
  void fillSpectralWindowTable(BinaryTable& bt, int nSpW);

  // fill spectralwindow table from header
  void fillSpectralWindowTable();

  // fill Field table from FITS SU table
  void fillFieldTable(BinaryTable& bt, int nField);

  // fill Field table from header (single source fits)
  void fillFieldTable(int nField);

  // fill the Pointing table (from Field table, all antennas are assumed
  // to point in the field direction) and possibly the Source table.
  void fillExtraTables();

  // fix up the EPOCH MEASURE_REFERENCE keywords using the value found
  // in the (last) AN table
  void fixEpochReferences();

  // Returns the Direction Measure reference for UVW and other appropriate columns
  // in msc_p (which must exist but have empty columns before you can set it!).
  MDirection::Types getDirectionFrame(double epoch);

  // Check the frame if there is an SU table
  void setFreqFrameVar(BinaryTable& binTab);

  // update a the Spectral window post filling if necessary
  void updateSpectralWindowTable();

  void readRandomGroupUVFits(int obsType);
  void readPrimaryTableUVFits(int obsType);

  std::pair<int, int> _extractAntennas(int antenna1, int antenna2);
  std::pair<int, int> _extractAntennas(float baseline);

  void _fillSysPowerTable(BinaryTable& bt);

  void _doFillSysPowerSingleIF(
      const String& casaTableName, const ScalarColumn<double>& timeCol,
      const ScalarColumn<float>& intervalCol, const ScalarColumn<int>& antNoCol,
      const ScalarColumn<int>& freqIDCol, const ScalarColumn<float>& powerDif1Col,
      const ScalarColumn<float>& powerSum1Col, const ScalarColumn<float>& postGain1Col,
      const ScalarColumn<float>& powerDif2Col, const ScalarColumn<float>& powerSum2Col,
      const ScalarColumn<float>& postGain2Col);
};

}  // namespace casacore

#endif
