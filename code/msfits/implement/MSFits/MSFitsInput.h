//# MSFitsInput:  simple uvfits (random group) to MeasurementSet conversion
//# Copyright (C) 1996,1997,1998,1999,2000
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify
//# it under the terms of the GNU General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or
//# (at your option) any later version.
//#
//# This program is distributed in the hope that it will be useful,
//# but WITHOUT ANY WARRANTY; without even the implied warranty of
//# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//# GNU General Public License for more details.
//#
//# You should have received a copy of the GNU General Public License
//# along with this program; if not, write to the Free Software
//# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#if !defined(TRIAL_MSFITSINPUT_H)
#define TRIAL_MSFITSINPUT_H

#include <aips/aips.h>
#include <aips/Arrays/Matrix.h>
#include <aips/FITS/hdu.h>
#include <aips/MeasurementSets/MeasurementSet.h>
#include <aips/MeasurementSets/MSColumns.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Utilities/String.h>

class FitsInput;
template<class T> class PrimaryGroup;
class BinaryTable;

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
class MSPrimaryGroupHolder
{
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

  //# forwarding functions

  // Number of dimensions
  Int dims()
  {return hdu_p->dims();}

  // Length of i'th axis
  Int dim(Int i)
  {return hdu_p->dim(i);}

  // Coordinate type
  Char* ctype(Int i)
  { return pf ? pf->ctype(i) : (pl ? pl->ctype(i) : ps->ctype(i));}

  // Coordinate reference value
  Double crval(Int i)
  { return pf ? pf->crval(i) : (pl ? pl->crval(i) : ps->crval(i));}

  // Coordinate reference pixel
  Double crpix(Int i)
  { return pf ? pf->crpix(i) : (pl ? pl->crpix(i) : ps->crpix(i));}

  // Coordinate delta
  Double cdelt(Int i)
  { return pf ? pf->cdelt(i) : (pl ? pl->cdelt(i) : ps->cdelt(i));}

  // Keyword of given type
  const FitsKeyword* kw(FITS::ReservedName &n)
  { return hdu_p->kw(n);}

  // All keywords
  ConstFitsKeywordList &kwlist()
  { return hdu_p->kwlist();}

  // Advance to next keyword
  const FitsKeyword *nextkw()
  { return hdu_p->nextkw();}

  // Number of groups
  Int gcount() const
  { return pf ? pf->gcount() : ( pl ? pl->gcount() : ps->gcount());}

  // Number of parameters
  Int pcount() const
  { return pf ? pf->pcount() : ( pl ? pl->pcount() : ps->pcount());}

  // Parameter type
  Char* ptype(Int i) const
  { return pf ? pf->ptype(i) : ( pl ? pl->ptype(i) : ps->ptype(i));}

  // Read the next group
  Int read()
  { return pf ? pf->read() : ( pl ? pl->read() : ps->read());}

  // Get i'th parameter
  Double parm(Int i)
  { return pf ? pf->parm(i) : ( pl ? pl->parm(i) : ps->parm(i));}

  // Get group data with index i, scaled and converted to Double
  Double operator () (Int i) const
  { return pf ? (*pf)(i) : ( pl ? (*pl)(i) : (*ps)(i));}

private:
  HeaderDataUnit* hdu_p;
  PrimaryGroup<Short>* ps;
  PrimaryGroup<FitsLong>* pl;
  PrimaryGroup<Float>* pf;
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

// <todo asof="1999/08/16">
//   <li> So far we interpret AN, FQ and SU tables only
// </todo>

class MSFitsInput
{
  // This is an implementation helper class used to store 'local' data
  // during the filling process.
public:
  // Create from output and input file names
  MSFitsInput(const String& msFile, const String& fitsFile);

  ~MSFitsInput();

  // Has the filler been constructed ok? If false, do not use any other
  // member functions.
  Bool ok() {return ok_p;}

  // read the FITS file and create the MeasurementSet
  Bool readFitsFile();

protected:

  // Check that the input is a UV fits file with required contents.
  // Returns False if not ok.
  Bool checkInput(FitsInput& infile);

  // Read the axis info of the primary group, returns False if required axes
  // are missing.
  Bool getPrimaryGroupAxisInfo();

  // Set up the MeasurementSet, including StorageManagers and fixed columns.
  // If useTSM is True, the Tiled Storage Manager will be used to store
  // DATA, FLAG and WEIGHT_SPECTRUM
  void setupMeasurementSet(const String& MSFileName, Bool useTSM=True);

  // Fill the Observation and ObsLog tables
  void fillObsTables();

  // Fill the main table from the Primary group data
  void fillMSMainTable(Int& nField, Int& nSpW);

  // Read a binary table extension of type AIPS AN and create an antenna table
  void fillAntennaTable(BinaryTable& bt);

  // fill spectralwindow table from FITS FQ table + header info
  void fillSpectralWindowTable(BinaryTable& bt, Int nSpW);

  // fill spectralwindow table from header
  void fillSpectralWindowTable();

  // fill Field table from FITS SU table
  void fillFieldTable(BinaryTable& bt, Int nField);

  // fill Field table from header (single source fits)
  void fillFieldTable(Int nField);

  // fill the Feed table with minimal info needed for synthesis processing
  void fillFeedTable();

  // fix up the EPOCH MEASURE_REFERENCE keywords using the value found
  // in the (last) AN table
  void fixEpochReferences();

private:
  FitsInput* infile_p;
  String msFile_p;
  MSPrimaryGroupHolder priGroup_p;
  Bool ok_p;
  MeasurementSet ms_p;
  MSColumns* msc_p;
  Int nAxis_p, nIF_p;
  Vector<Int> nPixel_p,corrType_p;
  Block<Int> corrIndex_p;
  Matrix<Int> corrProduct_p;
  Vector<String> coordType_p;
  Vector<Double> refVal_p, refPix_p, delta_p;
  String array_p,object_p,timsys_p;
  Double epoch_p;
  Int nAnt_p;
  Vector<Double> receptorAngle_p;
  MFrequency::Types freqsys_p;
  Double restfreq_p;

};


#endif

