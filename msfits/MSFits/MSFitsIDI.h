//# MSFitsIDI.h: Convert FITS-IDI data to MS format
//# Copyright (C) 1996,1997,1998,1999,2001
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
//#
//# $Id$

#ifndef MS_MSFITSIDI_H
#define MS_MSFITSIDI_H

#include <casacore/casa/aips.h>
#include <casacore/fits/FITS/fits.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> 
// MSFitsIDI: Convert FITS-IDI data to MS format
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="" demos="">

// <prerequisite>
//   <li> <linkto class="MeasurementSet">MeasurementSet</linkto> module
// </prerequisite>
//
// <etymology>
// From "MS" and "FITS-IDI
// </etymology>
//
// <synopsis>
// The MSFitsIDI class converts FITS-IDI data, on tape or disk,
// to MeasurementSet (MS) format.
// </etymology>
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// Encapsulate all FITS-IDI to MS conversion capabilities.
// </motivation>
//
// <todo asof="01/03/15">
// (i) General input filtering.
// (ii) VLBA digital correlator corrections
// (iii) Convert all sub-tables
// </todo>

class MSFitsIDI
{
 public:
  // Construct from a tape device name and MS output file name
  MSFitsIDI(const Path& tapeDevice, const String& msOut, 
	    const Bool& overWrite, const Int& obsType=0);

  // Construct from an input file name and an MS output file name
  MSFitsIDI(const String& inFile, const String& msOut, 
	    const Bool& overWrite, const Int& obsType=0);

  // Destructor
  ~MSFitsIDI();
  
  // Set which files are selected (1-rel; for tape-based data)
  void selectFiles(const Vector<Int>& files);

  // Convert the FITS-IDI data to MS format
  Bool fillMS();

 protected:
  // Initialization (called by all constructors)
  void init(const String& dataSource, const FITS::FitsDevice& deviceType,
	    const String& msOut, const Bool& overWrite, const Int& obsType);

  // Read and process a FITS-IDI file
  void readFITSFile(Bool& atEnd);

 private:
  // Data source and device type
  String itsDataSource;
  FITS::FitsDevice itsDeviceType;

  // MS, status and write options
  String itsMSOut;
  Bool itsMSExists;
  Int itsObsType;  // 0=standard, 1=fastmosaic, requiring small tiles in the measurement set

  // Selected file numbers (1-relative)
  Vector<Int> itsSelectedFiles;
  Bool itsAllFilesSelected;

};


} //# NAMESPACE CASACORE - END

#endif
   
