//# MSSummary.h: Helper class for applications listing an image header
//# Copyright (C) 1996,1997,1998,1999,2000,2001
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
//#
#ifndef MS_MSSUMMARY_H
#define MS_MSSUMMARY_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/PtrHolder.h>
#include <casacore/ms/MeasurementSets/MSColumns.h>
#include <memory>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class MeasurementSet;
class LogIO;
class MSMetaData;

// <summary>Provides and lists information about the header of an image</summary>
// <use visibility=export>
// 
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
// 
// <prerequisite>
//   <li> <linkto class=MeasurementSet>MeasurementSet</linkto>
//   <li> <linkto module=Coordinates>Coordinates</linkto> 
// </prerequisite>
//
// <etymology>
// This class lists the ancilliary or header information from a
// MeasurementSet in a Summary format.
// </etymology>
//
// <synopsis>
// MSs consist of pixels and descriptive information stored in what
// is loosely termed the header. This is information describing the
// coordinate system, the image units etc.  This class enables you to
// retrieve the descriptive header information and/or list it.
// </synopsis>
//
// <example>
// <srcBlock>
//     PagedMS<Float> inMS(fileName);
//     MSSummary<Float> header(inMS);
//     LogOrigin or("myClass", "myFunction(...)", WHERE);
//     LogIO os(or);
//     header.list(os);
// </srcBlock>
// A <src>PagedMS</src> object is constructed and then logged to the 
// supplied <src>LogIO</src> object.
// </example>
//
// <note role=caution>
// Note that if the <src>PagedMS</src> goes out of scope, this
// class will retrieve rubbish as it just maintains a pointer
// to the image.
// </note>
//
// <motivation>
// The viewing of the image header is a basic capability that is
// commonly required.
// </motivation>
//
// <todo asof="1998/12/09">
//  There are various placeholders which will need to be activated for
//  Version 2 of the MeasurementSet definition.
// </todo>
 

class MSSummary
{
public:
// Constructor
   MSSummary (const MeasurementSet&);
   MSSummary (const MeasurementSet*);
   MSSummary (const MeasurementSet* ms, const String msname);

// Destructor
  ~MSSummary();

// Retrieve number of rows
   Int nrow() const;
 
// Retrieve image name
   String name() const;

// Set a new MS
   Bool setMS (const MeasurementSet& ms);

// List all header information.
   void list (LogIO& os, Bool verbose=False, Bool oneBased=True) const;
//Return some useful info in a record too along with os
   void list (LogIO& os, Record& outRec,  Bool verbose=False,
              Bool fillRecord=True, Bool oneBased=True) const;

// List a title for the Summary.
   void listTitle (LogIO& os) const;

// List convenient groupings of tables: list where MS obtained
// (Observation and Array tables)
   void listWhere (LogIO& os, Bool verbose=False) const;

// List what was observed (Field and Main tables)
   void listWhat (LogIO& os, Bool verbose=False) const;
   void listWhat (LogIO& os, Record& outRec,  Bool verbose=False,
                  Bool fillRecord=True) const;
// List how data were obtained (SpectralWindow, Feed, and Antenna tables)
   void listHow (LogIO& os, Bool verbose=False, Bool oneBased=True) const;

// List main table
   void listMain (LogIO& os, Bool verbose=False) const;
//Return some useful info in a record too along with os
   void listMain (LogIO& os, Record& outRec, Bool verbose=False,
                  Bool fillRecord=True) const;
   // Return a Record with information derived from the main table
   void getScanSummary (Record& outRec) const;

// List subtables
// <group>
   void listAntenna (LogIO& os, Bool verbose=False) const;
   void listFeed (LogIO& os, Bool verbose=False, Bool oneBased=True) const;
   void listField (LogIO& os, Bool verbose=False) const;
   void listField (LogIO& os, Record& outRec, Bool verbose=False,
		   Bool fillRecord=True) const;
   void listObservation (LogIO& os, Bool verbose=False) const;
   void listHistory (LogIO& os) const;
   void listPolarization (LogIO& os, Bool verbose=False) const;
   void listSource (LogIO& os, Bool verbose=False) const;
   void listSpectralWindow (LogIO& os, Bool verbose=False) const;
   void getSpectralWindowInfo(Record& outRec) const;
   void listSpectralAndPolInfo (LogIO& os, Bool verbose=False,
                                Bool oneBased=True) const;
   void listSysCal (LogIO& os, Bool verbose=False) const;
   void listWeather (LogIO& os, Bool verbose=False) const;
// </group>

// List table size summary
   void listTables (LogIO& os, Bool verbose=False) const;

   void setListUnflaggedRowCount(Bool v) { _listUnflaggedRowCount = v; }

   // set the cache size, in MB, for the MSMetaData object.
   void setMetaDataCacheSizeInMB(Float cacheSize) { _cacheSizeMB = cacheSize; }

private:
// Pointer to MS
   const MeasurementSet* pMS;
   SPtrHolder<MSMetaData> _msmd;

// Formatting strings
   const String dashlin1, dashlin2;

// Clear formatting flags
   void clearFlags (LogIO& os) const;

// For keeping track of the number of vis per field
   mutable Vector<Int> nVisPerField_;

   // Name of the MS used in the constructor
   String msname_p;

   Bool _listUnflaggedRowCount;

   Float _cacheSizeMB;
};


} //# NAMESPACE CASACORE - END

#endif
