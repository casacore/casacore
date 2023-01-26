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
//     PagedMS<float> inMS(fileName);
//     MSSummary<float> header(inMS);
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
// <group>
// <src>maxCacheMB</src> is the maximum cache size in MB to use for the created
// MSMetaData object.
   MSSummary (const MeasurementSet& ms, float maxCacheMB = 50.0);
   MSSummary (const MeasurementSet* ms, float maxCacheMB = 50.0);
   MSSummary (const MeasurementSet* ms, const String msname, float maxCacheMB = 50.0);

   // construct the object using an MSMetaDataObject
   MSSummary (std::shared_ptr<MSMetaData> msmd);

// Destructor
  ~MSSummary();

// Retrieve number of rows
   int64_t nrow() const;
 
// Retrieve image name
   String name() const;

// Set a new MS. <src>maxCacheMB</src> is the maximum cache size of the
// created MSMetaData tool. If negative, the cache size used when this object
// was created is used.
   bool setMS (const MeasurementSet& ms, float maxCacheMB=-1);

// List all header information.
   void list (LogIO& os, bool verbose=false, bool oneBased=true) const;
//Return some useful info in a record too along with os
   void list (LogIO& os, Record& outRec,  bool verbose=false,
              bool fillRecord=true, bool oneBased=true) const;

// List a title for the Summary.
   void listTitle (LogIO& os) const;

// List convenient groupings of tables: list where MS obtained
// (Observation and Array tables)
   void listWhere (LogIO& os, bool verbose=false) const;

// List what was observed (Field and Main tables)
   void listWhat (LogIO& os, bool verbose=false) const;
   void listWhat (LogIO& os, Record& outRec,  bool verbose=false,
                  bool fillRecord=true) const;
// List how data were obtained (SpectralWindow, Feed, and Antenna tables)
   void listHow (LogIO& os, bool verbose=false, bool oneBased=true) const;

// List main table
   void listMain (LogIO& os, bool verbose=false) const;
//Return some useful info in a record too along with os
   void listMain (LogIO& os, Record& outRec, bool verbose=false,
                  bool fillRecord=true) const;
   // Return a Record with information derived from the main table
   void getScanSummary (Record& outRec) const;

// List subtables
// <group>
   void listAntenna (LogIO& os, bool verbose=false) const;
   void listFeed (LogIO& os, bool verbose=false, bool oneBased=true) const;
   void listField (LogIO& os, bool verbose=false) const;
   void listField (LogIO& os, Record& outRec, bool verbose=false,
           bool fillRecord=true) const;
   void listObservation (LogIO& os, bool verbose=false) const;
   void listHistory (LogIO& os) const;
   void listPolarization (LogIO& os, bool verbose=false) const;
   void listSource (LogIO& os, bool verbose=false) const;
   void listSpectralWindow (LogIO& os, bool verbose=false) const;
   void getSpectralWindowInfo(Record& outRec) const;
   void listSpectralAndPolInfo (LogIO& os, bool verbose=false,
                                bool oneBased=true) const;
   void listSysCal (LogIO& os, bool verbose=false) const;
   void listWeather (LogIO& os, bool verbose=false) const;
// </group>

// List table size summary
   void listTables (LogIO& os, bool verbose=false) const;

   void setListUnflaggedRowCount(bool v) { _listUnflaggedRowCount = v; }

   // OBSOLETE. No longer does anything, kept for compilation backward compatibility.
   void setMetaDataCacheSizeInMB(float) {}

private:
// Pointer to MS
   const MeasurementSet* pMS;
   std::shared_ptr<MSMetaData> _msmd;

// Formatting strings
   const String dashlin1, dashlin2;

// Clear formatting flags
   void clearFlags (LogIO& os) const;

// For keeping track of the number of vis per field
   mutable Vector<int32_t> nVisPerField_;

   // Name of the MS used in the constructor
   String msname_p;

   bool _listUnflaggedRowCount;

   float _cacheSizeMB;
};


} //# NAMESPACE CASACORE - END

#endif
