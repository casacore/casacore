//# MSSelection.h: Class to represent a selection on an MS
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
//# Correspondence concerning AIPS++ should be adressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#

//# $Id$


#if !defined MS_MSSELECTION_H
#define MS_MSSELECTION_H

/*
#include <aips/aips.h>
#include <aips/Utilities/String.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Glish/GlishRecord.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MRadialVelocity.h>
#include <aips/Tables/ExprNode.h>
#include <aips/MeasurementSets/MeasurementSet.h>
*/

#include <casa/aips.h>
#include <casa/BasicSL/String.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Matrix.h>
#include <measures/Measures/MEpoch.h>
#include <measures/Measures/MRadialVelocity.h>
#include <tables/Tables/ExprNode.h>
#include <ms/MeasurementSets/MeasurementSet.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary> 
// MSSelection: Class to represent a selection on an MS
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="" demos="">

// <prerequisite>
//   <li> <linkto class="MeasurementSet">MeasurementSet</linkto> module
// </prerequisite>
//
// <etymology>
// From "MeasurementSet" and "selection".
// </etymology>
//
// <synopsis>
// The MSSelection class represents a selection on a MeasurementSet (MS).
// This class is used in translating MS selections represented as
// selection items in the user interface, and for converting between
// MS selection and pure TaQL selection.
// </etymology>
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// This class is used by the MS access classes.
// </motivation>
//
// <todo asof="01/03/01">
// </todo>

class MSSelection
{
 public:
   // Default null constructor, and destructor
   MSSelection();
   virtual ~MSSelection();

   // Construct from a Glish record representing a selection item
   // at the Glish or user interface level.
   MSSelection(const Record& selectionItem);

   // Copy constructor
   MSSelection(const MSSelection& other);

   // Assignment operator
   MSSelection& operator=(const MSSelection& other);

   // Field accessors
   void setStartTime(const MEpoch& startTime);
   void setEndTime(const MEpoch& endTime);
   void setFieldIds(const Vector<Int>& fieldIds);
   void setFieldNames(const Vector<String>& fieldNames);
   void setSourceNames(const Vector<String>& sourceNames);
   void setScanNos(const Vector<Int>& scanNos);
   void setSpwIds(const Vector<Int>& spwIds);
   void setFreqGrps(const Vector<Int>& freqGrps);
   void setChanSel(const Int& nchan, const Int& start, const Int& step);
   void setVelocitySel(const Int& nchan, const MRadialVelocity& velocityStart,
		       const MRadialVelocity& velocityStep);
   void setAntennaIds(const Vector<Int>& antennaIds);
   void setAntennaNames(const Vector<String>& antennaNames);
   void setInterferometerIds(const Matrix<Int>& interferometerIds);
   void setFeedIds(const Vector<Int>& feedIds);
   void setCorrTypes(const Vector<String>& corrTypes);
   void setArrayIds(const Vector<Int>& arrayIds);
   void setUVRange(const Double& startUV, const Double& endUV);
   void setMSSelect(const String& msSelect);
   void setObsModes(const Vector<String>& obsModes);
   void setCalGrps(const Vector<String>& calGrps);
   
   //Add for ms selection
   //   void setSelectionMS(TableExprNode nd);
   //   MeasurementSet& getSelectionMS();

   // Convert to TableExprNode format (C++ interface to TaQL)
   TableExprNode toTableExprNode(const MeasurementSet& ms);

   //TableExprNode msTableExprNode;
   static TableExprNode *msTableExprNode;

   // add for ms selection 
   static TableExprNode *msFieldTableExprNode;
   static TableExprNode *msSpwTableExprNode;

 private:
   // Initialize from a GlishRecord representing a selection
   // item from the user interface or Glish CLI
   void fromSelectionItem(const Record& selectionItem);

   // Check if record field exists and is not unset
   Bool definedAndSet(const Record& inpRec, const String& fieldName);

   // Convert an MS select string to TaQL
   //   const String msToTaQL(const String& msSelect) {};

   // Selection sub-fields
   MEpoch startTime_p, endTime_p;
   MRadialVelocity velocityStart_p, velocityStep_p;
   Double startUV_p, endUV_p;
   Vector<String> fieldNames_p, sourceNames_p, antennaNames_p, corrTypes_p, 
     obsModes_p, calGrps_p;
   Matrix<Int> interferometerIds_p;
   Vector<Int> fieldIds_p, scanNos_p, spwIds_p, freqGrps_p,
     antennaIds_p, feedIds_p, arrayIds_p;
   String msSelect_p;
   Int nchan_p, start_p, step_p;
   //Add for ms selection
   //   static MeasurementSet mssel;

   // Flags to indicate which selections are active
   Bool selectStartTime_p, selectEndTime_p, selectFieldIds_p, 
     selectFieldNames_p, selectSourceNames_p, selectScanNos_p, 
     selectSpwIds_p, selectFreqGrps_p, selectChanSel_p, 
     selectVelocitySel_p, selectAntennaIds_p, 
     selectAntennaNames_p, selectInterferometerIds_p, selectFeedIds_p, 
     selectCorrTypes_p, selectArrayIds_p, selectUVRange_p, 
     selectMSSelect_p, selectObsModes_p, selectCalGrps_p;
};


} //# NAMESPACE CASA - END

#endif
   

