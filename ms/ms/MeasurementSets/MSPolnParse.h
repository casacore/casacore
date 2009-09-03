//# MSPolnParse.h: Classes to hold results from poln grammar parseing
//# Copyright (C) 1994,1995,1997,1998,1999,2000,2001,2003
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

#ifndef MS_MSPOLNPARSE_H
#define MS_MSPOLNPARSE_H

//# Includes
#include <ms/MeasurementSets/MSParse.h>
#include <casa/Containers/OrderedMap.h>
#include <casa/Containers/MapIO.h>
#include <ms/MeasurementSets/MSPolarization.h>
#include <ms/MeasurementSets/MSPolColumns.h>
#include <ms/MeasurementSets/MSPolIndex.h>
#include <ms/MeasurementSets/MSDataDescIndex.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations


// <summary>
// Class to hold values from field grammar parser
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
// </prerequisite>

// <etymology>
// MSPolnParse is the class used to parse a polarization selection command.
// </etymology>

// <synopsis> 
//
// MSPolnParse is used by the parser of polarization sub-expression
// statements of the type [SPW:]POLN.  Since this is a relatively
// simple expression to tokenize and parse, this parser is written
// without Bison or Flex.  The methods of this class take an
// expression, and internally generate a list of the Data Description
// IDs that should be used to select the rows in the MS main table.
// The map of Polarization IDs (row numbers in the POLARIZATION
// sub-table) and the list of indices to be used to pick the user
// selected polarzation data (in the DATA columns of the MS main
// table) is also generated.  This map is intended to be used along
// with the map of SPW and selected channels to apply the in-row
// selection (Slice on the data columns).
//
// </synopsis>

// <motivation> 
// It is necessary to be able to give a data selection
// command in ASCII.  This can be used in a CLI or in the table
// browser to get a subset of a table or to sort a table.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class MSPolnParse : public MSParse
{
public:
  // Default constructor
  MSPolnParse ();
  //  ~MSPolnParse() {if (node_p) delete node_p;node_p=0x0;};

  // Associate the ms and the shorthand.
  MSPolnParse (const MeasurementSet* ms);

  const TableExprNode *selectFromIDList(const Vector<Int>& ddIDs);

  // Get table expression node object.
  const TableExprNode* node();
  //  static MSPolnParse* thisMSSParser;
  void reset() {polMap_p.clear(); ddIDList_p.resize(0);};
  void cleanup() {if (node_p) delete node_p;node_p=0x0;};
  Int theParser(const String& command); 
		// Vector<Int>& selectedDDIDs, 
		// Matrix<Int>& selectedSpwPolnMap);
  OrderedMap<Int, Vector<Int> > selectedPolnMap()           {return polMap_p;}
  OrderedMap<Int, Vector<Vector<Int> > > selectedSetupMap() {return setupMap_p;}
  Vector<Int> selectedDDIDs()                               {return ddIDList_p;}
private:
  Vector<Int> getMapToDDIDs(MSDataDescIndex& msDDNdx, MSPolarizationIndex& msPolNdx,
			    const Vector<Int>& spwIDs, Vector<Int>& polnIDs,
			    Vector<Int>& polIndices);
  Vector<Int> matchPolIDsToPolTableRow(const Vector<Int>& polIds,
				       OrderedMap<Int, Vector<Int> >& polIndexMap,
				       Vector<Int>& polIndices,
				       Bool addToMap=False);
  Vector<Int> getPolnIDs(const String& polSpec, Vector<Int>& polIndices);
  Vector<Int> getPolnIndices(const Int& polnID, const Vector<Int>& polnIDList);
  //
  // These are the versions used in the code.
  Vector<Int> getPolnIDsV2(const String& polSpec, Vector<Int>& polTypes);
  Vector<Int> getMapToDDIDsV2(const String& polExpr, 
			      const Vector<Int>& spwIDs, 
			      Vector<Int>& polnIDs,
			      Vector<Int>& polnIndices);
  TableExprNode* node_p;
  OrderedMap<Int, Vector<Int> > polMap_p;
  OrderedMap<Int, Vector<Vector<Int> > > setupMap_p;
  Vector<Int> ddIDList_p;

  void setIDLists(const Int key, const Int ndx, Vector<Int>& val);
};

} //# NAMESPACE CASA - END

#endif
