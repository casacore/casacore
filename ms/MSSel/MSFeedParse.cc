//# MSFeedParse.cc: Classes to hold results from feed grammar parser
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

#include <casacore/ms/MSSel/MSFeedParse.h>
#include <casacore/ms/MSSel/MSFeedIndex.h>
#include <casacore/ms/MSSel/MSSelectionError.h>
#include <casacore/ms/MeasurementSets/MSFeedColumns.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/ms/MSSel/MSSelectionTools.h>
#include <casacore/ms/MSSel/MSSelectionErrorHandler.h>
namespace casacore { //# NAMESPACE CASACORE - BEGIN

 // Global pointer to the parser object
  MSFeedParse* MSFeedParse::thisMSFParser = 0;
  TableExprNode MSFeedParse::column1AsTEN_p, MSFeedParse::column2AsTEN_p;
  MSSelectionErrorHandler* MSFeedParse::thisMSFErrorHandler = 0;
  
  //# Constructor
  MSFeedParse::MSFeedParse ()
    : MSParse(),
      colName1(MS::columnName(MS::FEED1)),
      colName2(MS::columnName(MS::FEED2)),
      feed1List(0),feed2List(0), feedPairList(0,2)
  {
  }
  
  //# Constructor with given ms name.
  MSFeedParse::MSFeedParse (const MSFeed& feedSubTable, 
				  const TableExprNode& feed1AsTEN, const TableExprNode& feed2AsTEN)
    : MSParse(),
      colName1(MS::columnName(MS::FEED1)),
      colName2(MS::columnName(MS::FEED2)),
      feed1List(0),feed2List(0), feedPairList(0,2),
      msSubTable_p(feedSubTable)
      
  {
    column1AsTEN_p = feed1AsTEN;
    column2AsTEN_p = feed2AsTEN;
  }

  //# Constructor with given ms name.
  MSFeedParse::MSFeedParse (const MeasurementSet* myms)
    : MSParse(myms, "Feed"),
      colName1(MS::columnName(MS::FEED1)),
      colName2(MS::columnName(MS::FEED2)),
      feed1List(0),feed2List(0), feedPairList(0,2),
      msSubTable_p(myms->feed())
  {
    column1AsTEN_p = myms->col(myms->columnName(MS::FEED1));
    column2AsTEN_p = myms->col(myms->columnName(MS::FEED2));
  }

  // Add the current condition to the TableExprNode tree.  Mask auto
  // correlations if baselineType==CrossOnly
  // 
  const TableExprNode* MSFeedParse::setTEN(TableExprNode& condition, 
                                              BaselineListType baselineType,
                                              Bool negate)
  {
    if (baselineType==CrossOnly) 
      {
	TableExprNode noAutoCorr = (column1AsTEN_p != column2AsTEN_p);
	condition = noAutoCorr && condition;
      }
    if (negate) condition = !condition;
    if(node_p.isNull()) node_p = condition;
    else
      if (negate) node_p = node_p && condition;
      else        node_p = node_p || condition;

    return &node_p;
  }

  const TableExprNode* MSFeedParse::selectFeedIds(const Vector<Int>& feedIds, 
							BaselineListType baselineType,
							Bool negate) 
  {
    TableExprNode condition;
    if ((baselineType==AutoCorrAlso) || (baselineType==AutoCorrOnly)) 
      {
	Int n=feedIds.nelements();
	if (n) 
	  {
	    condition = ((column1AsTEN_p == feedIds[0]) &&
			 (column2AsTEN_p == feedIds[0]));
	    for (Int i=1;i<n;i++) 
	      {
		condition = condition || 
		  ((column1AsTEN_p == feedIds[i]) && (column2AsTEN_p == feedIds[i]));
	      }
	  }
      } 
    else 
      {
	condition =
	  (column1AsTEN_p.in(feedIds) ||
	   column2AsTEN_p.in(feedIds)); 
      }
    {
      // cannot use indgen for this, rows of feed table may have same feed ID
      MSFeedColumns* msfc = new MSFeedColumns(subTable());
      Vector<Int> f2 = msfc->feedId().getColumn();
      delete msfc;
      /*
      Int nrows_p = subTable().nrow();
      Vector<Int> f2(nrows_p);
      f2.resize(nrows_p);
      indgen(f2);
      */

      makeFeedList(feed1List, feedIds, negate);
      makeFeedList(feed2List, f2);
      if (negate) makeFeedPairList(-feedIds,f2,feedPairList,baselineType, negate);
      else        makeFeedPairList( feedIds,f2,feedPairList,baselineType, negate);
    }
    return setTEN(condition, baselineType, negate);
  }

  void MSFeedParse::makeFeedList(Vector<Int>& feedList,const Vector<Int>& thisList,
				       Bool negate)
  {
    Vector<Int> f2;
    if (negate) f2=-thisList;
    else        f2=thisList;

    Vector<Int> tmp1(set_union(f2,feedList));
    feedList.resize(tmp1.nelements());feedList = tmp1;
  }
  
  const TableExprNode* MSFeedParse::selectFeedIds(const Vector<Int>& feedIds1,
							const Vector<Int>& feedIds2,
							BaselineListType baselineType,
							Bool negate)
  {
    TableExprNode condition;

    condition =
      (column1AsTEN_p.in(feedIds1)  && column2AsTEN_p.in(feedIds2)) ||
      (column1AsTEN_p.in(feedIds2)  && column2AsTEN_p.in(feedIds1));
    makeFeedList(feed1List, feedIds1, negate);
    makeFeedList(feed2List, feedIds2, negate);

    if (negate) makeFeedPairList(-feedIds1, -feedIds2, feedPairList, baselineType, negate);
    else        makeFeedPairList( feedIds1,  feedIds2, feedPairList, baselineType, negate);

    return setTEN(condition,baselineType,negate);
  }

  Bool MSFeedParse::addFeedPair(const Matrix<Int>& feedpairlist,
                                   const Int feed1, const Int feed2, 
 				   BaselineListType baselineType)
  {
    Bool doAutoCorr;
    doAutoCorr = (baselineType==AutoCorrAlso) || (baselineType==AutoCorrOnly);
    if ((feed1 == feed2) && (!doAutoCorr)) return False;
    if ((baselineType==AutoCorrOnly) && (feed1!=feed2)) return False;

    Int n=feedpairlist.shape()(0);
    for (Int i=0;i<n;i++) {
      if (((feedpairlist(i,0)==feed1) && (feedpairlist(i,1)==feed2)) ||
          ((feedpairlist(i,1)==feed1) && (feedpairlist(i,0)==feed2))) {
        return False;
      }
    }
    return True;
  }
  //
  // Method to make a list of unique feed pairs, given a list of
  // feed1 and feed2.  The feed pairs list is appended to the
  // existing list.  The required sizing could be done better.
  //
  void MSFeedParse::makeFeedPairList(const Vector<Int>& f1,
                    const Vector<Int>& f2, 
					Matrix<Int>& feedpairlist, 
					BaselineListType baselineType,
					Bool /*negate*/)
  {
    Int n1,n2,nb0;
    n1=f1.nelements();  n2=f2.nelements();
    nb0=feedpairlist.shape()(0);
    IPosition newSize(2,nb0,2);

    for (Int i1=0;i1<n1;i1++) {
      for (int i2=0;i2<n2;i2++) {
        Int feed1, feed2;
        feed1=f1[i1]; feed2=f2[i2];
        if (addFeedPair(feedpairlist,feed1,feed2,baselineType)) {
          nb0++;
          newSize[0]=nb0;
          feedpairlist.resize(newSize,True);
          feedpairlist(nb0-1,0)=feed1;
          feedpairlist(nb0-1,1)=feed2;
        }
      }
    }
  }
  
} //# NAMESPACE CASACORE - END
