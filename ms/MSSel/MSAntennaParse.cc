//# MSAntennaParse.cc: Classes to hold results from antenna grammar parser
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

#include <casacore/ms/MSSel/MSAntennaParse.h>
#include <casacore/ms/MSSel/MSAntennaIndex.h>
#include <casacore/ms/MSSel/MSSelectionError.h>
#include <casacore/ms/MeasurementSets/MSAntennaColumns.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/ms/MSSel/MSSelectionTools.h>
#include <casacore/ms/MSSel/MSSelectionErrorHandler.h>
namespace casacore { //# NAMESPACE CASACORE - BEGIN

 // Global pointer to the parser object
  MSAntennaParse* MSAntennaParse::thisMSAParser = 0;
  TableExprNode MSAntennaParse::column1AsTEN_p,MSAntennaParse::column2AsTEN_p;
  MSSelectionErrorHandler* MSAntennaParse::thisMSAErrorHandler = 0;
  
  //# Constructor
  MSAntennaParse::MSAntennaParse ()
    : MSParse(),
      colName1(MS::columnName(MS::ANTENNA1)),
      colName2(MS::columnName(MS::ANTENNA2)),
      ant1List(0),ant2List(0), baselineList(0,2)
  {
  }
  
  //# Constructor with given ms name.
  MSAntennaParse::MSAntennaParse (const MSAntenna& antSubTable, 
				  const TableExprNode& ant1AsTEN, const TableExprNode& ant2AsTEN)
    : MSParse(),
      colName1(MS::columnName(MS::ANTENNA1)),
      colName2(MS::columnName(MS::ANTENNA2)),
      ant1List(0),ant2List(0), baselineList(0,2),
      msSubTable_p(antSubTable)
      
  {
    column1AsTEN_p = ant1AsTEN;
    column2AsTEN_p = ant2AsTEN;
  }

  //# Constructor with given ms name.
  MSAntennaParse::MSAntennaParse (const MeasurementSet* myms)
    : MSParse(myms, "Antenna"),
      colName1(MS::columnName(MS::ANTENNA1)),
      colName2(MS::columnName(MS::ANTENNA2)),
      ant1List(0),ant2List(0), baselineList(0,2),
      msSubTable_p(myms->antenna())
  {
    column1AsTEN_p = myms->col(myms->columnName(MS::ANTENNA1));
    column2AsTEN_p = myms->col(myms->columnName(MS::ANTENNA2));
  }

  // Add the current condition to the TableExprNode tree.  Mask auto
  // correlations if baselineType==CrossOnly
  // 
  const TableExprNode* MSAntennaParse::setTEN(TableExprNode& condition, 
                                              BaselineListType baselineType,
                                              Bool negate)
  {
    if (baselineType==CrossOnly) 
      {
	//	TableExprNode noAutoCorr = (ms()->col(colName1) != ms()->col(colName2));
	TableExprNode noAutoCorr = (column1AsTEN_p != column2AsTEN_p);
	condition = noAutoCorr && condition;
      }
    //    if (negate) cerr << "Generating a negation condition" << endl;
    if (negate) condition = !condition;
    if(node_p.isNull()) node_p = condition;
    else
      if (negate) node_p = node_p && condition;
      else        node_p = node_p || condition;

    return &node_p;
  }

  const TableExprNode* MSAntennaParse::selectAntennaIds(const Vector<Int>& antennaIds, 
							BaselineListType baselineType,
							Bool negate) 
  {
    TableExprNode condition;
    if ((baselineType==AutoCorrAlso) || (baselineType==AutoCorrOnly)) 
      {
	Int n=antennaIds.nelements();
	if (n) 
	  {
	    // condition = ((ms()->col(colName1) == antennaIds[0]) &&
	    // 		 (ms()->col(colName2) == antennaIds[0]));
	    condition = ((column1AsTEN_p == antennaIds[0]) &&
			 (column2AsTEN_p == antennaIds[0]));
	    for (Int i=1;i<n;i++) 
	      {
		condition = condition || 
		  ((column1AsTEN_p == antennaIds[i]) && (column2AsTEN_p == antennaIds[i]));
		  // ((ms()->col(colName1) == antennaIds[i]) &&
		  //  (ms()->col(colName2) == antennaIds[i]));
	      }
	  }
      } 
    else 
      {
	condition =
	  // (ms()->col(colName1).in(antennaIds) ||
	  //  ms()->col(colName2).in(antennaIds)); //&& ms()->col(colName1) != ms()->col(colName2);
	  (column1AsTEN_p.in(antennaIds) ||
	   column2AsTEN_p.in(antennaIds)); //&& ms()->col(colName1) != ms()->col(colName2);
      }
    {
      Int nrows_p = subTable().nrow();//ms()->antenna().nrow();
      Vector<Int> a2(nrows_p);
      a2.resize(nrows_p);
      indgen(a2);

      makeAntennaList(ant1List, antennaIds,negate);
      makeAntennaList(ant2List, a2);
      if (negate) makeBaselineList(-antennaIds,a2,baselineList,baselineType, negate);
      else        makeBaselineList(antennaIds,a2,baselineList,baselineType, negate);
    }
    //    setTEN(condition, AutoCorrAlso, negate);
    return setTEN(condition, baselineType, negate);
  }

  void MSAntennaParse::makeAntennaList(Vector<Int>& antList,const Vector<Int>& thisList,
				       Bool negate)
  {
    Vector<Int> a2;
    if (negate) a2=-thisList;
    else        a2=thisList;

    Vector<Int> tmp1(set_union(a2,antList));
    antList.resize(tmp1.nelements());antList = tmp1;
  }
  
  const TableExprNode* MSAntennaParse::selectAntennaIds(const Vector<Int>& antennaIds1,
							const Vector<Int>& antennaIds2,
							BaselineListType baselineType,
							Bool negate)
  {
    TableExprNode condition;

    if (antennaIds2.size()) 
      {
	condition =
	  // (ms()->col(colName1).in(antennaIds1)  && ms()->col(colName2).in(antennaIds2)) ||
	  // (ms()->col(colName1).in(antennaIds2)  && ms()->col(colName2).in(antennaIds1));
	  (column1AsTEN_p.in(antennaIds1)  && column2AsTEN_p.in(antennaIds2)) ||
	  (column1AsTEN_p.in(antennaIds2)  && column2AsTEN_p.in(antennaIds1));
      } 
    else 
      {
	condition =
	  //	  (ms()->col(colName1).in(antennaIds1) && ms()->col(colName2).in(antennaIds1));
	  (column1AsTEN_p.in(antennaIds1) && column2AsTEN_p.in(antennaIds1));
      }    
    makeAntennaList(ant1List, antennaIds1,negate);
    makeAntennaList(ant2List, antennaIds2,negate);

    if (negate) makeBaselineList(-antennaIds1, -antennaIds2,baselineList, baselineType, negate);
    else        makeBaselineList(antennaIds1, antennaIds2,baselineList, baselineType, negate);

    return setTEN(condition,baselineType,negate);
  }
  
  const TableExprNode* MSAntennaParse::selectNameOrStation(const Vector<String>& antenna, 
							   BaselineListType baselineType,
							   Bool negate)
  {
    //    MSAntennaIndex msAI(ms()->antenna());
    MSAntennaIndex msAI(subTable());
    
    Vector<Int> ant=msAI.matchAntennaName(antenna);

    //    TableExprNode condition =(ms()->col(colName1).in(ant) || ms()->col(colName2).in(ant));
    TableExprNode condition =(column1AsTEN_p.in(ant) || column2AsTEN_p.in(ant));
    
    return setTEN(condition,baselineType,negate);
  }
  
  const TableExprNode* MSAntennaParse::selectNameOrStation(const Vector<String>& antenna1,
							   const Vector<String>& antenna2,
							   BaselineListType baselineType,
							   Bool negate)
  {
    //    MSAntennaIndex msAI(ms()->antenna());
    MSAntennaIndex msAI(subTable());
    
    Vector<Int> a1=msAI.matchAntennaName(antenna1),
      a2 = msAI.matchAntennaName(antenna2);

    // TableExprNode condition =
    //   (ms()->col(colName1).in(a1) && ms()->col(colName2).in(a2)) ||
    //   (ms()->col(colName1).in(a2) && ms()->col(colName2).in(a1));
    TableExprNode condition =
      (column1AsTEN_p.in(a1) && column2AsTEN_p.in(a2)) ||
      (column1AsTEN_p.in(a2) && column2AsTEN_p.in(a1));
    
    return setTEN(condition,baselineType,negate);
  }
  
  const TableExprNode* MSAntennaParse::selectNameOrStation(const String& antenna1,
							   const String& antenna2,
							   BaselineListType baselineType,
							   Bool negate)
  {
    // TableExprNode condition =
    //   (ms()->col(colName1) >= antenna1 && ms()->col(colName2) <= antenna2) ||
    //   (ms()->col(colName2) >= antenna1 && ms()->col(colName1) <= antenna2);
    TableExprNode condition =
      (column1AsTEN_p >= antenna1 && column2AsTEN_p <= antenna2) ||
      (column2AsTEN_p >= antenna1 && column1AsTEN_p <= antenna2);
    
    return setTEN(condition,baselineType,negate);
  }
  
  
  const TableExprNode* MSAntennaParse::selectLength
  (const std::vector<double>& lengths, Bool negate)
  {
    TableExprNode selAnt1, selAnt2;
    Matrix<double> blength = getBaselineLengths();
    Matrix<Bool> match(blength.shape());
    match = False;
    int nr=0;
    for (Int j=0; j<blength.shape()[1]; ++j) {
      for (Int i=0; i<blength.shape()[0]; ++i) {
        double bl = blength(i,j);
        for (uInt k=0; k<lengths.size(); k+=2) {
          if (bl >= lengths[k]  &&  bl <= lengths[k+1]) {
            match(i,j) = True;
            ++nr;
          }
        }
      }
    }
    vector<Int> ant1, ant2;
    for (Int i=0; i<blength.shape()[0]; ++i) {
      for (Int j=0; j<blength.shape()[1]; ++j) {
        if (match(i,j)) {
          ant1.push_back (i);
          ant2.push_back (j);
          if (addBaseline (baselineList, i, j, AutoCorrAlso)) {
            IPosition newSize = baselineList.shape();
            int nb = newSize[0];
            newSize[0] = nb+1;
            baselineList.resize (newSize,True);
            baselineList(nb,0) = i;
            baselineList(nb,1) = j;
          }
        }
      }
    }
    TableExprNode condition(False);
    if (ant1.size() > 0) {
      Array<Int> arrAnt1(IPosition(1,ant1.size()), &(ant1[0]), SHARE);
      Array<Int> arrAnt2(IPosition(1,ant1.size()), &(ant2[0]), SHARE);
      // condition = TableExprNode(any((ms()->col(colName1) == arrAnt1  &&
      //                                ms()->col(colName2) == arrAnt2)));
      condition = TableExprNode(any((column1AsTEN_p == arrAnt1  &&
                                     column2AsTEN_p == arrAnt2)));
    }
    return setTEN (condition, AutoCorrAlso, negate);
  }

  Matrix<double> MSAntennaParse::getBaselineLengths()
  {
    //    MSAntenna msant(ms()->antenna());
    MSAntenna msant(subTable());
    ROMSAntennaColumns antCols(msant);
    // First get the antenna positions.
    vector<Vector<double> > antVec;
    antVec.reserve (msant.nrow());
    for (uInt i=0; i<msant.nrow(); ++i) {
      // Convert to ITRF and keep as x,y,z in m.
      antVec.push_back
        (MPosition::Convert(antCols.positionMeas()(i),
                            MPosition::ITRF)().getValue().getValue());
    }
    // Fill in the length of each baseline.
    Matrix<double> blength(antVec.size(), antVec.size());
    for (uInt j=0; j<antVec.size(); ++j) {
      for (uInt i=0; i<antVec.size(); ++i) {
        Array<double> diff(antVec[i] - antVec[j]);
        blength(i,j) = sqrt(sum(diff*diff));
      }
    }
    return blength;
  }

  double MSAntennaParse::getUnitFactor (const char* unit)
  {
    // Check if conversion is possible.
    Unit u(unit);
    Quantity q(1., "m");
    if (! q.isConform (u)) {
      throw MSSelectionAntennaParseError (String("Unit ") + unit +
                                          " must be a distance unit (like m)");
    }
    // Get conversion factor.
    return q.getValue (unit);
  }

  Bool MSAntennaParse::addBaseline(const Matrix<Int>& baselist,
                                   const Int ant1, const Int ant2, 
 				   BaselineListType baselineType)
  {
    Bool doAutoCorr;
    doAutoCorr = (baselineType==AutoCorrAlso) || (baselineType==AutoCorrOnly);
    if ((ant1 == ant2) && (!doAutoCorr)) return False;
    if ((baselineType==AutoCorrOnly) && (ant1!=ant2)) return False;

    Int n=baselist.shape()(0);
    for (Int i=0;i<n;i++) {
      if (((baselist(i,0)==ant1) && (baselist(i,1)==ant2)) ||
          ((baselist(i,1)==ant1) && (baselist(i,0)==ant2))) {
        return False;
      }
    }
    return True;
  }

  //
  // Method to make a list of unique baselines, given a list of
  // antenna1 and antenna2.  The baselines list is appended to the
  // existing list.  The required sizing could be done better.
  //
  void MSAntennaParse::makeBaselineList(const Vector<Int>& a1,
                                        const Vector<Int>& a2, 
					Matrix<Int>& baselist, 
					BaselineListType baselineType,
					Bool /*negate*/)
  {
    Int n1,n2,nb0;
    n1=a1.nelements();  n2=a2.nelements();
    nb0=baselist.shape()(0);
    IPosition newSize(2,nb0,2);

    for (Int i1=0;i1<n1;i1++) {
      for (int i2=0;i2<n2;i2++) {
        Int ant1, ant2;
        ant1=a1[i1]; ant2=a2[i2];
        if (addBaseline(baselist,ant1,ant2,baselineType)) {
          nb0++;
          newSize[0]=nb0;
          baselist.resize(newSize,True);
          baselist(nb0-1,0)=ant1;
          baselist(nb0-1,1)=ant2;
        }
      }
    }
  }
  
} //# NAMESPACE CASACORE - END
