//# MsPlot.cc:  this defines ClassName, which ...
//# Copyright (C) 2003
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

//# Includes
#include <ms/MSPlot/MsPlot.h>
#include <ms/MeasurementSets/MSColumns.h>
#include <measures/Measures/MeasTable.h>
//
#include <tables/Tables/ScalarColumn.h>
//#include <tables/Tables/ArrayColumn.h>
#include <tables/Tables/SetupNewTab.h>
#include <tables/Tables/TableDesc.h>
#include <tables/Tables/ScaColDesc.h>

// MeasConvert.h must be directly included here!?( if including it in the MsPlot.h
// file instead of here, it would not work).
#include <measures/Measures/MeasConvert.h>
//#include <tables/Tables/ExprNode.h>
namespace casa { //# NAMESPACE CASA - BEGIN
// default constructor. In case for some reason, one need to pass a Table object, not
// a MeasurementSet to TABS_P, he can use this constructor.
template <class T> MsPlot<T>::MsPlot()
   :TablePlot<T>(), m_dataIsSet( False ){
	    m_dbg = True;
   }
template <class T> MsPlot<T>::MsPlot( const MeasurementSet &ms )
   :TablePlot<T>(), m_dataIsSet( False ), m_ms( ms ){
	    m_dbg = True;
		 //m_ms = ms;
		 //m_dataIsSet = False;
		 //m_fitstPlot = True; 
   }

// destructor 
template <class T> MsPlot<T>::~MsPlot(){}
//
template <class T> 
Bool MsPlot<T>::antennaSelection( const Vector<String>& antennaNames, const Vector<Int>& antennaIndex )
	{
		  if( antennaNames[0].compare(String("")) ){ 
		     if(!(m_select.setAntennaExpr( MSSelection::nameExprStr( antennaNames)))) return False;
			  if( m_dbg ){ cout << "[ MsPlot<T>::antennaSelection() ] antennaNames are: " 
			      << MSSelection::nameExprStr( antennaNames) << endl;  }
		  }
		  if( antennaIndex != -1 ){ 
		     if(!(m_select.setAntennaExpr( "'"+MSSelection::indexExprStr( antennaIndex )+"'" ))) return False;
			  if( m_dbg ){ cout << "[ MsPlot<T>::antennaSelection() ] antennaIndex converted to String::"
			     << MSSelection::indexExprStr( antennaIndex ) << endl;  } 
		  }		
		return True;
	}
template <class T>
Bool MsPlot<T>::spwSelection( const Vector<String>& spwNames, const Vector<Int>& spwIndex )
	{
		 if(spwNames[0].compare(String("")) ){ 
			 if(!(m_select.setSpwExpr( MSSelection::nameExprStr( spwNames)))) return False;
			 if( m_dbg ) cout << "[ MsPlot<T>::spwSelection() ] spwNames set. " << endl;
		 }
		 if( spwIndex != -1 ){ 
			 if(!(m_select.setSpwExpr( MSSelection::indexExprStr( spwIndex )))) return False;
			 if( m_dbg ) cout << "[ MsPlot<T>::spwSelection() ] spwIndex set. " << endl;
		 }  
		 return True;		
	}
template <class T>
Bool MsPlot<T>::fieldSelection( const Vector<String>& fieldNames,  const Vector<Int>& fieldIndex )
	{
		if(fieldNames[0].compare(String("")) ){
		   if(!(m_select.setFieldExpr( MSSelection::nameExprStr( fieldNames)))) return False;
			if( m_dbg ) cout << "[ MsPlot<T>::fieldSelection() ] fieldNames set. " << endl;
		}
		if( fieldIndex != -1 ){
		   if(!(m_select.setFieldExpr( MSSelection::indexExprStr( fieldIndex )))) return False;
			if( m_dbg ) cout << "[ MsPlot<T>::fieldSelection() ] fieldIndex set. " << endl;
		} 
		return True;			
	}
template <class T>
Bool MsPlot<T>::uvDistSelection( const Vector<String>& uvDists )
   {
		if( uvDists[0].compare(String("")) ){ 
		   if(!(m_select.setUvDistExpr( MSSelection::nameExprStr( uvDists )))) return False;
			if( m_dbg ) cout << "[ MsPlot<T>::uvDistSelection() ] uvDists set. " << endl;
	   }  	
		return True;	
	}
template<class T>
Bool MsPlot<T>::timeSelection( const Vector<String>& times )
   {
		if( times[0].compare(String("")) ){
		   if( times.nelements() == 1 ){
			    if( m_dbg ) cout << "[ MsPlot<T>::timeSelection()] times[0] = " << times[0] << endl;
			    if(!(m_select.setTimeExpr( times[0] ))) return False; 
			 }else{ 
		       if(!(m_select.setTimeExpr( MSSelection::nameExprStr( times )))) return False;			
			 }
			if( m_dbg ) cout << "[ MsPlot<T>::timesSelection() ] times set. " << endl;
	   } 	
		return True;	
   }
template<class T>
Bool MsPlot<T>::corrSelection( const Vector<String>& correlations )
   {
		if( correlations[0].compare(String("")) ){ 
		   if(!(m_select.setCorrExpr( MSSelection::nameExprStr( correlations )))) return False;
			if( m_dbg ) cout << "[ MsPlot<T>::corrSelection() ] correlations set. " << endl;
	   } 	
		return True;	
   }
template<class T>
Bool MsPlot<T>::setData( const Vector<String>& antennaNames, const Vector<Int>& antennaIndex,
              const Vector<String>& spwNames, const Vector<Int>& spwIndex,
				  const Vector<String>& fieldNames,  const Vector<Int>& fieldIndex,
				  const Vector<String>& uvDists,
				  const Vector<String>& times,
				  const Vector<String>& correlations 
            )
	{
	// set selection expression first.
		if( antennaNames[0].compare(String("")) || antennaIndex!=-1 ) {
		   if( m_dbg ) cout << "[ MsPlot<T>::setData() ] calling antennaSelection()..." << endl;
		   if(!antennaSelection( antennaNames, antennaIndex )) return False;
		}
		if( spwNames[0].compare(String("")) || spwIndex!=-1 ) {
		   if( m_dbg ) cout << "[ MsPlot<T>::setData() ] calling spwSelection()..." << endl;
		   if(!spwSelection( spwNames, spwIndex )) return False;
		}
		if( fieldNames[0].compare(String("")) || fieldIndex!=-1 ){
		   if( m_dbg ) cout << "[ MsPlot<T>::setData() ] calling fieldSelection()..." << endl;
		   if( !fieldSelection( fieldNames, fieldIndex )) return False;
		}
		if( uvDists[0].compare(String("")) ) {
		   if( m_dbg ) cout << "[ MsPlot<T>::setData() ] calling uvDistSelection()..." << endl;
		   if(!uvDistSelection( uvDists )) return False;
		}
		if( times[0].compare(String("")) ) {
		   if( m_dbg ) cout << "[ MsPlot<T>::setData() ] calling timesSelection()..." << endl;
		   if(!timeSelection( times )) return False;
		}
		if( correlations[0].compare(String("")) ) {
		   if( m_dbg ) cout << "[ MsPlot<T>::setData() ] calling correlationsSelection()..." << endl;
		   if(!corrSelection( correlations )) return False;
		}
			
		// generate node from this expression
		TableExprNode node = m_select.toTableExprNode(&m_ms);
		// Create a table and new MS based on this node
		Table subTable( m_ms.tableName(), Table::Update);
      m_subMS = MeasurementSet(subTable(node, node.nrow()));
		Int nTabs = 1;
		// set the subset of MS to TABS_P for plotting. In the case when the user wants to plot the
		// original table, we set it in plot() method.
		setTableT( nTabs, m_subMS );
		if( m_dbg ) cout << "[ MsPlot<T>::setData() ] setTableT() called." << endl;
		// now the data for plotting has been set, so set the member m_dataIsSet to true.
		m_dataIsSet = True;

		return True;		
	}
template<class T>
Bool MsPlot<T>::setAxes( PtrBlock<BasePlot<T>* > &BPS, Vector<String> & dataStr ){
       // check if the TABS_P has been assigned any table (MS).
		 /*if( !m_dataIsSet ){
		     // no sub-dataset has been selected, so set the original MS dataset to TABS_P.
			  cout << "[MsPlot<T>::setAxes()] Set the original MS dataset to TABS_P." << endl;
			  cout << "[MsPlot<T>::setAxes()] m_ms.historyTableName() is " << m_ms.historyTableName() << endl;
		     Int nTabs = 1;
		     //setTableT( nTabs, m_ms );
			  String msName = m_ms.tableName();
			  cout << "[ MsPlot<T>::setAxes() ]  msName is " << msName << endl;
			  setTableS( nTabs, msName );
		     // now the data for plotting has been set, so set the member m_dataIsSet to true.
	        m_dataIsSet = True; 
		 }*/	
		if( m_dbg ) cout << "[MsPlot<T>::setAxes()] dataStr = " << dataStr << endl;
		if( getData( BPS,dataStr) == -1){
		   cout<< "[MsPlot<T>::setAxes()] failed!" << endl;
		   return False;
		}
		if( m_dbg ) cout << "[MsPlot<T>::setAxes()] After calling getData()." << endl;
		return True;		
	}// end of setAxes
template<class T>
//Bool MsPlot<T>::setLabels( TPPlotter<T> &TPLP, const GlishRecord &poption, Vector<String> &labels )
Bool MsPlot<T>::setLabels( TPPlotter<T> &TPLP, Record &plotOption, Vector<String> &labels )
   {
	/*	poption.toRecord(m_plotOption);
		m_nXPanels=1; m_nYPanels=1;
 	   m_nPanels= m_nXPanels*m_nYPanels;

		if(m_plotOption.isDefined("nxpanels"))
		{
			RecordFieldId ridnx("nxpanels");
			m_plotOption.define(ridnx,m_nXPanels);
		}
		if(m_plotOption.isDefined("nypanels"))
		{
			RecordFieldId ridny("nypanels");
			m_plotOption.define(ridny,m_nYPanels);
		}
		m_nPanels = m_nXPanels*m_nYPanels;
		if( m_dbg ) cout << "[ MsPlot<T>::setLabels() ] m_nPanels = " << m_nPanels << endl;
		if( m_dbg ) cout << "[ MsPlot<T>::setLabels() ] labels = " << labels << endl;
		*/
		//setPlotParameters( TPLP,m_plotOption,labels);
		setPlotParameters( TPLP, plotOption ,labels);
		return True;
   }
// Convert the antenna coordinates into local frame and put the antenna positions into a MemoryTable.
template<class T>
Bool MsPlot<T>::antennaPositions( Table& ants ){
   ROMSColumns msc( m_ms );
	const ROMSAntennaColumns & antCols  = msc.antenna();
	const ROScalarMeasColumn<MPosition> & antPositions = antCols.positionMeas();
	uInt nAnt= antCols.position().nrow();
	Vector<MPosition> antPoses( nAnt );
	Vector<Double> xTopo( nAnt ), yTopo( nAnt ), zTopo( nAnt );

	for( uInt i=0; i< nAnt; i++) antPoses( i ) = antPositions( i );
	// find the array center
	String telescope = msc.observation().telescopeName()(0);
	MPosition obs;
	if( MeasTable::Observatory( obs, telescope )){
	   if( obs.type()!= MPosition::ITRF ){
		    MPosition::Convert toItrf( obs, MPosition::ITRF );
			 obs = toItrf( obs );
		}
		// convert the coordinats from the geocentric frame to the topocentric frame
		global2local( obs, antPoses, xTopo, yTopo, zTopo );
		// prepare to create a MemoryTable
		TableDesc td("AntXY","1", TableDesc::New );
		td.comment()="A MemoryTable for antenna local XY plane";
		td.rwKeywordSet().define("XYPlane", "tangential" );
		td.addColumn(ScalarColumnDesc<float>("XEAST", "Towards east" ));
		td.addColumn(ScalarColumnDesc<float>("YNORTH", "Towards north" ));
		SetupNewTable antMaker( "ANTXY", td, Table::New );
		Table antXY( antMaker, Table::Memory, nAnt, False );
		ScalarColumn<float> xCol( antXY, "XEAST" );
		ScalarColumn<float> yCol( antXY, "YNORTH" );
		for( uInt i=0; i<nAnt; i++ ){
		   if( m_dbg ){
			  cout << "[MsPlot<T>::antennaPositions()] X("<<i<<") = " << xTopo(i) << endl;
			  cout << "[MsPlot<T>::antennaPositions()] Y("<<i<<") = " << yTopo(i) << endl;
			}
		   xCol.put( i, xTopo(i) );
			yCol.put( i, yTopo(i) );
		}
		ants = antXY;
		// we do not try to pass antXY to the TablePlot::TABS_P here because then we will need to
		// copy the subMS object to another variable to keep it in memory( a reference will not
		// work because then if we assign antXY to subMS, the reference to subMS will also be 
		// changed.). That means at a time subMS and a copy of it exist simultaneously. This
		// will take too much memory.
		return True;
	}else{
	   cout<<"[ MsPlot::plotArray()] MeasTable::Observatory() failed." << endl;
		return False;
	}
}
/*
// Convert the antenna coordinates into local frame and put the antenna positions into a MemoryTable.
template<class T>
Bool MsPlot<T>::antennaPositions( MemoryTable& ants ){
   MSColumns msc( m_ms );
	ROMSAntennaColumns & antCols  = msc.antenna();
	ScalarMeasColumn<MPosition> & antPositions = antCols.positionMeas();
	Int nAnt= antCols.position().nrow();
	Vector<MPosition> antPoses( nAnt );
	Vector<Double> xTopo( nAnt ), yTopo( nAnt ), zTopo( nAnt );

	for( uInt i=0; i< nAnt; i++) antPoses( i ) = antPositions( i );
	// find the array center
	String telescope = msc.observation().telescopeName()(0);
	MPosition obs;
	if( MeasTable::Observatory( obs, telescope )){
	   if( obs.type()!= MPosition::ITRF ){
		    MPosition::Convert toItrf( obs, MPosition::ITRF );
			 obs = toItrf( obs );
		}
		// convert the coordinats from the geocentric frame to the topocentric frame
		global2local( obs, antPoses, xTopo, yTopo, zTopo );
		// prepare to create a MemoryTable
		TableDesc td("AntXY","1", TableDesc::New );
		td.comment()="A MemoryTable for antenna local XY plane";
		td.rwKeywordSet().define("XYPlane", "tangential" );
		td.addColumn(ScalarColumnDesc<float>("X-EAST", "Towards east" ));
		td.addColumn(ScalarColumnDesc<float>("Y-NORTH", "Towards north" ));
		SetupNewTable antMaker( td.tableName(), td, Table::New );
		MemoryTable antXY( antMaker, nAnt, False );
		ScalarColumn<float> xCol( antXY, "X-EAST" );
		ScalarColumn<float> yCol( antXY, "Y-NORTH" );
		for( uInt i=0; i<nAnt; i++ ){
		   xCol.put( i, xTopo(i) );
			yCol.put( i, yTopo(i) );
		}
		ants = antXY;
		// we do not try to pass antXY to the TablePlot::TABS_P here because then we will need to
		// copy the subMS object to another variable to keep it in memory( a reference will not
		// work because then if we assign antXY to subMS, the reference to subMS will also be 
		// changed.). That means at a time subMS and a copy of it exist simultaneously. This
		// will take too much memory.
	}else{
	   cout<<"[ MsPlot::plotArray()] MeasTable::Observatory() failed." << endl;
	}
	
}
*/
template<class T>
Int MsPlot<T>::plot( PtrBlock<BasePlot<T>* > &BPS, TPPlotter<T> &TPLP, Int panel )
   {
       // check if the TABS_P has been assigned any table (MS)
       /*if( !m_dataIsSet ){
		     // no sub-dataset has been selected, so set the original MS dataset to TABS_P.
		     Int nTabs = 1;
		     setTableT( nTabs, m_ms );
		     // now the data for plotting has been set, so set the member m_dataIsSet to true.
	        m_dataIsSet = True;	 
		 }*/ // move this part to msplot::plot() if really necessary.
		 // call the plot method inheritated from TablePlot.
		 plotData( BPS, TPLP, panel );
	    return 0;
	}
// help function to transform the coordinates from the global geocentric frame( e.g ITRF ) to
// the local topocentric frame ( origin: observatory=center of antenna array; x-axis: local east;
// y-axis: local north; z-axis: local radial direction. ). Here we are not worrying about the effect
// of the earth's ellipticity even though did use latitude instead of the angle between the earth
// pole and the local radial direction. The error stemed from this is about 1/300.
// Parameter:  
// obervatroy: observatory position in the geocentric frame. It is the origin of the local frame;
// posGeo: MPositions in geocentric frame
// xTopo, yTopo, zTopo: x,y z components of posGet after converting into local topocentric frame.
template<class T>
void MsPlot<T>::global2local( const MPosition& observatory,
                             const Vector<MPosition>& posGeo,
			                     Vector<Double>& xTopo,
			                     Vector<Double>& yTopo,
			                     Vector<Double>& zTopo ) 
{
  uInt ne = posGeo.nelements();
  // store the the (X,Y,Z) after translation, but before rotation.
  Vector<Double> xTrans(ne), yTrans(ne), zTrans(ne);
  Vector<MPosition> posGeoItrf( ne );
  xTopo.resize(ne);
  yTopo.resize(ne);
  zTopo.resize(ne);
  // convert all the MPosition object into ITRF
  //MPosition obsItrf = MPosition( observatory );
  //if( observatory.type()!= MPosition::ITRF ){
  //   MPosition::Convert toItrf( observatory, MPosition::ITRF);
  //   obsItrf = toItrf( observatory );
  //}
  //Vector<Double> XYZ0 = obsItrf.get("m").getValue();
  Vector<Double> XYZ0 = observatory.get("m").getValue();
  Vector<Double> XYZ(3);
  for( uInt i = 0; i< ne; i++ ){
     //if( posGeo(i).type()!= MPosition::ITRF ){
	  if( posGeo(i).type()!= observatory.type() ){
	     // convert MPosition object into the frame type of obervatory if it is not.
	     //posGeoItrf(i) = MPosition::Convert( posGeo(i), MPosition::ITRF )();
		  posGeoItrf(i) = MPosition::Convert( posGeo(i), (MPosition::Types)observatory.type() )();
	  }else{
	     posGeoItrf(i) = posGeo(i);
	  }
     XYZ = posGeoItrf(i).get("m").getValue();
	  // translate the global coordinates compenents to local components without rotating
	  xTrans(i) = XYZ(0) - XYZ0(0);
	  yTrans(i) = XYZ(1) - XYZ0(1);
	  zTrans(i) = XYZ(2) - XYZ0(2);
	  if( m_dbg ){
		cout << "[MsPlot<T>::global2local()] X("<<i<<") = " << xTrans(i) << endl;
		cout << "[MsPlot<T>::global2local()] Y("<<i<<") = " << yTrans(i) << endl;
		cout << "[MsPlot<T>::global2local()] Z("<<i<<") = " << zTrans(i) << endl;
 	 }

  }
  // now rotate the tranlated frame to coincide with the local frame axes
  Vector<Double> angles = observatory.getAngle("rad").getValue();
  Double dLong, dLat;
  dLong = angles(0);
  dLat = angles(1);
  Double cosLong = cos(dLong);
  Double sinLong = sin(dLong);
  Double cosLat = cos(dLat);
  Double sinLat = sin(dLat);

  for (uInt i=0; i< ne; i++) {
    xTopo(i) = -sinLong * xTrans(i) + cosLong * yTrans(i);
    yTopo(i) = -sinLat * cosLong * xTrans(i) - sinLat * sinLong * yTrans(i)  + cosLat * zTrans(i);
    zTopo(i) =  cosLat * cosLong * xTrans(i) + cosLat * sinLong * yTrans(i)  + sinLat * zTrans(i);
	 if( m_dbg ){
		cout << "[MsPlot<T>::global2local()] X("<<i<<") = " << xTopo(i) << endl;
		cout << "[MsPlot<T>::global2local()] Y("<<i<<") = " << yTopo(i) << endl;
 	 }
  }// end of for loop
}; // end of global2local
 	
} //# NAMESPACE CASA - END
// end of file
