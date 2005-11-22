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
#include <casa/BasicSL/String.h>
#include <casa/string.h>
#include <casa/Quanta/Quantum.h>

#include <ms/MSPlot/MsPlot.h>
//
#include <ms/MeasurementSets/MSDataDescIndex.h>
#include <ms/MeasurementSets/MSDataDescColumns.h>
#include <ms/MeasurementSets/MSDataDescription.h>
#include <ms/MeasurementSets/MSColumns.h>
#include <ms/MeasurementSets/MSDerivedValues.h>
//
#include <measures/Measures/Stokes.h>
#include <measures/Measures/MEpoch.h>
//#include <measures/Measures/MCEpoch.h>
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
   :TablePlot<T>(), m_dataIsSet( False ), m_spwExpr( String("")), m_corrExpr( String("")){
	    m_dbg = True;
   }
template <class T> MsPlot<T>::MsPlot( const MeasurementSet &ms )
   :TablePlot<T>(), m_dataIsSet( False ), m_ms( ms ), m_spwExpr( String("")), m_corrExpr( String("")){
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
			  m_antennaNames.resize( antennaNames.nelements() );
			  m_antennaNames = antennaNames;
			  if( m_dbg ){ cout << "[ MsPlot<T>::antennaSelection() ] antennaNames are: " 
			      << MSSelection::nameExprStr( antennaNames) << endl;  }
		  }
		  if( antennaIndex != -1 ){ 
		     if(!(m_select.setAntennaExpr( "'"+MSSelection::indexExprStr( antennaIndex )+"'" ))) return False;
			  m_antennaIndex.resize( antennaIndex.nelements() );
			  m_antennaIndex = antennaIndex;
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
Bool MsPlot<T>::corrSelection( const String& correlations )
   {/*
		if( correlations.compare(String("")) ){ 
		   if(!(m_select.setCorrExpr( MSSelection::nameExprStr( correlations )))) return False;
			if( m_dbg ) cout << "[ MsPlot<T>::corrSelection() ] correlations set. " << endl;
	   } */	
		return True;	
   }
template<class T>
Bool MsPlot<T>::setData( const Vector<String>& antennaNames, const Vector<Int>& antennaIndex,
              const Vector<String>& spwNames, const Vector<Int>& spwIndex,
				  const Vector<String>& fieldNames,  const Vector<Int>& fieldIndex,
				  const Vector<String>& uvDists,
				  const Vector<String>& times,
				  const String& correlations 
            )
	{
	// set selection expression first.
		if( antennaNames[0].compare(String("")) || antennaIndex!=-1 ) {
		   if( m_dbg ) cout << "[ MsPlot<T>::setData() ] calling antennaSelection()..." << endl;
		   if(!antennaSelection( antennaNames, antennaIndex )) return False;
		}
		if( spwNames[0].compare(String("")) || spwIndex!=-1 ) {
		   if( m_dbg ) cout << "[ MsPlot<T>::setData() ] found spw input..." << endl;
			// parse the spwNames[0]
			Vector<Int> spwIndices, chanIndices;
			Vector<String> spwNms(1);
			spwNms[0] = String("");
			Vector<String> chanRange;
			m_spwExpr = spwNames[0];
			if( m_dbg ) cout << "[ MsPlot<T>::setData() ] calling spwParser()..." << endl;
			spwParser( spwNames[0], spwIndices, chanIndices, chanRange );
		   if( m_dbg ) cout << "[ MsPlot<T>::setData() ] after calling spwParser()..." << endl;
			if( m_dbg ){
			   cout<<"[MsPlot::setData] spwName[0] = "<< spwNames[0] << endl;
				cout<<"[MsPlot::setData] spwIndex = " << spwIndex << endl;
				cout<<"[MsPlot::setData] chanIndices = " << chanIndices << endl;
				cout<<"[MsPlot::setData] chanRange = " << chanRange << endl;
			}
		   //if(!spwSelection( spwNames, spwIndex )) return False;
			if(!spwSelection( spwNms, spwIndices )) return False;
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
		if( correlations.compare(String("")) ) {
		   if( m_dbg ) cout << "[ MsPlot<T>::setData() ] calling correlationsSelection()..." << endl;
		   //if(!corrSelection( correlations )) return False;
			m_corrExpr = correlations;
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
// Convert the antenna coordinates into local frame and put the antenna positions into a Table in memory
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
		// convert the coordinates from the geocentric frame to the topocentric frame
		global2local( obs, antPoses, xTopo, yTopo, zTopo );
		// prepare to create a Table in memory
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
		// will take more memory.
		return True;
	}else{
	   cout<<"[ MsPlot::plotArray()] MeasTable::Observatory() failed." << endl;
		return False;
	}
}
// get polarization indices from spw and Stokes type( RR, RL, LR, LL, etc) for
// the first index of DATA[ , ], MODEL[ , ], CORRECTED_DATA[ , ] etc. to 
// plot quantities ( amplitude, phase, or both ) versus uv distance.
// Input: spwIDs;( Stokes Types are obtained within this method from parsing the input corrExpr. 
// Output:: polarIndices
template<class T>
Bool MsPlot<T>::polarIndices( const Vector<Int> spwIDs, /*const Vector<String> polarTypes,*/PtrBlock<Vector<Int>* >& polarsIndices  ){
   Vector<String> stokesNames;
	if( m_corrExpr.compare( String("") )){
	   corrParser( m_corrExpr, stokesNames );
		if( m_dbg ) cout<<"[ MsPlot::polarIndices()] skokesNames = " << stokesNames << endl;
	}else{ // no correlation parameter is inputted. So return a empty poloarsIndices vector
	    polarsIndices.resize(0);
		 return False;
   }
	MSDataDescIndex msDDI( m_ms.dataDescription());
	// get DATA_DESC_IDs from the SPECTRAL_WINDOW_IDs
	if( m_dbg ) cout<<"[MsPlot::polarIndices()] spwIDs = " << spwIDs << endl;
	Vector<Int> descIDs = msDDI.matchSpwId( spwIDs );
	if( m_dbg ) cout<<"[MsPlot::polarIndices()] descIDs = " << descIDs << endl;
	// get the POLARIZATION_IDs from the DATA_DESC_IDs
	ROMSDataDescColumns msddc( m_ms.dataDescription());
	uInt ndesc = descIDs.nelements();
	Vector<Int> polarIDs( ndesc );
	for (uInt i=0; i< ndesc; i++ ){
	   // Note,ArrayColumn takes row number as its index. This row number, however, counts from
		// 0 to totalRow - 1. So it is actually the subtable ID already. That is why we do not need
		// to add 1 to the descIDs[i].
	   polarIDs[i] = msddc.polarizationId()( descIDs[i] );
		if( m_dbg ) cout<<"[MsPlot::polarIndices()] polarIDs[i] = " << polarIDs[i] << endl; 
	}	
	// get the the indices for the first index of DATA[ , ], CORRECTED[ , ]. MODEL[ , ], etc
	// from the POLARIZATION_IDs->the CORR_TYPES and the Stokes types.
	ROMSPolarizationColumns polc( m_ms.polarization());
	polarsIndices.resize( ndesc );
	if( m_dbg ) cout<<"[MsPlot::polarIndices()] polarsIndices.nelements() = " << polarsIndices.nelements() << endl;
	Vector<Int> polIndices;
	Bool anyData = False;
	//	
	for( uInt i=0; i< ndesc; i++ ){
	  const Vector<Int> corrType = polc.corrType()( polarIDs[i] );// see above comment.
	  if( m_dbg ) cout<<"[MsPlot::polarIndices()] corrType = " << corrType << endl;
	  // check if corrType contains the given Stokes names
	  if( containStokes( corrType, stokesNames, polIndices )){
	    if( m_dbg ) cout<<"[MsPlot::polarIndices()] polIndices.nelements() = " << polIndices.nelements() << endl;
	    //polarsIndices[ i ].resize( (uInt)polIndices.nelements() );
		 if( m_dbg ) cout<<"[MsPLot::polarIndices()] polIndices = " << polIndices << endl;
	    polarsIndices[ i ] = new Vector<Int>(polIndices);
		 anyData = True;
		 if( m_dbg ) cout<<"[MsPLot::polarIndices()] *polarsIndices[i] = " << *polarsIndices[i] << endl;
	  }else{
	    polarsIndices[ i ] = NULL; // null length.
		 if( m_dbg ) cout<<"[MsPLot::polarIndices()] This corrType does not include the given Stokes names!" << endl;
	  }
	}
	if( !anyData ){
	   cout<<"[MsPLot::polarIndices()] No data match the given spws and Stokes parameters!" << endl;
		cout<<" So, the correlation selection criteria will be ignored!" << endl;
	   return False;
	}
	if( m_dbg ) cout<<"[MsPLot::polarIndices()] end of this method." << endl;
   return True;
}
// this method provides the data which are needed by msplot::uvdist()
template<class T>
Bool MsPlot<T>::polarNchannel( PtrBlock<Vector<Int>* >& polarsIndices, Vector<Int>& chanIndices, Vector<String>& chanRange ){
   // if the user does not input any spws:channels and correlations, use all the spws:channels and corrlations.
   if( !m_spwExpr.compare("") && !m_corrExpr.compare("") ) return False;
   Vector<Int> spwIndices;
		if( m_spwExpr.compare( String(""))){ 
				spwParser( m_spwExpr, spwIndices, chanIndices, chanRange );
	   }
		// if user does not input spw and channels, use all the spws of the present MS.
		// if user inputted only channels, but no spws. Use all the spws of the MS
		if( !m_spwExpr.compare( String("")) || spwIndices.nelements() == 0){
		   ROMSSpWindowColumns spwc(m_ms.spectralWindow());
	   	uInt nspw = spwc.nrow();
		   spwIndices.resize( nspw );
		   for( uInt i=1; i< nspw+1; i++) spwIndices[i-1] = i;
			if( !m_spwExpr.compare( String("")) ){
			   chanIndices.resize(0);
				chanRange.resize(0);
			} 			
		}
		if( !polarIndices( spwIndices, polarsIndices  ) && chanIndices.nelements() == 0 && chanRange.nelements() == 0 ) return False;
		return True;
}
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
// helper function to transform the coordinates from the global geocentric frame( e.g ITRF ) to
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
	  // translate the global coordinates components to local components without rotating
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
// parse the input String for spw. At present we handle the following four formats:
// spw='(spw1, spw2, ...):(ch1, ch2, ...)' = 'spw1-spw2:ch1-ch2'
//    ='(spw1, spw2, ...):(ch1-ch2)' = '(spw1-spw2):(ch1, ch2, ...)' 
//    ='[spw1, spw2, ...]:(ch1-ch2)' = '(spw1-spw2):[ch1, ch2, ...]'
//    ='(spw1, spw2, ...):[ch1-ch2]' = '(spw1-spw2:ch1, ch2, ...)'
template<class T>
void MsPlot<T>::spwParser( const String& spwExpr, Vector<Int>& spwIndices, Vector<Int>& chanIndices, Vector<String>& chanRange ){
     const String::size_type len = spwExpr.length();
	  uInt nmaxSpw = 60, nmaxChan = 600;
	  String spwArray[nmaxSpw], chanArray[nmaxChan];
     String spws = String("");
	  String channels = String("");

	  // split the input string into the polarisation part and the channel part 
     if( spwExpr.contains(':')){ 
	     if( m_dbg ) cout <<"[MsPlot::spwParser()] spwExpr = " << spwExpr << endl;
	     const String::size_type coluPos = spwExpr.find(':');
	     spws = spwExpr.substr( 0, coluPos );
	     channels = spwExpr.substr( coluPos+1, len-coluPos-1 );
		  if( m_dbg ){
		     cout<< "[MsPlot::spwParser()] spws = " << spws << endl;
		     cout<< "[MsPlot::spwParser()] channels = " << channels << endl;
		  }
	  }else{
	     spws = spwExpr;
		  if( m_dbg ) cout<< "[MsPlot::spwParser()] spws = " << spws << endl;
	  }
	  // split and convert spws into integers
	  if( !spws.compare(String("") )){
	     spwIndices.resize(0);
		  //spwIndex(0) = -1;
	  }else{ 
	  	  // peel off the braces 
		  uInt startPos = 0, endPos = spws.length()-1;
		  uInt subLen = endPos + 1;
		  if( (spws.chars())[0] == '(' || (spws.chars())[0]=='[' ){
		     startPos = 1;
			  subLen = subLen - 1;
		  }
		  if( (spws.chars())[endPos] == ')' || (spws.chars())[endPos] == ']' ) subLen = subLen-1;			     

		  String spwPure = spws.substr( startPos, subLen );
		  if( m_dbg ) cout<< "[MsPlot::spwParser()] spwPure = " << spwPure << endl;
		  // extract every single spw
		  if( spws.contains('-') ){
		     uInt dashPos = spwPure.find('-');
		     String startSpw = spwPure.substr( 0, dashPos );
			  String endSpw = spwPure.substr( dashPos+1, spwPure.length()-dashPos-1 );
			  // convert the range of spw into Vector<uInt>.
			  uInt startSpwN = atoi( startSpw.chars() );
			  uInt endSpwN = atoi( endSpw.chars() );
			  spwIndices.resize( endSpwN-startSpwN+1 );
			  spwIndices( 0 ) = startSpwN;
			  for( uInt i=1; i< endSpwN-startSpwN+1; i++ ) spwIndices(i) = spwIndices(0) + i;
			  if( m_dbg ) {
			      cout << "[MsPlot::spwParser()] spwIndex( endSpwN - startSpwN ) = " << spwIndices( endSpwN-startSpwN ) << endl;
					cout << "[MsPlot::spwParser()] endSpwN =? spwIndices( endSpwN - startSpwN -1 ) =" << endSpwN << endl;
				} 			  
		  }else{
		     if( !spwPure.contains(',') ){
			      spwIndices.resize(1);
					spwIndices[0] = atoi( spwPure.chars());
					if( m_dbg ) cout<<"[MsPlot::spwParser()] spwIndices = " << spwIndices[0] << endl;
			  
		     }else{
			     uInt totalSpwN = split( spwPure, spwArray, nmaxSpw, ',' );
			     // convert spwArray into Vector<uInt>
				  spwIndices.resize( totalSpwN );
			     for( uInt i=0; i< totalSpwN; i++ ){
				    if( m_dbg ) cout<< "[MsPlot::spwParser()] spwArray[i] = " << spwArray[i] << endl;
				    spwIndices( i ) = atoi( spwArray[i].chars());
					 if( m_dbg ) cout<< "[MsPlot::spwParser()] spwIndices[i] = " << spwIndices(i) << endl;
				  }
			  }
		  }
 	  }	  
	  // split and convert channel into integers
	  if( !channels.compare(String("") )){
	     chanIndices.resize(0);
		  chanRange.resize(0);
		  //chanIndices(0) = -1;
	  }else{ 
	  	  // peel off the braces 
		  uInt startPos = 0, endPos = channels.length()-1;
		  uInt subLen = endPos + 1;
		  if( (channels.chars())[0] == '(' || (channels.chars())[0]=='[' ){
		     startPos = 1;
			  subLen = subLen - 1;
		  }
		  if( (channels.chars())[endPos] == ')' || (channels.chars())[endPos] == ']' ) subLen = subLen-1;			     

		  String chanPure = channels.substr( startPos, subLen );
		  if( m_dbg ) cout<< "[MsPlot::spwParser()] chanPure = " << chanPure << endl;
		  // extract every single spw
		  if( chanPure.contains('-') ){
		    /* uInt dashPos = chanPure.find('-');
		     String startChan = chanPure.substr( 0, dashPos-1 );
			  String endChan = chanPure.substr( dashPos+1, chanPure.length()-1 );
			  // convert the range of spw into Vector<uInt>.
			  uInt startChanN = atoi( startChan.chars() );
			  uInt endChanN = atoi( endChan.chars() );
			  chanIndices.resize( endChanN-startChanN+1 );
			  chanIndices( 0 ) = startChanN;
			  for( uInt i=1; i< endChanN-startChanN+1; i++ ) chanIndices(i) = chanIndices(0) + i;
			  */
			  uInt dashPos = chanPure.find('-');
			  chanRange.resize(2);
			  chanRange[0] = chanPure.substr(0, dashPos ); // start channel number.
			  chanRange[1] = chanPure.substr(dashPos+1, chanPure.length()-dashPos-1); // end channel number.
			  chanIndices.resize(0);
			  //chanIndices[0] = -1;
			  if( m_dbg ) {
			      cout << "[MsPlot::spwParser()] chanRange = " << chanRange << endl;
				} 			  
		  }else{
		     if( !chanPure.contains(',') ){
			     chanIndices.resize(1);
				  chanIndices[0] = atoi( chanPure.chars() );			  
			  }else{
		        uInt totalChanN = split( chanPure, chanArray, nmaxChan, "," );
			     if( m_dbg ) cout<< "[MsPlot::spwParser()] chanArray[0] = " << chanArray[0] << endl;
			     // convert spwArray into Vector<uInt>
				  chanIndices.resize( totalChanN );
			     for( uInt i=0; i< totalChanN; i++ ){
				  	 if( m_dbg ) cout<< "[MsPlot::spwParser()] chanArray[i] = " << chanArray[i] << endl;
				    chanIndices( i ) = atoi( chanArray[i].chars());
					 if( m_dbg ) cout<< "[MsPlot::spwParser()] chanIndices[i] = " << chanIndices[i] << endl;
				  }
			 }
			  //chanRange = String("");
			  chanRange.resize(0);
		  }// end of channel split	
		}// end of split channels			   
   } // end of spwParser.
// corrParser parses the input String of corr parameter in the setData(...) method into Stokes type names defined
// in the Stokes class. The input is in the format "RR LL RL", or "[RR LL RL]" or "(RR LL RL)" different types of
// polarizations is separated by space.
template<class T>
void MsPlot<T>::corrParser( const String& corrExpr, Vector<String>& stokesNames ){
	if( m_dbg ) cout<<"[MsPlot::corrParser()] corrExpr = " << corrExpr << endl;
		uInt startPos = 0, endPos = corrExpr.length() - 1;
		uInt sbLen = endPos + 1;
		if( (corrExpr.chars())[0] == '(' || (corrExpr.chars())[0]=='[' ){		 
		   startPos = 1;
			sbLen = sbLen - 1;
		}
		if( (corrExpr.chars())[endPos] == ')' || (corrExpr.chars())[endPos] == ']' ) sbLen = sbLen-1;
		String corrExprPure = corrExpr.substr( startPos, sbLen );
		if( m_dbg ) cout<<"[MsPlot::corrParser()] corrExprPure = " << corrExprPure << endl;
		Int nmax = (uInt)Stokes::NumberOfTypes;
		String stokesNm[ nmax ];			
      uInt totalStokes = split( corrExprPure, stokesNm, nmax, " " );
		stokesNames.resize( totalStokes );
		for( uInt i=0; i<totalStokes; i++ ){
		   stokesNames[ i ] = stokesNm[i];
			if( m_dbg ) cout<<"[MsPlot::corrParser()] stokesNames[i] = " << stokesNames[i] << endl;
		}
}// end of corrParser	
// check if the corrType vector contains the Stokes types related to the given stokes names. If yes, return true. 
// If not, return False. Also return False if invalid Stokes types is inputted. If the corrType contains all the
// given stokes names, the correponding polarIndices in the corrTypes vector are returned as polarIndices.	
template<class T>
Bool MsPlot<T>::containStokes( const Vector<Int> corrType, const Vector<String>& stokesNames, Vector<Int>& polarIndices ){
     //const String name = stokesName;
	uInt nsn = stokesNames.nelements();
	Vector<Int> polIndices( nsn, -1 );
   for( uInt k=0; k<nsn; k++ ){
     Stokes::StokesTypes thisType = Stokes::type( stokesNames[k] );
	  if( m_dbg ) cout<<"[MsPlot::containStokes()] thisType = " << thisType << endl;
	  if( thisType == 0 ){
	    cout<<"MsPlot<T>::containStokes()] Invalid Stokes type entered!" << endl;
		 return False;
	  }
	  uInt ncorr = corrType.nelements();
	  if( m_dbg ) cout<<"[MsPlot::containStokes()] ncorr = " << ncorr << endl;
	  for( uInt i=1; i< ncorr+1; i++ ){
	     if( m_dbg ) cout<<"[MsPlot::containStokes()] corrType[i-1] = " << corrType[i-1]<< endl;
	     if( (uInt)corrType[i-1] == (uInt)thisType ){
		    if( m_dbg ) cout<<"[MsPlot::containStokes()] polIndices[ k ] = " << polIndices[ k ] << endl;
		    polIndices[ k ] = i;
			 if( m_dbg ) cout<<"[MsPlot::containStokes()] polIndices[ k ] = " << polIndices[ k ] << endl;
			 break;
		  }
	  }
	  if( m_dbg ) cout<<"[MsPlot::containStokes()] polIndices[ k ] = " << polIndices[ k ] << endl;
	  if( polIndices[k] == -1 ) return False;
	}
	polarIndices.resize( nsn );
	polarIndices = polIndices;
	return True;
}// end of containStokes
// This method calculated derived quantities, such as hour angle, azimuth, elevation,
// paralactic angle, etc.
template<class T>
Bool MsPlot<T>::derivedValues( const Vector<Double>& times, Vector<Double>& derivedQuan, const String& quanType ){
   // TIME from MAIN table of MS is in seconds.
   MSDerivedValues msd;
   //ROMSColumns msc( m_ms );
	//const ROMSAntennaColumns & antCols  = msc.antenna();
	//const ROScalarMeasColumn<MPosition> & antPositions = antCols.positionMeas();
   //uInt nAnt= antCols.position().nrow();
	//Vector<MPosition> antPoses( nAnt );
	//Vector<Double> xTopo( nAnt ), yTopo( nAnt ), zTopo( nAnt );
	//for( uInt i=0; i< nAnt; i++) antPoses( i ) = antPositions( i );

   // If the user wants to use the selected antennas
   if( m_antennaNames.nelements() || m_antennaIndex.nelements() ){
	   cout<<"[MsPlot::hourAngle()] To be implemented. " << endl;
	}else{ // if the user did not select antennas, use all the antennas
  	   ROMSColumns msc( m_ms );
		const ROMSAntennaColumns & antCols  = msc.antenna();
		msd.setAntennas( antCols );
		
      // the following block is needed by parAngle()
		Int nAnt=antCols.nrow();
      Vector<String> mount(nAnt);
      for (Int ant=0; ant<nAnt; ant++) {
        mount( ant) = antCols.mount()(ant);
      }
		msd.setAntennaMount( mount );
	}
	
	//if( m_fieldNames.nelements() || m_fieldIndex.nelements() ){
	//   cout<<"[MsPlot::hourAngle()] To be implemented. " << endl;
	//}else{ // if the user did not select antennas, use all the antennas
  	   ROMSColumns* msc;
		//const MSField msfield;
		if( m_dataIsSet ){
	 	  msc = new ROMSColumns( m_subMS );
		}else{
		  msc = new ROMSColumns( m_ms );
		}
		Vector<Int> fieldId = msc->fieldId().getColumn();
	 //}
	 Vector<Double> l_times = msc->time().getColumn();
	 //times.resize( l_times.nelements() );
	 //times = l_times; 
	 //uInt nt = times.nelements();
	  uInt nt = l_times.nelements();
	 uInt curFieldId = 0; 
	 uInt lastFieldId = 0;
	 for ( uInt k=0; k<nt; k++) {
	   curFieldId = fieldId( k );
	   if (curFieldId!=lastFieldId) {
		   if (msc->field().numPoly()(curFieldId)==0)
		   {  msd.setFieldCenter( msc->field().phaseDirMeas(curFieldId));  }
	      if (msc->field().numPoly()(curFieldId)>0)
	      {  msd.setFieldCenter( msc->field().phaseDirMeas(curFieldId,times(k)));  }
		}
		// TIME from MAIN table of MS is in seconds.
		// Quantity qt(times(k)/C::day, "day");
		// Quantity qt( l_times(k)/C::day, "day" );
		Quantity qt( l_times(k), "s" );
		MEpoch mep( qt );
      msd.setEpoch( mep );
		derivedQuan.resize( nt );
	   if( !quanType.compare( "hourAngle" )){
	      //derivedQuan( k ) = msd.hourAngle()/C::_2pi*C::day;
			derivedQuan( k ) = msd.hourAngle()*180.0/C::_2pi; // converted to degree
			if( m_dbg ) cout<<"[ MsPlot::derivedValues() ] hour angle = " << derivedQuan(k) << endl;
	   }else if( !quanType.compare( "azimuth" ) ){
			derivedQuan( k ) = msd.azel().getAngle("deg").getValue("deg")(0);
			if( m_dbg ) cout<<"[ MsPlot::derivedValues() ] azimuth = " << derivedQuan(k) << endl;
		}else if( !quanType.compare( "elevation" )){
		   derivedQuan( k ) = msd.azel().getAngle("deg").getValue("deg")(1);
			if( m_dbg ) cout<<"[ MsPlot::derivedValues() ] elevation = " << derivedQuan(k) << endl;
		}else if( !quanType.compare( "parallaticAngle" )){
		   // derivedQuan( k ) = msd.parAngle()/C::_2pi*C::day;
			derivedQuan( k ) = msd.parAngle()*180.0/C::_2pi; // converted to degree
			if( m_dbg ) cout<<"[ MsPlot::derivedValues() ] parallactic angle = " << derivedQuan(k) << endl;
		}else{
		   cout<<"[ MsPlot::derivedValues() ] Invalid quanType inputted. " << endl;
		}
		lastFieldId=curFieldId;
	 }// end of for loop
	 delete msc;
	 return True;
  }// end of hourAngles()
} //# NAMESPACE CASA - END
// end of file
