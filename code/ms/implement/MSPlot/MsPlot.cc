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
//#include <tables/Tables/ExprNode.h>
namespace casa { //# NAMESPACE CASA - BEGIN
//
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
} //# NAMESPACE CASA - END
// end of file
