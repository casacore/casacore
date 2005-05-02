//# BasePlot.h: Basic table access class for the TablePlot (tableplot) tool
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2002,2003
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
//# You should have receied a copy of the GNU Library General Public License
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


#ifndef BASEPLOT_H
#define BASEPLOT_H

//# Includes

#include <casa/aips.h>
#include <casa/iostream.h>
#include <casa/OS/Timer.h>

#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Array.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/Slicer.h>

#include <tables/Tables/Table.h>
#include <tables/Tables/ArrayColumn.h>
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/TableColumn.h>

#include <tables/Tables/ExprNode.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary>
// Basic table access class for the TablePlot (tableplot) tool
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//#! Classes or concepts you should understand before using this class.
// </prerequisite>

// <etymology>
// BasePlot is the basic class that accesses one table/subtable and extracts
// data to be plotted. It provides the interface between the TablePlot class
// and the actual tables.
// </etymology>

// <synopsis>
// Class BasePlot is the basic table access class for the purpose of plotting
// data via TaQL expressions. A BasePlot object can operate on one table/subtable
// and handles the extraction of data corresponding to TaQL expressions for the
// purpose of plotting. It is to be used in conjunction with the TPPlotter class
// via the TablePlot class, to connect the data from a table to a plotting device.
// 
// </synopsis>

// <example>
// <srcblock>
// // Instantiate BasePlot 
// BasePlot<T> BP();
// BP.Init(Table&);
// BP.CreateTENS(Vector<String> &TaQL);
// BP.GetData();
// ... followed by TPPlotter 'setPlotRange' and 'plotData' calls
// </srcblock>
// </example>

// <motivation>
// This class was written so that each table/subtable can be handled
// independant of each other. This allows simple bookkeeping and clean
// relationships between tables and data sets when multiple tables are
// being simultaneously accessed (plotting/editing). Issues like different 
// numbers of data points to be plotted from different tables, and 
// simultaneous editing on multiple tables/subtables (on one or more
// plot panels) are handled by this class organization.
// </motivation>

// <templating arg=T>
//    <li>
// </templating>

// <thrown>
//    <li>
//    <li>
// </thrown>


// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> Use formal exception handling rather than 'return -1' to handle irrecoverable errors.
//   <li> Store marked flag regions into some sort of flag_history associated with the table/subtable, to be able to later unflag specific flag regions if desired. Modify the clearFlags() function to include this information.
//   <li> Modify to allow user-specified flag column names to be used. Currently only FLAG and FLAG_ROW are used (if either or both exists).
//   <li> Provide the option to flag all channels (or a range) for a chosen range of stokes, or to flag all (or range of) stokes for the chosen channel range.
// </todo>


template<class T> class BasePlot 
{
	public:
		// Constructor
		BasePlot();  

		// Destructor
		~BasePlot();

		// Operator=
		// Equate by reference.
		BasePlot<T>& operator=(const BasePlot<T>&){return *this;}

		// Attach the BasePlot object to a table/subtable.
		// This function also checks if the FLAG and/or FLAG_ROW column names exist.
		Int init(Table &tab);   
		
		// Create TableExprNodes for all TaQL strings.
		// The RecordGram::parse() function is used to 
		// create TableExprNodes for each of the TaQL strings in the
		// input vector. The parse tree for each expression is also
		// traversed to extract table column names and corresponding
		// index ranges that are accessed. This is required while flagging
		// based on the result of a TaQL expression. For instance, in a MS
		// type table, if the 'MEAN' TaQL function is used to average data 
		// over several channels/stokes, only one set of data is plotted,
		// but while flagging all accessed channels/stokes must be flagged.
		// The function returns -1 if there is a TaQL syntax error, or if
		// the data-type of the expression result is not TpDouble, or if
		// an odd number of TaQL strings in sent in the input vector.
		// (Only expressions that return a TpDouble scalar or array can
		// be directly plotted).
		Int createTENS(Vector<String> &datastr);   
		
		// Read data from the table and fill up storage arrays.
		// This function reads the results of all TableExprNodes
		// from the first row of the table/subtable, to obtain the
		// shapes of the TaQL results. Data storage arrays are
		// accordingly resized and the reading continues for all
		// other rows.
		// Flags are read using the getFlags function.
		// If the shape of the data column being accessed matches that of
		// the FLAG column, a one-to-one mapping of flags is done.
		// If the shapes mis-match (or if only one flag exists per row), 
		// then the FLAG_ROW column is read. If neither FLAG nor FLAG_ROW
		// exist, all flags are assumed as False.
		// Errors in TaQL indices are caught and -1 is returned.
		Int getData();      
		
		// This function is callse from TPPlotter::setPlotRange().
		// Set plot range (all plots for this table).
		// Scan the data storage arrays to compute data ranges. In the case 
		// of overlay plots (due to Array TpDouble TaQL results), combined
		// data ranges for this tables data are computed.
		// This function requires that all stored data arrays be traversed.
		// This can get expensive for large number of data points. It is
		// assumed that for such a large number of data points, plotting
		// could broken down into chunks using an iteration axis.
		Int setPlotRange(T &xmin, T &xmax, T &ymin, T &ymax); 
		
		// This function is called from TPPlotter::setFlagRegions().
		// The list of regions that have been marked for flagging
		// via TPPlotter::markFlags() is passed into BasePlot and 
		// stored.
		Int convertCoords(Vector<Vector<T> > &flagmarks);
		
		// Fill in flags in the storage arrays.
		// The data storage arrays are traversed and flags for all
		// data values falling within the chosen flag regions are
		// set to true. If diskwrite=1, updated flags are written to disk.
		// If diskwrite=0, the flags are not written to disk, but will
		// be applied to plots that use the current instance of BasePlot.
		// If rowflag=1, a the FLAG_ROW column is set (if it exists) in
		// addition to the individual flags in the FLAG column (if it exists).
		Int flagData(Int diskwrite, Int rowflag);     
		
		// Clear all flags (FLAG and FLAG_ROW) from the current
		// table/subtable.
		Int clearFlags();   
		
		// This function is called from TPPlotter::thePlot()
		// to identify the plotdata arrays corresponding to data to be plotted
		// on the x and y axes , and corresponding sizes.
		Matrix<Int> getPlotMap();   

		Int NRows_p,NPlots_p; 
		Matrix<T> xplotdata_p,yplotdata_p;
		Matrix<Bool> theflags_p;
		

	private:
		// Get TaQL incides
		Int getIndices(TableExprNode &ten);
		
		// Traverse the parse tree and collect TaQL index ranges
		void ptTraverse(const TableExprNodeRep *tenr);
		
		// Read flags from the table into theflags_p
		Int getFlags(Int rownum);

		// Write flags to disk if desired.
		Int setFlags(Int rowflag);
		
		// Clears currently held flag region lists and TaQL index lists.
		Int cleanUp();      
		
		Table SelTab_p; // table

		Int nTStr_p,nTens_p; // #strings, #string pairs
		Vector<TableExprNode> xtens_p;
		Vector<TableExprNode> ytens_p;
		
		Matrix<Int> Pind_p,Tsize_p; // mapping x-y, #plots per TaQL pair
		Matrix<T> xprange_p, yprange_p; // plot range
		IPosition xshp_p,yshp_p,flagitshp_p;

		Vector<String> colnames_p; // accessed column names
		Vector<Slicer> ipslice_p; // accessed column slices
		Vector<IPosition> colshapes_p; // shapes of accessed cols
		Int nip_p; // number of pairs of colnames and indices.
		Vector<Int> IndCnt_p; // mapping from yplotdata index to colnames_p indices.
		
		ArrayColumn<Bool> Flags_p;
		ScalarColumn<Bool> RowFlags_p;
		String FlagColName_p,FlagRowName_p;
		IPosition FlagColShape_p;
		Bool fcol_p,frcol_p; // flags of FLAG,FLAG_ROW existence.
		
		Vector<Vector<T> > locflagmarks_p; // list of flag regions
		Int nflagmarks_p; // # flag regions

		Int dbg,ddbg,adbg;
		Timer tmr;
		Int zflg,fflg;

};

} //# NAMESPACE CASA - END 

#endif
