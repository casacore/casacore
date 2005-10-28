//# TablePlot.h: Implement class for the tableplot DO.
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

#ifndef TABLEPLOT_H
#define TABLEPLOT_H

//# Includes
#include <casa/aips.h>
#include <tables/Tables/TableIter.h>
#include <tables/Tables/TableError.h>

#include <tables/TablePlot/BasePlot.h>
#include <tables/TablePlot/CrossPlot.h>
#include <tables/TablePlot/TPPlotter.h>

// <summary>
// Implement class for the TablePlot (tableplot) tool.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//#! Classes or concepts you should understand before using this class.
//   <li> BasePlot
//   <li> TPPlotter
// </prerequisite>

// <etymology>
// The TablePlot class allows plotting of data from any kind of
// table.
// </etymology>

// <synopsis>
// Class TablePlot allows the plotting of data from any kind of table 
// using generic TaQL expressions to specify the data to be
// plotted. Access to the tables (reading/writing) is controlled via
// the BasePlot class (one basePlot object per table), and all
// plotting operations (plotting/editing) are controlled via the
// TPPlotter class. The TablePlot class controls the use of the
// BasePlot and TPPlotter classes, and provides an interface that
// application programs (DO) can directly use. 
//
// Relation between an application, TablePlot, BasePlot and TPPlotter : 
//
// The application code constructs and holds the TablePlot object, the TPPlotter object,
// and a list of BasePlot objects.
//
// <li> One TPPlotter object is used for one plot window. 
//   Single and multiple panels are supported.
// 
// <li> One BasePlot object attaches to and operates on one single table/subtable.
//   If the TaQL expressions for the attached table, result in TpDouble Arrays.
//   and correspond to multiple data sets to plot, an overlay plot is created.
//   
// <li> TablePlot accepts a list of BasePlot objects (one for each table being simultaneously
//   accessed) and a TPPlotter object from the application, 
//   and controls the mapping of the BasePlot objects to the supplied TPPlotter
//   objects. After the data is read from tables into all BasePlots, the list is passed
//   to the TPPlotter class, which reads the data to be plotted from all the BasePlots
//   and (currently) creates an overlay plot of data from all the tables in the list.
//   In the case of multi-panel plots, TablePlot accepts a list of BaseBlot lists,
//   and controls the mapping of each BasePlot list to a separate panel.
// 
//  
// This design allows the following.
//
// <li> Access to Table data and the Plotting package are independant of each other.
// <li> TaQL expressions can be applied to multiple tables and data from multiple tables
//      are kept independant of each other.
// <li> Editing operations on multiple panels of a plot window are handled independantly.
// <li> Design supports the idea of data parallelization and chunking of data to avoid
//      the simultaneous use of large amounts of memory.
// <li> Since the application level holds the BasePlot object lists and TPPlotter objects,
//      control of mapping BasePlot lists to a TPPlotter (list) is at the application
//      level.
// </synopsis>

// <example>
// (See DOtableplot.cc)
// Create a Single-Panel plot.
// <srcblock>
// // Instantiate a TablePlot object, Vector of BasePlots, and a TPPlotter obj.
// TablePlot<T> TP();
// TPPlotter<T> TPLP();
// PtrBlock<BasePlot<T>* > BPS;
// //'ntabs' is the number of tables to simultaneously operate on.
// //'tablenames' is a vector of table names as strings
// TP.nTabs = ntabs; 
// for(Int i=0;i<TP.nTabs;i++) TP.setTableS(tablenames[i]);
// // Set plot options and labels.
// // 'PlotOption' is a record with 'nxpanels','nypanels',etc..
// // 'labels' is a Vector of 3 strings (title,xlabel,ylabel).
// // This call can be omitted - default settings will be used.
// TP.setPlotParameters(TPLP,PlotOption,labels);
// // 'datastr' is a Vector of TaQL strings.
// // Create the BP objects - first string is used to decide type of plot.
// TP.createBP(BPS,datastr[0]);
// // Attach the tables to the BP objects
// TP.upDateBP(BPS);
// // Get data and plot it.
// TP.GetData(BPS,datastr);
// TP.PlotData(BPS,TPLP,1);
// </srcblock>
//
// To create a multi-panel plot for use with an iteration axis
// <srcblock>
// // Instantiate a TablePlot object, Vector of Vector of BasePlots, and a TPPlotter obj.
// TablePlot<T> TP();
// TPPlotter<T> TPLP();
// PtrBlock<PtrBlock<BasePlot<T>* >*> ATBPS;
// //'ntabs' is the number of tables to simultaneously operate on.
// //'tablenames' is a vector of table names as strings
// TP.nTabs = ntabs; 
// for(Int i=0;i<TP.nTabs;i++) TP.setTableS(tablenames[i]);
// // Set plot options and labels.
// // 'PlotOption' is a record with 'nxpanels','nypanels',etc..
// // 'labels' is a Vector of 3 strings (title,xlabel,ylabel).
// // This call can be omitted - default settings will be used.
// TP.setPlotParameters(TPLP,PlotOption,labels);
// // Start the iterations.
// TP.iterMultiPlotStart(ATBPS,TPLP,NPanels,datastr,iteraxes);
// // To advance to the next set of panels,
// ret = TP.iterMultiPlotNext(ATBPS,TPLP,apanels);
// if(ret==0){//end of iterations.};
// for(Int i=0;i<apanels;i++){
// if(TP.getData(*ATBPS[i],DataStr)) return -1;
// TP.plotData(*ATBPS[i],TPLP,i+1);
// }
// // To end the iterations and cleanup
// TP.iterMultiPlotStop(ATBPS,TPLP);
// </srcblock>
//
// To edit data via flagging...
// <srcblock>
// // To mark a flag regions in panel P. 
// TP.markFlags(P,TPLP);
// // To flag all marked regions on a single panel plot
// TP.flagData(BPS,TPLP,1,diskwrite,rowflag)
// // To flag all marked regions on a multi-panel plot
// for(Int p=0;p<NPanels;p++) 
// TP.FlagData(*ATBPS[p],TPLP,p+1,diskwrite,rowflag);
// </srcblock>

// To zoom on a panel P
// <srcblock>
// TP.markZoom(P,TPLP,direction);
// // If single panel plot
// TP.PlotData(BPS,TPLP,1);
// // If multi-panel plot
// TP.PlotData(*ATBPS[P-1],TPLP,panel);
// </srcblock>
// </example>

// <motivation>
// This class was written to provide a high-level interface to an application
// that needs to plot/edit data from multiple tables at once. Individual panels can
// be handled independant of each other. BasePlot and TPPlotter objects are owned
// by the application (persistant obj) and used by the TablePlot class.
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
//   <li> Add functions to read out (into glish arrays) the data being plotted.
//   <li> Add functionality to allow a choice between multipanel and single panel overlay plots for the plotData function.
// </todo>


namespace casa { //# NAMESPACE CASA - BEGIN

//enum { XYPLOT, CROSSPLOT, HISTPLOT };
	
template<class T> class TablePlot 
{
	public:
		// Constructor
		TablePlot();

		// Destructor
		~TablePlot();
		
		// Create a table object from the input string, and add to a list
		// of tables that are to be simultaneously acted upon.
		// Use this function when entire tables are to be accessed by directly
		// sending in the table name.
		Int setTableS(Int ntabs, String &inTabName);  
		
		// Add this table to the list of tables that are to be simultaneously 
		// acted upon. Use this function when table/subtable object references
		// already exist, and need to be used in plotting. For example, a table
		// selection can be performed to create a subtable which can then be 
		// passed in for plotting.
		Int setTableT(Int ntabs, Table &inTabObj);    
		
		// Instantiate a BasePlot/CrossPlot object for each table.
		// To be used in conjunction with upDateBP(). The second argument
		// should contain 'CROSS' for a CrossPlot object to be instantiated.
		// Otherwise (or left as default), BasePlot objects will be created.
		// One way to use this is to have the incoming TAQL pair have its 'X' TaQL
		// contain 'CROSS', since for CrossPlots, the x-axis corresponds to
		// column indices of the array column selected by the Y-TaQL.
		Int createBP(PtrBlock<BasePlot<T>* > &BPS, String pType = String("XYPLOT")); 
		
		// Attach each table in the list to a BasePlot object. The first time this
		// function is called, it must be preceded by createBP(). Successive calls
		// to this function only change the tables/subtables being accessed. This
		// feature is used in DOtableplot for plotting while iterating over an
		// iteration axis - subtables created in each iteration are assigned to a
		// fixed vector of BasePlot objects.
		Int upDateBP(PtrBlock<BasePlot<T>* > &BPS); 

		// Send in plotting options and plot labels. Plotting options are given as an
		// input record with the following fields. If this function is omitted, default
		// input parameters are used. 
		//
                // Record field names and default values :
                // <li> nxpanels = 1
                // <li> nypanels = 1
                // <li> windowsize = nxpanels * 5.0 (inches)
                // <li> aspectratio = nypanels/nxpanels
                // <li> plotstyle = 1 (points)
                // <li> plotcolour = 2 ( if single digit, that is the colour of all overlay
		//      plots. If double digit, the first digit is the colour of the first plot
		//      and the second digit is an increment for the colour index that is used
		//      to determine the colour for each successive overlay. )
                // <li> fontsize = 2.0 * nxpanels * nypanels (font size needs to increase
                //            for multiple panel plots.
                // <li> linewidth = 2 
		// <li> timeplot = 0 (timeplot=1 uses pgplot->tbox to label the x-axis
		//			values using HH::MM::SS (currently not functional))
		// <li> plotsymbol = 1 (dots)
		// <li> plotrange = [xmin,xmax,ymin,ymax] (vector). This overrides the
		//                  data ranges if they lie within it.
		// <li> useflags = 0 (0:plot only unflagged data, 1:plot only flagged data
		//		       2:plot both flagged and unflagged data in diff colours)
		//
		// The 'labels' parameter is a vector of strings with any of or all
		// three of the strings 'title','xlabel','ylabel' in this order.
                // In multiple panel plots, the same labels are applied to
                // all panels. (This will be improved to provide more options).
		// Default labels are blank strings.
		Int setPlotParameters(TPPlotter<T> &TPLP,Record &plotoptions,Vector<String> &labels);
		
		// Read the data from the tables.
		// Input TaQL strings via 'datastr' are used to create TableExprNode objects. 
		// 'datastr' must have an even number of TaQL strings corresponding to
		// x-y pairs of data to be plotted. 
		// 
		// Valid TaQL strings must satisfy the following conditions.
		// Each TaQL string must result in a Double scalar or array.\\
		// <li> 'AMPLITUDE(DATA[1,1])' results in a Double scalar (valid). \\
		// <li> 'AMPLITUDE(DATA[1:2,1])' results in a Double array (valid).\\
		// <li> 'MEAN(AMPLITUDE(DATA[1:2,1]))' results in a Double scalar (valid).\\
		// <li> 'DATA[1,1]' results in a Complex scalar (NOT valid).\\
		// <li> 'AMPLITUDE(DATA[1,1])<10' results in a Bool scalar (NOT valid).
                //
		// All TaQL functions resulting in Double Scalars/Arrays are allowed,
                // except for those involving an explicit collapse axis (means,sums,etc..).
		// Note that these functions are different from mean,sum,etc.. which are supported.
                //
		// TaQL strings must be provided as pairs of strings, with the
		// X-TaQL first, followed by the Y-TaQL. There are 3 cases.\\
		// <li> X-TaQL - Scalar, Y-TaQL - Scalar (one-to-one single plot)\\
		// <li> X-TaQL - Scalar, Y-TaQL - Array (one-to-many overlay plot)\\
		// <li> X-TaQL - Array, Y-TaQL - Array (if the shapes are
		// the same, then a one-to-one mapping is done, otherwise only the first 
		// X-TaQL result is used for a one-to-many mapping with the Y-TaQL Array.)
		//
		// The xdata,ydata and flags are then read into storage arrays, ready 
		// to be plotted. With multi-panel plots, this function must be called
		// once per panel.
		//
		// For 'cross' plots, only the Y-TaQL is used to read out the data.
		// The x-values are the indices of the array column selected by the
		// Y-TaQL.
		Int getData(PtrBlock<BasePlot<T>* > &BPS,Vector<String> &datastr);      
		
		// Read the data from the storage arrays filled in via getData(), and 
		// create a plot on one panel. For multi-panel plots this function must
		// be called once per panel.
		Int plotData(PtrBlock<BasePlot<T>* > &BPS,TPPlotter<T> &TPLP, Int panel);     
		
		// Mark regions to flag. This function calls TPPlotter::markFlags().
		Int markFlags(Int panel,TPPlotter<T> &TPLP);    
		
		// Mark region to zoom. This function calls TPPlotter::markZoom().
		Int markZoom(Int panel,TPPlotter<T> &TPLP,Int direction);    
		
		// For a specific panel, flag the data corresponding to regions marked
		// using markFlags(). If diskwrite=1, the flags are written to the disk. 
		// Otherwise only the in-memory flag arrays get updated with flags and 
		// successive plots reflect them. If rowflag=1, then if the FLAG_ROW
		// column exists flags will be set for the entire row of data in addition
		// to the individual data flags in the FLAG column. If these columns do not
		// exist, then this option is ignored.
		Int flagData(PtrBlock<BasePlot<T>* > &BPS,TPPlotter<T> &TPLP,Int panel, Int diskwrite, Int rowflag);     
		
		// Clear all flags from all the tables attached to the vector of BasePlots
		// (will be modified to provide a flag-history and do selective unflagging.)
		Int clearFlags(PtrBlock<BasePlot<T>* > &BPS);   

		// Plotting from a series of subtables created by iterating over a specified
		// iteration axis. This mode of plotting supports multiple panels, with the
		// panel configuration set via setPlotParameters(). 
		// This function initializes a TableIterator and a vector of
		// vector of BasePlots, mapping each BasePlot vector to a separate panel. 
		Int iterMultiPlotStart(PtrBlock<PtrBlock<BasePlot<T>* >* > &ATBPS, TPPlotter<T> &TPLP, Int npanels,Vector<String> &datastr,Vector<String> &iteraxes);
		
		// Advances to the next iteration. BasePlot objects persist, but their attached
		// tables are replaced by the new subtables created by TableIter.
		Int iterMultiPlotNext(PtrBlock<PtrBlock<BasePlot<T>* >* > &ATBPS, TPPlotter<T> &TPLP,Int &apanels);
		
		// Terminates the iterations. It is called automatically when the end of the table is
		// reached. It can also be called before the iterations end, to cleanly terminate the
		// iterations when desired.
		Int iterMultiPlotStop(PtrBlock<PtrBlock<BasePlot<T>* >* > &ATBPS, TPPlotter<T> &TPLP);

		// Number of simultaneously accessed Tables - sizeof(TABS)
		// To be set at the application level.
		Int nTabs_p; 
		          		
	private:

		Vector<Table> TABS_p;
		
		Int iTabs_p;

		Vector<TableIterator> Iters_p;
		Int panelcounter_p;
		Int NPanels_p;
		
		Int adbg;
		
};

} //# NAMESPACE CASA - END 

#ifndef AIPS_NO_TEMPLATE_SRC
#include <tables/TablePlot/TablePlot.cc>
#endif //# AIPS_NO_TEMPLATE_SRC
#endif
