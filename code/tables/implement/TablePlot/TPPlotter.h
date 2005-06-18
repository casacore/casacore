//# TPPlotter.h: Plotter class for the TablePlot (tableplot) tool
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


#ifndef TPPLOTTER_H
#define TPPLOTTER_H

//# Includes

#include <casa/aips.h>
#include <casa/BasicSL/String.h>
#include <casa/Containers/Record.h>
#include <casa/System/PGPlotter.h>

#include <tables/TablePlot/BasePlot.h>

// <summary>
// Plotter class for the TablePlot (tableplot) tool
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//#! Classes or concepts you should understand before using this class.
//   <li> BasePlot
//   <li> PGPlotter
// </prerequisite>

// <etymology>
// TPPlotter is the class that controls all plot operations
// for the TablePlot (TP) class.
// </etymology>

// <synopsis>
// Class TPPlotter is the plotting interface that the TablePlot class
// uses. It has to be used along with the BasePlot and TablePlot classes.
// It currently uses the PGPlotter class interface to the pgplot
// plotting package.
// 
// </synopsis>

// <example>
// <srcblock>
// // Instantiate a TPPlotter
// TPPlotter<T> TPLP();
// TPLP.SetPlotOptions(...);
// TPLP.SelPlotRange(...);
// TPLP.PlotData(...);
// </srcblock>
// </example>

// <motivation>
// This class was written to keep all plotting function calls and
// display window management issues independant of the BasePlot class
// that handles the actual tables. Also if the current plotting 
// package has to be replaced, changes need be made only in this one
// class. The TablePlot manipulates the TPPlotter objects.
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
//   <li> Add additional plotting window interaction commands/calls accessible
//        via PlotOptions.
//   <li> Allow glish-level access to the choice of plotting device
//   <li> Add functions to display sensible metadata info and labels on the plots.
//   <li> Optional labelling on multi-panel plots
//   <li> Provide the option of plotting with/without flagging and/or plotting only flagged/unflagged data.
//   <li> Provide functions to clear a plot/panel window, or hold the current plot for an overlay from another plot command. 
//   <li> Provide a 'rubber-band' facility to mark regions. The basic PGPlotter class currently does not emulate the corresponding PGPLOT function.
//   <li> Use formal exception handling rather than 'return -1' to handle irrecoverable errors.
// </todo>

#define TP_PGPLOT
//#define TP_PLPLOT

namespace casa { //# NAMESPACE CASA - BEGIN

template<class T> class TPPlotter 
{
	public:
		// Constructor
		TPPlotter(); 
		
		// Destructor
		~TPPlotter();
		
		// Set Plotting options from an input record of plot options.
		// 
		// Record field names and default values : 
		// <li> nxpanels = 1
		// <li> nypanels = 1
		// <li> windowsize = nxpanels * 5.0 (inches)
		// <li> aspectratio = nypanels/nxpanels
		// <li> plotstyle = 1 (points)
		// <li> plotcolour = 2 (start with red and change for each overlay plot)
		// <li> fontsize = 2.0 * nxpanels * nypanels (font size needs to increase
		//            for multiple panel plots.
		Int setPlotOptions(Record &plotoptions);
		
		// Set Title, X-axis Label,Y-axis Label for the plot.
		// The Vector of Strings must have 'title','xlabel','ylabel'
		// in this order.
		// In multiple panel plots, the same labels are applied to
		// all panels. (This will be improved to provide more options).
		Int setLabels(Vector<String> &labels);
		
		// Consolidate ranges from all BasePlots and set plot range.
		// For the chosen panel, this function queries all the
		// BasePlot objects held in the input list for the ranges
		// of data being plotted from each BasePlot object. It then
		// combines these ranges along with zooming ranges (if applicable)
		// to create the final world-coordinate plot range for the
		// panel.
		Int setPlotRange(PtrBlock<BasePlot<T>* > &PBP,Int panel);  
		
		// Read data from Baseplots and plot the data.
		// For the chosen panel, this function reads the data from
		// all BasePlots in the input list, applies the stored flags,
		// and 
		// (Will be improved to optionally plot with/without applying
		// flags, or plotting only flagged or unflagged data)
		Int plotData(PtrBlock<BasePlot<T>* > &PBP,Int &panel);      
		
		// Mark a rectangular region on the chosen panel with two
		// mouse clicks at diagonally opposite corners.
		// This function grabs two mouse click events via the plotting
		// package, draws a hatched rectangle on the plot panel, and 
		// appends the area co-ordinates to a list of flag regions
		// maintained per panel in the TPPlotter class.
		Int markFlags(Int panel);
		
		// Passes on the list of marked flag regions (created by single
		// or multiple calls to MarkFlags) to each of the tables/subtables
		// in the input list of BasePlot objects.
		Int setFlagRegions(PtrBlock<BasePlot<T>* > &PBP,Int panel);
		
		// Mark a rectangular region on the chosen panel with two
		// mouse clicks at diagonally opposite corners.
		// This function grabs two mouse click events via the plotting
		// package, and modifies a panel-wise list of zoom plot ranges
		// which come into effect when SetPlotRange is next called.
		// <li> direction = 1 : zoom
		// <li> direction = 0 : unzoom. 
		// Note : successive zooming can be done. Flagging can be done
		// while zoomed. Unzooming at any stage returns to the 
		// original full plot range.
		Int markZoom(Int panel,Int direction);
		
		// Clear a Plot window (currently does nothing except clearing
		// the stored list of marked flag regions.
		Int clearPlot();
		
	private:
		// Do the actual plotting. Read xdata,ydata,flags from the input
		// BasePlot object, accumulate data-to-be-plotted (usually unflagged
		// data) into plot arrays and call the plotting package draw routines.
		// (Modify (if possible) to not have to accumulate data into
		// separate plot arrays before calling the plot draw routines. This
		// would be trivial if the plotting package draw routines could work
		// with a (Bool) mask.)
		Int thePlot(BasePlot<T> &BP,Int ibp, Int XRow, Int YRow, Int colour, Int ptype, Int panel);

		// Set plot environment parameters for a panel.
		// Sets up the viewport and window (world) coordinates from SetFlagRegions
		Int setWinEnv(Int panel);

		// Advances to a particular panel
		// For use with plotting packages (like PGPLOT) that do not easily
		// return information about which panel a mouse event has occured in.
		Int gotoPanel(Int panel);

		// The following are Plotting Package Specific functions.
		
		// Initialize the plotter object
		Int initPlot();
		
		// Set number of panels, windowsize, aspectratio, plot style/colour, fontsize.
		Int setOptions(Int nxpanels, Int nypanels, Float windowsize, Float aspectratio, Int plotstyle, Int plotcolour, Float fontsize);
		
		//Set Plot labels from internally stored Vector of label strings.
		Int setPlotLabels();

		// Mark a Rectangular region (currently via 2 mouse clicks) and
		// draw a hatched rectangle to mark the region.
		Int markRegion(Int panel, T &xmin,T &xmax, T &ymin, T &ymax);

		// Manage memory for plot arrays into which data-to-be-plotted is
		// accumulated while plotting.
		Int allocPlotArrays(Int size);
		
		// Call the plotting function draw routines.
		Int plotXY(Int col, Int ch);

	#ifdef TP_PLPLOT	
		plstream *pls;
		PLFLT *x,*y;
	#endif
	#ifdef TP_PGPLOT	
		Vector<T> x_p,y_p;
		PGPlotter *pgp_p;
	#endif
		
		Int nRanges_p;
		T Xmin_p, Xmax_p, Ymin_p, Ymax_p;
		T nxmin_p,nxmax_p,nymin_p,nymax_p,expan_p;
		Int pcnt_p;
		String XLabel_p,YLabel_p,Title_p;
		Int nxpanel_p,nypanel_p;
		Int NPanels_p;
		Float windowsize_p,aspectratio_p,fontsize_p;
		Int plotstyle_p,plotcolour_p;
		
		Int nflagmarks_p;
		PtrBlock<Vector<Vector<T > >* > flaglist_p;

		Vector<Vector<T> > panelPrange_p;
		Vector<Vector<T> > panelZrange_p;
		Vector<Int> panelZflag_p;
		
		Int ddbg,adbg;
};

} //# NAMESPACE CASA - END 

#ifndef AIPS_NO_TEMPLATE_SRC
#include <tables/TablePlot/TPPlotter.cc>
#endif //# AIPS_NO_TEMPLATE_SRC
#endif
