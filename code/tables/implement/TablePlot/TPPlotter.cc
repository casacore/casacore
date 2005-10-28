//# TPPlotter.cc: Plotter class for the TablePlot (tableplot) tool
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

#include <cmath>
#include <casa/iostream.h>
#include <casa/OS/Timer.h>

#include <tables/TablePlot/TPPlotter.h>

#define TP_PGPLOT
//#define TP_PLPLOT

namespace casa {  //# NAMESPACE CASA - BEGIN

#define TMR(a) "[User: " << a.user() << "] [System: " << a.system() << "] [Real: " << a.real() << "]"
#define MIN(a,b) ((a)<(b) ? (a) : (b))
#define MAX(a,b) ((a)>(b) ? (a) : (b))

#define YPAN(pan,nx,ny) ( (pan%ny)!=0 ? (pan%ny) : (ny) )
#define XPAN(pan,nx,ny) ( 1+ (pan-YPAN(pan,nx,ny))/(ny) )

/* Default Constructor */
template<class T> TPPlotter<T>::TPPlotter()
{
	ddbg=0; adbg=0;
	if(adbg)cout << "TPPlotter constructor" << endl;
#ifdef TP_PLPLOT	
	pls=NULL; x=NULL; y=NULL;
#endif	
#ifdef TP_PGPLOT	
	pgp_p=NULL; 
#endif	
	
	panelPrange_p.resize(0);
	panelZrange_p.resize(0);
	panelZflag_p.resize(0);
	flaglist_p.resize(0);
	nflagmarks_p=0;
	XLabel_p="XLabel"; YLabel_p="YLabel"; Title_p="Title";
	nxmin_p=0;nxmax_p=0;nymin_p=0;nymax_p=0;expan_p=0.02;
	nxpanel_p=1; nypanel_p=1; NPanels_p=1;

	timeplot_p = 1;
	plotrange_p.resize();
	pr_p = IPosition(1,0);
}

/*********************************************************************************/

/* Destructor */
template<class T> TPPlotter<T>::~TPPlotter()
{
	if(adbg)cout << "TPPlotter destructor" << endl;
#ifdef TP_PLPLOT	
	if(pls != NULL) delete pls;
#endif	
#ifdef TP_PGPLOT	
	if(pgp_p != NULL)
	{
		pgp_p->ask(0);
		pgp_p->detach();
		if(adbg)cout << "Detached !" << endl;
		delete pgp_p;
	}
#endif	
	/* Explicitly clean up PtrBlock data members*/
	if(flaglist_p.nelements()>0) 
		for(Int i=0;i<NPanels_p;i++) {flaglist_p[i]->resize(0);delete flaglist_p[i];}
	flaglist_p.resize(0);
}

/*********************************************************************************/

/* Clear a Plot - current only a place-holder, and cleans the flagmark list */
template<class T> Int TPPlotter<T>::clearPlot()
{
	if(adbg)cout << "clearPlot" << endl;
	if(flaglist_p.nelements()>0) 
		for(Int i=0;i<NPanels_p;i++) {flaglist_p[i]->resize(0);delete flaglist_p[i];}
	flaglist_p.resize(0);
	nflagmarks_p=0;
	
	return 0;
}

/*********************************************************************************/

/* Query all BasePlots in the list and consolidate all plot ranges */
template<class T> Int TPPlotter<T>::setPlotRange(PtrBlock<BasePlot<T>* > &PBP,Int panel)
{
	if(adbg)cout << "TPPlotter :: Set Global Plot Range" << endl;

#ifdef TP_PLPLOT
	if(pls==NULL) 
#endif
#ifdef TP_PGPLOT
	if(pgp_p==NULL) 
#endif
	{
		cout << "Need to Initialize the Plotter first ! " << endl;
		initPlot();
	}
	
	nRanges_p = PBP.nelements();
	if(ddbg)cout << "nRanges_p = n BPs.. = " << nRanges_p << endl;
	
	/* Query all BasePlots in the list for individual plot ranges */
	Xmin_p=1e+30; Xmax_p=-1e+30; Ymin_p=1e+30; Ymax_p=-1e+30;
	T xmin=0,xmax=0,ymin=0,ymax=0;
	for(Int i=0;i<nRanges_p;i++) 
	{
		PBP[i]->setPlotRange(xmin,xmax,ymin,ymax,useflags_p);
		Xmin_p = MIN(Xmin_p,xmin);
		Xmax_p = MAX(Xmax_p,xmax);
		Ymin_p = MIN(Ymin_p,ymin);
		Ymax_p = MAX(Ymax_p,ymax);
	}

	/* Expand ranges by 2% so that data points at the plot edge do not
	 fall on the plot boundary lines. */
	nxmin_p = Xmin_p - expan_p*(Xmax_p-Xmin_p);
	nxmax_p = Xmax_p + expan_p*(Xmax_p-Xmin_p);
	nymin_p = Ymin_p - expan_p*(Ymax_p-Ymin_p);
	nymax_p = Ymax_p + expan_p*(Ymax_p-Ymin_p);

	/* Store plot range for this panel*/
	(panelPrange_p[panel-1])[0] = nxmin_p;
	(panelPrange_p[panel-1])[1] = nxmax_p;
	(panelPrange_p[panel-1])[2] = nymin_p;
	(panelPrange_p[panel-1])[3] = nymax_p;
	
	/* Check if zooming is turned on for this panel.*/
	if(panelZflag_p[panel-1])
	{
		if(adbg)cout << " WITH zoom " << endl;
	}
	else
	{
		if(adbg)cout << " WITHOUT zoom " << endl;
		
		(panelZrange_p[panel-1])[0] = nxmin_p;
		(panelZrange_p[panel-1])[1] = nxmax_p;
		(panelZrange_p[panel-1])[2] = nymin_p;
		(panelZrange_p[panel-1])[3] = nymax_p;

	}
		
	adjustPlotRange(panel);		
	
	if(adbg)cout << "EnvRanges : " << panel << " :: " << nxmin_p << " " << nxmax_p << " "  << nymin_p << " " << nymax_p << endl;
	
	/* Set up plot environment : viewport/window.*/
	setWinEnv(panel);

	return 0;
}

/*********************************************************************************/

/* Plot data */
template<class T> Int TPPlotter<T>::plotData(PtrBlock<BasePlot<T>* > &PBP,Int &panel)
{
	if(adbg)cout << "TPPlotter :: Plot the Data" << endl;
	Int colourcount=2;
	Int colourinc = 0;
	if(plotcolour_p > 1 && plotcolour_p < 10) 
	{
		colourcount = plotcolour_p;
		colourinc = 0;
	}
	else if(plotcolour_p > 9 ) 
	{
		colourinc = plotcolour_p%10;
		colourcount = (plotcolour_p - colourinc)/10;
	}

	/* Call 'thePlot' for all tables within each BasePlot in the list.*/
	for(Int i=0;i<nRanges_p;i++)
	{
		for(Int tt=0;tt<PBP[i]->getNumPlots();tt++)
		{
			if(useflags_p > 0) thePlot(*PBP[i],i,tt,12,0,panel,1); // plot flags in purple!

			if(useflags_p != 1) thePlot(*PBP[i],i,tt,colourcount,0,panel,0);
			colourcount+=colourinc;
			colourcount = colourcount%10;
			if(colourcount==0) colourcount=10;
		}
		colourcount++;
		colourcount = colourcount%10;
		if(colourcount==0) colourcount=10;
	}


	return 0;
}

/*********************************************************************************/
/* Mark Flag Region */
template<class T> Int TPPlotter<T>::markFlags(Int panel)
{
	if(adbg)cout << "TPPlotter :: markFlags " << endl;
	
	T x1=0,x2=0,y1=0,y2=0;
	Int cpanel=0;

	gotoPanel(panel);
	if(markRegion(panel,x1,x2,y1,y2)==-1) return -1;
	
	cpanel=panel;
	
	/* creating a new little vector each time... */
	Vector<T> tlim(4) ;
	tlim[0] = x1;
	tlim[1] = x2;
	tlim[2] = y1;
	tlim[3] = y2;
	
	/* Append to the list of marked flag regions */
	nflagmarks_p = flaglist_p[cpanel-1]->nelements();
	flaglist_p[cpanel-1]->resize(nflagmarks_p+1,True);
	(*flaglist_p[cpanel-1])[nflagmarks_p] = tlim;
	
	if(adbg)cout << "nflagmarks_p in panel " << cpanel << " is " << nflagmarks_p+1 << endl;
	if(adbg)for(Int i=0;i<nflagmarks_p+1;i++)
	cout << (*flaglist_p[cpanel-1])[i] << endl;
	
	return 0;
}

/*********************************************************************************/

/* Mark region to zoom on */
template<class T> Int TPPlotter<T>::markZoom(Int panel,Int direction)
{
	if(adbg)cout << "TPPlotter :: markZoom " << endl;
	//Int cpanel=0;
	T x1=0,x2=0,y1=0,y2=0;

	if(direction)
	{
		if(adbg)cout << "Zoom.." << endl;
		
		gotoPanel(panel);
		if(markRegion(panel,x1,x2,y1,y2)==-1) return -1;
		//cpanel=panel;//// get rid of one variable 

		/* Set ZoomRange to chosen region. */
		panelZflag_p[panel-1] = 1;
		(panelZrange_p[panel-1])[0] = x1;
		(panelZrange_p[panel-1])[1] = x2;
		(panelZrange_p[panel-1])[2] = y1;
		(panelZrange_p[panel-1])[3] = y2;
		
		adjustPlotRange(panel);
		
		return (panel);
	}
	else
	{
		if(adbg)cout << "Unzoom panel : " << panel << endl;
		gotoPanel(panel);

		/* Set ZoomRange to original full plot region */
		panelZflag_p[panel-1]=0;
		for(Int j=0;j<4;j++) (panelZrange_p[panel-1])[j] = (panelPrange_p[panel-1])[j];
		
		adjustPlotRange(panel);
		
		return(0);
	}
}

/*********************************************************************************/

/* Adjust plot ranges */
template<class T> Int TPPlotter<T>::adjustPlotRange(Int panel)
{
	if( (panelZrange_p[panel-1])[0] == (panelZrange_p[panel-1])[1] )
	{
		(panelZrange_p[panel-1])[0] -= 0.1;
		(panelZrange_p[panel-1])[1] += 0.1;
	}
	
	if( (panelZrange_p[panel-1])[2] == (panelZrange_p[panel-1])[3] )
	{
		(panelZrange_p[panel-1])[2] -= 0.1;
		(panelZrange_p[panel-1])[3] += 0.1;
	}

	pr_p[0]=0;panelZrange_p[panel-1][0] = MAX(panelZrange_p[panel-1][0],plotrange_p(pr_p));
	pr_p[0]=1;panelZrange_p[panel-1][1] = MIN(panelZrange_p[panel-1][1],plotrange_p(pr_p));
	pr_p[0]=2;panelZrange_p[panel-1][2] = MAX(panelZrange_p[panel-1][2],plotrange_p(pr_p));
	pr_p[0]=3;panelZrange_p[panel-1][3] = MIN(panelZrange_p[panel-1][3],plotrange_p(pr_p));

	
	if(adbg) cout << "range for panel " << panel << ": " << panelZrange_p[panel-1][0] << " , " << panelZrange_p[panel-1][1] << " , " << panelZrange_p[panel-1][2] << " , " << panelZrange_p[panel-1][0] << endl;
	
	return 0;
}
/*********************************************************************************/

/* Send flag region list to all BasePlots */
template<class T> Int TPPlotter<T>::setFlagRegions(PtrBlock<BasePlot<T>* > &PBP,Int panel)
{
	if(adbg)cout << "TPPlotter :: setFlagRegions for panel : " << panel << endl;
	
	for(Int i=0;i<nRanges_p;i++)
	{
		if(adbg)for(Int j=0;j<(Int)flaglist_p[panel-1]->nelements();j++) 
			cout << i << " " << (*(flaglist_p[panel-1]))[j] << endl;

		PBP[i]->convertCoords(*(flaglist_p[panel-1]));
	}
	
	flaglist_p[panel-1]->resize(0);

	return 0;
}

/*********************************************************************************/

/* Set Plot Options */
template<class T> Int TPPlotter<T>::setPlotOptions(Record &plotoptions)
{
	if(adbg)cout << "TPPlotter :: setPlotOptions" << endl;
	
	/* Parse the input record and set options. defaults are provided.*/
	if(plotoptions.isDefined("nxpanels"))
	{
		RecordFieldId ridnx("nxpanels");
		plotoptions.get(ridnx,nxpanel_p);
	}else nxpanel_p = 1;

	if(plotoptions.isDefined("nypanels"))
	{
		RecordFieldId ridny("nypanels");
		plotoptions.get(ridny,nypanel_p);
	}else nypanel_p = 1;

	if(plotoptions.isDefined("windowsize"))
	{
		RecordFieldId ridwin("windowsize");
		plotoptions.get(ridwin,windowsize_p);
	}else 	windowsize_p = nxpanel_p*5.0; 

	if(plotoptions.isDefined("aspectratio"))
	{
		RecordFieldId ridasp("aspectratio");
		plotoptions.get(ridasp,aspectratio_p);
	}else aspectratio_p = (nypanel_p/nxpanel_p); 

	if(plotoptions.isDefined("plotstyle"))
	{
		RecordFieldId ridsty("plotstyle");
		plotoptions.get(ridsty,plotstyle_p);
	}else plotstyle_p = 1;

	if(plotoptions.isDefined("plotcolour"))
	{
		RecordFieldId ridcol("plotcolour");
		plotoptions.get(ridcol,plotcolour_p);
	}else plotcolour_p = 1;

	if(plotoptions.isDefined("fontsize"))
	{
		RecordFieldId ridfont("fontsize");
		plotoptions.get(ridfont,fontsize_p);
	}else fontsize_p = 2.0 * nxpanel_p*nypanel_p;
	
	if(plotoptions.isDefined("linewidth"))
	{
		RecordFieldId ridfont("linewidth");
		plotoptions.get(ridfont,linewidth_p);
	}else linewidth_p = 2;

	if(plotoptions.isDefined("timeplot"))
	{
		RecordFieldId ridfont("timeplot");
		plotoptions.get(ridfont,timeplot_p);
	}else timeplot_p = 0;

	if(plotoptions.isDefined("plotsymbol"))
	{
		RecordFieldId ridfont("plotsymbol");
		plotoptions.get(ridfont,plotsymbol_p);
	}else plotsymbol_p = 1;

	if(plotoptions.isDefined("plotrange"))
	{
		RecordFieldId ridfont("plotrange");
		//cout << "DATATYPE : " << plotoptions.dataType(ridfont) << endl;
		plotoptions.get(ridfont,plotrange_p);
	}else 
	{
		plotrange_p.resize(IPosition(1,4));
		pr_p[0] = 0;	plotrange_p(pr_p) = -1e+30;
		pr_p[0] = 1;	plotrange_p(pr_p) = +1e+30;
		pr_p[0] = 2;	plotrange_p(pr_p) = -1e+30;
		pr_p[0] = 3;	plotrange_p(pr_p) = +1e+30;
	}
	if(ddbg)cout << " Plotrange : " << plotrange_p << endl;

	if(plotoptions.isDefined("useflags"))
	{
		RecordFieldId ridfont("useflags");
		plotoptions.get(ridfont,useflags_p);
	}else useflags_p = 0;
	// 0 : plot only unflagged data
	// 1 : plot only flagged data
	// 2 : plot flagged and unflagged data (diff colours)

	
#ifdef TP_PLPLOT
	if(pls==NULL) 
#endif
#ifdef TP_PGPLOT
	if(pgp_p==NULL) 
#endif
	{
		/* Initialize the plotter */
		initPlot();
	}
	else
	{
		/* Clean up bookkeeping vectors and flag region lists.*/
		for(Int i=0;i<NPanels_p;i++) 
		{
			flaglist_p[i]->resize(0);delete flaglist_p[i];
			panelZflag_p[i]=0;
			panelZrange_p[i].resize(0);
			panelPrange_p[i].resize(0);
		}
	}
	
	/* Calculate number of panels */
	NPanels_p = nxpanel_p*nypanel_p;
	
	/* Resize panel bookkeeping vectors (small vectors) */
	flaglist_p.resize(NPanels_p);
	panelZflag_p.resize(NPanels_p);
	panelZrange_p.resize(NPanels_p);
	panelPrange_p.resize(NPanels_p);
	
	for(Int i=0;i<NPanels_p;i++) 
	{
		flaglist_p[i] = new Vector<Vector<T> >();
		flaglist_p[i]->resize(0);
		panelZflag_p[i]=0;
		panelZrange_p[i].resize(4);
		for(Int j=0;j<4;j++) (panelZrange_p[i])[j]=0;
		panelPrange_p[i].resize(4);
	}
	
	/* Set Plotting options */
	setOptions(nxpanel_p, nypanel_p, windowsize_p, aspectratio_p, plotstyle_p, plotcolour_p, fontsize_p, linewidth_p);
		
	return 0;
}

/*********************************************************************************/
/* Set Labels ! */
template<class T> Int TPPlotter<T>::setLabels(Vector<String> &labels)
{
	if(adbg)cout << "TPPlotter :: Set Labels" << endl;
	if(labels.nelements()>0)Title_p = labels[0] ;
	else Title_p = "";
	if(labels.nelements()>1)XLabel_p = labels[1];
	else XLabel_p = "";
	if(labels.nelements()>2)YLabel_p = labels[2];
	else YLabel_p = "";
	return 0;
}

/*********************************************************************************/
/*********************************************************************************/

/* Read data from BasePlots, apply flags and do the actual plotting */
template<class T> Int TPPlotter<T>::thePlot(BasePlot<T> &BP,Int ibp, Int Pnum, Int colour, Int ptype, Int panel, Int flagged)
{
	T xval,yval;
	Int NRows = BP.getNumRows();

	if(ddbg)cout << "About to allocate the PLFLT arrays of size : " << NRows << endl;
	
	allocPlotArrays(NRows);

	// 'flagged' = 1 --> plot flagged data
	// 'flagged' = 0 --> plot unflagged data
	
	/* Fill up the Plot Arrays with data-to-be-plotted. */
	pcnt_p=0;
	for(Int rc=0;rc<NRows;rc++)
	{
		if(BP.getYFlags(Pnum,rc) == Bool(flagged)) 
		{
			xval = BP.getXVal(Pnum,rc);
			yval = BP.getYVal(Pnum,rc);
		
			if(xval>(panelZrange_p[panel-1])[0] && xval<=(panelZrange_p[panel-1])[1] && 
			yval > (panelZrange_p[panel-1])[2] && yval <= (panelZrange_p[panel-1])[3])
			{
				x_p[pcnt_p] = xval;
				y_p[pcnt_p] = yval;
				pcnt_p++;
			}
		}
	}

#ifdef TP_PGPLOT
	x_p.resize(pcnt_p,True);
	y_p.resize(pcnt_p,True);
#endif

	if(adbg)cout << "Plotting : " << pcnt_p << " points ...." << endl;
	setPlotLabels();
	

	/* Plot */
	switch(ptype)
	{
		case 0: /* points */
			plotXY(colour,flagged?1:plotsymbol_p);
			break;
		case 1: /* lines */
			plotXY(colour,flagged?1:plotsymbol_p);
			break;
		case 2: /* histogram */
			break;
	}

	
	return 0;
}

/*********************************************************************************/
/********************* PLOTTING FUNCTIONS ************************/
/*********************************************************************************/

template<class T> Int TPPlotter<T>::setWinEnv(Int panel)
{
#ifdef TP_PGPLOT	
	Int xpan = XPAN(panel,nxpanel_p,nypanel_p);
	Int ypan = YPAN(panel,nxpanel_p,nypanel_p);
	if(adbg)cout << "Advance to panel : " << panel << endl;
	
	pgp_p->panl(xpan,ypan);
	pgp_p->eras();
	pgp_p->sci(1);
	
	pgp_p->svp(0.1,0.9,0.2,0.8);
	pgp_p->swin((panelZrange_p[panel-1])[0],(panelZrange_p[panel-1])[1],(panelZrange_p[panel-1])[2],(panelZrange_p[panel-1])[3]);
	if(timeplot_p)pgp_p->tbox("BCNST",0.0,0,"BCNST",0.0,0);
	else pgp_p->box("BCNST",0.0,0,"BCNST",0.0,0);

	
#endif
#ifdef TP_PLPLOT	
	if(adbg)cout << "Advance to panel : " << panel << endl;
	//if(panel==1) panel = NPanels_p;
	//pls->adv(panel-1);
	//if(panel==1) {pls->eop();pls->bop();}
	
	pls->adv(panel);
	pls->clear();
	
	pls->col0(15);
	pls->vpor(0.1,0.9,0.15,0.85);
	pls->wind((panelZrange_p[panel-1])[0],(panelZrange_p[panel-1])[1],(panelZrange_p[panel-1])[2],(panelZrange_p[panel-1])[3]);
	pls->box("bcnt",0,0,"bcnt",0,0);

	//pls->env0((panelZrange_p[panel-1])[0],(panelZrange_p[panel-1])[1],(panelZrange_p[panel-1])[2],(panelZrange_p[panel-1])[3], 0, 0);
	
#endif
	return 0;
}

/*********************************************************************************/

template<class T> Int TPPlotter<T>::markRegion(Int panel, T &xmin,T &xmax, T &ymin, T &ymax)
{
#ifdef TP_PGPLOT	

	cout << " Choose Region to Mark... Click at top-left and bottom-right" << endl;
	Float xx=0,yy=0;
	String ch;
	Record rec;

	Vector<T> xbox(5),ybox(5);
	Vector<T> px(1),py(1);
	
	pgp_p->sci(1);
     
	rec = pgp_p->curs(xx,yy);
     
	RecordFieldId fidx1("x");
   	rec.get(fidx1,xx);
 	RecordFieldId fidy1("y");
	rec.get(fidy1,yy);
    	RecordFieldId fidc1("ch");
   	rec.get(fidc1,ch);
	
	if(xx < (panelZrange_p[panel-1])[0] || xx >(panelZrange_p[panel-1])[1] || yy < (panelZrange_p[panel-1])[2] || yy >(panelZrange_p[panel-1])[3])
	{
		cout << " Click was on incorrect panel. No action taken  " << endl;
		return -1;
	}

     
  	px[0]=xx; py[0]=yy;
 	pgp_p->pt(px,py,2);
	
    	if(adbg)cout << " xx : " << xx << "   yy : " << yy << "  ch : " << ch << endl;

       	xbox[0]=xx; ybox[0]=yy;
        xbox[1]=xx; 
         
        	    ybox[3]=yy;
        xbox[4]=xx; ybox[4]=yy;

    	rec = pgp_p->curs(xx,yy);
     
  	RecordFieldId fidx2("x");
    	rec.get(fidx2,xx);
   	RecordFieldId fidy2("y");
  	rec.get(fidy2,yy);
 	RecordFieldId fidc2("ch");
	rec.get(fidc2,ch);
     
	if(xx < (panelZrange_p[panel-1])[0] || xx >(panelZrange_p[panel-1])[1] || yy < (panelZrange_p[panel-1])[2] || yy >(panelZrange_p[panel-1])[3])
	{
		cout << " Click was on incorrect panel. No action taken  " << endl;
		return -1;
	}
    	
	px[0]=xx; py[0]=yy;
   	pgp_p->pt(px,py,2);
     
  	if(adbg)cout << " xx : " << xx << "   yy : " << yy << "  ch : " << ch << endl;
        
                    ybox[1]=yy;
        xbox[2]=xx; ybox[2]=yy;
        xbox[3]=xx; 
         
	pgp_p->sfs(3);
	pgp_p->line(xbox,ybox);
	pgp_p->poly(xbox,ybox);

	T x1,x2,y1,y2;
	x1 = xbox[0]; y1 = ybox[0];
	x2 = xbox[2]; y2 = ybox[2];
	
	T temp;
	if(x1>x2){temp=x1; x1=x2; x2=temp;}
	if(y1>y2){temp=y1; y1=y2; y2=temp;}
	if(ddbg) cout << x1 << " , " <<  y1 << " , " << x2 << " , " << y2 << endl;

	xmin=x1;
	xmax=x2;
	ymin=y1;
	ymax=y2;
	

#endif
#ifdef TP_PLPLOT	
	PLINT xpanel,ypanel,panel1,panel2;
	PLGraphicsIn pos1,pos2;
	
	// make this an infinite loop - so that the box moves.
	cout << " Choose region to mark ***" << endl;
	pls->GetCursor(&pos1);
	cout << "World coordinates : " << pos1.wX << " , " << pos1.wY << endl;
	pls->GetCursor(&pos2);
	cout << "World coordinates : " << pos2.wX << " , " << pos2.wY << endl;

	// use pls->gspa() to get subpage boundaries in abs coords...

	ypanel = nypanel_p - (Int)(pos1.dY*nypanel_p);
	xpanel = (Int)(pos1.dX*nxpanel_p)+1;
	panel1 = (ypanel-1)*nxpanel_p + xpanel;
	
	ypanel = nypanel_p - (Int)(pos2.dY*nypanel_p);
	xpanel = (Int)(pos2.dX*nxpanel_p)+1;
	panel2 = (ypanel-1)*nxpanel_p + xpanel;
	
	cout << "panels 1 and 2: " << panel1 << "  " << panel2 << endl;
	if(panel1!=panel2) 
	{
		cout << "ERROR : region spans multiple panels..." << endl;
		return -1;
	}
	panel = panel1;
	cout << "Current panel : " << panel << endl;
	
	pls->adv(panel);
	pls->col0(15);
	pls->vpor(0.1,0.9,0.1,0.9);
	pls->wind((panelZrange_p[panel-1])[0],(panelZrange_p[panel-1])[1],(panelZrange_p[panel-1])[2],(panelZrange_p[panel-1])[3]);
	//pls->env0((panelZrange_p[panel-1])[0],(panelZrange_p[panel-1])[1],(panelZrange_p[panel-1])[2],(panelZrange_p[panel-1])[3], 0, 0);
	
	
	PLFLT xbox[5],ybox[5];
	xbox[0]=pos1.wX; ybox[0]=pos1.wY;
	xbox[1]=pos1.wX; ybox[1]=pos2.wY;
	xbox[2]=pos2.wX; ybox[2]=pos2.wY;
	xbox[3]=pos2.wX; ybox[3]=pos1.wY;
	xbox[4]=pos1.wX; ybox[4]=pos1.wY;
	
	pls->col0(5);
	pls->line(5,xbox,ybox);
	
	pls->col0(1);
	pls->psty(3);
	
	pls->fill(4,xbox,ybox);
	pls->flush();

	T x1,x2,y1,y2;
	x1 = pos1.wX; y1 = pos1.wY;
	x2 = pos2.wX; y2 = pos2.wY;
	
	T temp;
	if(x1>x2){temp=x1; x1=x2; x2=temp;}
	if(y1>y2){temp=y1; y1=y2; y2=temp;}
	if(ddbg) cout << x1 << " , " <<  y1 << " , " << x2 << " , " << y2 << endl;

	xmin=x1;
	xmax=x2;
	ymin=y1;
	ymax=y2;
	
#endif
	
	return 0;
}

/*********************************************************************************/

template<class T> Int TPPlotter<T>::gotoPanel(Int panel)
{
#ifdef TP_PGPLOT	
	pgp_p->panl(XPAN(panel,nxpanel_p,nypanel_p),YPAN(panel,nxpanel_p,nypanel_p));
	pgp_p->sci(1);
	pgp_p->svp(0.1,0.9,0.2,0.8);
	pgp_p->swin((panelZrange_p[panel-1])[0],(panelZrange_p[panel-1])[1],(panelZrange_p[panel-1])[2],(panelZrange_p[panel-1])[3]);
	if(timeplot_p)pgp_p->tbox("BCNST",0.0,0,"BCNST",0.0,0);
	else pgp_p->box("BCNST",0.0,0,"BCNST",0.0,0);
	
#endif
	
#ifdef TP_PLPLOT	
	pls->adv(panel);
#endif
	
	return 0;
}
/*********************************************************************************/

template<class T> Int TPPlotter<T>::setOptions(Int nxpanels, Int nypanels, Float windowsize, Float aspectratio, Int plotstyle, Int plotcolour, Float fontsize, Int linewidth)
{
#ifdef TP_PGPLOT	
	pgp_p->pap(windowsize,aspectratio);
	pgp_p->sch(fontsize);
	pgp_p->slw(linewidth);
	pgp_p->subp(nxpanels,nypanels);
#endif
	
#ifdef TP_PLPLOT	
	pls->ssub(nxpanels,nypanels);
	pls->eop();
	pls->bop();
#endif
	
	return 0;
}

/*********************************************************************************/

template<class T> Int TPPlotter<T>::initPlot()
{
#ifdef TP_PGPLOT	
	if(adbg)cout << " Init PGPLOT " << endl;
	String dev = "/xw";
	if(adbg)cout << "dev : " << dev << endl;
	
	pgp_p = new PGPlotter("/xw");
	
	if(adbg)cout << "is attached : " << pgp_p->isAttached() << endl;
	if(!pgp_p->isAttached()) return(0);

	pgp_p->pap(5.0,1.0);

#endif
#ifdef TP_PLPLOT	
	cout << " Init PLPLOT" << endl;
	pls = new plstream();
	//char devn[5];
	//devn[0]='x'; devn[1]='w'; devn[2]='i'; devn[3]='n'; devn[4]='\0';
	char devn[3];
	devn[0]='t'; devn[1]='k'; devn[2]='\0';
	pls->sdev(devn);
	//pls->ssub(nxpanel,nypanel);
	pls->init();
#endif
	
return 0;
}

/*********************************************************************************/

template<class T> Int TPPlotter<T>::setPlotLabels()
{
#ifdef TP_PGPLOT	

	pgp_p->sci(1);
	pgp_p->lab(XLabel_p,YLabel_p,Title_p);
#endif
#ifdef TP_PLPLOT	
	pls->col0(15);
	pls->lab(XLabel_p.chars(),YLabel_p.chars(),Title_p.chars());
#endif
	return 0;
}

/*********************************************************************************/

template<class T> Int TPPlotter<T>::allocPlotArrays(Int size)
{
#ifdef TP_PGPLOT	
	x_p.resize(size);
	x_p.set(0.0);
	y_p.resize(size);
	y_p.set(0.0);
#endif
#ifdef TP_PLPLOT	
	x_p = (PLFLT*)realloc(x_p,sizeof(PLFLT)*size);
	y_p = (PLFLT*)realloc(y_p,sizeof(PLFLT)*size);
	
#endif
	return 0;
}
	
	
/*********************************************************************************/

template<class T> Int TPPlotter<T>::plotXY(Int col,Int ch)
{
#ifdef TP_PGPLOT	
	pgp_p->sci(col);
	pgp_p->pt(x_p,y_p,ch);
#endif
#ifdef TP_PLPLOT	
	pls->col0(col);
	pls->poin(pcnt_p, x, y, ch);
	pls->flush();
#endif
	return 0;
}
/*********************************************************************************/
}//# NAMESPACE CASA - END 

