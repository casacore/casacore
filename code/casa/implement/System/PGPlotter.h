//# PGPlotter.h: Standard plotting object for application programmers.
//# Copyright (C) 1997
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
//#
//# $Id$

#if !defined(AIPS_PGPLOTTER_H)
#define AIPS_PGPLOTTER_H

#include <aips/aips.h>
#include <trial/Tasking/PGPlotterInterface.h>
#include <aips/Utilities/CountedPtr.h>

class String;
template<class T> class Vector;

// <summary>
// Standard plotting object for application programmers.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="PGPlotterInterface">PGPlotterInterface</linkto>
// </prerequisite>
//
// <synopsis>
// This is the class that a programmer should instantiate if he wants to open
// a "device" to plot to. The device might be local, or it might be remote
// (i.e., running under Glish). The philosophy of the plotting interface is
// described in the 
// <linkto class="PGPlotterInterface">PGPlotterInterface</linkto> documentation.
//
// It is possible that the object might not be attached to a valid plot device
// (for example, the user might have said "no plotting." Programerss should 
// check the <src>isAttached()</src> member before plotting. If you attempt to
// plot to an unattached plotter, an exception is thrown.
//
// Copying a <src>PGPlotter</src> uses reference semantics -- after copying
// plotting on the old and new objects will result in the plot commands 
// appearing on the same device. The device is closed only when the last 
// reference is destructed.
// </synopsis>
//
// <example>
// <srcblock>
//     // plot y = x*x
//     Vector<Float> x(100), y(100);
//     indgen(x);
//     y = x*x;

//     PGPlotter plotter("myplot.ps/ps");
//     plotter.env(0, 100, 0, 100*100, 0, 0);
//     plotter.line(x, y);
// </srcblock>
// </example>
//
// <todo asof="1997/12/31">
//   <li> Add more plot calls.
// </todo>


class PGPlotter : public PGPlotterInterface
{
public:
    // The default constructor does not attach to any plotter, that is
    // <src>isAttached()</src> returns False. An exception is thrown if you 
    // attempt to plot to an unattached PGPlotter.
    PGPlotter();

    // Open "device", which must be a valid PGPLOT style device, for example
    // <src>/cps</src> for colour postscript (or <src>myfile.ps/cps</src>
    // if you want to name the file), or <src>/xs</src> or <src>/xw</src> for
    // and X-windows display. This constructor (only) causes
    // PGPLOT and everything PGPLOT requires to be linked in to the current
    // executable.
    // <thrown>
    //   <li> An <linkto class="AipsError">AipsError</linkto> will be thrown
    //        if the underlying PGPLOT open fails for some reason.
    // </thrown>
    PGPlotter(const String &device);

    // Copies use reference semantics, i.e. after copying the new and old
    // copy both draw onto the same surface.
    // <group>
    PGPlotter(const PGPlotter &other);
    PGPlotter &operator=(const PGPlotter &other);
    // </group>

    // If this is the last reference, close the plot.
    virtual ~PGPlotter();

    // True if it is OK to plot to this object.
    Bool isAttached() const;

    // This is not a standard PGPLOT command. In the Glish/PGPLOT window, it
    // puts a message in the message line. By default it sends it to the logger.
    // In any event, this is intended for short helpful messages (e.g.
    // saying which keys to press to mark a spectrum).
    virtual void message(const String &text);

    // This is an emulated standard PGPLOT command. It returns a record
    // containing the fields:
    // <srcblock>
    // [ok=Bool, x=Float, y=Float, ch=String];
    // If the remote device cannot do cursor feedback, ok==F.
    // </srcblock>
    virtual Record curs(Float x, Float y);

    // Standard PGPLOT commands. Documentation for the individual commands
    // can be found in the Glish manual and in the standard PGPLOT documentation
    // which may be found at <src>http://astro.caltech.edu/~tjp/pgplot/</src>.
    // The Glish/PGPLOT documentation is preferred since this interface follows
    // it exactly (e.g. the array sizes are inferred both here and in Glish,
    // whereas they must be passed into standard PGPLOT).
    // <thrown>
    //   <li> An <linkto class="AipsError">AipsError</linkto> will be thrown
    //        if the plotter is unattached.
    // </thrown>
    // <group>
    virtual void arro(Float x1, Float y1, Float x2, Float y2);
    virtual void ask(Bool flag);
    virtual void bbuf();
    virtual void box(const String &xopt, Float xtick, Int nxsub, 
	     const String &yopt, Float ytick, Int nysub);
    virtual void circ(Float xcent, Float ycent, Float radius);
    virtual void conl(const Matrix<Float> &a, Float c,
		      const Vector<Float> &tr, const String &label,
		      Int intval, Int minint);
    virtual void cont(const Matrix<Float> &a, const Vector<Float> &c,
		      Bool nc, const Vector<Float> &tr);
    virtual void ctab(const Vector<Float> &l, const Vector<Float> &r,
		      const Vector<Float> &g, const Vector<Float> &b,
		      Float contra, Float bright);
    virtual void draw(Float x, Float y);
    virtual void ebuf();
    virtual void env(Float xmin, Float xmax, Float ymin, Float ymax, Int just,
	     Int axis);
    virtual void eras();
    virtual void errb(Int dir, const Vector<Float> &x, const Vector<Float> &y,
	      const Vector<Float> &e, Float t);
    virtual void erry(const Vector<Float> &x, const Vector<Float> &y1,
	      const Vector<Float> &y2, Float t);
    virtual void gray(const Matrix<Float> &a, Float fg, Float bg,
		      const Vector<Float> &tr); 
    virtual void hist(const Vector<Float> &data, Float datmin, Float datmax, 
		    Int nbin, Int pcflag);
    virtual void iden();
    virtual void imag(const Matrix<Float> &a, Float a1, Float a2,
		      const Vector<Float> &tr);
    virtual void lab(const String &xlbl, const String &ylbl, 
		   const String &toplbl);
    virtual void line(const Vector<Float> &xpts, const Vector<Float> &ypts);
    virtual void move(Float x, Float y);
    virtual void mtxt(const String &side, Float disp, Float coord, Float fjust,
		    const String &text);
    virtual void page();
    virtual void poly(const Vector<Float> &xpts, const Vector<Float> &ypts);
    virtual void pt(const Vector<Float> &xpts, const Vector<Float> &ypts, 
		  Int symbol);
    virtual void ptxt(Float x, Float y, Float angle, Float fjust, 
		    const String &text);
    virtual Int qci();
    virtual Vector<Int> qcir();
    virtual Vector<Int> qcol();
    virtual Int qtbg();
    virtual Vector<Float> qtxt(Float x, Float y, Float angle, Float fjust, 
		    const String &text);
    virtual Vector<Float> qwin();
    virtual void rect(Float x1, Float x2, Float y1, Float y2);
    virtual void sah(Int fs, Float angle, Float vent);
    virtual void save();
    virtual void sch(Float size);
    virtual void sci(Int ci);
    virtual void scir(Int icilo, Int icihi);
    virtual void scr(Int ci, Float cr, Float cg, Float cb);
    virtual void sfs(Int fs);
    virtual void sitf(Int itf);
    virtual void sls(Int ls);
    virtual void slw(Int lw);
    virtual void stbg(Int tbci);
    virtual void subp(Int nxsub, Int nysub);
    virtual void svp(Float xleft, Float xright, Float ybot, Float ytop);
    virtual void swin(Float x1, Float x2, Float y1, Float y2);
    virtual void tbox(const String &xopt, Float xtick, Int nxsub,
		    const String &yopt, Float ytick, Int nysub);
    virtual void text(Float x, Float y, const String &text);
    virtual void unsa();
    virtual void updt();
    virtual void vstd();
    virtual void wnad(Float x1, Float x2, Float y1, Float y2);
    // </group>
 private:
    CountedPtr<PGPlotterInterface> worker_p;

    // Throws an exception if !isAttached()
    void ok() const;
};

//# Inlines
inline Bool PGPlotter::isAttached() const
{
    return ToBool(!worker_p.null());
}

#endif
