//# PGPlotterNull.h: Plot to a PGPLOT device "null" to this process.
//# Copyright (C) 1997,2001
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

#ifndef GRAPHICS_PGPLOTTERNULL_H
#define GRAPHICS_PGPLOTTERNULL_H

#include <casacore/casa/aips.h>
#include <casacore/casa/System/PGPlotter.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class String;
template<class T> class Vector;

// <summary>
// Plot to a PGPLOT device "local" to this process.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="PGPlotterInterface">PGPlotterInterface</linkto>
// </prerequisite>
//
// <etymology>
// "Null" is used to denote that no plotting is done
// </etymology>
//
// <synopsis>
// Generally programmers should not use this class, instead they should use
// <linkto class="PGPlotter">PGPlotter</linkto> instead.
//
// This class make a concrete
// <linkto class="PGPlotterInterface">PGPlotterInterface</linkto> object which
// calls PGPLOT directly, i.e. PGPLOT is linked into the current executable.
// </synopsis>
//
// <example>
// <srcblock>
//     // plot y = x*x
//     Vector<Float> x(100), y(100);
//     indgen(x);
//     y = x*x;

//     PGPlotterNull plotter("myplot.ps/ps");
//     plotter.env(0, 100, 0, 100*100, 0, 0);
//     plotter.line(x, y);
// </srcblock>
// </example>
//
// <motivation>
// It might be necessary to call PGPLOT directly in some circumstances. For
// example, it might be too inefficient to pass a lot of Image data over the
// glish bus.
// </motivation>
//
// <todo asof="1997/12/31">
//   <li> Add more plot calls.
// </todo>

class PGPlotterNull : public PGPlotterInterface
{
public:
    // Open "device", which must be a valid PGPLOT style device, for example
    // <src>/cps</src> for colour postscript (or <src>myfile.ps/cps</src>
    // if you want to name the file), or <src>/xs</src> or <src>/xw</src> for
    // and X-windows display.
    // <thrown>
    //   <li> An <linkto class="AipsError">AipsError</linkto> will be thrown
    //        if the underlying PGPLOT open fails for some reason.
    // </thrown>
    PGPlotterNull(const String &device);
    // The destructor closes the pgplot device.
    virtual ~PGPlotterNull();

    // The create function to create a PGPlotter object using a PGPlotterNull.
    // It only uses the device argument.
    static PGPlotter createPlotter (const String &device,
				    uInt, uInt, uInt, uInt);

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
    // <group>
    virtual void arro(Float x1, Float y1, Float x2, Float y2);
    virtual void ask(Bool flag);
    virtual void bbuf();
    virtual void bin(const Vector<Float> &x, const Vector<Float> &data,
		     Bool center);
    virtual void box(const String &xopt, Float xtick, Int nxsub, 
	     const String &yopt, Float ytick, Int nysub);
    virtual void circ(Float xcent, Float ycent, Float radius);
    virtual void conb(const Matrix<Float> &a, const Vector<Float> &c,
		      const Vector<Float> &tr, Float blank);
    virtual void conl(const Matrix<Float> &a, Float c,
		      const Vector<Float> &tr, const String &label,
		      Int intval, Int minint);
    virtual void cons(const Matrix<Float> &a, const Vector<Float> &c,
		      const Vector<Float> &tr);
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
    virtual void errx(const Vector<Float> &x1, const Vector<Float> &x2,
		      const Vector<Float> &y, Float t);
    virtual void erry(const Vector<Float> &x, const Vector<Float> &y1,
	      const Vector<Float> &y2, Float t);
    virtual void gray(const Matrix<Float> &a, Float fg, Float bg,
		      const Vector<Float> &tr); 
    virtual void hi2d(const Matrix<Float> &data, const Vector<Float> &x,
		      Int ioff, Float bias, Bool center, 
		      const Vector<Float> &ylims);
    virtual void hist(const Vector<Float> &data, Float datmin, Float datmax, 
		    Int nbin, Int pcflag);
    virtual void iden();
    virtual void imag(const Matrix<Float> &a, Float a1, Float a2,
		      const Vector<Float> &tr);
    virtual void lab(const String &xlbl, const String &ylbl, 
		   const String &toplbl);
    virtual void ldev();
    virtual Vector<Float> len(Int units, const String &string);
    virtual void line(const Vector<Float> &xpts, const Vector<Float> &ypts);
    virtual void move(Float x, Float y);
    virtual void mtxt(const String &side, Float disp, Float coord, Float fjust,
		    const String &text);
    virtual String numb(Int mm, Int pp, Int form);
    virtual void page();
    virtual void panl(Int ix, Int iy);
    virtual void pap(Float width, Float aspect);
    virtual void pixl(const Matrix<Int> &ia, Float x1, Float x2,
		      Float y1, Float y2);
    virtual void pnts(const Vector<Float> &x, const Vector<Float> &y,
		      const Vector<Int> symbol);
    virtual void poly(const Vector<Float> &xpts, const Vector<Float> &ypts);
    virtual void pt(const Vector<Float> &xpts, const Vector<Float> &ypts, 
		  Int symbol);
    virtual void ptxt(Float x, Float y, Float angle, Float fjust, 
		    const String &text);
    virtual Vector<Float>  qah();
    virtual Int qcf();
    virtual Float qch();
    virtual Int qci();
    virtual Vector<Int> qcir();
    virtual Vector<Int> qcol();
    virtual Vector<Float> qcr(Int ci);
    virtual Vector<Float> qcs(Int units);
    virtual Int qfs();
    virtual Vector<Float> qhs();
    virtual Int qid();
    virtual String qinf(const String &item);
    virtual Int qitf();
    virtual Int qls();
    virtual Int qlw();
    virtual Vector<Float> qpos();
    virtual Int qtbg();
    virtual Vector<Float> qtxt(Float x, Float y, Float angle, Float fjust, 
		    const String &text);
    virtual Vector<Float> qvp(Int units);
    virtual Vector<Float> qvsz(Int units);
    virtual Vector<Float> qwin();
    virtual void rect(Float x1, Float x2, Float y1, Float y2);
    virtual Float rnd(Float x, Int nsub);
    virtual Vector<Float> rnge(Float x1, Float x2);
    virtual void sah(Int fs, Float angle, Float vent);
    virtual void save();
    virtual void scf(Int font);
    virtual void sch(Float size);
    virtual void sci(Int ci);
    virtual void scir(Int icilo, Int icihi);
    virtual void scr(Int ci, Float cr, Float cg, Float cb);
    virtual void scrn(Int ci, const String &name);
    virtual void sfs(Int fs);
    virtual void shls(Int ci, Float ch, Float cl, Float cs);
    virtual void shs(Float angle, Float sepn, Float phase);
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
    virtual void vect(const Matrix<Float> &a, const Matrix<Float> &b,
		      Float c, Int nc, 
		      const Vector<Float> &tr, Float blank);
    virtual void vsiz(Float xleft, Float xright, Float ybot,
		      Float ytop);
    virtual void vstd();
    virtual void wedg(const String &side, Float disp, Float width,
		      Float fg, Float bg, const String &label);
    virtual void wnad(Float x1, Float x2, Float y1, Float y2);
    // </group>

 private:
    // Undefined and inaccessible
    PGPlotterNull(const PGPlotterNull &);
    PGPlotterNull& operator=(const PGPlotterNull &);

    // PGPLOT id
    int id_p;
    Bool beenWarned;
    void noplotter();
};



} //# NAMESPACE CASACORE - END

#endif
