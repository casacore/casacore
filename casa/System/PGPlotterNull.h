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

#ifndef GRAPHICS_PGPLOTTERNULL_H
#define GRAPHICS_PGPLOTTERNULL_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/casa/System/PGPlotter.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class String;

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
//     Vector<float> x(100), y(100);
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
				    uint32_t, uint32_t, uint32_t, uint32_t);

    // This is an emulated standard PGPLOT command. It returns a record
    // containing the fields:
    // <srcblock>
    // [ok=bool, x=float, y=float, ch=String];
    // If the remote device cannot do cursor feedback, ok==F.
    // </srcblock>
    virtual Record curs(float x, float y);


    // Standard PGPLOT commands. Documentation for the individual commands
    // can be found in the Glish manual and in the standard PGPLOT documentation
    // which may be found at <src>http://astro.caltech.edu/~tjp/pgplot/</src>.
    // The Glish/PGPLOT documentation is preferred since this interface follows
    // it exactly (e.g. the array sizes are inferred both here and in Glish,
    // whereas they must be passed into standard PGPLOT).
    // <group>
    virtual void arro(float x1, float y1, float x2, float y2);
    virtual void ask(bool flag);
    virtual void bbuf();
    virtual void bin(const Vector<float> &x, const Vector<float> &data,
		     bool center);
    virtual void box(const String &xopt, float xtick, int32_t nxsub, 
	     const String &yopt, float ytick, int32_t nysub);
    virtual void circ(float xcent, float ycent, float radius);
    virtual void conb(const Matrix<float> &a, const Vector<float> &c,
		      const Vector<float> &tr, float blank);
    virtual void conl(const Matrix<float> &a, float c,
		      const Vector<float> &tr, const String &label,
		      int32_t intval, int32_t minint);
    virtual void cons(const Matrix<float> &a, const Vector<float> &c,
		      const Vector<float> &tr);
    virtual void cont(const Matrix<float> &a, const Vector<float> &c,
		      bool nc, const Vector<float> &tr);
    virtual void ctab(const Vector<float> &l, const Vector<float> &r,
		      const Vector<float> &g, const Vector<float> &b,
		      float contra, float bright);
    virtual void draw(float x, float y);
    virtual void ebuf();
    virtual void env(float xmin, float xmax, float ymin, float ymax, int32_t just,
	     int32_t axis);
    virtual void eras();
    virtual void errb(int32_t dir, const Vector<float> &x, const Vector<float> &y,
	      const Vector<float> &e, float t);
    virtual void errx(const Vector<float> &x1, const Vector<float> &x2,
		      const Vector<float> &y, float t);
    virtual void erry(const Vector<float> &x, const Vector<float> &y1,
	      const Vector<float> &y2, float t);
    virtual void gray(const Matrix<float> &a, float fg, float bg,
		      const Vector<float> &tr); 
    virtual void hi2d(const Matrix<float> &data, const Vector<float> &x,
		      int32_t ioff, float bias, bool center, 
		      const Vector<float> &ylims);
    virtual void hist(const Vector<float> &data, float datmin, float datmax, 
		    int32_t nbin, int32_t pcflag);
    virtual void iden();
    virtual void imag(const Matrix<float> &a, float a1, float a2,
		      const Vector<float> &tr);
    virtual void lab(const String &xlbl, const String &ylbl, 
		   const String &toplbl);
    virtual void ldev();
    virtual Vector<float> len(int32_t units, const String &string);
    virtual void line(const Vector<float> &xpts, const Vector<float> &ypts);
    virtual void move(float x, float y);
    virtual void mtxt(const String &side, float disp, float coord, float fjust,
		    const String &text);
    virtual String numb(int32_t mm, int32_t pp, int32_t form);
    virtual void page();
    virtual void panl(int32_t ix, int32_t iy);
    virtual void pap(float width, float aspect);
    virtual void pixl(const Matrix<int32_t> &ia, float x1, float x2,
		      float y1, float y2);
    virtual void pnts(const Vector<float> &x, const Vector<float> &y,
		      const Vector<int32_t> symbol);
    virtual void poly(const Vector<float> &xpts, const Vector<float> &ypts);
    virtual void pt(const Vector<float> &xpts, const Vector<float> &ypts, 
		  int32_t symbol);
    virtual void ptxt(float x, float y, float angle, float fjust, 
		    const String &text);
    virtual Vector<float>  qah();
    virtual int32_t qcf();
    virtual float qch();
    virtual int32_t qci();
    virtual Vector<int32_t> qcir();
    virtual Vector<int32_t> qcol();
    virtual Vector<float> qcr(int32_t ci);
    virtual Vector<float> qcs(int32_t units);
    virtual int32_t qfs();
    virtual Vector<float> qhs();
    virtual int32_t qid();
    virtual String qinf(const String &item);
    virtual int32_t qitf();
    virtual int32_t qls();
    virtual int32_t qlw();
    virtual Vector<float> qpos();
    virtual int32_t qtbg();
    virtual Vector<float> qtxt(float x, float y, float angle, float fjust, 
		    const String &text);
    virtual Vector<float> qvp(int32_t units);
    virtual Vector<float> qvsz(int32_t units);
    virtual Vector<float> qwin();
    virtual void rect(float x1, float x2, float y1, float y2);
    virtual float rnd(float x, int32_t nsub);
    virtual Vector<float> rnge(float x1, float x2);
    virtual void sah(int32_t fs, float angle, float vent);
    virtual void save();
    virtual void scf(int32_t font);
    virtual void sch(float size);
    virtual void sci(int32_t ci);
    virtual void scir(int32_t icilo, int32_t icihi);
    virtual void scr(int32_t ci, float cr, float cg, float cb);
    virtual void scrn(int32_t ci, const String &name);
    virtual void sfs(int32_t fs);
    virtual void shls(int32_t ci, float ch, float cl, float cs);
    virtual void shs(float angle, float sepn, float phase);
    virtual void sitf(int32_t itf);
    virtual void sls(int32_t ls);
    virtual void slw(int32_t lw);
    virtual void stbg(int32_t tbci);
    virtual void subp(int32_t nxsub, int32_t nysub);
    virtual void svp(float xleft, float xright, float ybot, float ytop);
    virtual void swin(float x1, float x2, float y1, float y2);
    virtual void tbox(const String &xopt, float xtick, int32_t nxsub,
		    const String &yopt, float ytick, int32_t nysub);
    virtual void text(float x, float y, const String &text);
    virtual void unsa();
    virtual void updt();
    virtual void vect(const Matrix<float> &a, const Matrix<float> &b,
		      float c, int32_t nc, 
		      const Vector<float> &tr, float blank);
    virtual void vsiz(float xleft, float xright, float ybot,
		      float ytop);
    virtual void vstd();
    virtual void wedg(const String &side, float disp, float width,
		      float fg, float bg, const String &label);
    virtual void wnad(float x1, float x2, float y1, float y2);
    // </group>

 private:
    // Undefined and inaccessible
    PGPlotterNull(const PGPlotterNull &);
    PGPlotterNull& operator=(const PGPlotterNull &);

    // PGPLOT id
    int id_p;
    bool beenWarned;
    void noplotter();
};



} //# NAMESPACE CASACORE - END

#endif
