// # PGPlotterInterface.h: Abstract base class for PGPLOT style plotting.
// # Copyright (C) 1997,2000,2001
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This library is free software; you can redistribute it and/or modify it
// # under the terms of the GNU Library General Public License as published by
// # the Free Software Foundation; either version 2 of the License, or (at your
// # option) any later version.
// #
// # This library is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
// # License for more details.
// #
// # You should have received a copy of the GNU Library General Public License
// # along with this library; if not, write to the Free Software Foundation,
// # Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

#ifndef CASA_PGPLOTTERINTERFACE_H
#define CASA_PGPLOTTERINTERFACE_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayFwd.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

class Record;
class String;

// <summary>
// Abstract base class for PGPLOT style plotting.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> General familiarity with PGPLOT, especially of the style of the
//        Glish/PGPLOT binding.
// </prerequisite>
//
// <etymology>
// PGPlotter for the plotting style, Interface because it is an abstract base
// class, not a concrete derived class.
// </etymology>
//
// <synopsis>
// This class represents an interface for plotting to a PGPLOT style plotting
// interface. In general, the differences between actual PGPLOT and this
// interface is:
// <ol>
//   <li> The functions related to opening and closing are not implemented,
//        since it is assumed the derived class constructor/destructor will
//        handle this.
//   <li> The leading "pg" is removed from the name since by being in a class
//        there are no namespace issues.
//   <li> Casacore array classes are used in place of raw pointers. This also
//        obviates the need for passing in array dimensions. Similarly the
//        subregion arguments (I1, I2, J1, J2) are left out since the array
//        classes have their own subsectioning methods.
//   <li> Output values are returned from the function
// </ol>
// The rules are basically the same as for the Glish/PGPLOT binding, and thus
// the individual routines are not documented here.
// </synopsis>
//
// <example>
// <srcblock>
// void plotFunction(const PGPlotterInterface &plotter) {
//     // plot y = x*x
//     Vector<Float> x(100), y(100);
//     indgen(x);
//     y = x*x;
//     plotter.env(0, 100, 0, 100*100, 0, 0);
//     plotter.line(x, y);
// }
// </srcblock>
// </example>
//
// <motivation>
// General plotting interface for programmers, while allowing the location and
// form of the plot to vary.
// </motivation>
//
// <todo asof="1997/1/15">
//   <li> Add the missing PGPLOT functions.
//   <li> Emulate band as well as curs?
// </todo>

class PGPlotterInterface {
 public:
  virtual ~PGPlotterInterface();

  // true if it is OK to plot to this object. This method is implemented for
  // devices where you have to worry about devices detaching (e.g., the Glish
  // pgplotter might be dismissed by the user). The default implementation is
  // to always return true.
  virtual bool isAttached() const;

  // This is not a standard PGPLOT command. In the Glish/PGPLOT window, it
  // puts a message in the message line. By default it sends it to the logger.
  // In any event, this is intended for short helpful messages (e.g.
  // saying which keys to press to mark a spectrum).
  virtual void message(const String &text);

  // This is not a standard PGPLOT command.  It is only needed for
  // the PGPlotterGlish class which connects to Glish/PGPLOT window
  // This Glish object (actually a pgplotter/pgplotwidget.g) has an
  // internal counter plot counter which needs to be reset to 0
  // when the process detaches from the plotter, so that the next
  // plot on the device is the first one again.  Without this, the
  // prompting behaviour of the Glish plotter  is different from native
  // PGPLOT
  virtual void resetPlotNumber() { ; };

  // This is an emulated standard PGPLOT command. It returns a record
  // containing the fields:
  // <srcblock>
  // [ok=Bool, x=Float, y=Float, ch=String];
  // If the remote device cannot do cursor feedback, ok==F.
  // </srcblock>
  // The input x,y values is the "guess" for the location the user will want
  // to pick. On some devices, the cursor will be positioned at (world
  // coordinates) x,y.
  virtual Record curs(float x, float y) = 0;

  // Standard PGPLOT commands. Documentation for the individual commands
  // can be found in the Glish manual and in the standard PGPLOT documentation
  // which may be found at <src>http://astro.caltech.edu/~tjp/pgplot/</src>.
  // The Glish/PGPLOT documentation is preferred since this interface follows
  // it exactly (e.g. the array sizes are inferred both here and in Glish,
  // whereas they must be passed into standard PGPLOT).
  // <group>
  virtual void arro(float x1, float y1, float x2, float y2) = 0;
  virtual void ask(bool flag) = 0;
  virtual void bbuf() = 0;
  virtual void bin(const Vector<float> &x, const Vector<float> &data, bool center) = 0;
  virtual void box(const String &xopt, float xtick, int nxsub, const String &yopt, float ytick,
                   int nysub) = 0;
  virtual void circ(float xcent, float ycent, float radius) = 0;
  virtual void conb(const Matrix<float> &a, const Vector<float> &c, const Vector<float> &tr,
                    float blank) = 0;
  virtual void conl(const Matrix<float> &a, float c, const Vector<float> &tr, const String &label,
                    int intval, int minint) = 0;
  virtual void cons(const Matrix<float> &a, const Vector<float> &c, const Vector<float> &tr) = 0;
  virtual void cont(const Matrix<float> &a, const Vector<float> &c, bool nc,
                    const Vector<float> &tr) = 0;
  virtual void ctab(const Vector<float> &l, const Vector<float> &r, const Vector<float> &g,
                    const Vector<float> &b, float contra, float bright) = 0;
  virtual void draw(float x, float y) = 0;
  virtual void ebuf() = 0;
  virtual void env(float xmin, float xmax, float ymin, float ymax, int just, int axis) = 0;
  virtual void eras() = 0;
  virtual void errb(int dir, const Vector<float> &x, const Vector<float> &y, const Vector<float> &e,
                    float t) = 0;
  virtual void errx(const Vector<float> &x1, const Vector<float> &x2, const Vector<float> &y,
                    float t) = 0;
  virtual void erry(const Vector<float> &x, const Vector<float> &y1, const Vector<float> &y2,
                    float t) = 0;
  virtual void gray(const Matrix<float> &a, float fg, float bg, const Vector<float> &tr) = 0;
  virtual void hi2d(const Matrix<float> &data, const Vector<float> &x, int ioff, float bias,
                    bool center, const Vector<float> &ylims) = 0;
  virtual void hist(const Vector<float> &data, float datmin, float datmax, int nbin,
                    int pcflag) = 0;
  virtual void iden() = 0;
  virtual void imag(const Matrix<float> &a, float a1, float a2, const Vector<float> &tr) = 0;
  virtual void lab(const String &xlbl, const String &ylbl, const String &toplbl) = 0;
  virtual void ldev() = 0;
  virtual Vector<float> len(int units, const String &string) = 0;
  virtual void line(const Vector<float> &xpts, const Vector<float> &ypts) = 0;
  virtual void move(float x, float y) = 0;
  virtual void mtxt(const String &side, float disp, float coord, float fjust,
                    const String &text) = 0;
  virtual String numb(int mm, int pp, int form) = 0;
  virtual void page() = 0;
  virtual void panl(int ix, int iy) = 0;
  virtual void pap(float width, float aspect) = 0;
  virtual void pixl(const Matrix<int> &ia, float x1, float x2, float y1, float y2) = 0;
  virtual void pnts(const Vector<float> &x, const Vector<float> &y, const Vector<int> symbol) = 0;
  virtual void poly(const Vector<float> &xpts, const Vector<float> &ypts) = 0;
  virtual void pt(const Vector<float> &xpts, const Vector<float> &ypts, int symbol) = 0;
  virtual void ptxt(float x, float y, float angle, float fjust, const String &text) = 0;
  virtual Vector<float> qah() = 0;
  virtual int qcf() = 0;
  virtual float qch() = 0;
  virtual int qci() = 0;
  virtual Vector<int> qcir() = 0;
  virtual Vector<int> qcol() = 0;
  virtual Vector<float> qcr(int ci) = 0;
  virtual Vector<float> qcs(int units) = 0;
  virtual int qfs() = 0;
  virtual Vector<float> qhs() = 0;
  virtual int qid() = 0;
  virtual String qinf(const String &item) = 0;
  virtual int qitf() = 0;
  virtual int qls() = 0;
  virtual int qlw() = 0;
  virtual Vector<float> qpos() = 0;
  virtual int qtbg() = 0;
  virtual Vector<float> qtxt(float x, float y, float angle, float fjust, const String &text) = 0;
  virtual Vector<float> qvp(int units) = 0;
  virtual Vector<float> qvsz(int units) = 0;
  virtual Vector<float> qwin() = 0;
  virtual void rect(float x1, float x2, float y1, float y2) = 0;
  virtual float rnd(float x, int nsub) = 0;
  virtual Vector<float> rnge(float x1, float x2) = 0;
  virtual void sah(int fs, float angle, float vent) = 0;
  virtual void save() = 0;
  virtual void scf(int font) = 0;
  virtual void sch(float size) = 0;
  virtual void sci(int ci) = 0;
  virtual void scir(int icilo, int icihi) = 0;
  virtual void scr(int ci, float cr, float cg, float cb) = 0;
  virtual void scrn(int ci, const String &name) = 0;
  virtual void sfs(int fs) = 0;
  virtual void shls(int ci, float ch, float cl, float cs) = 0;
  virtual void shs(float angle, float sepn, float phase) = 0;
  virtual void sitf(int itf) = 0;
  virtual void sls(int ls) = 0;
  virtual void slw(int lw) = 0;
  virtual void stbg(int tbci) = 0;
  virtual void subp(int nxsub, int nysub) = 0;
  virtual void svp(float xleft, float xright, float ybot, float ytop) = 0;
  virtual void swin(float x1, float x2, float y1, float y2) = 0;
  virtual void tbox(const String &xopt, float xtick, int nxsub, const String &yopt, float ytick,
                    int nysub) = 0;
  virtual void text(float x, float y, const String &text) = 0;
  virtual void unsa() = 0;
  virtual void updt() = 0;
  virtual void vect(const Matrix<float> &a, const Matrix<float> &b, float c, int nc,
                    const Vector<float> &tr, float blank) = 0;
  virtual void vsiz(float xleft, float xright, float ybot, float ytop) = 0;
  virtual void vstd() = 0;
  virtual void wedg(const String &side, float disp, float width, float fg, float bg,
                    const String &label) = 0;
  virtual void wnad(float x1, float x2, float y1, float y2) = 0;
  // </group>
};

}  // namespace casacore

#endif
