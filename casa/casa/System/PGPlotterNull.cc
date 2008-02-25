//# PGPlotterNull.cc: Plot to a PGPLOT device "NULL" to this process.
//# Copyright (C) 1997,2001,2002
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

#include <casa/System/PGPlotterNull.h>
#include <casa/BasicSL/String.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/Vector.h>
#include <casa/Exceptions/Error.h>
#include <casa/Utilities/Assert.h>
#include <casa/Containers/Record.h>
#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN

PGPlotterNull::PGPlotterNull(const String &device)
    : beenWarned(True)
{
    // If this fails, we need a bit more development to copy Float*'s to
    // float*'s.
    AlwaysAssertExit(sizeof(Float) == sizeof(float));

    noplotter();
}

PGPlotterNull::~PGPlotterNull()
{
    noplotter();
}

PGPlotter PGPlotterNull::createPlotter (const String &device,
					 uInt, uInt, uInt, uInt)
{
    return PGPlotter (new PGPlotterNull (device));
}

Record PGPlotterNull::curs(Float X, Float Y)
{
    Record retval;
    noplotter();
    return retval;
}


void PGPlotterNull::arro(Float x1, Float y1, Float x2, Float y2)
{
    noplotter();
}

void PGPlotterNull::ask(Bool flag)
{
    noplotter();
}

void PGPlotterNull::bbuf()
{
    noplotter();
}

void PGPlotterNull::box(const String &xopt, Float xtick, Int nxsub, 
	     const String &yopt, Float ytick, Int nysub)
{
    noplotter();
}

void PGPlotterNull::circ(Float xcent, Float ycent, Float radius)
{
    noplotter();
}

void PGPlotterNull::draw(Float x, Float y)
{
    noplotter();
}

void PGPlotterNull::ebuf()
{
    noplotter();
}

void PGPlotterNull::env(Float xmin, Float xmax, Float ymin, Float ymax, 
			 Int just, Int axis)
{
    noplotter();
}

void PGPlotterNull::eras()
{
    noplotter();
}

void PGPlotterNull::errb(Int dir, const Vector<Float> &x, 
			  const Vector<Float> &y,
			  const Vector<Float> &e, Float t)
{
    noplotter();
}

void PGPlotterNull::erry(const Vector<Float> &x, const Vector<Float> &y1,
	      const Vector<Float> &y2, Float t)
{
    noplotter();
}

void PGPlotterNull::hist(const Vector<Float> &data, Float datmin, 
			  Float datmax, 
			  Int nbin, Int pcflag)
{
    noplotter();
}

void PGPlotterNull::lab(const String &xlbl, const String &ylbl, 
		   const String &toplbl)
{
    noplotter();
}

void PGPlotterNull::line(const Vector<Float> &xpts, const Vector<Float> &ypts)
{
    noplotter();
}

void PGPlotterNull::move(Float x, Float y)
{
    noplotter();
}

void PGPlotterNull::mtxt(const String &side, Float disp, Float coord, 
			  Float fjust, const String &text)
{
    noplotter();
}

void PGPlotterNull::page()
{
    noplotter();
}

void PGPlotterNull::poly(const Vector<Float> &xpts, const Vector<Float> &ypts)
{
    noplotter();
}

void PGPlotterNull::pt(const Vector<Float> &xpts, const Vector<Float> &ypts, 
		  Int symbol)
{
    noplotter();
}

void PGPlotterNull::ptxt(Float x, Float y, Float angle, Float fjust, 
		    const String &text)
{
    noplotter();
}

Int PGPlotterNull::qci()
{
    int i;
    noplotter();
    return i;
}

Int PGPlotterNull::qtbg()
{
    int i;
    noplotter();
    return i;
}

Vector<Float> PGPlotterNull::qtxt(Float x, Float y, Float angle, Float fjust, 
		    const String &text)
{
    Vector<Float> xboxybox(8);
    noplotter();
    return xboxybox;
}

Vector<Float> PGPlotterNull::qwin()
{
    Vector<Float> retval(4);
    noplotter();
    return retval;
}

void PGPlotterNull::rect(Float x1, Float x2, Float y1, Float y2)
{
    noplotter();
}

void PGPlotterNull::sah(Int fs, Float angle, Float vent)
{
    noplotter();
}

void PGPlotterNull::save()
{
    noplotter();
}

void PGPlotterNull::sch(Float size)
{
    noplotter();
}

void PGPlotterNull::sci(Int ci)
{
    noplotter();
}

void PGPlotterNull::scr(Int ci, Float cr, Float cg, Float cb)
{
    noplotter();
}

void PGPlotterNull::sfs(Int fs)
{
    noplotter();
}

void PGPlotterNull::sls(Int ls)
{
    noplotter();
}

void PGPlotterNull::slw(Int lw)
{
    noplotter();
}

void PGPlotterNull::stbg(Int tbci)
{
    noplotter();
}

void PGPlotterNull::subp(Int nxsub, Int nysub)
{
    noplotter();
}

void PGPlotterNull::svp(Float xleft, Float xright, Float ybot, Float ytop)
{
    noplotter();
}

void PGPlotterNull::swin(Float x1, Float x2, Float y1, Float y2)
{
    noplotter();
}

void PGPlotterNull::tbox(const String &xopt, Float xtick, Int nxsub,
			  const String &yopt, Float ytick, Int nysub)
{
    noplotter();
}

void PGPlotterNull::text(Float x, Float y, const String &text)
{
    noplotter();
}

void PGPlotterNull::unsa()
{
    noplotter();
}

void PGPlotterNull::updt()
{
    noplotter();
}

void PGPlotterNull::vstd()
{
    noplotter();
}

void PGPlotterNull::wnad(Float x1, Float x2, Float y1, Float y2)
{
    noplotter();
}

void PGPlotterNull::conl(const Matrix<Float> &a, Float c,
			  const Vector<Float> &tr, const String &label,
			  Int intval, Int minint)
{
    noplotter();
}

void PGPlotterNull::cont(const Matrix<Float> &a, const Vector<Float> &c,
			  Bool nc, const Vector<Float> &tr)
{
    noplotter();
}

void PGPlotterNull::ctab(const Vector<Float> &l, const Vector<Float> &r,
			  const Vector<Float> &g, const Vector<Float> &b,
			  Float contra, Float bright)
{
    noplotter();
}

void PGPlotterNull::gray(const Matrix<Float> &a, Float fg, Float bg,
			  const Vector<Float> &tr)
{
    noplotter();
} 

void PGPlotterNull::iden()
{
    noplotter();
}

void PGPlotterNull::imag(const Matrix<Float> &a, Float a1, Float a2,
			  const Vector<Float> &tr)
{
    noplotter();
}

Vector<Int> PGPlotterNull::qcir()
{
    Vector<Int> retval(2);
    noplotter();
    return retval;
}

Vector<Int> PGPlotterNull::qcol()
{
    Vector<Int> retval(2);
    noplotter();
    return retval;
}

void PGPlotterNull::scir(Int icilo, Int icihi)
{
    noplotter();
}

void PGPlotterNull::sitf(Int itf)
{
    noplotter();
}

void PGPlotterNull::bin(const Vector<Float> &x, const Vector<Float> &data,
		     Bool center)
{
    noplotter();
}

void PGPlotterNull::conb(const Matrix<Float> &a, const Vector<Float> &c,
		      const Vector<Float> &tr, Float blank)
{
    noplotter();
}

void PGPlotterNull::cons(const Matrix<Float> &a, const Vector<Float> &c,
		      const Vector<Float> &tr)
{
    noplotter();
}

void PGPlotterNull::errx(const Vector<Float> &x1, const Vector<Float> &x2,
		      const Vector<Float> &y, Float t)
{
    noplotter();
}

void PGPlotterNull::hi2d(const Matrix<Float> &data, const Vector<Float> &x,
		      Int ioff, Float bias, Bool center, 
		      const Vector<Float> &ylims)
{
    noplotter();
}

void PGPlotterNull::ldev()
{
    noplotter();
}

Vector<Float> PGPlotterNull::len(Int units, const String &string)
{
    Vector<Float> retval(2);
    noplotter();
    return retval;
}

String PGPlotterNull::numb(Int mm, Int pp, Int form)
{
    noplotter();
    return String("");
}

void PGPlotterNull::panl(Int ix, Int iy)
{
    noplotter();
}

void PGPlotterNull::pap(Float width, Float aspect)
{
    noplotter();
}

void PGPlotterNull::pixl(const Matrix<Int> &ia, Float x1, Float x2,
		      Float y1, Float y2)
{
    noplotter();
}

void PGPlotterNull::pnts(const Vector<Float> &x, const Vector<Float> &y,
		      const Vector<Int> symbol)
{
    noplotter();
}

Vector<Float>  PGPlotterNull::qah()
{
    Vector<Float> retval(3);
    noplotter();
    return retval;
}

Int PGPlotterNull::qcf()
{
    int retval;
    noplotter();
    return retval;
}

Float PGPlotterNull::qch()
{
    float retval;
    noplotter();
    return retval;
}

Vector<Float> PGPlotterNull::qcr(Int ci)
{
    Vector<Float> retval(3);
    noplotter();
    return retval;
}

Vector<Float> PGPlotterNull::qcs(Int units)
{
    Vector<Float> retval(2);
    noplotter();
    return retval;
}

Int PGPlotterNull::qfs()
{
    int retval;
    noplotter();
    return retval;
}

Vector<Float> PGPlotterNull::qhs()
{
    Vector<Float> retval(3);
    noplotter();
    return retval;
}

Int PGPlotterNull::qid()
{
    int retval;
    noplotter();
    return retval;
}

String PGPlotterNull::qinf(const String &item)
{
    noplotter();
    return String("");
}

Int PGPlotterNull::qitf()
{
    int retval;
    noplotter();
    return retval;
}

Int PGPlotterNull::qls()
{
    int retval;
    noplotter();
    return retval;
}

Int PGPlotterNull::qlw()
{
    int retval;
    noplotter();
    return retval;
}

Vector<Float> PGPlotterNull::qpos()
{
    Vector<Float> retval(2);
    noplotter();
    return retval;
}

Vector<Float> PGPlotterNull::qvp(Int units)
{
    Vector<Float> retval(4);
    noplotter();
    return retval;
}

Vector<Float> PGPlotterNull::qvsz(Int units)
{
    Vector<Float> retval(4);
    noplotter();
    return retval;
}

Float PGPlotterNull::rnd(Float x, Int nsub)
{
    noplotter();
    return x;
}

Vector<Float> PGPlotterNull::rnge(Float x1, Float x2)
{
    Vector<Float> retval(2);
    noplotter();
    return retval;
}

void PGPlotterNull::scf(Int font)
{
  noplotter();
}

void PGPlotterNull::scrn(Int ci, const String &name)
{
  noplotter();
}

void PGPlotterNull::shls(Int ci, Float ch, Float cl, Float cs)
{
  noplotter();
}

void PGPlotterNull::shs(Float angle, Float sepn, Float phase)
{
  noplotter();
}

void PGPlotterNull::vect(const Matrix<Float> &a, const Matrix<Float> &b,
		      Float c, Int nc, 
		      const Vector<Float> &tr, Float blank)
{
  noplotter();
}

void PGPlotterNull::vsiz(Float xleft, Float xright, Float ybot,
		      Float ytop)
{
  noplotter();
}

void PGPlotterNull::wedg(const String &side, Float disp, Float width,
		      Float fg, Float bg, const String &label)
{
  noplotter();
}

void PGPlotterNull::noplotter()
{
    if(!beenWarned){
       std::cerr << "Warning no plotter attached.  Attach a plotter to get plots" << std::endl;
       beenWarned = True;
    }
}

} //# NAMESPACE CASA - END

