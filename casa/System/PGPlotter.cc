//# PGPlotter.h: Standard plotting object for application programmers.
//# Copyright (C) 1997,2000,2001
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

#include <casacore/casa/System/PGPlotter.h>
#include <casacore/casa/System/PGPlotterNull.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/Record.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Default is no create function, thus use createLocal.
PGPlotter::CreateFunction* PGPlotter::creator_p = 0;


PGPlotter::PGPlotter()
{
    // Nothing
}

PGPlotter::PGPlotter(PGPlotterInterface* worker)
  : worker_p(worker)
{
    // Nothing
}

PGPlotter::PGPlotter (const String &device,
		      uInt mincolors, uInt maxcolors,
		      uInt sizex, uInt sizey)
{
    *this = create (device, mincolors, maxcolors, sizex, sizey);
}

PGPlotter::PGPlotter(const PGPlotter &other)
  : PGPlotterInterface(),
    worker_p(other.worker_p)
{
    // Nothing
}

PGPlotter &PGPlotter::operator=(const PGPlotter &other)
{
    worker_p = other.worker_p;
    return *this;
}

PGPlotter::~PGPlotter()
{
    // Nothing
}

PGPlotter PGPlotter::create (const String &device,
			     uInt mincolors, uInt maxcolors,
			     uInt sizex, uInt sizey)
{
    if (creator_p == 0) {
        return PGPlotterNull::createPlotter(device, mincolors, maxcolors,
					    sizex, sizey);
    }
    return creator_p (device, mincolors, maxcolors, sizex, sizey);
}

PGPlotter::CreateFunction* PGPlotter::setCreateFunction
                                          (PGPlotter::CreateFunction* func,
					   Bool override)
{
    CreateFunction* tmp = creator_p;
    if (override  ||  tmp == 0) {
        creator_p = func;
    }
    return tmp;
}

void PGPlotter::detach()
{
    worker_p->resetPlotNumber();        // Implemented for PGPlotterGlish only
    CountedPtr<PGPlotterInterface> empty;
    worker_p = empty;
}

Bool PGPlotter::isAttached() const
{
    return (!worker_p.null());
}

void PGPlotter::message(const String &text)
{
    ok();
    worker_p->message(text);
}

Record PGPlotter::curs(Float x, Float y)
{
    ok();
    Record retval =  worker_p->curs(x, y);
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

void PGPlotter::arro(Float x1, Float y1, Float x2, Float y2)
{
    ok();
    worker_p->arro(x1, y1, x2, y2);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::ask(Bool flag)
{
    ok();
    worker_p->ask(flag);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::bbuf()
{
    ok();
    worker_p->bbuf();
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::box(const String &xopt, Float xtick, Int nxsub, 
	     const String &yopt, Float ytick, Int nysub)
{
    ok();
    worker_p->box(xopt, xtick, nxsub, yopt, ytick, nysub);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::circ(Float xcent, Float ycent, Float radius)
{
    ok();
    worker_p->circ(xcent, ycent, radius);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::draw(Float x, Float y)
{
    ok();
    worker_p->draw(x, y);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::ebuf()
{
    ok();
    worker_p->ebuf();
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::env(Float xmin, Float xmax, Float ymin, Float ymax, Int just,
	     Int axis)
{
    ok();
    worker_p->env(xmin, xmax, ymin, ymax, just, axis);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::eras()
{
    ok();
    worker_p->eras();
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::errb(Int dir, const Vector<Float> &x, const Vector<Float> &y,
	      const Vector<Float> &e, Float t)
{
    ok();
    worker_p->errb(dir, x, y, e, t);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::erry(const Vector<Float> &x, const Vector<Float> &y1,
	      const Vector<Float> &y2, Float t)
{
    ok();
    worker_p->erry(x, y1, y2, t);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::hist(const Vector<Float> &data, Float datmin, Float datmax, 
		    Int nbin, Int pcflag)
{
    ok();
    worker_p->hist(data, datmin, datmax, nbin, pcflag);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::lab(const String &xlbl, const String &ylbl, 
		   const String &toplbl)
{
    ok();
    worker_p->lab(xlbl, ylbl, toplbl);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::line(const Vector<Float> &xpts, const Vector<Float> &ypts)
{
    ok();
    worker_p->line(xpts, ypts);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::move(Float x, Float y)
{
    ok();
    worker_p->move(x, y);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::mtxt(const String &side, Float disp, Float coord, Float fjust,
		    const String &text)
{
    ok();
    worker_p->mtxt(side, disp, coord, fjust, text);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::page()
{
    ok();
    worker_p->page();
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::poly(const Vector<Float> &xpts, const Vector<Float> &ypts)
{
    ok();
    worker_p->poly(xpts, ypts);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::pt(const Vector<Float> &xpts, const Vector<Float> &ypts, 
		  Int symbol)
{
    ok();
    worker_p->pt(xpts, ypts, symbol);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::ptxt(Float x, Float y, Float angle, Float fjust, 
		    const String &text)
{
    ok();
    worker_p->ptxt(x, y, angle, fjust, text);
    if (!worker_p->isAttached()) worker_p = 0;
}

Int PGPlotter::qci()
{
    ok();
    Int retval = worker_p->qci();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Int PGPlotter::qtbg()
{
    ok();
    Int retval = worker_p->qtbg();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Vector<Float> PGPlotter::qtxt(Float x, Float y, Float angle, Float fjust, 
		    const String &text)
{
    ok();
    Vector<Float> retval = worker_p->qtxt(x, y, angle, fjust, text);
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Vector<Float> PGPlotter::qwin()
{
    ok();
    Vector<Float> retval = worker_p->qwin();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

void PGPlotter::rect(Float x1, Float x2, Float y1, Float y2)
{
    ok();
    worker_p->rect(x1, x2, y1, y2);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::sah(Int fs, Float angle, Float vent)
{
    ok();
    worker_p->sah(fs, angle, vent);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::save()
{
    ok();
    worker_p->save();
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::sch(Float size)
{
    ok();
    worker_p->sch(size);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::sci(Int ci)
{
    ok();
    worker_p->sci(ci);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::scr(Int ci, Float cr, Float cg, Float cb)
{
    ok();
    worker_p->scr(ci, cr, cg, cb);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::sfs(Int fs)
{
    ok();
    worker_p->sfs(fs);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::sls(Int ls)
{
    ok();
    worker_p->sls(ls);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::slw(Int lw)
{
    ok();
    worker_p->slw(lw);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::stbg(Int tbci)
{
    ok();
    worker_p->stbg(tbci);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::subp(Int nxsub, Int nysub)
{
    ok();
    worker_p->subp(nxsub, nysub);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::svp(Float xleft, Float xright, Float ybot, Float ytop)
{
    ok();
    worker_p->svp(xleft, xright, ybot, ytop);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::swin(Float x1, Float x2, Float y1, Float y2)
{
    ok();
    worker_p->swin(x1, x2, y1, y2);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::tbox(const String &xopt, Float xtick, Int nxsub,
		    const String &yopt, Float ytick, Int nysub)
{
    ok();
    worker_p->tbox(xopt, xtick, nxsub, yopt, ytick, nysub);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::text(Float x, Float y, const String &text)
{
    ok();
    worker_p->text(x, y, text);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::unsa()
{
    ok();
    worker_p->unsa();
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::updt()
{
    ok();
    worker_p->updt();
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::vstd()
{
    ok();
    worker_p->vstd();
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::wnad(Float x1, Float x2, Float y1, Float y2)
{
    ok();
    worker_p->wnad(x1, x2,  y1,  y2);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::ok() const
{
    if (!isAttached()) {
	throw(AipsError("Attempt to plot to an unattached PGPlotter!"));
    }
}

void PGPlotter::conl(const Matrix<Float> &a, Float c,
		      const Vector<Float> &tr, const String &label,
		      Int intval, Int minint)
{
    ok();
    worker_p->conl(a, c, tr, label, intval, minint);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::cont(const Matrix<Float> &a, const Vector<Float> &c,
		      Bool nc, const Vector<Float> &tr)
{
    ok();
    worker_p->cont(a, c, nc, tr);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::ctab(const Vector<Float> &l, const Vector<Float> &r,
		      const Vector<Float> &g, const Vector<Float> &b,
		      Float contra, Float bright)
{
    ok();
    worker_p->ctab(l, r, g, b, contra, bright);
    if (!worker_p->isAttached()) worker_p = 0;
}
void PGPlotter::gray(const Matrix<Float> &a, Float fg, Float bg,
		      const Vector<Float> &tr)
{
    ok();
    worker_p->gray(a, fg, bg, tr);
    if (!worker_p->isAttached()) worker_p = 0;
} 

void PGPlotter::iden()
{
    ok();
    worker_p->iden();
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::imag(const Matrix<Float> &a, Float a1, Float a2,
		      const Vector<Float> &tr)
{
    ok();
    worker_p->imag(a, a1, a2, tr);
    if (!worker_p->isAttached()) worker_p = 0;
}

Vector<Int> PGPlotter::qcir()
{
    ok();
    Vector<Int> retval = worker_p->qcir();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Vector<Int> PGPlotter::qcol()
{
    ok();
    Vector<Int> retval = worker_p->qcol();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

void PGPlotter::scir(Int icilo, Int icihi)
{
    ok();
    worker_p->scir(icilo, icihi);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::sitf(Int itf)
{
    ok();
    worker_p->sitf(itf);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::bin(const Vector<Float> &x, const Vector<Float> &data,
		     Bool center)
{
    ok();
    worker_p->bin(x, data, center);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::conb(const Matrix<Float> &a, const Vector<Float> &c,
		      const Vector<Float> &tr, Float blank)
{
    ok();
    worker_p->conb(a, c, tr, blank);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::cons(const Matrix<Float> &a, const Vector<Float> &c,
		      const Vector<Float> &tr)
{
    ok();
    worker_p->cons(a, c, tr);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::errx(const Vector<Float> &x1, const Vector<Float> &x2,
		      const Vector<Float> &y, Float t)
{
    ok();
    worker_p->errx(x1, x2, y, t);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::hi2d(const Matrix<Float> &data, const Vector<Float> &x,
		      Int ioff, Float bias, Bool center, 
		      const Vector<Float> &ylims)
{
    ok();
    worker_p->hi2d(data, x, ioff, bias, center, ylims);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::ldev()
{
    ok();
    worker_p->ldev();
    if (!worker_p->isAttached()) worker_p = 0;
}

Vector<Float> PGPlotter::len(Int units, const String &string)
{
    ok();
    Vector<Float> retval = worker_p->len(units, string);
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

String PGPlotter::numb(Int mm, Int pp, Int form)
{
    ok();
    String retval = worker_p->numb(mm, pp, form);
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

void PGPlotter::panl(Int ix, Int iy)
{
    ok();
    worker_p->panl(ix, iy);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::pap(Float width, Float aspect)
{
    ok();
    worker_p->pap(width, aspect);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::pixl(const Matrix<Int> &ia, Float x1, Float x2,
		      Float y1, Float y2)
{
    ok();
    worker_p->pixl(ia, x1, x2, y1, y2);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::pnts(const Vector<Float> &x, const Vector<Float> &y,
		      const Vector<Int> symbol)
{
    ok();
    worker_p->pnts(x, y, symbol);
    if (!worker_p->isAttached()) worker_p = 0;
}

Vector<Float>  PGPlotter::qah()
{
    ok();
    Vector<Float> retval = worker_p->qah();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Int PGPlotter::qcf()
{
    ok();
    Int retval = worker_p->qcf();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Float PGPlotter::qch()
{
    ok();
    Float retval = worker_p->qch();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Vector<Float> PGPlotter::qcr(Int ci)
{
    ok();
    Vector<Float> retval = worker_p->qcr(ci);
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Vector<Float> PGPlotter::qcs(Int units)
{
    ok();
    Vector<Float> retval = worker_p->qcs(units);
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Int PGPlotter::qfs()
{
    ok();
    Int retval = worker_p->qfs();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Vector<Float> PGPlotter::qhs()
{
    ok();
    Vector<Float> retval = worker_p->qhs();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Int PGPlotter::qid()
{
    ok();
    Int retval = worker_p->qid();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

String PGPlotter::qinf(const String &item)
{
    ok();
    String retval = worker_p->qinf(item);
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Int PGPlotter::qitf()
{
    ok();
    Int retval = worker_p->qitf();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Int PGPlotter::qls()
{
    ok();
    Int retval = worker_p->qls();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Int PGPlotter::qlw()
{
    ok();
    Int retval = worker_p->qlw();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Vector<Float> PGPlotter::qpos()
{
    ok();
    Vector<Float> retval = worker_p->qpos();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Vector<Float> PGPlotter::qvp(Int units)
{
    ok();
    Vector<Float> retval = worker_p->qvp(units);
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Vector<Float> PGPlotter::qvsz(Int units)
{
    ok();
    Vector<Float> retval = worker_p->qvsz(units);
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Float PGPlotter::rnd(Float x, Int nsub)
{
    ok();
    Float retval = worker_p->rnd(x, nsub);
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Vector<Float> PGPlotter::rnge(Float x1, Float x2)
{
    ok();
    Vector<Float> retval = worker_p->rnge(x1, x2);
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

void PGPlotter::scf(Int font)
{
    ok();
    worker_p->scf(font);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::scrn(Int ci, const String &name)
{
    ok();
    worker_p->scrn(ci, name);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::shls(Int ci, Float ch, Float cl, Float cs)
{
    ok();
    worker_p->shls(ci, ch, cl, cs);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::shs(Float angle, Float sepn, Float phase)
{
    ok();
    worker_p->shs(angle, sepn, phase);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::vect(const Matrix<Float> &a, const Matrix<Float> &b,
		      Float c, Int nc, 
		      const Vector<Float> &tr, Float blank)
{
    ok();
    worker_p->vect(a, b, c, nc, tr, blank);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::vsiz(Float xleft, Float xright, Float ybot,
		      Float ytop)
{
    ok();
    worker_p->vsiz(xleft, xright, ybot, ytop);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::wedg(const String &side, Float disp, Float width,
		      Float fg, Float bg, const String &label)
{
    ok();
    worker_p->wedg(side, disp, width, fg, bg, label);
    if (!worker_p->isAttached()) worker_p = 0;
}

} //# NAMESPACE CASACORE - END

