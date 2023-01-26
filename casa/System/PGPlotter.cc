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
		      uint32_t mincolors, uint32_t maxcolors,
		      uint32_t sizex, uint32_t sizey)
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
			     uint32_t mincolors, uint32_t maxcolors,
			     uint32_t sizex, uint32_t sizey)
{
    if (creator_p == 0) {
        return PGPlotterNull::createPlotter(device, mincolors, maxcolors,
					    sizex, sizey);
    }
    return creator_p (device, mincolors, maxcolors, sizex, sizey);
}

PGPlotter::CreateFunction* PGPlotter::setCreateFunction
                                          (PGPlotter::CreateFunction* func,
					   bool override)
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

bool PGPlotter::isAttached() const
{
    return (!worker_p.null());
}

void PGPlotter::message(const String &text)
{
    ok();
    worker_p->message(text);
}

Record PGPlotter::curs(float x, float y)
{
    ok();
    Record retval =  worker_p->curs(x, y);
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

void PGPlotter::arro(float x1, float y1, float x2, float y2)
{
    ok();
    worker_p->arro(x1, y1, x2, y2);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::ask(bool flag)
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

void PGPlotter::box(const String &xopt, float xtick, int32_t nxsub, 
	     const String &yopt, float ytick, int32_t nysub)
{
    ok();
    worker_p->box(xopt, xtick, nxsub, yopt, ytick, nysub);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::circ(float xcent, float ycent, float radius)
{
    ok();
    worker_p->circ(xcent, ycent, radius);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::draw(float x, float y)
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

void PGPlotter::env(float xmin, float xmax, float ymin, float ymax, int32_t just,
	     int32_t axis)
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

void PGPlotter::errb(int32_t dir, const Vector<float> &x, const Vector<float> &y,
	      const Vector<float> &e, float t)
{
    ok();
    worker_p->errb(dir, x, y, e, t);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::erry(const Vector<float> &x, const Vector<float> &y1,
	      const Vector<float> &y2, float t)
{
    ok();
    worker_p->erry(x, y1, y2, t);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::hist(const Vector<float> &data, float datmin, float datmax, 
		    int32_t nbin, int32_t pcflag)
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

void PGPlotter::line(const Vector<float> &xpts, const Vector<float> &ypts)
{
    ok();
    worker_p->line(xpts, ypts);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::move(float x, float y)
{
    ok();
    worker_p->move(x, y);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::mtxt(const String &side, float disp, float coord, float fjust,
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

void PGPlotter::poly(const Vector<float> &xpts, const Vector<float> &ypts)
{
    ok();
    worker_p->poly(xpts, ypts);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::pt(const Vector<float> &xpts, const Vector<float> &ypts, 
		  int32_t symbol)
{
    ok();
    worker_p->pt(xpts, ypts, symbol);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::ptxt(float x, float y, float angle, float fjust, 
		    const String &text)
{
    ok();
    worker_p->ptxt(x, y, angle, fjust, text);
    if (!worker_p->isAttached()) worker_p = 0;
}

int32_t PGPlotter::qci()
{
    ok();
    int32_t retval = worker_p->qci();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

int32_t PGPlotter::qtbg()
{
    ok();
    int32_t retval = worker_p->qtbg();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Vector<float> PGPlotter::qtxt(float x, float y, float angle, float fjust, 
		    const String &text)
{
    ok();
    Vector<float> retval = worker_p->qtxt(x, y, angle, fjust, text);
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Vector<float> PGPlotter::qwin()
{
    ok();
    Vector<float> retval = worker_p->qwin();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

void PGPlotter::rect(float x1, float x2, float y1, float y2)
{
    ok();
    worker_p->rect(x1, x2, y1, y2);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::sah(int32_t fs, float angle, float vent)
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

void PGPlotter::sch(float size)
{
    ok();
    worker_p->sch(size);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::sci(int32_t ci)
{
    ok();
    worker_p->sci(ci);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::scr(int32_t ci, float cr, float cg, float cb)
{
    ok();
    worker_p->scr(ci, cr, cg, cb);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::sfs(int32_t fs)
{
    ok();
    worker_p->sfs(fs);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::sls(int32_t ls)
{
    ok();
    worker_p->sls(ls);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::slw(int32_t lw)
{
    ok();
    worker_p->slw(lw);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::stbg(int32_t tbci)
{
    ok();
    worker_p->stbg(tbci);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::subp(int32_t nxsub, int32_t nysub)
{
    ok();
    worker_p->subp(nxsub, nysub);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::svp(float xleft, float xright, float ybot, float ytop)
{
    ok();
    worker_p->svp(xleft, xright, ybot, ytop);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::swin(float x1, float x2, float y1, float y2)
{
    ok();
    worker_p->swin(x1, x2, y1, y2);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::tbox(const String &xopt, float xtick, int32_t nxsub,
		    const String &yopt, float ytick, int32_t nysub)
{
    ok();
    worker_p->tbox(xopt, xtick, nxsub, yopt, ytick, nysub);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::text(float x, float y, const String &text)
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

void PGPlotter::wnad(float x1, float x2, float y1, float y2)
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

void PGPlotter::conl(const Matrix<float> &a, float c,
		      const Vector<float> &tr, const String &label,
		      int32_t intval, int32_t minint)
{
    ok();
    worker_p->conl(a, c, tr, label, intval, minint);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::cont(const Matrix<float> &a, const Vector<float> &c,
		      bool nc, const Vector<float> &tr)
{
    ok();
    worker_p->cont(a, c, nc, tr);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::ctab(const Vector<float> &l, const Vector<float> &r,
		      const Vector<float> &g, const Vector<float> &b,
		      float contra, float bright)
{
    ok();
    worker_p->ctab(l, r, g, b, contra, bright);
    if (!worker_p->isAttached()) worker_p = 0;
}
void PGPlotter::gray(const Matrix<float> &a, float fg, float bg,
		      const Vector<float> &tr)
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

void PGPlotter::imag(const Matrix<float> &a, float a1, float a2,
		      const Vector<float> &tr)
{
    ok();
    worker_p->imag(a, a1, a2, tr);
    if (!worker_p->isAttached()) worker_p = 0;
}

Vector<int32_t> PGPlotter::qcir()
{
    ok();
    Vector<int32_t> retval = worker_p->qcir();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Vector<int32_t> PGPlotter::qcol()
{
    ok();
    Vector<int32_t> retval = worker_p->qcol();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

void PGPlotter::scir(int32_t icilo, int32_t icihi)
{
    ok();
    worker_p->scir(icilo, icihi);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::sitf(int32_t itf)
{
    ok();
    worker_p->sitf(itf);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::bin(const Vector<float> &x, const Vector<float> &data,
		     bool center)
{
    ok();
    worker_p->bin(x, data, center);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::conb(const Matrix<float> &a, const Vector<float> &c,
		      const Vector<float> &tr, float blank)
{
    ok();
    worker_p->conb(a, c, tr, blank);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::cons(const Matrix<float> &a, const Vector<float> &c,
		      const Vector<float> &tr)
{
    ok();
    worker_p->cons(a, c, tr);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::errx(const Vector<float> &x1, const Vector<float> &x2,
		      const Vector<float> &y, float t)
{
    ok();
    worker_p->errx(x1, x2, y, t);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::hi2d(const Matrix<float> &data, const Vector<float> &x,
		      int32_t ioff, float bias, bool center, 
		      const Vector<float> &ylims)
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

Vector<float> PGPlotter::len(int32_t units, const String &string)
{
    ok();
    Vector<float> retval = worker_p->len(units, string);
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

String PGPlotter::numb(int32_t mm, int32_t pp, int32_t form)
{
    ok();
    String retval = worker_p->numb(mm, pp, form);
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

void PGPlotter::panl(int32_t ix, int32_t iy)
{
    ok();
    worker_p->panl(ix, iy);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::pap(float width, float aspect)
{
    ok();
    worker_p->pap(width, aspect);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::pixl(const Matrix<int32_t> &ia, float x1, float x2,
		      float y1, float y2)
{
    ok();
    worker_p->pixl(ia, x1, x2, y1, y2);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::pnts(const Vector<float> &x, const Vector<float> &y,
		      const Vector<int32_t> symbol)
{
    ok();
    worker_p->pnts(x, y, symbol);
    if (!worker_p->isAttached()) worker_p = 0;
}

Vector<float>  PGPlotter::qah()
{
    ok();
    Vector<float> retval = worker_p->qah();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

int32_t PGPlotter::qcf()
{
    ok();
    int32_t retval = worker_p->qcf();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

float PGPlotter::qch()
{
    ok();
    float retval = worker_p->qch();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Vector<float> PGPlotter::qcr(int32_t ci)
{
    ok();
    Vector<float> retval = worker_p->qcr(ci);
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Vector<float> PGPlotter::qcs(int32_t units)
{
    ok();
    Vector<float> retval = worker_p->qcs(units);
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

int32_t PGPlotter::qfs()
{
    ok();
    int32_t retval = worker_p->qfs();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Vector<float> PGPlotter::qhs()
{
    ok();
    Vector<float> retval = worker_p->qhs();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

int32_t PGPlotter::qid()
{
    ok();
    int32_t retval = worker_p->qid();
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

int32_t PGPlotter::qitf()
{
    ok();
    int32_t retval = worker_p->qitf();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

int32_t PGPlotter::qls()
{
    ok();
    int32_t retval = worker_p->qls();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

int32_t PGPlotter::qlw()
{
    ok();
    int32_t retval = worker_p->qlw();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Vector<float> PGPlotter::qpos()
{
    ok();
    Vector<float> retval = worker_p->qpos();
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Vector<float> PGPlotter::qvp(int32_t units)
{
    ok();
    Vector<float> retval = worker_p->qvp(units);
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Vector<float> PGPlotter::qvsz(int32_t units)
{
    ok();
    Vector<float> retval = worker_p->qvsz(units);
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

float PGPlotter::rnd(float x, int32_t nsub)
{
    ok();
    float retval = worker_p->rnd(x, nsub);
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

Vector<float> PGPlotter::rnge(float x1, float x2)
{
    ok();
    Vector<float> retval = worker_p->rnge(x1, x2);
    if (!worker_p->isAttached()) worker_p = 0;
    return retval;
}

void PGPlotter::scf(int32_t font)
{
    ok();
    worker_p->scf(font);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::scrn(int32_t ci, const String &name)
{
    ok();
    worker_p->scrn(ci, name);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::shls(int32_t ci, float ch, float cl, float cs)
{
    ok();
    worker_p->shls(ci, ch, cl, cs);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::shs(float angle, float sepn, float phase)
{
    ok();
    worker_p->shs(angle, sepn, phase);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::vect(const Matrix<float> &a, const Matrix<float> &b,
		      float c, int32_t nc, 
		      const Vector<float> &tr, float blank)
{
    ok();
    worker_p->vect(a, b, c, nc, tr, blank);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::vsiz(float xleft, float xright, float ybot,
		      float ytop)
{
    ok();
    worker_p->vsiz(xleft, xright, ybot, ytop);
    if (!worker_p->isAttached()) worker_p = 0;
}

void PGPlotter::wedg(const String &side, float disp, float width,
		      float fg, float bg, const String &label)
{
    ok();
    worker_p->wedg(side, disp, width, fg, bg, label);
    if (!worker_p->isAttached()) worker_p = 0;
}

} //# NAMESPACE CASACORE - END

