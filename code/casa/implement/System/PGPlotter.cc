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

#include <trial/Tasking/PGPlotter.h>
#include <aips/Exceptions/Error.h>
#include <aips/Arrays/Vector.h>

PGPlotter::PGPlotter() : worker_p(0)
{
    // Nothing
}

PGPlotter::PGPlotter(const PGPlotter &other)
    : worker_p(other.worker_p)
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

void PGPlotter::arro(Float x1, Float y1, Float x2, Float y2)
{
    ok();
    worker_p->arro(x1, y1, x2, y2);
}

void PGPlotter::ask(Bool flag)
{
    ok();
    worker_p->ask(flag);
}

void PGPlotter::bbuf()
{
    ok();
    worker_p->bbuf();
}

void PGPlotter::box(const String &xopt, Float xtick, Int nxsub, 
	     const String &yopt, Float ytick, Int nysub)
{
    ok();
    worker_p->box(xopt, xtick, nxsub, yopt, ytick, nysub);
}

void PGPlotter::circ(Float xcent, Float ycent, Float radius)
{
    ok();
    worker_p->circ(xcent, ycent, radius);
}

void PGPlotter::draw(Float x, Float y)
{
    ok();
    worker_p->draw(x, y);
}

void PGPlotter::ebuf()
{
    ok();
    worker_p->ebuf();
}

void PGPlotter::env(Float xmin, Float xmax, Float ymin, Float ymax, Int just,
	     Int axis)
{
    ok();
    worker_p->env(xmin, xmax, ymin, ymax, just, axis);
}

void PGPlotter::eras()
{
    ok();
    worker_p->eras();
}

void PGPlotter::errb(Int dir, const Vector<Float> &x, const Vector<Float> &y,
	      const Vector<Float> &e, Float t)
{
    ok();
    worker_p->errb(dir, x, y, e, t);
}

void PGPlotter::erry(const Vector<Float> &x, const Vector<Float> &y1,
	      const Vector<Float> &y2, Float t)
{
    ok();
    worker_p->erry(x, y1, y2, t);
}

void PGPlotter::hist(const Vector<Float> &data, Float datamin, Float datamax, 
		    Int nbin, Int pcflag)
{
    ok();
    worker_p->hist(data, datamin, datamax, nbin, pcflag);
}

void PGPlotter::lab(const String &xlbl, const String &ylbl, 
		   const String &toplbl)
{
    ok();
    worker_p->lab(xlbl, ylbl, toplbl);
}

void PGPlotter::line(const Vector<Float> &xpts, const Vector<Float> &ypts)
{
    ok();
    worker_p->line(xpts, ypts);
}

void PGPlotter::move(Float x, Float y)
{
    ok();
    worker_p->move(x, y);
}

void PGPlotter::mtxt(const String &side, Float disp, Float coord, Float fjust,
		    const String &text)
{
    ok();
    worker_p->mtxt(side, disp, coord, fjust, text);
}

void PGPlotter::page()
{
    ok();
    worker_p->page();
}

void PGPlotter::poly(const Vector<Float> &xpts, const Vector<Float> &ypts)
{
    ok();
    worker_p->poly(xpts, ypts);
}

void PGPlotter::pt(const Vector<Float> &xpts, const Vector<Float> &ypts, 
		  Int symbol)
{
    ok();
    worker_p->pt(xpts, ypts, symbol);
}

void PGPlotter::ptxt(Float x, Float y, Float angle, Float fjust, 
		    const String &text)
{
    ok();
    worker_p->ptxt(x, y, angle, fjust, text);
}

Int PGPlotter::qci()
{
    ok();
    return worker_p->qci();
}

Int PGPlotter::qtbg()
{
    ok();
    return worker_p->qtbg();
}

Vector<Float> PGPlotter::qtxt(Float x, Float y, Float angle, Float fjust, 
		    const String &text)
{
    ok();
    return worker_p->qtxt(x, y, angle, fjust, text);
}

Vector<Float> PGPlotter::qwin()
{
    ok();
    return worker_p->qwin();
}

void PGPlotter::rect(Float x1, Float x2, Float y1, Float y2)
{
    ok();
    worker_p->rect(x1, x2, y1, y2);
}

void PGPlotter::sah(Int fs, Float angle, Float vent)
{
    ok();
    worker_p->sah(fs, angle, vent);
}

void PGPlotter::save()
{
    ok();
    worker_p->save();
}

void PGPlotter::sch(Float size)
{
    ok();
    worker_p->sch(size);
}

void PGPlotter::sci(Int ci)
{
    ok();
    worker_p->sci(ci);
}

void PGPlotter::scr(Int ci, Float cr, Float cg, Float cb)
{
    ok();
    worker_p->scr(ci, cr, cg, cb);
}

void PGPlotter::sfs(Int fs)
{
    ok();
    worker_p->sfs(fs);
}

void PGPlotter::sls(Int ls)
{
    ok();
    worker_p->sls(ls);
}

void PGPlotter::slw(Int lw)
{
    ok();
    worker_p->slw(lw);
}

void PGPlotter::stbg(Int tbci)
{
    ok();
    worker_p->stbg(tbci);
}

void PGPlotter::subp(Int nxsub, Int nysub)
{
    ok();
    worker_p->subp(nxsub, nysub);
}

void PGPlotter::svp(Float xleft, Float xright, Float ybot, Float ytop)
{
    ok();
    worker_p->svp(xleft, xright, ybot, ytop);
}

void PGPlotter::swin(Float x1, Float x2, Float y1, Float y2)
{
    ok();
    worker_p->swin(x1, x2, y1, y2);
}

void PGPlotter::tbox(const String &xopt, Float xtick, Int nxsub,
		    const String &yopt, Float ytick, Int nysub)
{
    ok();
    worker_p->tbox(xopt, xtick, nxsub, yopt, ytick, nysub);
}

void PGPlotter::text(Float x, Float y, const String &text)
{
    ok();
    worker_p->text(x, y, text);
}

void PGPlotter::unsa()
{
    ok();
    worker_p->unsa();
}

void PGPlotter::updt()
{
    ok();
    worker_p->updt();
}

void PGPlotter::vstd()
{
    ok();
    worker_p->vstd();
}

void PGPlotter::wnad(Float x1, Float x2, Float y1, Float y2)
{
    ok();
    worker_p->wnad(x1, x2,  y1,  y2);
}

void PGPlotter::ok() const
{
    if (!isAttached()) {
	throw(AipsError("Attempt to plot to an unattached PGPlotter!"));
    }
}
