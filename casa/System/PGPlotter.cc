// # PGPlotter.cc: Standard plotting object for application programmers.
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

#include <casacore/casa/System/PGPlotter.h>
#include <casacore/casa/System/PGPlotterNull.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/Record.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

// # Default is no create function, thus use createLocal.
PGPlotter::CreateFunction *PGPlotter::creator_p = nullptr;

PGPlotter::PGPlotter() {
  // Nothing
}

PGPlotter::PGPlotter(PGPlotterInterface *worker) : worker_p(worker) {
  // Nothing
}

PGPlotter::PGPlotter(const String &device, unsigned int mincolors, unsigned int maxcolors,
                     unsigned int sizex, unsigned int sizey) {
  *this = create(device, mincolors, maxcolors, sizex, sizey);
}

PGPlotter::PGPlotter(const PGPlotter &other) : PGPlotterInterface(), worker_p(other.worker_p) {
  // Nothing
}

PGPlotter &PGPlotter::operator=(const PGPlotter &other) {
  worker_p = other.worker_p;
  return *this;
}

PGPlotter::~PGPlotter() {
  // Nothing
}

PGPlotter PGPlotter::create(const String &device, unsigned int mincolors, unsigned int maxcolors,
                            unsigned int sizex, unsigned int sizey) {
  if (creator_p == nullptr) {
    return PGPlotterNull::createPlotter(device, mincolors, maxcolors, sizex, sizey);
  }
  return creator_p(device, mincolors, maxcolors, sizex, sizey);
}

PGPlotter::CreateFunction *PGPlotter::setCreateFunction(PGPlotter::CreateFunction *func,
                                                        bool override) {
  CreateFunction *tmp = creator_p;
  if (override || tmp == nullptr) {
    creator_p = func;
  }
  return tmp;
}

void PGPlotter::detach() {
  worker_p->resetPlotNumber();  // Implemented for PGPlotterGlish only
  std::shared_ptr<PGPlotterInterface> empty;
  worker_p = empty;
}

bool PGPlotter::isAttached() const { return (static_cast<bool>(worker_p)); }

void PGPlotter::message(const String &text) {
  ok();
  worker_p->message(text);
}

Record PGPlotter::curs(float x, float y) {
  ok();
  Record retval = worker_p->curs(x, y);
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

void PGPlotter::arro(float x1, float y1, float x2, float y2) {
  ok();
  worker_p->arro(x1, y1, x2, y2);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::ask(bool flag) {
  ok();
  worker_p->ask(flag);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::bbuf() {
  ok();
  worker_p->bbuf();
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::box(const String &xopt, float xtick, int nxsub, const String &yopt, float ytick,
                    int nysub) {
  ok();
  worker_p->box(xopt, xtick, nxsub, yopt, ytick, nysub);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::circ(float xcent, float ycent, float radius) {
  ok();
  worker_p->circ(xcent, ycent, radius);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::draw(float x, float y) {
  ok();
  worker_p->draw(x, y);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::ebuf() {
  ok();
  worker_p->ebuf();
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::env(float xmin, float xmax, float ymin, float ymax, int just, int axis) {
  ok();
  worker_p->env(xmin, xmax, ymin, ymax, just, axis);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::eras() {
  ok();
  worker_p->eras();
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::errb(int dir, const Vector<float> &x, const Vector<float> &y,
                     const Vector<float> &e, float t) {
  ok();
  worker_p->errb(dir, x, y, e, t);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::erry(const Vector<float> &x, const Vector<float> &y1, const Vector<float> &y2,
                     float t) {
  ok();
  worker_p->erry(x, y1, y2, t);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::hist(const Vector<float> &data, float datmin, float datmax, int nbin, int pcflag) {
  ok();
  worker_p->hist(data, datmin, datmax, nbin, pcflag);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::lab(const String &xlbl, const String &ylbl, const String &toplbl) {
  ok();
  worker_p->lab(xlbl, ylbl, toplbl);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::line(const Vector<float> &xpts, const Vector<float> &ypts) {
  ok();
  worker_p->line(xpts, ypts);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::move(float x, float y) {
  ok();
  worker_p->move(x, y);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::mtxt(const String &side, float disp, float coord, float fjust, const String &text) {
  ok();
  worker_p->mtxt(side, disp, coord, fjust, text);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::page() {
  ok();
  worker_p->page();
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::poly(const Vector<float> &xpts, const Vector<float> &ypts) {
  ok();
  worker_p->poly(xpts, ypts);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::pt(const Vector<float> &xpts, const Vector<float> &ypts, int symbol) {
  ok();
  worker_p->pt(xpts, ypts, symbol);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::ptxt(float x, float y, float angle, float fjust, const String &text) {
  ok();
  worker_p->ptxt(x, y, angle, fjust, text);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

int PGPlotter::qci() {
  ok();
  int retval = worker_p->qci();
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

int PGPlotter::qtbg() {
  ok();
  int retval = worker_p->qtbg();
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

Vector<float> PGPlotter::qtxt(float x, float y, float angle, float fjust, const String &text) {
  ok();
  Vector<float> retval = worker_p->qtxt(x, y, angle, fjust, text);
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

Vector<float> PGPlotter::qwin() {
  ok();
  Vector<float> retval = worker_p->qwin();
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

void PGPlotter::rect(float x1, float x2, float y1, float y2) {
  ok();
  worker_p->rect(x1, x2, y1, y2);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::sah(int fs, float angle, float vent) {
  ok();
  worker_p->sah(fs, angle, vent);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::save() {
  ok();
  worker_p->save();
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::sch(float size) {
  ok();
  worker_p->sch(size);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::sci(int ci) {
  ok();
  worker_p->sci(ci);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::scr(int ci, float cr, float cg, float cb) {
  ok();
  worker_p->scr(ci, cr, cg, cb);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::sfs(int fs) {
  ok();
  worker_p->sfs(fs);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::sls(int ls) {
  ok();
  worker_p->sls(ls);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::slw(int lw) {
  ok();
  worker_p->slw(lw);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::stbg(int tbci) {
  ok();
  worker_p->stbg(tbci);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::subp(int nxsub, int nysub) {
  ok();
  worker_p->subp(nxsub, nysub);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::svp(float xleft, float xright, float ybot, float ytop) {
  ok();
  worker_p->svp(xleft, xright, ybot, ytop);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::swin(float x1, float x2, float y1, float y2) {
  ok();
  worker_p->swin(x1, x2, y1, y2);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::tbox(const String &xopt, float xtick, int nxsub, const String &yopt, float ytick,
                     int nysub) {
  ok();
  worker_p->tbox(xopt, xtick, nxsub, yopt, ytick, nysub);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::text(float x, float y, const String &text) {
  ok();
  worker_p->text(x, y, text);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::unsa() {
  ok();
  worker_p->unsa();
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::updt() {
  ok();
  worker_p->updt();
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::vstd() {
  ok();
  worker_p->vstd();
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::wnad(float x1, float x2, float y1, float y2) {
  ok();
  worker_p->wnad(x1, x2, y1, y2);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::ok() const {
  if (!isAttached()) {
    throw(AipsError("Attempt to plot to an unattached PGPlotter!"));
  }
}

void PGPlotter::conl(const Matrix<float> &a, float c, const Vector<float> &tr, const String &label,
                     int intval, int minint) {
  ok();
  worker_p->conl(a, c, tr, label, intval, minint);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::cont(const Matrix<float> &a, const Vector<float> &c, bool nc,
                     const Vector<float> &tr) {
  ok();
  worker_p->cont(a, c, nc, tr);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::ctab(const Vector<float> &l, const Vector<float> &r, const Vector<float> &g,
                     const Vector<float> &b, float contra, float bright) {
  ok();
  worker_p->ctab(l, r, g, b, contra, bright);
  if (!worker_p->isAttached()) worker_p = nullptr;
}
void PGPlotter::gray(const Matrix<float> &a, float fg, float bg, const Vector<float> &tr) {
  ok();
  worker_p->gray(a, fg, bg, tr);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::iden() {
  ok();
  worker_p->iden();
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::imag(const Matrix<float> &a, float a1, float a2, const Vector<float> &tr) {
  ok();
  worker_p->imag(a, a1, a2, tr);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

Vector<int> PGPlotter::qcir() {
  ok();
  Vector<int> retval = worker_p->qcir();
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

Vector<int> PGPlotter::qcol() {
  ok();
  Vector<int> retval = worker_p->qcol();
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

void PGPlotter::scir(int icilo, int icihi) {
  ok();
  worker_p->scir(icilo, icihi);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::sitf(int itf) {
  ok();
  worker_p->sitf(itf);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::bin(const Vector<float> &x, const Vector<float> &data, bool center) {
  ok();
  worker_p->bin(x, data, center);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::conb(const Matrix<float> &a, const Vector<float> &c, const Vector<float> &tr,
                     float blank) {
  ok();
  worker_p->conb(a, c, tr, blank);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::cons(const Matrix<float> &a, const Vector<float> &c, const Vector<float> &tr) {
  ok();
  worker_p->cons(a, c, tr);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::errx(const Vector<float> &x1, const Vector<float> &x2, const Vector<float> &y,
                     float t) {
  ok();
  worker_p->errx(x1, x2, y, t);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::hi2d(const Matrix<float> &data, const Vector<float> &x, int ioff, float bias,
                     bool center, const Vector<float> &ylims) {
  ok();
  worker_p->hi2d(data, x, ioff, bias, center, ylims);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::ldev() {
  ok();
  worker_p->ldev();
  if (!worker_p->isAttached()) worker_p = nullptr;
}

Vector<float> PGPlotter::len(int units, const String &string) {
  ok();
  Vector<float> retval = worker_p->len(units, string);
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

String PGPlotter::numb(int mm, int pp, int form) {
  ok();
  String retval = worker_p->numb(mm, pp, form);
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

void PGPlotter::panl(int ix, int iy) {
  ok();
  worker_p->panl(ix, iy);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::pap(float width, float aspect) {
  ok();
  worker_p->pap(width, aspect);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::pixl(const Matrix<int> &ia, float x1, float x2, float y1, float y2) {
  ok();
  worker_p->pixl(ia, x1, x2, y1, y2);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::pnts(const Vector<float> &x, const Vector<float> &y, const Vector<int> symbol) {
  ok();
  worker_p->pnts(x, y, symbol);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

Vector<float> PGPlotter::qah() {
  ok();
  Vector<float> retval = worker_p->qah();
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

int PGPlotter::qcf() {
  ok();
  int retval = worker_p->qcf();
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

float PGPlotter::qch() {
  ok();
  float retval = worker_p->qch();
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

Vector<float> PGPlotter::qcr(int ci) {
  ok();
  Vector<float> retval = worker_p->qcr(ci);
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

Vector<float> PGPlotter::qcs(int units) {
  ok();
  Vector<float> retval = worker_p->qcs(units);
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

int PGPlotter::qfs() {
  ok();
  int retval = worker_p->qfs();
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

Vector<float> PGPlotter::qhs() {
  ok();
  Vector<float> retval = worker_p->qhs();
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

int PGPlotter::qid() {
  ok();
  int retval = worker_p->qid();
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

String PGPlotter::qinf(const String &item) {
  ok();
  String retval = worker_p->qinf(item);
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

int PGPlotter::qitf() {
  ok();
  int retval = worker_p->qitf();
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

int PGPlotter::qls() {
  ok();
  int retval = worker_p->qls();
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

int PGPlotter::qlw() {
  ok();
  int retval = worker_p->qlw();
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

Vector<float> PGPlotter::qpos() {
  ok();
  Vector<float> retval = worker_p->qpos();
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

Vector<float> PGPlotter::qvp(int units) {
  ok();
  Vector<float> retval = worker_p->qvp(units);
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

Vector<float> PGPlotter::qvsz(int units) {
  ok();
  Vector<float> retval = worker_p->qvsz(units);
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

float PGPlotter::rnd(float x, int nsub) {
  ok();
  float retval = worker_p->rnd(x, nsub);
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

Vector<float> PGPlotter::rnge(float x1, float x2) {
  ok();
  Vector<float> retval = worker_p->rnge(x1, x2);
  if (!worker_p->isAttached()) worker_p = nullptr;
  return retval;
}

void PGPlotter::scf(int font) {
  ok();
  worker_p->scf(font);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::scrn(int ci, const String &name) {
  ok();
  worker_p->scrn(ci, name);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::shls(int ci, float ch, float cl, float cs) {
  ok();
  worker_p->shls(ci, ch, cl, cs);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::shs(float angle, float sepn, float phase) {
  ok();
  worker_p->shs(angle, sepn, phase);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::vect(const Matrix<float> &a, const Matrix<float> &b, float c, int nc,
                     const Vector<float> &tr, float blank) {
  ok();
  worker_p->vect(a, b, c, nc, tr, blank);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::vsiz(float xleft, float xright, float ybot, float ytop) {
  ok();
  worker_p->vsiz(xleft, xright, ybot, ytop);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

void PGPlotter::wedg(const String &side, float disp, float width, float fg, float bg,
                     const String &label) {
  ok();
  worker_p->wedg(side, disp, width, fg, bg, label);
  if (!worker_p->isAttached()) worker_p = nullptr;
}

}  // namespace casacore
