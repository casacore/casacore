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

#include <casacore/casa/System/PGPlotterNull.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

PGPlotterNull::PGPlotterNull(const String &)
    : beenWarned(true)
{
    // If this fails, we need a bit more development to copy float*'s to
    // float*'s.
    AlwaysAssertExit(sizeof(float) == sizeof(float));

    noplotter();
}

PGPlotterNull::~PGPlotterNull()
{
    noplotter();
}

PGPlotter PGPlotterNull::createPlotter (const String &device,
					 uint32_t, uint32_t, uint32_t, uint32_t)
{
    return PGPlotter (new PGPlotterNull (device));
}

Record PGPlotterNull::curs(float, float)
{
    Record retval;
    noplotter();
    return retval;
}


void PGPlotterNull::arro(float, float, float, float)
{
    noplotter();
}

void PGPlotterNull::ask(bool)
{
    noplotter();
}

void PGPlotterNull::bbuf()
{
    noplotter();
}

void PGPlotterNull::box(const String &, float, int32_t, 
                        const String &, float, int32_t)
{
    noplotter();
}

void PGPlotterNull::circ(float, float, float)
{
    noplotter();
}

void PGPlotterNull::draw(float, float)
{
    noplotter();
}

void PGPlotterNull::ebuf()
{
    noplotter();
}

void PGPlotterNull::env(float, float, float, float, 
			 int32_t, int32_t)
{
    noplotter();
}

void PGPlotterNull::eras()
{
    noplotter();
}

void PGPlotterNull::errb(int32_t, const Vector<float> &, 
                         const Vector<float> &,
                         const Vector<float> &, float)
{
    noplotter();
}

void PGPlotterNull::erry(const Vector<float> &, const Vector<float> &,
                         const Vector<float> &, float)
{
    noplotter();
}

void PGPlotterNull::hist(const Vector<float> &, float, 
                         float, 
                         int32_t, int32_t)
{
    noplotter();
}

void PGPlotterNull::lab(const String &, const String &, 
                        const String &)
{
    noplotter();
}

void PGPlotterNull::line(const Vector<float> &, const Vector<float> &)
{
    noplotter();
}

void PGPlotterNull::move(float, float)
{
    noplotter();
}

void PGPlotterNull::mtxt(const String &, float, float, 
                         float, const String &)
{
    noplotter();
}

void PGPlotterNull::page()
{
    noplotter();
}

void PGPlotterNull::poly(const Vector<float> &, const Vector<float> &)
{
    noplotter();
}

void PGPlotterNull::pt(const Vector<float> &, const Vector<float> &, 
                       int32_t)
{
    noplotter();
}

void PGPlotterNull::ptxt(float, float, float, float, 
                         const String &)
{
    noplotter();
}

int32_t PGPlotterNull::qci()
{
    noplotter();
    return 0;
}

int32_t PGPlotterNull::qtbg()
{
    noplotter();
    return 0;
}

Vector<float> PGPlotterNull::qtxt(float, float, float, float, 
                                  const String &)
{
    Vector<float> xboxybox(8);
    xboxybox = 0.;
    noplotter();
    return xboxybox;
}

Vector<float> PGPlotterNull::qwin()
{
    Vector<float> retval(4);
    retval = 0;
    noplotter();
    return retval;
}

void PGPlotterNull::rect(float, float, float, float)
{
    noplotter();
}

void PGPlotterNull::sah(int32_t, float, float)
{
    noplotter();
}

void PGPlotterNull::save()
{
    noplotter();
}

void PGPlotterNull::sch(float)
{
    noplotter();
}

void PGPlotterNull::sci(int32_t)
{
    noplotter();
}

void PGPlotterNull::scr(int32_t, float, float, float)
{
    noplotter();
}

void PGPlotterNull::sfs(int32_t)
{
    noplotter();
}

void PGPlotterNull::sls(int32_t)
{
    noplotter();
}

void PGPlotterNull::slw(int32_t)
{
    noplotter();
}

void PGPlotterNull::stbg(int32_t)
{
    noplotter();
}

void PGPlotterNull::subp(int32_t, int32_t)
{
    noplotter();
}

void PGPlotterNull::svp(float, float, float, float)
{
    noplotter();
}

void PGPlotterNull::swin(float, float, float, float)
{
    noplotter();
}

void PGPlotterNull::tbox(const String &, float, int32_t,
                         const String &, float, int32_t)
{
    noplotter();
}

void PGPlotterNull::text(float, float, const String &)
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

void PGPlotterNull::wnad(float, float, float, float)
{
    noplotter();
}

void PGPlotterNull::conl(const Matrix<float> &, float,
                         const Vector<float> &, const String &,
                         int32_t, int32_t)
{
    noplotter();
}

void PGPlotterNull::cont(const Matrix<float> &, const Vector<float> &,
                         bool, const Vector<float> &)
{
    noplotter();
}

void PGPlotterNull::ctab(const Vector<float> &, const Vector<float> &,
                         const Vector<float> &, const Vector<float> &,
                         float, float)
{
    noplotter();
}

void PGPlotterNull::gray(const Matrix<float> &, float, float,
                         const Vector<float> &)
{
    noplotter();
} 

void PGPlotterNull::iden()
{
    noplotter();
}

void PGPlotterNull::imag(const Matrix<float> &, float, float,
                         const Vector<float> &)
{
    noplotter();
}

Vector<int32_t> PGPlotterNull::qcir()
{
    Vector<int32_t> retval(2);
    retval = 0;
    noplotter();
    return retval;
}

Vector<int32_t> PGPlotterNull::qcol()
{
    Vector<int32_t> retval(2);
    retval = 0;
    noplotter();
    return retval;
}

void PGPlotterNull::scir(int32_t, int32_t)
{
    noplotter();
}

void PGPlotterNull::sitf(int32_t)
{
    noplotter();
}

void PGPlotterNull::bin(const Vector<float> &, const Vector<float> &,
                        bool)
{
    noplotter();
}

void PGPlotterNull::conb(const Matrix<float> &, const Vector<float> &,
                         const Vector<float> &, float)
{
    noplotter();
}

void PGPlotterNull::cons(const Matrix<float> &, const Vector<float> &,
                         const Vector<float> &)
{
    noplotter();
}

void PGPlotterNull::errx(const Vector<float> &, const Vector<float> &,
                         const Vector<float> &, float)
{
    noplotter();
}

void PGPlotterNull::hi2d(const Matrix<float> &, const Vector<float> &,
                         int32_t, float, bool, 
                         const Vector<float> &)
{
    noplotter();
}

void PGPlotterNull::ldev()
{
    noplotter();
}

Vector<float> PGPlotterNull::len(int32_t, const String &)
{
    Vector<float> retval(2);
    retval =0;
    noplotter();
    return retval;
}

String PGPlotterNull::numb(int32_t, int32_t, int32_t)
{
    noplotter();
    return String();
}

void PGPlotterNull::panl(int32_t, int32_t)
{
    noplotter();
}

void PGPlotterNull::pap(float, float)
{
    noplotter();
}

void PGPlotterNull::pixl(const Matrix<int32_t> &, float, float,
                         float, float)
{
    noplotter();
}

void PGPlotterNull::pnts(const Vector<float> &, const Vector<float> &,
                         const Vector<int32_t>)
{
    noplotter();
}

Vector<float>  PGPlotterNull::qah()
{
    Vector<float> retval(3);
    retval = 0;
    noplotter();
    return retval;
}

int32_t PGPlotterNull::qcf()
{
    noplotter();
    return 0;
}

float PGPlotterNull::qch()
{
    noplotter();
    return 0;
}

Vector<float> PGPlotterNull::qcr(int32_t)
{
    Vector<float> retval(3);
    retval = 0;
    noplotter();
    return retval;
}

Vector<float> PGPlotterNull::qcs(int32_t)
{
    Vector<float> retval(2);
    retval = 0;
    noplotter();
    return retval;
}

int32_t PGPlotterNull::qfs()
{
    noplotter();
    return 0;
}

Vector<float> PGPlotterNull::qhs()
{
    Vector<float> retval(3);
    retval = 0;
    noplotter();
    return retval;
}

int32_t PGPlotterNull::qid()
{
    noplotter();
    return 0;
}

String PGPlotterNull::qinf(const String &)
{
    noplotter();
    return String();
}

int32_t PGPlotterNull::qitf()
{
    noplotter();
    return 0;
}

int32_t PGPlotterNull::qls()
{
    noplotter();
    return 0;
}

int32_t PGPlotterNull::qlw()
{
    noplotter();
    return 0;
}

Vector<float> PGPlotterNull::qpos()
{
    Vector<float> retval(2);
    retval = 0;
    noplotter();
    return retval;
}

Vector<float> PGPlotterNull::qvp(int32_t)
{
    Vector<float> retval(4);
    retval = 0;
    noplotter();
    return retval;
}

Vector<float> PGPlotterNull::qvsz(int32_t)
{
    Vector<float> retval(4);
    retval = 0;
    noplotter();
    return retval;
}

float PGPlotterNull::rnd(float x, int32_t)
{
    noplotter();
    return x;
}

Vector<float> PGPlotterNull::rnge(float, float)
{
    Vector<float> retval(2);
    retval = 0;
    noplotter();
    return retval;
}

void PGPlotterNull::scf(int32_t)
{
  noplotter();
}

void PGPlotterNull::scrn(int32_t, const String &)
{
  noplotter();
}

void PGPlotterNull::shls(int32_t, float, float, float)
{
  noplotter();
}

void PGPlotterNull::shs(float, float, float)
{
  noplotter();
}

void PGPlotterNull::vect(const Matrix<float> &, const Matrix<float> &,
                         float, int32_t, 
                         const Vector<float> &, float)
{
  noplotter();
}

  void PGPlotterNull::vsiz(float, float, float,
                           float)
{
  noplotter();
}

void PGPlotterNull::wedg(const String &, float, float,
                         float, float, const String &)
{
  noplotter();
}

void PGPlotterNull::noplotter()
{
    if(!beenWarned){
       std::cerr << "Warning no plotter attached.  Attach a plotter to get plots" << std::endl;
       beenWarned = true;
    }
}

} //# NAMESPACE CASACORE - END

