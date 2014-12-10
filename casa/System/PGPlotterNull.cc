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

Record PGPlotterNull::curs(Float, Float)
{
    Record retval;
    noplotter();
    return retval;
}


void PGPlotterNull::arro(Float, Float, Float, Float)
{
    noplotter();
}

void PGPlotterNull::ask(Bool)
{
    noplotter();
}

void PGPlotterNull::bbuf()
{
    noplotter();
}

void PGPlotterNull::box(const String &, Float, Int, 
                        const String &, Float, Int)
{
    noplotter();
}

void PGPlotterNull::circ(Float, Float, Float)
{
    noplotter();
}

void PGPlotterNull::draw(Float, Float)
{
    noplotter();
}

void PGPlotterNull::ebuf()
{
    noplotter();
}

void PGPlotterNull::env(Float, Float, Float, Float, 
			 Int, Int)
{
    noplotter();
}

void PGPlotterNull::eras()
{
    noplotter();
}

void PGPlotterNull::errb(Int, const Vector<Float> &, 
                         const Vector<Float> &,
                         const Vector<Float> &, Float)
{
    noplotter();
}

void PGPlotterNull::erry(const Vector<Float> &, const Vector<Float> &,
                         const Vector<Float> &, Float)
{
    noplotter();
}

void PGPlotterNull::hist(const Vector<Float> &, Float, 
                         Float, 
                         Int, Int)
{
    noplotter();
}

void PGPlotterNull::lab(const String &, const String &, 
                        const String &)
{
    noplotter();
}

void PGPlotterNull::line(const Vector<Float> &, const Vector<Float> &)
{
    noplotter();
}

void PGPlotterNull::move(Float, Float)
{
    noplotter();
}

void PGPlotterNull::mtxt(const String &, Float, Float, 
                         Float, const String &)
{
    noplotter();
}

void PGPlotterNull::page()
{
    noplotter();
}

void PGPlotterNull::poly(const Vector<Float> &, const Vector<Float> &)
{
    noplotter();
}

void PGPlotterNull::pt(const Vector<Float> &, const Vector<Float> &, 
                       Int)
{
    noplotter();
}

void PGPlotterNull::ptxt(Float, Float, Float, Float, 
                         const String &)
{
    noplotter();
}

Int PGPlotterNull::qci()
{
    noplotter();
    return 0;
}

Int PGPlotterNull::qtbg()
{
    noplotter();
    return 0;
}

Vector<Float> PGPlotterNull::qtxt(Float, Float, Float, Float, 
                                  const String &)
{
    Vector<Float> xboxybox(8);
    xboxybox = 0.;
    noplotter();
    return xboxybox;
}

Vector<Float> PGPlotterNull::qwin()
{
    Vector<Float> retval(4);
    retval = 0;
    noplotter();
    return retval;
}

void PGPlotterNull::rect(Float, Float, Float, Float)
{
    noplotter();
}

void PGPlotterNull::sah(Int, Float, Float)
{
    noplotter();
}

void PGPlotterNull::save()
{
    noplotter();
}

void PGPlotterNull::sch(Float)
{
    noplotter();
}

void PGPlotterNull::sci(Int)
{
    noplotter();
}

void PGPlotterNull::scr(Int, Float, Float, Float)
{
    noplotter();
}

void PGPlotterNull::sfs(Int)
{
    noplotter();
}

void PGPlotterNull::sls(Int)
{
    noplotter();
}

void PGPlotterNull::slw(Int)
{
    noplotter();
}

void PGPlotterNull::stbg(Int)
{
    noplotter();
}

void PGPlotterNull::subp(Int, Int)
{
    noplotter();
}

void PGPlotterNull::svp(Float, Float, Float, Float)
{
    noplotter();
}

void PGPlotterNull::swin(Float, Float, Float, Float)
{
    noplotter();
}

void PGPlotterNull::tbox(const String &, Float, Int,
                         const String &, Float, Int)
{
    noplotter();
}

void PGPlotterNull::text(Float, Float, const String &)
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

void PGPlotterNull::wnad(Float, Float, Float, Float)
{
    noplotter();
}

void PGPlotterNull::conl(const Matrix<Float> &, Float,
                         const Vector<Float> &, const String &,
                         Int, Int)
{
    noplotter();
}

void PGPlotterNull::cont(const Matrix<Float> &, const Vector<Float> &,
                         Bool, const Vector<Float> &)
{
    noplotter();
}

void PGPlotterNull::ctab(const Vector<Float> &, const Vector<Float> &,
                         const Vector<Float> &, const Vector<Float> &,
                         Float, Float)
{
    noplotter();
}

void PGPlotterNull::gray(const Matrix<Float> &, Float, Float,
                         const Vector<Float> &)
{
    noplotter();
} 

void PGPlotterNull::iden()
{
    noplotter();
}

void PGPlotterNull::imag(const Matrix<Float> &, Float, Float,
                         const Vector<Float> &)
{
    noplotter();
}

Vector<Int> PGPlotterNull::qcir()
{
    Vector<Int> retval(2);
    retval = 0;
    noplotter();
    return retval;
}

Vector<Int> PGPlotterNull::qcol()
{
    Vector<Int> retval(2);
    retval = 0;
    noplotter();
    return retval;
}

void PGPlotterNull::scir(Int, Int)
{
    noplotter();
}

void PGPlotterNull::sitf(Int)
{
    noplotter();
}

void PGPlotterNull::bin(const Vector<Float> &, const Vector<Float> &,
                        Bool)
{
    noplotter();
}

void PGPlotterNull::conb(const Matrix<Float> &, const Vector<Float> &,
                         const Vector<Float> &, Float)
{
    noplotter();
}

void PGPlotterNull::cons(const Matrix<Float> &, const Vector<Float> &,
                         const Vector<Float> &)
{
    noplotter();
}

void PGPlotterNull::errx(const Vector<Float> &, const Vector<Float> &,
                         const Vector<Float> &, Float)
{
    noplotter();
}

void PGPlotterNull::hi2d(const Matrix<Float> &, const Vector<Float> &,
                         Int, Float, Bool, 
                         const Vector<Float> &)
{
    noplotter();
}

void PGPlotterNull::ldev()
{
    noplotter();
}

Vector<Float> PGPlotterNull::len(Int, const String &)
{
    Vector<Float> retval(2);
    retval =0;
    noplotter();
    return retval;
}

String PGPlotterNull::numb(Int, Int, Int)
{
    noplotter();
    return String();
}

void PGPlotterNull::panl(Int, Int)
{
    noplotter();
}

void PGPlotterNull::pap(Float, Float)
{
    noplotter();
}

void PGPlotterNull::pixl(const Matrix<Int> &, Float, Float,
                         Float, Float)
{
    noplotter();
}

void PGPlotterNull::pnts(const Vector<Float> &, const Vector<Float> &,
                         const Vector<Int>)
{
    noplotter();
}

Vector<Float>  PGPlotterNull::qah()
{
    Vector<Float> retval(3);
    retval = 0;
    noplotter();
    return retval;
}

Int PGPlotterNull::qcf()
{
    noplotter();
    return 0;
}

Float PGPlotterNull::qch()
{
    noplotter();
    return 0;
}

Vector<Float> PGPlotterNull::qcr(Int)
{
    Vector<Float> retval(3);
    retval = 0;
    noplotter();
    return retval;
}

Vector<Float> PGPlotterNull::qcs(Int)
{
    Vector<Float> retval(2);
    retval = 0;
    noplotter();
    return retval;
}

Int PGPlotterNull::qfs()
{
    noplotter();
    return 0;
}

Vector<Float> PGPlotterNull::qhs()
{
    Vector<Float> retval(3);
    retval = 0;
    noplotter();
    return retval;
}

Int PGPlotterNull::qid()
{
    noplotter();
    return 0;
}

String PGPlotterNull::qinf(const String &)
{
    noplotter();
    return String();
}

Int PGPlotterNull::qitf()
{
    noplotter();
    return 0;
}

Int PGPlotterNull::qls()
{
    noplotter();
    return 0;
}

Int PGPlotterNull::qlw()
{
    noplotter();
    return 0;
}

Vector<Float> PGPlotterNull::qpos()
{
    Vector<Float> retval(2);
    retval = 0;
    noplotter();
    return retval;
}

Vector<Float> PGPlotterNull::qvp(Int)
{
    Vector<Float> retval(4);
    retval = 0;
    noplotter();
    return retval;
}

Vector<Float> PGPlotterNull::qvsz(Int)
{
    Vector<Float> retval(4);
    retval = 0;
    noplotter();
    return retval;
}

Float PGPlotterNull::rnd(Float x, Int)
{
    noplotter();
    return x;
}

Vector<Float> PGPlotterNull::rnge(Float, Float)
{
    Vector<Float> retval(2);
    retval = 0;
    noplotter();
    return retval;
}

void PGPlotterNull::scf(Int)
{
  noplotter();
}

void PGPlotterNull::scrn(Int, const String &)
{
  noplotter();
}

void PGPlotterNull::shls(Int, Float, Float, Float)
{
  noplotter();
}

void PGPlotterNull::shs(Float, Float, Float)
{
  noplotter();
}

void PGPlotterNull::vect(const Matrix<Float> &, const Matrix<Float> &,
                         Float, Int, 
                         const Vector<Float> &, Float)
{
  noplotter();
}

  void PGPlotterNull::vsiz(Float, Float, Float,
                           Float)
{
  noplotter();
}

void PGPlotterNull::wedg(const String &, Float, Float,
                         Float, Float, const String &)
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

} //# NAMESPACE CASACORE - END

