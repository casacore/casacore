//# Register.cc: Register Measure UDFs
//# Copyright (C) 2011
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
//# $Id$

#include <casacore/meas/MeasUDF/Register.h>
#include <casacore/meas/MeasUDF/PositionUDF.h>
#include <casacore/meas/MeasUDF/EpochUDF.h>
#include <casacore/meas/MeasUDF/DirectionUDF.h>

using namespace casacore;

void register_meas()
{
  // Register the TaQL Meas UDFs.
  UDFBase::registerUDF ("meas.POSITION",      PositionUDF::makePOS);
  UDFBase::registerUDF ("meas.POS",           PositionUDF::makePOS);
  UDFBase::registerUDF ("meas.ITRF",          PositionUDF::makeITRFXYZ);
  UDFBase::registerUDF ("meas.ITRFXYZ",       PositionUDF::makeITRFXYZ);
  UDFBase::registerUDF ("meas.ITRFLL",        PositionUDF::makeITRFLL);
  UDFBase::registerUDF ("meas.ITRFLONLAT",    PositionUDF::makeITRFLL);
  UDFBase::registerUDF ("meas.ITRFH",         PositionUDF::makeITRFH);
  UDFBase::registerUDF ("meas.ITRFHEIGHT",    PositionUDF::makeITRFH);
  UDFBase::registerUDF ("meas.WGS",           PositionUDF::makeWGSXYZ);
  UDFBase::registerUDF ("meas.WGSXYZ",        PositionUDF::makeWGSXYZ);
  UDFBase::registerUDF ("meas.WGSLL",         PositionUDF::makeWGSLL);
  UDFBase::registerUDF ("meas.WGSLONLAT",     PositionUDF::makeWGSLL);
  UDFBase::registerUDF ("meas.WGSH",          PositionUDF::makeWGSH);
  UDFBase::registerUDF ("meas.WGSHEIGHT",     PositionUDF::makeWGSH);
  UDFBase::registerUDF ("meas.EPOCH",         EpochUDF::makeEPOCH);
  UDFBase::registerUDF ("meas.LAST",          EpochUDF::makeLAST);
  UDFBase::registerUDF ("meas.LST",           EpochUDF::makeLAST);
  UDFBase::registerUDF ("meas.DIRECTION",     DirectionUDF::makeDIR);
  UDFBase::registerUDF ("meas.DIR",           DirectionUDF::makeDIR);
  UDFBase::registerUDF ("meas.HADEC",         DirectionUDF::makeHADEC);
  UDFBase::registerUDF ("meas.AZEL",          DirectionUDF::makeAZEL);
  UDFBase::registerUDF ("meas.APPARENT",      DirectionUDF::makeAPP);
  UDFBase::registerUDF ("meas.APP",           DirectionUDF::makeAPP);
  UDFBase::registerUDF ("meas.J2000",         DirectionUDF::makeJ2000);
  UDFBase::registerUDF ("meas.B1950",         DirectionUDF::makeB1950);
  UDFBase::registerUDF ("meas.ECLIPTIC",      DirectionUDF::makeECL);
  UDFBase::registerUDF ("meas.ECL",           DirectionUDF::makeECL);
  UDFBase::registerUDF ("meas.GALACTIC",      DirectionUDF::makeGAL);
  UDFBase::registerUDF ("meas.GAL",           DirectionUDF::makeGAL);
  UDFBase::registerUDF ("meas.SUPERGALACTIC", DirectionUDF::makeSGAL);
  UDFBase::registerUDF ("meas.SUPERGAL",      DirectionUDF::makeSGAL);
  UDFBase::registerUDF ("meas.SGAL",          DirectionUDF::makeSGAL);
  UDFBase::registerUDF ("meas.RISESET",       DirectionUDF::makeRISESET);
}
