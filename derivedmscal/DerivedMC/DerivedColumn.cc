//# DerivedColumn.cc: The derived MS columns.
//# Copyright (C) 2010
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

//# Includes
#include <casacore/derivedmscal/DerivedMC/DerivedColumn.h>

namespace casacore {

  HourangleColumn::~HourangleColumn()
  {}
  void HourangleColumn::get (uInt rowNr, Double& data)
  {
    data = itsEngine->getHA (itsAntNr, rowNr);
  }

  ParAngleColumn::~ParAngleColumn()
  {}
  void ParAngleColumn::get (uInt rowNr, Double& data)
  {
    data = itsEngine->getPA (itsAntNr, rowNr);
  }

  LASTColumn::~LASTColumn()
  {}
  void LASTColumn::get (uInt rowNr, Double& data)
  {
    data = itsEngine->getLAST (itsAntNr, rowNr);
  }

  HaDecColumn::~HaDecColumn()
  {}
  IPosition HaDecColumn::shape (uInt)
  {
    return IPosition(1,2);
  }
  void HaDecColumn::getArray (uInt rowNr, Array<Double>& data)
  {
    itsEngine->getHaDec (itsAntNr, rowNr, data);
  }

  AzElColumn::~AzElColumn()
  {}
  IPosition AzElColumn::shape (uInt)
  {
    return IPosition(1,2);
  }
  void AzElColumn::getArray (uInt rowNr, Array<Double>& data)
  {
    itsEngine->getAzEl (itsAntNr, rowNr, data);
  }

  UVWJ2000Column::~UVWJ2000Column()
  {}
  IPosition UVWJ2000Column::shape (uInt)
  {
    return IPosition(1,3);
  }
  void UVWJ2000Column::getArray (uInt rowNr, Array<Double>& data)
  {
    itsEngine->getUVWJ2000 (rowNr, data);
  }

} //# end namespace
