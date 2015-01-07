//# DerivedColumn.h: A derived MS column.
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

#ifndef DERIVEDMSCAL_DERIVEDCOLUMN_H
#define DERIVEDMSCAL_DERIVEDCOLUMN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/derivedmscal/DerivedMC/MSCalEngine.h>
#include <casacore/tables/DataMan/VirtScaCol.h>
#include <casacore/tables/DataMan/VirtArrCol.h>

namespace casacore {


  // <summary>Hourangle derived from TIME, etc.</summary>
  // <use visibility=local>
  class HourangleColumn : public VirtualScalarColumn<Double>
  {
  public:
    explicit HourangleColumn (MSCalEngine* engine, Int antnr)
      : itsEngine (engine),
        itsAntNr  (antnr)
    {}
    virtual ~HourangleColumn();
    virtual void get (uInt rowNr, Double& data);
  private:
    MSCalEngine* itsEngine;
    Int          itsAntNr;    //# -1=array 0=antenna1 1=antenna2
  };


  // <summary>Local sidereal time derived from TIME, etc.</summary>
  // <use visibility=local>
  class LASTColumn : public VirtualScalarColumn<Double>
  {
  public:
    explicit LASTColumn (MSCalEngine* engine, Int antnr)
      : itsEngine (engine),
        itsAntNr  (antnr)
    {}
    virtual ~LASTColumn();
    virtual void get (uInt rowNr, Double& data);
  private:
    MSCalEngine* itsEngine;
    Int          itsAntNr;    //# -1=array 0=antenna1 1=antenna2
  };


  // <summary>Parallactic angle derived from TIME, etc.</summary>
  // <use visibility=local>
  class ParAngleColumn : public VirtualScalarColumn<Double>
  {
  public:
    explicit ParAngleColumn (MSCalEngine* engine, Int antnr)
      : itsEngine (engine),
        itsAntNr  (antnr)
    {}
    virtual ~ParAngleColumn();
    virtual void get (uInt rowNr, Double& data);
  private:
    MSCalEngine* itsEngine;
    Int          itsAntNr;    //# 0=antenna1 1=antenna2
  };


  // <summary>Hourangle/declination derived from TIME, etc.</summary>
  // <use visibility=local>
  class HaDecColumn : public VirtualArrayColumn<Double>
  {
  public:
    explicit HaDecColumn (MSCalEngine* engine, Int antnr)
      : itsEngine (engine),
        itsAntNr  (antnr)
    {}
    virtual ~HaDecColumn();
    virtual IPosition shape (uInt rownr);
    virtual void getArray (uInt rowNr, Array<Double>& data);
  private:
    MSCalEngine* itsEngine;
    Int          itsAntNr;    //# 0=antenna1 1=antenna2
  };


  // <summary>Azimuth/elevation derived from TIME, etc.</summary>
  // <use visibility=local>
  class AzElColumn : public VirtualArrayColumn<Double>
  {
  public:
    explicit AzElColumn (MSCalEngine* engine, Int antnr)
      : itsEngine (engine),
        itsAntNr  (antnr)
    {}
    virtual ~AzElColumn();
    virtual IPosition shape (uInt rownr);
    virtual void getArray (uInt rowNr, Array<Double>& data);
  private:
    MSCalEngine* itsEngine;
    Int          itsAntNr;    //# 0=antenna1 1=antenna2
  };


  // <summary>UVW J2000 derived from TIME, etc.</summary>
  // <use visibility=local>
  class UVWJ2000Column : public VirtualArrayColumn<Double>
  {
  public:
    explicit UVWJ2000Column (MSCalEngine* engine)
      : itsEngine (engine)
    {}
    virtual ~UVWJ2000Column();
    virtual IPosition shape (uInt rownr);
    virtual void getArray (uInt rowNr, Array<Double>& data);
  private:
    MSCalEngine* itsEngine;
  };


} //# end namespace

#endif
