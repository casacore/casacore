//# QuantaProxy.h: This class gives a high-level intrface to Quantity
//# Copyright (C) 2006
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#ifndef QUANTA_QUANTAPROXY_H
#define QUANTA_QUANTAPROXY_H

//# Includes

#include <casa/aips.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Quanta/QuantumHolder.h>
#include <casa/Quanta/UnitName.h>
#include <casa/Containers/Record.h>
#include <casa/stdmap.h>

#include <casa/namespace.h>

class QuantaProxy
{
public:
  QuantaProxy() {;}
  virtual ~QuantaProxy() {;}

  Record mapit(const String& tp="all");

  Record constants(const String &in="pi");
  void fits();
  void define(const String& ustr, const Record& rec);
  Record unit(const Vector<Double>& val, const String& ustr);
  Record qfunc1(const Record& rec, Int form);
  Record qfunc2(const Record& lrec, const Record& rrec, Int form);
  Record norm(const Record& rec, Double ang);
  Bool compare(const Record& lrec, const Record& rrec);
  Bool check(const String& str);
  Record pow(const Record& rec, Int powerof);
  Record toTime(const Record& rec);
  Record toAngle(const Record& rec);
  Record dopcv(const Record& lrec, const Record& rrec);
  Record frqcv(const Record& lrec, const Record& rrec);
  Vector<String> time(const Record& rec, const Vector<String>& fmt,
                      Int prec, Bool form2);
  Vector<String> angle(const Record& rec, const Vector<String>& fmt,
                      Int prec, Bool form2);
  Vector<String> tfreq(const Record& rec, const String& fmt,
                       Bool form2);
  Record splitDate(const Record& rec);
  Record qlogical(const Record& lrec, const Record& rrec, Int form);
  Record quant(const Vector<String>& vstr);

private:
  QuantumHolder rec2qh(const Record& rec);
  Record quant2rec(const Quantity& q);
  Record quant2rec(const Quantum<Vector<Double> >& q);

  void mapInsert(Record& out, const String& type,
                 const map<String, UnitName> &mp);

  Int makeForm(const Vector<String>& in, const String& mode);
};
#endif
