//# Stokes.cc: Stokes parameter definitions for interface to table data
//# Copyright (C) 1994,1995,1997
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

#include <aips/aips.h>
#include <aips/Measures/Stokes.h>
#include <aips/Utilities/String.h>

Stokes::StokesTypes Stokes::type(Int stokesNumber) 
{
   StokesTypes val = Undefined;
   if (stokesNumber > Undefined && stokesNumber < NumberOfTypes) {
      val = StokesTypes(stokesNumber);
   }
   return val;
}

Stokes::StokesTypes Stokes::type(const String &stokesName) 
{
   StokesTypes val = Undefined;
   if      (stokesName == "I")  val = I;
   else if (stokesName == "Q")  val = Q;
   else if (stokesName == "U")  val = U;
   else if (stokesName == "V")  val = V;
   else if (stokesName == "RR") val = RR;
   else if (stokesName == "RL") val = RL;
   else if (stokesName == "LR") val = LR;
   else if (stokesName == "LL") val = LL;
   else if (stokesName == "XX") val = XX;
   else if (stokesName == "XY") val = XY;
   else if (stokesName == "YX") val = YX;
   else if (stokesName == "YY") val = YY;
   else if (stokesName == "RX") val = RX;
   else if (stokesName == "RY") val = RY;
   else if (stokesName == "LX") val = LX;
   else if (stokesName == "LY") val = LY;
   else if (stokesName == "XR") val = XR;
   else if (stokesName == "XL") val = XL;
   else if (stokesName == "YR") val = YR;
   else if (stokesName == "YL") val = YL;
   else if (stokesName == "PP") val = PP;
   else if (stokesName == "PQ") val = PQ;
   else if (stokesName == "QP") val = QP;
   else if (stokesName == "QQ") val = QQ;
   else if (stokesName == "RCircular") val = RCircular;
   else if (stokesName == "LCircular") val = LCircular;
   else if (stokesName == "Linear") val = Linear;
   else if (stokesName == "Ptotal") val = Ptotal;
   else if (stokesName == "Plinear") val = Plinear;
   else if (stokesName == "PFtotal") val = PFtotal;
   else if (stokesName == "PFlinear") val = PFlinear;
   else if (stokesName == "Pangle") val = Pangle;
   return val;
}

String Stokes::name(StokesTypes stokesType) 
{
   String stokesName;
   switch (stokesType) {
   case Undefined: stokesName="??"; break;
   case I: stokesName="I"; break;
   case Q: stokesName="Q"; break;
   case U: stokesName="U"; break;
   case V: stokesName="V"; break;
   case RR: stokesName="RR"; break;
   case RL: stokesName="RL"; break;
   case LR: stokesName="LR"; break;
   case LL: stokesName="LL"; break;
   case XX: stokesName="XX"; break;
   case XY: stokesName="XY"; break;
   case YX: stokesName="YX"; break;
   case YY: stokesName="YY"; break;
   case RX: stokesName="RX"; break;
   case RY: stokesName="RY"; break;
   case LX: stokesName="LX"; break;
   case LY: stokesName="LY"; break;
   case XR: stokesName="XR"; break;
   case XL: stokesName="XL"; break;
   case YR: stokesName="YR"; break;
   case YL: stokesName="YL"; break;
   case PP: stokesName="PP"; break;
   case PQ: stokesName="PQ"; break;
   case QP: stokesName="QP"; break;
   case QQ: stokesName="QQ"; break;
   case RCircular: stokesName="RCircular"; break;
   case LCircular: stokesName="LCircular"; break;
   case Linear: stokesName="Linear"; break;
   case Ptotal: stokesName="Ptotal"; break;
   case Plinear: stokesName="Plinear"; break;
   case PFtotal: stokesName="PFtotal"; break;
   case PFlinear: stokesName="PFlinear"; break;
   case Pangle: stokesName="Pangle"; break;
   }
   return stokesName;
}

Fallible<Int> Stokes::receptor1(StokesTypes stokesType)
{
    Int rec1 = (stokesType-1)%4; 
    if (rec1<2) rec1=0; else rec1=1;
    if (stokesType>Stokes::V && stokesType<Stokes::RCircular) 
	return Fallible<Int>(rec1);
    else return Fallible<Int>();
}

Fallible<Int> Stokes::receptor2(StokesTypes stokesType)
{
    Int rec2 = (stokesType-1)%4; 
    if (rec2==0 || rec2==2) rec2=0; else rec2=1;
    if (stokesType>Stokes::V && stokesType<Stokes::RCircular) 
	return Fallible<Int>(rec2);
    else return Fallible<Int>();
}

Int Stokes::FITSValue(StokesTypes which)
{
   Int retval;

   switch (which) {
   case I: retval = 1; break;
   case Q: retval = 2; break;
   case U: retval = 3; break;
   case V: retval = 4; break;
   case RR: retval = -1; break;
   case LL: retval = -2; break;
   case RL: retval = -3; break;
   case LR: retval = -4; break;
   case XX: retval = -5; break;
   case YY: retval = -6; break;
   case XY: retval = -7; break;
   case YX: retval = -8; break;
   default: retval = 100 + Int(which);
   }
   return retval;
}

Stokes::StokesTypes Stokes::fromFITSValue(Int which)
{
   StokesTypes retval;

   switch (which) {
   case 1: retval = I; break;
   case 2: retval = Q; break;
   case 3: retval = U; break;
   case 4: retval = V; break;
   case -1: retval = RR; break;
   case -2: retval = LL; break;
   case -3: retval = RL; break;
   case -4: retval = LR; break;
   case -5: retval = XX; break;
   case -6: retval = YY; break;
   case -7: retval = XY; break;
   case -8: retval = YX; break;
   default: retval = Stokes::type(which-100);
   }
   return retval;
}
