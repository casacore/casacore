//# <ClassFileName.h>: this defines <ClassName>, which ...
//# Copyright (C) 1997,1998
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

#include <trial/Coordinates/Projection.h>
#include <aips/Exceptions/Error.h>

Projection::Projection(Projection::Type which) 
: which_p(which), parameters_p(0)
{
    validate();
}

Projection::Projection(Projection::Type which, const Vector<Double> &parameters)
: which_p(which), parameters_p(parameters.copy())
{
    validate();
}

Projection::Projection(const Projection &other)
    : which_p(other.which_p), parameters_p(other.parameters_p.copy())
{
    validate();
}

Projection &Projection::operator=(const Projection &other)
{
    if (this != &other) {
	which_p = other.which_p;
	parameters_p.resize(other.parameters_p.nelements());
	parameters_p = other.parameters_p;
	validate();
    }

    return *this;
}


Projection::~Projection()
{
    // Nothing
}

String Projection::name() const
{
    return name(which_p);
}

String Projection::name(Projection::Type proj)
{
    switch (proj) {
    case AZP: return "AZP";
    case TAN: return "TAN";
    case SIN: return "SIN";
    case STG: return "STG";
    case ARC: return "ARC";
    case ZPN: return "ZPN";
    case ZEA: return "ZEA";
    case AIR: return "AIR";
    case CYP: return "CYP";
    case CAR: return "CAR";
    case MER: return "MER";
    case CEA: return "CEA";
    case COP: return "COP";
    case COD: return "COD";
    case COE: return "COE";
    case COO: return "COO";
    case BON: return "BON";
    case PCO: return "PCO";
    case GLS: return "GLS";
    case PAR: return "PAR";
    case AIT: return "AIT";
    case MOL: return "MOL";
    case CSC: return "CSC";
    case QSC: return "QSC";
    case TSC: return "TSC";
    default:
	throw(AipsError("Projection::name(Type) - unknown projection"));
    };
    return "An impossible error has occurred! NOTREACHED";
}

Projection::Type Projection::type(const String &name)
{
    Projection::Type retval = N_PROJ;
    if (name == "AZP") {
	retval = AZP;
    } else if (name == "TAN") {
	retval = TAN;
    } else if (name == "SIN") {
	retval = SIN;
    } else if (name == "STG") {
	retval = STG;
    } else if (name == "ARC") {
	retval = ARC;
    } else if (name == "ZPN") {
	retval = ZPN;
    } else if (name == "ZEA") {
	retval = ZEA;
    } else if (name == "AIR") {
	retval = AIR;
    } else if (name == "CYP") {
	retval = CYP;
    } else if (name == "CAR") {
	retval = CAR;
    } else if (name == "MER") {
	retval = MER;
    } else if (name == "CEA") {
	retval = CEA;
    } else if (name == "COP") {
	retval = COP;
    } else if (name == "COD") {
	retval = COD;
    } else if (name == "COE") {
	retval = COE;
    } else if (name == "COO") {
	retval = COO;
    } else if (name == "BON") {
	retval = BON;
    } else if (name == "PCO") {
	retval = PCO;
    } else if (name == "GLS") {
	retval = GLS;
    } else if (name == "PAR") {
	retval = PAR;
    } else if (name == "AIT") {
	retval = AIT;
    } else if (name == "MOL") {
	retval = MOL;
    } else if (name == "CSC") {
	retval = CSC;
    } else if (name == "QSC") {
	retval = QSC;
    } else if (name == "TSC") {
	retval = TSC;
    }

    return retval;
}

uInt Projection::nParameters(Projection::Type proj)
{
    switch (proj) {
    case AZP: return 1;
    case TAN: return 0;
    case SIN: return 2;
    case STG: return 0;
    case ARC: return 0;
    case ZPN: return 10;
    case ZEA: return 0;
    case AIR: return 1;
    case CYP: return 2;
    case CAR: return 0;
    case MER: return 0;
    case CEA: return 1;
    case COP: return 2;
    case COD: return 2;
    case COE: return 2;
    case COO: return 2;
    case BON: return 1;
    case PCO: return 0;
    case GLS: return 0;
    case PAR: return 0;
    case AIT: return 0;
    case MOL: return 0;
    case CSC: return 0;
    case QSC: return 0;
    case TSC: return 0;
    default:
	throw(AipsError("Projection::nParameters() - unknown projection"));
    }
    return 0; // NOTREACHED
}

Bool Projection::near(const Projection &other, Double tol) const
{
   if (which_p != other.which_p) return False;
   if (parameters_p.nelements() != other.parameters_p.nelements()) return False;
   
   for (uInt i=0; i<parameters_p.nelements(); i++) {
     if (!::near(parameters_p(i),other.parameters_p(i),tol)) return False;
   }
   
   return True;
}


void Projection::validate()
{
    // SIN is a special case - it can have zero or two parameters. Always
    // canonicalize it to the latter.
    if (which_p == SIN && parameters_p.nelements() == 0) {
	parameters_p.resize(2);
	parameters_p = 0.0;
    }
    uInt requiredSize = nParameters(which_p);
    if (requiredSize != parameters_p.nelements()) {
	throw(AipsError("Projection::validate() - the projection has the wrong"
			" number of parameters"));
    }
}
