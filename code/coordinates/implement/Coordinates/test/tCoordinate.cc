//# tCoordinate.cc: Test program for Coordinate
//# Copyright (C) 1998,1999,2000
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
//#

 
#include <trial/Coordinates/Coordinate.h>
#include <aips/Exceptions/Error.h>

#include <iostream.h>



int main()
{
  try {
     String type = Coordinate::typeToString(Coordinate::DIRECTION);
     if (type!=String("Direction")) {
        throw(AipsError("Failed typeToString for DIRECTION"));
     }
//
     type = Coordinate::typeToString(Coordinate::SPECTRAL);
     if (type!=String("Spectral")) {
        throw(AipsError("Failed typeToString for SPECTRAL"));
     }
//
     type = Coordinate::typeToString(Coordinate::TABULAR);
     if (type!=String("Tabular")) {
        throw(AipsError("Failed typeToString for TABULAR"));
     }
//
     type = Coordinate::typeToString(Coordinate::STOKES);
     if (type!=String("Stokes")) {
        throw(AipsError("Failed typeToString for STOKES"));
     }
//
     type = Coordinate::typeToString(Coordinate::LINEAR);
     if (type!=String("Linear")) {
        throw(AipsError("Failed typeToString for LINEAR"));
     }
//
     type = Coordinate::typeToString(Coordinate::COORDSYS);
     if (type!=String("System")) {
        throw(AipsError("Failed typeToString for COORDSYS"));
     }

  
  } catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      return (1);
  }

   cout << "ok" << endl; 
   return (0);
}

