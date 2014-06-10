//# tfitsreader.cc: FITS test program to read and list a FITS file
//# Copyright (C) 1993,1994,1996,1998,1999,2001
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


# include <fits/FITS/FITSReader.h>

# include <casa/namespace.h>

int main(int argc, const char* argv[])
{
   cout << "Test of reading fits files" << endl;
   if (argc != 2) {
       cout << "ex1 <filename>" << "\n";
       exit(0);
   }
   cout << "argv[1]=" << argv[1] << endl;
   FITSReader fr;
   fr.listFits(argv[1]);
   cout << "tfitsreader Before retrun." << endl;
   return 0;
}
