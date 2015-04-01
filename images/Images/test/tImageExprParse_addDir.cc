//# tImageExprParse_addDir.cc: test the PagedImage::addDir function
//# Copyright (C) 2008
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or(at your option)
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

#include <casacore/images/Images/ImageExprParse.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/namespace.h>

int main()
{
  try {
    AlwaysAssertExit (ImageExprParse::setAddDir("/c/d", "") == "");
    AlwaysAssertExit (ImageExprParse::setAddDir("/c/d", "a/b") == "/c/d/a/b");
    AlwaysAssertExit (ImageExprParse::setAddDir("/c/d", "/a/b") == "/a/b");
    AlwaysAssertExit (ImageExprParse::setAddDir("/c/d", "$HOME") == "$HOME");
    AlwaysAssertExit (ImageExprParse::setAddDir("", "") == "");
    AlwaysAssertExit (ImageExprParse::setAddDir("", "a/b") == "a/b");
    AlwaysAssertExit (ImageExprParse::setAddDir("", "/a/b") == "/a/b");
    AlwaysAssertExit (ImageExprParse::setAddDir("", "$HOME") == "$HOME");
    cout<< "ok"<< endl;
  } catch (AipsError x) {
    cerr << "Exception caught: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;
}
