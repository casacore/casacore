//# tPath.cc: Test program for class Path
//# Copyright (C) 1993,1994,1995,1996,1998,1999,2000,2001,2002
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


#include <casacore/casa/OS/Path.h>
#include <casacore/casa/OS/EnvVar.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions.h>
#include <casacore/casa/iostream.h>
#include <unistd.h>


#include <casacore/casa/namespace.h>
// <summary>
// Test program for class Path.
// </summary>

// This program tests the class Path.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.
// <p>
// When an argument is given, no exceptions will be thrown.
// This can be used to check if no memory leaks occur in normal operation.


void check (const String& path, const String& expanded,
	    const String& absolute, Bool& success)
{
    cout << ">>> testing path = " << path << endl;
    cout << "<<<" << endl;
    Path test (path);
    const String& ov = test.originalName();
    if (ov != path) {
	cout << "original: " << ov << endl;
	cout << "expected: " << path << endl;
	success = False;
    }
    const String& ev = test.expandedName();
    if (ev != expanded) {
	cout << "expanded: " << ev << endl;
	cout << "expected: " << expanded << endl;
	success = False;
    }
    const String& av = test.absoluteName();
    if (av != absolute) {
	cout << "absolute: " << av << endl;
	cout << "expected: " << absolute << endl;
	success = False;
    }
}

void checkDirBase (const String& path, const String& dir,
		   const String& base, Bool& success)
{
    cout << "testing dirbase path = " << path << endl;
    Path test (path);
    String dv = test.dirName();
    if (dv != dir) {
	cout << "dir:      " << dv << endl;
	cout << "expected: " << dir << endl;
	success = False;
    }
    String bv = test.baseName();
    if (bv != base) {
	cout << "base:     " << bv << endl;
	cout << "expected: " << base << endl;
	success = False;
    }
}

void doIt (Bool doExcp, Bool& success)
{
    // Get the home directory.
    String home (EnvironmentVariable::get ("HOME"));
    AlwaysAssertExit (! home.empty());
    // Get the current working directory (set in tPath.exec).
    String curr (EnvironmentVariable::get ("tPath_Env_Curr"));
    AlwaysAssertExit (! curr.empty());
    // Get the user name.
    String user (EnvironmentVariable::get ("tPath_Env_Username"));
    AlwaysAssertExit (! user.empty());

    // Define all kind of path names (relative, absolute, with and
    // without environment variables and tilde).
    // Test the resulting expanded and absolute name.
    check ("~", home, home, success);
    check ("$HOME", home, home, success);
    check ("/$HOME/", "/"+home+"/", home, success);
    check ("~/testx", home+"/testx", home+"/testx", success);
    check ("$HOME/testx", home+"/testx", home+"/testx", success);
    check ("~" + user, home, home, success);
    check ("~" + user + "/test/test2", home + "/test/test2",
	   home + "/test/test2", success);
    check ("$tPath_Env_User/test", home + "/test", home + "/test", success);
    check ("${tPath_Env_User}/test", home + "/test", home + "/test", success);
    check ("/aa$HOME/test", "/aa" + home + "/test",
           "/aa" + home + "/test", success);
    check ("/aa${HOME}bb/test", "/aa" + home + "bb/test",
           "/aa" + home + "bb/test", success);
    check ("/aa${HOME}bb${HOME}cc/test", "/aa"+ home+"bb"+home+"cc/test",
           "/aa"+home+"bb"+home+"cc/test", success);
    check ("/testx", "/testx", "/testx", success);
    check ("testx", "testx", curr + "/testx", success);
    check (user, user, curr + "/" + user, success);
    check ("$tPath_Env_Test1/$HOME", home + "/" +  home,
	   home + home, success);

    // Check resolvedName.
    String tpDir = Path("$tPath_Env_Curr/tPath_tmpdir").absoluteName();
    AlwaysAssertExit (Path("$tPath_Env_Curr/tPath_tmpdir//d1/../d1/.//d2/").
                      resolvedName() == tpDir + "/d1/d2");
    if (doExcp) {
      Bool ok = True;
	try {
            Path("/a/b").resolvedName();
	} catch (AipsError x) {
            cout << ">>> " << x.getMesg() << endl << "<<<" << endl;
            ok = False;
	}
        AlwaysAssertExit (!ok);
    }

    // Check copy ctor and assignment (also self-assignment).
    String str;
    Path test1 ("~");
    Path test2 (test1);
    AlwaysAssertExit (test1.originalName() == test2.originalName());
    AlwaysAssertExit (test1.expandedName() == test2.expandedName());
    AlwaysAssertExit (test1.absoluteName() == test2.absoluteName());
    Path test3 ("~/test");
    str = test3.expandedName();
    str = test3.absoluteName();
    test3 = test2;
    AlwaysAssertExit (test3.originalName() == test2.originalName());
    AlwaysAssertExit (test3.expandedName() == test2.expandedName());
    AlwaysAssertExit (test3.absoluteName() == test2.absoluteName());
    test3 = test3;
    AlwaysAssertExit (test3.originalName() == test2.originalName());
    AlwaysAssertExit (test3.expandedName() == test2.expandedName());
    AlwaysAssertExit (test3.absoluteName() == test2.absoluteName());
    
    // Testing exception because of recursive environment variables.
    EnvironmentVariable::set ("TEST1", "$TEST2");
    EnvironmentVariable::set ("TEST2", "$TEST1");
    test1 = Path ("$TEST1");
    if (doExcp) {
	try {
	    test1.expandedName ();
	} catch (AipsError x) {
	    cout << x.getMesg () << endl;
	} 
    }
	
    // Tests for isValid and isStrictlyPosix
    test1 = Path ("HOME//test");
    test2 = Path ("");
    test3 = Path ("\033");
    AlwaysAssertExit (! test1.isValid());
    AlwaysAssertExit (test2.isValid());
    AlwaysAssertExit (! test3.isValid());
    AlwaysAssertExit (! test1.isStrictlyPosix());
    AlwaysAssertExit (test2.isStrictlyPosix());
    AlwaysAssertExit (! test3.isStrictlyPosix());
    test1 = Path ("$HOME/test");
    AlwaysAssertExit (test1.isValid());
    AlwaysAssertExit (test1.isStrictlyPosix());

    cout << test1.length() << endl;
    cout << ">>> ";
    cout << test1.maxLength() << endl;
    cout << "<<<" << endl;

    // Tests for dir- and baseName.
    checkDirBase (".", ".", ".", success);
    checkDirBase ("..", ".", "..", success);
    checkDirBase ("a", ".", "a", success);
    checkDirBase ("a/", ".", "a", success);
    checkDirBase ("/b", "/", "b", success);
    checkDirBase ("/b/", "/", "b", success);
    checkDirBase ("/", "/", "", success);
    checkDirBase ("a/b", "a", "b", success);
    checkDirBase ("a/b/", "a", "b", success);
    checkDirBase ("~/test/test1", home + "/test", "test1", success);
    checkDirBase ("/home/", "/", "home", success);
    checkDirBase ("test/test1/..", "test/test1", "..", success);
    checkDirBase ("harry", ".", "harry", success);
    checkDirBase ("/home", "/", "home", success);
    checkDirBase ("$HOME/test/ha./test", home + "/test/ha.", "test", success);

    Path tpath;
    AlwaysAssertExit (tpath.originalName() == ".");
    tpath = Path("");
    AlwaysAssertExit (tpath.originalName() == ".");
    tpath = Path("/");
    AlwaysAssertExit (tpath.originalName() == "/");
    tpath.append ("abc/");
    AlwaysAssertExit (tpath.originalName() == "/abc/");
    tpath.append ("/abc");
    AlwaysAssertExit (tpath.originalName() == "/abc//abc");
    AlwaysAssertExit (tpath.absoluteName() == "/abc/abc");
    tpath.append ("abc");
    AlwaysAssertExit (tpath.originalName() == "/abc//abc/abc");
    tpath.append ("/abc");
    AlwaysAssertExit (tpath.originalName() == "/abc//abc/abc/abc");
    AlwaysAssertExit (tpath.absoluteName() == "/abc/abc/abc/abc");

    // Test the strip/addDirectory functionality.
    AlwaysAssertExit (Path::stripDirectory ("././abc", "././././abc/de")
		      == "de/.");
    AlwaysAssertExit (Path::stripDirectory ("././abc", "././././abd/de")
		      == "abc");
    AlwaysAssertExit (Path::stripDirectory ("abc", "abc/de")
		      == "de/.");
    AlwaysAssertExit (Path::stripDirectory ("././abc/de/ef", "././././abc/de")
		      == "././ef");
    AlwaysAssertExit (Path::stripDirectory ("abc/ef", "abc/de")
		      == "./ef");
    AlwaysAssertExit (Path::stripDirectory ("ef", "de")
		      == "./ef");
    AlwaysAssertExit (Path::stripDirectory ("$HOME/ef", "de")
		      == "$HOME/ef");
    AlwaysAssertExit (Path::stripDirectory ("/de/abc/ab/ef", "/de/abc/de")
		      == "./ab/ef");
    AlwaysAssertExit (Path::stripDirectory ("/de/abc/ab/ef", "/de/abc/ab")
		      == "././ef");
    AlwaysAssertExit (Path::addDirectory ("de/ab", "/ghi/a")
		      == "de/ab");
    AlwaysAssertExit (Path::addDirectory ("./de/ab", "/ghi/a")
		      == "/ghi/de/ab");
    AlwaysAssertExit (Path::addDirectory ("././de/ab", "/ghi/a")
		      == "/ghi/a/de/ab");
}


int main (int argc, const char*[])
{
    Bool success = True;
    try {
	doIt ( (argc<2), success);
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    if (success) {
	cout << "OK" << endl;
	return 0;                           // exit with success status
    }
}
