//
//  c++ -g -o tCasarc01 tCasarc01.cc -I../../../../include/casacore -L../../../../osx/lib -lcasacore
//
#include <casa/System/Casarc.h>
#include <iostream>

int main( int argc, char *argv[] ) {
    casa::Casarc &rc = casa::Casarc::instance("tCasarc01.rc");

    rc.put("viewer.dpg.position.mousetools", "top");
    rc.put("viewer.dpg.position.mousetools", "right");
    rc.put("viewer.dpg.position.mousetools", "top");
    rc.put("viewer.dpg.position.mousetools", "left");

    for ( casa::Casarc::iterator iter = rc.begin(); iter != rc.end(); ++iter )
	std::cout << iter->first << ": " << iter->second << std::endl;
}
