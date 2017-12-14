//# azel.cc: utility to print azimut and elevation of a given source

#include <casacore/tables/TaQL/TableParse.h>

using namespace casacore;
using namespace std;

int main(int argc, char** argv) {
  string source("Jupiter"), observatory("DWL"), time("datetime()");

  if (argc==1) {
    cout << "Usage: azel [source] [observatory] [time]" << endl;
    cout << "  source: default Jupiter" << endl;
    cout << "  observatory: default DWL" << endl;
    cout << "  time: default now, example (UTC) \"2016-06-28 19:54\"" << endl;
    exit(0);
  }
  if (argc > 1) {
    source = argv[1];
  }
  if (argc > 2) {
    observatory = argv[2];
  }
  if (argc > 3) {
    time = argv[3];
  }

  string query = "calc meas.azel(";
  query += "'" + source + "', ";
  query += time + ", ";
  query += "'" + observatory + "'";
  query += ") deg";

  TaQLResult result = tableCommand(query);

  Vector<double> coords = result.node().getArrayDouble(0);
  cout << "az: " << coords[0] << " deg" << endl;
  cout << "el: " << coords[1] << " deg" << endl;
}
