//# tMSTimeGram.cc: Test program for MS Time selection parser
//# Copyright (C) 2004
//# Associated Universities, Inc. Washington Dc, Usa.
//#
//# This Library Is Free Software; You Can Redistribute It And/Or Modify It
//# Under The Terms Of The Gnu Library General Public License As Published By
//# The Free Software Foundation; Either Version 2 Of The License, Or (At Your
//# Option) Any Later Version.
//#
//# This Library Is Distributed In The Hope That It Will Be Useful, But Without
//# Any Warranty; Without Even The Implied Warranty Of Merchantability Or
//# Fitness For A Particular Purpose.  See The Gnu Library General Public
//# License For More Details.
//#
//# You Should Have Received A Copy Of The Gnu Library General Public License
//# Along With This Library; If Not, Write To The Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, Ma 02139, Usa.
//#
//# Correspondence Concerning Aips++ Should Be Addressed As Follows:
//#        Internet Email: Aips2-Request@Nrao.Edu.
//#        Postal Address: Aips++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, Va 22903-2475 Usa
//#
//# $Id$

#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/iostream.h>


#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/ms/MeasurementSets/MSColumns.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/ArrayMath.h>

#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/IO/ByteIO.h>


#include <casacore/ms/MSSel/MSTimeGram.h>
#include <casacore/ms/MSSel/MSSelection.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableRecord.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Inputs.h>

#include <casacore/casa/namespace.h>


int main(int argc, const char* argv[])
{
  try 
    {
      if (argc < 3)
	{
	  String mesg = String("Usage: " ) + 
	    String(argv[0]) + String(" <MSNAME> <TIME SELECTION EXPRESSION>");
	  throw(AipsError(mesg));
	}
      const String msName = argv[1];
      MeasurementSet ms(msName);
      MeasurementSet * mssel;
      cout << "Original table has rows " << ms.nrow() << endl;
      MSSelection mss;
      mss.setTimeExpr(String(argv[2]));
      TableExprNode node=mss.toTableExprNode(&ms);

      cout << "TableExprNode has rows = " << node.nrow() << endl;
      Table tablesel(ms.tableName(), Table::Update);
      mssel = new MeasurementSet(tablesel(node, node.nrow() ));
      cout << "After mssel constructor called " << endl;
      mssel->rename(ms.tableName()+"/SELECTED_TABLE", Table::Scratch);
      mssel->flush();
      if(mssel->nrow()==0) 
	cout << "Check your input, No data selected" << endl;
      else 
	cout << "selected table has rows " << mssel->nrow() << endl;
      delete mssel;
    } 
  catch (AipsError x) 
    {
      cout << "ERROR: " << x.getMesg() << endl;
      return 1;
    } 
  return 0;
}
