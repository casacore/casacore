//# tAdios2StMan.cc: Test program for the ADIOS2 storage manager
//# Copyright (C) 2018
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/tables/DataMan/Adios2StMan.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/TableCopy.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/casa/namespace.h>


template<class T>
void GenData(Array<T> &arr, uInt row){
    indgen(arr, static_cast<T>(row));
}

template<class T>
void GenData(T &sca, uInt row){
    sca = row + 1;
}

void GenData(String &str, uInt row){
    str = "string for Row " + std::to_string(row);
}

void GenData(Array<String> &str, uInt row){
    size_t s=0;
    for(auto &i : str){
        i = "string for Row " + std::to_string(row) + " Element " + std::to_string(s);
        ++s;
    }
}

template<class T>
void VerifyArrayColumn(Table &table, std::string column, uInt rows, IPosition array_pos)
{
    ArrayColumn<T> array_column(table, column);
    for(uInt i=0; i<rows; ++i)
    {
        Array<T> arr_read = array_column.get(i);
        Array<T> arr_gen(array_pos);
        GenData(arr_gen, i);
        AlwaysAssertExit (arr_read.nelements() == arr_gen.nelements());
        for(size_t j=0; j<arr_read.nelements(); ++j)
        {
            std::cout << "Column : " << column << ", Row : " << i;
            std::cout << ", Read : " << arr_read.data()[j];
            std::cout << ", Generated : " << arr_gen.data()[j] << std::endl;
            AlwaysAssertExit (arr_read.data()[j] == arr_gen.data()[j]);
        }
    }
}

template<class T>
void VerifyScalarColumn(Table &table, std::string column, uInt rows)
{
    ScalarColumn<T> scalar_column(table, column);
    for(uInt i=0; i<rows; ++i)
    {
        T scalar_read = scalar_column.get(i);
        T scalar_gen;
        GenData(scalar_gen, i);
        std::cout << "Column : " << column << ", Row : " << i;
        std::cout << ", Read : " << scalar_read;
        std::cout << ", Generated : " << scalar_gen << std::endl;
        AlwaysAssertExit (scalar_gen == scalar_read);
    }
}

void doWriteDefault(std::string filename, uInt rows, IPosition array_pos)
{
    TableDesc td("", "1", TableDesc::Scratch);
    td.addColumn (ScalarColumnDesc<Bool>("scalar_Bool"));
    td.addColumn (ScalarColumnDesc<uChar>("scalar_uChar"));
    td.addColumn (ScalarColumnDesc<Short>("scalar_Short"));
    td.addColumn (ScalarColumnDesc<uShort>("scalar_uShort"));
    td.addColumn (ScalarColumnDesc<Int>("scalar_Int"));
    td.addColumn (ScalarColumnDesc<uInt>("scalar_uInt"));
    td.addColumn (ScalarColumnDesc<Float>("scalar_Float"));
    td.addColumn (ScalarColumnDesc<Double>("scalar_Double"));
    td.addColumn (ScalarColumnDesc<Complex>("scalar_Complex"));
    td.addColumn (ScalarColumnDesc<DComplex>("scalar_DComplex"));
    td.addColumn (ScalarColumnDesc<String>("scalar_String"));

    td.addColumn (ArrayColumnDesc<Bool>("array_Bool", array_pos, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<uChar>("array_uChar", array_pos, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<Short>("array_Short", array_pos, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<uShort>("array_uShort", array_pos, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<Int>("array_Int", array_pos, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<uInt>("array_uInt", array_pos, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<Float>("array_Float", array_pos, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<Double>("array_Double", array_pos, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<Complex>("array_Complex", array_pos, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<DComplex>("array_DComplex", array_pos, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<String>("array_String", array_pos, ColumnDesc::FixedShape));

    SetupNewTable newtab(filename, td, Table::New);
#ifdef HAVE_MPI
    Adios2StMan stman(MPI_COMM_WORLD);
    newtab.bindAll(stman);
    Table tab(MPI_COMM_WORLD, newtab, rows);
#else
    Adios2StMan stman;
    newtab.bindAll(stman);
    Table tab(newtab, rows);
#endif // HAVE_MPI

    ScalarColumn<Bool> scalar_Bool (tab, "scalar_Bool");
    ScalarColumn<uChar> scalar_uChar (tab, "scalar_uChar");
    ScalarColumn<Short> scalar_Short (tab, "scalar_Short");
    ScalarColumn<uShort> scalar_uShort (tab, "scalar_uShort");
    ScalarColumn<Int> scalar_Int (tab, "scalar_Int");
    ScalarColumn<uInt> scalar_uInt (tab, "scalar_uInt");
    ScalarColumn<Float> scalar_Float (tab, "scalar_Float");
    ScalarColumn<Double> scalar_Double (tab, "scalar_Double");
    ScalarColumn<Complex> scalar_Complex (tab, "scalar_Complex");
    ScalarColumn<DComplex> scalar_DComplex (tab, "scalar_DComplex");
    ScalarColumn<String> scalar_String (tab, "scalar_String");

    ArrayColumn<Bool> array_Bool (tab, "array_Bool");
    ArrayColumn<uChar> array_uChar (tab, "array_uChar");
    ArrayColumn<Short> array_Short (tab, "array_Short");
    ArrayColumn<uShort> array_uShort (tab, "array_uShort");
    ArrayColumn<Int> array_Int (tab, "array_Int");
    ArrayColumn<uInt> array_uInt (tab, "array_uInt");
    ArrayColumn<Float> array_Float (tab, "array_Float");
    ArrayColumn<Double> array_Double (tab, "array_Double");
    ArrayColumn<Complex> array_Complex (tab, "array_Complex");
    ArrayColumn<DComplex> array_DComplex (tab, "array_DComplex");
    ArrayColumn<String> array_String (tab, "array_String");

    Array<Bool> arr_Bool(array_pos);
    Array<Char> arr_Char(array_pos);
    Array<uChar> arr_uChar(array_pos);
    Array<Short> arr_Short(array_pos);
    Array<uShort> arr_uShort(array_pos);
    Array<Int> arr_Int(array_pos);
    Array<uInt> arr_uInt(array_pos);
    Array<Float> arr_Float(array_pos);
    Array<Double> arr_Double(array_pos);
    Array<Complex> arr_Complex(array_pos);
    Array<DComplex> arr_DComplex(array_pos);
    Array<String> arr_String(array_pos);

    Bool sca_Bool;
    uChar sca_uChar;
    Short sca_Short;
    uShort sca_uShort;
    Int sca_Int;
    uInt sca_uInt;
    Float sca_Float;
    Double sca_Double;
    Complex sca_Complex;
    DComplex sca_DComplex;
    String sca_String;

    for(uInt i=0; i<rows; ++i)
    {
        GenData(sca_Bool, i);
        GenData(sca_uChar, i);
        GenData(sca_Short, i);
        GenData(sca_uShort, i);
        GenData(sca_Int, i);
        GenData(sca_uInt, i);
        GenData(sca_Float, i);
        GenData(sca_Double, i);
        GenData(sca_Complex, i);
        GenData(sca_DComplex, i);
        GenData(sca_String, i);

        GenData(arr_Bool, i);
        GenData(arr_uChar, i);
        GenData(arr_Short, i);
        GenData(arr_uShort, i);
        GenData(arr_Int, i);
        GenData(arr_uInt, i);
        GenData(arr_Float, i);
        GenData(arr_Double, i);
        GenData(arr_Complex, i);
        GenData(arr_DComplex, i);
        GenData(arr_String, i);

        scalar_Bool.put (i, sca_Bool);
        scalar_uChar.put (i, sca_uChar);
        scalar_Short.put (i, sca_Short);
        scalar_uShort.put (i, sca_uShort);
        scalar_Int.put (i, sca_Int);
        scalar_uInt.put (i, sca_uInt);
        scalar_Float.put (i, sca_Float);
        scalar_Double.put (i, sca_Double);
        scalar_Complex.put (i, sca_Complex);
        scalar_DComplex.put (i, sca_DComplex);
        scalar_String.put (i, sca_String);

        array_Bool.put(i, arr_Bool);
        array_uChar.put(i, arr_uChar);
        array_Short.put(i, arr_Short);
        array_uShort.put(i, arr_uShort);
        array_Int.put(i, arr_Int);
        array_uInt.put(i, arr_uInt);
        array_Float.put(i, arr_Float);
        array_Double.put(i, arr_Double);
        array_Complex.put(i, arr_Complex);
        array_DComplex.put(i, arr_DComplex);
        array_String.put(i, arr_String);
    }
}

void doReadScalar(std::string filename, uInt rows){
    Table casa_table(filename);
    VerifyScalarColumn<Bool>(casa_table, "scalar_Bool", rows);
    VerifyScalarColumn<uChar>(casa_table, "scalar_uChar", rows);
    VerifyScalarColumn<Short>(casa_table, "scalar_Short", rows);
    VerifyScalarColumn<uShort>(casa_table, "scalar_uShort", rows);
    VerifyScalarColumn<Int>(casa_table, "scalar_Int", rows);
    VerifyScalarColumn<uInt>(casa_table, "scalar_uInt", rows);
    VerifyScalarColumn<Float>(casa_table, "scalar_Float", rows);
    VerifyScalarColumn<Double>(casa_table, "scalar_Double", rows);
    VerifyScalarColumn<Complex>(casa_table, "scalar_Complex", rows);
    VerifyScalarColumn<DComplex>(casa_table, "scalar_DComplex", rows);
    VerifyScalarColumn<String>(casa_table, "scalar_String", rows);
}

void doReadArray(std::string filename, uInt rows, IPosition array_pos){
    Table casa_table(filename);
    VerifyArrayColumn<Bool>(casa_table, "array_Bool", rows, array_pos);
    VerifyArrayColumn<uChar>(casa_table, "array_uChar", rows, array_pos);
    VerifyArrayColumn<Short>(casa_table, "array_Short", rows, array_pos);
    VerifyArrayColumn<uShort>(casa_table, "array_uShort", rows, array_pos);
    VerifyArrayColumn<Int>(casa_table, "array_Int", rows, array_pos);
    VerifyArrayColumn<uInt>(casa_table, "array_uInt", rows, array_pos);
    VerifyArrayColumn<Float>(casa_table, "array_Float", rows, array_pos);
    VerifyArrayColumn<Double>(casa_table, "array_Double", rows, array_pos);
    VerifyArrayColumn<Complex>(casa_table, "array_Complex", rows, array_pos);
    VerifyArrayColumn<DComplex>(casa_table, "array_DComplex", rows, array_pos);
    VerifyArrayColumn<String>(casa_table, "array_String", rows, array_pos);
}

void doCopyTable(std::string inTable, std::string outTable, std::string column)
{
    Table tab(inTable);
    TableDesc td("", "1", TableDesc::Scratch);
    SetupNewTable newtab(outTable, td, Table::New);
    Table duptab(newtab);
    duptab.addRow(tab.nrow());
    Adios2StMan a2stman;
    duptab.addColumn(tab.tableDesc().columnDesc(column), a2stman);
    TableCopy::copyColumnData(tab, column, duptab, column, false);
}

void doReadCopiedTable(std::string filename, std::string column, uInt rows, IPosition array_pos)
{
    Table tab(filename);
    VerifyArrayColumn<Complex>(tab, column, rows, array_pos);
}

int main(int argc, char **argv){

#ifdef HAVE_MPI
    MPI_Init(&argc,&argv);
#endif

    uInt rows = 100;
    IPosition array_pos = IPosition(2,5,6);

    doWriteDefault("default.table", rows, array_pos);
    doReadScalar("default.table", rows);
    doReadArray("default.table", rows, array_pos);

    doCopyTable("default.table", "duplicated.table", "array_Complex");
    doReadCopiedTable("duplicated.table", "array_Complex", rows, array_pos);

#ifdef HAVE_MPI
    MPI_Finalize();
#endif
}


