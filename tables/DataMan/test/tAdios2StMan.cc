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
//#        Internet email: aips2-request@nrao.edu.
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
void GenData(Array<T> &arr, uint32_t row){
    indgen(arr, static_cast<T>(row));
}

template<class T>
void GenData(T &sca, uint32_t row){
    sca = row + 1;
}

void GenData(String &str, uint32_t row){
    str = "string for Row " + std::to_string(row);
}

void GenData(Array<String> &str, uint32_t row){
    size_t s=0;
    for(auto &i : str){
        i = "string for Row " + std::to_string(row) + " Element " + std::to_string(s);
        ++s;
    }
}

template<class T>
void VerifyArrayColumn(Table &table, std::string column, uint32_t rows, IPosition array_pos)
{
    ArrayColumn<T> array_column(table, column);
    for(uint32_t i=0; i<rows; ++i)
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
void VerifyScalarColumn(Table &table, std::string column, uint32_t rows)
{
    ScalarColumn<T> scalar_column(table, column);
    for(uint32_t i=0; i<rows; ++i)
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

void doWriteDefault(std::string filename, uint32_t rows, IPosition array_pos)
{
    TableDesc td("", "1", TableDesc::Scratch);
    td.addColumn (ScalarColumnDesc<bool>("scalar_Bool"));
    td.addColumn (ScalarColumnDesc<unsigned char>("scalar_uChar"));
    td.addColumn (ScalarColumnDesc<int16_t>("scalar_Short"));
    td.addColumn (ScalarColumnDesc<uint16_t>("scalar_uShort"));
    td.addColumn (ScalarColumnDesc<int32_t>("scalar_Int"));
    td.addColumn (ScalarColumnDesc<uint32_t>("scalar_uInt"));
    td.addColumn (ScalarColumnDesc<float>("scalar_Float"));
    td.addColumn (ScalarColumnDesc<double>("scalar_Double"));
    td.addColumn (ScalarColumnDesc<Complex>("scalar_Complex"));
    td.addColumn (ScalarColumnDesc<DComplex>("scalar_DComplex"));
    td.addColumn (ScalarColumnDesc<String>("scalar_String"));

    td.addColumn (ArrayColumnDesc<bool>("array_Bool", array_pos, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<unsigned char>("array_uChar", array_pos, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<int16_t>("array_Short", array_pos, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<uint16_t>("array_uShort", array_pos, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<int32_t>("array_Int", array_pos, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<uint32_t>("array_uInt", array_pos, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float>("array_Float", array_pos, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<double>("array_Double", array_pos, ColumnDesc::FixedShape));
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

    ScalarColumn<bool> scalar_Bool (tab, "scalar_Bool");
    ScalarColumn<unsigned char> scalar_uChar (tab, "scalar_uChar");
    ScalarColumn<int16_t> scalar_Short (tab, "scalar_Short");
    ScalarColumn<uint16_t> scalar_uShort (tab, "scalar_uShort");
    ScalarColumn<int32_t> scalar_Int (tab, "scalar_Int");
    ScalarColumn<uint32_t> scalar_uInt (tab, "scalar_uInt");
    ScalarColumn<float> scalar_Float (tab, "scalar_Float");
    ScalarColumn<double> scalar_Double (tab, "scalar_Double");
    ScalarColumn<Complex> scalar_Complex (tab, "scalar_Complex");
    ScalarColumn<DComplex> scalar_DComplex (tab, "scalar_DComplex");
    ScalarColumn<String> scalar_String (tab, "scalar_String");

    ArrayColumn<bool> array_Bool (tab, "array_Bool");
    ArrayColumn<unsigned char> array_uChar (tab, "array_uChar");
    ArrayColumn<int16_t> array_Short (tab, "array_Short");
    ArrayColumn<uint16_t> array_uShort (tab, "array_uShort");
    ArrayColumn<int32_t> array_Int (tab, "array_Int");
    ArrayColumn<uint32_t> array_uInt (tab, "array_uInt");
    ArrayColumn<float> array_Float (tab, "array_Float");
    ArrayColumn<double> array_Double (tab, "array_Double");
    ArrayColumn<Complex> array_Complex (tab, "array_Complex");
    ArrayColumn<DComplex> array_DComplex (tab, "array_DComplex");
    ArrayColumn<String> array_String (tab, "array_String");

    Array<bool> arr_Bool(array_pos);
    Array<char> arr_Char(array_pos);
    Array<unsigned char> arr_uChar(array_pos);
    Array<int16_t> arr_Short(array_pos);
    Array<uint16_t> arr_uShort(array_pos);
    Array<int32_t> arr_Int(array_pos);
    Array<uint32_t> arr_uInt(array_pos);
    Array<float> arr_Float(array_pos);
    Array<double> arr_Double(array_pos);
    Array<Complex> arr_Complex(array_pos);
    Array<DComplex> arr_DComplex(array_pos);
    Array<String> arr_String(array_pos);

    bool sca_Bool;
    unsigned char sca_uChar;
    int16_t sca_Short;
    uint16_t sca_uShort;
    int32_t sca_Int;
    uint32_t sca_uInt;
    float sca_Float;
    double sca_Double;
    Complex sca_Complex;
    DComplex sca_DComplex;
    String sca_String;

    for(uint32_t i=0; i<rows; ++i)
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

void doReadScalar(std::string filename, uint32_t rows){
    Table casa_table(filename);
    VerifyScalarColumn<bool>(casa_table, "scalar_Bool", rows);
    VerifyScalarColumn<unsigned char>(casa_table, "scalar_uChar", rows);
    VerifyScalarColumn<int16_t>(casa_table, "scalar_Short", rows);
    VerifyScalarColumn<uint16_t>(casa_table, "scalar_uShort", rows);
    VerifyScalarColumn<int32_t>(casa_table, "scalar_Int", rows);
    VerifyScalarColumn<uint32_t>(casa_table, "scalar_uInt", rows);
    VerifyScalarColumn<float>(casa_table, "scalar_Float", rows);
    VerifyScalarColumn<double>(casa_table, "scalar_Double", rows);
    VerifyScalarColumn<Complex>(casa_table, "scalar_Complex", rows);
    VerifyScalarColumn<DComplex>(casa_table, "scalar_DComplex", rows);
    VerifyScalarColumn<String>(casa_table, "scalar_String", rows);
}

void doReadArray(std::string filename, uint32_t rows, IPosition array_pos){
    Table casa_table(filename);
    VerifyArrayColumn<bool>(casa_table, "array_Bool", rows, array_pos);
    VerifyArrayColumn<unsigned char>(casa_table, "array_uChar", rows, array_pos);
    VerifyArrayColumn<int16_t>(casa_table, "array_Short", rows, array_pos);
    VerifyArrayColumn<uint16_t>(casa_table, "array_uShort", rows, array_pos);
    VerifyArrayColumn<int32_t>(casa_table, "array_Int", rows, array_pos);
    VerifyArrayColumn<uint32_t>(casa_table, "array_uInt", rows, array_pos);
    VerifyArrayColumn<float>(casa_table, "array_Float", rows, array_pos);
    VerifyArrayColumn<double>(casa_table, "array_Double", rows, array_pos);
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

void doReadCopiedTable(std::string filename, std::string column, uint32_t rows, IPosition array_pos)
{
    Table tab(filename);
    VerifyArrayColumn<Complex>(tab, column, rows, array_pos);
}

int main(int argc, char **argv){

#ifdef HAVE_MPI
    MPI_Init(&argc,&argv);
#endif

    uint32_t rows = 100;
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


