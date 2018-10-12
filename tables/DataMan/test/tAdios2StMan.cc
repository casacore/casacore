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
//#
//# $Id$

#include <casacore/tables/DataMan/Adios2StMan.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/casa/namespace.h>
#include <mpi.h>

int mpiRank, mpiSize;
std::string filename = "test.table";
uInt rows = 100;
IPosition array_pos = IPosition(2,5,6);

template<class T>
void GenData(Array<T> &arr, uInt row){
    arr = row + 1;
}

template<class T>
void GenData(T &arr, uInt row){
    arr = row + 1;
}

int doWrite(){

    Adios2StMan *stman = new Adios2StMan(MPI_COMM_WORLD);

    int NrRows = mpiSize;


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

    SetupNewTable newtab(filename, td, Table::New);
    newtab.bindAll(*stman);
    Table *tab = new Table(MPI_COMM_WORLD, newtab, NrRows);

    ScalarColumn<Bool> scalar_Bool (*tab, "scalar_Bool");
    ScalarColumn<uChar> scalar_uChar (*tab, "scalar_uChar");
    ScalarColumn<Short> scalar_Short (*tab, "scalar_Short");
    ScalarColumn<uShort> scalar_uShort (*tab, "scalar_uShort");
    ScalarColumn<Int> scalar_Int (*tab, "scalar_Int");
    ScalarColumn<uInt> scalar_uInt (*tab, "scalar_uInt");
    ScalarColumn<Float> scalar_Float (*tab, "scalar_Float");
    ScalarColumn<Double> scalar_Double (*tab, "scalar_Double");
    ScalarColumn<Complex> scalar_Complex (*tab, "scalar_Complex");
    ScalarColumn<DComplex> scalar_DComplex (*tab, "scalar_DComplex");

    ArrayColumn<Bool> array_Bool (*tab, "array_Bool");
    ArrayColumn<uChar> array_uChar (*tab, "array_uChar");
    ArrayColumn<Short> array_Short (*tab, "array_Short");
    ArrayColumn<uShort> array_uShort (*tab, "array_uShort");
    ArrayColumn<Int> array_Int (*tab, "array_Int");
    ArrayColumn<uInt> array_uInt (*tab, "array_uInt");
    ArrayColumn<Float> array_Float (*tab, "array_Float");
    ArrayColumn<Double> array_Double (*tab, "array_Double");
    ArrayColumn<Complex> array_Complex (*tab, "array_Complex");
    ArrayColumn<DComplex> array_DComplex (*tab, "array_DComplex");

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

    for(uInt i=0; i<rows; ++i)
    {
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
    }

    delete tab;
    delete stman;

    return 0;
}

template<class T>
void VerifyArrayColumn(Table &table, std::string column)
{
    ROArrayColumn<T> array_column(table, column);
    for(uInt i=0; i<rows; ++i)
    {
        Array<T> arr_read = array_column.get(i);
        Array<T> arr_gen(array_pos);
        GenData(arr_gen, i);
        AlwaysAssertExit (arr_read.nelements() == arr_gen.nelements());
        for(size_t j=0; j<arr_read.nelements(); ++j)
        {
            AlwaysAssertExit (arr_read.data()[j] == arr_gen.data()[j]);
        }
    }
}

template<class T>
void VerifyScalarColumn(Table &table, std::string column)
{
    ROScalarColumn<T> scalar_column(table, column);
    for(uInt i=0; i<rows; ++i)
    {
        T scalar_read = scalar_column.get(i);
        T scalar_gen;
        GenData(scalar_gen, i);
        AlwaysAssertExit (scalar_gen == scalar_read);
    }
}

void doRead(){

    Table casa_table(filename);

    VerifyArrayColumn<Bool>(casa_table, "array_Bool");
    VerifyArrayColumn<uChar>(casa_table, "array_uChar");
    VerifyArrayColumn<Short>(casa_table, "array_Short");
    VerifyArrayColumn<uShort>(casa_table, "array_uShort");
    VerifyArrayColumn<Int>(casa_table, "array_Int");
    VerifyArrayColumn<uInt>(casa_table, "array_uInt");
    VerifyArrayColumn<Float>(casa_table, "array_Float");
    VerifyArrayColumn<Double>(casa_table, "array_Double");
    VerifyArrayColumn<Complex>(casa_table, "array_Complex");
    VerifyArrayColumn<DComplex>(casa_table, "array_DComplex");

    VerifyScalarColumn<Bool>(casa_table, "scalar_Bool");
    VerifyScalarColumn<uChar>(casa_table, "scalar_uChar");
    VerifyScalarColumn<Short>(casa_table, "scalar_Short");
    VerifyScalarColumn<uShort>(casa_table, "scalar_uShort");
    VerifyScalarColumn<Int>(casa_table, "scalar_Int");
    VerifyScalarColumn<uInt>(casa_table, "scalar_uInt");
    VerifyScalarColumn<Float>(casa_table, "scalar_Float");
    VerifyScalarColumn<Double>(casa_table, "scalar_Double");
    VerifyScalarColumn<Complex>(casa_table, "scalar_Complex");
    VerifyScalarColumn<DComplex>(casa_table, "scalar_DComplex");

}

int main(int argc, char **argv){

    MPI_Init(&argc,&argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpiRank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpiSize);

    doWrite();
    doRead();

    MPI_Finalize();
}


