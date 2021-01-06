import gdb.printing
from typing import List, Dict, Tuple

from PODArray import PODArrayPrinter
from ColumnVector import ColumnVectorPrinter


def print_icolumn_type(gdb.Value col) -> str:
    obj_type: gdb.Type = col.dynamic_type
    obj_casted: gdb.Value = col.cast(obj_type)

    if obj_type == t_col_array:
        offsets: gdb.Value = gdb.dereference(obj_casted["offsets"]).cast(t_col_offsets)["data"]

        start, end, _= get_podarray_bounds(offsets)

        nested_printed: str = print_icolumn_type(gdb.dereference(obj_casted["data"]))

        return "ColumnArray[size: {}, {}]".format(end - start, nested_printed)
    elif obj_type == t_col_const:
        data: gdb.Value = gdb.dereference(obj_casted["data"])
        nested_printed: str = print_icolumn_type(data)

        return "ColumnConst[size: {}, {}]".format(obj_casted["s"], nested_printed)
    elif obj_type == t_col_lc:
        dictionary: gdb.Value = obj_casted["dictionary"]
        index: gdb.Value = obj_casted["idx"]

        size_of_index_type: int = int(index["size_of_type"])

        index_type: str = "UInt8"

        if size_of_index_type == t_uint16.sizeof:
            index_type = "UInt16"
        elif size_of_index_type == t_uint32.sizeof:
            index_type = "UInt32"
        else
            index_type = "UInt64"

        nested_col: gdb.Value = gdb.dereference(
            gdb.dereference(dictionary["column_unique"])["column_holder"])

        nested_printed: str = print_icolumn_type(nested_col)

        return "ColumnLowCardinality[index: {}, {}]".format(index_type, nested_printed)
    elif obj_type == t_col_nullable:
        col_nested: gdb.Value = gdb.dereference(obj_casted["nested_column"])
        nested_printed: str = print_icolumn_type(col_nested)

        return "ColumnNullable[{}]".format(nested_printed)
    elif obj_type == t_col_vector:
        nested_type = obj_casted.template_argument(0)
        start, end, _ = get_podarray_bounds(obj_casted["data"])

        return "ColumnVector[size: {}, {}]".format(nested_type, end - start)
    else:
        return "NI {}".format(obj_type)

def build_pretty_printers():
    pp = gdb.printing.RegexpCollectionPrettyPrinter("clickhouse")

    pp.add_printer('PODArray', '^DB::PODArray<.*>$', PODArrayPrinter)
    pp.add_printer('PaddedPODArray', '^DB::PaddedPODArray<.*>$', PODArrayPrinter)

    pp.add_printer('ColumnVector', '^DB::ColumnVector<.*>$', ColumnVectorPrinter)

    return pp

def register_ch_printers():
    gdb.printing.register_pretty_printer(
        gdb.current_objfile(),
        build_pretty_printers())
