from typing import Tuple
from registry import *
import gdb

class ColumnVectorPrinter:
    def __init__(self, val: gdb.Value) -> None:
        self.val: gdb.Value = val

    class _iterator:
        def __init__(self, start: gdb.Value, finish: gdb.Value) -> None:
            self.item: gdb.Value = start
            self.finish: gdb.Value = finish
            self.count: int = 0

        def __iter__(self) -> _iterator:
            return self

        def __next__(self):
            count: int = self.count
            self.count += 1

            if self.item == self.finish:
                raise StopIteration

            elt = self.item.dereference()
            self.item += 1

            return ('[%d]'.format(count), elt)

    def to_string(self) -> str:
        offsets: gdb.Value = gdb.dereference(self.val["offsets"]).cast(t_col_offsets)["data"]

        start, end, _= get_podarray_bounds(offsets)

        nested_printed: str = print_icolumn_type(gdb.dereference(obj_casted["data"]))

        nested_type = obj_casted.template_argument(0)

        return "ColumnArray[size: {}, {}]".format(end - start, nested_printed)

    def children(self) -> _iterator:
        return self._iterator(
            self.val['c_start'],
            self.val['c_end'])

    def display_hint(self) -> str:
        return "array"
