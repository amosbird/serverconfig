--[[
    Copyright (C) 2013-2014 Draios inc.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License version 2 as
    published by the Free Software Foundation.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
--]]

-- Chisel description
description = "Shows the top files in terms of disk usage. This chisel is compatible with containers using the sysdig -pc or -pcontainer argument, otherwise no container information will be shown."
short_description = "Top files by R+W bytes"
category = "I/O"

-- Argument list
args =
    {
    {
        name = "pid",
        description = "Top number of files.",
        argtype = "int",
        optional = true
    }
}

local top = 10
require "common"

-- Argument notification callback
function on_set_arg(name, val)
    if name == "pid" then
        pid = parse_numeric_input(val, name)
        return true
    end
    return false
end

-- Initialization callback
function on_init()

    -- The -pc or -pcontainer options was supplied on the cmd line
    print_container = sysdig.is_print_container_data()

    if pid then
        pattern = "(fd.type=ipv4 or fd.type=ipv6) and evt.is_io=true and proc.pid="..pid
    else
        pattern = "(fd.type=ipv4 or fd.type=ipv6) and evt.is_io=true"
    end
    if print_container then
        chisel.exec("amos_table_generator",
            "evt.type,container.name,fd.name",
            "Type,container.name,Connection",
            "evt.rawarg.res",
            "Bytes",
            pattern,
            "" .. top,
            "bytes")
    else
        chisel.exec("amos_table_generator",
            "evt.type,fd.name",
            "Type,Connection",
            "evt.rawarg.res",
            "Bytes",
            pattern,
            "" .. top,
            "bytes")
    end

    return true
end
