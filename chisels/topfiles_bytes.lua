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
                name = "top",
                description = "Top number of files.",
                argtype = "int",
                optional = true
        }
}

local top = 10

-- Argument notification callback
function on_set_arg(name, val)
    if name == "top" then
        top = val
        return true
    end
    return false
end

-- Initialization callback
function on_init()

	-- The -pc or -pcontainer options was supplied on the cmd line
	print_container = sysdig.is_print_container_data()

	if print_container then
		chisel.exec("table_generator",
					"container.name,fd.name",
					"container.name,Filename",
					"evt.rawarg.res",
					"Bytes",
					"fd.type=file and evt.is_io=true",
					"" .. top,
					"bytes")
	else
		chisel.exec("table_generator",
					"fd.name",
					"Filename",
					"evt.rawarg.res",
					"Bytes",
					"fd.type=file and evt.is_io=true",
					"" .. top,
					"bytes")
	end

	return true
end
