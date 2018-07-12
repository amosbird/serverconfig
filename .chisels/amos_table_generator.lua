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
description = "Given two filter fields, a key and a value, this chisel creates and renders a table to the screen."
short_description = "Filter on a key and value"
category = "Filter"
hidden = true

-- Chisel argument list
args =
{
	{
		name = "keys",
		description = "comma-separated list of filter fields to use for grouping",
		argtype = "string"
	},
	{
		name = "keydescs",
		description = "comma separated list of human readable descriptions for the key",
		argtype = "string"
	},
	{
		name = "value",
		description = "the value to count for every key",
		argtype = "string"
	},
	{
		name = "valuedesc",
		description = "human readable description for the value",
		argtype = "string"
	},
	{
		name = "filter",
		description = "the filter to apply",
		argtype = "string"
	},
	{
		name = "top_number",
		description = "maximum number of elements to display",
		argtype = "string"
	},
	{
		name = "value_units",
		description = "how to render the values in the result. Can be 'bytes', 'time', 'timepct', or 'none'.",
		argtype = "string"
	},
}

require "common"
terminal = require "ansiterminal"

grtable = {}
filter = ""
islive = false
fkeys = {}

vizinfo =
{
	key_fld = {},
	key_desc = {},
	value_fld = "",
	value_desc = "",
	value_units = "none",
	top_number = 0,
	output_format = "normal"
}

-- Argument notification callback
function on_set_arg(name, val)
	if name == "keys" then
		vizinfo.key_fld = split(val, ",")
		return true
	elseif name == "keydescs" then
		vizinfo.key_desc = split(val, ",")
		return true
	elseif name == "value" then
		vizinfo.value_fld = val
		return true
	elseif name == "valuedesc" then
		vizinfo.value_desc = val
		return true
	elseif name == "filter" then
		filter = val
		return true
	elseif name == "top_number" then
		vizinfo.top_number = tonumber(val)
		return true
	elseif name == "value_units" then
		vizinfo.value_units = val
		return true
	end

	return false
end

-- Initialization callback
function on_init()
	if #vizinfo.key_fld ~= #vizinfo.key_desc then
		print("error: number of entries in keys different from number entries in keydescs")
		return false
	end

	-- Request the fields we need
	for i, name in ipairs(vizinfo.key_fld) do
		fkeys[i] = chisel.request_field(name)
	end

	fvalue = chisel.request_field(vizinfo.value_fld)

	-- set the filter
	if filter ~= "" then
		chisel.set_filter(filter)
	end

	return true
end

-- Final chisel initialization
function on_capture_start()
	islive = sysdig.is_live()
	vizinfo.output_format = sysdig.get_output_format()

	if islive then
		chisel.set_interval_s(1)
		if vizinfo.output_format ~= "json" then
			terminal.clearscreen()
			terminal.hidecursor()
		end
	end

	return true
end

-- Event parsing callback
function on_event()
	local key = nil
	local kv = nil

	for i, fld in ipairs(fkeys) do
		kv = evt.field(fld)
		if kv == nil then
			return
		end

		if key == nil then
			key = kv
		else
			key = key .. "\001\001" .. evt.field(fld)
		end
	end

	value = evt.field(fvalue)

	if value ~= nil and value > 0 then
		entryval = grtable[key]

		if entryval == nil then
			grtable[key] = value
		else
			grtable[key] = grtable[key] + value
		end
	end

	return true
end

local tsize = require "tsize"

local r, c = tsize.tsize()
c = (c - 40) / 2 - 12
local padding = string.rep(" ", c)
local function print_c(s)
	print(padding .. s)
end

local function print_sorted_table(stable, ts_s, ts_ns, timedelta, viz_info)
	local sorted_grtable = pairs_top_by_val(stable, viz_info.top_number, function(t,a,b) return t[b] < t[a] end)

	if viz_info.output_format == "json" then
		local jdata = {}
		local j = 1

		for k,v in sorted_grtable do
			local vals = split(k, "\001\001")
			vals[#vals + 1] = v
			jdata[j] = vals
			j = j + 1
		end

		local jinfo = {}

		for i, keyname in ipairs(viz_info.key_fld) do
			jinfo[i] = {name = keyname, desc = viz_info.key_desc[i], is_key = true}
		end
		jinfo[3] = {name = viz_info.value_fld, desc = viz_info.value_desc, is_key = false}

		local res = {ts = sysdig.make_ts(ts_s, ts_ns), data = jdata, info = jinfo}

		local str = json.encode(res, { indent = true })
		print(str)
	else
		-- Same size to extend each string
		local EXTEND_STRING_SIZE = 12
		local header = extend_string(viz_info.value_desc, EXTEND_STRING_SIZE)

		for i, fldname in ipairs(viz_info.key_desc) do
			header = header .. extend_string(fldname, EXTEND_STRING_SIZE)
		end

		print_c("            ----------------------------------------")
		print_c("            |   aggregate over last " .. math.floor(0.5 + timedelta / 1000000000) .. " second(s).   |")
		print_c("            ----------------------------------------")
		print()
		print_c(header)
		print_c("--------------------------------------------------------------------------------")

		for k,v in sorted_grtable do
			local keystr = ""

			local singlekeys = split(k, "\001\001")

			for i, singlekey in ipairs(singlekeys) do
				if i < #singlekeys then
					keystr = keystr .. extend_string(string.sub(singlekey, 0, EXTEND_STRING_SIZE), EXTEND_STRING_SIZE)
				else
					keystr = keystr .. singlekey
				end
			end

			if viz_info.value_units == "none" then
				print_c(extend_string(tostring(v), EXTEND_STRING_SIZE) .. keystr)
			elseif viz_info.value_units == "bytes" then
				print_c(extend_string(format_bytes(v), EXTEND_STRING_SIZE) .. keystr)
			elseif viz_info.value_units == "time" then
				print_c(extend_string(format_time_interval(v), EXTEND_STRING_SIZE) .. keystr)
			elseif viz_info.value_units == "timepct" then
				if timedelta > 0 then
					pctstr = string.format("%.2f%%", v / timedelta * 100)
				else
					pctstr = "0.00%"
				end

				print_c(extend_string(pctstr, EXTEND_STRING_SIZE) .. keystr)
			end
		end
	end
end

-- Periodic timeout callback
function on_interval(ts_s, ts_ns, delta)
	chisel.set_interval_s(3)
	if vizinfo.output_format ~= "json" then
		terminal.clearscreen()
		terminal.moveto(0, 0)
	end

	print_sorted_table(grtable, ts_s, 0, delta, vizinfo)

	-- Clear the table
	grtable = {}

	return true
end

-- Called by the engine at the end of the capture (Ctrl-C)
function on_capture_end(ts_s, ts_ns, delta)
	if islive and vizinfo.output_format ~= "json" then
		terminal.clearscreen()
		terminal.moveto(0 ,0)
		terminal.showcursor()
		return true
	end

	print_sorted_table(grtable, ts_s, 0, delta, vizinfo)

	return true
end
