local w = weechat
local g = {
    script = {
        name = "cut",
        version = "0.1",
        author = "amosbird <amosbird@gmail.com>",
        license = "WTFPL",
        description = "Cut command buffer"
    }
}

function setup()
    assert(
        w.register(
            g.script.name,
            g.script.author,
            g.script.version,
            g.script.license,
            g.script.description,
            "", ""),
        "Unable to register script. Perhaps it's already loaded before?")

    local wee_ver = tonumber(w.info_get("version_number", "") or 0)
    if wee_ver < 0x01000000 then
        error("This script requires WeeChat v1.0 or higher")
    end
    w.hook_command(
        g.script.name,
        "Cut the buffer string into system's clipboard",
        "", "", "", "command_cb", "")
end

function cmd_cb(a, b, cmd, rc, out, err)
    w.print("", a)
    w.print("", b)
    w.print("", cmd)
    w.print("", rc)
    w.print("", out)
    w.print("", err)
end

function command_cb(_, buffer, _)
    line = w.buffer_get_string(buffer, "input")
    input_pos = w.buffer_get_integer(buffer, "input_pos")
    input_line = string.sub(line, 0, input_pos + 1)
    -- w.hook_process("bash -c 'echo " .. input_line .. " | osc52clip'", 10 * 1000, "", "")
    local f = assert(io.open("/tmp/s", "w"))
    assert(f:write(input_line))
    assert(f:close())
    assert(os.execute("osc52clip < /tmp/s"))
    w.buffer_set(buffer, "input", string.sub(line, input_pos + 1))
    w.buffer_set(buffer, "input_pos", "0")
    return w.WEECHAT_RC_OK
end

setup()

