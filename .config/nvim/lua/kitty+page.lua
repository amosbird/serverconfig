vim.api.nvim_create_autocmd({ "VimEnter", "VimResume" }, {
    group = vim.api.nvim_create_augroup("KittySetVarVimEnter", { clear = true }),
    callback = function()
        io.stdout:write("\x1b]1337;SetUserVar=in_editor=MQo\007")
    end,
})

vim.api.nvim_create_autocmd({ "VimLeave", "VimSuspend" }, {
    group = vim.api.nvim_create_augroup("KittyUnsetVarVimLeave", { clear = true }),
    callback = function()
        io.stdout:write("\x1b]1337;SetUserVar=in_editor\007")
    end,
})

return function(INPUT_LINE_NUMBER, CURSOR_LINE, CURSOR_COLUMN, SEARCH)
  -- use :messages to view log
  print('kitty sent:', INPUT_LINE_NUMBER, CURSOR_LINE, CURSOR_COLUMN, SEARCH)

  local function setOptions()
    vim.opt.encoding='utf-8'
    vim.opt.clipboard = 'unnamedplus'
    vim.opt.compatible = false
    vim.opt.number = false
    vim.opt.relativenumber = false
    vim.opt.termguicolors = true
    vim.opt.showmode = false
    vim.opt.ruler = false
    vim.opt.laststatus = 0
    vim.o.cmdheight = 0
    vim.opt.showcmd = false
    vim.opt.scrollback = INPUT_LINE_NUMBER + CURSOR_LINE
  end

  -- Use pcall to execute setOptions and catch any errors
  local success, errorMessage = pcall(setOptions)

  -- If an error occurred, substitute INPUT_LINE_NUMBER, CURSOR_LINE, CURSOR_COLUMN with 0,0,0
  if not success then
    -- print("Error setting options:", errorMessage)
    INPUT_LINE_NUMBER, CURSOR_LINE, CURSOR_COLUMN = 0, 0, 0
  end
  local term_buf = vim.api.nvim_create_buf(true, false);
  local term_io = vim.api.nvim_open_term(term_buf, {})

  -- vim.api.nvim_buf_set_keymap(term_buf, 'v', '<ESC>',
  --                             "<Esc>'<0v'>g_y<Cmd>lua vim.defer_fn(function() vim.cmd('quit') end, 10)<CR>",
  --                             { noremap = true })
  vim.api.nvim_buf_set_keymap(term_buf, 'v', '<ESC>', 'y<Cmd>lua vim.defer_fn(function() vim.cmd("quit") end, 10)<CR>', { noremap = true })
  vim.api.nvim_buf_set_keymap(term_buf, 'n', '<ESC>', '<Cmd>q<CR>', { noremap = true })
  vim.keymap.set('n', '<M-u>', function() pcall(vim.cmd, 'normal! n') end,
                 { buffer = term_buf, noremap = true, silent = true })
  vim.keymap.set('n', '<M-n>', function() pcall(vim.cmd, 'normal! N') end,
                 { buffer = term_buf, noremap = true, silent = true })
  vim.keymap.set('n', 'n', function() pcall(vim.cmd, 'normal! n') end,
                 { buffer = term_buf, noremap = true, silent = true })
  vim.keymap.set('n', 'N', function() pcall(vim.cmd, 'normal! N') end,
  vim.keymap.set('v', '<M-u>', function()
                   -- Set the search pattern to the selected text
                   vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('k?❯<CR>', true, false, true), 'n', true)
                   -- Perform the search forward within the visual selection
                   vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('n', true, false, true), 'x', false)
  end, { buffer = term_buf, noremap = true, silent = true })

  -- vim.keymap.set('v', '<M-u>', function() pcall(vim.cmd, 'normal! n') end,
  --                { buffer = term_buf, noremap = true, silent = true })
  vim.keymap.set('v', '<M-n>', function() pcall(vim.cmd, 'normal! N') end,
                 { buffer = term_buf, noremap = true, silent = true })

  -- local function search_next_from_cmdline()
  --   vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('<C-c>', true, true, true), 'n', true)
  --   vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('N', true, true, true), 'n', true)
  --   vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('?<Up>', true, true, true), 'n', true)
  -- end
  -- vim.keymap.set('c', '<M-s>', search_next_from_cmdline, { noremap = true, silent = true })
  vim.api.nvim_buf_set_keymap(term_buf, 'c', '<M-s>', '<C-t>', { noremap = true })
  vim.api.nvim_buf_set_keymap(term_buf, 'c', '<M-u>', '<C-t>', { noremap = true })
  vim.api.nvim_buf_set_keymap(term_buf, 'c', '<M-n>', '<C-g>', { noremap = true })

  vim.api.nvim_buf_set_keymap(term_buf, 'n', 'u', '<C-u>', { noremap = true })
  vim.api.nvim_buf_set_keymap(term_buf, 'n', 'd', '<C-d>', { noremap = true })
  vim.api.nvim_buf_set_keymap(term_buf, 'n', 'g', 'gg', { noremap = true, nowait = true })
  vim.api.nvim_buf_set_keymap(term_buf, 'v', 'u', '<C-u>', { noremap = true })
  vim.api.nvim_buf_set_keymap(term_buf, 'v', 'd', '<C-d>', { noremap = true })
  vim.api.nvim_buf_set_keymap(term_buf, 'v', 'g', 'gg', { noremap = true, nowait = true })

  local group = vim.api.nvim_create_augroup('kitty+page', {})

  function UpdateTabline()
    local query = vim.fn.getcmdline()
    if query ~= '' then
      vim.fn.setreg('/', query)
    end
    query = vim.fn.getreg('/')
    if query == '' then
      vim.o.tabline = ' No search '
      return
    end

    local searchcount = vim.fn.searchcount({recompute = true})
    local total_matches = searchcount.total
    local current_match = searchcount.current

    if total_matches > 0 then
      vim.o.tabline = ' Search: ' .. query .. ' | Match ' .. current_match .. ' of ' .. total_matches .. ' '
    else
      vim.o.tabline = ' Search: ' .. query .. ' | No matches '
    end
  end

  vim.fn.setreg('/', '')
  vim.opt.hlsearch = true
  vim.opt.ignorecase = true
  vim.opt.smartcase = true
  vim.opt.wrapscan = false

  vim.g.clipboard = {
    name = 'OSC 52',
    copy = {
      ['+'] = require('vim.ui.clipboard.osc52').copy('+'),
      ['*'] = require('vim.ui.clipboard.osc52').copy('*'),
    },
    paste = {
      ['+'] = require('vim.ui.clipboard.osc52').paste('+'),
      ['*'] = require('vim.ui.clipboard.osc52').paste('*'),
    },
  }

  vim.cmd([[
    augroup UpdateTablineOnSearch
      autocmd!
      autocmd CmdlineChanged * lua UpdateTabline()
      autocmd CursorMoved * lua UpdateTabline()
      autocmd CursorMovedI * lua UpdateTabline()
      autocmd InsertEnter * lua UpdateTabline()
    augroup END
  ]])

  vim.o.showtabline = 2

  UpdateTabline()

  local setCursor = function()
    local line = vim.api.nvim_buf_line_count(term_buf)
    local start_line = INPUT_LINE_NUMBER
    local scroll_line = line - 1
    if (CURSOR_LINE < line) then
      line = CURSOR_LINE
      start_line = start_line + 1
      scroll_line = line - 2
    end
    vim.api.nvim_feedkeys(tostring(start_line) .. [[ggzt]], 'n', true)
    if (scroll_line > 0) then
      vim.api.nvim_feedkeys(tostring(scroll_line) .. [[j]], 'n', true)
    end
    vim.api.nvim_feedkeys([[0]], 'n', true)
    vim.api.nvim_feedkeys(tostring(CURSOR_COLUMN - 1) .. [[l]], 'n', true)

    vim.cmd('highlight MsgArea guibg=#00346E')
    vim.cmd('highlight TabLineFill guibg=#00346E')
    vim.cmd('highlight Search guibg=#70C0BA guifg=#000000')
    vim.cmd('highlight IncSearch guibg=#C397D8 guifg=#000000')
    vim.cmd('highlight CurSearch guibg=#C397D8 guifg=#000000')

    if (SEARCH == 's') then
      vim.api.nvim_feedkeys('?', 'n', true)
    elseif (SEARCH == 'u') then
      vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('k?❯<CR>', true, false, true), 'n', true)
    end
  end

  vim.api.nvim_create_autocmd('ModeChanged', {
                                group = group,
                                buffer = term_buf,
                                callback = function()
                                  local mode = vim.fn.mode()
                                  if mode == 't' then
                                    vim.cmd.stopinsert()
                                  end
                                end,
  })

  vim.api.nvim_create_autocmd('VimEnter', {
                                group = group,
                                pattern = '*',
                                once = true,
                                callback = function(ev)
                                  local current_win = vim.fn.win_getid()
                                  for _, line in ipairs(vim.api.nvim_buf_get_lines(ev.buf, 0, -2, false)) do
                                    vim.api.nvim_chan_send(term_io, line)
                                    vim.api.nvim_chan_send(term_io, '\r\n')
                                  end
                                  for _, line in ipairs(vim.api.nvim_buf_get_lines(ev.buf, -2, -1, false)) do
                                    vim.api.nvim_chan_send(term_io, line)
                                  end
                                  vim.api.nvim_win_set_buf(current_win, term_buf)
                                  vim.api.nvim_buf_delete(ev.buf, { force = true } )
                                end
  })

  vim.defer_fn(setCursor, 10)
end
