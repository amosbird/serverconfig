return function(INPUT_LINE_NUMBER, CURSOR_LINE, CURSOR_COLUMN, SEARCH)
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
  vim.api.nvim_buf_set_keymap(term_buf, 'n', '<ESC>', '<Cmd>q<CR>', { })
  vim.api.nvim_buf_set_keymap(term_buf, 'n', 'u', '<C-u>', { noremap = true })
  vim.api.nvim_buf_set_keymap(term_buf, 'n', 'd', '<C-d>', { noremap = true })
  vim.api.nvim_buf_set_keymap(term_buf, 'n', 'g', 'gg', { noremap = true, nowait = true })

  -- vim.api.nvim_buf_set_keymap(term_buf, 'n', '<ESC>', '<Cmd>q<CR>', { })
  -- vim.api.nvim_buf_set_keymap(term_buf, 'n', '<ESC>', '<Cmd>q<CR>', { })
  -- vim.api.nvim_buf_set_keymap(term_buf, 'n', '<ESC>', '<Cmd>q<CR>', { })
  -- vim.api.nvim_buf_set_keymap(term_buf, 'n', '<ESC>', '<Cmd>q<CR>', { })
  local group = vim.api.nvim_create_augroup('kitty+page', {})

  local searchBackwardWithPrompt = function()
    local prompt = 'Search backward: '
    local search_term = vim.fn.input(prompt)
    if search_term ~= '' then
        vim.cmd('?' .. search_term)
    end
  end

  function UpdateTabline()
    local query = vim.fn.getcmdline()
    if query ~= '' then
      vim.fn.setreg('/', query)
    end
    query = vim.fn.getreg('/')

    local searchcount = vim.fn.searchcount({recompute = true})
    local total_matches = searchcount.total
    local current_match = searchcount.current

    if total_matches > 0 then
      vim.o.tabline = ' Search: ' .. query .. ' | Match ' .. current_match .. ' of ' .. total_matches .. ' '
    else
      vim.o.tabline = ' Search: ' .. query .. ' | No matches '
    end
  end

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
    vim.api.nvim_feedkeys(tostring(INPUT_LINE_NUMBER) .. [[ggzt]], 'n', true)
    local line = vim.api.nvim_buf_line_count(term_buf)
    if (CURSOR_LINE <= line) then
      line = CURSOR_LINE
    end
    vim.api.nvim_feedkeys(tostring(line - 1) .. [[j]], 'n', true)
    vim.api.nvim_feedkeys([[0]], 'n', true)
    vim.api.nvim_feedkeys(tostring(CURSOR_COLUMN - 1) .. [[l]], 'n', true)
    vim.cmd('highlight MsgArea guibg=#00346E')
    vim.cmd('highlight TabLineFill guibg=#00346E')
    vim.o.tabline = ' No search pattern '

    if (SEARCH == 1) then
      -- require('searchbox').incsearch()
      -- searchBackwardWithPrompt()
      vim.api.nvim_feedkeys('?', 'n', true)
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

  -- vim.opt.incsearch = true
  vim.opt.hlsearch = true
  vim.opt.ignorecase = true
  vim.opt.smartcase = true
  vim.defer_fn(setCursor, 10)
end
