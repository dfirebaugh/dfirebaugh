local o = vim.opt

o.clipboard = "unnamedplus" -- fix the clipboard
o.list = true
o.listchars = {
	space    = "⋅",
	tab      = "→ ",
	trail    = "•",
	extends  = "⟩",
	precedes = "⟨",
	eol      = "↴",
}

-- line numbers
o.number = true
o.relativenumber = true

-- indent settings
-- o.cindent = true
-- o.cino = ":0"       -- Use tabs for indentation
-- o.tabstop = 4       -- width of a tab character
-- o.shiftwidth = 4    -- spaces per indent
-- o.expandtab = false -- use tabs

o.mouse = "a"          -- enable mouse in all modes
o.termguicolors = true -- needed for modern colorschemes
o.updatetime = 300     -- faster CursorHold
o.signcolumn = "yes"   -- keep signcolumn open

-- vim.g.base46_cache = vim.fn.stdpath("data") .. "/.cache/base46/"

local undodir = vim.fn.stdpath("state") .. "/undo"
if vim.fn.isdirectory(undodir) == 0 then
	vim.fn.mkdir(undodir, "p")
end

vim.opt.undodir = undodir
vim.opt.undofile = true

o.cursorline = true
o.cursorcolumn = true

vim.cmd([[
  highlight CursorLine guibg=#ffff2a
  highlight CursorColumn guibg=#2a2a2a
]])
